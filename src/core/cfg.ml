open Core_kernel

module S = Syntax
module E = Error
module AU = Ast_util
module TI = Tok_info

[@@@warning "-32"]
[@@@warning "-26"]
[@@@warning "-27"]

type bb_ty =
  | BB (** Regular basic block *)
  | BBEntry (** Point of entry *)
  | BBExit (** Point of exit *)
  | BBJump (** Indirect jump to/from a node *)
[@@deriving show { with_path = false }, to_yojson]

and bb =
  {
    id : int;
    ty: bb_ty;
    mutable preds : int list; (** Ids of predecessor nodes *)
    mutable succs : int list; (** Ids of successor nodes *)
    stmt : S.statement [@opaque];
  }

let bb_to_yojson bb =
  let yojson_ints (ids : int list) : Yojson.Safe.t list =
    List.fold_left ids
      ~init:[]
      ~f:(fun acc i -> acc @ [`Int(i)])
  in
  `Assoc [
    "id", `Int(bb.id);
    "type", bb_ty_to_yojson bb.ty;
    "preds", `List(yojson_ints bb.preds);
    "succs", `List(yojson_ints bb.succs);
    "stmt_id", `Int(S.stmt_get_id bb.stmt);
  ]

(** Map for basic blocks in CFG accessible by unique identifier *)
module BBMap = struct
  type t = (int, bb, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)

  let add m bb = Map.set m ~key:bb.id ~data:bb

  (** Remove last element from the bbs list *)
  let pop_exn (m : t) : (t) =
    let bbs = Map.to_alist m in
    let (last_id, _) = List.last_exn bbs in
    Map.remove m last_id

  let to_alist m = Map.to_alist m

  let first (m : t) : (bb option) =
    let all = Map.to_alist m in
    let values_opt = List.nth all 0 in
    match values_opt with
    | Some (_, bb) -> Some(bb)
    | None -> None

  let find m k = Map.find m k

  let to_yojson (m : t) : Yojson.Safe.t =
    let items =
      Map.fold_right m
        ~init:[]
        ~f:(fun[@warning "-27"] ~key ~data lst ->
            let bb = data in
            [bb_to_yojson bb] @ lst
          ) in
    `List(items)
end

type t = {
  mutable bb_map : BBMap.t; (** Map of basic blocks *)
  entry_bb_id : int; (** Id of the initial basic block *)
  pou_id : int; (** Id of the POU that this graph belongs to *)
}

(** Generate unique id *)
let mk_id =
  let n = ref (-1) in
  fun () -> incr n; !n

(** Create basic block instance from a statement *)
let mk_bb ty stmt =
  let id = mk_id () in
  let preds = [] in
  let succs = [] in
  Printf.printf "mk_bb id=%d stmt.id=%d\n" id (S.stmt_get_id stmt);
  { id; ty; preds; succs; stmt }

let empty_cfg () =
  let bb_map = BBMap.empty in
  let entry_bb_id = -1 in
  let pou_id = -1 in
  { bb_map; entry_bb_id; pou_id }

(** [mk_bbs iec_element stmts] Create a list of bounded basic blocks for [stmts]
    and their nested statements. *)
let mk_bbs stmts : (bb list) =
  let sublist l low high =
    List.filteri l ~f:(fun i _ -> i >= low && i < high)
  in
  let rec mk_bbs_aux stmts bbs_acc (bb_pred : bb option) : (bb list) =
    (** Link two basic blocks *)
    let link_bbs pred succ =
      pred.succs <- (List.append pred.succs [succ.id]);
      succ.preds <- (List.append succ.preds [pred.id]);
      (pred, succ)
    in
    (** Extend [succ] links of [bbs] with [next_bb.id].
        Extend [prev] links of [next_bb] with [bbs] [id]s. *)
    let set_links_to_next (next_bb : bb) (bbs : bb list) : (bb list * bb) =
      List.fold_left
        bbs
        ~init:([], next_bb)
        ~f:(fun (acc, next_bb) bb -> (
              acc @ [{bb with succs = (bb.succs @ [next_bb.id])}],
              {next_bb with preds = (next_bb.preds @ [bb.id])}))
    in
    (** Wrapper over [set_links_to_next]. *)
    let set_links_to_next_by_ids next_bb bbs (ids : int list) =
      List.filter
        bbs
        ~f:(fun bb -> List.exists ids ~f:(fun id -> phys_equal id bb.id))
      |> set_links_to_next next_bb
    in
    (** Create basic blocks for the statements nested into [stmt].
        First created BB will be bound with [bb_pred].
        Result will contain list with [bb_pred] extended with created BBs. *)
    let rec mk_nested_bbs (stmt : S.statement) (bb_pred : bb) : (bb list * int list) =
      (** Create a list of basic blocks for the consecutive list of statements
          nested in [stmt]. [bb_pred] is a basic block that corresponds to
          [stmt]. It will be the first item of the result list; its [succs] and
          [preds] edges will contains corresponding ids, block type stills the
          same.

          @return List of nested basic blocks and IDs of created basic blocks
                  that should be bound with the next statement. *)
      let fold_nested_stmts stmts bb_pred =
        if (List.is_empty stmts) then
          [bb_pred]
        else
          (* Closure that keeps default bb_pred ty *)
          (* let saved_ty () = *)
          (*     bb_pred.ty    *)
          (* in                *)
          let (bbs, _) =
            List.fold_left
              stmts
              ~init:([] (* bbs *), None (* bb_pred *))
              ~f:(fun (acc, fold_bb_pred_opt) s -> begin
                    match fold_bb_pred_opt with
                    | None -> begin
                        (* First fold iteration. Bound with the initial bb_pred. *)
                        let (bbs, _) = mk_nested_bbs s bb_pred in
                        (* Restore type of the first BB. *)
                        let upd_bb_pred = List.nth_exn bbs 0 in
                        let upd_bb_pred = { upd_bb_pred with ty = bb_pred.ty } in
                        (* TODO: What if it was a "complex" statement with a few exit
                           points? *)
                        acc @ [upd_bb_pred] @ (sublist bbs 1 (List.length bbs)), Some(upd_bb_pred)
                      end
                    | Some fold_bb_pred -> begin
                        (* We're inside List.fold_left iteration. Each next BB should
                           be bounded with the BB from the previous iteration. *)
                        let (bbs, _) = mk_nested_bbs s fold_bb_pred in
                        (* Restore type of the first BB. *)
                        let upd_bb_pred = List.nth_exn bbs 0 in
                        let upd_bb_pred = { upd_bb_pred with ty = fold_bb_pred.ty } in
                        (* TODO: What if it was a "complex" statement with a few exit
                           points? *)
                        acc @ [upd_bb_pred] @ (sublist bbs 1 (List.length bbs)), Some(upd_bb_pred)
                      end
                  end)
          in
          bbs
      in
      (* Fetch nested statement from the complex expressions like function calls. *)
      let fold_nested_expr e bb_pred =
        fold_nested_stmts (AU.expr_to_stmts e) bb_pred
      in
      match stmt with
      | S.StmExpr (_, expr) ->
        begin
          let nested_bbs = fold_nested_expr expr bb_pred in
          let expr_stmt_bb = List.last_exn nested_bbs in
          Printf.printf "EXPR: len(expr_stmt_bbs)=%d\n" (List.length nested_bbs);
          Printf.printf "EXPR: expr_stmt_bb.stmt.id=%d\n" (S.stmt_get_id expr_stmt_bb.stmt);
          ([expr_stmt_bb] @ (sublist nested_bbs 0 ((List.length nested_bbs) - 1)),
             [expr_stmt_bb.id])
        end
      | S.StmElsif (_, cond_stmt, body_stmts) ->
        begin
          (* BB of ELSIF statement ([stmt]). We will modify it to connect it with
             the control statements of ELSIF. *)
          let elsif_stmt_bb = bb_pred in
          Printf.printf "ELSIF: len(elsif_stmt.succs)=%d\n" (List.length elsif_stmt_bb.succs);

          (* Create basic blocks for [cond_stmt]. *)
          let cond_head_bb = mk_bb BB cond_stmt in
          let (elsif_stmt_bb, cond_head_bb) = link_bbs elsif_stmt_bb cond_head_bb in
          (* TODO: Nested statements? They are no really possible because we have S.StmExpr here. *)
          let (cond_bbs, _) = mk_nested_bbs cond_stmt cond_head_bb in
          (* Fisrt condition BB will be added as first BB of the body BBs after reconnecting. *)
          let cond_head_bb = List.nth_exn cond_bbs 0 in
          let cond_bbs = (sublist cond_bbs 1 (List.length cond_bbs)) in

          (* Create basic blocks for [body_stmts]. *)
          let body_bbs = fold_nested_stmts body_stmts cond_head_bb in
          let body_last_bb = List.last_exn body_bbs in (* to bound with expr next after if *)

          ([cond_head_bb] @ cond_bbs @ body_bbs,
           [body_last_bb.id])
        end
      | S.StmIf (_, cond_stmt, body_stmts, elsif_stmts, else_stmts) ->
        begin
          (* BB of IF statement ([stmt]). We will modify it to connect it with
             the control statements of IF. *)
          let if_stmt_bb = bb_pred in
          Printf.printf "IF: len(if_stmt.succs)=%d\n" (List.length if_stmt_bb.succs);

          (* Create basic blocks for [cond_stmt]. *)
          let cond_head_bb = mk_bb BB cond_stmt in
          let (if_stmt_bb, cond_head_bb) = link_bbs if_stmt_bb cond_head_bb in
          (* This is not possible to have multiple exit entries from here. This is always S.StmExpr. *)
          let (cond_bbs, _) = mk_nested_bbs cond_stmt cond_head_bb in
          (* Fisrt condition BB will be added as first BB of body BBs after reconnecting. *)
          let cond_head_bb = List.nth_exn cond_bbs 0 in
          let cond_bbs = (sublist cond_bbs 1 (List.length cond_bbs)) in
          Printf.printf "IF:cond: len(cond_bbs)=%d\n" (List.length cond_bbs);
          Printf.printf "IF:cond: cond_head_bb.stmt.id=%d\n" (S.stmt_get_id cond_head_bb.stmt);

          (* Create basic blocks for [body_stmts]. *)
          let body_bbs = fold_nested_stmts body_stmts cond_head_bb in
          let body_last_bb = List.last_exn body_bbs in (* to bound with expr next after if *)
          let cond_head_bb = List.nth_exn body_bbs 0 in (* to connect with elsifs *)
          Printf.printf "IF:body: len(body_bbs)=%d\n" (List.length body_bbs);

          (* Create basic blocks for [elsif_stmts]. *)
          let ((elsif_bbs: bb list list), (elsif_last_bb_ids: int list), cond_head_bb) =
            (* We iterate over elsif statements and update connections to [cond_head_bb]. *)
            List.fold_left
              elsif_stmts
              ~init:([], [], cond_head_bb)
              ~f:(fun (bbs, last_bbs, cond_head_bb) v -> begin
                    (* TODO *)
                    let (elsif_bbs, _) = mk_nested_bbs v cond_head_bb in
                    let elsif_last_bb = List.last_exn elsif_bbs in
                    let cond_head_bb = List.nth_exn elsif_bbs 0 in
                    (bbs @ [elsif_bbs],
                     last_bbs @ [elsif_last_bb.id],
                     cond_head_bb)
                  end)
          in
          Printf.printf "IF:elsif: len(elsif_bbs)=%d\n" (List.length elsif_bbs);

          (* Create basic blocks for [stmts_else]. *)
          let else_bbs = fold_nested_stmts else_stmts cond_head_bb in
          let else_last_bb = List.last_exn else_bbs in (* to bound with expr next after if *)
          let cond_head_bb = List.nth_exn else_bbs 0 in (* we have filled [cond_head_bb.succs] *)
          Printf.printf "IF:else: len(elsif_bbs)=%d\n" (List.length else_bbs);

          ([if_stmt_bb] @
           [cond_head_bb] @ cond_bbs @
           body_bbs @
           List.fold_left elsif_bbs ~init:[] ~f:(fun acc bbs -> acc @ bbs) @
           else_bbs,
           (* ids of BBs to bound with the next statement after the IF *)
           (elsif_last_bb_ids @ [body_last_bb.id] @ [else_last_bb.id]))
        end
      | _ -> ([bb_pred], [bb_pred.id]) (* TODO: Need test previous statements first. *)
      (* | S.StmCase (_, e, cs_list, stmts_else) ->                               *)
      (*   begin                                                                  *)
      (*     let get_case_bbs bb_stm_parent =                                     *)
      (*       List.fold_left cs_list                                             *)
      (*         ~f:(fun bbs cs ->                                                *)
      (*             begin                                                        *)
      (*               bbs @                                                      *)
      (*               (* case expressions                                     *) *)
      (*               (List.fold_left cs.S.case                                  *)
      (*                  ~f:(fun bbs ec -> bbs @ (expr_to_bbs ec bb_stm_parent)) *)
      (*                  ~init:[]) @                                             *)
      (*               (* case statements                                      *) *)
      (*               (stmts_to_bbs cs.S.body bb_stm_parent)                     *)
      (*             end)                                                         *)
      (*         ~init:[]                                                         *)
      (*     in                                                                   *)
      (*   end                                                                    *)
      (* | S.StmFor (_, _, _, _, _, stmts_body) ->                                *)
      (*   begin                                                                  *)
      (*     (* TODO: Generate assign expression *)                               *)
      (*     let bb_stm_parent = mk_parent stmt bb_parent BB in                   *)
      (*     bb_stm_parent ::                                                     *)
      (*     (stmts_to_bbs stmts_body bb_stm_parent)                              *)
      (*   end                                                                    *)
      (* | S.StmWhile (_, e, stmts_body) ->                                       *)
      (*   begin                                                                  *)
      (*   end                                                                    *)
      (* | S.StmRepeat (_, stmts, e) ->                                           *)
      (*   begin                                                                  *)
      (*   end                                                                    *)
      (* | S.StmFuncParamAssign (_, e, _) ->                                      *)
      (*   begin                                                                  *)
      (*   end                                                                    *)
      (* | S.StmFuncCall (_, _, stmts_body) ->                                    *)
      (*   begin                                                                  *)
      (*   end                                                                    *)
      (* | S.StmExit _ | S.StmReturn _ -> _                                       *)
      (* | S.StmContinue _ -> (* TODO: Add jump edge     *)                       *)
    in
    match stmts with
    | [] -> begin
        match bb_pred with
        | None -> begin
            (* Empty *)
            []
          end
        | Some bbp -> begin
            (* Insert the last BB *)
            bbs_acc @ [bbp]
          end
      end
    | [s] -> begin
        let bb = mk_bb BBExit s in
        match bb_pred with
        | None -> begin
            (* Single BB *)
            (* TODO *)
            let (nested_bbs, _) = mk_nested_bbs s bb in
            let last_nested_bb = List.last_exn nested_bbs in
            mk_bbs_aux [] (bbs_acc @ nested_bbs) (Some(last_nested_bb))
          end
        | Some bbp -> begin
            (* Last BB *)
            let (bbp, bb) = link_bbs bbp bb in
            (* TODO *)
            let (nested_bbs, _) = mk_nested_bbs s bb in
            let last_nested_bb = List.last_exn nested_bbs in
            mk_bbs_aux [] (bbs_acc @ [bbp] @ nested_bbs) (Some(last_nested_bb))
          end
      end
    | s :: tail -> begin
        match bb_pred with
        | None -> begin
            (* First BB *)
            let bb = mk_bb BBEntry s in
            (* TODO *)
            let (nested_bbs, _) = mk_nested_bbs s bb in
            (* Printf.printf "len(nested_bbs)=%d\n" (List.length nested_bbs); *)
            let last_nested_bb = List.last_exn nested_bbs in
            mk_bbs_aux tail nested_bbs (Some(last_nested_bb))
          end
        | Some bbp -> begin
            (* Regular BB *)
            let bb = mk_bb BB s in
            let (bbp, bb) = link_bbs bbp bb in
            (* TODO *)
            let (nested_bbs, _) = mk_nested_bbs s bb in
            let last_nested_bb = List.last_exn nested_bbs in
            mk_bbs_aux tail (bbs_acc @ [bbp] @ nested_bbs) (Some(last_nested_bb))
          end
      end
  in
  List.rev (mk_bbs_aux stmts [] None)

let mk iec_element =
  let cfg = empty_cfg () in
  let cfg = { cfg with pou_id = (S.get_pou_id iec_element) } in
  let stmts = AU.get_top_stmts iec_element in
  List.iter
    (mk_bbs stmts)
    ~f:(fun bb -> cfg.bb_map <- BBMap.add cfg.bb_map bb);
  let entry_bb = BBMap.first cfg.bb_map in
  let entry_bb_id = match entry_bb with
    | Some bb -> bb.id
    | None -> -1
  in
  let cfg = { cfg with entry_bb_id = entry_bb_id } in
  cfg

let get_pou_id c = c.pou_id

let list_basic_blocks cfg =
  BBMap.to_alist cfg.bb_map
  |> List.fold_left
    ~init:[]
    ~f:(fun acc i -> match i with (_, bb) -> acc @ [bb])

let bb_by_id cfg (id : int) =
  BBMap.find cfg.bb_map id

let to_string (cfg : t) : string =
  BBMap.to_alist cfg.bb_map
  |> List.fold_left
    ~init:[]
    ~f:(fun acc i -> match i with (id, bb) ->
        begin
          let edges_to_string (ids: int list) =
            List.fold_left ids
              ~init:[]
              ~f:(fun acc id -> acc @ [string_of_int id])
            |> String.concat ~sep:" "
          in
          let bb_repr =
            Printf.sprintf "[%03d %6s] [preds: %s] [succs: %s]"
              id
              (show_bb_ty bb.ty)
              (edges_to_string bb.preds)
              (edges_to_string bb.succs)
          in
          acc @ [bb_repr]
        end)
  |> String.concat ~sep:"\n"

let to_yojson (c : t) : Yojson.Safe.t =
  let m = c.bb_map in
  `Assoc [
    "pou_id", `Int(c.pou_id);
    "entry_bb_id", `Int(c.entry_bb_id);
    "basic_blocks", BBMap.to_yojson m;
  ]

let bb_get_ti bb =
  S.stmt_get_ti bb.stmt

let create_cfgs elements =
  List.fold_left
    elements
    ~f:(fun cfgs e -> (mk e) :: cfgs)
    ~init:[]
