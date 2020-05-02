open Core_kernel

module S = Syntax
module E = Error
module AU = Ast_util
module TI = Tok_info

[@@@warning "-32"]

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
  { id; ty; preds; succs; stmt }

let empty_cfg () =
  let bb_map = BBMap.empty in
  let entry_bb_id = -1 in
  let pou_id = -1 in
  { bb_map; entry_bb_id; pou_id }

(** [mk_bbs iec_element stmts] Create a list of bounded basic blocks for [stmts]
    and their nested statements. *)
let mk_bbs stmts =
  let sublist l low high =
    List.filteri l ~f:(fun i _ -> i >= low && i < high)
  in
  let rec mk_bbs_aux stmts acc (bb_pred : bb option) =
    (** Bound two basic blocks *)
    let bound_bbs pred succ =
      pred.succs <- (List.append pred.succs [succ.id]);
      succ.preds <- (List.append succ.preds [pred.id]);
      (pred, succ)
    in
    (** Create basic blocks for the statements nested into [stmt].
        First created BB will be bound with [bb_pred].
        Result will contain list with [bb_pred] extended with created BBs. *)
    let rec mk_nested_bbs (stmt : S.statement) (bb_pred : bb) : (bb list) =
      (** Create a list of basic blocks for the consecutive list of statements
          nested in [stmt]. [bb_pred] is a basic block that corresponds to
          [stmt]. It will be the first item of the result list; its succs and
          preds edges will contains corresponding ids, block type stills the
          same. *)
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
                        let bbs = mk_nested_bbs s bb_pred in
                        (* First item will be updated bb_pred anyway *)
                        let upd_bb_pred = List.nth_exn bbs 0 in
                        let upd_bb_pred = { upd_bb_pred with ty = bb_pred.ty } in
                        acc @ [upd_bb_pred] @ (sublist bbs 1 (List.length bbs)), Some(upd_bb_pred)
                      end
                    | Some fold_bb_pred -> begin
                        (* We're inside List.fold_left iteration. Each next BB should
                           be bounded with the BB from the previous iteration. *)
                        let bbs = mk_nested_bbs s fold_bb_pred in
                        let upd_bb_pred = List.nth_exn bbs 0 in
                        let upd_bb_pred = { upd_bb_pred with ty = fold_bb_pred.ty } in
                        acc @ [upd_bb_pred] @ (sublist bbs 1 (List.length bbs)), Some(upd_bb_pred)
                      end
                  end)
          in
          bbs
      in
      let fold_nested_expr e bb_pred =
        fold_nested_stmts (AU.expr_to_stmts e) bb_pred
      in
      match stmt with
      | S.StmAssign (_, _, e) -> begin
          fold_nested_expr e bb_pred
        end
      | S.StmElsif (_, e, stmts) ->
        begin
          let cond_bbs = fold_nested_expr e bb_pred in
          let last_cond_bb = List.last_exn cond_bbs in
          let body_bbs = fold_nested_stmts stmts last_cond_bb in
          (* Last condition BB is already included in [body_bbs] *)
          let cond_bbs = (sublist cond_bbs 0 ((List.length cond_bbs) - 1)) in
          cond_bbs @ body_bbs
        end
      | S.StmIf (_, e, stmts_body, stmts_elsif, stmts_else) ->
        begin
          let cond_bbs = fold_nested_expr e bb_pred in
          let last_cond_bb = List.last_exn cond_bbs in
          let body_bbs = fold_nested_stmts stmts_body last_cond_bb in
          (* Last condition BB will be updated after every traverse over nested statements *)
          let cond_bbs = (sublist cond_bbs 0 ((List.length cond_bbs) - 1)) in
          let last_cond_bb = List.nth_exn body_bbs 0 in
          (* First BB of [body_bbs] will be replaced with new bounds from [elsif_bbs] *)
          let body_bbs = (sublist body_bbs 1 (List.length body_bbs)) in
          let elsif_bbs = fold_nested_stmts stmts_elsif last_cond_bb in
          let last_cond_bb = List.nth_exn elsif_bbs 0 in
          (* First BB of [elsif_bbs] will be replaced with new bounds from [else_bbs] *)
          let elsif_bbs = (sublist elsif_bbs 1 (List.length elsif_bbs)) in
          let else_bbs = fold_nested_stmts stmts_elsif last_cond_bb in
          (* Finally, we cut [else_bbs] last element. It place is after [cond_bbs].*)
          let last_cond_bb = List.nth_exn else_bbs 0 in
          let else_bbs = fold_nested_stmts stmts_else last_cond_bb in
          cond_bbs @ [last_cond_bb] @ body_bbs @ elsif_bbs @ else_bbs
        end
      | _ -> [] (* TODO: Need test previous statements first. *)
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
            (* Insert last BB *)
            acc @ [bbp]
          end
      end
    | [s] -> begin
        let bb = mk_bb BBExit s in
        match bb_pred with
        | None -> begin
            (* Single BB *)
            let nested_bbs = mk_nested_bbs s bb in
            let last_nested_bb = List.last_exn nested_bbs in
            mk_bbs_aux [] (acc @ nested_bbs) (Some(last_nested_bb))
          end
        | Some bbp -> begin
            (* Last BB *)
            let (bbp, bb) = bound_bbs bbp bb in
            let nested_bbs = mk_nested_bbs s bb in
            let last_nested_bb = List.last_exn nested_bbs in
            mk_bbs_aux [] (acc @ [bbp] @ nested_bbs) (Some(last_nested_bb))
          end
      end
    | s :: tail -> begin
        match bb_pred with
        | None -> begin
            (* First BB *)
            let bb = mk_bb BBEntry s in
            let nested_bbs = mk_nested_bbs s bb in
            (* Printf.printf "len(nested_bbs)=%d\n" (List.length nested_bbs); *)
            let last_nested_bb = List.last_exn nested_bbs in
            mk_bbs_aux tail nested_bbs (Some(last_nested_bb))
          end
        | Some bbp -> begin
            (* Regular BB *)
            let bb = mk_bb BB s in
            let (bbp, bb) = bound_bbs bbp bb in
            let nested_bbs = mk_nested_bbs s bb in
            let last_nested_bb = List.last_exn nested_bbs in
            mk_bbs_aux tail (acc @ [bbp] @ nested_bbs) (Some(last_nested_bb))
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
