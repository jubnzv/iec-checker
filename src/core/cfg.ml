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
  let rec mk_bbs_aux stmts bbs_acc (bb_preds : bb list) (bb_preds_ids : int list) : (bb list) =
    (** Link previous [bbs] blocks with the [next_bb] block.
        Links represented as mutable [succs] and [preds] fields, so we return nothing.

        TODO: Unique constraint check.
        Need to figure out how to use sets instead lists for keeping links.
    *)
    let link_with_next (next_bb : bb) (bbs : bb list) (ids : int list) : (unit) =
      let aux (next_bb : bb) (bbs : bb list) : (unit) =
        List.iter
          bbs
          ~f:(fun bb -> begin
                next_bb.preds <- (next_bb.preds @ [bb.id]);
                bb.succs <- (bb.succs @ [next_bb.id]);
              end)
      in
      List.filter
        bbs
        ~f:(fun bb -> List.exists ids ~f:(fun id -> phys_equal id bb.id))
      |> aux next_bb
    in
    (** Create basic blocks for the statements nested into [stmt].
        First created BB will be bound with blocks from [bb_preds] that have
        id from [bb_preds_ids].
        Result will contain list with [bb_preds] extended with created BBs. *)
    let rec mk_nested_bbs (stmt : S.statement) (bb_preds : bb list) (bb_preds_ids : int list) : (bb list * int list) =
      (** Create a list of basic blocks for the consecutive list of statements
          nested in [stmt]. [bb_pred] is a basic block that corresponds to
          [stmt]. It will be the first item of the result list; its [succs] and
          [preds] edges will contains corresponding ids, block type stills the
          same.

          @return List of nested basic blocks and IDs of created basic blocks
                  that should be linked with the next statement. *)
      let fold_nested_stmts stmts bb_preds (bb_preds_ids : int list) : (bb list * int list) =
        List.fold_left
          stmts
          ~init:([] (* created bbs *), bb_preds_ids (* ids of the last bbs *))
          ~f:(fun (acc, last_ids) s -> begin
                let new_bb = mk_bb BB s in

                match acc with
                (* Link with [bb_preds] at the first fold iteration.*)
                | [] -> link_with_next new_bb bb_preds last_ids;
                  (* Link with basic blocks from previous fold iteration.*)
                | _ -> link_with_next new_bb acc last_ids;
                  ;

                  let (nbbs, nbbs_last_ids) = mk_nested_bbs s [new_bb] [new_bb.id] in
                  (acc @ nbbs, nbbs_last_ids)
              end)
      in
      (** Same as [fold_nested_stmts] but all created nodes would be linked with
          [bb_preds] and returned last_ids accumulates over fold iterations. *)
      let fold_nested_stmts_inconsist stmts bb_preds (bb_preds_ids : int list) : (bb list * int list) =
        List.fold_left
          stmts
          ~init:([] (* created bbs *), bb_preds_ids (* ids of the last bbs *))
          ~f:(fun (acc, acc_ids) s -> begin
                let new_bb = mk_bb BB s in
                link_with_next new_bb bb_preds acc_ids;
                let (nbbs, nbbs_last_ids) = mk_nested_bbs s [new_bb] [new_bb.id] in
                (acc @ nbbs, acc_ids @ nbbs_last_ids)
              end)
      in
      match stmt with
      | S.StmExpr (_, _) ->
        begin
          (* FIXME: This doesn't handle nested statements in function params
             assignment. I suppose we need to replace these expressions with
             statements in parser/AST. *)
          (bb_preds, bb_preds_ids)
        end
      | S.StmElsif (_, cond_stmt, body_stmts) ->
        begin
          (* Create basic blocks for [cond_stmt]. *)
          let (cond_bbs, cond_bbs_last_ids) = fold_nested_stmts [cond_stmt] bb_preds bb_preds_ids in
          (* Connect BB for the ELSIF statement with condition BB. *)
          let first_cond_bb = List.nth_exn cond_bbs 0 in
          link_with_next first_cond_bb bb_preds bb_preds_ids;

          (* Create basic blocks for [body_stmts]. *)
          let (body_bbs, body_bbs_last_ids) = fold_nested_stmts body_stmts cond_bbs cond_bbs_last_ids in

          (bb_preds @ cond_bbs @ body_bbs, body_bbs_last_ids)
        end
      | S.StmIf (_, cond_stmt, body_stmts, elsif_stmts, else_stmts) ->
        begin
          (* Create basic blocks for [cond_stmt]. *)
          let (cond_bbs, cond_bbs_last_ids) = fold_nested_stmts [cond_stmt] bb_preds bb_preds_ids in
          (* Connect BB for the IF statement with condition BB. *)
          let first_cond_bb = List.nth_exn cond_bbs 0 in
          link_with_next first_cond_bb bb_preds bb_preds_ids;

          (* Create basic blocks for [body_stmts]. *)
          let (body_bbs, body_bbs_last_ids) = fold_nested_stmts body_stmts cond_bbs cond_bbs_last_ids in

          (* Create basic blocks for [elsif_stmts]. *)
          let (elsif_bbs, elsif_last_ids) =
            List.fold_left
              elsif_stmts
              ~init:([], [])
              ~f:(fun (acc, acc_ids) stmt -> begin
                    let (bbs, bbs_last_ids) = fold_nested_stmts [stmt] cond_bbs cond_bbs_last_ids in
                    (acc @ bbs, acc_ids @ bbs_last_ids)
                  end)
          in

          (* Create basic blocks for [else_stmt]. *)
          let (else_bbs, else_last_ids) = fold_nested_stmts else_stmts cond_bbs cond_bbs_last_ids in

          (* IDs of BBs that will be linked with the next statement after IF. *)
          let last_bbs_ids =
            (* Direct jump from the IF condition if there are no else statements. *)
            let cond_bbs_last_ids =
              match else_stmts with
              | [] -> cond_bbs_last_ids
              | _ -> []
            in
            cond_bbs_last_ids @ body_bbs_last_ids @ elsif_last_ids @ else_last_ids
          in

          (bb_preds @ cond_bbs @ body_bbs @ elsif_bbs @ else_bbs, last_bbs_ids)
        end
      | S.StmCase (_, cond_stmt, case_sels, else_stmts) ->
        begin
          (* Create basic blocks for [cond_stmt]. *)
          let (cond_bbs, cond_bbs_last_ids) = fold_nested_stmts [cond_stmt] bb_preds bb_preds_ids in
          (* Connect BB for the CASE statement with condition BB. *)
          let first_cond_bb = List.nth_exn cond_bbs 0 in
          link_with_next first_cond_bb bb_preds bb_preds_ids;

          (* Create basic blocks for [case_sels] statements. *)
          let (cs_bbs, cs_bbs_last_ids) = List.fold_left
              case_sels
              ~init:([], [])
              ~f:(fun (acc, acc_ids) case_sel -> begin
                    (* There are cases when we have multiple case selections.
                       For example:
                           CASE 3,4 : a := 19;
                       This means that we want to link each of case selection
                       statements with appropriate body statement:
                           3 <-> a := 19;
                           4 <-> a := 19; *)
                    let (case_bbs, case_bbs_last_ids) =
                      (* Each case selection will be linked with [cond_bbs]. *)
                      fold_nested_stmts_inconsist case_sel.case cond_bbs cond_bbs_last_ids
                    in
                    let (body_bbs, body_bbs_last_ids) =
                      fold_nested_stmts case_sel.body case_bbs case_bbs_last_ids
                    in
                    ((acc @ case_bbs @ body_bbs),
                     (acc_ids @ body_bbs_last_ids))
                  end)
          in

          (* Create basic blocks for [else_stmt]. *)
          let (else_bbs, else_last_ids) = fold_nested_stmts else_stmts cond_bbs cond_bbs_last_ids in

          (* IDs of BBs that will be linked with the next statement after CASE. *)
          let last_bbs_ids =
            (* Direct jump from the CASE condition if there are no else (default) statements. *)
            let cond_bbs_last_ids =
              match else_stmts with
              | [] -> cond_bbs_last_ids
              | _ -> []
            in
            cond_bbs_last_ids @ cs_bbs_last_ids @ else_last_ids
          in

          (bb_preds @ cond_bbs @ cs_bbs @ else_bbs, last_bbs_ids)
        end
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
      | _ -> (bb_preds, bb_preds_ids) (* TODO: Need test previous statements first. *)
    in
    match stmts with
    | [] -> begin
        match bb_preds with
        | [] -> begin
            (* Empty *)
            []
          end
        | _ -> begin
            (* Insert the last BBs *)
            bbs_acc @ bb_preds
          end
      end
    | [s] -> begin
        let bb = mk_bb BBExit s in
        match bb_preds with
        | [] -> begin
            (* Single BB *)
            let (n_bbs, n_bbs_last_ids) = mk_nested_bbs s [bb] [bb.id] in
            mk_bbs_aux [] [] n_bbs n_bbs_last_ids
          end
        | _ -> begin
            (* Create BBs for the last statement. *)
            link_with_next bb bb_preds bb_preds_ids;
            let (n_bbs, n_bbs_last_ids) = mk_nested_bbs s [bb] [bb.id] in
            mk_bbs_aux [] (bbs_acc @ bb_preds) n_bbs n_bbs_last_ids
          end
      end
    | s :: stail -> begin
        match bb_preds with
        | [] -> begin
            (* First BB *)
            let bb = mk_bb BBEntry s in
            let (n_bbs, n_bbs_last_ids) = mk_nested_bbs s [bb] [bb.id] in
            mk_bbs_aux stail n_bbs n_bbs n_bbs_last_ids
          end
        | _ -> begin
            (* Regular BB *)
            let bb = mk_bb BB s in
            link_with_next bb bb_preds bb_preds_ids;
            let (n_bbs, n_bbs_last_ids) = mk_nested_bbs s [bb] [bb.id] in
            mk_bbs_aux stail (bbs_acc @ bb_preds) n_bbs n_bbs_last_ids
          end
      end
  in
  List.rev (mk_bbs_aux stmts [] [] [])

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
