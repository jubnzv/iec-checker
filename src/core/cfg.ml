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
    mutable ty: bb_ty;
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

  let set m bb = Map.set m ~key:bb.id ~data:bb

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

  let find_exn m k = Map.find_exn m k

  let iter (m: t) f = Map.iter m ~f:f

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
  mutable bbs_map : BBMap.t; (** Map of basic blocks *)
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
  let bbs_map = BBMap.empty in
  let entry_bb_id = -1 in
  let pou_id = -1 in
  { bbs_map; entry_bb_id; pou_id }

(** [fill_bbs_map cfg stmts] Fill [cfg.map] with a linked basic blocks for
    [stmts] and their nested statements. *)
let fill_bbs_map (cfg : t) (stmts : S.statement list) : (unit) =
  (** Recursively traverse over [stmts] to create basic blocks for each
       statement, including the nested ones.
      [bbs_pred_ids] is a list of identifiers of basic blocks from the previous
      iteration. They will be linked with the block created in the current
      iteration. *)
  let rec fill_bbs_map_aux stmts (bbs_pred_ids : int list) : (unit) =
    (** Link blocks from the [bbs_map] with id from [ids] with a [next_bb] block.
        Links are represented as mutable [succs] and [preds] fields, so we
        return nothing.

        TODO: Unique constraint check.
        Need to figure out how to use sets instead lists for keeping links.
    *)
    let link_preds (next_bb : bb) (pred_ids : int list) : (unit) =
      (* TODO: This is horrible. I should use LRU cache (memoization) here and
         stop iteration when all ids was found. *)
      BBMap.iter
        cfg.bbs_map
        (fun (bb) -> begin
             if List.exists pred_ids ~f:(fun id -> phys_equal id bb.id) then
               begin
                 next_bb.preds <- (next_bb.preds @ [bb.id]);
                 bb.succs <- (bb.succs @ [next_bb.id]);
               end
           end)
    in
    let link_preds_by_id (id : int) (pred_ids : int list) : (unit) =
      let bb = BBMap.find_exn cfg.bbs_map id in
      link_preds bb pred_ids
    in
    (** Create basic blocks for the statement [stmt] and its nested statements.
        First created BB will be linked with blocks from [bbs_pred_ids].
        @return List of IDs of the last basic blocks. *)
    let rec mk_nested_bbs (stmt : S.statement) (bbs_pred_ids : int list) : (int list) =
      (** Create a list of basic blocks for the consecutive list of statements
          nested in [stmts]. [bbs_pred_ids] is a list of identifiers for basic
          blocks that will be linked with a BB created for the first statement.

          Next created BBs will be linked with BB created as a previous iteration.

          @return ID of the first created nested block and list of IDs for last
          created basic blocks. *)
      let mk_bbs_nested_stmts_consist stmts (bbs_preds_ids : int list) : (int * int list) =
        let (first_id, last_ids) = List.fold_left
            stmts
            ~init:(None, bbs_preds_ids)
            ~f:(fun (first_id, last_ids) s -> begin
                  let bb = mk_bb BB s in
                  link_preds bb last_ids;
                  cfg.bbs_map <- BBMap.set cfg.bbs_map bb;
                  let first_id = match first_id with None -> bb.id | Some id -> id in
                  (Some(first_id), mk_nested_bbs s [bb.id])
                end)
        in
        let first_id = match first_id with
          | None (* no statements *) -> -1
          | Some(id) -> id
        in
        (first_id, last_ids)
      in
      (** Same as [mk_bbs_nested_stmts_consist] but all created nodes would be
          linked with [bbs_pred_ids] and returned list of last IDs accumulates
          over [stmts] iterations. *)
      let mk_bbs_nested_stmts_inconsist stmts (bbs_pred_ids : int list) : (int * int list) =
        let (first_id, last_ids) = List.fold_left
            stmts
            ~init:(None, bbs_pred_ids)
            ~f:(fun (first_id, acc_last_ids) s -> begin
                  let bb = mk_bb BB s in
                  link_preds bb bbs_pred_ids;
                  cfg.bbs_map <- BBMap.set cfg.bbs_map bb;
                  match first_id with
                  (* first iteration; don't include preds *)
                  | None -> (Some(bb.id), (mk_nested_bbs s [bb.id]))
                  | Some _ -> (first_id, (acc_last_ids @ (mk_nested_bbs s [bb.id])))
                end)
        in
        let first_id = match first_id with
          | None (* no statements *) -> -1
          | Some(id) -> id
        in
        (first_id, last_ids)
      in
      match stmt with
      | S.StmExpr (_, _) ->
        begin
          (* FIXME: This doesn't handle nested statements in function params
             assignment. I suppose we need to replace these expressions with
             statements in parser/AST. *)
          (bbs_pred_ids)
        end
      | S.StmElsif (_, cond_stmt, body_stmts) ->
        begin
          (* Create basic blocks for [cond_stmt]. *)
          let (first_cond_id, cond_last_ids) = mk_bbs_nested_stmts_consist [cond_stmt] bbs_pred_ids in
          (* Connect BB for the ELSIF statement with a condition BB. *)
          link_preds_by_id first_cond_id bbs_pred_ids;
          (* Create basic blocks for [body_stmts]. *)
          let (_, body_last_ids) = mk_bbs_nested_stmts_consist body_stmts cond_last_ids in
          (body_last_ids)
        end
      | S.StmIf (_, cond_stmt, body_stmts, elsif_stmts, else_stmts) ->
        begin
          (* Create basic blocks for [cond_stmt]. *)
          let (first_cond_id, cond_last_ids) = mk_bbs_nested_stmts_consist [cond_stmt] bbs_pred_ids in
          (* Connect BB for the IF statement with condition BB. *)
          link_preds_by_id first_cond_id bbs_pred_ids;

          (* Create basic blocks for [body_stmts]. *)
          let (_, body_last_ids) = mk_bbs_nested_stmts_consist body_stmts cond_last_ids in

          (* Create basic blocks for [elsif_stmts]. *)
          let elsif_last_ids =
            List.fold_left
              elsif_stmts
              ~init:([])
              ~f:(fun (acc) stmt -> begin
                    let (_, last_ids) = mk_bbs_nested_stmts_consist [stmt] cond_last_ids in
                    (acc @ last_ids)
                  end)
          in

          (* Create basic blocks for [else_stmt]. *)
          let (_, else_last_ids) = mk_bbs_nested_stmts_consist else_stmts cond_last_ids in

          (* Set direct jump from the IF condition if there are no else statements. *)
          let cond_last_ids =
            match else_stmts with
            | [] -> cond_last_ids
            | _ -> []
          in
          (cond_last_ids @ body_last_ids @ elsif_last_ids @ else_last_ids)
        end
      | S.StmCase (_, cond_stmt, case_sels, else_stmts) ->
        begin
          (* Create basic blocks for [cond_stmt]. *)
          let (first_cond_id, cond_last_ids) = mk_bbs_nested_stmts_consist [cond_stmt] bbs_pred_ids in
          (* Connect BB for the CASE statement with condition BB. *)
          link_preds_by_id first_cond_id bbs_pred_ids;

          (* Create basic blocks for [case_sels] statements. *)
          let cs_last_ids = List.fold_left
              case_sels
              ~init:([])
              ~f:(fun (acc_ids) case_sel -> begin
                    (* There are cases when we have multiple case selections.
                       For example:
                           CASE 3,4 : a := 19;
                       This means that we want to link each of case selection
                       statements with appropriate body statement:
                           3 <-> a := 19;
                           4 <-> a := 19; *)
                    let (_, case_last_ids) =
                      (* Each case selection will be linked with [cond_bbs]. *)
                      mk_bbs_nested_stmts_inconsist case_sel.case cond_last_ids
                    in
                    let (_, body_last_ids) =
                      mk_bbs_nested_stmts_consist case_sel.body case_last_ids
                    in
                    (acc_ids @ body_last_ids)
                  end)
          in

          (* Create basic blocks for [else_stmt]. *)
          let (_, else_last_ids) = mk_bbs_nested_stmts_consist else_stmts cond_last_ids in

          (* Set direct jump from the CASE condition if there are no else (default) statements. *)
          let cond_last_ids =
            match else_stmts with
            | [] -> cond_last_ids
            | _ -> []
          in
          (cond_last_ids @ cs_last_ids @ else_last_ids)
        end
      | S.StmFor (_, ctrl, body_stmts) ->
        begin
          (* Id of the basic block created for FOR statement by [fill_bbs_map_aux]. *)
          let for_bb_id = List.nth_exn bbs_pred_ids 0 in

          (* Create basic blocks for the control variable assignment statement. *)
          let (_, ctrl_last_ids) = mk_bbs_nested_stmts_consist [ctrl.assign] bbs_pred_ids in
          assert (phys_equal 1 (List.length ctrl_last_ids)); (* always single assignment stmt *)

          (* Create basic blocks for [body_stmts]. *)
          (* TODO: What about EXIT statements? *)
          let (_, body_last_ids) = mk_bbs_nested_stmts_consist body_stmts ctrl_last_ids in

          (* Link the last body statements with a FOR control statement. *)
          (* link_preds_by_id for_bb_id body_last_ids; *)

          (bbs_pred_ids)
        end
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
      | _ -> (bbs_pred_ids) (* TODO: Need test previous statements first. *)
    in
    match stmts with
    | [] -> begin
        match bbs_pred_ids with
        | [] (* empty map *) -> ()
        | _ -> begin
            (* Set block type type of the last BBs. *)
            BBMap.iter
              cfg.bbs_map
              (fun bb -> begin
                   if List.exists bbs_pred_ids ~f:(fun id -> phys_equal id bb.id) then
                     bb.ty <- BBExit;
                 end)
          end
      end
    | [s] -> begin
        let bb = match bbs_pred_ids with
          (* Only one BB in the map. *)
          | [] -> mk_bb BBEntry s
          (* Create nested BBs for the last statement. *)
          | _ -> let bb = mk_bb BB s in link_preds bb bbs_pred_ids; bb
        in
        cfg.bbs_map <- BBMap.set cfg.bbs_map bb;
        let n_bbs_last_ids = mk_nested_bbs s [bb.id] in
        fill_bbs_map_aux [] n_bbs_last_ids
      end
    | s :: stail -> begin
        let bb = match bbs_pred_ids with
          | [] (* first BB *)   -> mk_bb BBEntry s
          | _  (* regular BB *) -> let bb = mk_bb BB s in link_preds bb bbs_pred_ids; bb
        in
        cfg.bbs_map <- BBMap.set cfg.bbs_map bb;
        let n_bbs_last_ids = mk_nested_bbs s [bb.id] in
        fill_bbs_map_aux stail n_bbs_last_ids
      end
  in
  fill_bbs_map_aux stmts []

let mk iec_element =
  let cfg = empty_cfg () in
  let cfg = { cfg with pou_id = (S.get_pou_id iec_element) } in
  let stmts = AU.get_top_stmts iec_element in
  fill_bbs_map cfg stmts;
  let entry_bb = BBMap.first cfg.bbs_map in
  let entry_bb_id = match entry_bb with
    | Some bb -> bb.id
    | None -> -1
  in
  let cfg = { cfg with entry_bb_id = entry_bb_id } in
  cfg

let get_pou_id c = c.pou_id

let list_basic_blocks cfg =
  BBMap.to_alist cfg.bbs_map
  |> List.fold_left
    ~init:[]
    ~f:(fun acc i -> match i with (_, bb) -> acc @ [bb])

let bb_by_id cfg (id : int) =
  BBMap.find cfg.bbs_map id

let to_string (cfg : t) : string =
  BBMap.to_alist cfg.bbs_map
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
  let m = c.bbs_map in
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
