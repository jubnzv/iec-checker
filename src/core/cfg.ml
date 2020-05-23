open Core_kernel
open Common

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
    mutable ty: bb_ty;
    mutable preds : int list; (** Ids of predecessor nodes *)
    mutable succs : int list; (** Ids of successor nodes *)
    (* TODO: This should be replaced with ids of statements. But it will
       require additional symbol tables and a lot of refactoring in the parser. *)
    mutable stmts : S.statement list [@opaque]; (** Statements that makes up this BB *)
  }

let bb_to_yojson bb =
  let yojson_ints (ids : int list) : Yojson.Safe.t list =
    List.fold_left ids
      ~init:[]
      ~f:(fun acc i -> acc @ [`Int(i)])
  in
  let stmt_ids : Yojson.Safe.t list =
    List.fold_left
      bb.stmts
      ~init:[]
      ~f:(fun acc stmt -> acc @ [`Int(S.stmt_get_id stmt)])
  in
  `Assoc [
    "id", `Int(bb.id);
    "type", bb_ty_to_yojson bb.ty;
    "preds", `List(yojson_ints bb.preds);
    "succs", `List(yojson_ints bb.succs);
    "stmt_ids", `List(stmt_ids);
  ]

(** Map for basic blocks in CFG accessible by unique identifier *)
module BBMap = struct
  type t = (int, bb, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)

  let set m bb = Map.set m ~key:bb.id ~data:bb

  let to_alist m = Map.to_alist m

  let ids m = List.fold_left
      (Map.to_alist m)
      ~init:[]
      ~f:(fun acc (k, _) -> acc @ [k])

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

module IntStack = struct
  let create () = Stack.create ()
  let push s v = Stack.push s v
  let pop s = Stack.pop s
  let top s : (int option) = Stack.top s
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
  let stmts = [stmt] in
  { id; ty; preds; succs; stmts }

let empty_cfg () =
  let bbs_map = BBMap.empty in
  let entry_bb_id = -1 in
  let pou_id = -1 in
  { bbs_map; entry_bb_id; pou_id }

(** [fill_bbs_map cfg stmts] Fill [cfg.map] with a linked basic blocks for
    [stmts] and their nested statements. *)
let fill_bbs_map (cfg : t) (stmts : S.statement list) : (unit) =
  let (@) = append_tr in (* Use tail-recursive append because we process the large lists here *)
  (** Recursively traverse over [stmts] to create basic blocks for each
       statement, including the nested ones.

      @param bbs_pred_ids is a list of identifiers of basic blocks from the
      previous iteration. They will be linked with the block created in the
      current iteration.

      @param previous_bb_id optional identifier of basic block created in
      previous function call. It helps handle linear sequence of statements on top
      level of POU. *)
  let rec fill_bbs_map_aux stmts (bbs_pred_ids : int list) (previous_bb_id : int option) : (unit) =
    (** Link blocks from the [bbs_map] with id from [ids] with a [next_bb] block.
        Links are represented as mutable [succs] and [preds] fields, so we
        return nothing.

        TODO: Unique constraint check.
        Need to figure out how to use sets instead lists for keeping links.
    *)
    (* Stack that keeps IDs of basic blocks for control statments (FOR, WHILE,
       REPEAT) to link them with corresponding CONTNUE statements in the loop
       body. *)
    let loop_ctrl_stack = IntStack.create () in
    (* Stack that keeps IDs of EXIT basic blocks found in body of control
       statements. *)
    let loop_exit_stack = IntStack.create () in
    let link_preds (next_bb : bb) (pred_ids : int list) : (unit) =
      (* TODO: This is horrible. I should use LRU cache (memoization) here and
         make this operation short-circuiting. *)
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
    (** Process a statement [stmt] and its nested statements to create new
        basic blocks.

        @param created_bb is a basic block created by [fill_bbs_map_aux]
        before calling this function. It contains single top-level statement, and
        this function will extend it with the nested statements and create consecutive
        basic blocks if required.

        First created BB will be linked with blocks from [bbs_pred_ids].

        @return List of IDs of the last basic blocks and optional identifier of
        basic block to link it with next consecutive statement on top-level. *)
    let rec mk_nested_bbs (stmt : S.statement) (created_bb : bb) (bbs_pred_ids : int list) : (int list * int option) =
      (** [mk_body_bbs stmts ] Create a list of the basic blocks for the
          body of control statement starting from first elemenet of [stmts].

          @param bbs_pred_ids List of identifiers for basic blocks that will be
          linked with a BB created for the first statement.

          @return List of identifiers of the last basic blocks. *)
      let mk_body_bbs stmts (bbs_pred_ids : int list) : (int list) =
        let handle_exit_continue bb last_ids stmt =
          match stmt with
          | S.StmExit _ -> begin
              (* EXIT basic blocks always will be linked with the block after
                 the loop. The next statements in this loop will be
                 unreachable. *)
              IntStack.push loop_exit_stack bb.id;
              []
            end
          | S.StmContinue _ -> begin
              (* Link CONTINUE basic blocks with loop control BB. *)
              match IntStack.top loop_ctrl_stack with
              | Some ctrl_bb_id -> begin
                  link_preds_by_id ctrl_bb_id [bb.id];
                  (* Don't link subsequent block with CONTINUE. *)
                  []
                end
              | None (* semantic error: not in the loop *) -> []
            end
          | _ -> last_ids
        in
        (** Create a new BB for the first body statement. *)
        let mk_first_bb (stmt : S.statement) : (int list * int option * S.statement list) =
          let first_bb = mk_bb BB stmt in
          cfg.bbs_map <- BBMap.set cfg.bbs_map first_bb;
          link_preds first_bb bbs_pred_ids;

          let (first_bb_last_ids, first_bb_id) = (mk_nested_bbs stmt first_bb [first_bb.id])
          and stmts_tail = List.tl_exn stmts in

          (* Link EXIT/CONTINUE statement properely. *)
          let first_bb_last_ids = handle_exit_continue first_bb first_bb_last_ids stmt in

          (* FIXME: How to pass multiple arguments inside >>| monad? *)
          (first_bb_last_ids, first_bb_id, stmts_tail)
        in
        (** Create BBs for the tail of body statements. *)
        let mk_body_bbs_tail (invals : (int list * int option * S.statement list)) : (int list) =
          let (first_bb_last_ids, first_bb_id, stmts_tail) = invals in
          let (last_ids, _) = List.fold_left
              stmts_tail
              ~init:(first_bb_last_ids, first_bb_id)
              ~f:(fun (acc_bb_last_ids, acc_bb_previous_id_opt) stmt -> begin
                    let (bb, acc_bb_last_ids) =
                      match acc_bb_previous_id_opt with
                      | Some id -> begin
                          (* Use previously created BB. *)
                          let bb = BBMap.find_exn cfg.bbs_map id in
                          (bb, acc_bb_last_ids)
                        end
                      | None -> begin
                          (* We need a new BB. *)
                          let bb = mk_bb BB stmt in
                          link_preds bb acc_bb_last_ids;
                          cfg.bbs_map <- BBMap.set cfg.bbs_map bb;
                          (bb, [bb.id])
                        end
                    in
                    (* Save current statement in BB. *)
                    bb.stmts <- bb.stmts @ [stmt];
                    (* Process nested statements. *)
                    let (upd_last_bb_ids, upd_previous_id_opt) =
                      mk_nested_bbs stmt bb acc_bb_last_ids
                    in
                    (* Link EXIT/CONTINUE statement properely. *)
                    let upd_last_bb_ids  = handle_exit_continue bb upd_last_bb_ids stmt in
                    (upd_last_bb_ids, upd_previous_id_opt)
                  end)
          in
          last_ids
        in
        List.nth stmts 0
        >>| mk_first_bb
        >>| mk_body_bbs_tail
        |> unwrap_list
      in
      (** [mk_cond_bbs cond_stmt] Process condition statement: link condition
          with control basic block and add nested statements (if exists). *)
      let mk_cond_bbs ?(pred_ids=bbs_pred_ids) cond_stmt =
        (* Link [cond_stmt] to [created_bb]. *)
        created_bb.stmts <- created_bb.stmts @ [cond_stmt];
        (* Link statements nested in condition statement. *)
        let (cond_last_ids, _) = mk_nested_bbs cond_stmt created_bb pred_ids in
        (cond_last_ids)
      in
      match stmt with
      | S.StmExpr (_, expr) ->
        begin
          let rec link_func_calls_stmts = function
            (* Nested statements can not be used here. *)
            | S.ExprFuncCall (_, stmt) -> begin
                created_bb.stmts <- created_bb.stmts @ [stmt];
                ()
              end
            | S.ExprVariable _ -> ()
            | S.ExprConstant _ -> ()
            | S.ExprBin (_,e1,_,e2) -> begin
                link_func_calls_stmts e1 |> ignore;
                link_func_calls_stmts e2 |> ignore;
                ()
              end
            | S.ExprUn (_,_,e) -> ignore @@ link_func_calls_stmts e
          in
          link_func_calls_stmts expr |> ignore;
          (bbs_pred_ids, Some(created_bb.id))
        end
      | S.StmElsif (_, cond_stmt, body_stmts) ->
        begin
          (* Process [cond_stmt]. *)
          let cond_last_ids = mk_cond_bbs cond_stmt in

          (* Create basic blocks for [body_stmts]. *)
          let body_last_ids = mk_body_bbs body_stmts cond_last_ids in

          (cond_last_ids @ body_last_ids, None)
        end
      | S.StmIf (_, cond_stmt, body_stmts, elsif_stmts, else_stmts) ->
        begin
          (* Process [cond_stmt]. *)
          let cond_last_ids = mk_cond_bbs cond_stmt in

          (* Create basic blocks for [body_stmts]. *)
          let body_last_ids = mk_body_bbs body_stmts cond_last_ids in

          (* Create basic blocks for [elsif_stmts]. *)
          let (elsif_last_ids, elsif_last_bb_ids) =
            List.fold_left
              elsif_stmts
              ~init:([], cond_last_ids)
              ~f:(fun (acc, acc_last) stmt -> begin
                    (* Create basic block for each ELSIF statement (including nested ones). *)
                    let last_ids = mk_body_bbs [stmt] acc_last in
                    (* First created basic block contains ELSIF statement with condition expr.
                       It will be linked with the next ELSIF statement. *)
                    let elsif_bb_id = List.nth_exn last_ids 0 in
                    (* Other BBs should be linked with statement after ELSIF. *)
                    let elsif_last_bbs = sublist last_ids 1 (List.length last_ids) in
                    ((acc @ elsif_last_bbs), ([elsif_bb_id]))
                  end)
          in

          (* Create basic blocks for [else_stmt]. *)
          let else_last_ids = mk_body_bbs else_stmts cond_last_ids in

          (* Set direct jump from the IF or ELSIF condition if there are no ELSE statements. *)
          let cond_last_ids =
            match else_stmts with
            | [] -> begin
                match elsif_last_ids with
                | [] -> cond_last_ids
                | _ -> (* Set jump from last ELSIF bb *) elsif_last_bb_ids
              end
            | _ -> []
          in

          ((cond_last_ids @ body_last_ids @ elsif_last_ids @ else_last_ids), None)
        end
      | S.StmCase (_, cond_stmt, case_sels, else_stmts) ->
        begin
          (* Process [cond_stmt]. *)
          let cond_last_ids = mk_cond_bbs cond_stmt in

          (* Create basic blocks for [case_sels] statements. *)
          let (cs_last_ids, last_case_ids) = List.fold_left
              case_sels
              ~init:([], cond_last_ids)
              ~f:(fun (acc_ids, acc_last_ids) case_sel -> begin
                    (* There are cases when we have multiple case selections.
                       For example:
                           CASE 3,4 : a := 19;
                       This means that we want to link each of case selection
                       statements with appropriate body statement:
                           3 <-> a := 19;
                           4 <-> a := 19; *)
                    let case_last_ids =
                      (* Each case selection will be linked with the previous case selection. *)
                      mk_body_bbs case_sel.case acc_last_ids
                    in
                    let body_last_ids =
                      mk_body_bbs case_sel.body case_last_ids
                    in
                    (acc_ids @ body_last_ids, case_last_ids)
                  end)
          in

          (* Create basic blocks for [else_stmt]. *)
          let else_last_ids = mk_body_bbs else_stmts last_case_ids in

          (* Set direct jump from the last CASE if there are no ELSE (default) statements. *)
          let cond_last_ids =
            match else_stmts with
            | [] -> last_case_ids
            | _ -> []
          in

          (cond_last_ids @ cs_last_ids @ else_last_ids, None)
        end
      | S.StmFor (_, ctrl, body_stmts) ->
        begin
          (* Id of the basic block created for FOR statement by [fill_bbs_map_aux]. *)
          let for_bb_id = List.nth_exn bbs_pred_ids 0 in
          assert (phys_equal 1 (List.length bbs_pred_ids));

          (* Keep ID of the control BB to handle CONTINUE blocks. *)
          IntStack.push loop_ctrl_stack for_bb_id;

          (* Create basic blocks for the control variable assignment statement. *)
          let ctrl_last_ids = mk_cond_bbs ctrl.assign in
          assert (phys_equal 1 (List.length ctrl_last_ids)); (* always single assignment stmt *)

          (* Create basic blocks for [body_stmts]. *)
          let body_last_ids = mk_body_bbs body_stmts ctrl_last_ids in

          (* Link the last body statements with a FOR control statement. *)
          link_preds_by_id for_bb_id body_last_ids;

          let _ = IntStack.pop loop_ctrl_stack in

          (* Link found EXIT statements with the next blocks. *)
          let ret_last_ids = match IntStack.pop loop_exit_stack with
            | Some id ->  [for_bb_id; id]
            | None -> [for_bb_id]
          in

          (ret_last_ids, None)
        end
      | S.StmWhile (_, cond_stmt, body_stmts) ->
        begin
          (* Id of the basic block created for WHILE statement by [fill_bbs_map_aux]. *)
          let while_bb_id = List.nth_exn bbs_pred_ids 0 in
          assert (phys_equal 1 (List.length bbs_pred_ids));

          (* Keep ID of the control BB to handle CONTINUE blocks. *)
          IntStack.push loop_ctrl_stack while_bb_id;

          (* Create basic block for [cond_stmt]. *)
          let cond_last_ids = mk_cond_bbs cond_stmt in
          assert (phys_equal 1 (List.length cond_last_ids)); (* always single expression stmt *)

          (* Create basic blocks for [body_stmts]. *)
          let body_last_ids = mk_body_bbs body_stmts cond_last_ids in

          (* Link the last body statements with a WHILE control statement. *)
          link_preds_by_id while_bb_id body_last_ids;

          let _ = IntStack.pop loop_ctrl_stack in

          (* Link found EXIT statements with the next blocks. *)
          let ret_last_ids = match IntStack.pop loop_exit_stack with
            | Some id ->  [while_bb_id; id]
            | None -> [while_bb_id]
          in

          (ret_last_ids, None)
        end
      | S.StmRepeat (_, body_stmts, cond_stmt) ->
        begin
          (* Id of the basic block created for REPEAT statement by [fill_bbs_map_aux]. *)
          let repeat_bb_id = List.nth_exn bbs_pred_ids 0 in
          assert (phys_equal 1 (List.length bbs_pred_ids));

          (* Keep ID of the control BB to handle CONTINUE blocks. *)
          IntStack.push loop_ctrl_stack repeat_bb_id;

          (* Create basic blocks for [body_stmts]. *)
          let body_last_ids = mk_body_bbs body_stmts [repeat_bb_id] in

          (* Create basic block for [cond_stmt]. *)
          let cond_last_ids = mk_cond_bbs cond_stmt ~pred_ids:(body_last_ids) in
          assert (phys_equal 1 (List.length cond_last_ids)); (* always single expression stmt *)

          (* Link the condition statement with a REPEAT control statement. *)
          link_preds_by_id repeat_bb_id cond_last_ids;

          let _ = IntStack.pop loop_ctrl_stack in

          (* Link found EXIT statements with the next blocks. *)
          let ret_last_ids = match IntStack.pop loop_exit_stack with
            | Some id ->  cond_last_ids @ [id]
            | None -> cond_last_ids
          in
          (ret_last_ids, None)
        end
      | S.StmFuncCall (_, _, func_params) ->
        begin
          (* Id of the basic block created for function call statement by [fill_bbs_map_aux]. *)
          let funccall_bb_id = List.nth_exn bbs_pred_ids 0 in
          assert (phys_equal 1 (List.length bbs_pred_ids));

          let func_params_stmts = List.fold_left
              func_params
              ~init:[]
              ~f:(fun acc fb -> acc @ [fb.stmt])
          in

          (* Create basic blocks for function parameters statements. *)
          let func_param_assign_last_ids = mk_body_bbs func_params_stmts bbs_pred_ids in
          (* Link assigment BB for the last parameter with a function call BB. *)
          link_preds_by_id funccall_bb_id func_param_assign_last_ids;

          (bbs_pred_ids, None)
        end
      | S.StmReturn _ -> begin
          created_bb.ty <- BBExit;
          ([], None)
        end
      (* The implementation a bit tricky: we can encounter with these
         statements only inside the loops which are handled by [mk_body_bbs].
         So, if we got one of these statements at the top of POU this is a
         semantic error. We create a regular block in this case. *)
      | S.StmExit _ | S.StmContinue _ -> (bbs_pred_ids, None)
    in
    (* Create basic blocks for top-level statements. *)
    match stmts with
    | [] -> begin
        match bbs_pred_ids with
        | [] (* Empty CFG. *) -> ()
        | _ -> begin
            (* Set block type type for the last BBs. *)
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
          | [] -> begin
              (* Only one BB in this CFG. *)
              let bb = mk_bb BBEntry s in
              cfg.bbs_map <- BBMap.set cfg.bbs_map bb;
              bb
            end
          | _ -> begin
              (* Create nested BBs for the last statement. *)
              match previous_bb_id with
              | Some id -> begin
                  (* Link with previously created BB. *)
                  let bb = BBMap.find_exn cfg.bbs_map id in
                  bb
                end
              | None -> begin
                  (* Create new BB. *)
                  let bb = mk_bb BB s in
                  link_preds bb bbs_pred_ids;
                  cfg.bbs_map <- BBMap.set cfg.bbs_map bb;
                  bb
                end
            end
        in
        let (n_bbs_last_ids, n_previous_bb_id) = mk_nested_bbs s bb [bb.id] in
        fill_bbs_map_aux [] n_bbs_last_ids n_previous_bb_id
      end
    | s :: stail -> begin
        let bb = match bbs_pred_ids with
          | [] (* first BB *)   -> begin
              let bb = mk_bb BBEntry s in
              cfg.bbs_map <- BBMap.set cfg.bbs_map bb;
              bb
            end
          | _  (* regular BB *) -> begin
              match previous_bb_id with
              | Some id -> begin
                  (* Add this statement to a basic block created in the previous
                     [fill_bbs_map_aux] call. This is usual for linear sequence
                     of statements. *)
                  let bb = BBMap.find_exn cfg.bbs_map id in
                  bb
                end
              | None -> begin
                  (* We need a new basic block. *)
                  let bb = mk_bb BB s in
                  link_preds bb bbs_pred_ids;
                  cfg.bbs_map <- BBMap.set cfg.bbs_map bb;
                  bb
                end
            end
        in
        let (n_bbs_last_ids, n_previous_bb_id) = mk_nested_bbs s bb [bb.id] in
        fill_bbs_map_aux stail n_bbs_last_ids n_previous_bb_id
      end
  in
  fill_bbs_map_aux stmts [] None

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

let get_bb_by_id_exn cfg id =
  BBMap.find_exn cfg.bbs_map id

let get_all_ids cfg =
  BBMap.ids cfg.bbs_map

let get_reachable_ids cfg =
  (* There are no nodes in CFG. *)
  if (phys_equal cfg.entry_bb_id (-1)) then [] else
    let module IntSet = Set.Make(Int) in
    let visited = ref IntSet.empty in
    let rec dfs (bb_id : int) =
      let handle_node id =
        if not (IntSet.mem !visited id) then dfs id
      in
      let bb = (BBMap.find_exn cfg.bbs_map bb_id) in
      visited := IntSet.add !visited bb_id;
      List.iter bb.succs ~f:(fun id -> handle_node id);
      ()
    in
    dfs cfg.entry_bb_id;
    IntSet.to_list !visited

let get_number_of_edges cfg =
  List.fold_left
    (BBMap.ids cfg.bbs_map)
    ~init:0
    ~f:(fun acc id -> begin
          let bb = (BBMap.find_exn cfg.bbs_map id) in
          acc + (List.length bb.succs) + (List.length bb.preds)
        end)

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
  List.nth_exn bb.stmts 0
  |> S.stmt_get_ti

let create_cfgs elements =
  List.fold_left
    elements
    ~f:(fun cfgs e -> (mk e) :: cfgs)
    ~init:[]
