open Core_kernel

module S = Syntax
module AU = Ast_util

[@@@warning "-26"]

type bb_ty =
  | BB (** Regular basic block *)
  | BBInit (** Initial basic block *)
  | BBOut (** Return/exit node *)
  | BBJump (** Indirect jump to/from a node *)

and bb =
  {
    id : int;
    ty: bb_ty;
    mutable in_edges : edge list; (** Incoming edges *)
    mutable out_edges : edge list; (** Outcoming edges *)
    stmt : S.statement;
    pou : S.iec_library_element; (** The POU that this BB belongs to *)
  }

and edge =
  {
    in_bb : bb; (** Input basic block *)
    out_bb : bb; (** Output basic block *)
  }

(** Map for basic blocks in CFG accessible by unique identifier *)
module BBMap = struct
  type t = (int, bb, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)

  (* let lookup m id = Map.find m id *)

  let add m bb = Map.set m ~key:bb.id ~data:bb

  let to_alist m = Map.to_alist m
end

type t = {
  mutable bb_map : BBMap.t; (** Map of basic blocks *)
  mutable init_bb_id : int; (** Id of the initial basic block *)
}

(** Generate unique id *)
let mk_id =
  let n = ref (-1) in
  fun () -> incr n; !n

(** Create basic block instance from a statement *)
let mk_bb pou ty stmt =
  let id = mk_id () in
  let in_edges = [] in
  let out_edges = [] in
  let pou = pou in
  { id; ty; in_edges; out_edges; stmt; pou }

(** Add edge between two basic blocks *)
let mk_edge bb_in bb_out =
  let set_edges bb e_in e_out =
    bb.in_edges <- (List.append bb.in_edges [e_in]);
    bb.out_edges <- (List.append bb.out_edges [e_out]);
    bb
  in
  let e_in = { in_bb = bb_in; out_bb = bb_out } in
  let e_out = { in_bb = bb_out; out_bb = bb_in } in
  ((set_edges bb_in e_in e_out), (set_edges bb_out e_out e_in))

(** Insert basic block in a given CFG *)
let cfg_add_bb bb_map bb =
  let m = BBMap.add bb_map bb in m

(** Create edges between basic blocks in a given map *)
  let[@warning "-32"] cfg_create_edges bb_map =
  let map_add_edges m bb_list bb =
    let update_bb m bb =
      let map m =
        match bb.ty with
        | BB       -> Map.update m bb.id ~f:(fun _ -> Some bb)
        | BBInit   -> Map.update m bb.id ~f:(fun _ -> Some bb)
        | BBJump   -> Map.update m bb.id ~f:(fun _ -> Some bb)
        | BBOut    -> m
      in map m
    in
    let bb_prev =
      match bb_list with
      | [] -> None
      | [bb_first] -> Some bb_first
      | bb_head :: _ -> Some bb_head
    in
    match bb_prev with
    | None -> bb_map
    | Some bbp ->
      begin
        let (bb, bbp) = mk_edge bb bbp in
        (* Update the map *)
        let bb_map = update_bb m bbp in
        update_bb m bb
      end
  in
  let bb_list bb_map =
    let alist = BBMap.to_alist bb_map in
    List.fold_left alist
      ~f:(fun bbs bb -> let (_, bb_v) = bb in bb_v :: bbs)
      ~init:[]
  in
  []
  (* List.fold bb_list                         *)
  (*   ~f:(fun m bb -> map_add_edges m bbs bb) *)
  (*   ~init:m                                 *)

let empty_cfg () =
  let bb_map = BBMap.empty in
  let init_bb_id = 0 in
  { bb_map; init_bb_id }

(** Recursively create a list of basic blocks for a given statement and nested statements *)
let rec create_bbs iec_element stmt =
  (** Create list of BBs from the nested statements found in expression *)
  let expr_to_bbs expr =
    let res =
      List.fold_left (AU.expr_to_stmts expr)
        ~f:(fun bbs s -> bbs @ (create_bbs iec_element s))
        ~init:[]
    in res
  in
  (** Create list of BBs from a list of statements *)
  let stmts_to_bbs stmts =
    List.fold_left stmts ~f:(fun bbs s -> bbs @ (create_bbs iec_element s)) ~init:[]
  in
  match stmt with
  | S.StmAssign (_, _, e) ->
    begin
      (mk_bb iec_element BB stmt) ::
      (expr_to_bbs e)
    end
  | S.StmElsif (_, e, stmts) ->
    begin
      (mk_bb iec_element BB stmt) ::
      (expr_to_bbs e) @ (stmts_to_bbs stmts)
    end
  | S.StmIf (_, e, stmts_body, stmts_elsif, stmts_else) ->
    begin
      (mk_bb iec_element BB stmt) ::
      (expr_to_bbs e) @
      (stmts_to_bbs stmts_body) @
      (stmts_to_bbs stmts_elsif) @
      (stmts_to_bbs stmts_else)
    end
  | S.StmCase (_, e, cs_list, stmts_else) ->
    begin
      let case_bbs =
        List.fold_left cs_list
          ~f:(fun bbs cs ->
              begin
                bbs @
                (* case expressions *)
                (List.fold_left cs.S.case
                   ~f:(fun bbs ec -> bbs @ (expr_to_bbs ec))
                   ~init:[]) @
                (* case statements *)
                (stmts_to_bbs cs.S.body)
              end)
          ~init:[]
      in
      (mk_bb iec_element BB stmt) ::
      (expr_to_bbs e) @ case_bbs @ (stmts_to_bbs stmts_else)
    end
  | S.StmFor (_, _, e_start, e_end, e_step, stmts_body) ->
    begin
      let step_bbs = match e_step with
        | Some e -> (expr_to_bbs e)
        | None -> []
      in
      (mk_bb iec_element BB stmt) ::
      (expr_to_bbs e_start) @
      (expr_to_bbs e_end) @
      step_bbs @
      (stmts_to_bbs stmts_body)
    end
  | S.StmWhile (_, e, stmts_body) ->
    begin
      (mk_bb iec_element BB stmt) ::
      (expr_to_bbs e) @ (stmts_to_bbs stmts_body)
    end
  | S.StmRepeat (_, stmts, e) ->
    begin
      (mk_bb iec_element BB stmt) ::
      (stmts_to_bbs stmts) @ (expr_to_bbs e)
    end
  | S.StmExit _ ->
    begin
      [(mk_bb iec_element BBOut stmt)]
    end
  | S.StmContinue _ ->
    begin
      [(mk_bb iec_element BB stmt)]
    end
  | S.StmFuncParamAssign (_, e, _) ->
    begin
      (mk_bb iec_element BB stmt) ::
      (expr_to_bbs e)
    end
  | S.StmFuncCall (_, _, stmts_body) ->
    begin
      (mk_bb iec_element BB stmt) ::
      (stmts_to_bbs stmts_body)
    end

let mk iec_element =
  let cfg = empty_cfg () in
  let stmts = AU.get_top_stmts iec_element in
  List.iteri ~f:(fun i stmt ->
      begin
        (* Detect block type *)
        let ty =
          if phys_equal i 0 then BBInit
          else if phys_equal i (List.length stmts) then BBOut
          else BB
        in
        (* Create BB for a top-level statement *)
        let bb_top = mk_bb iec_element ty stmt in
        (* Create BBs for a nested statements *)
        let bbs = bb_top :: (create_bbs iec_element stmt) in
        (* Update basic blocks map *)
        List.iter bbs ~f:(fun bb -> cfg.bb_map <- cfg_add_bb cfg.bb_map bb);
        (* Set initial id *)
        if phys_equal i 0 then
          cfg.init_bb_id <- bb_top.id;
        ()
      end) stmts;
  (* Create edges for the basic blocks *)
  (* cfg.bb_map <- cfg_create_edges cfg.bb_map; *)
  cfg
