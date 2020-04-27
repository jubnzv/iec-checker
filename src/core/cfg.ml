open Core_kernel

module S = Syntax
module AU = Ast_util
module TI = Tok_info

[@@@warning "-32"]

type bb_ty =
  | BB (** Regular basic block *)
  | BBInit (** Initial basic block *)
  | BBOut (** Return/exit node *)
  | BBJump (** Indirect jump to/from a node *)
[@@deriving show { with_path = false }]

and bb =
  {
    id : int;
    ty: bb_ty;
    mutable in_edges : edge list; (** Incoming edges *)
    mutable out_edges : edge list; (** Outcoming edges *)
    stmt : S.statement; [@opaque]
    pou : S.iec_library_element; [@opaque] (** The POU that this BB belongs to *)
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
    ()
  in
  let e_in = { in_bb = bb_in; out_bb = bb_out } in
  let e_out = { in_bb = bb_out; out_bb = bb_in } in
  (set_edges bb_in e_in e_out);
  (set_edges bb_out e_out e_in);
  ()

(** Insert basic block in a given CFG *)
let cfg_add_bb bb_map bb =
  let m = BBMap.add bb_map bb in m

let empty_cfg () =
  let bb_map = BBMap.empty in
  let init_bb_id = 0 in
  { bb_map; init_bb_id }

(** Recursively create a list of basic blocks for a given statement and the
    nested statements. *)
let rec create_bbs iec_element stmt bb_parent =
  (** Create list of BBs from the nested statements found in expression *)
  let expr_to_bbs expr bb_parent =
    let res =
      List.fold_left (AU.expr_to_stmts expr)
        ~f:(fun bbs s -> bbs @ (create_bbs iec_element s bb_parent))
        ~init:[]
    in res
  in
  (** Create list of BBs from a list of statements *)
  let stmts_to_bbs stmts bb_parent =
    List.fold_left stmts ~f:(fun bbs s -> bbs @ (create_bbs iec_element s bb_parent)) ~init:[]
  in
  (** Create parent block for a given statement *)
  let mk_parent stmt iec_element bb_parent ty =
    let bb = mk_bb iec_element ty stmt in
    mk_edge bb_parent bb;
    bb
  in
  match stmt with
  | S.StmAssign (_, _, e) ->
    begin
      let bb_stm_parent = mk_parent stmt iec_element bb_parent BB in
      bb_stm_parent :: (expr_to_bbs e bb_stm_parent)
    end
  | S.StmElsif (_, e, stmts) ->
    begin
      let bb_stm_parent = mk_parent stmt iec_element bb_parent BB in
      bb_stm_parent :: (expr_to_bbs e bb_stm_parent) @ (stmts_to_bbs stmts bb_stm_parent)
    end
  | S.StmIf (_, e, stmts_body, stmts_elsif, stmts_else) ->
    begin
      let bb_stm_parent = mk_parent stmt iec_element bb_parent BB in
      bb_stm_parent ::
      (expr_to_bbs e bb_stm_parent) @
      (stmts_to_bbs stmts_body bb_stm_parent) @
      (stmts_to_bbs stmts_elsif bb_stm_parent) @
      (stmts_to_bbs stmts_else bb_stm_parent)
    end
  | S.StmCase (_, e, cs_list, stmts_else) ->
    begin
      let get_case_bbs bb_stm_parent =
        List.fold_left cs_list
          ~f:(fun bbs cs ->
              begin
                bbs @
                (* case expressions *)
                (List.fold_left cs.S.case
                   ~f:(fun bbs ec -> bbs @ (expr_to_bbs ec bb_stm_parent))
                   ~init:[]) @
                (* case statements *)
                (stmts_to_bbs cs.S.body bb_stm_parent)
              end)
          ~init:[]
      in
      let bb_stm_parent = mk_parent stmt iec_element bb_parent BB in
      bb_stm_parent ::
      (expr_to_bbs e bb_stm_parent) @
      (get_case_bbs bb_stm_parent) @
      (stmts_to_bbs stmts_else bb_stm_parent)
    end
  | S.StmFor (_, _, e_start, e_end, e_step, stmts_body) ->
    begin
      let get_step_bbs bb_stm_parent = match e_step with
        | Some e -> (expr_to_bbs e bb_stm_parent)
        | None -> []
      in
      let bb_stm_parent = mk_parent stmt iec_element bb_parent BB in
      bb_stm_parent ::
      (expr_to_bbs e_start bb_stm_parent) @
      (expr_to_bbs e_end bb_stm_parent) @
      (get_step_bbs bb_stm_parent) @
      (stmts_to_bbs stmts_body bb_stm_parent)
    end
  | S.StmWhile (_, e, stmts_body) ->
    begin
      let bb_stm_parent = mk_parent stmt iec_element bb_parent BB in
      bb_stm_parent ::
      (expr_to_bbs e bb_stm_parent) @
      (stmts_to_bbs stmts_body bb_stm_parent)
    end
  | S.StmRepeat (_, stmts, e) ->
    begin
      let bb_stm_parent = mk_parent stmt iec_element bb_parent BB in
      bb_stm_parent ::
      (stmts_to_bbs stmts bb_stm_parent) @
      (expr_to_bbs e bb_stm_parent)
    end
  | S.StmExit _
  | S.StmReturn _ ->
    begin
      [(mk_parent stmt iec_element bb_parent BBOut)]
    end
  | S.StmContinue _ ->
    begin
      (* TODO: Add jump edge *)
      [(mk_parent stmt iec_element bb_parent BBJump)]
    end
  | S.StmFuncParamAssign (_, e, _) ->
    begin
      let bb_stm_parent = mk_parent stmt iec_element bb_parent BB in
      bb_stm_parent ::
      (expr_to_bbs e bb_stm_parent)
    end
  | S.StmFuncCall (_, _, stmts_body) ->
    begin
      let bb_stm_parent = mk_parent stmt iec_element bb_parent BB in
      bb_stm_parent ::
      (stmts_to_bbs stmts_body bb_stm_parent)
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
        let bbs = bb_top :: (create_bbs iec_element stmt bb_top) in
        (* Update basic blocks map *)
        List.iter bbs ~f:(fun bb -> cfg.bb_map <- cfg_add_bb cfg.bb_map bb);
        (* Set initial id *)
        if phys_equal i 0 then
          cfg.init_bb_id <- bb_top.id;
        ()
      end) stmts;
  cfg

let to_string (cfg : t) : string =
  BBMap.to_alist cfg.bb_map
  |> List.fold_left
    ~init:[]
    ~f:(fun acc i -> match i with (id, bb) ->
        begin
          let edges_to_string (edges : edge list) =
            List.fold_left edges
              ~init:[]
              ~f:(fun acc e -> acc @ [Printf.sprintf "%d->%d" e.in_bb.id e.out_bb.id])
            |> String.concat ~sep:" "
          in
          let bb_repr =
            Printf.sprintf "[%03d %6s] [in_edges: %s] [out_edges: %s]"
              id
              (show_bb_ty bb.ty)
              (edges_to_string bb.in_edges)
              (edges_to_string bb.out_edges)
          in
          acc @ [bb_repr]
        end)
  |> String.concat ~sep:"\n"

let bb_get_ti bb =
  S.stmt_get_ti bb.stmt

let create_cfgs elements =
  List.fold_left
    elements
    ~f:(fun cfgs e -> (mk e) :: cfgs)
    ~init:[]
