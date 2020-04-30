open Core_kernel

module S = Syntax
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
    stmt : S.statement; [@opaque]
    pou : S.iec_library_element; [@opaque] (** The POU that this BB belongs to *)
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
    "pou_id", `Int(S.get_pou_id bb.pou);
  ]

(** Map for basic blocks in CFG accessible by unique identifier *)
module BBMap = struct
  type t = (int, bb, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)

  (* let lookup m id = Map.find m id *)

  let add m bb = Map.set m ~key:bb.id ~data:bb

  let to_alist m = Map.to_alist m

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
  mutable init_bb_id : int; (** Id of the initial basic block *)
}

(** Generate unique id *)
let mk_id =
  let n = ref (-1) in
  fun () -> incr n; !n

(** Create basic block instance from a statement *)
let mk_bb pou ty stmt =
  let id = mk_id () in
  let preds = [] in
  let succs = [] in
  let pou = pou in
  { id; ty; preds; succs; stmt; pou }

(** Bound two nodes of CFG with an edge. *)
let bound_blocks prec suc =
  prec.succs <- (List.append prec.succs [suc.id]);
  suc.preds <- (List.append suc.preds [prec.id]);
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
    bound_blocks bb_parent bb;
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
      [(mk_parent stmt iec_element bb_parent BBExit)]
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
          if phys_equal i 0 then BBEntry
          else if phys_equal i (List.length stmts) then BBExit
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
    "initial_bb_id", `Int (c.init_bb_id);
    "basic_blocks", BBMap.to_yojson m;
  ]

let bb_get_ti bb =
  S.stmt_get_ti bb.stmt

let create_cfgs elements =
  List.fold_left
    elements
    ~f:(fun cfgs e -> (mk e) :: cfgs)
    ~init:[]
