open Core_kernel
open IECCheckerCore

module TI = Tok_info
module AU = Ast_util
module S = Syntax

(** Generate warning for a given basic block *)
let mk_warn ?(title="UnreachableCode") ?(text="Code block will never be reached") (bb : Cfg.bb) : Warn.t =
  let ti = Cfg.bb_get_ti bb in
  Warn.mk ti.linenr ti.col title text

(** Find top statements that are unreachable after RETURN keyword in the
    functions and function blocks. *)
let find_unreachable_top_statements (elements: S.iec_library_element list) : (Warn.t list) =
  let handle_return stmts : (Warn.t list) =
    let rec get_first_unreachable elem stmts found =
      match stmts with
      | [] | [_] -> elem
      | s :: stail -> begin
          if found then
            get_first_unreachable (Some(s)) [] true
          else
            match s with
            | S.StmReturn _ -> get_first_unreachable None stail true
            | _ -> get_first_unreachable None stail found
        end
    in
    match stmts with
    | [] | [_] -> []
    | _ -> begin
        let first_unreachable = get_first_unreachable None stmts false in
        match first_unreachable with
        | Some stmt -> begin
            let ti = S.stmt_get_ti stmt in
            [(Warn.mk ti.linenr ti.col "UnreachableCode" "Code block will never be reached")]
          end
        | None -> []
      end
  in
  List.fold_left
    elements
    ~init:[]
    ~f:(fun warns elem -> begin
          match elem with
          | S.IECFunction _ | S.IECFunctionBlock _ ->
            warns @ (handle_return (AU.get_top_stmts elem))
          | _ -> []
        end)

(** Find basic blocks inside the loop statements that are unreachable after
    CONTINUE/EXIT blocks. *)
let find_unreachable_blocks (cfgs : Cfg.t list) : (Warn.t list) =
  let check_cfg (cfg : Cfg.t) : (Warn.t list) =
    let module IntSet = Set.Make(Int) in

    let reachable_set = IntSet.of_list (Cfg.get_reachable_ids cfg)
    and all_set = IntSet.of_list (Cfg.get_all_ids cfg) in
    let unreachable_set = IntSet.diff all_set reachable_set in

    Set.fold
      unreachable_set
      ~init:[]
      ~f:(fun acc id -> begin
            let bb = Cfg.get_bb_by_id_exn cfg id in
            (* Add blocks without previous nodes in CFG. *)
            match bb.preds with
            | [] -> acc @ [(mk_warn bb)]
            | _ -> acc
          end)
  in
  List.fold_left
    cfgs
    ~init:[]
    ~f:(fun warns c -> warns @ (check_cfg c))

let run elements (cfgs : Cfg.t list) : Warn.t list =
  (* List.iter cfgs ~f:(fun c -> Printf.printf "%s\n" (Cfg.to_string c)); *)
  (find_unreachable_top_statements elements) @
  (find_unreachable_blocks cfgs)
