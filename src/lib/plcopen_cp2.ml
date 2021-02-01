open Core_kernel
open IECCheckerCore

module TI = Tok_info
module AU = Ast_util
module S = Syntax

(** Generate warning for a given basic block *)
let mk_warn (bb : Cfg.bb) : Warn.t =
  let ti = Cfg.bb_get_ti bb in
  Warn.mk ti.linenr ti.col "PLCOPEN-CP2" "All code shall be used in the application"

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

let do_check (cfgs : Cfg.t list) : Warn.t list =
  (* List.iter cfgs ~f:(fun c -> Printf.printf "%s\n" (Cfg.to_string c)); *)
  (find_unreachable_blocks cfgs)
