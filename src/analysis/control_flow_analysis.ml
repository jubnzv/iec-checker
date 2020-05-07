[@@@warning "-32"]
[@@@warning "-33"]
[@@@warning "-27"]

open Core_kernel
open IECCheckerCore

module TI = Tok_info

(** Generate warning for a given basic block *)
let mk_warn ?(title="CFA")(bb : Cfg.bb) : Warn.t =
  let ti = Cfg.bb_get_ti bb in
  Warn.mk ti.linenr ti.col title ("Code block will never be reached")

(** Find unreachable basic blocks (dead code) *)
(* TODO: Should be rewritten due endless recursion when traversing loops. *)
let find_unreachable_bbs (cfg: Cfg.t) : Warn.t list =
  let rec visit (cfg: Cfg.t) visited (bb : Cfg.bb) =
    let visited = Set.add visited bb.id in
    match bb.ty with
    | Cfg.BBEntry | Cfg.BB | Cfg.BBJump -> begin
        List.fold_left
          bb.succs
          ~init:(visited)
          ~f:(fun set succ_id -> begin
                match (Cfg.bb_by_id cfg succ_id) with
                | Some v -> visit cfg set v
                | None -> set
              end)
      end
    | Cfg.BBExit -> visited
  in
  let all_bbs = Cfg.list_basic_blocks cfg in
  if List.is_empty all_bbs then
    []
  else
    let rec find_first_unreachable visited all_bbs =
      let gen_warn visited (bb : Cfg.bb) : (Warn.t option) =
        match (Set.find visited ~f:(fun vis_id -> phys_equal vis_id bb.id)) with
        | Some _ -> None
        | None -> Some(mk_warn ~title:"UnreachableCode" bb)
      in
      match all_bbs with
      | [] -> []
      | bb :: bbs -> begin
          let wo = gen_warn visited bb in
          match wo with
          | Some w -> [w]
          | None -> find_first_unreachable visited bbs
        end
    in
    let visited = visit cfg (Set.empty(module Int)) (List.nth_exn all_bbs 0) in
    find_first_unreachable visited all_bbs

(** TODO: Find unused variables in POU. *)
(* let find_unused_variables pou env = *)

let run (cfgs : Cfg.t list) : Warn.t list =
  (* List.iter cfgs ~f:(fun c -> Printf.printf "%s\n" (Cfg.to_string c)); *)
  (* List.fold_left                                         *)
  (*   cfgs                                                 *)
  (*   ~init:[]                                             *)
  (*   ~f:(fun acc cfg -> acc @ (find_unreachable_bbs cfg)) *)
 []
