[@@@warning "-32"]
[@@@warning "-33"]
[@@@warning "-27"]

open Core_kernel
open IECCheckerCore

module TI = Tok_info

(** Generate warning for a given basic block *)
let mk_warn (bb : Cfg.bb) : Warn.t =
  Warn.mk 0 0 "CFA" ((TI.to_string (Cfg.bb_get_ti bb)) ^ " is unreachable!")

(** Find unreachable basic blocks (dead code) *)
let find_unreachable_bbs (cfg: Cfg.t) : Warn.t list =
  let rec visit (cfg: Cfg.t) visited (bb : Cfg.bb) =
    match bb.ty with
    | Cfg.BBEntry | Cfg.BB | Cfg.BBJump -> begin
        let visited = Set.add visited bb.id in
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
  let visited = List.fold_left
      all_bbs
      ~init:(Set.empty(module Int))
      ~f:(fun set bb -> visit cfg set bb)
  in
  List.fold_left
    all_bbs
    ~init:[]
    ~f:(fun warns bb -> begin
          match (Set.find visited ~f:(fun id -> phys_equal id bb.id)) with
          | Some _ -> warns
          | None -> (warns @ [(mk_warn bb)])
        end)

(** TODO: Find unused variables in POU. *)
(* let find_unused_variables pou env = *)

let run (cfgs : Cfg.t list) : Warn.t list =
  (* List.iter cfgs ~f:(fun c -> Printf.printf "%s\n" (Cfg.to_string c)); *)
  List.fold_left
    cfgs
    ~init:[]
    ~f:(fun acc cfg -> acc @ (find_unreachable_bbs cfg))
