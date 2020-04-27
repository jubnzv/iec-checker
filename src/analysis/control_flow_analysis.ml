[@@@warning "-32"]
[@@@warning "-33"]
[@@@warning "-27"]

open Core_kernel
open IECCheckerCore

module TI = Tok_info

(** Generate warning for a given basic block *)
let gen_warn (bb : Cfg.bb) : Warn.t =
  Warn.mk 0 0 "CFA" ((TI.to_string (Cfg.bb_get_ti bb)) ^ "is unreachable!")

(** Find unreachable basic blocks (dead code) *)
let find_unreachable_bbs cfg =
  []

(** TODO: Find unused variables in POU. *)
(* let find_unused_variables pou env = *)

let run (cfgs : Cfg.t list) =
  (* List.iter cfgs ~f:(fun c -> Printf.printf "%s\n" (Cfg.to_string c)); *)
  []
  (* List.fold_left cfgs                                     *)
  (*   ~init:[]                                              *)
  (*   ~f:(fun bbs cfg -> (find_unreachable_bbs cfg) :: bbs) *)
  (* |> List.fold_left                                       *)
  (*   ~init:[]                                              *)
  (*   ~f:(fun warns bb -> (gen_warn bb) :: warns)           *)
