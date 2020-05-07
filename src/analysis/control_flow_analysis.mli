(** Intraprocedural control flow analysis: Find unreachable code. *)
open IECCheckerCore
module S = Syntax

val run : S.iec_library_element list -> Cfg.t list -> Warn.t list
(** Run all intraprocedural inspections. *)
