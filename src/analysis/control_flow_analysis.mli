(** Control flow analysis *)
open IECCheckerCore

val run : Cfg.t list -> Warn.t list
(** Run all intraprocedural inspections *)
