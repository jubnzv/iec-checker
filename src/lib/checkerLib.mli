open IECCheckerCore
module S = Syntax

val run_all_checks : S.iec_library_element list -> Env.t list -> Cfg.t list -> bool -> Warn.t list
(** [run_all_checks] Run all available checks *)
