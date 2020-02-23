open IECCheckerCore
module S = Syntax

val run_all_checks : S.iec_library_element list -> Scope.t list -> Warn.t list
(** Run all available checks *)
