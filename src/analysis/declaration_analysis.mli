(* Declaration analysis, checks identifiers and declaration compatibility. *)

open IECCheckerCore
module S = Syntax

val run_declaration_analysis : S.iec_library_element list -> Env.t list -> Warn.t list
