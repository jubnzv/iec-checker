(** Declaration analysis, checks identifiers and declaration compatibility. *)

open IECCheckerCore
module S = Syntax

val run : S.iec_library_element list -> Env.t list -> Warn.t list
