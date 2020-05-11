(** Detect unused variables in the source code. *)
open IECCheckerCore
module S = Syntax

val run : S.iec_library_element list -> Warn.t list
