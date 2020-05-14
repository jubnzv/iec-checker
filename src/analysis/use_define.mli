(* Detect common errors in 'use' occurrences of the local variables in POUs. *)
open IECCheckerCore
module S = Syntax

val run : S.iec_library_element list -> Warn.t list
