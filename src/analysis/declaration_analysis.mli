(** Declaration analysis: inspect compatibility of the identifiers and their
    declarations. *)
open IECCheckerCore
module S = Syntax

val run : S.iec_library_element list -> Env.t list -> Warn.t list
