(** PLCOPEN-N3 – Define the names to avoid
    See: PLCopen Coding Guidelines 3.2.1. *)

open IECCheckerCore
module S = IECCheckerCore.Syntax

val do_check : S.iec_library_element list -> Warn.t list
