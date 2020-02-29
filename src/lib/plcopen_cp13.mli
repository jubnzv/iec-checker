(** PLCOPEN-CP13 – POUs shall not call themselves directly or indirectly
    See: PLCopen Coding Guidelines 5.14. *)

open IECCheckerCore
module S = IECCheckerCore.Syntax

val do_check : S.iec_library_element list -> Warn.t list
