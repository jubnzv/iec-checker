(** PLCOPEN-L17 – Each IF instruction should have an ELSE clause
    See: PLCopen Coding Guidelines 6.5.9. *)

module S = IECCheckerCore.Syntax

val do_check : S.iec_library_element list -> Warn.t list
