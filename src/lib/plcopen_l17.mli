(** PLCOPEN-L17 – Each IF instruction should have an ELSE clause *)
open IECCheckerCore
module S = IECCheckerCore.Syntax
val do_check : S.iec_library_element list -> Warn.t list
