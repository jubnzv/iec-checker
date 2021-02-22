(** PLCOPEN-L10 – Usage of CONTINUE and EXIT instruction should be avoid *)
open IECCheckerCore
module S = IECCheckerCore.Syntax
val do_check : S.iec_library_element list -> Warn.t list
