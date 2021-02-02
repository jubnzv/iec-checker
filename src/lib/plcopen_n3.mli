(** PLCOPEN-N3 – Define the names to avoid *)
open IECCheckerCore
module S = IECCheckerCore.Syntax
val do_check : S.iec_library_element list -> Warn.t list
