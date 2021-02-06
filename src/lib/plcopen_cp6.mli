(** PLCOPEN-CP6: Avoid external variables in functions, function blocks and classes *)
open IECCheckerCore
module S = Syntax
val do_check : S.iec_library_element list -> Warn.t list
