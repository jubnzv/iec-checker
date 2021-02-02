(** PLCOPEN-CP3: All variables shall be initialized before being used *)
open IECCheckerCore
module S = Syntax
val do_check : S.iec_library_element list -> Warn.t list
