(** PLCOPEN-CP1 – Access to a member shall be by name *)
open IECCheckerCore
module S = Syntax
val do_check : S.iec_library_element list -> Warn.t list
