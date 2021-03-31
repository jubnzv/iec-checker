(** PLCOPEN-CP8: Floating point comparison shall not be equality or inequality *)
open IECCheckerCore
module S = Syntax
val do_check : S.iec_library_element list -> Warn.t list

