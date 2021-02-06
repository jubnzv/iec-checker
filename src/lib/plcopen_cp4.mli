(** PLCOPEN-CP4: Direct addressing should not overlap *)
open IECCheckerCore
module S = Syntax
val do_check : S.iec_library_element list -> Warn.t list
