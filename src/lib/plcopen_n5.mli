(** PLCOPEN-N5 – Local names shall not shadow global names *)
open IECCheckerCore
module S = IECCheckerCore.Syntax
val do_check : S.iec_library_element list -> Warn.t list
val detector : Detector.t
