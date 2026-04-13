(** PLCOPEN-CP26 – A global variable may be written only by one PROGRAM *)
open IECCheckerCore
module S = IECCheckerCore.Syntax
val do_check : S.iec_library_element list -> Warn.t list
val detector : Detector.t
