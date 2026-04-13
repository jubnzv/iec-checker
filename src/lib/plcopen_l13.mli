(** PLCOPEN-L13 – FOR loop variable should not be used outside the FOR loop *)
open IECCheckerCore
module S = IECCheckerCore.Syntax
val do_check : S.iec_library_element list -> Warn.t list
val detector : Detector.t
