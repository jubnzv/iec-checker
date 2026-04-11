(** PLCOPEN-N10 – Define name prefixes for user defined types *)
open IECCheckerCore
module S = IECCheckerCore.Syntax
val do_check : S.iec_library_element list -> Warn.t list
val detector : Detector.t
