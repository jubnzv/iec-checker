(** PLCOPEN-CP13 – POUs shall not call themselves directly or indirectly *)
open IECCheckerCore
module S = IECCheckerCore.Syntax
val do_check : S.iec_library_element list -> Warn.t list
val detector : Detector.t
