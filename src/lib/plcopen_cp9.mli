(** PLCOPEN-CP9 – Limit the complexity of POU code. *)
open IECCheckerCore
module S = Syntax
val do_check : S.iec_library_element list -> Cfg.t list -> Warn.t list
