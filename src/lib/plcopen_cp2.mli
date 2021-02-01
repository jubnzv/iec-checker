(** PLCOPEN-CP2: All code shall be used in the application *)
open IECCheckerCore
module S = Syntax
val do_check : Cfg.t list -> Warn.t list
