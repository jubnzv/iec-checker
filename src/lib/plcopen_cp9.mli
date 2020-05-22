(** PLCOPEN-CP9 – Limit the complexity of POU code.
    See: PLCopen Coding Guidelines 5.10. *)

open IECCheckerCore

module S = Syntax

val do_check : Cfg.t list -> Warn.t list
