(** Routines to evaluate cyclomatic complexity for intraprocedural control flow
    graph. *)
open IECCheckerCore
module S = Syntax

val eval_mccabe : Cfg.t -> int
(** [eval_mccabe cfg] Evaluate McCabe cyclomatic complexity for [cfg]. *)
