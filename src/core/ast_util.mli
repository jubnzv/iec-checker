(** Helpers to work with AST *)
module S = Syntax

val expr_to_stmts : S.expr -> S.statement list
(** [expr_to_stmts expr] Convert [expr] to a list of statements. *)

val get_var_decl : S.iec_library_element list -> S.VarDecl.t list
(** Collect variable declaration elemenets from each POU *)

val get_pou_stmts : S.iec_library_element -> S.statement list
(** Recursively get all statements from a given POU *)

val get_top_stmts : S.iec_library_element -> S.statement list
(** [get_top_stmts pou] Non-recursively get statements from a [pou]. *)

val get_stmts : S.iec_library_element list -> S.statement list
(** Collect all statements from each POU *)

val get_exprs : S.iec_library_element list -> S.expr list
(** Collect expressions from each statement of the POUs *)

val create_envs : S.iec_library_element list -> Env.t list
(** Create the environments for a given configuration elements *)
