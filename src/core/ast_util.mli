(** Helpers to work with AST *)
module S = Syntax

val expr_to_stmts : S.expr -> S.statement list
(** Convert given expression to a list of statements *)

val get_var_decl : S.iec_library_element list -> S.VarDecl.t list
(** Collect variable declaration elemenets from each POU *)

val get_pou_stmts : S.iec_library_element -> S.statement list
(** Recursively get all statements from a given POU *)

val get_top_stmts : S.iec_library_element -> S.statement list
(** Non-recursively get statements from a given POU *)

val get_stmts : S.iec_library_element list -> S.statement list
(** Collect statements from each POU *)

val create_envs : S.iec_library_element list -> Env.t list
(** Create the environments for a given configuration elements *)
