(** Helpers to work with AST *)
module S = Syntax

val get_var_decl : S.iec_library_element list -> S.VarDecl.t list
(** Collect variable declaration elemenets from each POU *)

val get_pou_stmts : S.iec_library_element -> S.statement list
(** Collect statements from a given POU *)

val get_stmts : S.iec_library_element list -> S.statement list
(** collect statements from each POU *)

val create_envs : S.iec_library_element list -> Env.t list
(** Create the environments for a given configuration elements *)
