(** Helpers to work with AST *)
module S = Syntax

val get_var_decl : S.iec_library_element list -> S.VarDecl.t list
(** Collect variable declaration elemenets from each POU *)

val get_stmts : S.iec_library_element list -> S.statement list
(** Collect statements from each POU *)
