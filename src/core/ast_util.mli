(** Helpers to work with AST *)
module S = Syntax
module TI = Tok_info

val expr_to_stmts : S.expr -> S.statement list
(** [expr_to_stmts expr] Convert [expr] to a list of statements. *)

val get_var_decls : S.iec_library_element -> S.VarDecl.t list
(** [get_var_decls elem] Get all variable declarations from the given [elem]. *)

val get_pou_stmts : S.iec_library_element -> S.statement list
(** Recursively get all statements from a given POU *)

val get_top_stmts : S.iec_library_element -> S.statement list
(** [get_top_stmts pou] Non-recursively get statements from a [pou]. *)

val get_stmts_num : S.iec_library_element -> int
(** [get_stmts_num elem] Return number of statements from [elem] including
    nested ones. *)

val get_stmts : S.iec_library_element list -> S.statement list
(** Collect all statements from each POU *)

val get_exprs : S.iec_library_element list -> S.expr list
(** Collect expressions from each statement of the POUs *)

val filter_exprs : f:(S.expr -> bool) -> S.iec_library_element -> S.expr list
(** [filter_exprs f elem] Return list of expressions that satisfy the predicate
    function [f].*)

val get_ti_by_name_exn : S.iec_library_element -> string -> TI.t
(** [get_ti_by_name_exn elem name] Get token info for variable declaration by it [name]. *)

val create_envs : S.iec_library_element list -> Env.t list
(** Create the environments for a given configuration elements *)

val eval_array_capacity : S.arr_subrange list -> int
(** [eval_array_capacity subranges] Evaluate maximum capacity of the array with
    respect to [subranges]. *)
