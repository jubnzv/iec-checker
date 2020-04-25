(** Helpers to work with AST *)
module S = Syntax

(** IEC program scheme used in yojson serialization. *)
type dump_scheme = {
    version: string; (** Scheme version *)
    functions: S.function_decl list;
    function_blocks: S.fb_decl list;
    programs: S.program_decl list;
    configurations: S.configuration_decl list;
    types: S.derived_ty_decl list;
    environments: Env.t list;
} [@@deriving to_yojson]

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

val create_dump : S.iec_library_element list -> Env.t list -> string (** source filename *) -> unit
(** Save input AST in a JSON file.  *)
