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
  cfgs: Cfg.t list;
} [@@deriving to_yojson]

val create_dump : dst_file:string -> S.iec_library_element list -> Env.t list -> Cfg.t list -> unit
(** [create_dump] Save input AST in a JSON file.  *)
