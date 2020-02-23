(** The scope defined in IEC61131-3 as portion of a language element within
 * which a declaration or label applies.
 *
 * See 6.5.2.2 for the reference. *)

module S = Syntax

type t

val empty : t
(** Create an empty scope *)

val mk_global : t
(** Make a new global scope. *)

val mk : t (** parent scope *) -> t
(** Add a new child scope *)

val add_vdecl : t -> S.VarDecl.t -> t
(** Insert variable declaration in the given scope *)

val get_vdecls : t -> S.VarDecl.t list
(** Return variables declared in the given scope *)

val lookup_vdecl : t -> string -> S.VarDecl.t option
(** Search for a given identifier name in the given scope *)
