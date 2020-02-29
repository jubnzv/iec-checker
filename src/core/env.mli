(**
   Representation of the environments for IEC61131-3 languages.

   IEC61131-3 standard has the similar definition of "scope" - a portion of a
   language element within which a declaration or label applies (see ch.
   6.5.2.2).

   This implementation keeps environment which map uses of identifiers to their
   semantic information.
*)

module S = Syntax

type t

val empty : t
(** Create an empty environment *)

val mk_global : t
(** Make a new global environment. *)

val mk : t (** parent environment *) -> t
(** Add a new child environment *)

val add_vdecl : t -> S.VarDecl.t -> t
(** Insert variable declaration in [t] *)

val get_vdecls : t -> S.VarDecl.t list
(** Return variables declared in [t] *)

val lookup_vdecl : t -> string -> S.VarDecl.t option
(** Search for a given identifier name in the given environment *)
