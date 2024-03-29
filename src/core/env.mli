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

val get_id : t -> int
(** [get_id] Get the unique ID of the POU that this env belongs to. *)

val empty : t
(** [empty] Create an empty environment *)

val mk_global : unit -> t
(** [mk_global] Make a new global environment. *)

val mk : t (** parent environment *) -> int (** POU id *) -> t
(** [mk] Add a new child environment *)

val add_vdecl : t -> S.VarDecl.t -> t
(** [add_vdecl] Insert variable declaration in [t]. *)

val get_vdecls : t -> S.VarDecl.t list
(** [get_vdecls] Return variables declared in [t]. *)

val lookup_vdecl : t -> string -> S.VarDecl.t option
(** [lookup_vdecl] Search for a given identifier name in the given environment. *)

val to_yojson : t -> Yojson.Safe.t
