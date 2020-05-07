(**  Control Flow Graph for Intraprocedural Analysis.

     The current implementation of CFG is used Syntax.stmt as nodes and store
     connections (edges) between them. This is useful for dead code detection.
*)
module S = Syntax
module TI = Tok_info

type t

(** Type of a basic block. *)
type bb_ty =
  | BB (** Regular basic block *)
  | BBEntry (** Point of entry *)
  | BBExit (** Point of exit *)
  | BBJump (** Indirect jump to/from a node *)

(** Basic block *)
type bb =
  {
    id : int;
    mutable ty: bb_ty;
    mutable preds : int list; (** Ids of predecessor nodes *)
    mutable succs : int list; (** Ids of successor nodes *)
    stmt : S.statement;
  }

val mk : S.iec_library_element -> t
(** Create a new CFG instance for a given iec_library_element *)

val get_pou_id : t -> int
(** Get the id of the POU that this CFG belongs to *)

val list_basic_blocks : t -> bb list
(** Get list of basic blocks from a given CFG instance. *)

val bb_by_id : t -> int -> bb option
(** Get basic block entry from a given id. *)

val to_string : t -> string

val bb_get_ti : bb -> TI.t
(** Get token info for the basic block. *)

val create_cfgs : S.iec_library_element list -> t list
(** Create list of CFGs for a given iec_library_element objects. *)

val to_yojson : t -> Yojson.Safe.t

