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
    (* TODO: This should be replaced with ids of statements. But it will
       require additional symbol tables and a lot of refactoring in the parser. *)
    mutable stmts : S.statement list [@opaque]; (** Statements that makes up this BB *)
  }

val mk : S.iec_library_element -> t
(** [mk] Create a new CFG instance for a given iec_library_element. *)

val get_pou_id : t -> int
(** [get_pou_id] Get the id of the POU that this CFG belongs to. *)

val get_bb_by_id_exn : t -> int -> bb
(** [get_bb_by_id_exn id cfg] Return basic block stored in [cfg] by given ID.
    Raise [Not_found] if there are no such block *)

val get_all_ids : t -> int list
(** [get_all_ids cfg] Return a list with ids of all basic blocks represented
    in [cfg]. *)

val get_reachable_ids : t -> int list
(** [get_reachable_ids cfg] Return a list with ids of basic blocks that are
    reachable from [cfg] entry point. *)

val get_number_of_edges : t -> int
(** [get_number_of_edges cfg] Return number of edges in [cfg]. *)

val bb_by_id : t -> int -> bb option
(** [bb_by_id] Get basic block entry from a given id. *)

val bb_get_ti : bb -> TI.t
(** [bb_get_ti] Get token info for the basic block. *)

val create_cfgs : S.iec_library_element list -> t list
(** [create_cfgs] Create list of CFGs for a given iec_library_element objects. *)

val to_string : t -> string

val to_yojson : t -> Yojson.Safe.t

