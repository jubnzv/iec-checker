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
  | BBInit (** Initial basic block *)
  | BBOut (** Return/exit node *)
  | BBJump (** Indirect jump to/from a node *)

(** Basic block *)
type bb =
  {
    id : int;
    ty: bb_ty;
    mutable in_edges : edge list; (** Incoming edges *)
    mutable out_edges : edge list; (** Outcoming edges *)
    stmt : S.statement;
    pou : S.iec_library_element; (** The POU that this BB belongs to *)
  }

(** Directed edges to represent jumps between basic blocks *)
and edge =
  {
    in_bb : int; (** Input basic block id *)
    out_bb : int; (** Output basic block id *)
  }

val mk : S.iec_library_element -> t
(** Create a new CFG instance for a given iec_library_element *)

val to_string : t -> string

val to_yojson : t -> Yojson.Safe.t

val bb_get_ti : bb -> TI.t
(** Get token info for the basic block. *)

val create_cfgs : S.iec_library_element list -> t list
(** Create list of CFGs for a given iec_library_element objects. *)
