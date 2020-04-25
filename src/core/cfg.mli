(**  Control Flow Graph for Intraprocedural Analysis.

     The current implementation of CFG is used Syntax.stmt as nodes and store
     connections (edges) between them. This is useful for dead code detection.
*)
module S = Syntax

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
    in_bb : bb; (** Input basic block *)
    out_bb : bb; (** Output basic block *)
  }

val mk : S.iec_library_element -> t
(** Create a new CFG instance for a given iec_library_element *)
