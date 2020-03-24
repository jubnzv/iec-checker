open Core_kernel

module S = Syntax
module AU = Ast_util

type bb_ty =
  | BB (** Regular basic block *)
  | BBOut   (** Return/exit node *)
  | BBJump (** Indirect jump to/from a node *)

type t = bb list

and bb =
  {
    ty: bb_ty;
    mutable in_edges : edge list; (** Incoming edges *)
    mutable out_edges : edge list; (** Outcoming edges *)
    content : S.statement;
  }

and edge =
  {
    in_bb : bb; (** Input basic block *)
    out_bb : bb; (** Output basic block *)
  }

(** Add edge between two basic blocks *)
(* let add_edge bb1 (** in *) bb2 (** out *) =
  () *)

(** Create basic block instance from a statement *)
let create_bb stmt =
  let ty = BB in
  let in_edges = [] in
  let out_edges = [] in
  let content = stmt in
  { ty; in_edges; out_edges; content }

let create g iec_element =
  let stmts = AU.get_pou_stmts iec_element in
  List.fold_left ~f:(fun bbs stmt -> let bb = create_bb stmt in bb :: bbs) ~init:[]
