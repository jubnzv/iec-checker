(** Describes single token of a parse tree *)

type t = { id : int; linenr : int; col : int }
(** Parse tree item *)

val create : Lexing.lexbuf -> t
(** Create new parse tree element from Lexing.lexbuf *)
