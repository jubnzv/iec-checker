(** Describes single token of a parse tree *)

type t = { id : int; linenr : int; col : int } [@@deriving yojson, show]
(** Parse tree item *)

val create : Lexing.lexbuf -> t
(** [create] Create new parse tree element from Lexing.lexbuf *)

val create_dummy : unit -> t
(** [create_dummy] Create a new dummy parse tree element *)

val to_string : t -> string
