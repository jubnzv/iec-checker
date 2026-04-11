(** Describes single token of a parse tree *)

type t = { id : int; linenr : int; col : int; raw : string } [@@deriving yojson, show]
(** Parse tree item.  [raw] stores the original source spelling of the token
    (preserving case) for identifiers; empty string otherwise. *)

val create : Lexing.lexbuf -> t
(** [create] Create new parse tree element from Lexing.lexbuf *)

val create_with_raw : Lexing.lexbuf -> string -> t
(** [create_with_raw lexbuf raw] like {!create} but also records the original
    token text in the [raw] field. *)

val create_dummy : unit -> t
(** [create_dummy] Create a new dummy parse tree element *)

val to_string : t -> string
