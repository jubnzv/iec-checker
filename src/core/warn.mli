(** Warning generated by static analazyer. *)
type warn_ty =
  | Inspection
  | InternalError
[@@deriving yojson]

type t = {
  linenr: int;
  column: int;
  file: string;
  id: string;
  msg: string;
  ty: warn_ty [@key "type"];
} [@@deriving yojson]

val mk : ?ty:(warn_ty) -> ?file:(string) -> int -> int -> string -> string -> t
val mk_internal : ?id:(string) -> string -> t
val mk_from_lexbuf : Lexing.lexbuf -> string -> string -> t

val to_string : t -> string
