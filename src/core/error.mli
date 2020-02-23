type error = InternalError | UnboundIdentifier

exception Error of string

val raise : error -> string -> 'a
(** Raise error with given message *)
