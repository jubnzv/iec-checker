type error = InternalError

exception Error of string

val raise : error -> string -> 'a
(** Raise error with given message *)
