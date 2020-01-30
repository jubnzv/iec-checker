type error = InternalError

exception Error of string

let to_string = function
 | InternalError -> "InternalError: "

let raise e msg =
 raise @@ Error ((to_string e) ^ msg)
