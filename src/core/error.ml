type error = InternalError | UnboundIdentifier

exception Error of string

let to_string = function
  | InternalError -> "InternalError: "
  | UnboundIdentifier -> "UnboundIdentifier: "

let raise e msg = raise @@ Error (to_string e ^ msg)
