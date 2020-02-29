type t = { name : string; msg : string }

let mk name msg = { name; msg }

let print w = Printf.printf "%s: %s" w.name w.msg
