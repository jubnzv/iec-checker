type t = { linenr: int; column: int; id: string; msg: string; } [@@deriving yojson]

let mk linenr column id msg = { linenr; column; id; msg }

let to_string w =
  Printf.sprintf "%d:%d %s: %s" w.linenr w.column w.id w.msg

