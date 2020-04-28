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

let mk ?(ty=Inspection) ?(file="") linenr column id msg = { linenr; column; file; id; msg; ty }
let mk_internal ?(id="InternalError") msg = mk ~ty:InternalError 0 0 id msg
let mk_from_lexbuf (lexbuf : Lexing.lexbuf) id msg =
  let pos = lexbuf.lex_curr_p in
  mk ~file:(pos.pos_fname) pos.pos_lnum (pos.pos_cnum - pos.pos_bol) id msg

let to_string w =
  match w.ty with
  | Inspection -> Printf.sprintf "%d:%d %s: %s" w.linenr w.column w.id w.msg
  | InternalError -> Printf.sprintf "%s: %s" w.id w.msg

