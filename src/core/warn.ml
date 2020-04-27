type warn_ty =
  | Inspection
  | InternalError
[@@deriving yojson]

type t = {
  linenr: int;
  column: int;
  id: string;
  msg: string;
  ty: warn_ty [@key "type"];
} [@@deriving yojson]

let mk ?(ty=Inspection) linenr column id msg = { linenr; column; id; msg; ty }
let mk_internal ?(id="InternalError") msg = mk ~ty:InternalError 0 0 id msg

let to_string w =
  match w.ty with
  | Inspection -> Printf.sprintf "%d:%d %s: %s" w.linenr w.column w.id w.msg
  | InternalError -> Printf.sprintf "%s: %s" w.id w.msg

