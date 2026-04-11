open Core
module S = IECCheckerCore.Syntax
module AU = IECCheckerCore.Ast_util
module Warn = IECCheckerCore.Warn

let is_identifier_char c =
  Char.is_alphanum c || Char.equal c '_'

let is_ascii_identifier name =
  (not (String.is_empty name))
  && (let c = name.[0] in Char.is_alpha c || Char.equal c '_')
  && String.for_all name ~f:is_identifier_char

let check_name name linenr col =
  if is_ascii_identifier name then None
  else
    let msg = Printf.sprintf
        "Identifier %s contains characters outside [A-Za-z_][A-Za-z0-9_]*"
        name
    in
    Some (Warn.mk linenr col "PLCOPEN-N8" msg)

let pou_name_and_loc = function
  | S.IECFunction (_, f) ->
    let ti = S.Function.get_ti f.id in
    Some (S.Function.get_name f.id, ti.linenr, ti.col)
  | S.IECFunctionBlock (_, fb) ->
    let ti = S.FunctionBlock.get_ti fb.id in
    Some (S.FunctionBlock.get_name fb.id, ti.linenr, ti.col)
  | S.IECProgram (_, p) -> Some (p.name, 0, 0)
  | S.IECClass (_, c) -> Some (c.class_name, 0, 0)
  | S.IECInterface (_, i) -> Some (i.interface_name, 0, 0)
  | S.IECType (_, (name, _)) -> Some (name, 0, 0)
  | S.IECConfiguration _ -> None

let check_elem elem =
  let var_warns =
    AU.get_var_decls elem
    |> List.filter_map ~f:(fun vd ->
        let name = S.VarDecl.get_var_name vd in
        let ti = S.VarDecl.get_var_ti vd in
        check_name name ti.linenr ti.col)
  in
  let pou_warn =
    match pou_name_and_loc elem with
    | Some (name, linenr, col) -> check_name name linenr col
    | None -> None
  in
  var_warns @ (Option.to_list pou_warn)

let do_check elems =
  List.concat_map elems ~f:check_elem

let detector : Detector.t = {
  id = "PLCOPEN-N8";
  name = "Define the acceptable character set";
  summary =
    "Identifiers must only contain ASCII letters, digits and underscores, \
     and must not start with a digit.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-N8";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
