open Core
module S = IECCheckerCore.Syntax
module AU = IECCheckerCore.Ast_util
module Warn = IECCheckerCore.Warn
module Config = IECCheckerCore.Config

let check_length ~min_len ~max_len name linenr col =
  let len = String.length name in
  if min_len > 0 && len < min_len then
    let msg = Printf.sprintf
        "Identifier %s is too short (%d chars, minimum %d)"
        name len min_len
    in
    Some (Warn.mk linenr col "PLCOPEN-N6" msg)
  else if max_len > 0 && len > max_len then
    let msg = Printf.sprintf
        "Identifier %s is too long (%d chars, maximum %d)"
        name len max_len
    in
    Some (Warn.mk linenr col "PLCOPEN-N6" msg)
  else None

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

let check_elem ~min_len ~max_len elem =
  let var_warns =
    AU.get_var_decls elem
    |> List.filter_map ~f:(fun vd ->
        let name = S.VarDecl.get_var_name vd in
        let ti = S.VarDecl.get_var_ti vd in
        check_length ~min_len ~max_len name ti.linenr ti.col)
  in
  let pou_warn =
    match pou_name_and_loc elem with
    | Some (name, linenr, col) ->
      check_length ~min_len ~max_len name linenr col
    | None -> None
  in
  var_warns @ (Option.to_list pou_warn)

let do_check elems =
  let cfg = Config.get () in
  let min_len = cfg.naming_min_length in
  let max_len = cfg.naming_max_length in
  if min_len <= 0 && max_len <= 0 then []
  else List.concat_map elems ~f:(check_elem ~min_len ~max_len)

let detector : Detector.t = {
  id = "PLCOPEN-N6";
  name = "Define an acceptable name length";
  summary =
    "Identifiers shorter than the configured minimum or longer than the \
     configured maximum should be renamed.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-N6";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
