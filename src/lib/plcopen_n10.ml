open Core
module S = IECCheckerCore.Syntax
module Warn = IECCheckerCore.Warn
module Config = IECCheckerCore.Config

let check_prefix prefixes kind name linenr col =
  match List.Assoc.find prefixes ~equal:String.equal kind with
  | None -> None
  | Some prefix when String.is_prefix name ~prefix -> None
  | Some prefix ->
    let msg = Printf.sprintf
        "%s %s should start with prefix %S" kind name prefix
    in
    Some (Warn.mk linenr col "PLCOPEN-N10" msg)

let check_elem prefixes = function
  | S.IECType (_, (name, spec)) ->
    let kind = S.dty_decl_spec_kind_to_string spec in
    Option.to_list (check_prefix prefixes kind name 0 0)
  | S.IECFunctionBlock (_, fb) ->
    let ti = S.FunctionBlock.get_ti fb.id in
    let name = S.FunctionBlock.get_name fb.id in
    Option.to_list
      (check_prefix prefixes "FUNCTION_BLOCK" name ti.linenr ti.col)
  | _ -> []

let do_check elems =
  let prefixes = (Config.get ()).naming_udt_prefixes in
  if List.is_empty prefixes then []
  else List.concat_map elems ~f:(check_elem prefixes)

let detector : Detector.t = {
  id = "PLCOPEN-N10";
  name = "Define name prefixes for user defined types";
  summary =
    "User-defined types and function blocks should start with a configurable \
     prefix based on their kind.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-N10";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
