open Core
module S = IECCheckerCore.Syntax
module AU = IECCheckerCore.Ast_util
module Warn = IECCheckerCore.Warn
module Config = IECCheckerCore.Config

(* Recognized case style names. *)
let upper_camel = "UpperCamelCase"
let lower_camel = "lowerCamelCase"
let upper_snake = "UPPER_SNAKE_CASE"
let lower_snake = "lower_snake_case"

let is_upper_camel s =
  (not (String.is_empty s))
  && Char.is_uppercase s.[0]
  && String.for_all s ~f:Char.is_alphanum

let is_lower_camel s =
  (not (String.is_empty s))
  && Char.is_lowercase s.[0]
  && String.for_all s ~f:Char.is_alphanum

let is_upper_snake s =
  (not (String.is_empty s))
  && not (Char.is_digit s.[0])
  && String.for_all s ~f:(fun c ->
      Char.is_uppercase c || Char.is_digit c || Char.equal c '_')

let is_lower_snake s =
  (not (String.is_empty s))
  && not (Char.is_digit s.[0])
  && String.for_all s ~f:(fun c ->
      Char.is_lowercase c || Char.is_digit c || Char.equal c '_')

let predicate_of_style = function
  | "UpperCamelCase" -> Some (is_upper_camel, upper_camel)
  | "lowerCamelCase" -> Some (is_lower_camel, lower_camel)
  | "UPPER_SNAKE_CASE" -> Some (is_upper_snake, upper_snake)
  | "lower_snake_case" -> Some (is_lower_snake, lower_snake)
  | _ -> None

let is_constant vd =
  match S.VarDecl.get_attr vd with
  | Some (S.VarDecl.Var (Some S.VarDecl.QConstant))
  | Some (S.VarDecl.VarOut (Some S.VarDecl.QConstant))
  | Some (S.VarDecl.VarIn (Some S.VarDecl.QConstant))
  | Some (S.VarDecl.VarExternal (Some S.VarDecl.QConstant))
  | Some (S.VarDecl.VarGlobal (Some S.VarDecl.QConstant)) -> true
  | _ -> false

let check_identifier ~style name linenr col =
  match Option.bind style ~f:predicate_of_style with
  | None -> None
  | Some (pred, label) ->
    if pred name then None
    else
      let msg = Printf.sprintf
          "Identifier %s does not match required case %s" name label
      in
      Some (Warn.mk linenr col "PLCOPEN-N4" msg)

let display_name_of_ti ti fallback =
  if String.is_empty ti.IECCheckerCore.Tok_info.raw then fallback else ti.raw

let check_var_decl cfg vd =
  let ti = S.VarDecl.get_var_ti vd in
  let name = display_name_of_ti ti (S.VarDecl.get_var_name vd) in
  let style =
    if is_constant vd then cfg.Config.naming_case_constant
    else cfg.Config.naming_case_variable
  in
  check_identifier ~style name ti.linenr ti.col

let pou_name_and_loc = function
  | S.IECFunction (_, f) ->
    let ti = S.Function.get_ti f.id in
    let name = display_name_of_ti ti (S.Function.get_name f.id) in
    Some (name, ti.linenr, ti.col)
  | S.IECFunctionBlock (_, fb) ->
    let ti = S.FunctionBlock.get_ti fb.id in
    let name = display_name_of_ti ti (S.FunctionBlock.get_name fb.id) in
    Some (name, ti.linenr, ti.col)
  | S.IECProgram (_, p) -> Some (p.name, 0, 0)
  | S.IECClass (_, c) -> Some (c.class_name, 0, 0)
  | S.IECInterface (_, i) -> Some (i.interface_name, 0, 0)
  | S.IECConfiguration _ | S.IECType _ -> None

let type_name_and_loc = function
  | S.IECType (_, (name, _)) -> Some (name, 0, 0)
  | _ -> None

let check_elem cfg elem =
  let var_warns =
    AU.get_var_decls elem
    |> List.filter_map ~f:(check_var_decl cfg)
  in
  let pou_warn =
    match pou_name_and_loc elem with
    | Some (name, linenr, col) ->
      check_identifier ~style:cfg.Config.naming_case_pou name linenr col
    | None -> None
  in
  let type_warn =
    match type_name_and_loc elem with
    | Some (name, linenr, col) ->
      check_identifier ~style:cfg.Config.naming_case_type name linenr col
    | None -> None
  in
  var_warns
  @ (Option.to_list pou_warn)
  @ (Option.to_list type_warn)

let do_check elems =
  let cfg = Config.get () in
  let any_case =
    Option.is_some cfg.naming_case_variable
    || Option.is_some cfg.naming_case_constant
    || Option.is_some cfg.naming_case_pou
    || Option.is_some cfg.naming_case_type
  in
  if not any_case then []
  else List.concat_map elems ~f:(check_elem cfg)

let detector : Detector.t = {
  id = "PLCOPEN-N4";
  name = "Define the use of case (capitals)";
  summary =
    "Identifiers should follow a configurable naming convention per element \
     kind (variable, constant, POU, type).";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-N4";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
