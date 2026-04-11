open Core
module S = IECCheckerCore.Syntax
module AU = IECCheckerCore.Ast_util
module Warn = IECCheckerCore.Warn
module Config = IECCheckerCore.Config

let type_name_of_decl vd =
  match S.VarDecl.get_ty_spec vd with
  | Some (S.DTyDeclSingleElement (S.DTySpecElementary ty, _)) ->
    Some (S.ety_to_string ty)
  | _ -> None

let display_name vd =
  let ti = S.VarDecl.get_var_ti vd in
  if String.is_empty ti.raw then S.VarDecl.get_var_name vd else ti.raw

let check_decl prefixes vd =
  match type_name_of_decl vd with
  | None -> None
  | Some ty_name ->
    match List.Assoc.find prefixes ~equal:String.equal ty_name with
    | None -> None
    | Some prefix when String.is_prefix (display_name vd) ~prefix -> None
    | Some prefix ->
      let ti = S.VarDecl.get_var_ti vd in
      let msg = Printf.sprintf
          "Variable %s of type %s should start with prefix %S"
          (display_name vd) ty_name prefix
      in
      Some (Warn.mk ti.linenr ti.col "PLCOPEN-N2" msg)

let do_check elems =
  let prefixes = (Config.get ()).naming_type_prefixes in
  if List.is_empty prefixes then []
  else
    List.concat_map elems ~f:(fun elem ->
        AU.get_var_decls elem
        |> List.filter_map ~f:(check_decl prefixes))

let detector : Detector.t = {
  id = "PLCOPEN-N2";
  name = "Define type prefixes for variables";
  summary =
    "Variable names should start with a configurable type-based prefix \
     (Hungarian notation).";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-N2";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
