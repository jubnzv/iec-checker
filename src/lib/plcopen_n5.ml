open Core
module S = IECCheckerCore.Syntax
module AU = IECCheckerCore.Ast_util
module Warn = IECCheckerCore.Warn

let is_global vd =
  match S.VarDecl.get_attr vd with
  | Some (S.VarDecl.VarGlobal _) -> true
  | _ -> false

let collect_globals elems =
  List.concat_map elems ~f:(fun elem ->
      AU.get_var_decls elem
      |> List.filter_map ~f:(fun vd ->
          if is_global vd then Some (S.VarDecl.get_var_name vd) else None))
  |> Set.of_list (module String)

let check_elem globals elem =
  AU.get_var_decls elem
  |> List.filter_map ~f:(fun vd ->
      if is_global vd then None
      else
        let name = S.VarDecl.get_var_name vd in
        if Set.mem globals name then
          let ti = S.VarDecl.get_var_ti vd in
          let msg = Printf.sprintf
              "Local name %s shadows a global variable" name
          in
          Some (Warn.mk ti.linenr ti.col "PLCOPEN-N5" msg)
        else None)

let do_check elems =
  let globals = collect_globals elems in
  if Set.is_empty globals then []
  else List.concat_map elems ~f:(check_elem globals)

let detector : Detector.t = {
  id = "PLCOPEN-N5";
  name = "Local names shall not shadow global names";
  summary =
    "Local variable declarations must not reuse a name already declared at \
     global scope.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-N5";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
