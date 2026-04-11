open Core
module S = IECCheckerCore.Syntax
module AU = IECCheckerCore.Ast_util
module Warn = IECCheckerCore.Warn

(** Collect warnings for every direct-address variable use inside the
    statements of a POU. Declarations using [AT %M...] are left alone
    because they live outside [get_var_uses]. *)
let check_elem elem =
  AU.get_var_uses elem
  |> List.filter_map ~f:(fun vu ->
      match S.VarUse.get_loc vu with
      | S.VarUse.DirVar dv ->
        let ti = S.DirVar.get_ti dv in
        let msg = Printf.sprintf
            "Avoid hardcoded physical address %s in code"
            (S.DirVar.get_name dv)
        in
        Some (Warn.mk ti.linenr ti.col "PLCOPEN-N1" msg)
      | S.VarUse.SymVar _ -> None)

let do_check elems =
  List.concat_map elems ~f:check_elem

let detector : Detector.t = {
  id = "PLCOPEN-N1";
  name = "Avoid physical addresses";
  summary =
    "Hardcoded physical addresses should be replaced with symbolic names \
     declared in a VAR block.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-N1";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
