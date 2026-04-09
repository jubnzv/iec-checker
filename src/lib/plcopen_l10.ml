open Core
open IECCheckerCore

module S = IECCheckerCore.Syntax

let do_check elems =
  Ast_util.get_stmts elems
  |> List.fold_left
    ~init:[]
    ~f:(fun acc s -> begin
          match s with
          | S.StmContinue ti | S.StmExit ti -> acc @ [Warn.mk ti.linenr ti.col "PLCOPEN-L10" "Usage of CONTINUE and EXIT instruction should be avoid"]
          | _ -> acc
        end)

let detector : Detector.t = {
  id = "PLCOPEN-L10";
  name = "Usage of CONTINUE and EXIT instructions should be avoided";
  summary =
    "Loop bodies should fall through naturally instead of using [CONTINUE] / [EXIT].";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-L10";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
