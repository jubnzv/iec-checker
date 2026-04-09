open Core
open IECCheckerCore

module S = Syntax
module AU = IECCheckerCore.Ast_util

let is_time_or_phys = function
  | S.ExprConstant (_, c) -> begin
      match c with
      | S.CTimeValue _ -> true
      | _ -> false
    end
  | _ -> false


let check_elem elem =
  AU.get_pou_exprs elem
  |> List.fold_left ~init:[]
    ~f:(fun acc expr -> begin
          match expr with
          | S.ExprBin(ti, lhs, operator, rhs) -> begin
              match operator with
              | NEG | EQ -> begin
                  if (is_time_or_phys lhs) || (is_time_or_phys rhs) then begin
                    let msg = "Time and physical measures comparisons shall not be equality or inequality" in
                    acc @ [(Warn.mk ti.linenr ti.col "PLCOPEN-CP28" msg)]
                  end
                  else acc
                end
              | _ -> acc
            end
          | _ -> acc
        end)

let do_check elems =
  List.fold_left
    ~init:[]
    elems
    ~f:(fun acc elem -> acc @ (check_elem elem))

let detector : Detector.t = {
  id = "PLCOPEN-CP28";
  name = "Time and physical measures comparisons shall not be equality or inequality";
  summary =
    "Use range comparisons instead of [=] / [<>] when comparing [TIME] values.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP28";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
