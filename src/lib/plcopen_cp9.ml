open Core_kernel
open IECCheckerCore
open IECCheckerAnalysis

module S = Syntax
module C = Config
module CC = Cyclomatic_complexity

let do_check cfgs =
  List.fold_left
    cfgs
    ~init:[]
    ~f:(fun acc cfg -> begin
      let cc = CC.eval_mccabe cfg in
      if cc > C.mccabe_complexity_threshold then
        let msg = Printf.sprintf "Code is too complex (%d)" cc in
        let w = Warn.mk 0 0 "PLCOPEN-CP9" msg in
        acc @ [w]
      else acc
    end)

