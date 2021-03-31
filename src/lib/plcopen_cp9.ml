open Core_kernel
open IECCheckerCore
open IECCheckerAnalysis

module AU = Ast_util
module S = Syntax
module CC = Cyclomatic_complexity

let get_mccabe_violations cfg =
  let cc = CC.eval_mccabe cfg in
  if cc > Config.mccabe_complexity_threshold then
    let msg = Printf.sprintf "Code is too complex (%d McCabe complexity)" cc in
    let w = Warn.mk 0 0 "PLCOPEN-CP9" msg in
    [w]
  else []

let get_statements_num_violations elem =
  let stmts_num = AU.get_stmts_num elem in
  if stmts_num > Config.statements_num_threshold then
    let msg = Printf.sprintf "Code is too complex (%d statements)" stmts_num  in
    let w = Warn.mk 0 0 "PLCOPEN-CP9" msg in
    [w]
  else []

let do_check elems cfgs =
  List.fold_left
    cfgs
    ~init:[]
    ~f:(fun acc cfg -> acc @ (get_mccabe_violations cfg))
  |> List.append
  @@ List.fold_left
    elems
    ~init:[]
    ~f:(fun acc elem -> acc @ (get_statements_num_violations elem))

