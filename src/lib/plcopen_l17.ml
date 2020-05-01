open Core_kernel
module S = IECCheckerCore.Syntax
module TI = IECCheckerCore.Tok_info
module Warn = IECCheckerCore.Warn
module AU = IECCheckerCore.Ast_util

let check_stmt = function
  | S.StmIf (ti, _, _, _, else_exprs) -> (
      match else_exprs with
      | [] ->
        let msg = "Each IF instruction should have an ELSE clause" in
        let w = Warn.mk ti.linenr ti.col "PLCOPEN-L17" msg in
        Some w
      | _ -> None )
  | _ -> None

let do_check elems =
  let stmts = AU.get_stmts elems in
  List.map stmts ~f:(fun s -> check_stmt s)
  |> List.filter ~f:(fun w -> match w with Some _ -> true | None -> false)
  |> List.map ~f:(fun w ->
      match w with Some w -> w | None -> assert false)
