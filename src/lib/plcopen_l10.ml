open Core_kernel
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
