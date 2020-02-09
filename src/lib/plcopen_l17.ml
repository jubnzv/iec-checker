(** PLCOPEN-L17 – Each IF instruction should have an ELSE clause

    Reference: PLCopen Coding Guidelines 6.5.9. *)

open Core_kernel
module S = IECCheckerCore.Syntax
module TI = IECCheckerCore.Tok_info

let check stmts =
  let check_stmt = function
    | S.StmIf (ti, _, _, _, else_exprs) -> (
        match else_exprs with
        | [] ->
            Printf.printf
              "PLCOPEN-L17 violation: (%d:%d): Each IF instruction should have \
               an ELSE clause"
              ti.linenr ti.col;
            ()
        | _ -> () )
    | _ -> ()
  in
  List.iter stmts ~f:check_stmt
