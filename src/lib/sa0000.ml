(* Demo check: Find division to a zero constant. *)

open Core_kernel
module S = IECCheckerCore.Syntax
module TI = IECCheckerCore.Tok_info

let check (statements : S.expr list) =
  let rec print_assign s =
    match s with
    | S.BinExpr (e1, op, e2) -> (
        match (e1, op, e2) with
        | S.BinExpr _, _, _ -> print_assign e1
        | _, _, S.BinExpr _ -> print_assign e2
        | S.Variable lhs, S.DIV, S.Constant rhs ->
            if S.c_is_zero rhs then
              let name = S.Variable.get_name lhs in
              let ti = S.Variable.get_ti lhs in
              Printf.printf
                "SA000: Zero division: variable %s (%d:%d) divided to zero!\n"
                name ti.linenr ti.col
        | S.Constant lhs, S.DIV, S.Constant rhs ->
            if S.c_is_zero rhs then
              let v_str = S.c_get_str_value lhs in
              let ti = S.c_get_ti lhs in
              Printf.printf
                "SA000: Zero division: constant %s (%d:%d) divided to zero!\n"
                v_str ti.linenr ti.col
        | _ -> () )
    | _ -> ()
  in
  List.iter statements ~f:print_assign
