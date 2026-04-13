open Core
open IECCheckerCore

module S = Syntax
module AU = Ast_util

(** Extract the control variable name from for_control.assign statement. *)
let get_ctrl_var_name (ctrl : S.for_control) =
  match ctrl.assign with
  | S.StmExpr (_, S.ExprBin (_, S.ExprVariable (_, vu), S.ASSIGN, _)) ->
    Some (S.VarUse.get_name vu)
  | _ -> None

(** Recursively find assignments to [var_name] in statements.
    Returns a list of warnings for each violating assignment. *)
let rec find_assignments_to var_name stmts =
  List.concat_map stmts ~f:(fun stmt -> find_in_stmt var_name stmt)

and find_in_stmt var_name = function
  | S.StmExpr (_, expr) -> find_in_expr var_name expr
  | S.StmElsif (_, cond_s, body_ss) ->
    find_in_stmt var_name cond_s @ find_assignments_to var_name body_ss
  | S.StmIf (_, cond_s, body_ss, elsif_ss, else_ss) ->
    find_in_stmt var_name cond_s
    @ find_assignments_to var_name body_ss
    @ find_assignments_to var_name elsif_ss
    @ find_assignments_to var_name else_ss
  | S.StmCase (_, cond_s, case_sels, else_ss) ->
    find_in_stmt var_name cond_s
    @ List.concat_map case_sels ~f:(fun cs ->
        find_assignments_to var_name cs.case
        @ find_assignments_to var_name cs.body)
    @ find_assignments_to var_name else_ss
  | S.StmFor (_, _, body_stmts) ->
    find_assignments_to var_name body_stmts
  | S.StmWhile (_, cond_s, body_ss) ->
    find_in_stmt var_name cond_s @ find_assignments_to var_name body_ss
  | S.StmRepeat (_, body_ss, cond_s) ->
    find_assignments_to var_name body_ss @ find_in_stmt var_name cond_s
  | S.StmFuncCall (_, _, func_params) ->
    List.concat_map func_params ~f:(fun fp -> find_in_stmt var_name fp.stmt)
  | S.StmExit _ | S.StmContinue _ | S.StmReturn _ -> []

and find_in_expr var_name = function
  | S.ExprBin (ti, S.ExprVariable (_, vu), S.ASSIGN, _)
    when String.equal (S.VarUse.get_name vu) var_name ->
    let msg =
      Printf.sprintf
        "Loop variable '%s' should not be modified inside a FOR loop"
        var_name
    in
    [Warn.mk ti.linenr ti.col "PLCOPEN-L22" msg]
  | _ -> []

(** Recursively walk statements to find FOR loops at any nesting depth
    and check their bodies for control variable modifications. *)
let rec check_stmts stmts =
  List.concat_map stmts ~f:check_stmt

and check_stmt = function
  | S.StmFor (_, ctrl, body_stmts) -> begin
      let body_warns = match get_ctrl_var_name ctrl with
        | Some var_name -> find_assignments_to var_name body_stmts
        | None -> []
      in
      body_warns @ check_stmts body_stmts
    end
  | S.StmElsif (_, cond_s, body_ss) ->
    check_stmt cond_s @ check_stmts body_ss
  | S.StmIf (_, cond_s, body_ss, elsif_ss, else_ss) ->
    check_stmt cond_s
    @ check_stmts body_ss
    @ check_stmts elsif_ss
    @ check_stmts else_ss
  | S.StmCase (_, cond_s, case_sels, else_ss) ->
    check_stmt cond_s
    @ List.concat_map case_sels ~f:(fun cs ->
        check_stmts cs.case @ check_stmts cs.body)
    @ check_stmts else_ss
  | S.StmWhile (_, cond_s, body_ss) ->
    check_stmt cond_s @ check_stmts body_ss
  | S.StmRepeat (_, body_ss, cond_s) ->
    check_stmts body_ss @ check_stmt cond_s
  | S.StmFuncCall (_, _, func_params) ->
    List.concat_map func_params ~f:(fun fp -> check_stmt fp.stmt)
  | S.StmExpr _ | S.StmExit _ | S.StmContinue _ | S.StmReturn _ -> []

let do_check elems =
  List.concat_map elems ~f:(fun elem ->
      AU.get_top_stmts elem |> check_stmts)

let detector : Detector.t = {
  id = "PLCOPEN-L22";
  name = "Loop variables should not be modified inside a FOR loop";
  summary =
    "Modifying the control variable of a [FOR] loop inside the loop body leads to unpredictable iteration behavior.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-L22";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
