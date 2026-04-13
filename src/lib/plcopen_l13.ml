open Core
open IECCheckerCore

module S = Syntax
module TI = Tok_info
module AU = Ast_util

(** Extract the control variable name from for_control.assign statement. *)
let get_ctrl_var_name (ctrl : S.for_control) =
  match ctrl.assign with
  | S.StmExpr (_, S.ExprBin (_, S.ExprVariable (_, vu), S.ASSIGN, _)) ->
    Some (S.VarUse.get_name vu)
  | _ -> None

(** Collect all locations where [var_name] is referenced in an expression. *)
let rec find_var_in_expr var_name = function
  | S.ExprVariable (ti, vu)
    when String.equal (S.VarUse.get_name vu) var_name -> [ti]
  | S.ExprBin (_, e1, _, e2) ->
    find_var_in_expr var_name e1 @ find_var_in_expr var_name e2
  | S.ExprUn (_, _, e) -> find_var_in_expr var_name e
  | S.ExprFuncCall (_, s) -> find_var_in_stmt var_name s
  | _ -> []

(** Collect all locations where [var_name] is referenced in a statement.
    When encountering a FOR that re-binds [var_name], skip it entirely. *)
and find_var_in_stmt var_name = function
  | S.StmExpr (_, e) -> find_var_in_expr var_name e
  | S.StmElsif (_, cond, body) ->
    find_var_in_stmt var_name cond @ find_var_in_stmts var_name body
  | S.StmIf (_, cond, body, elsif, els) ->
    find_var_in_stmt var_name cond
    @ find_var_in_stmts var_name body
    @ find_var_in_stmts var_name elsif
    @ find_var_in_stmts var_name els
  | S.StmCase (_, cond, sels, els) ->
    find_var_in_stmt var_name cond
    @ List.concat_map sels ~f:(fun cs ->
        find_var_in_stmts var_name cs.case
        @ find_var_in_stmts var_name cs.body)
    @ find_var_in_stmts var_name els
  | S.StmFor (_, ctrl, body) ->
    if Option.equal String.equal (get_ctrl_var_name ctrl) (Some var_name) then
      [] (* Re-binds our variable — skip *)
    else
      find_var_in_stmt var_name ctrl.assign
      @ find_var_in_expr var_name ctrl.range_end
      @ find_var_in_expr var_name ctrl.range_step
      @ find_var_in_stmts var_name body
  | S.StmWhile (_, cond, body) ->
    find_var_in_stmt var_name cond @ find_var_in_stmts var_name body
  | S.StmRepeat (_, body, cond) ->
    find_var_in_stmts var_name body @ find_var_in_stmt var_name cond
  | S.StmFuncCall (_, _, fps) ->
    List.concat_map fps ~f:(fun fp -> find_var_in_stmt var_name fp.stmt)
  | S.StmExit _ | S.StmContinue _ | S.StmReturn _ -> []

(** Scan a statement list for uses of [var_name].
    Stop when we hit a FOR that re-binds the same variable, since that
    FOR's own check (from [check_stmt_list]) covers everything after it. *)
and find_var_in_stmts var_name = function
  | [] -> []
  | (S.StmFor (_, ctrl, _)) :: _
    when Option.equal String.equal (get_ctrl_var_name ctrl) (Some var_name) ->
    []
  | stmt :: rest ->
    find_var_in_stmt var_name stmt @ find_var_in_stmts var_name rest

(** Scan subsequent sibling statements for uses of [var_name].
    Stop when we hit a sibling FOR that re-binds the same variable,
    since that FOR's own check covers everything after it. *)
let rec find_uses_after var_name = function
  | [] -> []
  | (S.StmFor (_, ctrl, _)) :: _
    when Option.equal String.equal (get_ctrl_var_name ctrl) (Some var_name) ->
    []
  | stmt :: rest ->
    find_var_in_stmt var_name stmt @ find_uses_after var_name rest

let mk_warn var_name (ti : TI.t) =
  let msg =
    Printf.sprintf
      "FOR loop variable '%s' should not be used outside the FOR loop"
      var_name
  in
  Warn.mk ti.linenr ti.col "PLCOPEN-L13" msg

(** Walk a statement list sequentially. For each FOR, check all subsequent
    siblings for uses of its control variable. Also recurse into compound
    statements to find nested FOR loops. *)
let rec check_stmt_list stmts =
  match stmts with
  | [] -> []
  | (S.StmFor (_, ctrl, body_stmts)) :: rest ->
    let after_warns = match get_ctrl_var_name ctrl with
      | Some var_name ->
        find_uses_after var_name rest
        |> List.map ~f:(mk_warn var_name)
      | None -> []
    in
    let body_warns = check_stmt_list body_stmts in
    let rest_warns = check_stmt_list rest in
    after_warns @ body_warns @ rest_warns
  | stmt :: rest ->
    let nested_warns = check_nested stmt in
    let rest_warns = check_stmt_list rest in
    nested_warns @ rest_warns

(** Recurse into compound statements to find FOR loops at any depth. *)
and check_nested = function
  | S.StmElsif (_, cond, body) ->
    check_nested cond @ check_stmt_list body
  | S.StmIf (_, cond, body, elsif, els) ->
    check_nested cond
    @ check_stmt_list body
    @ check_stmt_list elsif
    @ check_stmt_list els
  | S.StmCase (_, cond, sels, els) ->
    check_nested cond
    @ List.concat_map sels ~f:(fun cs ->
        check_stmt_list cs.case @ check_stmt_list cs.body)
    @ check_stmt_list els
  | S.StmFor (_, _, body_stmts) ->
    check_stmt_list body_stmts
  | S.StmWhile (_, cond, body) ->
    check_nested cond @ check_stmt_list body
  | S.StmRepeat (_, body, cond) ->
    check_stmt_list body @ check_nested cond
  | S.StmFuncCall (_, _, fps) ->
    List.concat_map fps ~f:(fun fp -> check_nested fp.stmt)
  | S.StmExpr _ | S.StmExit _ | S.StmContinue _ | S.StmReturn _ -> []

let do_check elems =
  List.concat_map elems ~f:(fun elem ->
      AU.get_top_stmts elem |> check_stmt_list)

let detector : Detector.t = {
  id = "PLCOPEN-L13";
  name = "FOR loop variable should not be used outside the FOR loop";
  summary =
    "Referencing a [FOR] loop control variable after [END_FOR] relies on implementation-defined behavior.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-L13";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
