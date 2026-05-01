module S = IECCheckerCore.Syntax
module Warn = IECCheckerCore.Warn

let are_var_names_equal v v' =
  S.VarUse.get_name v = S.VarUse.get_name v'

let rec check_expr = function
  | S.ExprBin (ti, S.ExprVariable (_, v), S.ASSIGN, S.ExprVariable (_, v')) 
    when are_var_names_equal v v' ->
      let var_name = S.VarUse.get_name v in
      [Warn.mk
        ti.linenr
        ti.col
        "Self-assignment"
        (Printf.sprintf
          "Variable '%s' is assigned to itself."
          var_name)]
  
  | S.ExprBin (_, e, _, e') ->
    check_expr e @ check_expr e'

  | S.ExprUn (_, _, e) ->
    check_expr e

  | S.ExprFuncCall (_, stmt) ->
    check_statement stmt

  | _ ->
    []

and check_statement = function
  | S.StmExpr (_, e) ->
    check_expr e

  | S.StmElsif (_, stmt, stmts) ->
    List.concat [
      check_statement stmt;
      check_statements stmts;
    ]

  | S.StmIf (_, stmt, stmts, stmts', stmts'') ->
    List.concat [
      check_statement stmt;
      check_statements stmts;
      check_statements stmts';
      check_statements stmts'';
    ]

  | S.StmCase (_, stmt, css, stmts) ->
    List.concat [
      check_statement stmt;
      check_case_selections css;
      check_statements stmts;
    ]

  | S.StmFor (_, fc, stmts) ->
    List.concat [
      check_statement fc.assign;
      check_expr fc.range_end;
      check_expr fc.range_step;
      check_statements stmts;
    ]

  | S.StmWhile (_, stmt, stmts) ->
    List.concat [
      check_statement stmt;
      check_statements stmts;
    ]

  | S.StmRepeat (_, stmts, stmt) ->
    List.concat [
      check_statements stmts;
      check_statement stmt;
    ]

  | S.StmEmpty _
  | S.StmExit _
  | S.StmContinue _
  | S.StmReturn _ ->
    []

  | S.StmFuncCall (_, _, assigns) ->
    check_func_param_assignments assigns

and check_case_selections css =
  List.concat_map
    (fun (cs : S.case_selection) ->  
      check_statements cs.case
      @ check_statements cs.body)
  css

and check_func_param_assignments assigns =
  List.concat_map
    (fun (a : S.func_param_assign) ->
      check_statement a.stmt)
    assigns

and check_statements stmts =
  List.concat_map check_statement stmts

let check_element = function
  | S.IECFunction (_, f) ->
    check_statements f.statements

  | S.IECFunctionBlock (_, fb) ->
    check_statements fb.statements

  | S.IECProgram (_, p) ->
    check_statements p.statements

  | S.IECClass (_, c) ->
    List.concat_map
      (fun (m : S.method_decl) ->
        check_statements m.statements)
    c.methods

  | S.IECInterface _
  | S.IECConfiguration _
  | S.IECType _ ->
    []

let do_check elems =
  List.concat_map check_element elems

let detector : Detector.t = {
  id = "SELF-ASSIGNMENT";
  name = "Detect self-assignments";
  summary =
    "Assignments where a variable is assigned to itself should be avoided.";
  doc_url = "";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
