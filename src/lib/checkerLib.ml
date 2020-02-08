open Core_kernel

module S = IECCheckerCore.Syntax
module TI = IECCheckerCore.Tok_info

let run_all_checks (elements : S.iec_library_element list) =
  let print_element (e : S.iec_library_element) =
    match e with
    | S.IECFunction f ->
        Printf.printf "Running check for function %s\n"
          (S.Function.get_name f.id)
    | S.IECFunctionBlock fb ->
        Printf.printf "Running check for function block %s\n"
          (S.FunctionBlock.get_name fb.id)
    | S.IECProgram p -> Printf.printf "Running check for program %s\n" p.name
    | S.IECConfiguration c -> Printf.printf "Running check for configuration %s\n" c.name
  in
  let check_statements (e : S.iec_library_element) =
    match e with
    (* | S.IECFunction f -> Sa0000.check f.statements
    | S.IECFunctionBlock fb -> Sa0000.check fb.statements
    | S.IECProgram p -> Sa0000.check p.statements *)
    | S.IECFunction f -> Plcopen_l17.check f.statements
    | S.IECFunctionBlock fb -> Plcopen_l17.check fb.statements
    | S.IECProgram p -> Plcopen_l17.check p.statements
    | S.IECConfiguration _ -> ()
  in
  let check_variables (e : S.iec_library_element) =
    match e with
    | S.IECFunction f ->
        Plcopen_n3.check f.variables;
    | S.IECFunctionBlock fb ->
        Plcopen_n3.check fb.variables;
    | S.IECProgram p ->
        Plcopen_n3.check p.variables;
    | S.IECConfiguration c ->
        Plcopen_n3.check c.variables;
  in
  List.iter elements ~f:print_element;
  List.iter elements ~f:check_statements;
  List.iter elements ~f:check_variables
