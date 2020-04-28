open Core_kernel
module S = IECCheckerCore.Syntax
module Env = IECCheckerCore.Env
module TI = IECCheckerCore.Tok_info

let print_element (e : S.iec_library_element) =
  match e with
  | S.IECFunction f ->
      Printf.printf "Running check for function %s\n" (S.Function.get_name f.id)
  | S.IECFunctionBlock fb ->
      Printf.printf "Running check for function block %s\n"
        (S.FunctionBlock.get_name fb.id)
  | S.IECProgram p -> Printf.printf "Running check for program %s\n" p.name
  | S.IECConfiguration c ->
      Printf.printf "Running check for configuration %s\n" c.name
  | S.IECType _ ->
      Printf.printf "Running check for derived type\n"

let[@warning "-27"] run_all_checks elements envs quiet =
  if not quiet then
      List.iter elements ~f:(fun e -> print_element e);
  Plcopen_n3.do_check elements
  |> List.append (Plcopen_l17.do_check elements)
  |> List.append (Plcopen_cp13.do_check elements)
  |> List.append (Zerodiv.do_check elements)
