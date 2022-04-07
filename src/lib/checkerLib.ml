open Core
module S = IECCheckerCore.Syntax
module Env = IECCheckerCore.Env
module TI = IECCheckerCore.Tok_info
module Config = IECCheckerCore.Config

let print_element (e : S.iec_library_element) =
  match e with
  | S.IECFunction (_, f) ->
    Printf.printf "Running check for function %s\n" (S.Function.get_name f.id)
  | S.IECFunctionBlock (_, fb) ->
    Printf.printf "Running check for function block %s\n"
      (S.FunctionBlock.get_name fb.id)
  | S.IECProgram (_, p) -> Printf.printf "Running check for program %s\n" p.name
  | S.IECClass (_, c) -> Printf.printf "Running check for class %s\n" c.class_name
  | S.IECInterface (_, i) -> Printf.printf "Running check for interafece %s\n" i.interface_name
  | S.IECConfiguration (_, c) ->
    Printf.printf "Running check for configuration %s\n" c.name
  | S.IECType _ ->
    Printf.printf "Running check for derived type\n"

let run_all_checks elements envs cfgs quiet =
  if not quiet then
    List.iter elements ~f:(fun e -> print_element e);
  []
  |> List.append (if Config.check_plcopen_cp1 then Plcopen_cp1.do_check elements else [])
  |> List.append (if Config.check_plcopen_cp2 then Plcopen_cp2.do_check cfgs else [])
  |> List.append (if Config.check_plcopen_cp3 then Plcopen_cp3.do_check elements else [])
  |> List.append (if Config.check_plcopen_cp4 then Plcopen_cp4.do_check elements else [])
  |> List.append (if Config.check_plcopen_cp6 then Plcopen_cp6.do_check elements else [])
  |> List.append (if Config.check_plcopen_cp8 then Plcopen_cp8.do_check elements else [])
  |> List.append (if Config.check_plcopen_cp9 then Plcopen_cp9.do_check elements cfgs else [])
  |> List.append (if Config.check_plcopen_cp13 then Plcopen_cp13.do_check elements else [])
  |> List.append (if Config.check_plcopen_cp25 then Plcopen_cp25.do_check elements envs else [])
  |> List.append (if Config.check_plcopen_cp28 then Plcopen_cp28.do_check elements else [])
  |> List.append (if Config.check_plcopen_l10 then Plcopen_l10.do_check elements else [])
  |> List.append (if Config.check_plcopen_l17 then Plcopen_l17.do_check elements else [])
  |> List.append (if Config.check_plcopen_n3 then Plcopen_n3.do_check elements else [])
  |> List.append (Zerodiv.do_check elements)
