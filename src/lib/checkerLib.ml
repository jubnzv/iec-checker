open Core
module S = IECCheckerCore.Syntax

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

let registered_detectors : Detector.t list = [
  Plcopen_cp1.detector;
  Plcopen_cp2.detector;
  Plcopen_cp3.detector;
  Plcopen_cp4.detector;
  Plcopen_cp6.detector;
  Plcopen_cp8.detector;
  Plcopen_cp9.detector;
  Plcopen_cp13.detector;
  Plcopen_cp16.detector;
  Plcopen_cp17.detector;
  Plcopen_cp25.detector;
  Plcopen_cp26.detector;
  Plcopen_cp28.detector;
  Plcopen_l10.detector;
  Plcopen_l13.detector;
  Plcopen_l17.detector;
  Plcopen_l22.detector;
  Plcopen_n1.detector;
  Plcopen_n2.detector;
  Plcopen_n3.detector;
  Plcopen_n4.detector;
  Plcopen_n5.detector;
  Plcopen_n6.detector;
  Plcopen_n8.detector;
  Plcopen_n9.detector;
  Plcopen_n10.detector;
]

let detector_enabled (cfg : IECCheckerCore.Config.t) (d : Detector.t) =
  match cfg.enabled_detectors with
  | _ :: _ as ids -> List.mem ids d.id ~equal:String.equal
  | [] -> not (List.mem cfg.disabled_detectors d.id ~equal:String.equal)

let run_all_checks elements envs cfgs quiet =
  let cfg = IECCheckerCore.Config.get () in
  let detectors = List.filter registered_detectors ~f:(detector_enabled cfg) in
  if not quiet then
    List.iter elements ~f:(fun e -> print_element e);
  let inputs = Detector.{ elements; envs; cfgs } in
  List.concat_map detectors ~f:(fun d -> d.check inputs)
