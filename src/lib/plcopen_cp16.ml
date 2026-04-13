open Core
open IECCheckerCore

module S = Syntax

(** Collect all known FUNCTION_BLOCK type names from the element list. *)
let collect_fb_names elems =
  List.filter_map elems ~f:(function
      | S.IECFunctionBlock (_, fb) -> Some (S.FunctionBlock.get_name fb.id)
      | _ -> None)
  |> Set.of_list (module String)

(** Check a single element for CP16 violations. *)
let check_elem fb_names = function
  | S.IECConfiguration (_, cfg) ->
    List.concat_map cfg.resources ~f:(fun res ->
        List.filter_map res.programs ~f:(fun pc ->
            match S.ProgramConfig.get_type_name pc,
                  S.ProgramConfig.get_task pc with
            | Some tn, Some _ when Set.mem fb_names tn ->
              let ti = S.ProgramConfig.get_ti pc in
              let msg =
                Printf.sprintf
                  "Task should call PROGRAM, not FUNCTION_BLOCK '%s'"
                  tn
              in
              Some (Warn.mk ti.linenr ti.col "PLCOPEN-CP16" msg)
            | _ -> None))
  | _ -> []

let do_check elems =
  let fb_names = collect_fb_names elems in
  if Set.is_empty fb_names then []
  else List.concat_map elems ~f:(check_elem fb_names)

let detector : Detector.t = {
  id = "PLCOPEN-CP16";
  name = "Tasks shall only call program POUs and not function blocks";
  summary =
    "A task in a RESOURCE block should only execute PROGRAM instances, \
     not FUNCTION_BLOCK instances.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP16";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
