open Core
open IECCheckerCore

module S = Syntax
module AU = Ast_util

(** Collect the names of VAR_EXTERNAL variables declared in an element. *)
let get_external_var_names elem =
  AU.get_var_decls elem
  |> List.filter_map ~f:(fun vd ->
      match S.VarDecl.get_attr vd with
      | Some (S.VarDecl.VarExternal _) -> Some (S.VarDecl.get_var_name vd)
      | _ -> None)
  |> Set.of_list (module String)

(** Find all assignment expressions targeting one of [ext_vars].
    Returns a list of (var_name, tok_info) pairs. *)
let find_external_writes elem ext_vars =
  AU.get_pou_exprs elem
  |> List.filter_map ~f:(fun expr ->
      match expr with
      | S.ExprBin (ti, S.ExprVariable (_, vu), op, _)
        when (phys_equal op S.ASSIGN || phys_equal op S.ASSIGN_REF)
             && Set.mem ext_vars (S.VarUse.get_name vu) ->
        Some (S.VarUse.get_name vu, ti)
      | _ -> None)

(** For each PROGRAM, collect (var_name, prog_name, ti) triples for every
    write to a VAR_EXTERNAL variable. *)
let collect_all_writes elems =
  List.concat_map elems ~f:(fun elem ->
      match elem with
      | S.IECProgram (_, p) ->
        let ext_vars = get_external_var_names elem in
        find_external_writes elem ext_vars
        |> List.map ~f:(fun (var_name, ti) -> (var_name, p.name, ti))
      | _ -> [])

let do_check elems =
  let all_writes = collect_all_writes elems in
  (* Group by var_name, preserving insertion order. *)
  let groups =
    List.fold all_writes ~init:(Map.empty (module String))
      ~f:(fun map (var_name, prog_name, ti) ->
          Map.update map var_name ~f:(function
              | None -> [(prog_name, ti)]
              | Some ws -> ws @ [(prog_name, ti)]))
  in
  (* For each global written by more than one PROGRAM, flag all writes
     except those from the first writing PROGRAM. *)
  Map.fold groups ~init:[] ~f:(fun ~key:var_name ~data:writes acc ->
      let writing_progs =
        List.map writes ~f:fst
        |> List.dedup_and_sort ~compare:String.compare
      in
      if List.length writing_progs <= 1 then acc
      else
        let first_prog = fst (List.hd_exn writes) in
        List.fold writes ~init:acc ~f:(fun acc (prog_name, ti) ->
            if String.equal prog_name first_prog then acc
            else
              let msg =
                Printf.sprintf
                  "Global variable '%s' should be written by only one \
                   PROGRAM (already written in '%s')"
                  var_name first_prog
              in
              acc @ [Warn.mk ti.linenr ti.col "PLCOPEN-CP26" msg]))

let detector : Detector.t = {
  id = "PLCOPEN-CP26";
  name = "A global variable may be written only by one PROGRAM";
  summary =
    "When multiple PROGRAMs write the same global variable, the resulting \
     value depends on scheduling order, creating a race condition.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-CP26";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
