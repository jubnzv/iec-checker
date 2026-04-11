open Core
module S = IECCheckerCore.Syntax
module AU = IECCheckerCore.Ast_util
module Warn = IECCheckerCore.Warn

type occurrence = {
  name : string;
  kind : [`Pou | `Type | `Var];
  linenr : int;
  col : int;
}

let pou_occurrence = function
  | S.IECFunction (_, f) ->
    let ti = S.Function.get_ti f.id in
    Some { name = S.Function.get_name f.id; kind = `Pou;
           linenr = ti.linenr; col = ti.col }
  | S.IECFunctionBlock (_, fb) ->
    let ti = S.FunctionBlock.get_ti fb.id in
    Some { name = S.FunctionBlock.get_name fb.id; kind = `Pou;
           linenr = ti.linenr; col = ti.col }
  | S.IECProgram (_, p) ->
    Some { name = p.name; kind = `Pou; linenr = 0; col = 0 }
  | S.IECClass (_, c) ->
    Some { name = c.class_name; kind = `Pou; linenr = 0; col = 0 }
  | S.IECInterface (_, i) ->
    Some { name = i.interface_name; kind = `Pou; linenr = 0; col = 0 }
  | S.IECType _ | S.IECConfiguration _ -> None

let type_occurrence = function
  | S.IECType (_, (name, _)) ->
    Some { name; kind = `Type; linenr = 0; col = 0 }
  | _ -> None

let var_occurrences elem =
  AU.get_var_decls elem
  |> List.map ~f:(fun vd ->
      let ti = S.VarDecl.get_var_ti vd in
      { name = S.VarDecl.get_var_name vd;
        kind = `Var;
        linenr = ti.linenr;
        col = ti.col })

let collect_occurrences elems =
  List.concat_map elems ~f:(fun elem ->
      (Option.to_list (pou_occurrence elem))
      @ (Option.to_list (type_occurrence elem))
      @ (var_occurrences elem))

let do_check elems =
  let occs = collect_occurrences elems in
  (* Compute distinct kinds per name. *)
  let kinds_by_name : (string, (string, [`Pou|`Type|`Var]) List.Assoc.t) Hashtbl.t =
    Hashtbl.create (module String)
  in
  List.iter occs ~f:(fun o ->
      Hashtbl.update kinds_by_name o.name ~f:(function
          | None -> [(Printf.sprintf "%d:%d" o.linenr o.col, o.kind)]
          | Some kinds -> (Printf.sprintf "%d:%d" o.linenr o.col, o.kind) :: kinds));
  let multi_kind_names =
    Hashtbl.fold kinds_by_name ~init:(Set.empty (module String))
      ~f:(fun ~key:name ~data:entries acc ->
          let distinct =
            List.fold entries ~init:[] ~f:(fun ds (_, k) ->
                if List.mem ds k ~equal:Poly.equal then ds else k :: ds)
          in
          if List.length distinct >= 2 then Set.add acc name else acc)
  in
  List.filter_map occs ~f:(fun o ->
      if Set.mem multi_kind_names o.name then
        let msg = Printf.sprintf
            "Name %s is used for multiple element kinds" o.name
        in
        Some (Warn.mk o.linenr o.col "PLCOPEN-N9" msg)
      else None)

let detector : Detector.t = {
  id = "PLCOPEN-N9";
  name = "Different element types should not bear the same name";
  summary =
    "A name used for a variable, a POU or a UDT must not also be used for a \
     different element kind.";
  doc_url = "https://iec-checker.github.io/docs/detectors/PLCOPEN-N9";
  check = (fun (i : Detector.inputs) -> do_check i.elements);
}
