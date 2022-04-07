(** PLCOpen XML reference: https://www.plcopen.org/system/files/downloads/tc6_xml_v201_technical_doc.pdf
    See also test/plcopen directory. *)
open Core
module S = Syntax

exception XMLError of string

(** Skip the current element in Xmlm traverse. *)
let rec xmlm_skip i d =
  if Xmlm.eoi i then () else
    match Xmlm.input i with
    | `El_start _ -> xmlm_skip i (d + 1)
    | `El_end -> if d = 1 then () else xmlm_skip i (d - 1)
    | _ -> xmlm_skip i d

(** Representation of POU from XML schema parse tree.
    These nodes are described in <pous> tags. *)
module POUNode = struct

  (** "Use" occurrence of IEC type. *)
  type iec_ty_use = {
    ty_name: string; (** String representation of this type *)
    is_pointer: bool;
  }

  (* See: tc6_xml_v201_technical_doc page 29. *)
  type pou_var = {
    name: string;
    var_ty: iec_ty_use option;
    address: string;
    global_id: int option;
  }

  let mk_pou_var () =
    let name = ""
    and var_ty = None
    and address = ""
    and global_id = None in
    { name; var_ty; address; global_id }

  type pou_var_list_ty =
    | VarLocal
    | VarTemp
    | VarInput
    | VarOutput
    | VarInOut
    | VarExternal
    | VarGlobal
    | VarAccess

  let pou_var_list_ty_to_string = function
    | VarLocal    -> "VAR"
    | VarTemp     -> "VAR_TEMP"
    | VarInput    -> "VAR_INPUT"
    | VarOutput   -> "VAR_OUTPUT"
    | VarInOut    -> "VAR_IN_OUT"
    | VarExternal -> "VAR_EXTERNAL"
    | VarGlobal   -> "VAR_GLOBAL"
    | VarAccess   -> "VAR_ACCESS"

  (* See: tc6_xml_v201_technical_doc page 27 *)
  type pou_var_list = {
    ty: pou_var_list_ty;
    name: string;
    constant: bool;
    retain: bool;
    nonretain: bool;
    persistent: bool;
    nonpersistent: bool;
    mutable vars: pou_var list;
  }

  let mk_pou_var_list ty =
    let name = ""
    and constant = false
    and retain = false
    and nonretain = false
    and persistent = false
    and nonpersistent = false
    and vars = [] in
    { ty; name; constant; retain; nonretain; persistent; nonpersistent; vars }

  type node_type =
    | Program
    | Function
    | FunctionBlock

  type t = {
    ty: node_type;
    return_ty: iec_ty_use option; (** Return type of Function and FBs *)
    name: string; (** POU name *)
    mutable var_lists: pou_var_list list; (** POU variables *)
    body: string; (** Body source code *)
  }

  let mk ty =
    let return_ty = None in
    let name = "" in
    let var_lists = [] in
    let body = "" in
    { ty; return_ty; name; var_lists; body; }

  let extract_source_ty ty =
    if ty.is_pointer then
      "REF_TO " ^ ty.ty_name
    else
      ty.ty_name

  (** Extract source code of a single variable *)
  let extract_source_var (var : pou_var) =
    let ty_str = match var.var_ty with
      | Some ty -> extract_source_ty ty
      | None -> raise @@ XMLError (Printf.sprintf "Variable type is missing: %s" var.name)
    in
    Printf.sprintf "%s : %s;" var.name ty_str

  (** Extract source code of variables list *)
  let extract_source_var_list (vl : pou_var_list) =
    let get_attrs (vl : pou_var_list) =
      let (acc : string list) = [] in
      let acc = if not (String.is_empty vl.name) then acc @ [vl.name] else acc in
      let acc = if vl.constant then acc @ ["CONSTANT"] else acc in
      let acc = if vl.retain then acc @ ["RETAIN"] else acc in
      let acc = if vl.nonretain then acc @ ["NON_RETAIN"] else acc in
      (* FIXME: I have no idea what does this means. These keywords are
         missing in the Standard. *)
      (* let acc = if vl.persistent then acc @ ["PERSISTENT"] else acc in        *)
      (* let acc = if vl.nonpersistent then acc @ ["NON_PERSISTENT"] else acc in *)
      acc |> String.concat ~sep:" "
    in
    let var_list_hdr = pou_var_list_ty_to_string vl.ty
    and attrs = get_attrs vl
    and vars_src = List.fold_left
        vl.vars
        ~init:[]
        ~f:(fun acc v -> acc @ [(extract_source_var v)])
                   |> String.concat ~sep:"\n"
    in
    Printf.sprintf "%s %s\n%s\nEND_VAR" var_list_hdr attrs vars_src

  (** [extract_source_exn node] Extract IEC61131 source code from [node]. *)
  let extract_source_exn (node : t) : string =
    let extract_program n vars_src body =
      Printf.sprintf "PROGRAM %s\n%s\n%s\nEND_PROGRAM\n" n.name vars_src body
    in
    let extract_function n vars_src body =
      let rt = match n.return_ty with
        | Some ty -> extract_source_ty ty
        | None -> raise @@ XMLError ("Undefined return type for function " ^ n.name)
      in
      Printf.sprintf "FUNCTION %s : %s\n%s\n%s\nEND_FUNCTION\n" n.name rt vars_src body
    in
    let extract_fb n vars_src body =
      Printf.sprintf "FUNCTION_BLOCK %s\n%s\n%s\nEND_FUNCTION_BLOCK\n" n.name vars_src body
    in
    let vars_src =
      List.fold_left
        node.var_lists
        ~init:[]
        ~f:(fun acc vl -> begin
              acc @ [(extract_source_var_list vl)]
            end)
      |> String.concat ~sep:"\n"
    in
    let body = if String.is_empty node.body then "{}" else node.body in
    let src = match node.ty with
      | Program -> extract_program node vars_src body
      | Function -> extract_function node vars_src body
      | FunctionBlock -> extract_fb node vars_src body
    in
    src

  (** Read data content for source code element *)
  let rec pull_src i d acc =
    match Xmlm.input i with
    (* CodeSyS use these tags inside source code, e.g.: <ST><xhtml ...> *)
    | `El_start ((_, tag), _) when (String.equal tag "xhtml") -> begin
        pull_src i (d + 1) acc
      end
    | `El_end -> if (d <= 1) then acc else pull_src i (d - 1) acc
    | `Data v -> pull_src i d v
    | _ -> begin
        let (linenr, col) = Xmlm.pos i in
        raise @@ XMLError (Printf.sprintf "XML is broken at %d:%d" linenr col)
      end

  (** Pull IEC61131-3 type use occurrence *)
  let pull_type_use i =
    let rec aux i d (ty : iec_ty_use) =
      match Xmlm.input i with
      | `El_start ((_, tag),_) when (String.equal tag "pointer") -> begin
          aux i (d + 1) { ty with is_pointer = true }
        end
      | `El_start ((_, tag), attrs) when (String.equal tag "derived") -> begin
          let ty_name_opt = List.find attrs ~f:(fun ((_,k),_) -> String.equal k "name") in
          match ty_name_opt with
          | Some (_,v) -> aux i (d + 1) { ty with ty_name = v }
          | None -> begin
              let (linenr, col) = Xmlm.pos i in
              raise @@ XMLError (Printf.sprintf "Unknown derived type name at %d:%d" linenr col)
            end
        end
      (* Elementary type, e.g. <BOOL /> *)
      | `El_start ((_, tag), _) -> aux i (d + 1) { ty with ty_name = tag }
      | `Data _ -> aux i d ty
      | `El_end -> if (phys_equal d 0) then ty else aux i (d - 1) ty
      | _ -> begin
          let (linenr, col) = Xmlm.pos i in
          raise @@ XMLError (Printf.sprintf "XML is broken at %d:%d" linenr col)
        end
    in
    aux i 0 ({ ty_name = ""; is_pointer = false })

  (** Pull a single variable *)
  let pull_var i attrs =
    (** Set variable attributes *)
    let set_attrs (var : pou_var) attrs =
      List.fold_left
        attrs
        ~init:var
        ~f:(fun var ((_,k),v) -> begin
              let ( == ) = String.equal in
              if      (k == "name")      then { var with name = v }
              else if (k == "address")   then { var with address = v }
              else if (k == "global_id") then { var with global_id = Some((int_of_string v)) }
              else raise @@ XMLError (Printf.sprintf "Unexpected variable attribute: %s" k)
            end)
    in
    let rec aux i d var =
      if Xmlm.eoi i then var else
        match Xmlm.input i with
        | `El_end -> if (phys_equal d 0) then var else aux i (d - 1) var
        | `Data _ -> aux i d var
        (* Pull variable type *)
        | `El_start ((_, tag), _) when (String.equal tag "type") -> begin
            let ty = pull_type_use i in
            aux i d { var with var_ty = Some(ty) }
          end
        | `El_start _ -> aux i (d + 1) var
        | _ -> aux i (d + 1) var
    in
    let var = set_attrs (mk_pou_var ()) attrs in
    aux i 1 var

  (** Pull variables list *)
  let pull_var_list i ty (attrs : Xmlm.attribute list) : pou_var_list =
    (** Set variables list attributes *)
    let set_attrs (vl : pou_var_list) attrs =
      List.fold_left
        attrs
        ~init:vl
        ~f:(fun vl ((_,k),v) -> begin
              let ( == ) = String.equal in
              if      (k == "name")          then { vl with name = v }
              else if (k == "constant")      then { vl with constant = (bool_of_string v) }
              else if (k == "reatin")        then { vl with retain = (bool_of_string v) }
              else if (k == "nonretain")     then { vl with nonretain = (bool_of_string v) }
              else if (k == "persistent")    then { vl with persistent = (bool_of_string v) }
              else if (k == "nonpersistent") then { vl with nonpersistent = (bool_of_string v) }
              else raise @@ XMLError (Printf.sprintf "Unexpected varList attribute: %s" k)
            end)
    in
    (* Pull variables from this list *)
    let rec aux i d acc =
      match Xmlm.input i with
      | `El_start ((_, tag), attrs) when (String.equal tag "variable") -> begin
          acc @ [pull_var i attrs]
        end
      | `Data _ -> aux i d acc
      | `El_end -> if (phys_equal d 0) then acc else aux i (d - 1) acc
      | _ -> begin
          let (linenr, col) = Xmlm.pos i in
          raise @@ XMLError (Printf.sprintf "Unexpected variable at %d:%d" linenr col)
        end
    in
    let vl = set_attrs (mk_pou_var_list ty) attrs
    and vars = aux i 0 [] in
    { vl with vars }

  (** Pull POU interfaces from the XML element. *)
  let pull_interfaces i (pou : t) : t =
    let rec aux i d acc =
      if Xmlm.eoi i then acc else
        match Xmlm.input i with
        | `El_start ((_, tag), _) when (String.equal tag "returnType") -> begin
            let rt = pull_type_use i in
            let rt_opt = if String.is_empty rt.ty_name then None else Some(rt) in
            let acc = { acc with return_ty = rt_opt } in
            aux i d acc
          end
        (* NOTE: According to tc6_xml_v201_technical_doc the valid PLCOpen tags
           are end with "Vars" suffix. But definitions with "Variables" suffix
           are used in Beremiz IDE (https://beremiz.org/) for some reasons. *)
        | `El_start ((_, tag), attrs)
          when (List.mem ["localVars"; "localVariables"] tag ~equal:String.equal) ->
          begin
            acc.var_lists <- acc.var_lists @ [(pull_var_list i VarLocal attrs)];
            aux i d acc
          end
        | `El_start ((_, tag), attrs)
          when (List.mem ["tempVars"; "tempVariables"] tag ~equal:String.equal) ->
          begin
            acc.var_lists <- acc.var_lists @ [(pull_var_list i VarTemp attrs)];
            aux i d acc
          end
        | `El_start ((_, tag), attrs)
          when (List.mem ["inputVars"; "inputVariables"] tag ~equal:String.equal) ->
          begin
            acc.var_lists <- acc.var_lists @ [(pull_var_list i VarInput attrs)];
            aux i d acc
          end
        | `El_start ((_, tag), attrs)
          when (List.mem ["outputVars"; "outputVariables"] tag ~equal:String.equal) ->
          begin
            acc.var_lists <- acc.var_lists @ [(pull_var_list i VarOutput attrs)];
            aux i d acc
          end
        | `El_start ((_, tag), attrs)
          when (List.mem ["inOutVars"; "inOutVariables"] tag ~equal:String.equal) ->
          begin
            acc.var_lists <- acc.var_lists @ [(pull_var_list i VarInOut attrs)];
            aux i d acc
          end
        | `El_start ((_, tag), attrs)
          when (List.mem ["externalVars"; "externalVariables"] tag ~equal:String.equal) ->
          begin
            acc.var_lists <- acc.var_lists @ [(pull_var_list i VarExternal attrs)];
            aux i d acc
          end
        | `El_start ((_, tag), attrs)
          when (List.mem ["globalVars"; "globalVariables"] tag ~equal:String.equal) ->
          begin
            acc.var_lists <- acc.var_lists @ [(pull_var_list i VarGlobal attrs)];
            aux i d acc
          end
        | `El_start ((_, tag), attrs)
          when (List.mem ["accessVars"; "accessVariables"] tag ~equal:String.equal) ->
          begin
            acc.var_lists <- acc.var_lists @ [(pull_var_list i VarAccess attrs)];
            aux i d acc
          end
        | `El_end -> begin
            if (phys_equal d 1) then begin
              acc
            end
            else aux i (d - 1) acc
          end
        | `El_start _ -> aux i (d + 1) acc
        | _ -> aux i d acc
    in
    aux i 0 pou

  (** Pull source code body from XML element. *)
  let rec pull_body i d data =
    if Xmlm.eoi i then data else
      match Xmlm.input i with
      | `El_start ((_, tag), _) when (String.equal tag "ST") -> begin
          let data = pull_src i 0 "" in
          pull_body i (d + 1) data
        end
      | `El_start ((_, tag), _) when (String.equal tag "body") -> begin
          pull_body i (d + 1) data
        end
      | `El_end -> if (phys_equal d 1)
        then begin
          data
        end
        else begin
          pull_body i (d - 1) data
        end
      | `El_start _ -> xmlm_skip i 1; pull_body i d data
      | _ -> pull_body i d data

  (** [from_xml_exn xml_object attrs] Create POUNode.t from a given [xml_object]. *)
  let from_xml_exn el (attrs : Xmlm.attribute list) : t =
    let mk_from_attrs_exn (attrs : Xmlm.attribute list) : t =
      let (ty_opt, name_opt) = List.fold_left
          attrs
          ~init:(None (* ty *), None (* name *))
          ~f:(fun (acc_ty, acc_name) ((_,k),v) -> begin
                if (String.equal k "name") then begin
                  (acc_ty, Some(v))
                end
                else if (String.equal k "pouType") then begin
                  if (String.equal v "program") then
                    (Some(Program),  acc_name)
                  else if (String.equal v "function") then
                    (Some(Function), acc_name)
                  else if (String.equal v "functionBlock") then
                    (Some(FunctionBlock), acc_name)
                  else
                    raise @@ XMLError (Printf.sprintf "Unexpected POU type: <%s>" v)
                end
                else begin
                  raise @@ XMLError (Printf.sprintf "Unexpected XML attribute: %s" k)
                end
              end)
      in
      let ty = match ty_opt with
        | Some v -> v
        | None -> raise @@ XMLError (Printf.sprintf "Missing POU type")
      in
      let name = match name_opt with
        | Some v -> v
        | None -> raise @@ XMLError (Printf.sprintf "Missing POU name")
      in
      let pou = mk ty in
      { pou with name = name }
    in
    let rec pull i pou d =
      if Xmlm.eoi i then pou else
        match Xmlm.peek i with
        (* Pull interfaces: variables, return type, etc. *)
        | `El_start ((_, tag), _) when (String.equal tag "interface") -> begin
            let pou = pull_interfaces i pou in
            pull i pou d
          end
        (* Pull ST source code from the body *)
        | `El_start ((_, tag), _) when (String.equal tag "body") -> begin
            let pou = { pou with body = pull_body i 0 ""} in
            pull i pou d
          end
        | `El_start _ -> begin
            xmlm_skip i 0;
            pull i pou d
          end
        | `El_end -> begin
            if (phys_equal d 0) then begin
              pou
            end
            else pull i pou (d - 1)
          end
        | `Dtd _ | `Data _ -> Xmlm.input i |> Common.ignore; pull i pou d
    in
    let pou = mk_from_attrs_exn attrs in
    pull el pou 0
end

(** Representation of data type from XML schema parse tree.
    These nodes are described in <dataTypes> tags. *)
(* module TypeNode = struct *)
(* end                      *)

(** Representation of configuration objects from XML schema parse tree.
    These nodes are described in <configurations> tags inside <instances> tag.
    Reference: tc6_xml_v201_technical_doc part 7. *)
module ConfigurationNode = struct

  type pou_instance = {
    name: string;
    type_name: string;
    global_id : int option;
    (* add_data *)
  }

  type task = {
    name: string;
    single: string option;
    interval : string option;
    priority: int;
    global_id : int option;
    pou_configurations : pou_instance list;
    (* add_data *)
  }

  type resource = {
    name: string;
    global_id : int option;
    tasks: task list;
    (* global_vars  *)
    pou_configurations : pou_instance list;
    (* add_data *)
  }

  type t = {
    name: string;
    global_id : int option;
    resources : resource list;
    (* global_vars  *)
    (* access_vars *)
    (* add_data *)
  }

  (** [from_xml_exn xml_object attrs] Create ConfigurationNode.t from the given
      [xml_object]. *)
  let [@warning "-27"] from_xml_exn el (attrs : Xmlm.attribute list) : t =
    let mk_from_attrs (attrs : Xmlm.attribute list) : t =
      let name_opt =
        List.find ~f:(fun ((_,k),_) -> String.equal "name" k) attrs in
      let name = match name_opt with
        | Some (_,v) -> v
        | None -> raise @@ XMLError "Configuration name is undefined"
      in
      { name; global_id = None; resources = []; }
    in
    mk_from_attrs attrs
end

type parse_tree = {
  mutable pous: POUNode.t list;
  mutable configurations : ConfigurationNode.t list;
}

(** [reconstruct_tree pt] Create IEC61131-3 listing from the given [pt]. *)
let reconstruct_tree (pt : parse_tree) : string =
  let res =
    List.fold_left
      pt.pous
      ~init:""
      ~f:(fun acc p -> acc ^ "\n" ^ POUNode.extract_source_exn p)
  in
  Printf.printf "%s\n" res;
  res

let pull_pous i =
  let rec pull_pous_aux i (d : int) (acc : POUNode.t list) : POUNode.t list =
    let skip () = pull_pous_aux i d acc in
    if Xmlm.eoi i then acc else
      match Xmlm.input i with
      | `El_start ((_, tag), attrs) when (String.equal tag "pou") -> begin
          let pou = POUNode.from_xml_exn i attrs in
          pou :: (pull_pous_aux i (d + 1) acc)
        end
      | `El_end -> begin
          if (phys_equal d 1) then begin
            acc
          end else pull_pous_aux i (d - 1) acc
        end
      | `El_start _ | `Dtd _ | `Data _ -> skip ()
  in
  pull_pous_aux i 1 []

let pull_configurations i =
  let rec pull_configurations_aux i d (acc : ConfigurationNode.t list) : ConfigurationNode.t list =
    let skip () = pull_configurations_aux i d acc in
    if Xmlm.eoi i then acc else
      match Xmlm.input i with
      | `El_start ((_, tag), attrs) when (String.equal tag "configuration") -> begin
          let conf = ConfigurationNode.from_xml_exn i attrs in
          conf :: (pull_configurations_aux i (d + 1) acc)
        end
      | `El_end -> begin
          if (phys_equal d 1) then begin
            acc
          end else pull_configurations_aux i (d - 1) acc
        end
      | `El_start _ | `Dtd _ | `Data _ -> skip ()
  in
  pull_configurations_aux i 1 []

(** Iterate over all XML elements in schema to parse their source code. *)
let rec pull_all i d (acc : parse_tree) =
  if Xmlm.eoi i then acc
  else
    match Xmlm.input i with
    |`El_start ((_, tag), _) when (String.equal tag "pous") -> begin
        acc.pous <- List.append acc.pous (pull_pous i);
        pull_all i d acc
      end
    |`El_start ((_, tag), _) when (String.equal tag "configurations") -> begin
        acc.configurations <- List.append acc.configurations (pull_configurations i);
        pull_all i d acc
      end
    |`El_start _ -> pull_all i (d + 1) acc
    | `El_end -> if (phys_equal d 1) then acc else pull_all i (d - 1) acc
    | _ -> pull_all i d acc

let reconstruct_from_channel ic =
  let i = Xmlm.make_input (`Channel ic) in
  let parse_tree = { pous = []; configurations = [] } in
  pull_all i 1 parse_tree
  |> reconstruct_tree
