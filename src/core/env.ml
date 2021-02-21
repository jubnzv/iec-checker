open Core_kernel
module S = Syntax
module E = Error

(** Maps over identifier uses, accessible by identifier name *)
module VarDeclMap = struct
  type t = (string, S.VarDecl.t, String.comparator_witness) Map.t

  let empty = Map.empty (module String)

  let lookup m (name : string) = Map.find m name

  let add m name var_decl = Map.set m ~key:name ~data:var_decl

  let to_list m =
    Map.to_alist m
    |> List.fold_left ~init:[] ~f:(fun vds (_, vd) -> vds @ [ vd ])

  let to_yojson (m : t) : Yojson.Safe.t =
    let items = Map.fold m ~init:[] ~f:(fun ~key ~data lst ->
        `Assoc [key, S.VarDecl.to_yojson data] :: lst
      ) in
    `List items
end

type t = {
  parent : t option;  (** Parent env *)
  id: int; (** Unique id of the POU. See: [Syntax.get_pou_id]. *)
  var_decls : VarDeclMap.t;  (** Variables declared in this env *)
}

let get_id t = t.id

let to_yojson env : Yojson.Safe.t =
  let var_decls = env.var_decls in
  `Assoc [
    "var_decls", VarDeclMap.to_yojson var_decls;
  ]

let empty = { parent = None; id = -1; var_decls = VarDeclMap.empty }

let mk_global () =
  let parent = None in
  let id = -1 in
  let var_decls = VarDeclMap.empty in
  { parent; id; var_decls }

let mk p id =
  let parent = Some p in
  let var_decls = VarDeclMap.empty in
  { parent; id; var_decls }

let add_vdecl env vd =
  let name = S.VarDecl.get_var_name vd in
  let vds = VarDeclMap.add env.var_decls name vd in
  { env with var_decls = vds }

let get_vdecls env = VarDeclMap.to_list env.var_decls

let lookup_vdecl env name = VarDeclMap.lookup env.var_decls name
