open Core_kernel
module S = Syntax
module E = Error

(** Maps over identifier uses, accessible by identifier name *)
module VarDeclMap = struct
  type t = (string, S.VarDecl.t, String.comparator_witness) Map.t

  let empty = Map.empty (module String)

  let lookup m (name: string) = Map.find m name

  (* let lookup_exn m name = match Map.find m name with
    | Some var_decl -> var_decl
    | None -> E.raise E.UnboundIdentifier name *)

  let add m name var_decl = Map.set m ~key:name ~data:var_decl

  let to_list m =
    Map.to_alist m
    |> List.fold_left ~init:[] ~f:(fun vds (_, vd) -> vds @ [ vd ])
end

type t = {
  parent : t option;  (** Parent scope *)
  var_decls : VarDeclMap.t;  (** Varaibles declared in this scope *)
}

let empty = { parent = None; var_decls = VarDeclMap.empty }

let mk_global =
  let parent = None in
  let var_decls = VarDeclMap.empty in
  { parent; var_decls }

let mk p =
  let parent = Some p in
  let var_decls = VarDeclMap.empty in
  { parent; var_decls }

let add_vdecl scope vd =
  let name = S.VarDecl.get_var_name vd in
  let vds = VarDeclMap.add scope.var_decls name vd in
  { scope with var_decls = vds }

let get_vdecls scope = VarDeclMap.to_list scope.var_decls

let lookup_vdecl scope name = VarDeclMap.lookup scope.var_decls name
