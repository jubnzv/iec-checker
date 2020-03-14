open Core_kernel
module S = IECCheckerCore.Syntax
module Env = IECCheckerCore.Env
module E = IECCheckerCore.Error
module Warn = IECCheckerCore.Warn

let str_len = function
  | S.DTyUseStringType (_, len) -> len
  | _ -> E.raise E.InternalError "Got unexpected type"

(** Compare length of declared string with initializer string size. *)
let check_str_init_size ty_init init_val =
  match init_val with
  | Some s ->
    let ty_len = str_len ty_init in
    let init_len = String.length s in
    if ty_len <> init_len then Some (ty_len, init_len) else None
  | None -> None

let check_ty_decl = function
  | S.DTyDeclSingleElement (_, _, _) -> []
  | S.DTyDeclStringType (_, ty_decl, init_val) ->
    match check_str_init_size ty_decl init_val with
    | Some (len_decl, len_init) when len_init > len_decl ->
      let msg =
        Printf.sprintf
          "Length of string initialization literal exceeds string length (%d > %d)"
          len_init len_decl
      in
      let w = Warn.mk 0 0 "DeclarationAnalysis" msg in
      [ w ]
    | _ -> []

let check_elem (el : S.iec_library_element) =
  match el with
  | S.IECType ty_decls ->
    List.fold_left ty_decls ~f:(fun warns ty -> List.append warns (check_ty_decl ty)) ~init:[]
  | _ -> []

let[@warning "-27"] run_declaration_analysis elements envs =
  List.fold_left elements
    ~f:(fun warns e ->
        let ws = check_elem e in
        warns @ ws)
    ~init:[]
