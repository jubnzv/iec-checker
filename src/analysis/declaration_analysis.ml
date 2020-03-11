open Core_kernel
module S = IECCheckerCore.Syntax
module Env = IECCheckerCore.Env
module E = IECCheckerCore.Error
module Warn = IECCheckerCore.Warn

let str_len = function
  | S.DTyUseStringType (_, len) -> len
  | _ -> E.raise E.InternalError "Got unexpected type"

(** Compare declaration and init string size *)
let check_str_size (ty_decl : S.derived_ty) (ty_init : S.derived_ty) =
  let len_d = str_len ty_decl in
  let len_i = str_len ty_init in
  if len_d <> len_i then Some (len_d, len_i) else None

let check_str_size_w ty_decl ty_init =
  match check_str_size ty_decl ty_init with
  | None -> []
  | Some (len_d, len_i) ->
      let msg =
        Printf.sprintf
          "String declaration and initialization doesn't match (%d <> %d)" len_d
          len_i
      in
      let w = Warn.mk "DeclarationAnalysis" msg in
      [ w ]

(** Compare length of declared string with initializer string size. *)
let check_str_init_size ty_init init_val =
  match init_val with
  | Some s ->
      let ty_len = str_len ty_init in
      let init_len = String.length s in
      if ty_len <> init_len then Some (ty_len, init_len) else None
  | None -> None

let check_str_init_size_w ty_init init_val =
  match check_str_init_size ty_init init_val with
  | None -> []
  | Some (len_i, len_c) ->
      let msg =
        Printf.sprintf
          "String size doesn't match size of initialization literal (%d <> %d)"
          len_i len_c
      in
      let w = Warn.mk "DeclarationAnalysis" msg in
      [ w ]

let check_ty_decl = function
  | S.DTyDeclSingleElement (_, _, _) -> []
  | S.DTyDeclStringType (ty_decl, ty_init, init_val) ->
      check_str_size_w ty_decl ty_init
      |> List.append (check_str_init_size_w ty_init init_val)

let check_elem (el : S.iec_library_element) =
  match el with S.IECType ty_d -> check_ty_decl ty_d | _ -> []

let[@warning "-27"] run_declaration_analysis elements envs =
  List.fold_left elements
    ~f:(fun warns e ->
      let ws = check_elem e in
      warns @ ws)
    ~init:[]
