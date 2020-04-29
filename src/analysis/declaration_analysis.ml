open Core_kernel
module S = IECCheckerCore.Syntax
module Env = IECCheckerCore.Env
module E = IECCheckerCore.Error
module Warn = IECCheckerCore.Warn

let str_len = function
  | S.STRING l  -> l
  | S.WSTRING l -> l
  | S.CHAR l    -> l
  | S.WCHAR l   -> l
  | _ -> E.raise E.InternalError "Got unexpected type"

(** Compare length of declared string with initializer string size. *)
let check_str_init_size ty_init init_expr =
  let check_length init_str =
    let ty_len = str_len ty_init in
    let init_len = String.length init_str in
    if ty_len <> init_len then Some (ty_len, init_len) else None
  in
  match init_expr with
  | Some(e) ->
    begin
      match e with
      | S.Constant(c) ->
        begin
          match c with
          | S.CString(_, str) -> check_length str
          | _ -> E.raise E.InternalError "Got unexpected type"
        end
      | _ -> None
    end
  | None -> None

(** Search for errors in initial value in declaration of a string type *)
let check_str_init_expr ty_init init_expr =
  match check_str_init_size ty_init init_expr with
  | Some (len_decl, len_init) when (len_init > len_decl) ->
    let msg =
      Printf.sprintf
        "Length of initialization string literal exceeds string length (%d > %d)"
        len_init len_decl
    in
    let w = Warn.mk 0 0 "DeclarationAnalysis" msg in
    [ w ]
  | Some _ -> [] (* no violations *)
  | None -> []

(** Search for errors in subrange initialization *)
let check_subrange_init_val ty_spec init_val =
  let (_, lb, ub) = ty_spec in
  if (init_val < lb) || (init_val > ub) then
    let msg =
      Printf.sprintf "Initial subrange value %d does not fit specified range (%d .. %d)"
        init_val lb ub
    in let w = Warn.mk 0 0 "DeclarationAnalysis" msg in [ w ]
  else []

let check_ty_decl = function
  | S.DTyDeclSingleElement (_, ty_spec, init_expr) ->
    begin
      match ty_spec with
      | S.DTySpecElementary ty_decl -> (check_str_init_expr ty_decl init_expr)
      | S.DTySpecSimple _ -> []
    end
  | S.DTyDeclSubrange (_, ty_spec, init_val) -> check_subrange_init_val ty_spec init_val

let[@warning "-27"] run elements envs =
  List.fold_left elements
    ~f:(fun warns e ->
        let ws = match e with
            | S.IECType (_, ty) -> check_ty_decl ty
            | _ -> []
        in
        warns @ ws)
    ~init:[]
