open Core

open IECCheckerCore
module AU = Ast_util
module S = Syntax

(** Map that bounds declaration name of variable with S.VarDecl.t objects. *)
module VarDeclMap = struct
  [@@@warning "-34"]
  [@@@warning "-32"]
  type t = (string, S.VarDecl.t, String.comparator_witness) Map.t
  let empty () = Map.empty (module String)
  let fold m = Map.fold m
  let find m name = Map.find m name
  let set m var_decl =
    Map.set m ~key:(S.VarDecl.get_var_name var_decl) ~data:(var_decl)
  let of_pou pou =
    AU.get_var_decls pou
    |> List.fold_left
      ~init:(empty ())
      ~f:(fun map var_decl -> set map var_decl)
end

(** Map that bounds variable name of variable with S.variable objects ("use"
    occurrence). *)
module VarUseMap = struct
  [@@@warning "-34"]
  [@@@warning "-32"]
  type t = (string, S.VarUse.t list, String.comparator_witness) Map.t
  let empty () = Map.empty (module String)
  let fold m = Map.fold m
  let find m name = Map.find m name
  let add m var_use =
    let name = S.VarUse.get_name var_use in
    match find m name with
    | Some vs -> Map.set m ~key:name ~data:(vs @ [var_use])
    | None -> Map.set m ~key:name ~data:([var_use])
  let of_pou pou =
    AU.filter_exprs
      pou
      ~f:(fun expr -> begin
            match expr with S.ExprVariable _ -> true | _ -> false
          end)
    |> List.fold
      ~init:(empty ())
      ~f:(fun m expr -> begin
            match expr with
            | S.ExprVariable (_, v) -> add m v
            | _ -> assert false
          end)
end

(** Find errors when array variables addressed to index that exceeds defined
    array size. *)
let check_array_out_of_bounds (decl_map : VarDeclMap.t) (use_map : VarUseMap.t) =
  let check_array_indexes (var_use : S.VarUse.t) (decl_subranges : S.arr_subrange list) : (Warn.t list) =
    let do_check idx_num (idx_value_opt : int option) =
      match List.nth decl_subranges idx_num with
      | Some sr -> begin
          match idx_value_opt with
          | Some idx_value -> begin
              if (idx_value < sr.arr_lower) || (idx_value > sr.arr_upper)then begin
                let ti = S.VarUse.get_ti var_use
                and name = S.VarUse.get_name var_use in
                let text =
                  Printf.sprintf "%s index %d is out of range [%d .. %d]"
                    name idx_value sr.arr_lower sr.arr_upper
                in
                [Warn.mk ti.linenr ti.col "OutOfBounds" text]
              end else []
            end
          | None (* opaque index *) -> []
        end
      | None -> begin
          let ti = S.VarUse.get_ti var_use
          and name = S.VarUse.get_name var_use in
          let text =
            Printf.sprintf "%s is addressed to %d dimension, but array was defined with %d dimensions"
              name (idx_num + 1) (List.length decl_subranges)
          in
          [Warn.mk ti.linenr ti.col "OutOfBounds" text]
        end
    in
    match S.VarUse.get_loc var_use with
    | S.VarUse.SymVar sv -> begin
        List.foldi
          (S.SymVar.get_array_indexes sv)
          ~init:[]
          ~f:(fun i acc_warns idx -> acc_warns @ (do_check i idx))
      end
    | S.VarUse.DirVar _ -> []
  in
  let check_var_use use_name var_use =
    match VarDeclMap.find decl_map use_name with
    | Some var_decl -> begin
        match S.VarDecl.get_ty_spec var_decl with
        | Some decl_spec -> begin
            match decl_spec with
            | S.DTyDeclArrayType (subranges, _, _) -> begin
                (check_array_indexes var_use subranges)
              end
            | S.DTyDeclSingleElement _ -> []
            | S.DTyDeclSubrange _ -> []
            | S.DTyDeclEnumType _ -> []
            | S.DTyDeclRefType _ -> []
            | S.DTyDeclStructType _ -> []
          end
        | None -> []
      end
    | None -> []
  in
  VarUseMap.fold
    use_map
    ~init:[]
    ~f:(fun ~key:use_name ~data:vars_use accm -> begin
          accm @ List.fold_left
            vars_use
            ~init:[]
            ~f:(fun accl var_use -> accl @ (check_var_use use_name var_use))
        end)

let check_pou pou =
  let decl_map = VarDeclMap.of_pou pou in
  let use_map = VarUseMap.of_pou pou in
  List.rev (check_array_out_of_bounds decl_map use_map)

let run elements =
  List.fold_left
    elements
    ~f:(fun warns e ->
        let ws = match e with
          | S.IECProgram _ | S.IECFunction _ | S.IECFunctionBlock _ -> check_pou e
          | _ -> []
        in
        warns @ ws)
    ~init:[]
