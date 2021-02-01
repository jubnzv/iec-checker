open Core_kernel
open IECCheckerCore
open IECCheckerAnalysis

module S = Syntax
module AU = IECCheckerCore.Ast_util

[@@@warning "-27"]

let get_located_vars_decls elem =
  AU.get_var_decls elem
  |> List.fold_left
    ~init:[]
    ~f:(fun acc var_decl -> begin
          match (S.VarDecl.get_located_at var_decl) with
          | Some loc -> acc @ [loc]
          | None -> acc
        end)

let get_located_values_uses elem =
  AU.get_pou_exprs elem
  |> List.fold_left ~init:[] ~f:(fun acc expr -> begin
        match expr with
        | S.ExprBin (_, lhs, operator, rhs) -> begin
            if phys_equal operator S.ASSIGN then
              match lhs with
              | S.ExprVariable (_, v) -> begin
                  match S.VarUse.get_loc v with
                  | S.VarUse.DirVar dirvar -> acc @ [S.DirVar.to_string dirvar]
                  | S.VarUse.SymVar _ -> acc
                end
              | _ -> acc
            else
              acc
          end
        | _ -> acc
      end)

let check_elem elem =
  let decls = get_located_vars_decls elem
  and uses = get_located_values_uses elem
  in
  List.fold_left
    uses
    ~init:[]
    ~f:(fun acc u -> begin
          acc @ List.fold_left
            decls
            ~init:[]
            ~f:(fun acc d -> begin
                  if String.equal (S.DirVar.get_name d) u then
                    let ti = S.DirVar.get_ti d
                    and msg = Printf.sprintf "Access to a member %s shall be by name" @@ S.DirVar.get_name d
                    in
                    acc @ [Warn.mk ti.linenr ti.col "PLCOPEN-CP1" msg]
                  else
                    acc
                end)
        end)

let do_check elems cfgs =
  List.fold_left
    elems
    ~init:[]
    ~f:(fun acc elem -> acc @ (check_elem elem))
