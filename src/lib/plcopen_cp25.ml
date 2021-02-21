open Core_kernel
open IECCheckerCore

module TI = Tok_info
module S = Syntax

let types_can_be_casted ty_from ty_to =
  let open S in
  match (ty_from, ty_to) with
  | (DTyDeclSingleElement(se_from_spec,_),DTyDeclSingleElement(se_to_spec,_)) -> begin
      match (se_from_spec, se_to_spec) with
      | (DTySpecElementary(REAL|LREAL),DTySpecElementary(SINT|INT|DINT|LINT|USINT|UDINT|ULINT|BOOL|BYTE|WORD|DWORD|LWORD)) -> false
      | (DTySpecElementary(SINT|INT|DINT|LINT|USINT|UDINT|ULINT|BOOL|BYTE|WORD|DWORD|LWORD),DTySpecElementary(REAL|LREAL)) -> false
      | _ -> true
    end
  | _ -> true

let check_assign_expr (ti : TI.t) lhs rhs env =
  let check_types lhs_decl rhs_decl =
    match (S.VarDecl.get_ty_spec lhs_decl, S.VarDecl.get_ty_spec rhs_decl) with
    | (Some(lhs_ty),Some(rhs_ty)) -> begin
        if not (types_can_be_casted lhs_ty rhs_ty) then
          Some(Warn.mk ti.linenr ti.col "PLCOPEN-CP25" "Data type conversion should be explicit.")
        else
          None
      end
    | _ -> None
  in
  let lhs_opt = Env.lookup_vdecl env (S.VarUse.get_name lhs)
  and rhs_opt = Env.lookup_vdecl env (S.VarUse.get_name rhs)
  in
  match (lhs_opt,rhs_opt) with
  | (Some(lhs), Some(rhs)) -> check_types lhs rhs
  | _ -> None

let check_pou pou env =
  Ast_util.get_pou_exprs pou
  |> List.fold_left
    ~init:[]
    ~f:(fun acc expr -> begin
          match expr with
          | S.ExprBin (ti,(S.ExprVariable (_, lhs)),(S.EQ|S.NEQ|S.ASSIGN|S.ASSIGN_REF|S.GT|S.LT|S.GE|S.LE|S.SENDTO),(S.ExprVariable (_, rhs))) -> begin
              check_assign_expr ti lhs rhs env
              |> Caml.Option.fold ~none:[] ~some:(fun w -> [w])
              |> List.append acc
            end
          | _ -> acc
        end)

let do_check elems envs =
  List.fold_left
    elems
    ~init:[]
    ~f:(fun acc pou -> begin
          let env = List.find_exn envs
              ~f:(fun env -> phys_equal (Env.get_id env) (S.get_pou_id pou))
          in
          acc @ check_pou pou env
        end)
