open Core_kernel
open IECCheckerCore

module S = Syntax
module AU = IECCheckerCore.Ast_util


let check_elem elem =
  match elem with
  | S.IECFunction _ | S.IECFunctionBlock _ | S.IECClass _ -> begin
      AU.get_var_decls elem
      |> List.fold_left
        ~init:[]
        ~f:(fun acc var_decl -> begin
              match S.VarDecl.get_attr var_decl with
              | Some S.VarDecl.VarExternal _ -> begin
                  let ti = S.VarDecl.get_var_ti var_decl
                  and msg = "External variables in functions, function blocks and classes should be avoided"
                  in
                  acc @ [(Warn.mk ti.linenr ti.col "PLCOPEN-CP6" msg)]
                end
              | _ -> acc
            end)
    end
  | S.IECProgram _ | S.IECConfiguration _ | S.IECType _ | S.IECInterface _ -> []

let do_check elems =
  List.fold_left elems ~init:[] ~f:(fun acc elem -> acc @ (check_elem elem))
