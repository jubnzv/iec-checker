open Core
open IECCheckerCore

module S = Syntax
module AU = IECCheckerCore.Ast_util

let do_check elems =
  List.fold_left elems ~init:[] ~f:(fun acc elem -> acc @ (AU.get_var_decls elem))
  |> List.fold_left
    ~init:[]
    ~f:(fun acc decl -> begin
          match S.VarDecl.get_located_at decl with
          | None -> begin
              if phys_equal (S.VarDecl.get_was_init decl) false then
                let ti = S.VarDecl.get_var_ti decl in
                let msg = Printf.sprintf("Variable %s shall be initialized before being used") @@ S.VarDecl.get_var_name decl in
                let w = Warn.mk ti.linenr ti.col "PLCOPEN-CP3" msg in
                acc @ [w]
              else
                acc
            end
          | _ -> acc
        end)
