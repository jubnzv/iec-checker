open Core_kernel
open IECCheckerCore
module S = Syntax
module TI = Tok_info

let test_add_var_decl () =
  let global_scope = Scope.mk_global in
  let vd1_sv = S.SymVar.create "dummy" S.TI.create_dummy in
  let vds1_variable = S.SymVar vd1_sv in
  let vd1 = S.VarDecl.create vds1_variable S.VarDecl.SpecTemp in
  let local_scope = Scope.mk global_scope in
  Alcotest.(check int)
    "no initial variable declarations" 0
    (List.length (Scope.get_vdecls local_scope));
  let local_scope = Scope.add_vdecl local_scope vd1 in
  Alcotest.(check int)
    "can add variable declarations" 1
    (List.length (Scope.get_vdecls local_scope));
  Alcotest.(check string)
    "added correct data" "dummy"
    ( match Scope.get_vdecls local_scope with
    | [ h ] -> S.VarDecl.get_var_name h
    | _ -> "error" )

let () =
  let open Alcotest in
  run "Scopes"
    [ ("test-add-var-declaration", [ test_case " " `Quick test_add_var_decl ]) ]
