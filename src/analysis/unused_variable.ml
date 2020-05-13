open Core_kernel
module AU = IECCheckerCore.Ast_util
module S = IECCheckerCore.Syntax
module Warn = IECCheckerCore.Warn

let check_pou elem =
  let module StringSet = Set.Make(String) in

  (* Get names of variables declared in POU. *)
  let get_decl_var_names () =
    AU.get_var_decls elem
    |> List.map ~f:(fun vardecl -> S.VarDecl.get_var_name vardecl)
  in

  (* Get names of variables used in POU. *)
  let get_use_var_names () =
    AU.filter_exprs
      elem
      ~f:(fun expr -> begin
            match expr with S.ExprVariable _ -> true | _ -> false
          end)
    |> List.map
      ~f:(fun expr -> begin
            match expr with
            | S.ExprVariable (_, v) -> (S.VarUse.get_name v)
            | _ -> assert false
          end)
  in

  let decl_set = StringSet.of_list (get_decl_var_names ())
  and use_set = StringSet.of_list (get_use_var_names ()) in

  StringSet.diff decl_set use_set
  |> Set.fold ~init:[]
    ~f:(fun acc var_name -> begin
          let ti = AU.get_ti_by_name_exn elem var_name in
          let text = Printf.sprintf "Found unused local variable: %s" var_name in
          acc @ [Warn.mk ti.linenr ti.col "UnusedVariable" text]
        end)

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
