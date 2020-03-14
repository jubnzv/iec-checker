open Core_kernel
module S = IECCheckerCore.Syntax
module TI = IECCheckerCore.Tok_info
module AU = IECCheckerCore.Ast_util
module Warn = IECCheckerCore.Warn
module E = IECCheckerCore.Error

let check_stmt func_name = function
  | S.StmFuncCall (ti, f, _) ->
      Printf.printf "%s - %s" func_name (S.Function.get_name f);
      if String.equal (S.Function.get_name f) func_name then
        let msg = "POUs shall not call themselves directly or indirectly" in
        let w = Warn.mk ti.linenr ti.col "PLCOPEN-CP13" msg in
        Some w
      else None
  | _ -> None

let check_func_stmts func =
  let name =
    match func with
    | S.IECFunction fd -> S.Function.get_name fd.id
    | S.IECFunctionBlock fbd -> S.FunctionBlock.get_name fbd.id
    | _ -> E.raise E.InternalError ""
  in
  AU.get_pou_stmts func
  |> List.fold_left
       ~f:(fun warns stmt ->
         let found_warn = check_stmt name stmt in
         found_warn :: warns)
       ~init:[]

let do_check elems =
  let functions =
    List.filter
      ~f:(fun e ->
        match e with
        | S.IECFunction _ | S.IECFunctionBlock _ -> true
        | _ -> false)
      elems
  in
  List.fold_left functions
    ~f:(fun warns f ->
      let found_warns = check_func_stmts f in
      List.append warns found_warns)
    ~init:[]
  |> List.filter ~f:(fun w -> match w with Some _ -> true | None -> false)
  |> List.map ~f:(fun w ->
         match w with Some w -> w | None -> E.raise E.InternalError "")
