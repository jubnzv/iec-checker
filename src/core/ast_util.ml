open Core_kernel
module S = Syntax

let get_var_decl elems =
  let get_vd = function
    | S.IECFunction f -> f.variables
    | S.IECFunctionBlock fb -> fb.variables
    | S.IECProgram p -> p.variables
    | S.IECConfiguration c -> c.variables
    | S.IECType _ -> []
  in
  List.fold_left elems
    ~f:(fun x e ->
        let es = get_vd e in
        x @ es)
    ~init:[]

let expr_to_stmts expr : S.statement list =
  let rec aux = function
    | S.Variable _ -> []
    | S.Constant _ -> []
    | S.BinExpr (e1, _, e2) -> aux e1 @ aux e2
    | S.UnExpr (_, e) -> aux e
    | S.FuncCall s -> [ s ]
  in
  aux expr

let rec stmts_to_list stmt =
  match stmt with
  | S.StmAssign (_, _, e) -> stmt :: expr_to_stmts e
  | S.StmElsif (_, e, ns) ->
    [ stmt ] @ expr_to_stmts e
    @ List.fold_left ns ~f:(fun ss s -> ss @ stmts_to_list s) ~init:[]
  | S.StmIf (_, e, ns1, ns2, ns3) ->
    [ stmt ] @ expr_to_stmts e
    @ List.fold_left
      (ns1 @ ns2 @ ns3)
      ~f:(fun ss s -> ss @ stmts_to_list s)
      ~init:[]
  | S.StmCase (_, e, cs, ns) ->
    let cs_stmts =
      List.fold_left cs ~f:(fun css cs -> css @ cs.body) ~init:[]
    in
    [ stmt ] @ expr_to_stmts e
    @ List.fold_left (ns @ cs_stmts)
      ~f:(fun ss s -> ss @ stmts_to_list s)
      ~init:[]
  | S.StmFor (_, _, e1, e2, e3_opt, ns) ->
    let e3_stmts =
      match e3_opt with Some e -> expr_to_stmts e | None -> []
    in
    [ stmt ] @ expr_to_stmts e1 @ expr_to_stmts e2 @ e3_stmts
    @ List.fold_left ns ~f:(fun ss s -> ss @ stmts_to_list s) ~init:[]
  | S.StmWhile (_, e, ns) ->
    [ stmt ] @ expr_to_stmts e
    @ List.fold_left ns ~f:(fun ss s -> ss @ stmts_to_list s) ~init:[]
  | S.StmRepeat (_, ns, e) ->
    [ stmt ] @ expr_to_stmts e
    @ List.fold_left ns ~f:(fun ss s -> ss @ stmts_to_list s) ~init:[]
  | S.StmExit _ -> [ stmt ]
  | S.StmContinue _ -> [ stmt ]
  | S.StmReturn _ -> [ stmt ]
  | S.StmFuncParamAssign (_, e, _) -> [ stmt ] @ expr_to_stmts e
  | S.StmFuncCall (_, _, ns) ->
    stmt
    :: (ns @ List.fold_left ns ~f:(fun ss s -> ss @ stmts_to_list s) ~init:[])

let get_pou_stmts = function
  | S.IECFunction f ->
    List.fold_left f.statements ~f:(fun ss s -> ss @ stmts_to_list s) ~init:[]
  | S.IECFunctionBlock fb ->
    List.fold_left fb.statements
      ~f:(fun ss s -> ss @ stmts_to_list s)
      ~init:[]
  | S.IECProgram p ->
    List.fold_left p.statements ~f:(fun ss s -> ss @ stmts_to_list s) ~init:[]
  | S.IECConfiguration _ -> []
  | S.IECType _ -> []

let get_top_stmts = function
  | S.IECFunction f -> f.statements
  | S.IECFunctionBlock fb -> fb.statements
  | S.IECProgram p -> p.statements
  | S.IECConfiguration _ -> []
  | S.IECType _ -> []

let get_stmts elems =
  List.fold_left elems
    ~f:(fun x e ->
        let es = get_pou_stmts e in
        x @ es)
    ~init:[]

let get_exprs elems =
  let all_stmts = get_stmts elems in
  let rec get_stmt_exprs stmt =
    let get_nested stmts =
      List.fold_left stmts ~init:[] ~f:(fun acc es -> acc @ (get_stmt_exprs es))
    in
    match stmt with
    | S.StmAssign (_, _, e) -> [e]
    | S.StmElsif (_, e, ss) -> [e] @ (get_nested ss)
    | S.StmIf (_, e, ss1, ss2, ss3) -> (
        [e] @ (get_nested ss1) @ (get_nested ss2) @ (get_nested ss3)
      )
    | S.StmCase (_, e, cs, ss) -> (
        let case_exprs =
          List.fold_left cs
            ~init:[]
            ~f:(fun acc case_sel -> acc @ (case_sel.case @ (get_nested case_sel.body)))
        in
        [e] @ case_exprs @ (get_nested ss)
      )
    | S.StmFor (_, _, e1, e2, e3_opt, ss) -> (
        let e3 = match e3_opt with Some e -> [e] | None -> [] in
        [e1] @ [e2] @ e3 @ (get_nested ss)
      )
    | S.StmWhile (_, e, ss) -> [e] @ (get_nested ss)
    | S.StmRepeat (_, ss, e) -> (get_nested ss) @ [e]
    | S.StmExit _ | S.StmContinue _ | S.StmReturn _ -> []
    | S.StmFuncParamAssign (_, e, _) -> [e]
    | S.StmFuncCall (_, _, ss) -> (get_nested ss)
  in
  List.fold_left all_stmts
    ~init:[]
    ~f:(fun acc stmt -> List.append acc (get_stmt_exprs stmt))

(** Bound declaration of global variables in global env. *)
let fill_global_env env = function
  | S.IECFunction _ | S.IECFunctionBlock _ | S.IECProgram _ | S.IECType _ -> env
  | S.IECConfiguration cfg ->
    List.fold_left cfg.variables ~f:(fun s v -> Env.add_vdecl s v) ~init:env

(** Bound declaration of local variables to given env. *)
let fill_pou_env env (elem : S.iec_library_element) =
  List.fold_left (S.get_pou_vars_decl elem)
    ~f:(fun s v -> Env.add_vdecl s v)
    ~init:env

let create_envs elems =
  let global_env = Env.mk_global in
  let global_env =
    List.fold_left elems ~f:(fun gs e -> fill_global_env gs e) ~init:global_env
  in
  List.fold_left elems
    ~f:(fun envs e ->
        let local_env = Env.mk global_env in
        let local_env = fill_pou_env local_env e in
        envs @ [ local_env ])
    ~init:[ global_env ]
