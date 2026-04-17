open Core
module S = Syntax
module TI = Tok_info

let get_var_decls = function
  | S.IECFunction (_, f) -> f.variables
  | S.IECFunctionBlock (_, fb) -> fb.variables
  | S.IECProgram (_, p) -> p.variables
  | S.IECClass (_, c) -> c.variables
  | S.IECInterface _ -> []
  | S.IECConfiguration (_, c) -> c.variables
  | S.IECType _ -> []

let expr_to_stmts expr : S.statement list =
  let rec aux = function
    | S.ExprVariable _ -> []
    | S.ExprConstant _ -> []
    | S.ExprBin (_, e1, _, e2) -> aux e1 @ aux e2
    | S.ExprUn (_, _, e) -> aux e
    | S.ExprFuncCall (_, s) -> [s]
  in
  aux expr

let rec stmts_to_list stmt =
  let get_nested stmts =
    List.fold_left
      stmts
      ~init:[]
      ~f:(fun acc s -> acc @ (stmts_to_list s))
  in
  match stmt with
  | S.StmExpr (_, e) -> [stmt] @ expr_to_stmts e
  | S.StmElsif (_, cond_stmts, body_stmts) ->
    [ stmt ] @ stmts_to_list cond_stmts
    @ List.fold_left
      body_stmts
      ~init:[]
      ~f:(fun acc s -> acc @ stmts_to_list s)
  | S.StmIf (_, cond_s, body_ss, elsif_ss, else_ss) ->
    [ stmt ]
    @ List.fold_left
      ([cond_s] @ body_ss @ elsif_ss @ else_ss)
      ~f:(fun ss s -> ss @ stmts_to_list s)
      ~init:[]
  | S.StmCase (_, cond_s, case_sels, else_ss) ->
    let case_stmts =
      List.fold_left
        case_sels
        ~init:[]
        ~f:(fun acc cs -> acc @ (get_nested cs.case) @ (get_nested cs.body))
    in
    [cond_s] @ case_stmts @ (get_nested else_ss)
  | S.StmFor (_, ctrl, body_stmts) ->
    [ ctrl.assign ] @
    List.fold_left body_stmts ~init:[] ~f:(fun acc s -> acc @ stmts_to_list s)
  | S.StmWhile (_, cond_stmt, ns) ->
    [stmt] @ [cond_stmt] @
    List.fold_left ns ~f:(fun ss s -> ss @ stmts_to_list s) ~init:[]
  | S.StmRepeat (_, body_stmts, cond_stmt) ->
    [stmt] @
    List.fold_left body_stmts
      ~init:[]
      ~f:(fun ss s -> ss @ stmts_to_list s) @
    [cond_stmt]
  | S.StmExit _ -> [ stmt ]
  | S.StmContinue _ -> [ stmt ]
  | S.StmReturn _ -> [ stmt ]
  | S.StmFuncCall (_, _, func_params) -> begin
      let func_params_stmts = List.fold_left
          func_params
          ~init:[]
          ~f:(fun acc fp -> acc @ [fp.stmt])
      in
      [stmt] @ func_params_stmts
    end
  | S.StmEmpty _ -> [ stmt ]

let get_pou_stmts = function
  | S.IECFunction (_, f) ->
    List.fold_left f.statements ~f:(fun ss s -> ss @ stmts_to_list s) ~init:[]
  | S.IECFunctionBlock (_, fb) ->
    List.fold_left fb.statements
      ~f:(fun ss s -> ss @ stmts_to_list s)
      ~init:[]
  | S.IECProgram (_, p) ->
    List.fold_left p.statements ~f:(fun ss s -> ss @ stmts_to_list s) ~init:[]
  | S.IECClass (_, c) ->
      List.fold_left
        c.methods
        ~init:[]
        ~f:(fun acc m -> acc @ List.fold_left m.statements ~init:[] ~f:(fun acc s -> acc @ stmts_to_list s))
  | S.IECInterface _ -> []
  | S.IECConfiguration _ -> []
  | S.IECType _ -> []

let get_top_stmts = function
  | S.IECFunction (_, f) -> f.statements
  | S.IECFunctionBlock (_, fb) -> fb.statements
  | S.IECProgram (_, p) -> p.statements
  | S.IECClass (_, c) -> List.fold_left c.methods ~init:[] ~f:(fun acc m -> acc @ m.statements)
  | S.IECInterface _ -> []
  | S.IECConfiguration _ -> []
  | S.IECType _ -> []

let get_stmts_num elem =
  List.length (get_pou_stmts elem)

let get_stmts elems =
  List.fold_left elems
    ~f:(fun x e ->
        let es = get_pou_stmts e in
        x @ es)
    ~init:[]

let rec get_stmt_exprs stmt =
  let get_nested stmts =
    List.fold_left stmts ~init:[] ~f:(fun acc es -> acc @ (get_stmt_exprs es))
  in
  match stmt with
  | S.StmExpr (_, e) -> [e]
  | S.StmElsif (_, cond_s, ss) -> (get_nested [cond_s]) @ (get_nested ss)
  | S.StmIf (_, cond_s, body_ss, elsif_ss, else_ss) -> (
      (get_nested [cond_s]) @
      (get_nested body_ss) @
      (get_nested elsif_ss) @
      (get_nested else_ss)
    )
  | S.StmCase (_, cond_s, case_sels, else_ss) ->
    begin
      let case_stmts =
        List.fold_left
          case_sels
          ~init:[]
          ~f:(fun acc case_sel -> acc @ (get_nested case_sel.case) @ (get_nested case_sel.body))
      in
      (get_nested [cond_s]) @
      case_stmts @
      (get_nested else_ss)
    end
  | S.StmFor (_, ctrl, body_stmts) -> (
      (get_nested [ctrl.assign]) @
      [ctrl.range_end; ctrl.range_step] @
      (get_nested body_stmts)
    )
  | S.StmWhile (_, cond_stmt, ss) -> (get_nested [cond_stmt]) @ (get_nested ss)
  | S.StmRepeat (_, body_stmts, cond_stmt) -> (get_nested body_stmts) @ (get_nested [cond_stmt])
  | S.StmFuncCall (_, _, func_params) -> begin
      let func_params_stmts = List.fold_left
          func_params
          ~init:[]
          ~f:(fun acc fp -> acc @ [fp.stmt])
      in
      (get_nested func_params_stmts)
    end
  | S.StmExit _ | S.StmContinue _ | S.StmReturn _ -> []
  | S.StmEmpty _ -> []

let get_pou_exprs elem =
  get_pou_stmts elem
  |> List.fold_left ~init:[] ~f:(fun acc stmt -> acc @ (get_stmt_exprs stmt))

let get_var_uses elem =
  let rec get_vars = function
    | S.ExprVariable (_, vu) -> [vu]
    | S.ExprConstant _ -> []
    | S.ExprBin (_, lhs, _, rhs) -> (get_vars lhs) @ (get_vars rhs)
    | S.ExprUn (_, _, e) -> get_vars e
    | S.ExprFuncCall (_, stmt) -> begin
        get_stmt_exprs stmt
        |> List.fold_left ~init:[] ~f:(fun acc e -> acc @ (get_vars e))
      end
  in
  get_pou_exprs elem
  |> List.fold_left ~init:[] ~f:(fun acc expr -> acc @ (get_vars expr))

let filter_exprs ~f elem =
  let rec aux acc stmt =
    let get_nested stmts =
      List.fold_left
        stmts
        ~init:[]
        ~f:(fun acc s -> acc @ (aux [] s))
    in
    let rec get_nested_exprs acc = function
      | S.ExprBin (_,e1,_,e2) -> begin
          acc @
          [e1] @ (get_nested_exprs acc e1) @
          [e2] @ (get_nested_exprs acc e2)
        end
      | S.ExprUn (_,_,e) -> begin
          acc @ [e] @ (get_nested_exprs acc e)
        end
      | S.ExprVariable _ | S.ExprConstant _ | S.ExprFuncCall _ -> acc
    in
    let apply_filter (exprs : S.expr list) =
      List.filter exprs ~f
    in
    match stmt with
    | S.StmExpr (_, e) -> begin
        [e] @ (get_nested_exprs [] e)
        |> apply_filter
        |> List.append acc
      end
    | S.StmElsif (_, cond_s, ss) -> begin
        (get_nested [cond_s]) @
        (get_nested ss)
        |> apply_filter
        |> List.append acc
      end
    | S.StmIf (_, cond_s, body_ss, elsif_ss, else_ss) -> begin
        (get_nested [cond_s]) @
        (get_nested body_ss) @
        (get_nested elsif_ss) @
        (get_nested else_ss)
        |> apply_filter
        |> List.append acc
      end
    | S.StmCase (_, cond_s, case_sels, else_ss) ->
      begin
        let case_stmts =
          List.fold_left
            case_sels
            ~init:[]
            ~f:(fun acc case_sel -> begin
                  acc @
                  (get_nested case_sel.case) @
                  (get_nested case_sel.body)
                end)
        in
        (get_nested [cond_s]) @
        (case_stmts) @
        (get_nested else_ss)
        |> apply_filter
        |> List.append acc
      end
    | S.StmFor (_, ctrl, body_stmts) -> begin
        (get_nested [ctrl.assign]) @
        [ctrl.range_end; ctrl.range_step] @
        (get_nested body_stmts)
        |> apply_filter
        |> List.append acc
      end
    | S.StmWhile (_, cond_stmt, ss) -> begin
        (get_nested [cond_stmt]) @
        (get_nested ss)
        |> apply_filter
        |> List.append acc
      end
    | S.StmRepeat (_, body_stmts, cond_stmt) -> begin
        (get_nested body_stmts) @
        (get_nested [cond_stmt])
        |> apply_filter
        |> List.append acc
      end
    | S.StmFuncCall (_, _, func_params) -> begin
        let func_params_stmts = List.fold_left
            func_params
            ~init:[]
            ~f:(fun acc fp -> acc @ [fp.stmt])
        in
        (get_nested func_params_stmts)
        |> apply_filter
        |> List.append acc
      end
    | S.StmExit _ | S.StmContinue _ | S.StmReturn _ -> acc
    | S.StmEmpty _ -> acc
  in
  let all_stmts = get_pou_stmts elem in
  List.fold_left
    all_stmts
    ~init:[]
    ~f:(fun acc stmt -> acc @ (aux [] stmt))

let get_ti_by_name_exn elem var_name =
  let (tis : TI.t list) = List.fold_left
      (get_var_decls elem)
      ~init:[]
      ~f:(fun acc vardecl -> begin
            if String.equal (S.VarDecl.get_var_name vardecl) var_name then
              acc @ [(S.VarDecl.get_var_ti vardecl)]
            else
              acc
          end)
  in
  List.nth_exn tis 0

(** Bound declaration of global variables in global env. *)
let fill_global_env env = function
  | S.IECFunction _ | S.IECFunctionBlock _ | S.IECProgram _ | S.IECType _ | S.IECClass _ | S.IECInterface _ -> env
  | S.IECConfiguration (_, cfg) ->
    List.fold_left cfg.variables ~f:(fun s v -> Env.add_vdecl s v) ~init:env

let create_envs elems =
  (** Bound declaration of local variables to given env. *)
  let fill_pou_env (elem : S.iec_library_element) env =
    let r = List.fold_left (S.get_pou_vars_decl elem)
        ~init:env
        ~f:(fun s v -> Env.add_vdecl s v)
    in [r]
  in
  let global_env = Env.mk_global () in
  let global_env =
    List.fold_left elems ~f:(fun gs e -> fill_global_env gs e) ~init:global_env
  in
  List.fold_left elems
    ~f:(fun envs e ->
        Env.mk global_env (S.get_pou_id e)
        |> fill_pou_env e
        |> List.append envs)
    ~init:[global_env]

let eval_array_capacity subranges =
  List.fold_left
    subranges
    ~init:(0)
    ~f:(fun acc (sr : S.arr_subrange) -> begin
          let mul = if phys_equal acc 0 then 1 else acc in
          (mul * (sr.arr_upper - sr.arr_lower + 1))
        end)
