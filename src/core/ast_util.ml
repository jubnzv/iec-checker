open Core_kernel
module S = Syntax

let get_var_decl elems =
  let get_vd = function
    | S.IECFunction f -> f.variables
    | S.IECFunctionBlock fb -> fb.variables
    | S.IECProgram p -> p.variables
    | S.IECConfiguration c -> c.variables
  in
  List.fold_left elems
    ~f:(fun x e ->
      let es = get_vd e in
      x @ es)
    ~init:[]

let rec expr_to_stmts expr : S.statement list =
  match expr with
  | S.Variable _ -> []
  | S.Constant _ -> []
  | S.BinExpr (e1, _, e2) -> expr_to_stmts e1 @ expr_to_stmts e2
  | S.UnExpr (_, e) -> expr_to_stmts e
  | S.FuncCall s -> [ s ]

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

let get_stmts elems =
  List.fold_left elems
    ~f:(fun x e ->
      let es = get_pou_stmts e in
      x @ es)
    ~init:[]

(** Bound declaration of global variables in global scope. *)
let fill_global_scope scope = function
  | S.IECFunction _ | S.IECFunctionBlock _ | S.IECProgram _ -> scope
  | S.IECConfiguration cfg ->
      List.fold_left cfg.variables
        ~f:(fun s v -> Scope.add_vdecl s v)
        ~init:scope

(** Bound declaration of local variables to given scope. *)
let fill_pou_scope scope (elem : S.iec_library_element) =
  List.fold_left (S.get_pou_vars_decl elem)
    ~f:(fun s v -> Scope.add_vdecl s v)
    ~init:scope

let create_scopes elems =
  let global_scope = Scope.mk_global in
  let global_scope =
    List.fold_left elems
      ~f:(fun gs e -> fill_global_scope gs e)
      ~init:global_scope
  in
  List.fold_left elems
    ~f:(fun scopes e ->
      let local_scope = Scope.mk global_scope in
      let local_scope = fill_pou_scope local_scope e in
      scopes @ [ local_scope ])
    ~init:[ global_scope ]
