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

let get_stmts elems =
  let get_vd = function
    | S.IECFunction f -> f.statements
    | S.IECFunctionBlock fb -> fb.statements
    | S.IECProgram p -> p.statements
    | S.IECConfiguration _ -> []
  in
  List.fold_left elems
    ~f:(fun x e ->
      let es = get_vd e in
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
