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
