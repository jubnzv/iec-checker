(** This module describes all common elements that which make up programming
    model of Structured Text language. See Annex B of IEC61131-3 for reference. *)
open Core_kernel
module TI = Tok_info

exception InternalError of string

(* FIXME: I have no idea what is wrong with yojson. It seems there are problems
 * with @opaque usage. *)
[@@@warning "-32"]

module TimeValue = struct
  type t = {
    y : int;
    mo : int;
    d : float;
    h : float;
    m : float;
    s : float;
    ms : float;
    us : float;
    ns : float;
  }
  [@@deriving to_yojson, show, fields]

  let mk ?(y = 0) ?(mo = 0) ?(d = 0.) ?(h = 0.) ?(m = 0.) ?(s = 0.) ?(ms = 0.)
      ?(us = 0.) ?(ns = 0.) () =
    { y; mo; d; h; m; s; ms; us; ns }

  let ( + ) lhs rhs =
    {
      y = rhs.y + lhs.y;
      mo = rhs.mo + lhs.mo;
      d = rhs.d +. lhs.d;
      h = rhs.h +. lhs.h;
      m = rhs.m +. lhs.m;
      s = rhs.s +. lhs.s;
      ms = rhs.ms +. lhs.ms;
      us = rhs.us +. lhs.us;
      ns = rhs.ns +. lhs.ns;
    }

  let inv tv =
    {
      y = tv.y * -1;
      mo = tv.mo * -1;
      d = tv.d *. -1.0;
      h = tv.h *. -1.0;
      m = tv.m *. -1.0;
      s = tv.s *. -1.0;
      ms = tv.ms *. -1.0;
      us = tv.us *. -1.0;
      ns = tv.ns *. -1.0;
    }

  let to_string tv = show tv

  let is_zero tv = phys_equal tv.d 0.
  (* TODO: List.map ~f(fun fv -> phys_equal fv 0.) ???Fields *)

  let to_yojson t = to_yojson t
end

(* {{{ Operators *)
type operator =
  | NEG
  | NOT
  | POW
  | MUL
  | DIV
  | MOD
  | AND
  | ADD
  | SUB
  | OR
  | XOR
  | GT
  | LT
  | GE
  | LE
  | EQ
  | NEQ
  | ASSIGN
[@@deriving to_yojson, show]
(* }}} *)

(* {{{ Variables and identifiers *)
module type ID = sig
  type t
  val create : string -> TI.t -> t
  val get_name : t -> string
  val get_ti : t -> TI.t
end

module SymVar = struct
  type t = { name : string; ti : TI.t } [@@deriving to_yojson]

  let create name ti =
    { name; ti }

  let get_name id = id.name

  let get_ti id = id.ti

  let to_yojson t = to_yojson t
end

module DirVar = struct
  type location = LocI | LocQ | LocM
  [@@deriving to_yojson]

  type size =
    | SizeX    (** single bit *)
    | SizeNone (** single bit *)
    | SizeB    (** byte *)
    | SizeW    (** word (16 bits) *)
    | SizeD    (** double word (32 bits) *)
    | SizeL    (** quad word (64 bits) *)
  [@@deriving to_yojson]

  let path_to_string path =
    List.fold_left path
      ~init:""
      ~f:(fun s p -> s ^ (Printf.sprintf ".%d" p))

  type t = {
    name: string;
    ti: TI.t;
    loc: location option;
    sz: size option;
    is_partly_located: bool; (** ExprVariable is defined using '*' symbol *)
    path: int list;
  } [@@deriving to_yojson]

  let create ti =
    let name = "" in
    let loc = None in
    let sz = None in
    let is_partly_located = false in
    let path = [] in
    { name; ti; loc; sz; is_partly_located; path }

  let get_name var = var.name
  let get_ti var = var.ti
  let get_loc var = var.loc
  let get_size var = var.sz
  let get_is_partly_located var = var.is_partly_located
  let get_path var = var.path

  let set_name var v =
    { var with name = v }
  let set_ti var v =
    { var with ti = v }
  let set_loc var v =
    { var with loc = Some(v) }
  let set_size var v =
    { var with sz = Some(v) }
  let set_is_partly_located var v =
    { var with is_partly_located = v }
  let set_path var v =
    { var with path = v }

  let size_to_string = function
    | SizeX    -> "X"
    | SizeNone -> "" (* TODO: Always single-byte value? *)
    | SizeB    -> "B"
    | SizeW    -> "W"
    | SizeD    -> "D"
    | SizeL    -> "L"

  let size_of_string v =
    if String.equal v "X" then
      Some(SizeX)
    else if String.equal v "B" then
      Some(SizeB)
    else if String.equal v "W" then
      Some(SizeW)
    else if String.equal v "D" then
      Some(SizeD)
    else if String.equal v "L" then
      Some(SizeL)
    else
      None

  let location_to_string = function
    | LocI -> "I"
    | LocQ -> "Q"
    | LocM -> "M"

  let location_of_string v =
    if String.equal v "I" then
      Some(LocI)
    else if String.equal v "Q" then
      Some(LocQ)
    else if String.equal v "M" then
      Some(LocM)
    else
      None

  let to_yojson t = to_yojson t
end

module FunctionBlock = struct
  type t = { name : string; ti : TI.t; is_std : bool }
  [@@deriving to_yojson]

  let create name ti =
    let is_std = false in
    { name; ti; is_std }

  let get_name fb = fb.name

  let get_ti fb = fb.ti

  let is_std fb = fb.is_std

  let to_yojson t = to_yojson t
end

module Function = struct
  type t = { name : string; ti : TI.t; is_std : bool }
  [@@deriving to_yojson, show]

  let create name ti =
    let is_std = false in
    { name; ti; is_std }

  let get_name fn = fn.name

  let get_ti fn = fn.ti

  let is_std fn = fn.is_std

  let to_yojson t = to_yojson t
end

type variable = SymVar of (SymVar.t [@opaque]) | DirVar of (DirVar.t [@opaque])
[@@deriving to_yojson, show]

let vget_name = function
  | SymVar(v) -> (let n = SymVar.get_name(v) in n) | DirVar(v) -> (let n = DirVar.get_name(v) in n)

let vget_ti = function
  | SymVar(v) -> (let ti = SymVar.get_ti(v) in ti) | DirVar(v) -> (let ti = DirVar.get_ti(v) in ti)

(* }}} *)

(* {{{ Data types, constants and statements *)
type iec_data_type =
  | TyElementary of elementary_ty
  | TyGeneric of generic_ty
  | TyDerived of derived_ty
[@@deriving to_yojson]

and elementary_ty =
  | NIL (* TODO: replace with an empty symbol *)
  | STRING of int (** length *)
  | WSTRING of int (** length *)
  | CHAR of int (** length *)
  | WCHAR of int (** length *)
  | TIME
  | LTIME
  | SINT
  | INT
  | DINT
  | LINT
  | USINT
  | UINT
  | UDINT
  | ULINT
  | REAL
  | LREAL
  | DATE
  | LDATE
  | TIME_OF_DAY
  | TOD
  | LTOD
  | DATE_AND_TIME
  | LDATE_AND_TIME
  | DT
  | LDT
  | BOOL
  | BYTE
  | WORD
  | DWORD
  | LWORD
[@@deriving to_yojson]

and generic_ty =
  | ANY
  | ANY_DERIVED
  | ANY_ELEMENTARY
  | ANY_MAGNITUDE
  | ANY_NUM
  | ANY_REAL
  | ANY_INT
  | ANY_BIT
  | ANY_STRING
  | ANY_DATE
[@@deriving to_yojson]

and derived_ty =
  | DTyUseSingleElement of single_element_ty_spec
  (* | DTyUseArrayType *)
  (* | DTyUseStructType *)
  | DTyUseStringType of elementary_ty
[@@deriving to_yojson]

and derived_ty_decl =
  | DTyDeclSingleElement of string (** type name *) *
                            single_element_ty_spec (** declaration ty *) *
                            expr option (** initialization expression *)
  | DTyDeclSubrange of string (** type name *) *
                       subrange_ty_spec *
                       int (** initial value *)
(* | DTyDeclArrayType *)
(* | DTyDeclStructType *)
[@@deriving to_yojson]

and single_element_ty_spec =
  | DTySpecElementary of elementary_ty
  | DTySpecSimple of string
  | DTySpecGeneric of generic_ty
[@@deriving to_yojson]

and subrange_ty_spec =
  elementary_ty (** integer ty *) *
  int (** lower bound *) *
  int (** upper bound *)
[@@deriving to_yojson]

and constant =
  | CInteger of TI.t * int
                [@name "Integer"]
  | CBool of TI.t * bool
             [@name "Bool"]
  | CReal of TI.t * float
             [@name "Real"]
  | CString of TI.t * string
               [@name "String"]
  | CTimeValue of TI.t * TimeValue.t
                  [@name "TimeValue"]
  | CRange of TI.t * int (** lower bound *) * int (** upper bound *)
              [@name "Range"]
[@@deriving to_yojson, show]

and statement =
  | StmExpr of TI.t *
               expr
               [@name "Expression"]
  | StmElsif of TI.t *
                statement * (** condition *)
                statement list (** body *)
                [@name "Elsif"]
  | StmIf of TI.t *
             statement * (** condition *)
             statement list * (** body *)
             statement list * (** elsif statements *)
             statement list (** else *)
             [@name "If"]
  | StmCase of TI.t *
               expr * (** condition *)
               case_selection list *
               statement list (* else *)
               [@name "Case"]
  | StmFor of (TI.t *
               SymVar.t * (** control variable *)
               expr * (** range start *)
               expr * (** range end *)
               expr option * (** range step *)
               statement list (** body statements *) [@opaque])
              [@name "For"]
  | StmWhile of TI.t *
                expr * (** condition *)
                statement list (** body *)
                [@name "While"]
  | StmRepeat of TI.t *
                 statement list * (** body *)
                 expr (** condition *)
                 [@name "Repeat"]
  | StmExit of TI.t
               [@name "Exit"]
  | StmContinue of TI.t
                   [@name "Continue"]
  | StmReturn of TI.t
                 [@name "Return"]
  | StmFuncParamAssign of string option * (** function param name *)
                          expr * (** assignment expression *)
                          bool (** has inversion in output assignment *)
                          [@name "FuncParamAssign"]
  | StmFuncCall of TI.t *
                   Function.t *
                   statement list (** params assignment *)
                   [@name "FuncCall"]
[@@deriving to_yojson, show { with_path = false }]
and expr =
  | ExprVariable of TI.t * variable               [@name "Variable"]
  | ExprConstant of TI.t * constant               [@name "Constant"]
  | ExprBin      of TI.t * expr * operator * expr [@name "Bin"]
  | ExprUn       of TI.t * operator * expr        [@name "Un"]
  | ExprFuncCall of TI.t * statement              [@name "FuncCall"]
[@@deriving to_yojson, show]
and case_selection = {case: expr list; body: statement list}
[@@deriving to_yojson, show]
(* }}} *)

(* {{{ Functions to work with statements *)
let stmt_get_ti = function
  | StmExpr (ti,_) -> ti
  | StmElsif (ti,_,_) -> ti
  | StmIf (ti,_,_,_,_) -> ti
  | StmCase (ti,_,_,_) -> ti
  | StmFor (ti,_,_,_,_,_) -> ti
  | StmWhile (ti,_,_) -> ti
  | StmRepeat (ti,_,_) -> ti
  | StmExit (ti) -> ti
  | StmContinue (ti) -> ti
  | StmReturn (ti) -> ti
  | StmFuncParamAssign _ -> TI.create_dummy  (* TODO *)
  | StmFuncCall (ti,_,_) -> ti

let stmt_get_id stmt =
  let ti = stmt_get_ti stmt in
  ti.id
(* }}} *)

(* {{{ Functions to work with expressions *)
let expr_get_ti = function
  | ExprVariable (ti,_) -> ti
  | ExprConstant (ti,_) -> ti
  | ExprBin (ti,_,_,_) -> ti
  | ExprUn (ti,_,_) -> ti
  | ExprFuncCall (ti,_) -> ti

let expr_get_id e =
  let ti = expr_get_ti e in
  ti.id
(* }}} *)

(* {{{ Functions to work with constants *)
let c_is_zero c =
  match c with
  | CInteger (_, v) -> phys_equal v 0
  | CBool (_, v) -> phys_equal v false
  | CReal (_, v) -> phys_equal v 0.0
  | CString _ -> false
  | CTimeValue (_, tv) -> TimeValue.is_zero tv
  | CRange (_, lb, ub) -> (phys_equal lb 0) && (phys_equal ub 0)

let c_get_str_value c =
  match c with
  | CInteger (_, v) -> string_of_int v
  | CBool (_, v) -> string_of_bool v
  | CReal (_, v) -> string_of_float v
  | CString (_, v) -> v
  | CTimeValue (_, v) -> TimeValue.to_string v
  | CRange (_, lb, ub) -> Printf.sprintf "%d..%d" lb ub

let c_get_ti c =
  match c with
  | CInteger (ti, _) -> ti
  | CBool (ti, _) -> ti
  | CReal (ti, _) -> ti
  | CString (ti, _) -> ti
  | CTimeValue (ti, _) -> ti
  | CRange (ti, _, _) -> ti

let c_add c1 c2 =
  match (c1, c2) with
  | CInteger (ti, v1), CInteger (_, v2) ->
    let v = v1 + v2 in
    CInteger (ti, v)
  | CBool (ti, v1), CBool (_, v2) ->
    let v = v1 || v2 in
    CBool (ti, v)
  | CReal (ti, v1), CReal (_, v2) ->
    let v = v1 +. v2 in
    CReal (ti, v)
  | CString (ti, v1), CString (_, v2) ->
    let v = v1 ^ v2 in
    CString (ti, v)
  | CTimeValue (ti, v1), CTimeValue (_, v2) ->
    let v = TimeValue.( + ) v1 v2 in
    CTimeValue (ti, v)
  | _ -> raise @@ InternalError "Incompatible types"

let c_from_expr = function
  | ExprConstant(_,v) -> Some v
  | _ -> None

let c_from_expr_exn = function
  | ExprConstant(_,v) -> v
  | _ -> raise @@ InternalError "Incompatible types"
(* }}} *)

(* {{{ Elementary type helpers *)
let ety_is_integer = function
  | SINT | INT | DINT | LINT | USINT | UINT | UDINT | ULINT -> true
  | _ -> false

let ety_is_string = function
  | STRING _ | WSTRING _ | CHAR _ | WCHAR _ -> true
  | _ -> false
(* }}} *)

(* {{{ Configuration objects *)
module Task = struct

  type t = {
    name : string;
    ti : TI.t;
    interval : data_source option;
    single : data_source option;
    priority : int option;
  }
  [@@deriving to_yojson]
  and data_source =
    | DSConstant of constant
    | DSGlobalVar of variable
    | DSDirectVar of variable
    | DSProgOutput of string * variable
  [@@deriving to_yojson]

  let create name ti =
    let interval = None in
    let single = None in
    let priority = None in
    { name; ti; interval; single; priority }

  let set_interval t v = {t with interval = Some v}

  let set_single t v = {t with single = Some v}

  let set_priority t v = {t with priority = Some v}

end

module ProgramConfig = struct
  (** Qualifier of IEC program *)
  type qualifier = QRetain | QNonRetain | QConstant
  [@@deriving to_yojson]

  type t = {
    name : string;
    ti : TI.t;
    qual : qualifier option;
    task : Task.t option;
    conn_vars : variable list; (** Variables connected to program data flow. *)
  } [@@deriving to_yojson]

  let create name ti =
    let qual = None in
    let task = None in
    let conn_vars = [] in
    { name; ti; qual; task; conn_vars }

  let set_qualifier pc q = { pc with qual = Some q }

  let set_task pc t = { pc with task = Some t }

  let set_conn_vars pc conn_vars = { pc with conn_vars }

  let get_name t = t.name

  let to_yojson t = to_yojson t
end
(* }}} *)

(* {{{ Declarations *)
module VarDecl = struct

  type direction = Input | Output
  [@@deriving to_yojson]

  type qualifier = QRetain | QNonRetain | QConstant
  [@@deriving to_yojson]

  type spec =
    | Spec of qualifier option
    | SpecDirect of qualifier option
    | SpecOut of qualifier option
    | SpecIn of qualifier option
    | SpecInOut
    | SpecExternal of qualifier option
    | SpecGlobal of qualifier option
    | SpecAccess of string (** access name *)
    | SpecTemp
    | SpecConfig of
        string (** resource name *) * string (** program name *) * string (** fb name *)
  [@@deriving to_yojson]

  type t = {
    var : variable;
    spec : spec;
    qual: qualifier option;
    dir: direction option;
  }
  [@@deriving to_yojson]

  let create var spec =
    let qual = None in
    let dir = None in
    { var; spec; qual; dir }

  let get_var dcl = dcl.var

  let get_var_name dcl =
    match dcl.var with
    | SymVar v -> SymVar.get_name v
    | DirVar v -> DirVar.get_name v

  let set_qualifier_exn dcl qa =
    match dcl.spec with
    | Spec _ | SpecDirect _ | SpecOut _ | SpecIn _
    | SpecExternal _ | SpecGlobal _ ->
      let s = Spec (Some qa) in
      { dcl with spec = s }
    | _ -> raise @@ InternalError "Can't set qualifier for this type of variables!"

  let get_direction dcl = dcl.dir

  let set_direction dcl d = { dcl with dir = Some d }

  let to_yojson t = to_yojson t
end

type function_decl = {
  id : Function.t;
  return_ty : iec_data_type;
  variables : VarDecl.t list;
  statements : statement list;
}
[@@deriving to_yojson]

type fb_decl = {
  id : FunctionBlock.t;
  variables : VarDecl.t list;
  statements : statement list;
}
[@@deriving to_yojson]

type program_decl = {
  is_retain : bool;
  name : string;
  variables : VarDecl.t list;
  statements : statement list;
}
[@@deriving to_yojson]

type resource_decl = {
  name : string option;
  tasks : Task.t list;
  variables : VarDecl.t list;
  programs : ProgramConfig.t list;
}
[@@deriving to_yojson]

type configuration_decl = {
  name : string;
  resources : resource_decl list;
  variables : VarDecl.t list;
  access_paths : string list;
}
[@@deriving to_yojson]
(* }}} *)

type iec_library_element =
  | IECFunction of      int (** id *) * function_decl      [@name "Function"]
  | IECFunctionBlock of int (** id *) * fb_decl            [@name "FunctionBlock"]
  | IECProgram of       int (** id *) * program_decl       [@name "Program"]
  | IECConfiguration of int (** id *) * configuration_decl [@name "Configuration"]
  | IECType of          int (** id *) * derived_ty_decl    [@name "Type"]
[@@deriving to_yojson]

let next_id =
  let n = ref (-1) in
  fun () ->
    incr n;
    !n

let mk_pou = function
  | `Function      decl -> let id = next_id () in IECFunction(id, decl)
  | `FunctionBlock decl -> let id = next_id () in IECFunctionBlock(id, decl)
  | `Program       decl -> let id = next_id () in IECProgram(id, decl)
  | `Configuration decl -> let id = next_id () in IECConfiguration(id, decl)
  | `Type          decl -> let id = next_id () in IECType(id, decl)

let get_pou_id = function
  | IECFunction (id, _)      -> id
  | IECFunctionBlock (id, _) -> id
  | IECProgram (id, _)       -> id
  | IECConfiguration (id, _) -> id
  | IECType (id, _)          -> id

let get_pou_vars_decl = function
  | IECFunction (_, f)       -> f.variables
  | IECFunctionBlock (_, fb) -> fb.variables
  | IECProgram (_, p)        -> p.variables
  | IECConfiguration _       -> []
  | IECType _                -> []

(* vim: set foldmethod=marker foldlevel=0 foldenable : *)
