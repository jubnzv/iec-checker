(** This module describes all common elements that which make up programming
    model of Structured Text language. See Annex B of IEC61131-3 for reference. *)
open Core_kernel
module TI = Tok_info
module E = Error

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

  let loc_to_string = function
    | LocI -> "I"
    | LocQ -> "Q"
    | LocM -> "M"

  type size =
    | SizeX    (** single bit *)
    | SizeNone (** single bit *)
    | SizeB    (** byte *)
    | SizeW    (** word (16 bits) *)
    | SizeD    (** double word (32 bits) *)
    | SizeL    (** quad word (64 bits) *)
  [@@deriving to_yojson]

  let size_to_string = function
    | None -> ""
    | Some(s) -> (match s with
        | SizeX    -> "X"
        | SizeNone -> ""
        | SizeB    -> "B"
        | SizeW    -> "W"
        | SizeD    -> "D"
        | SizeL    -> "L")

  let path_to_string path = List.fold_left path ~f:(fun s p -> s ^ (Printf.sprintf ".%d" p)) ~init:""

  type t = { name: string; ti: TI.t; loc: location; sz: size option; path: int list; } [@@deriving to_yojson]

  let create opt_name ti loc sz path =
    let name =
      match opt_name with
      | None -> let n = Printf.sprintf "%s%s%s" (loc_to_string loc) (size_to_string sz) (path_to_string path) in n
      | Some(n) -> n
    in
    { name; ti; loc; sz; path; }

  let get_name var = var.name

  let get_ti var = var.ti

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
[@@deriving to_yojson]

and subrange_ty_spec =
  elementary_ty (** integer ty *) *
  int (** lower bound *) *
  int (** upper bound *)
[@@deriving to_yojson]

and constant =
  | CInteger of int * TI.t
  | CBool of bool * TI.t
  | CReal of float * TI.t
  | CString of string * TI.t
  | CTimeValue of TimeValue.t * TI.t
  | CRange of TI.t * int * int
[@@deriving to_yojson, show]

and statement =
  | StmAssign of TI.t *
                 variable *
                 expr
  | StmElsif of TI.t *
                expr * (** condition *)
                statement list (** body *)
  | StmIf of TI.t *
             expr * (** condition *)
             statement list * (** body *)
             statement list * (** elsif statements *)
             statement list (** else *)
  | StmCase of TI.t *
               expr * (** condition *)
               case_selection list *
               statement list (* else *)
  | StmFor of (TI.t *
               SymVar.t * (** control variable *)
               expr * (** range start *)
               expr * (** range end *)
               expr option * (** range step *)
               statement list (** body statements *) [@opaque])
  | StmWhile of TI.t *
                expr * (** condition *)
                statement list (** body *)
  | StmRepeat of TI.t *
                 statement list * (** body *)
                 expr (** condition *)
  | StmExit of TI.t
  | StmContinue of TI.t
  | StmFuncParamAssign of string option * (** function param name *)
                          expr * (** assignment expression *)
                          bool (** has inversion in output assignment *)
  | StmFuncCall of TI.t *
                   Function.t *
                   statement list (** params assignment *)
[@@deriving to_yojson, show { with_path = false }]
and expr =
  | Variable of variable
  | Constant of constant
  | BinExpr of expr * operator * expr
  | UnExpr of operator * expr
  | FuncCall of statement
[@@deriving to_yojson, show]
and case_selection = {case: expr list; body: statement list}
[@@deriving to_yojson, show]
(* }}} *)

(* {{{ Functions to work with constants *)
let c_is_zero c =
  match c with
  | CInteger (v, _) -> phys_equal v 0
  | CBool (v, _) -> phys_equal v false
  | CReal (v, _) -> phys_equal v 0.0
  | CString _ -> false
  | CTimeValue (tv, _) -> TimeValue.is_zero tv
  | CRange (_, lb, ub) -> (phys_equal lb 0) && (phys_equal ub 0)

let c_get_str_value c =
  match c with
  | CInteger (v, _) -> string_of_int v
  | CBool (v, _) -> string_of_bool v
  | CReal (v, _) -> string_of_float v
  | CString (v, _) -> v
  | CTimeValue (v, _) -> TimeValue.to_string v
  | CRange (_, lb, ub) -> Printf.sprintf "%d..%d" lb ub

let c_get_ti c =
  match c with
  | CInteger (_, ti) -> ti
  | CBool (_, ti) -> ti
  | CReal (_, ti) -> ti
  | CString (_, ti) -> ti
  | CTimeValue (_, ti) -> ti
  | CRange (ti, _, _) -> ti

let c_add c1 c2 =
  match (c1, c2) with
  | CInteger (v1, ti), CInteger (v2, _) ->
    let v = v1 + v2 in
    CInteger (v, ti)
  | CBool (v1, ti), CBool (v2, _) ->
    let v = v1 || v2 in
    CBool (v, ti)
  | CReal (v1, ti), CReal (v2, _) ->
    let v = v1 +. v2 in
    CReal (v, ti)
  | CString (v1, ti), CString (v2, _) ->
    let v = v1 ^ v2 in
    CString (v, ti)
  | CTimeValue (v1, ti), CTimeValue (v2, _) ->
    let v = TimeValue.( + ) v1 v2 in
    CTimeValue (v, ti)
  | _ -> raise @@ InternalError "Incompatible types"

let c_from_expr = function
  | Constant(v) -> Some v
  | _ -> None

let c_from_expr_exn = function
  | Constant(v) -> v
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

  type t = { var : variable; spec : spec; qual: qualifier option; dir: direction option;  }
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
  | IECFunction of function_decl
  | IECFunctionBlock of fb_decl
  | IECProgram of program_decl
  | IECConfiguration of configuration_decl
  | IECType of derived_ty_decl list
[@@deriving to_yojson]

let get_pou_vars_decl = function
  | IECFunction f -> f.variables
  | IECFunctionBlock fb -> fb.variables
  | IECProgram p -> p.variables
  | IECConfiguration _ -> []
  | IECType _ -> []

(* vim: set foldmethod=marker foldlevel=0 foldenable : *)
