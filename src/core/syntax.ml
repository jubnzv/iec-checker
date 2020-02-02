(* This module describes all common elements that which make up programming
   model of Structured Text language. See Annex B of IEC61131-3 for reference. *)
open Core_kernel
module TI = Tok_info

exception InternalError of string

type operator =
  | NEG
  | NOT
  | POW
  | MUL
  | DIV
  | MOD
  | AND (* & AND *)
  | ADD (* + *)
  | SUB (* - *)
  | OR (* OR *)
  | XOR (* XOR *)
  | GT (* > *)
  | LT (* < *)
  | GE (* >= *)
  | LE (* <= *)
  | EQ (* = *)
  | NEQ (* <> *)
  | ASSIGN

type iec_data_type =
  | TyElementary of elementary_ty
  | TyGeneric of generic_ty
  | TyDerived of derived_ty

and elementary_ty =
  | NIL (* TODO: replace with an empty symbol *)
  | STRING
  | WSTRING
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

and ty_decl = DTyDecl of string (* name *) * derived_ty

(* Specification of derived ty *)
and derived_ty =
  | DTySingleElementTy of single_element_ty_spec
  (* | ArrayType of string * iec_array_type_spec *)
  (* | StructureType of string * iec_structure_type_spec *)
  (* | StringType of string * iec_string_type_spec *)
  | DTyStringTy of string * int

and iec_array_size = Capacity of int | Range of int * int

and iec_array_type_spec = {
  (* TODO: array_type: iec_data_type; *) size : iec_array_size;
}

and iec_structure_type_element = {
  name : string; (* TODO: element_type : iec_data_type; *)
}

and iec_structure_type_spec = { elements : iec_structure_type_element list }

and iec_string_type_spec = { capacity : int }

and single_element_ty_spec =
  | SETyElementaryTy of elementary_ty
  | SETySETy of string

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
  [@@deriving show, fields]

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
end

type constant =
  | CInteger of int * TI.t
  | CBool of bool * TI.t
  | CReal of float * TI.t
  | CString of string * TI.t
  | CTimeValue of TimeValue.t * TI.t

let c_is_zero c =
  match c with
  | CInteger (v, _) -> phys_equal v 0
  | CBool (v, _) -> phys_equal v false
  | CReal (v, _) -> phys_equal v 0.0
  | CString _ -> false
  | CTimeValue (tv, _) -> TimeValue.is_zero tv

let c_get_str_value c =
  match c with
  | CInteger (v, _) -> string_of_int v
  | CBool (v, _) -> string_of_bool v
  | CReal (v, _) -> string_of_float v
  | CString (v, _) -> v
  | CTimeValue (v, _) -> TimeValue.to_string v

let c_get_ti c =
  match c with
  | CInteger (_, ti) -> ti
  | CBool (_, ti) -> ti
  | CReal (_, ti) -> ti
  | CString (_, ti) -> ti
  | CTimeValue (_, ti) -> ti

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

module Variable = struct
  type direction = Input | Output

  type t = { name : string; ti : TI.t; dir : direction option }

  let create name ti =
    let dir = None in
    { name; ti; dir }

  let get_name var = var.name

  let get_ti var = var.ti

  let get_direction var = var.dir

  let set_direction var d = { var with dir = Some d }
end

(* Qualifier of IEC variable. *)
type var_qualifier = VarQRetain | VarQNonRetain | VarQConstant

(* Location prefixes for directly represented variables.
   See 2.4.1.1 and Table 15 for explaination. *)
type direct_var_location = DirVarLocI | DirVarLocQ | DirVarLocM

(* Size prefixes for directly represented variables.
   See 2.4.1.1 and Table 15 for explaination. *)
and direct_var_size =
  | DirVarSizeX (* single bit *)
  | DirVarSizeNone (* single bit *)
  | DirVarSizeB (* byte *)
  | DirVarSizeW (* word (16 bits) *)
  | DirVarSizeD (* double word (32 bits) *)
  | DirVarSizeL

(* quad word (64 bits) *)

(* Variable specification *)
and var_spec =
  | VarSpec of var_qualifier option
  | VarSpecDirect of
      direct_var_location
      * direct_var_size option
      * int list
  (* address *)
      * var_qualifier option
  | VarSpecOut of var_qualifier option
  | VarSpecIn of var_qualifier option
  | VarSpecInOut
  | VarSpecExternal of var_qualifier option
  | VarSpecGlobal of var_qualifier option
  | VarSpecAccess of string (* access name *)
  | VarSpecTemp
  | VarSpecConfig of
      string (* resource name *) * string (* program name *) * string

(* fb name *)

module VariableDecl = struct
  type t = { var : Variable.t; spec : var_spec }

  let create var spec = { var; spec }

  let get_var d = d.var

  let set_qualifier d qa =
    match d.spec with
    | VarSpec _ | VarSpecDirect _ | VarSpecOut _ | VarSpecIn _
    | VarSpecExternal _ | VarSpecGlobal _ ->
      let s = VarSpec (Some qa) in
      { d with spec = s }
    | VarSpecInOut | VarSpecAccess _ | VarSpecTemp | VarSpecConfig _ -> d
end

(* Arbitrary expressions of ST language *)
type expr =
  | Nil of TI.t
  | Variable of Variable.t
  | Constant of constant
  | BinExpr of expr * operator * expr
  | UnExpr of operator * expr

let c_from_expr = function
    | Constant(v) -> Some v
    | _ -> None

let c_from_expr_exn = function
    | Constant(v) -> v
    | _ -> raise @@ InternalError "Incompatible types"

module Function = struct
  type t = { name : string; ti : TI.t; is_std : bool }

  let create name ti =
    let is_std = false in
    { name; ti; is_std }

  let get_name fn = fn.name

  let get_ti fn = fn.ti

  let is_std fn = fn.is_std
end

type function_decl = {
  id : Function.t;
  return_ty : iec_data_type;
  variables : VariableDecl.t list;
  statements : expr list;
}

module FunctionBlock = struct
  type t = { name : string; ti : TI.t; is_std : bool }

  let create name ti =
    let is_std = false in
    { name; ti; is_std }

  let get_name fb = fb.name

  let get_ti fb = fb.ti

  let is_std fb = fb.is_std
end

type fb_decl = {
  id : FunctionBlock.t;
  variables : VariableDecl.t list;
  statements : expr list; (* return_ty : iec_data_type; *)
}

type program_decl = {
  is_retain : bool;
  name : string;
  variables : VariableDecl.t list;
  statements : expr list;
}

module Task = struct

  type t = {
    name : string;
    ti : TI.t;
    interval : data_source option;
    single : data_source option;
    priority : int option;
  }
  (** Data sources used in task configruation *)
  and data_source =
    | DSConstant of constant
    | DSGlobalVar of Variable.t
    | DSDirectVar of Variable.t
    | DSProgOutput of string * Variable.t

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
  type t = {
    name : string;
    ti : TI.t;
    qual : var_qualifier option;
    task : Task.t option;
    conn_vars : Variable.t list; (* Variables connected to program data flow. *)
  }

  let create name ti =
    let qual = None in
    let task = None in
    let conn_vars = [] in
    { name; ti; qual; task; conn_vars }

  let set_qualifier pc q = { pc with qual = Some q }

  let set_task pc t = { pc with task = Some t }

  let set_conn_vars pc conn_vars = { pc with conn_vars }

  let get_name t = t.name
end

type resource_decl = {
  name : string option;
  tasks : Task.t list;
  variables : VariableDecl.t list;
  programs : ProgramConfig.t list;
}

type configuration_decl = {
  name : string;
  resources : resource_decl list;
  variables : VariableDecl.t list;
  access_paths : string list;
}

type iec_library_element =
  | IECFunction of function_decl
  | IECFunctionBlock of fb_decl
  | IECProgram of program_decl
  | IECConfiguration of configuration_decl
