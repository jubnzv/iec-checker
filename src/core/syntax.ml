(** This module describes all common elements that which make up programming
    model of Structured Text language. See Annex B of IEC61131-3 for reference. *)
open Core
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
  | ASSIGN_REF (** ?= *)
  | SENDTO
  | DEREF (** ^ *)
[@@deriving to_yojson, show]
(* }}} *)

type access_specifier = ASPublic | ASProtected | ASPrivate | ASInternal
[@@deriving to_yojson, show]

type class_specifier = CFinal | CAbstract
[@@deriving to_yojson, show]

(* {{{ Variables and identifiers *)
module type ID = sig
  type t
  val create : string -> TI.t -> t
  val get_name : t -> string
  val get_ti : t -> TI.t
end

module SymVar = struct
  type t = {
    name : string;
    ti : TI.t;
    array_indexes: int option list;
  } [@@deriving to_yojson]

  let create name ti =
    let array_indexes = [] in
    { name; ti; array_indexes; }

  let get_name id = id.name
  let get_ti id = id.ti

  let add_array_index var idx =
    { var with array_indexes = var.array_indexes @ [Some(idx)] }
  let add_array_index_opaque var =
    { var with array_indexes = var.array_indexes @ [None] }
  let get_array_indexes var = var.array_indexes

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
    ti: TI.t;
    loc: location option;
    sz: size option;
    is_partly_located: bool; (** ExprVariable is defined using '*' symbol *)
    path: int list;
  } [@@deriving to_yojson]

  let create ti =
    let loc = None in
    let sz = None in
    let is_partly_located = false in
    let path = [] in
    { ti; loc; sz; is_partly_located; path }

  let get_ti var = var.ti
  let get_loc var = var.loc
  let get_size var = var.sz
  let get_is_partly_located var = var.is_partly_located
  let get_path var = var.path

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

  let path_to_string path =
    List.fold_left
      path
      ~init:[]
      ~f:(fun acc v -> acc @ [(string_of_int v)])
    |> String.concat ~sep:"."

  let to_string t =
    let loc_str = match t.loc with
      | Some(v) -> (location_to_string v)
      | None -> ""
    in
    let sz_str = match t.sz with
      | Some(v) -> (size_to_string v)
      | None -> ""
    in
    Printf.sprintf "%%%s%s%s" sz_str loc_str (path_to_string t.path)

  let to_yojson t = to_yojson t
  let get_name var = to_string var
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

(** Method prototype *)
module MethodPrototype = struct
  type t = {
    name : string;
    ti : TI.t;
    (*return_type : iec_data_type option; (** Type of return value. *)*)
  }
  [@@deriving to_yojson, show]

  (* let create name ti aspec cspec override return_type = *)
  (*   { name; ti; aspec; cspec; override; return_type }   *)

  let create name ti =
    { name; ti; }

  let get_name m = m.name
  let get_ti m = m.ti

  let to_yojson t = to_yojson t
end

module VarUse = struct
  type var_type =
    | Elementary
    | Array
    | String
    | Enum
    | Struct
  [@@deriving to_yojson, show]

  type loc_type =
    | SymVar of SymVar.t
    | DirVar of DirVar.t
  [@@deriving to_yojson]

  type t = {
    loc: loc_type;
    ty: var_type;
  } [@@deriving to_yojson]

  let create_sym sym_var ty =
    let loc = SymVar(sym_var) in
    { loc; ty; }

  let create_dir dir_var ty =
    let loc = DirVar(dir_var) in
    { loc; ty; }

  let get_name var =
    match var.loc with
    | SymVar(v) -> (let n = SymVar.get_name(v) in n)
    | DirVar(v) -> (let n = DirVar.get_name(v) in n)

  let get_ti var =
    match var.loc with
    | SymVar(v) -> (let ti = SymVar.get_ti(v) in ti)
    | DirVar(v) -> (let ti = DirVar.get_ti(v) in ti)

  let get_loc var = var.loc
end
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

(** "Use" occurrence of the derived type. *)
and derived_ty =
  | DTyUseSingleElement of single_element_ty_spec [@name "UseSingleElement"]
  | DTyUseStructType of string                    [@name "UseStructElement"]
  | DTyUseStringType of elementary_ty             [@name "UseStringType"]
[@@deriving to_yojson]

(** Declaration of the derived type (defined with TYPE .. END_TYPE signature). *)
and derived_ty_decl = string (** type name *) * derived_ty_decl_spec

(** Specification used in declaration of the derived type. It could be used
    with variables declaration in POU as well as to declare new types. *)
and derived_ty_decl_spec =
  (* Elementary type synonyms and strings *)
  | DTyDeclSingleElement of single_element_ty_spec (** declaration ty *) *
                            expr option (** initialization expression *)
  | DTyDeclSubrange of subrange_ty_spec *
                       int (** initial value *)
  | DTyDeclEnumType of elementary_ty option (** type of the elements *) *
                       enum_element_spec list (** elements *) *
                       enum_element_spec option (** default element *)
  | DTyDeclArrayType of arr_subrange list (** subranges for dimensions *) *
                        iec_data_type (** type of the elements *) *
                        arr_inval option (** initial value *)
  | DTyDeclRefType of int (** pointers level *) *
                      iec_data_type (** reference type *) *
                      ref_value option (** initial value *)
  | DTyDeclStructType of bool (** is overlap *) *
                         struct_elem_spec list (** elements *)

and single_element_ty_spec =
  | DTySpecElementary of elementary_ty
  | DTySpecSimple of string
  | DTySpecEnum of string
  | DTySpecGeneric of generic_ty
[@@deriving to_yojson]

and subrange_ty_spec =
  elementary_ty (** integer ty *) *
  int (** lower bound *) *
  int (** upper bound *)
[@@deriving to_yojson]

and enum_element_spec = {
  enum_type_name: string option;  (** name of enum which this element belongs to *)
  elem_name: string; (** name of the element *)
  initial_value: constant option; (** initial value *)
} [@@deriving to_yojson]

(** Subranges of array dimensions (e.g. [1..2, 1..3] means list with two
    subranges). *)
and arr_subrange = {
  arr_lower: int [@name "lower"]; (** lower bound *)
  arr_upper: int [@name "upper"]; (** upper bound *)
} [@@deriving to_yojson]

(** Initial value of array elements. Values like [1,2(3),4] will be converted
    to [1,3,3,4] in the parser. *)
and arr_inval = constant list [@@deriving to_yojson]

(** Struct element specification *)
and struct_elem_spec = {
  struct_elem_name: string;
  struct_elem_loc: DirVar.t option;
  struct_elem_ty: single_element_ty_spec;
  struct_elem_init_value: struct_elem_init_value_spec option; (** initial values *)
} [@@deriving to_yojson]

(** Initial value of a struct element *)
and struct_elem_init_value_spec =
  | StructElemInvalConstant of constant                  [@name "InvalConstant"]
  | StructElemInvalEnum of     enum_element_spec         [@name "InvalEnum"]
  (* | StructElemInvalArray of string                    [@name "InvalArray"] *)
  | StructElemInvalStruct of   string (** struct name *) [@name "InvalStruct"]
[@@deriving to_yojson]

and ref_value =
  | RefNull
  | RefSymVar of SymVar.t
  | RefFBInstance of string (** instance name *)
(* | RefClassInstance of string (* instance name *) *)
[@@deriving to_yojson]

and constant =
  | CInteger of TI.t * int           [@name "Integer"]
  | CBool of TI.t * bool             [@name "Bool"]
  | CReal of TI.t * float            [@name "Real"]
  | CString of TI.t * string         [@name "String"]
  | CPointer of TI.t * ref_value     [@name "Pointer"]
  | CTimeValue of TI.t * TimeValue.t [@name "TimeValue"]
  | CRange of TI.t * int (** lower bound *) * int (** upper bound *)
              [@name "Range"]
  | CEnumValue of TI.t * string      [@name "EnumValue"]
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
               statement * (** condition *)
               case_selection list *
               statement list (* else *)
               [@name "Case"]
  | StmFor of (TI.t *
               for_control *
               statement list (** body statements *) [@opaque])
              [@name "For"]
  | StmWhile of TI.t *
                statement * (** condition *)
                statement list (** body *)
                [@name "While"]
  | StmRepeat of TI.t *
                 statement list * (** body *)
                 statement (** condition *)
                 [@name "Repeat"]
  | StmExit of TI.t
               [@name "Exit"]
  | StmContinue of TI.t
                   [@name "Continue"]
  | StmReturn of TI.t
                 [@name "Return"]
  | StmFuncCall of TI.t *
                   Function.t *
                   func_param_assign list
                   [@name "FuncCall"]
[@@deriving to_yojson, show]
and expr =
  | ExprVariable of TI.t * VarUse.t               [@name "Variable"]
  | ExprConstant of TI.t * constant               [@name "Constant"]
  | ExprBin      of TI.t * expr * operator * expr [@name "Bin"]
  | ExprUn       of TI.t * operator * expr        [@name "Un"]
  | ExprFuncCall of TI.t * statement              [@name "FuncCall"]
[@@deriving show, to_yojson]
and case_selection = {case: statement list; body: statement list}
[@@deriving to_yojson, show]
and for_control = {
  assign : statement; (** control variable assignment *)
  range_end : expr; (** range end value *)
  range_step : expr; (** step *)
} [@@deriving to_yojson, show]
and func_param_assign = {
  name : string option; (** function param name *)
  stmt : statement; (** assignment or sendto statement *)
  inverted : bool; (** has inversion in output assignment *)
} [@@deriving to_yojson, show]
(* }}} *)

(* {{{ Functions to work with statements *)
let stmt_get_ti = function
  | StmExpr (ti,_) -> ti
  | StmElsif (ti,_,_) -> ti
  | StmIf (ti,_,_,_,_) -> ti
  | StmCase (ti,_,_,_) -> ti
  | StmFor (ti,_,_) -> ti
  | StmWhile (ti,_,_) -> ti
  | StmRepeat (ti,_,_) -> ti
  | StmExit (ti) -> ti
  | StmContinue (ti) -> ti
  | StmReturn (ti) -> ti
  | StmFuncCall (ti,_,_) -> ti

let stmt_get_id stmt =
  let ti = stmt_get_ti stmt in
  ti.id

let stmt_to_string = function
  | StmExpr _ -> "Expr"
  | StmElsif _ -> "Elsif"
  | StmIf _ -> "If"
  | StmCase _ -> "Case"
  | StmFor _ -> "For"
  | StmWhile _ -> "While"
  | StmRepeat _ -> "Repeat"
  | StmExit _ -> "Exit"
  | StmContinue _ -> "Continue"
  | StmReturn _ -> "Return"
  | StmFuncCall _ -> "FuncCall"
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
  | CPointer (_, v) -> begin
      match v with
      | RefNull -> true
      | RefSymVar _ | RefFBInstance _ -> false
    end
  | CTimeValue (_, tv) -> TimeValue.is_zero tv
  | CRange (_, lb, ub) -> (phys_equal lb 0) && (phys_equal ub 0)
  | CEnumValue _ -> false

let c_get_str_value c =
  match c with
  | CInteger (_, v) -> string_of_int v
  | CBool (_, v) -> string_of_bool v
  | CReal (_, v) -> string_of_float v
  | CString (_, v) -> v
  | CPointer (_, v) -> begin
      match v with
      | RefNull -> "NULL"
      | RefSymVar sv -> SymVar.get_name sv
      | RefFBInstance v -> v
    end
  | CTimeValue (_, v) -> TimeValue.to_string v
  | CRange (_, lb, ub) -> Printf.sprintf "%d..%d" lb ub
  | CEnumValue (_, v) -> v

let c_get_ti c =
  match c with
  | CInteger (ti, _) -> ti
  | CBool (ti, _) -> ti
  | CReal (ti, _) -> ti
  | CString (ti, _) -> ti
  | CPointer (ti, _) -> ti
  | CTimeValue (ti, _) -> ti
  | CRange (ti, _, _) -> ti
  | CEnumValue (ti, _) -> ti

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
    | DSGlobalVar of VarUse.t
    | DSDirectVar of VarUse.t
    | DSProgOutput of string * VarUse.t
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
    conn_vars : VarUse.t list; (** Variables connected to program data flow. *)
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

  type attribute =
    | Var of qualifier option
    | VarOut of qualifier option
    | VarIn of qualifier option
    | VarInOut
    | VarExternal of qualifier option
    | VarGlobal of qualifier option
    | VarAccess of string (** access name *)
    | VarTemp
    | VarConfig of string (** resource name *) *
                   string (** program name *) *
                   string (** fb name *)
  [@@deriving to_yojson]

  type t = {
    var : VarUse.t;
    attr : attribute option;
    qual: qualifier option;
    dir: direction option;
    located_at: DirVar.t option;
    ty_spec: derived_ty_decl_spec option; (** type of this variable *)
    was_init : bool; (** does variable was initialized on declaraction? *)
  }
  [@@deriving to_yojson]

  let create ?(ty_spec=None) var  =
    let qual = None
    and attr = None
    and dir = None
    and located_at = None
    and was_init=false
    in
    { var; attr; qual; dir; located_at; ty_spec; was_init }

  let get_var dcl = dcl.var

  let get_var_name dcl = VarUse.get_name dcl.var

  let get_var_ti dcl = VarUse.get_ti dcl.var

  let set_qualifier_exn dcl qa =
    match dcl.attr with
    | None -> let new_a = Var (Some qa) in { dcl with attr = Some new_a }
    | Some a -> begin
        match a with
        | Var _ | VarOut _ | VarIn _
        | VarExternal _ | VarGlobal _ ->
          let new_a = Var (Some qa) in
          { dcl with attr = Some new_a }
        | _ -> raise @@ InternalError "Can't set qualifier for this type of variables!"
      end

  let set_attr dcl v = { dcl with attr = Some v }
  let get_attr dcl = dcl.attr
  let set_direction dcl v = { dcl with dir = Some v }
  let get_direction dcl = dcl.dir
  let set_located_at var v = { var with located_at = Some v }
  let get_located_at var = var.located_at
  let set_ty_spec var v = { var with ty_spec = Some v }
  let get_ty_spec var = var.ty_spec
  let set_was_init var v = { var with was_init = v }
  let get_was_init var = var.was_init

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

type class_decl = {
  specifier : class_specifier option;
  class_name : string;
  parent_name : string option; (** Name of the parent class. *)
  interfaces : string list; (** Names of the implemented interfaces. *)
  variables : VarDecl.t list; (** Variables declared in this class. *)
  methods : method_decl list;
}
[@@deriving to_yojson]
and interface_decl = {
  interface_name : string;
  parents : string list; (** Names of the parent interfaces. *)
  method_prototypes : MethodPrototype.t list; (** Prototypes of the methods provided by this interface. *)
}
[@@deriving to_yojson]
and method_decl = {
  prototype : MethodPrototype.t;
  statements : statement list;
  aspec : access_specifier; (** Access specifier. *)
  cspec : class_specifier option; (** Class specifier: ABSTRACT or FINAL. *)
  override : bool; (** True if the method is marked with OVERRIDE keyword. *)
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
  | IECClass of         int (** id *) * class_decl         [@name "Class"]
  | IECInterface of     int (** id *) * interface_decl     [@name "Interface"]
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
  | `Class         decl -> let id = next_id () in IECClass(id, decl)
  | `Interface     decl -> let id = next_id () in IECInterface(id, decl)
  | `Configuration decl -> let id = next_id () in IECConfiguration(id, decl)
  | `Type          decl -> let id = next_id () in IECType(id, decl)

let get_pou_id = function
  | IECFunction (id, _)      -> id
  | IECFunctionBlock (id, _) -> id
  | IECProgram (id, _)       -> id
  | IECClass (id, _)         -> id
  | IECInterface (id, _)     -> id
  | IECConfiguration (id, _) -> id
  | IECType (id, _)          -> id

let get_pou_vars_decl = function
  | IECFunction (_, f)       -> f.variables
  | IECFunctionBlock (_, fb) -> fb.variables
  | IECProgram (_, p)        -> p.variables
  | IECClass (_, p)          -> p.variables
  | IECInterface _           -> []
  | IECConfiguration _       -> []
  | IECType _                -> []

(* {{{ Yojson helpers *)
let derived_ty_decl_to_yojson (dty : derived_ty_decl) : Yojson.Safe.t =
  let (type_name, dty_spec) = dty in
  match dty_spec with
  | DTyDeclSingleElement (declaration_ty, expr_opt) -> begin
      match expr_opt with
      | Some (e) ->
        `Assoc [
          "type", `String("SingleElement");
          "name", `String(type_name);
          "declaration_type", single_element_ty_spec_to_yojson declaration_ty;
          "init_expr", expr_to_yojson e;
        ]
      | None ->
        `Assoc [
          "type", `String("SingleElement");
          "name", `String(type_name);
          "declaration_type", single_element_ty_spec_to_yojson declaration_ty;
        ]
    end
  | DTyDeclSubrange (spec, init_val) -> begin
      let (int_ty, lb, ub) = spec in
      `Assoc [
        "type", `String("Subrange");
        "name", `String(type_name);
        "values_type", elementary_ty_to_yojson int_ty;
        "lower_bound", `Int(lb);
        "upper_bound", `Int(ub);
        "init_val", `Int(init_val);
      ]
    end
  | DTyDeclEnumType (elements_type_opt, _, _) -> begin
      match elements_type_opt with
      | Some (elements_type) ->
        `Assoc [
          "type", `String("Enum");
          "name", `String(type_name);
          "elements_type", elementary_ty_to_yojson elements_type;
        ]
      | None ->
        `Assoc [
          "type", `String("Enum");
          "name", `String(type_name);
        ]
    end
  | DTyDeclArrayType (subranges, ty, inval_opt) -> begin
      let (subranges_json : Yojson.Safe.t list) = List.fold_left
          subranges
          ~init:[]
          ~f:(fun acc sr -> acc @ [(arr_subrange_to_yojson sr)])
      in
      match inval_opt with
      | Some(inval) ->
        `Assoc [
          "type", `String("Array");
          "name", `String(type_name);
          "subranges", `List(subranges_json);
          "elements_type", iec_data_type_to_yojson ty;
          "init_val", arr_inval_to_yojson inval;
        ]
      | None ->
        `Assoc [
          "type", `String("Array");
          "name", `String(type_name);
          "subranges", `List(subranges_json);
          "elements_type", iec_data_type_to_yojson ty;
        ]
    end
  | DTyDeclRefType (ref_level, ty, opt_inval) -> begin
      match opt_inval with
      | Some(inval) ->
        `Assoc [
          "type", `String("Ref");
          "name", `String(type_name);
          "ref_level", `Int(ref_level);
          "ref_type", iec_data_type_to_yojson ty;
          "init_val", ref_value_to_yojson inval;
        ]
      | None ->
        `Assoc [
          "type", `String("Ref");
          "name", `String(type_name);
          "ref_level", `Int(ref_level);
          "ref_type", iec_data_type_to_yojson ty;
        ]
    end
  | DTyDeclStructType (is_overlap, elem_specs) -> begin
      let elems_yojson = List.fold_left
          elem_specs
          ~init:[]
          ~f:(fun acc e -> acc @ [struct_elem_spec_to_yojson e])
      in
      `Assoc [
        "type", `String("Struct");
        "name", `String(type_name);
        "overlap", `Bool(is_overlap);
        "elem_specs", `List(elems_yojson);
      ]
    end
(* }}} *)

(* vim: set foldmethod=marker foldlevel=0 foldenable : *)
