module TI = Tok_info

exception InternalError of string

(** Representation of time interval

    According the IEC61131-3 3rd edition grammar, all interval values defined as
    fix_point. That means that these values could be represented as float values.
*)
module TimeValue : sig
  type t

  val mk :
    ?y:int ->
    ?mo:int ->
    ?d:float ->
    ?h:float ->
    ?m:float ->
    ?s:float ->
    ?ms:float ->
    ?us:float ->
    ?ns:float ->
    unit ->
    t

  val ( + ) : t -> t -> t

  val inv : t -> t
  (** Invert time value to represent a negative duration. *)

  val to_string : t -> string

  val is_zero : t -> bool

  val to_yojson : t -> Yojson.Safe.t
end

(* {{{ Operators *)
(** Operators of the ST language *)
type operator =
  | NEG
  | NOT
  | POW
  | MUL
  | DIV
  | MOD
  | AND (** & AND *)
  | ADD (** + *)
  | SUB (** - *)
  | OR (** OR *)
  | XOR (** XOR *)
  | GT (** > *)
  | LT (** < *)
  | GE (** >= *)
  | LE (** <= *)
  | EQ (** = *)
  | NEQ (** <> *)
  | ASSIGN (** := *)
  | ASSIGN_REF (** ?= *)
  | SENDTO (** => *)
  | DEREF (** ^ *)
[@@deriving to_yojson, show]
(* }}} *)

type access_specifier = ASPublic | ASProtected | ASPrivate | ASInternal
[@@deriving to_yojson, show]

type class_specifier = CFinal | CAbstract
[@@deriving to_yojson, show]

(* {{{ Variables and identifiers *)

(** Description of use -- nondefining occurrence of an identifier. *)
module type ID = sig
  type t
  val create : string -> TI.t -> t
  val get_name : t -> string
  val get_ti : t -> TI.t
end

(** Identifier of symbolically represented variable *)
module SymVar : sig
  include ID

  val add_array_index : t -> int -> t
  (** [add_array_index var index] Add array addressation to [index]. There may
      be more than one indexes for multi-dimension arrays, e.g. A[1][3]. *)
  val add_array_index_opaque : t -> t
  (** [add_array_index_opaque var] Add array addressation to unknown index
      that can't be evaluated before run time, e.g. A[f(a,b)]. *)
  val get_array_indexes : t -> int option list

  val to_yojson : t -> Yojson.Safe.t
end

(** Identifier of directly represented variable *)
module DirVar : sig
  include ID

  (** Location prefixes for directly represented variables.
      See 6.5.5.2 for explainations. *)
  type location = LocI | LocQ | LocM
  [@@deriving to_yojson]

  (** Size prefixes for directly represented variables. *)
  type size =
    | SizeX    (** single bit *)
    | SizeNone (** single bit *)
    | SizeB    (** byte *)
    | SizeW    (** word (16 bits) *)
    | SizeD    (** double word (32 bits) *)
    | SizeL    (** quad word (64 bits) *)
  [@@deriving to_yojson]

  val create : TI.t -> t
  (** Create an empty instance of the direct variable. *)

  val get_ti : t -> TI.t
  val get_loc : t -> location option
  val get_size : t -> size option
  val get_is_partly_located : t -> bool
  val get_path : t -> int list

  val set_ti : t -> TI.t -> t
  val set_loc : t -> location -> t
  val set_size : t -> size -> t
  val set_is_partly_located : t -> bool -> t
  val set_path : t -> int list -> t

  val size_to_string : size -> string
  val size_of_string : string -> size option

  val location_to_string : location -> string
  val location_of_string : string -> location option

  val path_to_string : int list -> string

  val to_string : t -> string
  val to_yojson : t -> Yojson.Safe.t
end

(** Function Block identifier *)
module FunctionBlock : sig
  include ID
  val is_std : t -> bool
  val to_yojson : t -> Yojson.Safe.t
end

(** Function identifier *)
module Function : sig
  include ID
  val is_std : t -> bool
  (** Returns true if function declared in standard library. *)
  val to_yojson : t -> Yojson.Safe.t
end

(** Method prototype *)
module MethodPrototype : sig
  type t
  val create : string -> TI.t -> t
  val to_yojson : t -> Yojson.Safe.t
  val get_name : t -> string
  val get_ti : t -> TI.t
end

(** "Use" occurrence of the variable *)
module VarUse : sig
  type t

  type var_type =
    | Elementary
    | Array
    | String
    | Enum
    | Struct

  (** Location type of the variable *)
  type loc_type =
    | SymVar of SymVar.t
    | DirVar of DirVar.t

  val create_sym : SymVar.t -> var_type -> t
  val create_dir : DirVar.t -> var_type -> t

  val get_name : t -> string
  val get_ti : t -> TI.t
  val get_loc : t -> loc_type
end
(* }}} *)

(* {{{ Data types, constants and statements *)
(* FIXME: Here is some problems with redefining derived_ty_decl_to_yojson. *)
[@@@warning "-32"]
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

(** "Use" occurence of the derived type. *)
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

(** Single element type specification (it works like typedef in C) *)
and single_element_ty_spec =
  | DTySpecElementary of elementary_ty
  | DTySpecSimple of string (** derived ty name *)
  | DTySpecEnum of string
  | DTySpecGeneric of generic_ty
[@@deriving to_yojson]

(** Subrange type specification *)
and subrange_ty_spec =
  elementary_ty (** integer ty *) *
  int (** lower bound *) *
  int (** upper bound *)
[@@deriving to_yojson]

(** Enum element specification *)
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
and for_control = {
  assign : statement; (** control variable assignment *)
  range_end : expr; (** range end value *)
  range_step : expr; (** step *)
}
[@@deriving to_yojson, show]
and func_param_assign = {
  name : string option; (** function param name *)
  stmt : statement; (** assignment or sendto statement *)
  inverted : bool; (** has inversion in output assignment *)
} [@@deriving to_yojson, show]
(* }}} *)

(* {{{ Functions to work with statements *)
val stmt_get_ti : statement -> TI.t
val stmt_get_id : statement -> int
val stmt_to_string : statement -> string
(* }}} *)

(* {{{ Functions to work with expressions *)
val expr_get_ti : expr -> TI.t
val expr_get_id : expr -> int
(* }}} *)

(* {{{ Functions to work with constants *)
val c_is_zero : constant -> bool
(** Return true if constant value is zero, false otherwise *)

val c_get_str_value : constant -> string
(** Return string representation for value of a given constant *)

val c_get_ti : constant -> TI.t
(** Return token info of a given constant *)

val c_add : constant -> constant -> constant
(** Add value to existing constant. *)

val c_from_expr : expr -> constant option
(** Convert given expr to const. *)

val c_from_expr_exn : expr -> constant
(** Convert given expr to const. Raise an InternalError exception if given expr is not constant.  *)
(* }}} *)

(* {{{ Elementary type helpers *)
val ety_is_integer : elementary_ty -> bool
val ety_is_string : elementary_ty -> bool
(* }}} *)

(* {{{ Configuration objects *)
(** Task configuration.
    See: 6.8.2 Tasks *)
module Task : sig
  type t

  (** Data sources used in task configuration *)
  type data_source =
    | DSConstant of constant
    | DSGlobalVar of VarUse.t
    | DSDirectVar of VarUse.t
    | DSProgOutput of string (** program name *) * VarUse.t
  [@@deriving to_yojson]

  val create : string -> TI.t -> t

  val set_interval : t -> data_source -> t
  (** Set task interval input. *)

  val set_single : t -> data_source -> t
  (** Set task single input. *)

  val set_priority : t -> int -> t
  (** Set task priority value. *)
end

module ProgramConfig : sig
  type t

  (** Qualifier of IEC program *)
  type qualifier = QRetain | QNonRetain | QConstant
  [@@deriving to_yojson]

  val create : string -> TI.t -> t

  val set_qualifier : t -> qualifier -> t
  (** Set program qualifier. *)

  val set_task : t -> Task.t -> t
  (** Set task configuration. *)

  val set_conn_vars : t -> VarUse.t list -> t
  (** Set connected variables. *)

  val get_name : t -> string
  (** Get name of a program. *)

  val to_yojson : t -> Yojson.Safe.t
end
(* }}} *)

(* {{{ Declarations *)
(** Declaration of the IEC variable *)
module VarDecl : sig
  type t

  (** ExprVariable could be connected to input/output data flow of POU. *)
  type direction = Input | Output
  [@@deriving to_yojson]

  (** Qualifier of IEC variable *)
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

  val create : ?ty_spec:(derived_ty_decl_spec option) -> VarUse.t -> t

  val get_var : t -> VarUse.t
  val get_var_name : t -> string
  val get_var_ti : t -> TI.t
  val set_qualifier_exn : t -> qualifier -> t
  (** Set qualifier for variable. Raise an exception if this variable doesn't support qualifiers. *)
  val set_attr : t -> attribute -> t
  val get_attr : t -> attribute option
  val set_direction : t -> direction -> t
  val get_direction : t -> direction option
  val set_located_at : t -> DirVar.t -> t
  val get_located_at : t -> DirVar.t option
  val set_ty_spec : t -> derived_ty_decl_spec -> t
  val get_ty_spec : t -> derived_ty_decl_spec option
  val set_was_init : t -> bool -> t
  val get_was_init : t -> bool

  val to_yojson : t -> Yojson.Safe.t
end

(** Function declaration *)
type function_decl = {
  id : Function.t;
  return_ty : iec_data_type;
  variables : VarDecl.t list;
  statements : statement list;
}
[@@deriving to_yojson]

(** Function block declaration *)
type fb_decl = {
  id : FunctionBlock.t;
  variables : VarDecl.t list;
  statements : statement list;
}
[@@deriving to_yojson]

type program_decl = {
  is_retain : bool;
  name : string;
  variables : VarDecl.t list; (** Variables declared in this program *)
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
  name : string option; (** Resource name. Can be is skipped in case of single resource *)
  tasks : Task.t list;
  variables : VarDecl.t list; (** Global variables *)
  programs : ProgramConfig.t list; (** Configuration of program instances. *)
}
[@@deriving to_yojson]

(** Configuration element.
    See: 2.7 Configuration elements *)
type configuration_decl = {
  name : string;
  resources : resource_decl list;
  variables : VarDecl.t list; (** Global variables and access lists *)
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

val mk_pou : [< `Function of function_decl
             | `FunctionBlock of fb_decl
             | `Program of program_decl
             | `Class of class_decl
             | `Interface of interface_decl
             | `Configuration of configuration_decl
             | `Type of derived_ty_decl ] -> iec_library_element

val get_pou_id : iec_library_element -> int
(** [get_pou_id] Get unique identifier of the given library element. *)

val get_pou_vars_decl : iec_library_element -> VarDecl.t list
(** [get_pou_vars_decl] Return variables declared for the given POU. *)

(* {{{ Yojson helpers *)
val derived_ty_decl_to_yojson : derived_ty_decl -> Yojson.Safe.t
(* }}} *)

(* vim: set foldmethod=marker foldlevel=0 foldenable : *)
