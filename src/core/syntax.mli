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
  | ASSIGN
[@@deriving to_yojson, show]
(* }}} *)

(* {{{ Variables and identifiers *)

(** Description of use -- nondefining occurence of an identifier *)
module type ID = sig
  type t
  val create : string -> TI.t -> t
  val get_name : t -> string
  val get_ti : t -> TI.t
end

(** Identifier of symbolically represented variable *)
module SymVar : sig
  include ID
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

  val create : string option -> TI.t -> location -> size option -> int list -> t
  val get_name : t -> string
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

type variable = SymVar of SymVar.t | DirVar of DirVar.t
[@@deriving to_yojson]

val vget_name : variable -> string
(** Return name of a given variable *)

val vget_ti : variable -> TI.t
(** Return token info of a given variable *)
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

(** "Use" occurence of a derived type *)
and derived_ty =
  | DTyUseSingleElement of single_element_ty_spec
  (* | DTyUseArrayType *)
  (* | DTyUseStructType *)
  | DTyUseStringType of elementary_ty
[@@deriving to_yojson]

(** Declaration of a derived type.
    This include "use" symbol value of a declared type and optional
    initialization values. *)
and derived_ty_decl =
  (* Elementary type synonyms and strings *)
  | DTyDeclSingleElement of string (** type name *) *
                            single_element_ty_spec (** declaration ty *) *
                            expr option (** initialization expression *)
  (* Reference: ch. 6.4.4.4 *)
  | DTyDeclSubrange of string (** type name *) *
                       subrange_ty_spec *
                       int (** initial value *)
(* | DTyDeclArrayType *)
(* | DTyDeclStructType *)
[@@deriving to_yojson]

(** Single element type specification (it works like typedef in C) *)
and single_element_ty_spec =
  | DTySpecElementary of elementary_ty
  | DTySpecSimple of string (** derived ty name *)
[@@deriving to_yojson]

(** Subrange type specification *)
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
  | StmAssign of TI.t *
                 variable *
                 expr
  [@name "Assign"]
  | StmElsif of TI.t *
                expr * (** condition *)
                statement list (** body *)
  [@name "Elsif"]
  | StmIf of TI.t *
             expr * (** condition *)
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
[@@deriving to_yojson, show]

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

(* {{{ Functions to work with statements *)
val stmt_get_ti : statement -> TI.t
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

  (** Data sources used in task configruation *)
  type data_source =
    | DSConstant of constant
    | DSGlobalVar of variable
    | DSDirectVar of variable
    | DSProgOutput of string (** program name *) * variable
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

  val set_conn_vars : t -> variable list -> t
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

  (** Variable could be connected to input/output data flow of POU. *)
  type direction = Input | Output
  [@@deriving to_yojson]

  (** Qualifier of IEC variable *)
  type qualifier = QRetain | QNonRetain | QConstant
  [@@deriving to_yojson]

  (** Variable specification *)
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

  val create : variable -> spec -> t

  val get_var : t -> variable

  val get_var_name : t -> string

  val set_qualifier_exn : t -> qualifier -> t
  (** Set qualifier for variable. Raise an exception if this variable doesn't support qualifiers. *)

  val get_direction : t -> direction option

  val set_direction : t -> direction -> t

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
  | IECFunction of function_decl [@name "Function"]
  | IECFunctionBlock of fb_decl [@name "FunctionBlock"]
  | IECProgram of program_decl [@name "Program"]
  | IECConfiguration of configuration_decl [@name "Configuration"]
  | IECType of derived_ty_decl [@name "Type"]
[@@deriving to_yojson]

val get_pou_vars_decl : iec_library_element -> VarDecl.t list
(** Return variables declared for given POU *)

(* vim: set foldmethod=marker foldlevel=0 foldenable : *)
