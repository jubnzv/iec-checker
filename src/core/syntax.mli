module TI = Tok_info

exception InternalError of string

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
  (* }}} *)

(* {{{ Data types *)
type iec_data_type =
  | TyElementary of elementary_ty
  | TyGeneric of generic_ty
  | TyDerived of derived_ty

and elementary_ty =
  | NIL (* TODO: replace with an empty symbol *)
  | STRING of int (** length *)
  | WSTRING of int (** length *)
  | CHAR
  | WCHAR
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

(** Declaration of derived type *)
and ty_decl = DTyDecl of string (* name *) * derived_ty

(** Specification of derived type *)
and derived_ty =
  | DTySingleElementTy of single_element_ty_spec
  (* | ArrayType of string * iec_array_type_spec *)
  (* | StructureType of string * iec_structure_type_spec *)
  (* | StringType of string * iec_string_type_spec *)
  | DTyStringTy of string * int

(** Arrays *)
and iec_array_size = Capacity of int | Range of int * int

and iec_array_type_spec = {
  (* TODO: array_type: iec_data_type; *) size : iec_array_size;
}

(** Structures *)
and iec_structure_type_element = {
  name : string; (* TODO: element_type : iec_data_type; *)
}

and iec_structure_type_spec = { elements : iec_structure_type_element list }

(** String literals *)
and iec_string_type_spec = { capacity : int }

(** Single element types is user defined tys which works like typedefs in C. *)
and single_element_ty_spec =
  | SETyElementaryTy of elementary_ty
  | SETySETy of string (** derived ty name *)

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
end
(* }}} *)

(* {{{ Constants *)
type constant =
  | CInteger of int * TI.t
  | CBool of bool * TI.t
  | CReal of float * TI.t
  | CString of string * TI.t
  | CTimeValue of TimeValue.t * TI.t

val c_is_zero : constant -> bool
(** Return true if constant value is zero, false otherwise *)

val c_get_str_value : constant -> string
(** Return string representation for value of a given constant *)

val c_get_ti : constant -> TI.t
(** Return token info of a given constant *)

val c_add : constant -> constant -> constant
(** Add value to existing constant. *)
(* }}} *)

(* {{{ Identifiers *)

(** Description of use -- nondefining occurence of an identifier *)
module type ID = sig
  type t
  val create : string -> TI.t -> t
  val get_name : t -> string
  val get_ti : t -> TI.t
end

(** Identifier of symbolically represented variable *)
module SymVar : ID

(** Identifier of directly represented variable *)
module DirVar : sig
  include ID

  (** Location prefixes for directly represented variables.
      See 6.5.5.2 for explainations. *)
  type location = LocI | LocQ | LocM

  (** Size prefixes for directly represented variables. *)
  type size =
    | SizeX    (** single bit *)
    | SizeNone (** single bit *)
    | SizeB    (** byte *)
    | SizeW    (** word (16 bits) *)
    | SizeD    (** double word (32 bits) *)
    | SizeL    (** quad word (64 bits) *)

  val create : string option -> TI.t -> location -> size option -> int list -> t
  val get_name : t -> string
end

(** Function Block identifier *)
module FunctionBlock : sig
  include ID
  val is_std : t -> bool
end

(** Function identifier *)
module Function : sig
  include ID
  val is_std : t -> bool
  (** Returns true if function declared in standard library. *)
end
(* }}} *)

type variable = SymVar of SymVar.t | DirVar of DirVar.t

val vget_name : variable -> string
(** Return name of a given variable *)

val vget_ti : variable -> TI.t
(** Return token info of a given variable *)

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

  val create : string -> TI.t -> t

  val set_qualifier : t -> qualifier -> t
  (** Set program qualifier. *)

  val set_task : t -> Task.t -> t
  (** Set task configuration. *)

  val set_conn_vars : t -> variable list -> t
  (** Set connected variables. *)

  val get_name : t -> string
  (** Get name of a program. *)
end
(* }}} *)

(* {{{ Statements and expressions *)
(** Statements *)
type statement =
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
  | StmFor of TI.t *
              SymVar.t * (** control variable *)
              expr * (** range start *)
              expr * (** range end *)
              expr option * (** range step *)
              statement list (** body statements *)
  | StmFuncParamAssign of string option * (** function param name *)
                          expr * (** assignment expression *)
                          bool (** has inversion in output assignment *)
  | StmFuncCall of TI.t *
                   Function.t *
                   statement list (** params assignment *)
and expr =
  | Variable of variable
  | Constant of constant
  | BinExpr of expr * operator * expr
  | UnExpr of operator * expr
(* FIXME: These values should have a separate type which supports subranges and enums. *)
and case_selection = {case: int list; body: statement list}

val c_from_expr : expr -> constant option
(** Convert given expr to const. *)

val c_from_expr_exn : expr -> constant
(** Convert given expr to const. Raise an InternalError exception if given expr is not constant.  *)
(* }}} *)

(* {{{ Declarations *)
(** Declaration of the IEC variable *)
module VarDecl : sig
  type t

  (** Variable could be connected to input/output data flow of POU. *)
  type direction = Input | Output

  (** Qualifier of IEC variable *)
  type qualifier = QRetain | QNonRetain | QConstant

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

  val create : variable -> spec -> t

  val get_var : t -> variable

  val get_var_name : t -> string

  val set_qualifier_exn : t -> qualifier -> t
  (** Set qualifier for variable. Raise an exception if this variable doesn't support qualifiers. *)

  val get_direction : t -> direction option

  val set_direction : t -> direction -> t
end

(** Function declaration *)
type function_decl = {
  id : Function.t;
  return_ty : iec_data_type;
  variables : VarDecl.t list;
  statements : statement list;
}

(** Function block declaration *)
type fb_decl = {
  id : FunctionBlock.t;
  variables : VarDecl.t list;
  statements : statement list;
}

type program_decl = {
  is_retain : bool;
  name : string;
  variables : VarDecl.t list; (** Variables declared in this program *)
  statements : statement list;
}

type resource_decl = {
  name : string option; (** Resource name. Can be is skipped in case of single resource *)
  tasks : Task.t list;
  variables : VarDecl.t list; (** Global variables *)
  programs : ProgramConfig.t list; (** Configuration of program instances. *)
}

(** Configuration element.
    See: 2.7 Configuration elements *)
type configuration_decl = {
  name : string;
  resources : resource_decl list;
  variables : VarDecl.t list; (** Global variables and access lists *)
  access_paths : string list;
}

(* }}} *)

type iec_library_element =
  | IECFunction of function_decl
  | IECFunctionBlock of fb_decl
  | IECProgram of program_decl
  | IECConfiguration of configuration_decl

val get_pou_vars_decl : iec_library_element -> VarDecl.t list
(** Return variables declared for given POU *)

(* vim: set foldmethod=marker foldlevel=0 foldenable : *)
