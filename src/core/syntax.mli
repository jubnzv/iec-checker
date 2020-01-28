module TI = Tok_info

(***************
 * Common elements
 ***************)
(* See Table 55 - Operators f the ST language *)
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

(***************
 * Types
 ***************)
type iec_data_type =
  | TyElementary of elementary_ty
  | TyGeneric of generic_ty
  | TyDerived of derived_ty

and elementary_ty =
  | NIL (* TODO: replace with an empty symbol *)
  | STRING
  | WSTRING
  | TIME
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
  | TIME_OF_DAY
  | TOD
  | DATE_AND_TIME
  | DT
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

(* Declaration of derived ty *)
and ty_decl = DTyDecl of string (* name *) * derived_ty

(* Specification of derived ty *)
and derived_ty =
  | DTySingleElementTy of single_element_ty_spec
  (* | ArrayType of string * iec_array_type_spec *)
  (* | StructureType of string * iec_structure_type_spec *)
  (* | StringType of string * iec_string_type_spec *)
  | DTyStringTy of string * int

(* Arrays*)
and iec_array_size = Capacity of int | Range of int * int

and iec_array_type_spec = {
  (* TODO: array_type: iec_data_type; *) size : iec_array_size;
}

(* Structures*)
and iec_structure_type_element = {
  name : string; (* TODO: element_type : iec_data_type; *)
}

and iec_structure_type_spec = { elements : iec_structure_type_element list }

(* String literals *)
and iec_string_type_spec = { capacity : int }

(* Single element types is user defined tys which works like typedefs in C. *)
and single_element_ty_spec =
  | SETyElementaryTy of elementary_ty
  | SETySETy of string (* derived ty name *)

(***************
 * Data representation
 ***************)

(* Constants *)
(** See: 2.2 Exernal representation of data *)
type constant =
  | CInteger of int * TI.t
  | CBool of bool * TI.t
  | CFloat of float * TI.t
  | CString of string * TI.t

val c_is_zero : constant -> bool
(** Return true if constant value is zero, false otherwise *)

val c_get_str_value : constant -> string
(** Return string representation for value of a given constant *)

val c_get_ti : constant -> TI.t
(** Return token info of a given constant *)

(* Variable identifier *)
module Variable : sig
  type t

  (* Variable could be connected to input/output data flow of POU. *)
  type direction = Input | Output

  val create : string -> TI.t -> t

  val get_name : t -> string

  val get_ti : t -> TI.t

  val get_direction : t -> direction option

  val set_direction : t -> direction -> t
end

(* Qualifier of IEC variable *)
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

(* Declaration of IEC variable *)
module VariableDecl : sig
  type t

  val create : Variable.t -> var_spec -> t

  val get_var : t -> Variable.t

  val set_qualifier : t -> var_qualifier -> t

  (* Set qualifier for variable. Do nothing if variable can't accept qualifier. *)
end

(* Arbitrary expressions of ST language *)
type expr =
  | Nil of TI.t
  | Variable of Variable.t
  | Constant of constant
  | BinExpr of expr * operator * expr
  | UnExpr of operator * expr

(* Function identifier *)
module Function : sig
  type t

  val create : string -> TI.t -> t

  val get_name : t -> string

  val get_ti : t -> TI.t

  val is_std : t -> bool
  (** Returns true if function declared in standard library. *)
end

(** Function declaration *)
type function_decl = {
  id : Function.t;
  return_ty : iec_data_type;
  variables : VariableDecl.t list;
  statements : expr list;
}

(* Function blocj identifier *)
module FunctionBlock : sig
  type t
  val create : string -> TI.t -> t
  val get_name : t -> string
  val get_ti : t -> TI.t
  val is_std : t -> bool
end

(** Function block declaration *)
type fb_decl = {
  id : FunctionBlock.t;
  variables : VariableDecl.t list;
  statements : expr list;
  (* return_ty : iec_data_type; *)
}

type program_decl = {
  is_retain : bool;
  name : string;
  variables : VariableDecl.t list;
  (* Variables declared in this program *)
  statements : expr list;
}

(* See: 2.7.2 Tasks *)
module Task : sig
  type t

  val create : string -> TI.t -> t
end

module ProgramConfig : sig
  type t

  val create : string -> TI.t -> t

  val set_qualifier : t -> var_qualifier -> t
  (** Set program qualifier. *)

  val set_task : t -> Task.t -> t
  (** Set task configuration. *)

  val set_conn_vars : t -> Variable.t list -> t
  (** Set connected variables. *)
end

type resource_decl = {
  name : string option;
  (* Resource name is skipped in case of single resource *)
  tasks : Task.t list;
  variables : VariableDecl.t list;
  (* Global variables *)
  programs : ProgramConfig.t list; (* Configuration of program instances. *)
}

(* See: 2.7 Configuration elements *)
type configuration_decl = {
  name : string;
  resources : resource_decl list;
  variables : VariableDecl.t list;
  (* Global variables and access lists *)
  access_paths : string list;
}

type iec_library_element =
  | IECFunction of function_decl
  | IECFunctionBlock of fb_decl
  | IECProgram of program_decl
  | IECConfiguration of configuration_decl
