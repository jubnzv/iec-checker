%{
  open Core_kernel
  open IECCheckerCore

  module S = Syntax
  module TI = Tok_info

  exception InternalError of string
  exception SemanticError of string

  (* Return known derived type from current context or raise exception. *)
  (* let get_derived_type (name: string) : S.iec_derived_data_type =
    if Context.is_derived_type_access name then
        Context.get_derived_type name
    else
        raise (InternalError "Unknown datatype\n") *)
%}

%token<IECCheckerCore.Tok_info.t> T_NIL
%token T_ASSIGN    ":="
%token T_SENDTO    "=>"
%token T_DOT       "."
%token T_GT        ">"
%token T_LT        "<"
%token T_GE        ">="
%token T_LE        "<="
%token T_MUL       "*"
%token T_MOD       "MOD"
%token T_DIV       "/"
%token T_POW       "**"
%token T_EQ        "="
%token T_NEQ       "<>"
%token T_NOT       "NOT"
%token T_RANGE     ".."
%token T_LBRACE    "{"
%token T_RBRACE    "}"
%token T_LPAREN    "("
%token T_RPAREN    ")"
%token T_LBRACK    "["
%token T_RBRACK    "]"
%token T_SHARP     "#"
%token T_PERCENT   "%"
%token T_COLON     ":"
%token T_PLUS      "+"
%token T_MINUS     "-"
%token T_SEMICOLON ";"
%token T_COMMA     ","
%token T_AT        "AT"
%token T_I         "I"
%token T_Q         "Q"
%token T_M         "M"
%token T_X         "X"
%token T_B         "B"
%token T_W         "W"
%token T_D         "D"
%token T_L         "L"
%token T_WITH
%token T_RETAIN
%token T_NON_RETAIN
%token T_PROGRAM
%token T_END_PROGRAM
%token T_FUNCTION
%token T_END_FUNCTION
%token T_FUNCTION_BLOCK
%token T_END_FUNCTION_BLOCK
%token T_CONFIGURATION
%token T_END_CONFIGURATION
%token T_RESOURCE
%token T_END_RESOURCE
%token T_ON
%token T_SINGLE
%token T_INTERVAL
%token T_PRIORITY
%token T_READ_WRITE
%token T_READ_ONLY
%token T_TASK
%token T_CONSTANT
%token T_VAR
%token T_VAR_INPUT
%token T_VAR_OUTPUT
%token T_VAR_IN_OUT
%token T_VAR_TEMP
%token T_VAR_EXTERNAL
%token T_VAR_ACCESS
%token T_VAR_CONFIG
%token T_VAR_GLOBAL
%token T_END_VAR
%token T_TYPE
%token T_END_TYPE
%token T_EOF

(* B.1.3.1 - Elementary data types *)
%token T_BYTE          "BYTE"
%token T_WORD          "WORD"
%token T_DWORD         "DWORD"
%token T_LWORD         "LWORD"
%token T_LREAL         "LREAL"
%token T_REAL          "REAL"
%token T_SINT          "SINT"
%token T_INT           "INT"
%token T_DINT          "DINT"
%token T_LINT          "LINT"
%token T_USINT         "USINT"
%token T_UINT          "UINT"
%token T_UDINT         "UDINT"
%token T_ULINT         "ULINT"
%token T_WSTRING       "WSTRING"
%token T_STRING        "STRING"
%token T_BOOL          "BOOL"
%token T_TIME          "TIME"
%token T_DATE          "DATE"
%token T_DATE_AND_TIME "DATE_AND_TIME"
%token T_DT            "DT"
%token T_TIME_OF_DAY   "TIME_OF_DAY"
%token T_TOD           "TOD"

(* B.1.3.2 - Generic data types *)
%token T_ANY            "ANY"
%token T_ANY_DERIVED    "ANY_DERIVED"
%token T_ANY_ELEMENTARY "ANY_ELEMENTARY"
%token T_ANY_MAGNITUDE  "ANY_MAGNITUDE"
%token T_ANY_NUM        "ANY_NUM"
%token T_ANY_REAL       "ANY_REAL"
%token T_ANY_INT        "ANY_INT"
%token T_ANY_BIT        "ANY_BIT"
%token T_ANY_STRING     "ANY_STRING"
%token T_ANY_DATE       "ANY_DATE"

(* B.2.2 Operators *)
%token T_OR             "OR"
%token T_XOR            "XOR"
%token T_AND            "AND"
%token T_EQU            "EQU"

(* Parametrized tokens *)
%token <string * IECCheckerCore.Tok_info.t> T_IDENTIFIER
%token <int * IECCheckerCore.Tok_info.t> T_INTEGER

(* Parser entry point. *)
%start <IECCheckerCore.Syntax.iec_library_element list> main

%%
(* Parser entry point *)
main:
    | T_EOF
    { [] }
    | dl = library_element_declaration_list; T_EOF
    { List.rev dl }

(* Programming model *)
library_element_declaration:
    | p = prog_decl
    { S.IECProgram(p) }
    | f = func_decl
    { S.IECFunction(f) }
    | fb = fb_decl
    { S.IECFunctionBlock(fb) }
    | c = config_decl
    { S.IECConfiguration(c) }

(* Helper symbol for library_element_declaration *)
library_element_declaration_list:
    | e = library_element_declaration
    { e :: [] }
    | el = library_element_declaration_list; e = library_element_declaration
    { e :: el }

(* {{{ Table 4 -- Pragmas *)
(* pragma: *)
(* }}} *)

(* {{{ Table 5 -- Numeric literals *)
(* In Annex B any external representation of data are designated as "constant".
 * So literally constant is just a literal value. *)
constant:
    | res = numeric_literal
    {
        S.Constant(res)
    }
    (* | res = character_string *)
    (* | { res } *)
    (* | res = time_literal *)
    (* | { res } *)
    (* | res = bit_string_literal *)
    (* | { res } *)
    (* | res = boolean_literal *)
    (* | { res } *)

numeric_literal:
    | res = int_literal
    { res }
    (* | res = real_integer *)
    (* { res } *)

int_literal:
    (* | tn = int_type_name SHARP v = int_literal_value *)
    (* { tn, v } *)
    | res = int_literal_value
    { res }

(* Helper symbol for int_literal *)
int_literal_value:
    | res = signed_int
    { res }
    (* | v = binary_integer *)
    (* { v } *)
    (* | v = octal_integer *)
    (* { v } *)
    (* | v = hex_integer *)
    (* { v } *)

signed_int:
    | i = integer
    { i }
    | T_PLUS i = integer
    { i }
    | T_MINUS res = T_INTEGER
    {
        let (v, ti) = res in
        S.CInteger(-v, ti)
    }

integer:
    | res = T_INTEGER
    {
        let (v, ti) = res in
        S.CInteger(v, ti)
    }

(* }}} *)

(* {{{ Table 6 -- String literals / Table 7 -- Two-character combinations in strings *)
(* char_literal: *)

(* char_str: *)

(* s_byte_char_str: *)

(* d_byte_char_str: *)

(* s_byte_char_value: *)

(* d_byte_char_value: *)

(* common_char_value: *)
(* }}} *)

(* {{{ Table 8 -- Diration literals / Table 9 -- Datetime literals *)
(* time_literal: *)

(* duration: *)

(* fix_point: *)

(* interval: *)

(* day: *)

(* hours: *)

(* minutes: *)

(* seconds: *)

(* miliseconds: *)

(* microseconds: *)

(* nanoseconds: *)

(* time_of_day: *)

(* daytime: *)

(* day_hour: *)

(* day_minute: *)

(* day_second: *)

(* date: *)

(* date_literal: *)

(* year: *)

(* month: *)

(* day: *)

(* date_and_time: *)
(* }}} *)

(* {{{ Table 10 -- Elementary data types *)
(* Note: *_name rules will return an actual Syntax AST type representation.
  This simplifies a processing of derived types specifications. *)

data_type_access:
  | ty = elem_type_name
  { S.TyElementary(ty) }
  | ty = derived_type_access
  { S.TyDerived(ty) }

elem_type_name:
  | T_STRING
  { S.STRING }
  | T_WSTRING
  { S.WSTRING }
  | T_TIME
  { S.TIME }
  | t = numeric_type_name
  { t }
  | t = date_type_name
  { t }
  | t = bit_str_type_name
    { t }

numeric_type_name:
  | t = int_type_name
  { t }
  | t = real_type_name
  { t }

int_type_name:
  | t = sign_int_type_name
  { t }
  | t = unsign_int_type_name
  { t }

sign_int_type_name:
  | T_SINT
  { S.SINT }
  | T_INT
  { S.INT }
  | T_DINT
  { S.DINT }
  | T_LINT
    { S.LINT }

unsign_int_type_name:
  | T_USINT
  { S.USINT }
  | T_UINT
  { S.UINT }
  | T_UDINT
  { S.UDINT }
  | T_ULINT
  { S.ULINT }

real_type_name:
  | T_REAL
  { S.REAL }
  | T_LREAL
  { S.LREAL }

string_type_name:
  | id = T_IDENTIFIER
  {
    let name, _ = id in
    name
  }

(* time_type_name: *)

date_type_name:
  | T_DATE
  { S.DATE }
  | T_TIME_OF_DAY
  { S.TIME_OF_DAY }
  | T_TOD
  { S.TOD }
  | T_DATE_AND_TIME
  { S.DATE_AND_TIME }
  | T_DT
    { S.DT }

(* tod_type_name: *)

(* dt_type_name: *)

bit_str_type_name:
  | ty = bool_type_name
  { ty }
  | T_BYTE
  { S.BYTE }
  | T_WORD
  { S.WORD }
  | T_DWORD
  { S.DWORD }
  | T_LWORD
    { S.LWORD }

bool_type_name:
  | T_BOOL
  { S.BOOL }

(* multibits_type_name: *)
(* }}} *)

(* {{{ Table 11 -- Derived data types *)
(* Return S.derived_ty *)
derived_type_access:
  | spec = single_element_type_access
  { S.DTySingleElementTy(spec) }
  (* | n = array_type_access
  {  }
  | n = struct_type_access
  {  }
  | n = string_type_name
  {  } *)

(* string_type_access: *)

(* Return S.single_element_ty_spec *)
single_element_type_access:
  | name = simple_type_access
  { S.SETySETy(name) }
  (* | n = subrange_type_access
  { n } *)
  (* | n = enum_type_access
    { n } *)

(* Return a name of derived ty *)
simple_type_access:
  | tn = simple_type_name
  { tn }

(* subrange_type_access: *)
  (* | id = T_IDENTIFIER *)
  (* { } *)

(* enum_type_access: *)
  (* | id = T_IDENTIFIER *)
  (* { } *)

(* array_type_access: *)
  (* | id = T_IDENTIFIER *)
  (* { } *)

(* struct_type_access:
  | id = T_IDENTIFIER
  { } *)

simple_type_name:
  | id = T_IDENTIFIER
  {
    let name, _ = id in
    name
  }

subrange_type_name:
  | id = T_IDENTIFIER
  {
    let name, _ = id in
    name
  }

enum_type_name:
  | id = T_IDENTIFIER
  {
    let name, _ = id in
    name
  }

array_type_name:
  | id = T_IDENTIFIER
  {
    let name, _ = id in
    name
  }

struct_type_name:
  | id = T_IDENTIFIER
  {
    let name, _ = id in
    name
  }

(* data_type_decl:
  | T_TYPE dtl = data_type_decl_list T_END_TYPE
  { dtl } *)

(* Helper for data_type_decl *)
(* data_type_decl_list:
  | t = type_decl T_SEMICOLON
  { t :: [] }
  | tl = data_type_decl_list t = type_decl T_SEMICOLON
  { t :: tl } *)

(* Return derived_ty_decl *)
type_decl:
  | td = simple_type_decl
  { td }
  (* | td = subrange_type_decl *)
  (* { td } *)
  (* | td = enum_type_decl *)
  (* { td } *)
  (* | td = array_type_decl *)
  (* { td } *)
  (* | td = struct_type_decl *)
  (* { td } *)
  (* | td = str_type_decl *)
  (* { td } *)
  (* | td = ref_type_decl *)
  (* { td } *)

simple_type_decl:
  | name = simple_type_name T_COLON spec = simple_spec_init
  { S.DTySingleElementTy(name, spec) }

(* Return S.single_element_ty_spec *)
simple_spec_init:
  | ty = simple_spec
  { ty }
  (* Should add initial value in HT *)
  (* | s = simple_spec ASSIGN c = constant *)
  (* { } *)

(* Return S.single_element_ty_spec  *)
simple_spec:
  | ty = elem_type_name
  { S.SETyElementaryTy(ty)  }
  | name = simple_type_access
  { S.SETySETy(name) }

(* subrange_type_decl:
  | n = subrange_type_access COLON s = subrange_spec_init
  { } *)

(* subrange_spec_init:
  | s = subrange_spec
  { s } *)
  (* | s = subrange_spec ASSIGN i = signed_int *)
  (* { s , i } *)

(* subrange_spec:
  | tn = int_type_name LBRACE s = subrange RBRACE
  { }
  | tn = subrange_type_access
    { tn } *)

(* subrange:
  | min = signed_int RANGE max = signed_int
  {
    let (vmin, _) = min in
    let (vmax, _) = max in
    (vmin, vmax)
  } *)

(* enum_type_decl: *)

(* name_spec_init: *)

(* enum_value_spec: *)

(* enum_value: *)

(* array_type_decl: *)

(* array_spec_init: *)

(* array_spec: *)

(* array_init: *)

(* array_elem_init: *)

(* array_elem_init_value: *)

(* struct_type_decl: *)

(* struct_spec: *)

(* struct_spec_init: *)

(* struct_decl: *)

(* struct_elem_decl: *)

struct_elem_name:
  | id = T_IDENTIFIER
  {
    let name, _ = id in
    name
  }

(* Helper symbol for struct_elem_name *)
struct_elem_name_list:
  | n = struct_elem_name;
  { n :: [] }
  | ns = struct_elem_name_list; T_DOT n = struct_elem_name;
  { n :: ns }

(* struct_init: *)

(* struct_elem_init: *)

(* str_type_decl: *)

(* }}} *)

(* {{{ Table 16 -- Direct variables *)
direct_variable:
  | T_PERCENT loc = location_prefix; sz = size_prefix; pcs = integers;
  {
    let get_int_val = function
      | S.CInteger (v, _) -> v
      | _ -> -1 (* FIXME: How should I handle this? *)
    in
    let pvals = List.map ~f:(fun c -> get_int_val c) pcs in
    S.VarSpecDirect(loc, Some sz, pvals, None)
  }

(* Helper symbol for direct_variable.
 * Return int list. *)
integers:
  | ci = integer;
  { ci :: [] }
  | cil = integers; ci = integer;
  { ci :: cil }
(* }}} *)

(* {{{ Table 12 -- Operations with references *)
(* ref_type_decl: *)

(* ref_spec_init: *)

(* ref_spec: *)

(* ref_type_name: *)

(* ref_type_access: *)

(* ref_name: *)

(* ref_value: *)

(* ref_addr: *)

(* ref_name: *)

(* ref_deref: *)
(* }}} *)

(* {{{ Table 13 -- Variables declaration / Table 14 -- Variables initialization *)
variable:
  (* | v = direct_variable *)
  (* | {} *)
  | v = symbolic_variable
  { v }

symbolic_variable:
  | v = variable_name
  { v }
  (* | multi_elem_var *)

(* var_access: *)

variable_name:
  | id = T_IDENTIFIER
  {
    let (name, ti) = id in
    S.Variable.create name ti
  }

(* multi_elem_var:
  | array_vairable
  {  }
  | struct_variable
  {  } *)

(* subscript_list: *)
  (* LBRACK subscript (* { COMMA subrscipt }*) RBRACK *)

(* index: *)

(* struct_variable: *)

(* struct_elem_select: *)

input_decls:
  | T_VAR_INPUT  vds = input_decl T_END_VAR
  { List.rev vds }
  | T_VAR_INPUT T_RETAIN vds = input_decl T_END_VAR
  {
    let vdsr = List.rev vds in
    List.map ~f:(fun v -> (S.VariableDecl.set_qualifier v S.VarQRetain)) vdsr
  }
  | T_VAR_INPUT T_NON_RETAIN vds = input_decl T_END_VAR
  {
    let vdsr = List.rev vds in
    List.map ~f:(fun v -> (S.VariableDecl.set_qualifier v S.VarQNonRetain)) vdsr
  }

input_decl:
  | vl = var_decl_init_list
  {
    let vlr = List.rev vl in
    List.map ~f:(fun v -> (
      let s = S.VarSpecIn(None) in
      S.VariableDecl.create v s
    )) vlr
  }

(* edge_decl: *)

var_decl_init:
  | vs = variable_list; T_COLON; simple_spec_init;
  { vs }

(* Helper symbol for var_decl_init *)
var_decl_init_list:
  | v = var_decl_init T_SEMICOLON
  { v }
  | vs = var_decl_init_list; v = var_decl_init T_SEMICOLON
  { List.append vs v }

(* ref_var_decl: *)

(* interface_var_decl: *)

variable_list:
  | v = variable_name
  { v :: [] }
  | vs = variable_list T_COMMA v = variable_name
  { v :: vs }

(* array_var_decl_init: *)

(* array_conformand: *)

(* array_conform_decl: *)

(* struct_var_decl_init: *)

(* fb_decl_no_init: *)

(* fb_decl_init: *)

fb_name:
  | id = T_IDENTIFIER
  {
    let (name, ti) = id in
    S.FunctionBlock.create name ti
  }

(* fb_instance_name: *)

output_decls:
  | T_VAR_OUTPUT vl = var_decl_init_list T_END_VAR
  {
    let vlr = List.rev vl in
    List.map ~f:(fun v -> (
      let s = S.VarSpecOut(None) in
      S.VariableDecl.create v s
    )) vlr
  }
  | T_VAR_OUTPUT T_RETAIN vl = var_decl_init_list T_END_VAR
  {
    let vlr = List.rev vl in
    List.map ~f:(fun v -> (
      let s = S.VarSpecOut(Some S.VarQRetain) in
      S.VariableDecl.create v s
    )) vlr
  }
  | T_VAR_OUTPUT T_NON_RETAIN vl = var_decl_init_list T_END_VAR
  {
    let vlr = List.rev vl in
    List.map ~f:(fun v -> (
      let s = S.VarSpecOut(Some S.VarQNonRetain) in
      S.VariableDecl.create v s
    )) vlr
  }

(* output_decl: *)

in_out_decls:
  | T_VAR_IN_OUT  vl = var_decl_list T_END_VAR
  {
    let vlr = List.rev vl in
    List.map ~f:(fun v -> S.VariableDecl.create v S.VarSpecInOut) vlr
  }

(* in_out_var_decl: *)

var_decl:
  | vs = temp_var_decl
  { vs }
  (* | f = fb_name_decl
  { f } *)

(* Helper symbol for var_decl *)
var_decl_list:
  | v = var_decl T_SEMICOLON
  { v }
  | vs = var_decl_list; v = var_decl  T_SEMICOLON
  { List.append vs v }

(* array_var_decl: *)

(* struct_var_decl: *)

var_decls:
  | T_VAR vds = var_decl_init_list T_END_VAR
  {
    let vdsr = List.rev vds in
    List.map ~f:(fun v -> (
      let s = S.VarSpec(None) in
      S.VariableDecl.create v s
    )) vdsr
  }
  | T_VAR T_RETAIN vds = var_decl_init_list T_END_VAR
  {
    let vdsr = List.rev vds in
    List.map ~f:(fun v -> (
      let s = S.VarSpec(Some S.VarQRetain) in
        S.VariableDecl.create v s
    )) vdsr
  }

retain_var_decls:
  | T_VAR T_RETAIN vds = var_decl_init_list T_END_VAR
  {
    let vdsr = List.rev vds in
    List.map ~f:(fun v -> (
      let s = S.VarSpec(Some S.VarQRetain) in
        S.VariableDecl.create v s
    )) vdsr
  }

(* loc_var_decls: *)

(* loc_var_decl: *)

temp_var_decls:
  | T_VAR_TEMP vl = temp_decl_list T_END_VAR
  {
    let vlr = List.rev vl in
    List.map ~f:(fun v -> S.VariableDecl.create v S.VarSpecTemp) vlr
  }

(* Helper symbol for temp_var_decls *)
temp_decl_list:
  | v = temp_var_decl T_SEMICOLON
  { v }
  | vs = temp_decl_list; v = temp_var_decl T_SEMICOLON
  { List.append vs v }

(* Helper symbol for temp_var_decls_list.
   Return S.Variable list *)
temp_var_decl:
  | vs = var1_init_decl
  { vs }

external_var_decls:
  | T_VAR_EXTERNAL vl = external_decl_list T_END_VAR
  {
    let vlr = List.rev vl in
    List.map ~f:(fun v -> (
      let s = S.VarSpecGlobal(None) in
      S.VariableDecl.create v s
    )) vlr
  }
  | T_VAR_EXTERNAL T_RETAIN vl = external_decl_list T_END_VAR
  {
    let vlr = List.rev vl in
    List.map ~f:(fun v -> (
      let s = S.VarSpecGlobal(Some S.VarQRetain) in
      S.VariableDecl.create v s
    )) vlr
  }

(* Return S.Variable list *)
external_decl:
  | v = variable_name T_COLON  simple_spec
  { v }

(* Helper symbol for external_decl. *)
external_decl_list:
  | v = external_decl T_SEMICOLON
  { v :: [] }
  | vs = external_decl_list; v = external_decl T_SEMICOLON
  { v :: vs }

global_var_name:
  | id = T_IDENTIFIER
  {
    let (name, ti) = id in
    S.Variable.create name ti
  }

(* Helper symbol for global_var_name *)
global_var_list:
  | v = global_var_name;
  {
    let s = S.VarSpecGlobal(None) in
    let vd = S.VariableDecl.create v s in
    vd :: []
  }
  | vds = global_var_list; T_COMMA v = global_var_name
  {
    let s = S.VarSpecGlobal(None) in
    let vd = S.VariableDecl.create v s in
    vd :: vds
  }

global_var_decls:
  | T_VAR_GLOBAL vds = global_var_decl_list; T_END_VAR
  { List.rev vds }
  | T_VAR_GLOBAL T_RETAIN vds = global_var_decl_list; T_END_VAR
  {
    let vdsr = List.rev vds in
    List.map ~f:(fun v -> (S.VariableDecl.set_qualifier v S.VarQNonRetain)) vdsr
  }
  | T_VAR_GLOBAL T_CONSTANT vds = global_var_decl_list; T_END_VAR
  {
    let vdsr = List.rev vds in
    List.map ~f:(fun v -> (S.VariableDecl.set_qualifier v S.VarQConstant)) vdsr
  }

(* Return S.VariableDecl.t list *)
global_var_decl:
  | ss = global_var_spec; T_COLON loc_var_spec_init;
  { ss }

(* Helper symbol for gloval_var_decl *)
global_var_decl_list:
  | v = global_var_decl T_SEMICOLON
  { v }
  | vs = global_var_decl_list; v = global_var_decl T_SEMICOLON
  { List.append vs v }

(* Return S.VariableDecl.t list *)
global_var_spec:
  | vs = global_var_list
  { vs }
  | v = global_var_name; l = located_at
  {
    let s = S.VarSpecGlobal(None) in
    let vd = S.VariableDecl.create v s in
    [vd]
  }

loc_var_spec_init:
  | s = simple_spec_init
  { s }

located_at:
  | T_AT v = direct_variable
  { v }

(* str_var_decl: *)

(* s_byte_str_val_decl: *)

(* s_byte_str_spec: *)

(* d_byte_str_val_decl: *)

(* d_byte_str_spec: *)

loc_partly_var_decl:
    | T_VAR vl = incompl_located_var_list T_END_VAR
    { List.rev vl }
    | T_VAR T_RETAIN vl = incompl_located_var_list T_END_VAR
    { List.rev vl } (* TODO: add qualifier *)
    | T_VAR T_NON_RETAIN vl = incompl_located_var_list T_END_VAR
    { List.rev vl } (* TODO: add qualifier *)

loc_partly_var:
    | v = variable_name; l = incompl_location; T_COLON s = var_spec
    {
        let s = S.VarSpecDirect(l, None, [], None) in
        S.VariableDecl.create v s
    }

(* Helper symbol for loc_partly_var *)
incompl_located_var_list:
    | v = loc_partly_var T_SEMICOLON
    { v :: [] }
    | vs = incompl_located_var_list; v = loc_partly_var T_SEMICOLON
    { v :: vs }

(* Helper symbol for loc_partly_var *)
incompl_location:
    | T_AT T_PERCENT T_I T_MUL
    { S.DirVarLocI }
    | T_AT T_PERCENT T_Q T_MUL
    { S.DirVarLocQ }
    | T_AT T_PERCENT T_M T_MUL
    { S.DirVarLocM }

var_spec:
    | ty = simple_spec
    { ty }

(* }}} *)

(* B.1.5 Program organization units *)
(* B.1.5.1 Functions *)
var2_init_decl:
  | vs = var1_init_decl
  {
    let vss = List.rev vs in
    List.map ~f:(fun v -> (
      let s = S.VarSpec(None) in
      S.VariableDecl.create v s
    )) vss
  }

(* {{{ Table 19 -- Function declaration *)
func_name:
  | id = T_IDENTIFIER
  {
    let (name, ti) = id in
    S.Function.create name ti
  }

(* func_access: *)

(* std_func_name:
  | id = T_IDENTIFIER
  {
    let (name, ti) = id in
    S.Function.create name ti
  } *)

(* derived_func_name:
  | id = T_IDENTIFIER
  {
    let (name, ti) = id in
    S.Function.create name ti
  } *)

func_decl:
  | T_FUNCTION id = func_name; T_COLON ret_ty = function_ty; vs = function_vars; body = func_body; T_END_FUNCTION
  {
    {
      S.id = id;
      S.return_ty = ret_ty;
      S.variables = vs;
      S.statements = body;
    }
  }
  | T_FUNCTION id = func_name; T_COLON ret_ty = function_ty; body = func_body; T_END_FUNCTION
  {
    {
      S.id = id;
      S.return_ty = ret_ty;
      S.variables = [];
      S.statements = body;
    }
  }

(* Helper symbol for func_decl *)
function_ty:
  | ty = elem_type_name
  { S.TyElementary(ty) }
  | ty = derived_type_access
  { S.TyDerived(ty) }

(* Helper symbol for func_decl *)
function_vars:
  | vs = io_var_decls
  { vs }
  | vs = func_var_decls
  { vs }

io_var_decls:
  | vs = input_decls
  { vs }
  | vs = output_decls
  { vs }
  | vs = in_out_decls
  { vs }

func_var_decls:
  | vds = external_var_decls
  { vds }
  | vds = var_decls
  { vds }

func_body:
  | sl = stmt_list
  { sl }
(* }}} *)

(* {{{ Table 40 -- Function block definition / Table 41 -- Function block instantiation *)
(* fb_type_name:
  | f = fb_name
  { f } *)

(* fb_type_access: *)

(* std_fb_name: *)

derived_fb_name:
  | id = T_IDENTIFIER
  {
    let (name, ti) = id in
    S.FunctionBlock.create name ti
  }

fb_decl:
  | T_FUNCTION_BLOCK id = derived_fb_name; T_END_FUNCTION_BLOCK
  { { S.id = id; S.variables = []; S.statements = [] } }
  | T_FUNCTION_BLOCK id = derived_fb_name; vds = fb_var_decls_list; T_END_FUNCTION_BLOCK
  { { S.id = id; S.variables = vds; S.statements = [] } }
  | T_FUNCTION_BLOCK id = derived_fb_name; ss = fb_body; T_END_FUNCTION_BLOCK
  { { S.id = id; S.variables = []; S.statements = ss } }
  | T_FUNCTION_BLOCK id = derived_fb_name; vds = fb_var_decls_list; ss = fb_body; T_END_FUNCTION_BLOCK
  { { S.id = id; S.variables = vds; S.statements = ss } }

(* Helper symbol for fb_decl *)
fb_var_decls_list:
  | vds = fb_io_var_decls
  { vds }
  | vds = func_var_decls;
  { vds }
  | vds = temp_var_decls;
  { vds }
  | vds = other_var_decls;
  { vds }
  | vdss = fb_var_decls_list; vds = fb_io_var_decls
  { List.append vdss vds }
  | vdss = fb_var_decls_list; vds = func_var_decls
  { List.append vdss vds }
  | vdss = fb_var_decls_list; vds = temp_var_decls
  { List.append vdss vds }
  | vdss = fb_var_decls_list; vds = other_var_decls
  { List.append vdss vds }

(* Reuse input_decls and output_decls defined in Table 13.
   These grammar rules are actually the same as fb_input_decls
   and fb_output_decls. *)
fb_io_var_decls:
  | vds = input_decls
  { vds }
  | vds = output_decls
  { vds }
  | vds = in_out_decls
  { vds }

(* fb_input_decls: *)
(* fb_input_decl: *)
(* fb_output_decls: *)
(* fb_output_decl: *)

other_var_decls:
    | vds = retain_var_decls
    { vds }
    | vds = no_retain_var_decls
    { vds }
    | vds = loc_partly_var_decl
    { vds }

no_retain_var_decls:
  | T_VAR T_NON_RETAIN vds = var_decl_init_list T_END_VAR
  {
    let vdsr = List.rev vds in
    List.map ~f:(fun v -> (
      let s = S.VarSpec(Some S.VarQNonRetain) in
        S.VariableDecl.create v s
    )) vdsr
  }

fb_body:
    | sl = stmt_list
    { sl }

(* method_decl: *)

(* method_name: *)
(* }}} *)

(* {{{ Table 48 -- Class / Table 50 -- Class method calls *)
(* class_decl: *)

(* class_type_name: *)

(* class_type_access: *)

(* class_name: *)

(* class_instance_name: *)

(* interface_decl: *)

(* method_prototype: *)

(* interface_spec_init: *)

(* interface_value: *)

(* interface_name_list: *)

(* interface_type_name: *)

(* interface_type_access: *)

(* interface_name: *)

(* access_spec: *)
(* }}} *)

(* {{{ Table 47 -- Program definition *)
prog_decl:
  | T_PROGRAM; n = prog_type_name; vdl = program_var_decls_list; ss = fb_body; T_END_PROGRAM;
  { {S.is_retain = false; S.name = n; S.variables = vdl; S.statements = ss} }
  (* TODO: Generate an error *)
  (* | PROGRAM n = prog_type_name END_PROGRAM; *)
  (* { {S.is_retain = false; S.name = n; S.variables = []} } *)

(* Helper for prog_decl *)
program_var_decls_list:
  | vds = io_var_decls
  { vds }
  | vds = func_var_decls
  { vds }
  | vds = temp_var_decls
  { vds }
  | vds = other_var_decls
  { vds }
  (* | vds = loc_var_decls
  { vds } *)
  | vds = prog_access_decls
  { vds }
  | vdss = program_var_decls_list vds = io_var_decls
  { List.append vdss vds }
  | vdss = program_var_decls_list vds = func_var_decls
  { List.append vdss vds }
  | vdss = program_var_decls_list vds = temp_var_decls
  { List.append vdss vds }
  | vdss = program_var_decls_list vds = other_var_decls
  { List.append vdss vds }
  (* | vars = located_vars_declaration *)
      (* {vars} *)
  | vdss = program_var_decls_list vds = prog_access_decls
  { List.append vdss vds }

prog_type_name:
  | id = T_IDENTIFIER
  {
    let name, _ = id in
    name
  }

prog_type_access:
  | n = prog_type_name
  { n }

prog_access_decls:
  | T_VAR_ACCESS vl = prog_access_decl_list T_END_VAR
  { List.rev vl }

(* Helper for prog_access_decls *)
prog_access_decl_list:
    | v = prog_access_decl T_SEMICOLON
    { v :: [] }
    | vs = prog_access_decl_list; v = prog_access_decl T_SEMICOLON
    { v :: vs }

prog_access_decl:
    | an = access_name T_COLON v = symbolic_variable T_COLON data_type_access
    {
        let s = S.VarSpecAccess(an) in
        S.VariableDecl.create v s
    }
(* }}} *)

(* {{{ Table 54-61 -- SFC *)
(* }}} *)

(* {{{ Table 62 -- Configuration elements *)
config_name:
    | id = T_IDENTIFIER
    {
        let name, _ = id in
        name
    }

resource_type_name:
    | id = T_IDENTIFIER
    {
        let name, _ = id in
        name
    }

config_decl:
    (* Without global variables *)
    | T_CONFIGURATION name = config_name; rd = resource_decls; T_END_CONFIGURATION
    { { S.name = name; S.resources = rd; S.variables = []; S.access_paths = [] } }
    (* With global variables *)
    | T_CONFIGURATION name = config_name; vds = global_var_decls; rd = resource_decls; T_END_CONFIGURATION
    { { S.name = name; S.resources = rd; S.variables = vds; S.access_paths = [] } }
    | T_CONFIGURATION name = config_name; vds = global_var_decls; rd = resource_decls; config_init; T_END_CONFIGURATION
    { { S.name = name; S.resources = rd; S.variables = vds; S.access_paths = [] } }
    | T_CONFIGURATION name = config_name; vds = global_var_decls; rd = resource_decls; access_decls; T_END_CONFIGURATION
    { { S.name = name; S.resources = rd; S.variables = vds; S.access_paths = [] } }
    | T_CONFIGURATION name = config_name; vds = global_var_decls; rd = resource_decls; access_decls; config_init; T_END_CONFIGURATION
    { { S.name = name; S.resources = rd; S.variables = vds; S.access_paths = [] } }

(* Helper symbol for config_decl *)
resource_decls:
    | rcs = single_resource_decl
    { [rcs] }
    | rcs = resource_decl_list
    { rcs }

(* Return S.resource_decl *)
resource_decl:
    | T_RESOURCE n = resource_name; T_ON resource_type_name; rc = single_resource_decl; T_END_RESOURCE
    { { S.name = Some n; S.tasks = rc.tasks; S.variables = []; S.programs = rc.programs } }
    | T_RESOURCE n = resource_name; T_ON resource_type_name; vs = global_var_decls; rc = single_resource_decl; T_END_RESOURCE
    { { S.name = Some n; S.tasks = rc.tasks; S.variables = vs; S.programs = rc.programs } }

(* Helper symbol for resource_decl *)
resource_decl_list:
    | rc = resource_decl
    { rc :: [] }
    | rcs = resource_decl_list; rc = resource_decl
    { rc :: rcs }

(* Return S.resource_decl *)
single_resource_decl:
    | pis = prog_config_list
    { { S.name = None; S.tasks = []; S.variables = []; S.programs = pis } }
    | ts = task_config_list; pis = prog_config_list
    { { S.name = None; S.tasks = ts; S.variables = []; S.programs = pis } }

resource_name:
    | id = T_IDENTIFIER
    {
        let name, _ = id in
        name
    }

(* Helper symbol for resource_name *)
resource_name_list:
    | n = resource_name;
    { n :: [] }
    | ns = resource_name_list; T_DOT n = resource_name;
    { n :: ns }

access_decls:
    | T_VAR_ACCESS ads = access_decl_list; T_END_VAR
    { ads }

(* Helper symbol for access_decl *)
access_decl_list:
    | v = access_decl; T_SEMICOLON
    { v :: [] }
    | vs = access_decl_list; v = access_decl; T_SEMICOLON
    { v :: vs }

access_decl:
    | access_name; T_COLON access_path; T_COLON data_type_access
    {  }
    | access_name; T_COLON access_path; T_COLON data_type_access; access_direction
    {  }

access_path:
    | resource_name; T_DOT; direct_variable
    {  }
    (* | dv = direct_variable
    {  } *)
    (* | resource_name; T_DOT; pn = program_name; T_DOT fn = fb_name; T_DOT; v = symbolic_variable
    {  } *)
    | resource_name; T_DOT; program_name; T_DOT; symbolic_variable
    {  }
    (* | pn = program_name; T_DOT fn = fb_name; T_DOT; v = symbolic_variable
    {  } *)
    | program_name; T_DOT; symbolic_variable
    {  }

global_var_access:
    | v = global_var_name;
    { v }
    | resource_name_list; v = global_var_name;
    { v }
    | v = global_var_name; struct_elem_name_list
    { v }
    | resource_name_list; v = global_var_name; struct_elem_name_list
    { v }

access_name:
    | id = T_IDENTIFIER
    {
        let name, _ = id in
        name
    }

(* prog_output_access: *)

program_name:
    | id = T_IDENTIFIER
    {
        let (name, ti) = id in
        S.ProgramConfig.create name ti
    }

(* Helper symbol for program_name *)
program_name_qual:
    | p = program_name;
    { p }
    | p = program_name; T_RETAIN
    { S.ProgramConfig.set_qualifier p S.VarQRetain }
    | p = program_name; T_NON_RETAIN
    { S.ProgramConfig.set_qualifier p S.VarQNonRetain }

access_direction:
    | T_READ_WRITE
    {  }
    | T_READ_ONLY
    {  }

task_config:
    | T_TASK t = task_name; task_init;
    { t }

(* Helper symbol for task_config *)
task_config_list:
    | t = task_config;
    { t :: [] }
    | ts = task_config_list; t = task_config;
    { t :: ts }

task_name:
    | id = T_IDENTIFIER
    {
        let (name, ti) = id in
        S.Task.create name ti
    }

task_init:
    | T_LBRACE T_SINGLE s = data_source; T_COMMA T_INTERVAL i = data_source; T_COMMA T_PRIORITY T_ASSIGN p = integer; T_RBRACE
    { (Some s, Some i, Some p) }
    | T_LBRACE T_SINGLE s = data_source; T_COMMA T_PRIORITY T_ASSIGN p = integer; T_RBRACE
    { (Some s, None, Some p) }
    | T_LBRACE T_INTERVAL i = data_source; T_COMMA T_PRIORITY T_ASSIGN p = integer; T_RBRACE
    { (None, Some i, Some p) }
    | T_LBRACE T_PRIORITY T_ASSIGN p = integer; T_RBRACE
    { (None, None, Some p) }

data_source:
    | v = constant
    { v }
    (* | v = global_var_access
    { v } *)
    (* | v = program_output_reference
    { v } *)
    (* | v = direct_variable
    { v } *)

prog_config:
    | T_PROGRAM pc = program_name_qual; T_COLON prog_type_access;
    { pc }
    | T_PROGRAM pc = program_name_qual; T_COLON prog_type_access; T_LBRACE; cvs = prog_conf_elems; T_RBRACE
    {
        let pc = S.ProgramConfig.set_conn_vars pc cvs in
        pc
    }
    | T_PROGRAM pc = program_name_qual; T_WITH; t = task_name; T_COLON ty = prog_type_name;
    {
        let pc = S.ProgramConfig.set_task pc t in
        pc
    }
    | T_PROGRAM pc = program_name_qual; T_WITH; t = task_name; T_COLON ty = prog_type_name; T_LBRACE; cvs = prog_conf_elems; T_RBRACE
    {
        let pc = S.ProgramConfig.set_conn_vars pc cvs in
        let pc = S.ProgramConfig.set_task pc t in
        pc
    }

(* Helper symbol for prog_config *)
prog_config_list:
    | pc = prog_config; T_SEMICOLON
    { pc :: [] }
    | pcs = prog_config_list; pc = prog_config T_SEMICOLON
    { pc :: pcs }

prog_conf_elems:
    | el = prog_conf_elem
    { el :: [] }
    | els = prog_conf_elems; T_COMMA el = prog_conf_elem
    { el :: els }

prog_conf_elem:
    (* | fb = fb_task
    { fb } *)
    | c = prog_cnxn
    { c }

(* fb_task:
    | fn = fb_name; T_WITH; tn = task_name
    {  } *)

(* This stmt assigns program inputs and outputs to IEC variable. *)
prog_cnxn:
    (* Input *)
    | v = symbolic_variable; T_ASSIGN; prog_data_source
    { v }
    (* Output *)
    (* | v = symbolic_variable; T_SENDTO; data_sink
    { v } *)

prog_data_source:
    | v = constant
    { v }
    (* | v = enumerated_value
    { v } *)
    (* | v = global_var_access
    { v } *)
    (* | v = direct_variable
    { v } *)

data_sink:
    | v = global_var_access
    { v }
    | v = direct_variable
    { v }

config_init:
    | T_VAR_CONFIG is = config_inst_init_list T_END_VAR
    { is }

config_inst_init:
    | resource_name; T_DOT; program_name; T_DOT;
    {  }
    (* | resource_name; T_DOT; program_name; T_DOT; fn = fb_name T_DOT;
    {  } *)

(* Helper symbol for config_inst_init *)
config_inst_init_list:
    | i = config_inst_init; T_SEMICOLON
    { i :: [] }
    | is = config_inst_init_list; i = config_inst_init; T_SEMICOLON
    { i :: is }

(* }}} *)

(* {{{ Table 64 -- Namespaces *)
(* namespace_decl: *)

(* namespace_elements: *)

(* namespace_h_name: *)

(* namespace_name: *)

(* using_directive: *)

(* pou_decl: *)
(* }}} *)

(* {{{ Table 67-70 -- IL *)
(* }}} *)

(* {{{ Table 71-72 -- ST *)
expression:
    | s = xor_expr
    { s }
    | e1 = xor_expr T_OR e2 = xor_expr
    { S.BinExpr(e1, S.OR, e2) }

(* constant_expr: *)

xor_expr:
    | s = and_expr
    { s }
    | e1 = and_expr T_XOR e2 = and_expr
    { S.BinExpr(e1, S.XOR, e2) }

and_expr:
    | s = compare_expr
    { s }
    | e1 = compare_expr T_AND e2 = compare_expr
    { S.BinExpr(e1, S.AND, e2) }

compare_expr:
    | s = equ_expr
    { s }
    | e1 = equ_expr T_EQ e2 = equ_expr
    { S.BinExpr(e1, S.EQ, e2) }
    | e1 = equ_expr T_NEQ e2= equ_expr
    { S.BinExpr(e1, S.NEQ, e2) }

equ_expr:
    | s = add_expr
    { s }
    | e1 = add_expr op = compare_expr_operator e2 = add_expr
    { S.BinExpr(e1, op, e2) }

add_expr:
    | t = term
    { t }
    | t1 = term op = add_operator t2 = term
    { S.BinExpr(t1, op, t2)}

term:
    | e = power_expr
    { e }
    | e1 = power_expr op = multiply_operator e2 = power_expr
    { S.BinExpr(e1, op, e2) }

power_expr:
    | e = unary_expr
    { e }
    | e1 = unary_expr T_POW e2 = unary_expr
    { S.BinExpr(e1, S.POW, e2) }

unary_expr:
    | op = unary_operator e = primary_expr
    { S.UnExpr(op, e) }
    | e = primary_expr
    { e }

primary_expr:
    | c = constant
    { c } (* | v = enumerated_value
    { v } *)
    | v = variable
    { S.Variable(v) }
    | T_LBRACE e = expression T_RBRACE
    { e }
    (* | f = func_name LBRACE param_assign RBRACE
    { f } *)
    (* | f = func_name LBRACE param_assign COMMA param_assign RBRACE
    { f } *)

(* variable_access: *)

(* multibit_part_access: *)

(* func_call: *)

stmt_list:
    | s = stmt T_SEMICOLON
    { s :: [] }
    | sl = stmt_list s = stmt T_SEMICOLON
    { s :: sl }

stmt:
    | s = T_NIL
    { S.Nil(s) }
    | s = assign_stmt
    { s }

assign_stmt:
    | v = variable T_ASSIGN e = expression
    { S.BinExpr(S.Variable(v), S.ASSIGN, e) }

(* assignment_attempt: *)

(* invocation: *)

(* subprog_ctrl_stmt: *)

(* param_assign: *)

(* selection_stmt: *)

(* if_stmt: *)

(* case_stmt: *)

(* case_selection: *)

(* case_list: *)

(* case_list_elem: *)

(* iteration_stmt: *)

(* for_stmt: *)

(* control_variable: *)

(* for_list: *)

(* while_stmt: *)

(* repeat_stmt: *)
(* }}} *)

(* {{{ Table 73-76 -- Graphical languages *)
(* }}} *)

(* {{{ Other symbols
 * Helper symbols and symbols which doesn't defined in standard grammar explicitly *)

(* Generic data types *)
generic_type_name:
    | T_ANY
    { S.ANY }
    | T_ANY_DERIVED
    { S.ANY_DERIVED }
    | T_ANY_ELEMENTARY
    { S.ANY_ELEMENTARY }
    | T_ANY_MAGNITUDE
    { S.ANY_MAGNITUDE }
    | T_ANY_NUM
    { S.ANY_NUM }
    | T_ANY_REAL
    { S.ANY_REAL }
    | T_ANY_INT
    { S.ANY_INT }
    | T_ANY_BIT
    { S.ANY_BIT }
    | T_ANY_STRING
    { S.ANY_STRING }
    | T_ANY_DATE
    { S.ANY_DATE }

var1_init_decl:
    | vs = variable_list; T_COLON; simple_spec_init;
    { vs }

location_prefix:
    | T_I
    { S.DirVarLocI }
    | T_Q
    { S.DirVarLocQ }
    | T_M
    { S.DirVarLocM }

size_prefix:
    | T_NIL
    { S.DirVarSizeNone }
    | T_X
    { S.DirVarSizeX }
    | T_B
    { S.DirVarSizeB }
    | T_W
    { S.DirVarSizeW }
    | T_D
    { S.DirVarSizeD }
    | T_L
    { S.DirVarSizeL }

compare_expr_operator:
    | T_GT
    | { S.GT }
    | T_LT
    | { S.LT }
    | T_GE
    | { S.GE }
    | T_LE
    | { S.LE }

add_operator:
    | T_PLUS
    | { S.ADD }
    | T_MINUS
    | { S.SUB }

multiply_operator:
    | T_MUL
    { S.MUL }
    | T_DIV
    { S.DIV }
    | T_MOD
    { S.MOD }

unary_operator:
    | T_MINUS
    { S.NEG }
    | T_NOT
    { S.NEG }
(* }}} *)

(* vim: set foldmethod=marker foldlevel=0 nofoldenable sw=2 tw=120 : *)
