%{
  open Core_kernel
  open IECCheckerCore

  module E = Error
  module S = Syntax
  module TI = Tok_info

  exception SyntaxError of string

  let creal_inv (vr, ti) =
    let vv = -1.0 *. vr in
    (vv, ti)

  let creal_conv_fp (vfp, ti) =
    let vv = float_of_string vfp in
    (vv, ti)

  let creal_mk t =
    let (v, ti) = t in
    S.CReal(v, ti)

  let ctime_mk fn t =
    let (v, ti) = t in
    let tv = (fn v) in
    S.CTimeValue(tv, ti)

  let cget_int_val = function
    | S.CInteger (v, _) -> v
    | _ -> E.raise E.InternalError "Unknown constant type"

  let mk_global_decl v =
    let vv = S.SymVar(v) in
    let s = S.VarDecl.SpecGlobal(None) in
    let vd = S.VarDecl.create vv s in
    vd
%}

(* {{{ Tokens *)

(* {{{ Common *)
%token T_ASSIGN    ":="
%token T_SENDTO    "=>"
%token T_DOT       "."
%token T_DEREF     "^"
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
%token T_WITH
%token T_RETAIN
%token T_NON_RETAIN
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
%token T_EOF
(* }}} *)

(* {{{ Picture 30 -- Common elements for textual languages *)
%token T_TYPE T_END_TYPE
%token T_VAR T_END_VAR
%token T_VAR_INPUT
%token T_VAR_OUTPUT
%token T_VAR_IN_OUT
%token T_VAR_EXTERNAL
%token T_VAR_TEMP
%token T_VAR_ACCESS
%token T_VAR_GLOBAL
%token T_VAR_CONFIG
%token T_FUNCTION T_END_FUNCTION
%token T_FUNCTION_BLOCK T_END_FUNCTION_BLOCK
%token T_PROGRAM T_END_PROGRAM
(* %token T_METHOD T_END_METHOD *)
(* %token T_STEP T_END_STEP *)
(* %token T_TRANSITION T_END_TRANSITION *)
(* %token T_ACTION T_END_ACTION *)
(* %token T_NAMESPACE T_END_NAMESPACE *)
(* }}} *)

(* {{{ Elementary data types *)
%token T_BYTE           "BYTE"
%token T_WORD           "WORD"
%token T_DWORD          "DWORD"
%token T_LWORD          "LWORD"
%token T_LREAL          "LREAL"
%token T_REAL           "REAL"
%token T_SINT           "SINT"
%token T_INT            "INT"
%token T_DINT           "DINT"
%token T_LINT           "LINT"
%token T_USINT          "USINT"
%token T_UINT           "UINT"
%token T_UDINT          "UDINT"
%token T_ULINT          "ULINT"
%token T_WSTRING        "WSTRING"
%token T_STRING         "STRING"
%token T_CHAR           "CHAR"
%token T_WCHAR          "WCHAR"
%token T_BOOL           "BOOL"
%token T_TIME           "TIME"
%token T_LTIME          "LTIME"
%token T_DATE           "DATE"
%token T_LDATE          "LDATE"
%token T_DATE_AND_TIME  "DATE_AND_TIME"
%token T_LDATE_AND_TIME "LDATE_AND_TIME"
%token T_DT             "DT"
%token T_LDT            "LDT"
%token T_TIME_OF_DAY    "TIME_OF_DAY"
%token T_LTIME_OF_DAY   "LTIME_OF_DAY"
%token T_TOD            "TOD"
%token T_LTOD           "LTOD"
(* }}} *)

(* {{{ Generic data types *)
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
(* }}} *)

(* {{{ ST operators *)
%token T_OR             "OR"
%token T_XOR            "XOR"
%token T_AND            "AND"
%token T_EQU            "EQU"
(* }}} *)

(* {{{ ST control statements *)
%token<IECCheckerCore.Tok_info.t> T_IF T_THEN T_ELSIF T_ELSE T_END_IF
%token<IECCheckerCore.Tok_info.t> T_CASE T_OF T_END_CASE
%token<IECCheckerCore.Tok_info.t> T_FOR T_TO T_BY T_DO T_END_FOR
%token<IECCheckerCore.Tok_info.t> T_WHILE T_END_WHILE T_REPEAT T_END_REPEAT T_UNTIL
(* }}} *)

(* {{{ Helpers for date and time literals

   According standard T, LT, D and DT are not keywords, so we need to
   define an extra terminal symbols to distinguish them from regular
   identifiers.
 *)
%token T_TSHARP     "T#"
%token T_LTSHARP    "LT#"
%token T_DSHARP     "D#"
%token T_LDSHARP    "LD#"
(* }}} *)

(* {{{ Non-terminal symbols *)
%token <string * IECCheckerCore.Tok_info.t> T_IDENTIFIER
%token <int * IECCheckerCore.Tok_info.t> T_INTEGER
%token <int * IECCheckerCore.Tok_info.t> T_BINARY_INTEGER
%token <int * IECCheckerCore.Tok_info.t> T_OCTAL_INTEGER
%token <int * IECCheckerCore.Tok_info.t> T_HEX_INTEGER
%token <bool * IECCheckerCore.Tok_info.t> T_BOOL_VALUE
%token <float * IECCheckerCore.Tok_info.t> T_REAL_VALUE
%token <string * IECCheckerCore.Tok_info.t> T_FIX_POINT_VALUE

%token <float * IECCheckerCore.Tok_info.t> T_TIME_INTERVAL_D
%token <float * IECCheckerCore.Tok_info.t> T_TIME_INTERVAL_H
%token <float * IECCheckerCore.Tok_info.t> T_TIME_INTERVAL_M
%token <float * IECCheckerCore.Tok_info.t> T_TIME_INTERVAL_S
%token <float * IECCheckerCore.Tok_info.t> T_TIME_INTERVAL_MS
%token <float * IECCheckerCore.Tok_info.t> T_TIME_INTERVAL_US
%token <float * IECCheckerCore.Tok_info.t> T_TIME_INTERVAL_NS
(* }}} *)

(* }}} *)

(* Parser entry point. *)
%start <IECCheckerCore.Syntax.iec_library_element list> main

%%
(* Parser entry point *)
main:
    | T_EOF
    { [] }
    | dl = library_element_declaration_list; T_EOF
    { List.rev dl }

(* {{{ Programming model *)
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
(* }}} *)

(* {{{ Table 1 -- Symbols / Table 2 -- Identifiers *)
(* Implemented in lexer. *)
(* }}} *)

(* {{{ Table 4 -- Pragmas *)
(* pragma: *)
(* }}} *)

(* {{{ Table 5 -- Numeric literals *)
(* In Annex A any external representation of data are designated as "constant".
 * So literally constant is just a literal value. *)
constant:
  | c = numeric_literal
  { S.Constant(c) }
  (* | c = char_literal
  | { c } *)
  | c = time_literal
  { S.Constant(c) }
  (* | c = bit_str_literal
  { c } *)
  | c = bool_literal
  { S.Constant(c) }

numeric_literal:
  | res = int_literal
  { res }
  | res = real_literal
  { res }

int_literal:
  | int_type_name T_SHARP v = signed_int
  { v }
  | int_type_name T_SHARP v = binary_int
  { v }
  | int_type_name T_SHARP v = octal_int
  { v }
  | int_type_name T_SHARP v = hex_int
  { v }
  | v = signed_int
  { v }
  | v = binary_int
  { v }
  | v = octal_int
  { v }
  | v = hex_int
  { v }

unsigned_int:
  | vi = T_INTEGER
  {
    let (v, ti) = vi in
    S.CInteger(v, ti)
  }

signed_int:
  | i = unsigned_int
  { i }
  | T_PLUS i = unsigned_int
  { i }
  | T_MINUS res = T_INTEGER
  {
    let (v, ti) = res in
    S.CInteger(-v, ti)
  }

binary_int:
  | vi = T_BINARY_INTEGER
  {
    let (v, ti) = vi in
    S.CInteger(v, ti)
  }

octal_int:
  | vi = T_OCTAL_INTEGER
  {
    let (v, ti) = vi in
    S.CInteger(v, ti)
  }

hex_int:
  | vi = T_HEX_INTEGER
  {
    let (v, ti) = vi in
    S.CInteger(v, ti)
  }

real_literal:
  (* With exponent *)
  | real_type_name T_SHARP vr = T_REAL_VALUE
  { creal_mk vr }
  | real_type_name T_SHARP T_PLUS vr = T_REAL_VALUE
  { creal_mk vr }
  | real_type_name T_SHARP T_MINUS vr = T_REAL_VALUE
  { creal_mk (creal_inv vr) }
  | vr = T_REAL_VALUE
  { creal_mk vr }
  | T_PLUS vr = T_REAL_VALUE
  { creal_mk vr }
  | T_MINUS vr = T_REAL_VALUE
  { creal_mk (creal_inv vr) }
  (* Conversion from fixed-point token *)
  | real_type_name T_SHARP vr = T_FIX_POINT_VALUE
  { creal_mk (creal_conv_fp vr) }
  | real_type_name T_SHARP T_PLUS vr = T_FIX_POINT_VALUE
  { creal_mk (creal_conv_fp vr) }
  | real_type_name T_SHARP T_MINUS vr = T_FIX_POINT_VALUE
  { creal_mk (creal_inv (creal_conv_fp vr)) }
  | vr = T_FIX_POINT_VALUE
  { creal_mk (creal_conv_fp vr) }
  | T_PLUS vr = T_FIX_POINT_VALUE
  { creal_mk (creal_conv_fp vr) }
  | T_MINUS vr = T_FIX_POINT_VALUE
  { creal_mk (creal_inv (creal_conv_fp vr)) }

(* bit_str_literal: *)

bool_literal:
  (* BOOL#<value> rules are implemented in lexer *)
  | vb = T_BOOL_VALUE
  {
    let (v, ti) = vb in
    S.CBool(v, ti)
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
time_literal:
  | v = duration
  { v }
  | v = time_of_day
  { v }
  | v = date
  { v }
  | v = date_and_time
  { v }

duration:
  | time_type_helper v = interval
  { v }
  | time_type_helper T_PLUS v = interval
  { v }
  | time_type_helper T_MINUS v = interval
  {
    match v with
    | S.CTimeValue(tv, ti) ->
        S.CTimeValue((S.TimeValue.inv tv), ti)
    | _ -> E.raise E.InternalError "Unknown constant type"
  }

(* Helper symbol for duration. *)
time_type_helper:
  | v = time_type_name T_SHARP
  { v }
  | v = T_TSHARP
  { S.TIME }
  | v = T_LTSHARP
  { S.LTIME }

(* Return string * TI.t *)
fix_point:
  | fp = T_FIX_POINT_VALUE
  { fp }
  | fp = T_INTEGER
  {
    let (vi, ti) = fp in
    let vs = string_of_int vi in
    (vs, ti)
  }

interval:
  | v = days
  { v }
  | v = hours
  { v }
  | v = minutes
  { v }
  | v = seconds
  { v }
  | v = miliseconds
  { v }
  | v = microseconds
  { v }
  | v = nanoseconds
  { v }

days:
  | vt = T_TIME_INTERVAL_D
  { ctime_mk (fun v -> S.TimeValue.mk ~d:v ()) vt }
  | vt = T_TIME_INTERVAL_D; v = hours
  { ctime_mk (fun v -> S.TimeValue.mk ~d:v ()) vt |> S.c_add v }

hours:
  | vt = T_TIME_INTERVAL_H
  { ctime_mk (fun v -> S.TimeValue.mk ~h:v ()) vt }
  | vt = T_TIME_INTERVAL_H; v = minutes
  { ctime_mk (fun v -> S.TimeValue.mk ~h:v ()) vt |> S.c_add v }

minutes:
  | vt = T_TIME_INTERVAL_M
  { ctime_mk (fun v -> S.TimeValue.mk ~m:v ()) vt }
  | vt = T_TIME_INTERVAL_M; v = seconds
  { ctime_mk (fun v -> S.TimeValue.mk ~m:v ()) vt |> S.c_add v }

seconds:
  | vt = T_TIME_INTERVAL_S
  { ctime_mk (fun v -> S.TimeValue.mk ~s:v ()) vt }
  | vt = T_TIME_INTERVAL_S; v = miliseconds
  { ctime_mk (fun v -> S.TimeValue.mk ~s:v ()) vt |> S.c_add v }

miliseconds:
  | vt = T_TIME_INTERVAL_MS
  { ctime_mk (fun v -> S.TimeValue.mk ~ms:v ()) vt }
  | vt = T_TIME_INTERVAL_MS; v = microseconds
  { ctime_mk (fun v -> S.TimeValue.mk ~ms:v ()) vt |> S.c_add v }

microseconds:
  | vt = T_TIME_INTERVAL_US
  { ctime_mk (fun v -> S.TimeValue.mk ~us:v ()) vt }
  | vt = T_TIME_INTERVAL_US; v = nanoseconds;
  { ctime_mk (fun v -> S.TimeValue.mk ~us:v ()) vt |> S.c_add v }

nanoseconds:
  | vt = T_TIME_INTERVAL_NS
  { ctime_mk (fun v -> S.TimeValue.mk ~ns:v ()) vt }

time_of_day:
  | tod_type_name; T_SHARP vt = daytime
  { vt }
  | T_LTIME_OF_DAY; T_SHARP vt = daytime
  { vt }

daytime:
  | hi = day_hour T_COLON mi = day_minute T_COLON ss = day_second
  {
    let ctime_of_timevals ch cm ss =
      let ti = S.c_get_ti ch in
      let hf = float_of_int (cget_int_val ch) in
      let mf = float_of_int (cget_int_val cm) in
      let sf = float_of_string ss in
      let tv = S.TimeValue.mk ~h:hf ~m:mf ~s:sf () in
      S.CTimeValue(tv, ti)
    in
    ctime_of_timevals hi mi ss
  }

day_hour:
  | v = unsigned_int
  { v }

day_minute:
  | v = unsigned_int
  { v }

day_second:
  | fp = fix_point
  { let (fps, _) = fp in fps }

date:
  | date_type_name; T_SHARP dt = date_literal
  { dt }
  | T_DSHARP dt = date_literal
  { dt }
  | T_LDSHARP dt = date_literal
  { dt }

date_literal:
  | cy = year T_MINUS cmo = month T_MINUS cd = day
  {
    let cdate_of_timevals cy cmo cd =
      let ti = S.c_get_ti cy in
      let yi = (cget_int_val cy) in
      let moi = (cget_int_val cmo) in
      let df = float_of_int (cget_int_val cd) in
      let tv = S.TimeValue.mk ~y:yi ~mo:moi ~d:df () in
      S.CTimeValue(tv, ti)
    in
    cdate_of_timevals cy cmo cd
  }

year:
  | v = unsigned_int
  { v }

month:
  | v = unsigned_int
  { v }

day:
  | v = unsigned_int
  { v }

date_and_time:
  | dt_type_name; T_SHARP; d = date_literal; T_MINUS dt = daytime
  { S.c_add d dt }
  | T_LDATE_AND_TIME T_SHARP; d = date_literal; T_MINUS dt = daytime
  { S.c_add d dt }
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
  | ty = numeric_type_name
  { ty }
  | ty = bit_str_type_name
  { ty }
  | ty = string_type_name
  { ty }
  | ty = date_type_name
  { ty }
  | ty = time_type_name
  { ty }
  (* NOTE: TOD and DT types are not defined as part of elem_type_name in 3rd edition of IEC61131-3
     standard. This seems like a typo because 2nd edition includes these types in date_type which
     has been splitted to few rules in new standard after introducing long types. *)
  | ty = tod_type_name
  { ty }
  | ty = dt_type_name
  { ty }

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
  | T_STRING l = string_type_length
  { S.STRING(l) }
  | T_WSTRING l = string_type_length
  { S.WSTRING(l) }
  | T_STRING
  { S.STRING(0) }
  | T_WSTRING
  { S.WSTRING(0) }
  | T_CHAR
  { S.CHAR }
  | T_WCHAR
  { S.WCHAR }

(* Helper symbol for string_type_name *)
string_type_length:
  | T_LBRACK c = unsigned_int T_RBRACK
  {
    match c with
    | CInteger(v, _) -> v
    | _ -> raise (SyntaxError "Incorrect string length value")
  }

time_type_name:
  | T_TIME
  { S.TIME }
  | T_LTIME
  { S.LTIME }

date_type_name:
  | T_DATE
  { S.DATE }
  | T_LDATE
  { S.LDATE }

tod_type_name:
  | T_TIME_OF_DAY
  { S.TIME_OF_DAY }
  | T_TOD
  { S.TOD }
  | T_LTOD
  { S.LTOD }

dt_type_name:
  | T_DATE_AND_TIME
  { S.DATE_AND_TIME }
  | T_LDATE_AND_TIME
  { S.LDATE_AND_TIME }
  | T_DT
  { S.DT }
  | T_LDT
  { S.LDT }

bit_str_type_name:
  | ty = bool_type_name
  { ty }
  | ty = multibits_type_name
  { ty }

bool_type_name:
  | T_BOOL
  { S.BOOL }

multibits_type_name:
  | T_BYTE
  { S.BYTE }
  | T_WORD
  { S.WORD }
  | T_DWORD
  { S.DWORD }
  | T_LWORD
  { S.LWORD }
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

string_type_access:
  | ty = string_type_name
  { ty }

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
  | T_PERCENT loc = dir_var_location_prefix; sz = dir_var_size_prefix; pcs = unsigned_int_list;
  {
    let pvals = List.map ~f:(fun c -> cget_int_val c) pcs in
    S.VarDecl.SpecDirect(None)
  }

(* Helper symbol for direct_variable.
 * Return int list. *)
unsigned_int_list:
  | ci = unsigned_int;
  { ci :: [] }
  | cil = unsigned_int_list; ci = unsigned_int;
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
  { S.SymVar(v) }

symbolic_variable:
  | out = variable_name
  { let (name, ti) = out in S.SymVar.create name ti }
  (* | multi_elem_var *)

(* var_access: *)

variable_name:
  | id = T_IDENTIFIER
  {
    let (name, ti) = id in
    (name, ti)
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
    List.map ~f:(fun v -> (S.VarDecl.set_qualifier_exn v S.VarDecl.QRetain)) vdsr
  }
  | T_VAR_INPUT T_NON_RETAIN vds = input_decl T_END_VAR
  {
    let vdsr = List.rev vds in
    List.map ~f:(fun v -> (S.VarDecl.set_qualifier_exn v S.VarDecl.QNonRetain)) vdsr
  }

input_decl:
  | vs = var_decl_init_list
  {
    let vsr = List.rev vs in
    List.map ~f:(fun v -> (
      let s = S.VarDecl.SpecIn(None) in
      S.VarDecl.create v s
    )) vsr
  }

(* edge_decl: *)

var_decl_init:
  | vs = variable_list; T_COLON; simple_spec_init;
  { List.map ~f:(fun (n, ti) -> (
    let v = S.SymVar.create n ti in
    S.SymVar(v)
  )) vs }

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
  | T_VAR_OUTPUT vs = var_decl_init_list T_END_VAR
  {
    let vsr = List.rev vs in
    List.map ~f:(fun v -> (
      let s = S.VarDecl.SpecOut(None) in
      S.VarDecl.create v s
    )) vsr
  }
  | T_VAR_OUTPUT T_RETAIN vs = var_decl_init_list T_END_VAR
  {
    let vsr = List.rev vs in
    List.map ~f:(fun v -> (
      let s = S.VarDecl.SpecOut(Some S.VarDecl.QRetain) in
      S.VarDecl.create v s
    )) vsr
  }
  | T_VAR_OUTPUT T_NON_RETAIN vs = var_decl_init_list T_END_VAR
  {
    let vsr = List.rev vs in
    List.map ~f:(fun v -> (
      let s = S.VarDecl.SpecOut(Some S.VarDecl.QNonRetain) in
      S.VarDecl.create v s
    )) vsr
  }

(* output_decl: *)

in_out_decls:
  | T_VAR_IN_OUT vds = in_out_var_decl T_END_VAR
  { vds }

(* Return list of S.VarDecl.t *)
in_out_var_decl:
  | vs = var_decl_list
  {
    let vsr = List.rev vs in
    List.map ~f:(fun v -> S.VarDecl.create v S.VarDecl.SpecInOut) vsr
  }
  (* | vs = array_conform_decl
  {  } *)
  (* | vs = fb_decl_no_init
  {  } *)

var_decl:
  | vs = variable_list; T_COLON; simple_spec;
  { List.map ~f:(fun (n, ti) -> (
    let v = S.SymVar.create n ti in
    S.SymVar(v)
  )) vs }
  (* | vs = variable_list; T_COLON; str_var_decl; *)
  (* | vs = variable_list; T_COLON; array_var_decl; *)
  (* | vs = variable_list; T_COLON; struct_var_decl; *)

(* Helper symbol for var_decl *)
var_decl_list:
  | v = var_decl T_SEMICOLON
  { v }
  | vs = var_decl_list; v = var_decl  T_SEMICOLON
  { List.append vs v }

(* array_var_decl: *)

(* struct_var_decl: *)

var_decls:
  | T_VAR vs = var_decl_init_list T_END_VAR
  {
    let vsr = List.rev vs in
    List.map ~f:(fun v -> (
      let s = S.VarDecl.Spec(None) in
      S.VarDecl.create v s
    )) vsr
  }
  | T_VAR T_RETAIN vs = var_decl_init_list T_END_VAR
  {
    let vsr = List.rev vs in
    List.map ~f:(fun v -> (
      let s = S.VarDecl.Spec(Some S.VarDecl.QRetain) in
      S.VarDecl.create v s
    )) vsr
  }

retain_var_decls:
  | T_VAR T_RETAIN vs = var_decl_init_list T_END_VAR
  {
    let vsr = List.rev vs in
    List.map ~f:(fun v -> (
      let s = S.VarDecl.Spec(Some S.VarDecl.QRetain) in
      S.VarDecl.create v s
    )) vsr
  }

(* loc_var_decls: *)

(* loc_var_decl: *)

temp_var_decls:
  | T_VAR_TEMP vds = temp_var_decl T_END_VAR
  { vds }
  (* | ref_var_decl *)
  (* | interface_var_decl *)

(* Helper symbol for temp_var_decls_list. *)
temp_var_decl:
  | vs = var_decl_list
  {
    let vsr = List.rev vs in
    List.map ~f:(fun v -> S.VarDecl.create v S.VarDecl.SpecTemp) vsr
  }

external_var_decls:
  | T_VAR_EXTERNAL vl = list(external_decl) T_END_VAR
  {
    let vlr = List.rev vl in
    List.map ~f:(fun v -> (
      let s = S.VarDecl.SpecGlobal(None) in
      S.VarDecl.create v s
    )) vlr
  }
  | T_VAR_EXTERNAL T_RETAIN vl = list(external_decl) T_END_VAR
  {
    let vlr = List.rev vl in
    List.map ~f:(fun v -> (
      let s = S.VarDecl.SpecGlobal(Some S.VarDecl.QRetain) in
      S.VarDecl.create v s
    )) vlr
  }

external_decl:
  | out = variable_name T_COLON  simple_spec T_SEMICOLON
  {
    let (n, ti) = out in
    let v = S.SymVar.create n ti in
    let vv = S.SymVar(v) in
    vv
  }

global_var_name:
  | id = T_IDENTIFIER
  {
    let (n, ti) = id in
    S.SymVar.create n ti
  }

(* Helper symbol for global_var_name *)
global_var_list:
  | out = global_var_name;
  {
    let vd = mk_global_decl out in
    vd :: []
  }
  | vds = global_var_list; T_COMMA out = global_var_name
  {
    let vd = mk_global_decl out in
    vd :: vds
  }

global_var_decls:
  | T_VAR_GLOBAL vds = global_var_decl_list; T_END_VAR
  { List.rev vds }
  | T_VAR_GLOBAL T_RETAIN vds = global_var_decl_list; T_END_VAR
  {
    let vdsr = List.rev vds in
    List.map ~f:(fun v -> (S.VarDecl.set_qualifier_exn v S.VarDecl.QNonRetain)) vdsr
  }
  | T_VAR_GLOBAL T_CONSTANT vds = global_var_decl_list; T_END_VAR
  {
    let vdsr = List.rev vds in
    List.map ~f:(fun v -> (S.VarDecl.set_qualifier_exn v S.VarDecl.QConstant)) vdsr
  }

(* Return S.VarDecl.t list *)
global_var_decl:
  | ss = global_var_spec; T_COLON loc_var_spec_init;
  { ss }

(* Helper symbol for gloval_var_decl *)
global_var_decl_list:
  | v = global_var_decl T_SEMICOLON
  { v }
  | vs = global_var_decl_list; v = global_var_decl T_SEMICOLON
  { List.append vs v }

(* Return S.VarDecl.t list *)
global_var_spec:
  | vs = global_var_list
  { vs }
  | v = global_var_name; l = located_at
  {
    let vv = S.SymVar(v) in
    let s = S.VarDecl.SpecGlobal(None) in
    let vd = S.VarDecl.create vv s in
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
    | T_VAR vl = list(loc_partly_var) T_END_VAR
    { List.rev vl }
    | T_VAR T_RETAIN vl = list(loc_partly_var) T_END_VAR
    { List.rev vl } (* TODO: add qualifier *)
    | T_VAR T_NON_RETAIN vl = list(loc_partly_var) T_END_VAR
    { List.rev vl } (* TODO: add qualifier *)

loc_partly_var:
    | out = variable_name; T_AT T_PERCENT l = dir_var_location_prefix T_MUL T_COLON s = var_spec T_SEMICOLON
    {
        let (n, ti) = out in
        let v = S.SymVar.create n ti in
        let vv = S.SymVar(v) in
        let s = S.VarDecl.SpecDirect(None) in
        S.VarDecl.create vv s
    }

var_spec:
    | ty = simple_spec
    { ty }

(* }}} *)

(* {{{ Table 19 -- Function declaration *)
func_name:
  | id = T_IDENTIFIER
  {
    let (name, ti) = id in
    S.Function.create name ti
  }

func_access:
  (* | namespace_name func_name
  {  } *)
  | f = func_name
  { f }

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
  | vs = temp_var_decls
  { vs }
  | vss = function_vars; vs = io_var_decls
  { List.append vss vs }
  | vss = function_vars; vs = func_var_decls
  { List.append vss vs }
  | vss = function_vars; vs = temp_var_decls
  { List.append vss vs }

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
  | T_VAR T_NON_RETAIN vs = var_decl_init_list T_END_VAR
  {
    let vsr = List.rev vs in
    List.map ~f:(fun v -> (
      let s = S.VarDecl.Spec(Some S.VarDecl.QNonRetain) in
      S.VarDecl.create v s
    )) vsr
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
  | T_VAR_ACCESS vl = list(prog_access_decl) T_END_VAR
  { List.rev vl }

prog_access_decl:
    | an = access_name T_COLON v = symbolic_variable T_COLON data_type_access T_SEMICOLON
    {
      let vv = S.SymVar(v) in
      let s = S.VarDecl.SpecAccess(an) in
      S.VarDecl.create vv s
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
    | rcs = list(resource_decl)
    { rcs }

(* Return S.resource_decl *)
resource_decl:
    | T_RESOURCE n = resource_name; T_ON resource_type_name; rc = single_resource_decl; T_END_RESOURCE
    { { S.name = Some n; S.tasks = rc.tasks; S.variables = []; S.programs = rc.programs } }
    | T_RESOURCE n = resource_name; T_ON resource_type_name; vs = global_var_decls; rc = single_resource_decl; T_END_RESOURCE
    { { S.name = Some n; S.tasks = rc.tasks; S.variables = vs; S.programs = rc.programs } }

(* Return S.resource_decl *)
single_resource_decl:
    | ts = list(task_config); pis = prog_config_list
    { { S.name = None; S.tasks = ts; S.variables = []; S.programs = pis } }
    | pis = prog_config_list
    { { S.name = None; S.tasks = []; S.variables = []; S.programs = pis } }

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
    | T_VAR_ACCESS ads = list(access_decl); T_END_VAR
    { ads }

access_decl:
    | access_name; T_COLON access_path; T_COLON data_type_access T_SEMICOLON
    {  }
    | access_name; T_COLON access_path; T_COLON data_type_access; access_direction T_SEMICOLON
    {  }

access_path:
    | resource_name; T_DOT; direct_variable
    {  }
    (* | dv = direct_variable
    {  } *)
    (* | resource_name; T_DOT; pn = prog_name; T_DOT fn = fb_name; T_DOT; v = symbolic_variable
    {  } *)
    | resource_name; T_DOT; prog_name; T_DOT; symbolic_variable
    {  }
    (* | pn = prog_name; T_DOT fn = fb_name; T_DOT; v = symbolic_variable
    {  } *)
    | prog_name; T_DOT; symbolic_variable
    {  }

(* Return S.SymVar *)
global_var_access:
    | out = global_var_name;
    { S.SymVar(out) }
    | resource_name_list; out = global_var_name;
    { S.SymVar(out) }
    | out = global_var_name; struct_elem_name_list
    { S.SymVar(out) }
    | resource_name_list; out = global_var_name; struct_elem_name_list
    { S.SymVar(out) }

access_name:
    | id = T_IDENTIFIER
    {
        let name, _ = id in
        name
    }

prog_output_access:
  | p = prog_name T_DOT v = symbolic_variable
  {
    let vv = S.SymVar(v) in
    let n = S.ProgramConfig.get_name p in (n, vv)
  }

prog_name:
  | id = T_IDENTIFIER
  {
    let (name, ti) = id in
    S.ProgramConfig.create name ti
  }

(* Helper symbol for prog_name *)
prog_name_qual:
  | p = prog_name;
  { p }
  | p = prog_name; T_RETAIN
  { S.ProgramConfig.set_qualifier p S.ProgramConfig.QRetain }
  | p = prog_name; T_NON_RETAIN
  { S.ProgramConfig.set_qualifier p S.ProgramConfig.QNonRetain }

access_direction:
  | T_READ_WRITE
  {  }
  | T_READ_ONLY
  {  }

task_config:
  | T_TASK t = task_name; i = task_init; T_SEMICOLON
  { t }

task_name:
    | id = T_IDENTIFIER
    {
        let (name, ti) = id in
        S.Task.create name ti
    }

task_init:
    | T_LPAREN T_SINGLE s = data_source; T_COMMA T_INTERVAL i = data_source; T_COMMA T_PRIORITY T_ASSIGN p = unsigned_int; T_RPAREN
    { (Some s, Some i, Some p) }
    | T_LPAREN T_SINGLE T_ASSIGN s = data_source; T_COMMA T_PRIORITY T_ASSIGN p = unsigned_int; T_RPAREN
    { (Some s, None, Some p) }
    | T_LPAREN T_INTERVAL T_ASSIGN i = data_source; T_COMMA T_PRIORITY T_ASSIGN p = unsigned_int; T_RPAREN
    { (None, Some i, Some p) }
    | T_LPAREN T_PRIORITY T_ASSIGN p = unsigned_int; T_RPAREN
    { (None, None, Some p) }

data_source:
    | v = constant
    { let c = S.c_from_expr_exn v in S.Task.DSConstant(c) }
    | v = global_var_access
    { S.Task.DSGlobalVar(v) }
    | out = prog_output_access
    {
      let (prog_name, var) = out in
      S.Task.DSProgOutput(prog_name, var)
    }
    (* TODO: Need refactor direct_variable rule to return Variable.t. *)
    (* | v = direct_variable
    { S.Task.DSGlobalVar(v) } *)

prog_config:
    | T_PROGRAM pc = prog_name_qual; T_COLON prog_type_access;
    { pc }
    | T_PROGRAM pc = prog_name_qual; T_COLON prog_type_access; T_LBRACE; cvs = prog_conf_elems; T_RBRACE
    {
        let pc = S.ProgramConfig.set_conn_vars pc cvs in
        pc
    }
    | T_PROGRAM pc = prog_name_qual; T_WITH; t = task_name; T_COLON ty = prog_type_name;
    {
        let pc = S.ProgramConfig.set_task pc t in
        pc
    }
    | T_PROGRAM pc = prog_name_qual; T_WITH; t = task_name; T_COLON ty = prog_type_name; T_LBRACE; cvs = prog_conf_elems; T_RBRACE
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
    { S.SymVar(v) }
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
    | resource_name; T_DOT; prog_name; T_DOT;
    {  }
    (* | resource_name; T_DOT; prog_name; T_DOT; fn = fb_name T_DOT;
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
  | e1 = expression T_OR e2 = xor_expr
  { S.BinExpr(e1, S.OR, e2) }

(* According IEC61131-3 constant expression should be evaluated at compile-time.

   There are no examples or more formal rules for supported compile-time expressions
   in a Standard text, so I don't know what does this means. For now I replace this
   rule from a previous version of standard which replaces constant expression with
   a following BNF rule:

   signed_integer | enumerated_value

*)
constant_expr:
  | v = signed_int
  {
    match v with
    | CInteger(v, _) -> v
    | _ -> raise (SyntaxError "Incorrect constant expression value")
  }
  (* | v = enum_value
  { v } *)

xor_expr:
  | s = and_expr
  { s }
  | e1 = xor_expr T_XOR e2 = and_expr
  { S.BinExpr(e1, S.XOR, e2) }

and_expr:
  | s = compare_expr
  { s }
  | e1 = and_expr T_AND e2 = compare_expr
  { S.BinExpr(e1, S.AND, e2) }

compare_expr:
  | s = equ_expr
  { s }
  | e1 = compare_expr T_EQ e2 = equ_expr
  { S.BinExpr(e1, S.EQ, e2) }
  | e1 = compare_expr T_NEQ e2= equ_expr
  { S.BinExpr(e1, S.NEQ, e2) }

equ_expr:
  | s = add_expr
  { s }
  | e1 = equ_expr op = compare_expr_operator e2 = add_expr
  { S.BinExpr(e1, op, e2) }

add_expr:
  | t = term
  { t }
  | t1 = add_expr op = add_operator t2 = term
  { S.BinExpr(t1, op, t2)}

term:
  | e = power_expr
  { e }
  | e1 = term op = multiply_operator e2 = power_expr
  { S.BinExpr(e1, op, e2) }

power_expr:
  | e = unary_expr
  { e }
  | e1 = power_expr T_POW e2 = unary_expr
  { S.BinExpr(e1, S.POW, e2) }

unary_expr:
  | op = unary_operator e = primary_expr
  { S.UnExpr(op, e) }
  | e = primary_expr
  { e }

primary_expr:
  | c = constant
  { c }
  (* | enum_value
  { } *)
  | v = variable_access
  { S.Variable(v) }
  | fc = func_call
  { S.FuncCall(fc) }
  (* | ref_value
  {  } *)
  | T_LPAREN e = expression T_RPAREN
  { e }

variable_access:
  | v = variable_expr multibit_part_access
  { v }
  | v = variable_expr
  { v }

(* Helper symbol for variable_access.
 * This is required to avoid shift/reduce conflict with identifier from func_name rule. *)
variable_expr:
  | id = T_IDENTIFIER
  {
    let (name, ti) = id in
    let sv = S.SymVar.create name ti in
    S.SymVar(sv)
  }

multibit_part_access:
  | T_DOT v = unsigned_int
  {  }
  | T_DOT T_PERCENT sz = dir_var_size_prefix v = unsigned_int
  {  }
  | T_DOT T_PERCENT v = unsigned_int
  {  }

func_call:
  | f = func_access T_LPAREN T_RPAREN
  {
    let ti = S.Function.get_ti f in
    S.StmFuncCall(ti, f, [])
  }
  | f = func_access T_LPAREN stmts = param_assign_list T_RPAREN
  {
    let ti = S.Function.get_ti f in
    S.StmFuncCall(ti, f, stmts)
  }

(* Helper symbol for func_call *)
param_assign_list:
  | p = param_assign
  { p :: [] }
  | ps = param_assign_list T_COMMA p = param_assign
  { p :: ps }

stmt_list:
  | s = stmt T_SEMICOLON
  { s :: [] }
  | sl = stmt_list; s = stmt T_SEMICOLON
  { s :: sl }

stmt:
  | s = assign_stmt
  { s }
  | s = subprog_ctrl_stmt
  { s }
  | s = selection_stmt
  { s }
  | s = iteration_stmt
  { s }

assign_stmt:
  | v = variable T_ASSIGN e = expression
  {
    let ti = S.vget_ti v in
    S.StmAssign(ti, v, e)
  }

(* assignment_attempt: *)

(* invocation: *)

subprog_ctrl_stmt:
  | fc = func_call
  { fc }
  (* | invocation
  {  } *)
  (* | T_SUPER T_LPAREN T_RPAREN
  {  } *)
  (* | T_RETURN
  {  } *)

param_assign:
  | vn = variable_name T_ASSIGN e = expression
  {
    let (n, _) = vn in
    S.StmFuncParamAssign(Some n, e, false)
  }
  | e = expression
  {
    S.StmFuncParamAssign(None, e, false)
  }
  (* | ref_assign
  {  } *)
  (* Use assignment expression for "=>" tokens that assigns given parameter
     to a function output variable. *)
  | T_NOT vn = variable_name T_SENDTO v = variable
  {
    let (n, _) = vn in
    let vexpr = S.Variable(v) in
    S.StmFuncParamAssign(Some n, vexpr, true)
  }
  | vn = variable_name T_SENDTO v = variable
  {
    let (n, _) = vn in
    let vexpr = S.Variable(v) in
    S.StmFuncParamAssign(Some n, vexpr, false)
  }

selection_stmt:
  | s = if_stmt
  { s }
  | s = case_stmt
  { s }

if_stmt:
  | ti = T_IF cond = expression T_THEN if_stmts = stmt_list T_END_IF
  { S.StmIf(ti, cond, if_stmts, [], []) }
  | ti = T_IF cond = expression T_THEN if_stmts = stmt_list; T_ELSE else_stmts = stmt_list T_END_IF
  { S.StmIf(ti, cond, if_stmts, [], else_stmts) }
  | ti = T_IF cond = expression T_THEN if_stmts = stmt_list; elsif_stmts = if_stmt_elsif_list T_END_IF
  { S.StmIf(ti, cond, if_stmts, elsif_stmts, []) }
  | ti = T_IF cond = expression T_THEN if_stmts = stmt_list; elsif_stmts = if_stmt_elsif_list; T_ELSE else_stmts = stmt_list T_END_IF
  { S.StmIf(ti, cond, if_stmts, elsif_stmts, else_stmts) }

(* Helper symbol for if_stmt *)
if_stmt_elsif_list:
  | ti = T_ELSIF cond = expression T_THEN stmts = stmt_list
  {
    let elsif = S.StmElsif(ti, cond, stmts) in
    elsif :: []
  }
  | elsifs = if_stmt_elsif_list; ti = T_ELSIF cond = expression T_THEN stmts = stmt_list
  {
    let elsif = S.StmElsif(ti, cond, stmts) in
    elsif :: elsifs
  }

case_stmt:
  | ti = T_CASE e = expression T_OF css = case_selection_list T_ELSE sl = stmt_list T_END_CASE
  { S.StmCase(ti, e, css, sl) }
  | ti = T_CASE e = expression T_OF css = case_selection_list T_END_CASE
  { S.StmCase(ti, e, css, []) }

(* Helper symbol for case_stmt *)
case_selection_list:
  | cs = case_selection
  { cs :: [] }
  | css = case_selection_list cs = case_selection
  { cs :: css }

case_selection:
  | cl = case_list T_COLON sl = stmt_list
  { {S.case = cl; S.body = sl} }

case_list:
  | e = case_list_elem
  { e :: [] }
  | es = case_list T_COMMA e = case_list_elem
  { e :: es }

case_list_elem:
  (* | e = subrange
  {  } *)
  | e = constant_expr
  { e }

iteration_stmt:
  | s = for_stmt
  { s }
  | s = while_stmt
  { s }
  | s = repeat_stmt
  { s }
  (* | T_EXIT
  { } *)
  (* | T_CONTINUE
  { } *)

for_stmt:
  | ti = T_FOR cv = control_variable T_ASSIGN fl = for_list T_DO sl = stmt_list T_END_FOR
  {
    let (e1,e2,e3) = fl in
    S.StmFor(ti, cv, e1, e2, e3, sl)
  }

control_variable:
  | id = T_IDENTIFIER
  {
    let (n, ti) = id in
    S.SymVar.create n ti
  }

for_list:
  | e1 = expression T_TO e2 = expression T_BY e3 = expression
  { (e1, e2, Some(e3)) }
  | e1 = expression T_TO e2 = expression
  { (e1, e2, None) }

while_stmt:
  | ti = T_WHILE e = expression T_DO sl = stmt_list T_END_WHILE
  { S.StmWhile(ti, e, sl) }

repeat_stmt:
  | ti = T_REPEAT sl = stmt_list T_UNTIL e = expression T_END_REPEAT
  { S.StmRepeat(ti, sl, e) }
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

dir_var_location_prefix:
  | id = T_IDENTIFIER
  {
    let (v, _) = id in
      if String.equal v "I" then
        S.DirVar.LocI
      else if String.equal v "Q" then
        S.DirVar.LocQ
      else if String.equal v "M" then
        S.DirVar.LocM
      else
        raise (SyntaxError ("Unknown direct variable location prefix: " ^ v))
  }

dir_var_size_prefix:
  | id = T_IDENTIFIER
  {
    let (v, _) = id in
      if String.equal v "X" then
        S.DirVar.SizeX
      else if String.equal v "B" then
        S.DirVar.SizeB
      else if String.equal v "W" then
        S.DirVar.SizeW
      else if String.equal v "D" then
        S.DirVar.SizeD
      else if String.equal v "L" then
        S.DirVar.SizeL
      else
        raise (SyntaxError ("Unknown direct variable size prefix: " ^ v))
  }

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

(* vim: set foldmethod=marker foldlevel=0 foldenable sw=2 tw=120 : *)
