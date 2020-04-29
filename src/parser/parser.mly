%{
  open Core_kernel
  open IECCheckerCore

  module E = Error
  module S = Syntax
  module TI = Tok_info

  let creal_inv (vr, ti) =
    let vv = -1.0 *. vr in
    (vv, ti)

  let creal_conv_fp (vfp, ti) =
    let vv = float_of_string vfp in
    (vv, ti)

  let creal_mk t =
    let (v, ti) = t in
    S.CReal(ti, v)

  let ctime_mk fn t =
    let (v, ti) = t in
    let tv = (fn v) in
    S.CTimeValue(ti, tv)

  let cget_int_val = function
    | S.CInteger (_, v) -> v
    | _ -> raise (E.InternalError "Unknown constant type")

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
%token<IECCheckerCore.Tok_info.t> T_EXIT T_CONTINUE
%token<IECCheckerCore.Tok_info.t> T_RETURN
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

%token <string * IECCheckerCore.Tok_info.t> T_SSTRING_LITERAL
%token <string * IECCheckerCore.Tok_info.t> T_DSTRING_LITERAL

%token <float * IECCheckerCore.Tok_info.t> T_TIME_INTERVAL_D
%token <float * IECCheckerCore.Tok_info.t> T_TIME_INTERVAL_H
%token <float * IECCheckerCore.Tok_info.t> T_TIME_INTERVAL_M
%token <float * IECCheckerCore.Tok_info.t> T_TIME_INTERVAL_S
%token <float * IECCheckerCore.Tok_info.t> T_TIME_INTERVAL_MS
%token <float * IECCheckerCore.Tok_info.t> T_TIME_INTERVAL_US
%token <float * IECCheckerCore.Tok_info.t> T_TIME_INTERVAL_NS
(* }}} *)

(* }}} *)

(* Parser entry point *)
%start <IECCheckerCore.Syntax.iec_library_element list> main

%%
let main :=
  | ~ = library_element_declarations; T_EOF; <>
  | T_EOF; { [] }

(* {{{ Programming model *)
(* This is required because data_type_decl defined as a list in IEC61131-3 grammar. *)
let library_element_declarations :=
  | ll = library_element_declarations; e = library_element_declaration;
  { List.append ll [e] }
  | e = library_element_declaration;
  { [e] }
  | ll = library_element_declarations; dl = data_type_decl;
  {
    let type_defs =
      List.map dl
      ~f:(fun t -> S.mk_pou (`Type t))
    in
    ll @ type_defs
  }
  | dl = data_type_decl;
  {
    List.map dl
    ~f:(fun t -> S.mk_pou (`Type t))
  }

let library_element_declaration :=
  | d = prog_decl;
  { S.mk_pou (`Program d) }
  | d = func_decl;
  { S.mk_pou (`Function d) }
  | d = fb_decl;
  { S.mk_pou (`FunctionBlock d) }
  | d = config_decl;
  { S.mk_pou (`Configuration d) }
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
let constant :=
  | ~ = numeric_literal; <S.Constant>
  | ~ = char_literal; <S.Constant>
  | ~ = time_literal; <S.Constant>
  (* | ~ = bit_str_literal <S.Constant> *)
  | ~ = bool_literal; <S.Constant>

let numeric_literal :=
  | ~ = int_literal; <>
  | ~ = real_literal; <>

let int_literal :=
  | int_type_name; T_SHARP; ~ = signed_int; <>
  | int_type_name; T_SHARP; ~ = binary_int; <>
  | int_type_name; T_SHARP; ~ = octal_int; <>
  | int_type_name; T_SHARP; ~ = hex_int; <>
  | ~ = signed_int; <>
  | ~ = binary_int; <>
  | ~ = octal_int; <>
  | ~ = hex_int; <>

unsigned_int:
  | vi = T_INTEGER
  {
    let (v, ti) = vi in
    S.CInteger(ti, v)
  }

signed_int:
  | i = unsigned_int
  { i }
  | T_PLUS i = unsigned_int
  { i }
  | T_MINUS res = T_INTEGER
  {
    let (v, ti) = res in
    S.CInteger(ti, -v)
  }

binary_int:
  | vi = T_BINARY_INTEGER
  {
    let (v, ti) = vi in
    S.CInteger(ti, v)
  }

octal_int:
  | vi = T_OCTAL_INTEGER
  {
    let (v, ti) = vi in
    S.CInteger(ti, v)
  }

hex_int:
  | vi = T_HEX_INTEGER
  {
    let (v, ti) = vi in
    S.CInteger(ti, v)
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
    S.CBool(ti, v)
  }

(* }}} *)

(* {{{ Table 6 -- String literals / Table 7 -- Two-character combinations in strings *)
let char_literal :=
  | ~ = char_str; <>

let char_str :=
  | ~ = s_byte_char_str; <>
  | ~ = d_byte_char_str; <>

s_byte_char_str:
  | str = T_SSTRING_LITERAL
  {
    let(v, ti) = str in
    S.CString(ti, v)
  }

d_byte_char_str:
  | str = T_DSTRING_LITERAL
  {
    let(v, ti) = str in
    S.CString(ti, v)
  }

(* Implemented in lexer *)
(* s_byte_char_value: *)
(* d_byte_char_value: *)
(* common_char_value: *)
(* }}} *)

(* {{{ Table 8 -- Duration literals / Table 9 -- Datetime literals *)
let time_literal :=
  | ~ = duration; <>
  | ~ = time_of_day; <>
  | ~ = date; <>
  | ~ = date_and_time; <>

duration:
  | time_type_helper v = interval
  { v }
  | time_type_helper T_PLUS v = interval
  { v }
  | time_type_helper T_MINUS v = interval
  {
    match v with
    | S.CTimeValue(ti, tv) ->
        S.CTimeValue(ti, (S.TimeValue.inv tv))
    | _ -> raise (E.InternalError "Unknown constant type")
  }

(* Helper rule for duration. *)
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

let interval :=
  | ~ = days; <>
  | ~ = hours; <>
  | ~ = minutes; <>
  | ~ = seconds; <>
  | ~ = miliseconds; <>
  | ~ = microseconds; <>
  | ~ = nanoseconds; <>

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
      S.CTimeValue(ti, tv)
    in
    ctime_of_timevals hi mi ss
  }

let day_hour :=
  | ~ = unsigned_int; <>

let day_minute :=
  | ~ = unsigned_int; <>

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
      S.CTimeValue(ti, tv)
    in
    cdate_of_timevals cy cmo cd
  }

let year :=
  | ~ = unsigned_int; <>

let month :=
  | ~ = unsigned_int; <>

let day :=
  | ~ = unsigned_int; <>

date_and_time:
  | dt_type_name; T_SHARP; d = date_literal; T_MINUS dt = daytime
  { S.c_add d dt }
  | T_LDATE_AND_TIME T_SHARP; d = date_literal; T_MINUS dt = daytime
  { S.c_add d dt }
(* }}} *)

(* {{{ Table 10 -- Elementary data types *)
(* Note: *_name rules will return an actual Syntax AST type representation.
  This simplifies a processing of derived types specifications. *)
let data_type_access :=
  | ~ = elem_type_name; <S.TyElementary>
  | ~ = derived_type_access; <S.TyDerived>

let elem_type_name :=
  | ~ = numeric_type_name; <>
  | ~ = bit_str_type_name; <>
  | ~ = string_type_name; <>
  | ~ = date_type_name; <>
  | ~ = time_type_name; <>
  (* NOTE: TOD and DT types are not defined as part of elem_type_name in 3rd edition of IEC61131-3
     standard. This seems like a typo because 2nd edition includes these types in date_type which
     has been splitted to few rules in new standard after introducing long types. *)
  | ~ = tod_type_name; <>
  | ~ = dt_type_name; <>

let numeric_type_name :=
  | ~ = int_type_name; <>
  | ~ = real_type_name; <>

let int_type_name :=
  | ~ = sign_int_type_name; <>
  | ~ = unsign_int_type_name; <>

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

let string_type_name :=
  | T_STRING; l = string_type_length; { S.STRING(l) }
  | T_WSTRING; l = string_type_length; { S.WSTRING(l) }
  | T_STRING; { S.STRING(Config.max_string_len) }
  | T_WSTRING; { S.WSTRING(Config.max_string_len) }
  | T_CHAR; { S.CHAR(1) }
  | T_WCHAR; { S.WCHAR(1) }

(* Helper rule for string_type_name *)
string_type_length:
  | T_LBRACK c = unsigned_int T_RBRACK
  {
    match c with
    | CInteger(_, v) -> v
    | _ -> raise (E.SyntaxError "Invalid length of string")
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

let bit_str_type_name :=
  | ~ = bool_type_name; <>
  | ~ = multibits_type_name; <>

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
let derived_type_access :=
  | ~ = single_elem_type_access; <S.DTyUseSingleElement>
  (* | ~ = array_type_access; <> *)
  (* | ~ = struct_type_access; <> *)
  | ty_def = string_type_access; <S.DTyUseStringType>
  (* | ~ = class_type_access; <> *)
  (* | ~ = ref_type_access; <> *)
  (* | ~ = interface_type_access; <> *)

let string_type_access :=
  | ~ = string_type_name; <>

let single_elem_type_access :=
  | ~ = simple_type_access; <S.DTySpecSimple>
  (* | ~ = subrange_type_access; <> *)
  (* | ~ = enum_type_access; <> *)

let simple_type_access :=
  | ~ = simple_type_name; <>

(* let subrange_type_access := *)
  (* | ~ = subrange_type_name; <> *)

(* enum_type_access: *)
  (* | id = T_IDENTIFIER *)
  (* { } *)

(* array_type_access: *)
  (* | id = T_IDENTIFIER *)
  (* { } *)

(* struct_type_access:
  | id = T_IDENTIFIER
  { } *)

let simple_type_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

let subrange_type_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

(** Helper rule for str_type_decl *)
let str_type_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

let enum_type_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

let array_type_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

let struct_type_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

let data_type_decl :=
  | T_TYPE; ~ = list(type_decl); T_END_TYPE; <>

let type_decl :=
  | ~ = simple_type_decl; T_SEMICOLON; <>
  | ~ = subrange_type_decl; T_SEMICOLON; <>
  (* | ~ = enum_type_decl; T_SEMICOLON; <> *)
  (* | ~ = array_type_decl; T_SEMICOLON; <> *)
  (* | ~ = struct_type_decl; T_SEMICOLON; <> *)
  | ~ = str_type_decl; T_SEMICOLON; <>
  (* | ~ = ref_type_decl; T_SEMICOLON; <> *)

(* Helper rules to resolve shift/reduce conflicts in types declaration *)
let type_decl_helper :=
  | name_id = T_IDENTIFIER; T_COLON; ty = type_spec_helper;
  { let name, _ = name_id in (name, ty) }
(* Same as elem_type_name, but with length of strings. *)
let type_spec_helper :=
  | ~ = numeric_type_name; <>
  | ~ = bit_str_type_name; <>
  | ~ = string_type_name; <>
  | ~ = date_type_name; <>
  | ~ = time_type_name; <>
  | ~ = tod_type_name; <>
  | ~ = dt_type_name; <>

(* Implementation is modified to avoid shift/reduce conflicts *)
let simple_type_decl :=
  (* | ty_decl_name = simple_type_name; T_COLON; init_vals = simple_spec_init; *)
  | name_type = type_decl_helper; ci = assign_constant_expr?;
  {
    let (ty_name, ty_decl) = name_type in
    let ty_spec = S.DTySpecElementary(ty_decl) in
    S.DTyDeclSingleElement(ty_name, ty_spec, ci)
  }
  | ty_name_id = T_IDENTIFIER; T_COLON; ty_decl = simple_type_access; ci = assign_constant_expr?;
  {
    let ty_name, _ = ty_name_id in
    let ty_spec = S.DTySpecSimple(ty_decl) in
    S.DTyDeclSingleElement(ty_name, ty_spec, ci)
  }

let simple_spec_init :=
  | ty = simple_spec; ci = assign_constant_expr?;
  { (ty, ci) }

(* Helper rule for simple_spec_init *)
let assign_constant_expr :=
  | T_ASSIGN; ~ = constant_expr; <>

let simple_spec :=
  | ~ = elem_type_name; <S.DTySpecElementary>
  | ~ = simple_type_access; <S.DTySpecSimple>

(* Implementation is modified to avoid shift/reduce conflicts *)
let subrange_type_decl :=
  | ~ = subrange_spec_init; <>

let subrange_spec_init :=
  | s = subrange_spec; T_ASSIGN; ic = signed_int;
  {
    match s with
    | S.DTyDeclSubrange(ty_name, ty_spec, _) ->
      S.DTyDeclSubrange(ty_name, ty_spec, (cget_int_val ic))
    | _ -> raise (E.InternalError "Unexpected subrange type")
  }
  | ~ = subrange_spec; <>

let subrange_spec :=
  | name_type = type_decl_helper; T_LPAREN; s = subrange; T_RPAREN;
  {
    let (ty_name, ty_decl) = name_type in
    if not (S.ety_is_integer ty_decl) then
        raise (E.SyntaxError "Subrange types must be integer")
    else
      let (_, lb, ub) = s in
      (* According the Standard, the initial value is assigned to lower bound by default. *)
      S.DTyDeclSubrange(ty_name, (ty_decl, lb, ub), lb)
  }
  (* | tn = subrange_type_access;
  { } *)

(* NOTE: In the Standard subrange could be described with constant expressions as
   follows:
     constant_expr '..' constant_expr

   I don't know how to evaluate constant expressions in compile time and have no
   any examples, so I suppose that we always have integer values here.
*)
let subrange :=
  | lbc = signed_int; T_RANGE; ubc = signed_int;
  { ((S.c_get_ti lbc), (cget_int_val lbc), (cget_int_val ubc)) }

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

(* Helper rule for struct_elem_name *)
struct_elem_name_list:
  | n = struct_elem_name;
  { n :: [] }
  | ns = struct_elem_name_list; T_DOT n = struct_elem_name;
  { n :: ns }

(* struct_init: *)

(* struct_elem_init: *)

(* NOTE: This rule contains a typo in the IEC61131-3 standard.

   The original definition was:
     string_type_name ':' string_type_name ( ':=' char_str )?

   It's just contradicts examples from the Standard text and common sense,
   my replacement based on IEC61131-3 2nd edition is:
     type_name ':' string_type_name ( ':=' char_str )?
*)
let str_type_decl :=
  | name_type = type_decl_helper; init_expr = option(assign_constant_expr);
  {
    let (ty_name, ty_decl) = name_type in
    if not (S.ety_is_string ty_decl) then
        raise (E.SyntaxError "Expected string type")
    else
      let ty_spec = S.DTySpecElementary(ty_decl) in
      (* Check optional initial value *)
      let initial_value =
        match init_expr with
          | Some(expr) ->
            begin
              match expr with
              | S.Constant(c) ->
                match c with
                  | S.CString(str, _) -> init_expr
                  | _ -> raise (E.InternalError "Unexpected constant type")
              | _ -> raise (E.InternalError "Unexpected expression")
            end
          | None -> None
      in
      S.DTyDeclSingleElement(ty_name, ty_spec, initial_value)
  }
(* }}} *)

(* {{{ Table 16 -- Direct variables *)
direct_variable:
  | T_PERCENT loc = dir_var_location_prefix; sz = dir_var_size_prefix; pcs = list(unsigned_int);
  {
    let pvals = List.map ~f:(fun c -> cget_int_val c) pcs in
    S.VarDecl.SpecDirect(None)
  }
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

let var_decl_init :=
  | vs = variable_list; T_COLON; simple_spec_init;
  {
    List.map vs ~f:(fun (n, ti) -> (
      let v = S.SymVar.create n ti in
      S.SymVar(v)))
  }

(* Helper rule for var_decl_init *)
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

(* Helper rule for var_decl *)
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

(* Helper rule for temp_var_decls_list. *)
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

(* Helper rule for global_var_name *)
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

let global_var_decl :=
  | ~ = global_var_spec; T_COLON; loc_var_spec_init; <>

(* Helper rule for gloval_var_decl *)
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

let loc_var_spec_init :=
  | ~ = simple_spec_init; <>

let located_at :=
  | T_AT; ~ = direct_variable; <>

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

(* Implemented in semantic analysis *)
(* std_func_name: *)

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

(* Helper rule for func_decl *)
let function_ty :=
  | ~ = elem_type_name; <S.TyElementary>
  | ~ = derived_type_access; <S.TyDerived>

(* Helper rule for func_decl *)
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

let io_var_decls :=
  | ~ = input_decls; <>
  | ~ = output_decls; <>
  | ~ = in_out_decls; <>

let func_var_decls :=
  | ~ = external_var_decls; <>
  | ~ = var_decls; <>

let func_body :=
  | ~ = stmt_list; <>
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

(* Helper rule for fb_decl *)
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
let fb_io_var_decls :=
  | ~ = input_decls; <>
  | ~ = output_decls; <>
  | ~ = in_out_decls; <>

(* fb_input_decls: *)
(* fb_input_decl: *)
(* fb_output_decls: *)
(* fb_output_decl: *)

let other_var_decls :=
  | ~ = retain_var_decls; <>
  | ~ = no_retain_var_decls; <>
  | ~ = loc_partly_var_decl; <>

no_retain_var_decls:
  | T_VAR T_NON_RETAIN vs = var_decl_init_list T_END_VAR
  {
    let vsr = List.rev vs in
    List.map ~f:(fun v -> (
      let s = S.VarDecl.Spec(Some S.VarDecl.QNonRetain) in
      S.VarDecl.create v s
    )) vsr
  }

let fb_body :=
  | ~ = stmt_list; <>

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

let prog_type_access :=
  | ~ = prog_type_name; <>

let prog_access_decls :=
  | T_VAR_ACCESS; ~ = list(prog_access_decl); T_END_VAR; <>

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

(* Helper rule for config_decl *)
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

let resource_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

let access_decls :=
  | T_VAR_ACCESS; ~ = list(access_decl); T_END_VAR; <>

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

global_var_access:
    | out = global_var_name;
    { S.SymVar(out) }
    | separated_list(T_DOT, resource_name); out = global_var_name;
    { S.SymVar(out) }
    | out = global_var_name; struct_elem_name_list
    { S.SymVar(out) }
    | separated_list(T_DOT, resource_name); out = global_var_name; struct_elem_name_list
    { S.SymVar(out) }

let access_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

let prog_output_access :=
  | p = prog_name; T_DOT; v = symbolic_variable;
  { let vv = S.SymVar(v) in let n = S.ProgramConfig.get_name p in (n, vv) }

let prog_name :=
  | id = T_IDENTIFIER;
  { let (name, ti) = id in S.ProgramConfig.create name ti }

(* Helper rule for prog_name *)
let prog_name_qual :=
  | ~ = prog_name; <>
  | p = prog_name; T_RETAIN;
  { S.ProgramConfig.set_qualifier p S.ProgramConfig.QRetain }
  | p = prog_name; T_NON_RETAIN;
  { S.ProgramConfig.set_qualifier p S.ProgramConfig.QNonRetain }

access_direction:
  | T_READ_WRITE
  {  }
  | T_READ_ONLY
  {  }

task_config:
  | T_TASK t = task_name; i = task_init; T_SEMICOLON
  { t }

let task_name :=
  | id = T_IDENTIFIER;
  { let (name, ti) = id in S.Task.create name ti }

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
    | T_PROGRAM pc = prog_name_qual; T_COLON prog_type_access; T_LBRACE; cvs = separated_list(T_COMMA, prog_conf_elem); T_RBRACE
    {
        let pc = S.ProgramConfig.set_conn_vars pc cvs in
        pc
    }
    | T_PROGRAM pc = prog_name_qual; T_WITH; t = task_name; T_COLON ty = prog_type_name;
    {
        let pc = S.ProgramConfig.set_task pc t in
        pc
    }
    | T_PROGRAM pc = prog_name_qual; T_WITH; t = task_name; T_COLON ty = prog_type_name; T_LBRACE; cvs = separated_list(T_COMMA, prog_conf_elem); T_RBRACE
    {
        let pc = S.ProgramConfig.set_conn_vars pc cvs in
        let pc = S.ProgramConfig.set_task pc t in
        pc
    }

(* Helper rule for prog_config *)
prog_config_list:
    | pc = prog_config; T_SEMICOLON
    { pc :: [] }
    | pcs = prog_config_list; pc = prog_config T_SEMICOLON
    { pc :: pcs }

(* prog_conf_elems: *)

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

  let data_sink :=
    | ~ = global_var_access; <>
    | ~ = direct_variable; <>

config_init:
    | T_VAR_CONFIG is = list(config_inst_init) T_END_VAR
    { is }

(* TODO: This is not complete *)
config_inst_init:
    | resource_name; T_DOT; prog_name; T_SEMICOLON
    {  }
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
   in a Standard text, so I don't know what does this means.
*)
let constant_expr :=
  | ~ = constant; <>

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

let variable_access :=
  | v = variable_expr; option(multibit_part_access);
  { v }

(* Helper rule for variable_access.
 * This is required to avoid shift/reduce conflict with identifier from func_name rule. *)
let variable_expr :=
  | id = T_IDENTIFIER;
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
  | f = func_access T_LPAREN stmts = separated_list(T_COMMA, param_assign) T_RPAREN
  {
    let ti = S.Function.get_ti f in
    S.StmFuncCall(ti, f, stmts)
  }

stmt_list:
  | s = stmt T_SEMICOLON
  { s :: [] }
  | sl = stmt_list; s = stmt T_SEMICOLON
  { s :: sl }

let stmt :=
  | ~ = assign_stmt; <>
  | ~ = subprog_ctrl_stmt; <>
  | ~ = selection_stmt; <>
  | ~ = iteration_stmt; <>

assign_stmt:
  | v = variable T_ASSIGN e = expression
  {
    let ti = S.vget_ti v in
    S.StmAssign(ti, v, e)
  }

(* assignment_attempt: *)

(* invocation: *)

let subprog_ctrl_stmt :=
  | ~ = func_call; <>
  (* | invocation
  {  } *)
  (* | T_SUPER T_LPAREN T_RPAREN
  {  } *)
  | ~ = T_RETURN; <S.StmReturn>

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

let selection_stmt :=
  | ~ = if_stmt; <>
  | ~ = case_stmt; <>

if_stmt:
  | ti = T_IF cond = expression T_THEN if_stmts = stmt_list T_END_IF
  { S.StmIf(ti, cond, if_stmts, [], []) }
  | ti = T_IF cond = expression T_THEN if_stmts = stmt_list; T_ELSE else_stmts = stmt_list T_END_IF
  { S.StmIf(ti, cond, if_stmts, [], else_stmts) }
  | ti = T_IF cond = expression T_THEN if_stmts = stmt_list; elsif_stmts = if_stmt_elsif_list T_END_IF
  { S.StmIf(ti, cond, if_stmts, elsif_stmts, []) }
  | ti = T_IF cond = expression T_THEN if_stmts = stmt_list; elsif_stmts = if_stmt_elsif_list; T_ELSE else_stmts = stmt_list T_END_IF
  { S.StmIf(ti, cond, if_stmts, elsif_stmts, else_stmts) }

(* Helper rule for if_stmt *)
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
  | ti = T_CASE e = expression T_OF css = list(case_selection) T_ELSE sl = stmt_list T_END_CASE
  { S.StmCase(ti, e, css, sl) }
  | ti = T_CASE e = expression T_OF css = list(case_selection) T_END_CASE
  { S.StmCase(ti, e, css, []) }

case_selection:
  | cl = case_list T_COLON sl = stmt_list
  { {S.case = cl; S.body = sl} }

let case_list :=
  | ~ = separated_list(T_COMMA, case_list_elem); <>

let case_list_elem :=
  | specs = subrange;
  { let (ti, lb, ub) = specs in S.Constant(S.CRange(ti, lb, ub)) }
  | ~ = constant_expr; <>

let iteration_stmt :=
  | ~ = for_stmt; <>
  | ~ = while_stmt; <>
  | ~ = repeat_stmt; <>
  | ~ = T_EXIT; <S.StmExit>
  | ~ = T_CONTINUE; <S.StmContinue>

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
 * Helper rules and symbols which doesn't defined in standard grammar explicitly *)

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
        raise (E.SyntaxError ("Unknown direct variable location prefix: " ^ v))
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
        raise (E.SyntaxError ("Unknown direct variable size prefix: " ^ v))
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
