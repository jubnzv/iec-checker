%{
  open Core_kernel
  open IECCheckerCore

  module TI = Tok_info

  exception SyntaxError of string
  (* Raised when parser found simple semnatics errors while constructing AST. *)
  exception SemanticError of string

  let rec list_flatten = function
    | [] -> []
    | [] :: t -> list_flatten t
    | (x::y) :: t -> x :: (list_flatten (y::t))

  let startswith s1 s2 =
    let len1 = String.length s1 and len2 = String.length s2 in
    if len1 < len2 then false
    else
      let sub = String.sub s1 ~pos:(0) ~len:(len2) in
      String.equal sub s2

  let creal_inv (vr, ti) =
    let vv = -1.0 *. vr in
    (vv, ti)

  let creal_conv_fp (vfp, ti) =
    let vv = float_of_string vfp in
    (vv, ti)

  let creal_mk t =
    let (v, ti) = t in
    Syntax.CReal(ti, v)

  let ctime_mk fn t =
    let (v, ti) = t in
    let tv = (fn v) in
    Syntax.CTimeValue(ti, tv)

  let c_get_int = function
    | Syntax.CInteger (_,v) -> Some(v)
    | _ -> None

  let c_get_int_exn = function
    | Syntax.CInteger (_, v) -> v
    | _ -> assert false

  let mk_global_decl (sym_var : Syntax.SymVar.t) =
    let var = Syntax.VarUse.create_sym sym_var Syntax.VarUse.Elementary in
    let attr = Syntax.VarDecl.VarGlobal(None) in
    let var_decl = Syntax.VarDecl.create var None in
    let var_decl = Syntax.VarDecl.set_attr var_decl attr in
    var_decl

  let mk_var_use name ti =
    let sv = Syntax.SymVar.create name ti in
    let var_use = Syntax.VarUse.create_sym sv Syntax.VarUse.Elementary in
    var_use

  let mk_var_use_sym sv =
    let var_use = Syntax.VarUse.create_sym sv Syntax.VarUse.Elementary in
    var_use

  let mk_var_use_dir dv =
    let var_use = Syntax.VarUse.create_dir dv Syntax.VarUse.Elementary in
    var_use
%}

(* {{{ Tokens *)
(* {{{ Common *)
%token T_ASSIGN     ":="
%token T_SENDTO     "=>"
%token T_DOT        "."
%token T_NULL
%token T_REF
%token T_REF_TO
%token T_ASSIGN_REF "?="
%token T_DEREF      "^"
%token T_GT         ">"
%token T_LT         "<"
%token T_GE         ">="
%token T_LE         "<="
%token T_MUL        "*"
%token T_MOD        "MOD"
%token T_DIV        "/"
%token T_POW        "**"
%token T_EQ         "="
%token T_NEQ        "<>"
%token T_NOT        "NOT"
%token T_RANGE      ".."
%token T_LBRACE     "{"
%token T_RBRACE     "}"
%token T_LPAREN     "("
%token T_RPAREN     ")"
%token T_LBRACK     "["
%token T_RBRACK     "]"
%token T_SHARP      "#"
%token T_COLON      ":"
%token T_PLUS       "+"
%token T_MINUS      "-"
%token T_SEMICOLON  ";"
%token T_COMMA      ","
%token T_AT         "AT"
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
%token T_STRUCT T_OVERLAP T_END_STRUCT
%token T_ARRAY
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

%token <IECCheckerCore.Syntax.DirVar.t> T_DIR_VAR

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
      ~f:(fun t -> Syntax.mk_pou (`Type t))
    in
    ll @ type_defs
  }
  | dl = data_type_decl;
  {
    List.map dl
    ~f:(fun t -> Syntax.mk_pou (`Type t))
  }

let library_element_declaration :=
  | d = prog_decl;
  { Syntax.mk_pou (`Program d) }
  | d = func_decl;
  { Syntax.mk_pou (`Function d) }
  | d = fb_decl;
  { Syntax.mk_pou (`FunctionBlock d) }
  | d = config_decl;
  { Syntax.mk_pou (`Configuration d) }
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
  | c = numeric_literal;
  {
    let ti = Syntax.c_get_ti c in
    Syntax.ExprConstant(ti, c)
  }
  | c = char_literal;
  {
    let ti = Syntax.c_get_ti c in
    Syntax.ExprConstant(ti, c)
  }
  | c = time_literal;
  {
    let ti = Syntax.c_get_ti c in
    Syntax.ExprConstant(ti, c)
  }
  (* | ~ = bit_str_literal <Syntax.ExprConstant> *)
  | c = bool_literal;
  {
    let ti = Syntax.c_get_ti c in
    Syntax.ExprConstant(ti, c)
  }

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

let unsigned_int :=
  | vi = T_INTEGER;
  {
    let (v, ti) = vi in
    Syntax.CInteger(ti, v)
  }

let signed_int :=
  | ~ = unsigned_int; <>
  | T_PLUS; ~ = unsigned_int; <>
  | T_MINUS; res = T_INTEGER;
  {
    let (v, ti) = res in
    Syntax.CInteger(ti, -v)
  }

let binary_int :=
  | vi = T_BINARY_INTEGER;
  {
    let (v, ti) = vi in
    Syntax.CInteger(ti, v)
  }

let octal_int :=
  | vi = T_OCTAL_INTEGER;
  {
    let (v, ti) = vi in
    Syntax.CInteger(ti, v)
  }

let hex_int :=
  | vi = T_HEX_INTEGER;
  {
    let (v, ti) = vi in
    Syntax.CInteger(ti, v)
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

let bool_literal :=
  (* BOOL#<value> rules are implemented in lexer *)
  | vb = T_BOOL_VALUE;
  {
    let (v, ti) = vb in
    Syntax.CBool(ti, v)
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
    Syntax.CString(ti, v)
  }

d_byte_char_str:
  | str = T_DSTRING_LITERAL
  {
    let(v, ti) = str in
    Syntax.CString(ti, v)
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
    | Syntax.CTimeValue(ti, tv) ->
        Syntax.CTimeValue(ti, (Syntax.TimeValue.inv tv))
    | _ -> assert false
  }

(* Helper rule for duration. *)
time_type_helper:
  | v = time_type_name T_SHARP
  { v }
  | v = T_TSHARP
  { Syntax.TIME }
  | v = T_LTSHARP
  { Syntax.LTIME }

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
  | ~ = milliseconds; <>
  | ~ = microseconds; <>
  | ~ = nanoseconds; <>

days:
  | vt = T_TIME_INTERVAL_D
  { ctime_mk (fun v -> Syntax.TimeValue.mk ~d:v ()) vt }
  | vt = T_TIME_INTERVAL_D; v = hours
  { ctime_mk (fun v -> Syntax.TimeValue.mk ~d:v ()) vt |> Syntax.c_add v }

hours:
  | vt = T_TIME_INTERVAL_H
  { ctime_mk (fun v -> Syntax.TimeValue.mk ~h:v ()) vt }
  | vt = T_TIME_INTERVAL_H; v = minutes
  { ctime_mk (fun v -> Syntax.TimeValue.mk ~h:v ()) vt |> Syntax.c_add v }

minutes:
  | vt = T_TIME_INTERVAL_M
  { ctime_mk (fun v -> Syntax.TimeValue.mk ~m:v ()) vt }
  | vt = T_TIME_INTERVAL_M; v = seconds
  { ctime_mk (fun v -> Syntax.TimeValue.mk ~m:v ()) vt |> Syntax.c_add v }

seconds:
  | vt = T_TIME_INTERVAL_S
  { ctime_mk (fun v -> Syntax.TimeValue.mk ~s:v ()) vt }
  | vt = T_TIME_INTERVAL_S; v = milliseconds
  { ctime_mk (fun v -> Syntax.TimeValue.mk ~s:v ()) vt |> Syntax.c_add v }

milliseconds:
  | vt = T_TIME_INTERVAL_MS
  { ctime_mk (fun v -> Syntax.TimeValue.mk ~ms:v ()) vt }
  | vt = T_TIME_INTERVAL_MS; v = microseconds
  { ctime_mk (fun v -> Syntax.TimeValue.mk ~ms:v ()) vt |> Syntax.c_add v }

microseconds:
  | vt = T_TIME_INTERVAL_US
  { ctime_mk (fun v -> Syntax.TimeValue.mk ~us:v ()) vt }
  | vt = T_TIME_INTERVAL_US; v = nanoseconds;
  { ctime_mk (fun v -> Syntax.TimeValue.mk ~us:v ()) vt |> Syntax.c_add v }

nanoseconds:
  | vt = T_TIME_INTERVAL_NS
  { ctime_mk (fun v -> Syntax.TimeValue.mk ~ns:v ()) vt }

time_of_day:
  | tod_type_name; T_SHARP vt = daytime
  { vt }
  | T_LTIME_OF_DAY; T_SHARP vt = daytime
  { vt }

daytime:
  | hi = day_hour T_COLON mi = day_minute T_COLON ss = day_second
  {
    let ctime_of_timevals ch cm ss =
      let ti = Syntax.c_get_ti ch in
      let hf = float_of_int (c_get_int_exn ch) in
      let mf = float_of_int (c_get_int_exn cm) in
      let sf = float_of_string ss in
      let tv = Syntax.TimeValue.mk ~h:hf ~m:mf ~s:sf () in
      Syntax.CTimeValue(ti, tv)
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
      let ti = Syntax.c_get_ti cy in
      let yi = (c_get_int_exn cy) in
      let moi = (c_get_int_exn cmo) in
      let df = float_of_int (c_get_int_exn cd) in
      let tv = Syntax.TimeValue.mk ~y:yi ~mo:moi ~d:df () in
      Syntax.CTimeValue(ti, tv)
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
  { Syntax.c_add d dt }
  | T_LDATE_AND_TIME T_SHARP; d = date_literal; T_MINUS dt = daytime
  { Syntax.c_add d dt }
(* }}} *)

(* {{{ Table 10 -- Elementary data types *)
(* Note: *_name rules will return an actual Syntax AST type representation.
  This simplifies a processing of derived types specifications. *)
let data_type_access :=
  | ~ = elem_type_name; <Syntax.TyElementary>
  | ~ = derived_type_access; <Syntax.TyDerived>
  (* NOTE:
     This is not represtned in the IEC 61131-3 3rd ed. grammar. But there are no generic types at all!
     I believe it should work like in 2nd edition of the Standard. *)
  | ~ = generic_type_name; <Syntax.TyGeneric>

let elem_type_name :=
  | ~ = numeric_type_name; <>
  | ~ = bit_str_type_name; <>
  | ~ = string_type_name; <>
  | ~ = date_type_name; <>
  | ~ = time_type_name; <>
  (* NOTE: TOD and DT types are not defined as part of elem_type_name in 3rd edition of IEC61131-3
     standard. This seems like a typo because 2nd edition includes these types in date_type which
     has been split to few rules in new standard after introducing long types. *)
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
  { Syntax.SINT }
  | T_INT
  { Syntax.INT }
  | T_DINT
  { Syntax.DINT }
  | T_LINT
  { Syntax.LINT }

unsign_int_type_name:
  | T_USINT
  { Syntax.USINT }
  | T_UINT
  { Syntax.UINT }
  | T_UDINT
  { Syntax.UDINT }
  | T_ULINT
  { Syntax.ULINT }

real_type_name:
  | T_REAL
  { Syntax.REAL }
  | T_LREAL
  { Syntax.LREAL }

let string_type_name :=
  | T_STRING; l = string_type_length; { Syntax.STRING(l) }
  | T_WSTRING; l = string_type_length; { Syntax.WSTRING(l) }
  | T_STRING; { Syntax.STRING(Config.max_string_len) }
  | T_WSTRING; { Syntax.WSTRING(Config.max_string_len) }
  | T_CHAR; { Syntax.CHAR(1) }
  | T_WCHAR; { Syntax.WCHAR(1) }

(* Helper rule for [string_type_name] *)
let string_type_length :=
  | T_LBRACK; c = unsigned_int; T_RBRACK;
  { match c with | CInteger(_, v) -> v | _ -> assert false }

time_type_name:
  | T_TIME
  { Syntax.TIME }
  | T_LTIME
  { Syntax.LTIME }

date_type_name:
  | T_DATE
  { Syntax.DATE }
  | T_LDATE
  { Syntax.LDATE }

tod_type_name:
  | T_TIME_OF_DAY
  { Syntax.TIME_OF_DAY }
  | T_TOD
  { Syntax.TOD }
  | T_LTOD
  { Syntax.LTOD }

dt_type_name:
  | T_DATE_AND_TIME
  { Syntax.DATE_AND_TIME }
  | T_LDATE_AND_TIME
  { Syntax.LDATE_AND_TIME }
  | T_DT
  { Syntax.DT }
  | T_LDT
  { Syntax.LDT }

let bit_str_type_name :=
  | ~ = bool_type_name; <>
  | ~ = multibits_type_name; <>

bool_type_name:
  | T_BOOL
  { Syntax.BOOL }

multibits_type_name:
  | T_BYTE
  { Syntax.BYTE }
  | T_WORD
  { Syntax.WORD }
  | T_DWORD
  { Syntax.DWORD }
  | T_LWORD
  { Syntax.LWORD }
(* }}} *)

(* {{{ Table 11 -- Derived data types *)
let derived_type_access :=
  | ~ = single_elem_type_access; <Syntax.DTyUseSingleElement>
  (* | ~ = array_type_access; <> *)
  | ~ = struct_type_access; <Syntax.DTyUseStructType>
  | ~ = string_type_access; <Syntax.DTyUseStringType>
  (* | ~ = class_type_access; <> *)
  (* | ~ = ref_type_access; <> *)
  (* | ~ = interface_type_access; <> *)

let string_type_access :=
  | ~ = string_type_name; <>

let single_elem_type_access :=
  | ~ = simple_type_access; <Syntax.DTySpecSimple>
  (* | ~ = subrange_type_access; <> *)
  | ~ = enum_type_access; <Syntax.DTySpecEnum>

let simple_type_access :=
  | ~ = simple_type_name; <>

(* let subrange_type_access := *)
  (* | ~ = subrange_type_name; <> *)

let enum_type_access :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

(* array_type_access: *)
  (* | id = T_IDENTIFIER *)
  (* { } *)

let struct_type_access :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

let simple_type_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

(* let subrange_type_name :=      *)
(*   | id = T_IDENTIFIER;         *)
(*   { let name, _ = id in name } *)

(** Helper rule for str_type_decl *)
(* let str_type_name :=           *)
(*   | id = T_IDENTIFIER;         *)
(*   { let name, _ = id in name } *)

let enum_type_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

(* let array_type_name :=         *)
(*   | id = T_IDENTIFIER;         *)
(*   { let name, _ = id in name } *)

let struct_type_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

let data_type_decl :=
  | T_TYPE; ~ = list(type_decl); T_END_TYPE; <>

let type_decl :=
  | ~ = simple_type_decl;   option(T_SEMICOLON); <>
  | ~ = subrange_type_decl; option(T_SEMICOLON); <>
  | ~ = enum_type_decl;     option(T_SEMICOLON); <>
  | ~ = array_type_decl;    option(T_SEMICOLON); <>
  | ~ = struct_type_decl;   option(T_SEMICOLON); <>
  | ~ = str_type_decl;      option(T_SEMICOLON); <>
  | ~ = ref_type_decl;      option(T_SEMICOLON); <>

(* Helper rules to resolve shift/reduce conflicts in types declaration *)
(* let type_decl_helper :=                                     *)
(*   | name_id = T_IDENTIFIER; T_COLON; ty = type_spec_helper; *)
(*   { let name, _ = name_id in (name, ty) }                   *)
let type_decl_helper_opt :=
  | name_id = T_IDENTIFIER; T_COLON; ty = option(type_spec_helper);
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
  | name_type = type_decl_helper_opt; ci = optional_assign(constant_expr);
  {
    let (ty_name, ty_decl_opt) = name_type in
    let ty_decl = match ty_decl_opt with
    | Some(v) -> v
    | None -> raise (SyntaxError "Missing type name declaration")
    in
    let ty_spec = Syntax.DTySpecElementary(ty_decl) in
    ty_name, Syntax.DTyDeclSingleElement(ty_spec, ci)
  }
  | ty_name_id = T_IDENTIFIER; T_COLON; ty_decl = simple_type_access; ci = optional_assign(constant_expr);
  {
    let ty_name, _ = ty_name_id in
    let ty_spec = Syntax.DTySpecSimple(ty_decl) in
    ty_name, Syntax.DTyDeclSingleElement(ty_spec, ci)
  }

let simple_spec_init :=
  | ty = simple_spec; const_expr = optional_assign(constant_expr);
  { (ty, const_expr) }

let simple_spec :=
  | ~ = elem_type_name; <Syntax.DTySpecElementary>
  | ~ = simple_type_access; <Syntax.DTySpecSimple>
  (* NOTE: This is not presented in 3rd edition, search my comment
     for generic_type_name bellow. *)
  | ~ = generic_type_name; <Syntax.DTySpecGeneric>

(* Implementation is modified to avoid shift/reduce conflicts *)
let subrange_type_decl :=
  | ~ = subrange_spec_init; <>

let subrange_spec_init :=
  | s = subrange_spec; T_ASSIGN; ic = signed_int;
  {
    match s with
    | ty_name, Syntax.DTyDeclSubrange(ty_spec, _) ->
      ty_name, Syntax.DTyDeclSubrange(ty_spec, (c_get_int_exn ic))
    | _ -> assert false
  }
  | ~ = subrange_spec; <>

let subrange_spec :=
  | name_type = type_decl_helper_opt; T_LPAREN; s = subrange; T_RPAREN;
  {
    let (ty_name, ty_decl_opt) = name_type in
    let ty_decl = match ty_decl_opt with
    | Some(v) -> v
    | None -> raise (SyntaxError "Missing subrange type declaration")
    in
    if not (Syntax.ety_is_integer ty_decl) then
        raise (SemanticError "Subrange types must be integer")
    else
      let (_, lb, ub) = s in
      (* According the Standard, the initial value is assigned to lower bound by default. *)
      ty_name, Syntax.DTyDeclSubrange((ty_decl, lb, ub), lb)
  }
  (* | tn = subrange_type_access;
  { } *)

(* NOTE: In the Standard subrange could be described with constant expressions as
   follows:
     constant_expr '..' constant_expr

   I don't know how to evaluate constant expressions in compile time and have no
   any examples of it, so I suppose, that we always have integer values here.
*)
let subrange :=
  | lbc = signed_int; T_RANGE; ubc = signed_int;
  { (Syntax.c_get_ti lbc), (c_get_int_exn lbc), (c_get_int_exn ubc) }

(* Implementation is modifiet to avoid shift/reduce conflicts with
   declaration of other types. *)
let enum_type_decl :=
  | type_opts = type_decl_helper_opt; specs = named_spec_init;
  {
    let (enum_name, elem_type_name) = type_opts
    and (element_specs, default_value) = specs in
    enum_name, Syntax.DTyDeclEnumType(elem_type_name, element_specs, default_value)
  }
  | enum_name = enum_type_name; T_COLON; specs = enum_spec_init;
  {
    let (element_specs, default_value) = specs in
    enum_name, Syntax.DTyDeclEnumType(None, element_specs, default_value)
  }

let named_spec_init :=
  | T_LPAREN; specs = separated_nonempty_list(T_COMMA, enum_value_spec); T_RPAREN; default_value_opt = optional_assign(enum_value_spec);
  { (specs, default_value_opt) }

let enum_spec_init :=
  | T_LPAREN; values = separated_nonempty_list(T_COMMA, T_IDENTIFIER); T_RPAREN; default_value_opt = optional_assign(enum_value_spec);
  {
    let specs = List.fold_left
      values
      ~init:[]
      ~f:(fun acc (name, _) -> acc @ [{ Syntax.enum_type_name = None; Syntax.elem_name = name; Syntax.initial_value = None }])
    in
    (specs, default_value_opt)
  }
  (* FIXME: How does this should work? Standard defines this rule in BNF grammar, but there are no description
     or examples. Neither in the standard nor in the manufacturers documentation (Beckhoff, Fernhill, CodeSyS).
     I don't know what that means. *)
  (* | enum_name = enum_type_access; initial_value = option(enum_spec_init_inval); { } *)

let enum_value_spec :=
  | id = T_IDENTIFIER;
  {
    let name, _ = id in
    { Syntax.enum_type_name = None; Syntax.elem_name = name; Syntax.initial_value = None; }
  }
  | id = T_IDENTIFIER; T_ASSIGN; initial_value = int_literal;
  {
    let name, _ = id in
    { Syntax.enum_type_name = None; Syntax.elem_name = name; Syntax.initial_value = Some(initial_value); }
  }
  | id = T_IDENTIFIER; T_ASSIGN; initial_value = constant_expr;
  {
    let name, _ = id in
    let initial_const = match initial_value with Syntax.ExprConstant (_, c) -> c | _ -> assert false in
    { Syntax.enum_type_name = None; Syntax.elem_name = name; Syntax.initial_value = Some(initial_const); }
  }

let enum_value :=
  | ty_opt = option(enum_value_opt); id = T_IDENTIFIER;
  {
    let name, _ = id in
    { Syntax.enum_type_name = ty_opt; Syntax.elem_name = name; Syntax.initial_value = None; }
  }

(* Helper rule for enum_value and enum_value_use  *)
let enum_value_opt :=
  | ~ = enum_type_name; T_SHARP; <>

let array_type_decl :=
  | name_type = type_decl_helper_opt; specs = array_spec_init;
  {
    let (name, _) = name_type
    and (subranges, ty, initializer_list) = specs in
    name, Syntax.DTyDeclArrayType(subranges, ty, initializer_list)
  }

let array_spec_init :=
  | specs = array_spec; initializer_list = optional_assign(array_init);
  {
    let (subranges, ty) = specs in
    (subranges, ty, initializer_list)
  }

let array_spec :=
  | T_ARRAY; T_LBRACK; ranges = separated_nonempty_list(T_COMMA, subrange); T_RBRACK; T_OF; ty = data_type_access;
  {
  let subranges = List.fold_left
    ranges
    ~init:[]
    ~f:(fun acc (_,l,u) -> acc @ [{Syntax.arr_lower = l; Syntax.arr_upper = u; }])
  in
  (subranges, ty)
  }
  (* FIXME: Not sure how this should work. See same comment for [enum_spec_init] bellow. *)
  (* | ~ = array_type_access; <> *)

(* Initializer list in the following format: [3, 2, 3(4), 5].
   It will be converted to [3, 2, 4, 4, 4 5]. *)
let array_init :=
  | T_LBRACK; init_list = separated_nonempty_list(T_COMMA, array_elem_init); T_RBRACK;
  { list_flatten init_list }

let array_elem_init :=
  | v = array_elem_init_value;
  { [v] }
  | mul_c = unsigned_int; T_LBRACE; inval = option(array_elem_init_value); T_RBRACE;
  {
    let mk_int_const (i: int) =
      Syntax.CInteger((TI.create_dummy ()), i)
    in
    let (inval_list : 'a list) = match inval with
      | Some v -> [v]
      | None -> [mk_int_const 0]
    in
    let (mul : int) = match mul_c with
      | Syntax.CInteger(_, v) -> v
      | _ -> assert false
    in
    let build_list i n =
      let get_next (pos : int) : Syntax.constant =
        let inval_opt = (List.nth inval_list pos) in
        match inval_opt with Some v -> v | None -> mk_int_const 0
      in
      let rec aux acc i =
        if i <= n then
          aux ((get_next (i+1))::acc) (i+1)
        else (List.rev acc)
      in
    aux [] i
    in
    (build_list 0 mul)
  }

let array_elem_init_value :=
  | expr = constant_expr;
  {
    let c = match expr with Syntax.ExprConstant(_, c) -> c | _ -> assert false in
    c
  }
  (* | ~ = enum_value; <>  *)
  (* | ~ = struct_init; <> *)
  (* | ~ = array_init; <>  *)

let struct_type_decl :=
  | name_type = type_decl_helper_opt; spec = struct_spec;
  {
    let (name, _) = name_type
    and (is_overlap, elem_specs) = spec in
    name, Syntax.DTyDeclStructType(is_overlap, elem_specs)
  }

let struct_spec :=
  | ~ = struct_decl; <>
  (* FIXME: Not sure how this should work. See same comment for [enum_spec_init] bellow. *)
  (* | ~ = struct_spec_init; <> *)

(* let struct_spec_init :=                                                               *)
(*   | type_name = struct_type_access; initial_value_opt = optional_assign(struct_init); *)
(*   { (type_name, initial_value_opt) }                                                  *)

let struct_decl :=
  | T_STRUCT; overlap = option(T_OVERLAP); elems = nonempty_list(struct_elem_decl); T_END_STRUCT;
  { match overlap with Some(v) -> (true, elems) | None -> (false, elems) }

let struct_elem_decl :=
  | name = struct_elem_name; loc = option(struct_elem_loc); T_COLON; ty = struct_elem_ty; inval = optional_assign(struct_elem_init); T_SEMICOLON;
  {
    let opt_inval = match inval with
      | Some (_, v) -> Some(v)
      | None -> None
    in
    { Syntax.struct_elem_name = name;
      Syntax.struct_elem_loc = loc;
      Syntax.struct_elem_ty = ty;
      Syntax.struct_elem_init_value = opt_inval; }
  }

(* Helper rule for [struct_elem_decl] *)
let struct_elem_ty :=
  | ~ = simple_spec; <>

(* Helper rule for struct_elem_decl. *)
let struct_elem_loc :=
  | T_AT; ~ = T_DIR_VAR; <>

let struct_elem_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

let struct_init :=
  | T_LPAREN; ~ = separated_nonempty_list(T_COMMA, struct_elem_init); T_RPAREN; <>

let struct_elem_init :=
  (* | name = struct_elem_name; T_ASSIGN; value = constant_expr; *)
  | value = constant_expr;
  {
    let c = match value with
      | Syntax.ExprConstant (_, c) -> c
      | _ -> assert false
    in
    ("" (* name *), Syntax.StructElemInvalConstant(c))
  }
  | name = struct_elem_name; T_ASSIGN; value = enum_value;
  { (name, Syntax.StructElemInvalEnum(value)) }
  (* | name = struct_elem_name; T_ASSIGN; value = array_init; {} *)
  (* | name = struct_elem_name; T_ASSIGN; value = struct_spec_init; *)
  (* {                                                              *)
  (*   let (inval, _) = value in                                    *)
  (*   (name, Syntax.StructElemInvalStruct(inval))                       *)
  (* }                                                              *)

(* NOTE: This rule contains a typo in the IEC61131-3 standard.

   The original definition was:
     string_type_name ':' string_type_name ( ':=' char_str )?

   It's just contradicts examples from the Standard text and common sense,
   my replacement based on IEC61131-3 2nd edition is:
     type_name ':' string_type_name ( ':=' char_str )?
*)
let str_type_decl :=
  | name_type = type_decl_helper_opt; init_expr = optional_assign(constant_expr);
  {
    let (ty_name, ty_decl_opt) = name_type in
    let ty_decl = match ty_decl_opt with
    | Some(v) -> v
    | None -> raise (SyntaxError "Missing string type name declaration")
    in
    if not (Syntax.ety_is_string ty_decl) then
        raise (SemanticError "Expected string type")
    else
      let ty_spec = Syntax.DTySpecElementary(ty_decl) in
      (* Check optional initial value *)
      let initial_value =
        match init_expr with
          | Some(expr) ->
            begin
              match expr with
              | Syntax.ExprConstant(_,c) -> begin
                match c with
                  | Syntax.CString _ -> init_expr
                  | _ -> assert false
              end
              | _ -> assert false
            end
          | None -> None
      in
      ty_name, Syntax.DTyDeclSingleElement(ty_spec, initial_value)
  }
(* }}} *)

(* {{{ Table 16 -- Direct variables *)
(* Lexer does all work. *)
let direct_variable :=
  | ~ = T_DIR_VAR; <>
(* }}} *)

(* {{{ Table 12 -- Operations with references *)
let ref_type_decl :=
  | name_type = type_decl_helper_opt; spec = ref_spec_init;
  {
    let (ref_name, _) = name_type
    and (num_of_refs, ty, inval_opt) = spec in
    ref_name, Syntax.DTyDeclRefType(num_of_refs, ty, inval_opt)
  }

(* There is typo in Standard. I believe that '; =' means ':=' *)
let ref_spec_init :=
  | specs = ref_spec; inval_opt = optional_assign(ref_value);
  { let (num_of_refs, ty) = specs in (num_of_refs, ty, inval_opt) }

let ref_spec :=
  | refs = nonempty_list(T_REF_TO); ty = data_type_access;
  { ((List.length refs), ty) }

(* ref_type_name: *)

(* ref_type_access: *)

let ref_name :=
  | id = T_IDENTIFIER;
  { let name, ti = id in (name, ti) }

let ref_value :=
  | T_NULL; { Syntax.RefNull }
  | ~ = ref_addr; <>

(* NOTE: They just forgot to add ref_assign rule to the BNF grammar. ¯\_(ツ)_/¯
   There are also no examples how to use it. I have no idea what they mean, so
   I drop it away from this rule. *)
let ref_addr :=
  | T_REF; T_LPAREN; ~ = ref_addr_value; T_RPAREN; <>

(* Helper symbol for [ref_addr] *)
let ref_addr_value :=
  | ~ = symbolic_variable; <Syntax.RefSymVar>
  (* | ~ = fb_instance_name; <Syntax.RefFBInstance> *)
  (* | ~ = class_instance_name; <> *)

let ref_deref :=
  | name_ti = ref_name; derefs = nonempty_list(T_DEREF);
  {
    let name, ti = name_ti in
    let var = mk_var_use name ti in
    let var_expr = Syntax.ExprVariable(ti, var) in
    List.fold_left
      derefs
      ~init:var_expr
      ~f:(fun prev_expr _ -> Syntax.ExprUn(ti, Syntax.DEREF, prev_expr))
  }
(* }}} *)

(* {{{ Table 13 -- Variables declaration / Table 14 -- Variables initialization *)
let variable :=
  | v = direct_variable;
  { mk_var_use_dir v }
  | sv = symbolic_variable;
  { mk_var_use_sym sv }

let symbolic_variable :=
  | out = variable_name;
  { let (name, ti) = out in Syntax.SymVar.create name ti }
  | ~ = multi_elem_var; <>

let var_access :=
  | ~ = variable_name; <>
  (* | ~ = ref_deref; <> *)

let variable_name :=
  | id = T_IDENTIFIER;
  { let (name, ti) = id in (name, ti) }

let multi_elem_var :=
  | name_ti = var_access; T_LBRACK; sub_list = subscript_list; T_RBRACK;
  {
   let (name, ti) = name_ti in
   let sv = Syntax.SymVar.create name ti in
   (* Add array indexes to variable. *)
   List.fold_left
     sub_list
     ~init:sv
     ~f:(fun acc_sv e -> begin
         match e with
         | Syntax.ExprConstant (_,c) -> begin
           let val_opt = c_get_int c in
           match val_opt with
           | Some v -> Syntax.SymVar.add_array_index acc_sv v
           | None -> Syntax.SymVar.add_array_index_opaque acc_sv
         end
         | _ -> Syntax.SymVar.add_array_index_opaque acc_sv
     end)
  }
  (* | ~ = var_access; sub_list = nonempty_list(struct_variable); *)

let subscript_list :=
 | s = subscript;
 { [s] }
 | vs = subscript_list; T_COMMA; s = subscript;
 { List.append vs [s] }

let subscript :=
  | e = expression; <>

(* struct_variable: *)

(* struct_elem_select: *)

(* edge_decl: *)

(* array_conformand: *)

(* array_conform_decl: *)

(* struct_var_decl_init: *)

(* fb_decl_no_init: *)

(* fb_decl_init: *)

(* let fb_name :=                     *)
(*   | id = T_IDENTIFIER;             *)
(*   {                                *)
(*     let (name, ti) = id in         *)
(*     Syntax.FunctionBlock.create name ti *)
(*   }                                *)

let fb_instance_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

let global_var_name :=
  | id = T_IDENTIFIER;
  {
    let (n, ti) = id in
    Syntax.SymVar.create n ti
  }

(* Helper rule for global_var_name *)
let global_var_list :=
  | out = global_var_name;
  { [(mk_global_decl out)] }
  | vds = global_var_list; T_COMMA; out = global_var_name;
  {
    let vd = mk_global_decl out in
    vd :: vds
  }

let global_var_decl :=
  | ~ = global_var_spec; T_COLON; loc_var_spec_init; <>

(* Helper rule for gloval_var_decl *)
let global_var_decl_list :=
  | ~ = global_var_decl; T_SEMICOLON; <>
  | vs = global_var_decl_list; v = global_var_decl; T_SEMICOLON;
  { List.append vs v }

let global_var_spec :=
  | ~ = global_var_list; <>
  | sv = global_var_name; l = located_at;
  {
    let var = mk_var_use_sym sv in
    let var_decl = Syntax.VarDecl.create var None in
    let attr = Syntax.VarDecl.VarGlobal(None) in
    [(Syntax.VarDecl.set_attr var_decl attr)]
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

let var_spec :=
  | ~ = simple_spec; option(var_spec_index); <>
  (* | ~ = array_spec; <> *)
  (* | ~ = struct_type_access; <> *)

(* Helper rule for var_spec *)
let var_spec_index :=
  | T_LBRACK; c = unsigned_int; T_RBRACK;
  { c_get_int_exn c }

(* }}} *)

(* {{{ Table 19 -- Function declaration *)
let func_name :=
  | id = T_IDENTIFIER;
  {
    let (name, ti) = id in
    Syntax.Function.create name ti
  }

let func_access :=
  (* | namespace_name func_name
  {  } *)
  | ~ = func_name; <>

(* Implemented in semantic analysis *)
(* std_func_name: *)

(* derived_func_name:
  | id = T_IDENTIFIER
  {
    let (name, ti) = id in
    Syntax.Function.create name ti
  } *)

let func_decl :=
  | T_FUNCTION; id = func_name; T_COLON; ret_ty = function_ty; vs = var_decls; body = func_body; T_END_FUNCTION;
  {
    {
      Syntax.id = id;
      Syntax.return_ty = ret_ty;
      Syntax.variables = vs;
      Syntax.statements = body;
    }
  }
  | T_FUNCTION; id = func_name; T_COLON; ret_ty = function_ty; body = func_body; T_END_FUNCTION;
  {
    {
      Syntax.id = id;
      Syntax.return_ty = ret_ty;
      Syntax.variables = [];
      Syntax.statements = body;
    }
  }

(* Helper rule for func_decl *)
let function_ty :=
  | ~ = elem_type_name; <Syntax.TyElementary>
  | ~ = derived_type_access; <Syntax.TyDerived>

let func_body :=
  | ~ = stmt_list; <>
  (* Allow empty body of function with {} *)
  | T_LBRACE; T_RBRACE; { [] }
(* }}} *)

(* {{{ Table 40 -- Function block definition / Table 41 -- Function block instantiation *)
(* fb_type_name:
  | f = fb_name
  { f } *)

(* fb_type_access: *)

(* std_fb_name: *)

let derived_fb_name :=
  | id = T_IDENTIFIER;
  {
    let (name, ti) = id in
    Syntax.FunctionBlock.create name ti
  }

let fb_decl :=
  | T_FUNCTION_BLOCK; id = derived_fb_name; T_END_FUNCTION_BLOCK;
  { { Syntax.id = id; Syntax.variables = []; Syntax.statements = [] } }
  | T_FUNCTION_BLOCK; id = derived_fb_name; vds = var_decls; T_END_FUNCTION_BLOCK;
  { { Syntax.id = id; Syntax.variables = vds; Syntax.statements = [] } }
  | T_FUNCTION_BLOCK; id = derived_fb_name; ss = fb_body; T_END_FUNCTION_BLOCK;
  { { Syntax.id = id; Syntax.variables = []; Syntax.statements = ss } }
  | T_FUNCTION_BLOCK; id = derived_fb_name; vds = var_decls; ss = fb_body; T_END_FUNCTION_BLOCK;
  { { Syntax.id = id; Syntax.variables = vds; Syntax.statements = ss } }

let fb_body :=
  | ~ = stmt_list; <>
  (* Allow empty body of function block with {} *)
  | T_LBRACE; T_RBRACE; { [] }

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
let prog_decl :=
  | T_PROGRAM; n = prog_type_name; var_decls_opt = option(var_decls); ss = fb_body; T_END_PROGRAM;
  {
    let vds = match var_decls_opt with
      | Some v -> v
      | None -> []
    in
    { Syntax.is_retain = false;
      Syntax.name = n;
      Syntax.variables = vds;
      Syntax.statements = ss }
  }

let prog_type_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

let prog_type_access :=
  | ~ = prog_type_name; <>
(* }}} *)

(* {{{ Table 54-61 -- SFC *)
(* }}} *)

(* {{{ Table 62 -- Configuration elements *)
let config_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

let resource_type_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

let config_decl :=
  (* Without global variables *)
  | T_CONFIGURATION; name = config_name; rd = resource_decls; T_END_CONFIGURATION;
  { { Syntax.name = name; Syntax.resources = rd; Syntax.variables = []; Syntax.access_paths = [] } }
  (* With global variables *)
  | T_CONFIGURATION; name = config_name; vds = var_global_decl; rd = resource_decls; T_END_CONFIGURATION;
  { { Syntax.name = name; Syntax.resources = rd; Syntax.variables = vds; Syntax.access_paths = [] } }
  | T_CONFIGURATION; name = config_name; vds = var_global_decl; rd = resource_decls; var_decls; T_END_CONFIGURATION;
  { { Syntax.name = name; Syntax.resources = rd; Syntax.variables = vds; Syntax.access_paths = [] } }
  | T_CONFIGURATION; name = config_name; vds = var_global_decl; rd = resource_decls; access_decls; T_END_CONFIGURATION;
  { { Syntax.name = name; Syntax.resources = rd; Syntax.variables = vds; Syntax.access_paths = [] } }
  | T_CONFIGURATION; name = config_name; vds = var_global_decl; rd = resource_decls; access_decls; var_decls; T_END_CONFIGURATION;
  { { Syntax.name = name; Syntax.resources = rd; Syntax.variables = vds; Syntax.access_paths = [] } }

(* Helper rule for config_decl *)
let resource_decls :=
  | r = single_resource_decl; {[r]}
  | ~ = list(resource_decl); <>

(* Return Syntax.resource_decl *)
let resource_decl :=
  | T_RESOURCE; n = resource_name; T_ON; resource_type_name; rc = single_resource_decl; T_END_RESOURCE;
  { { Syntax.name = Some n; Syntax.tasks = rc.tasks; Syntax.variables = []; Syntax.programs = rc.programs } }
  | T_RESOURCE; n = resource_name; T_ON; resource_type_name; vs = var_global_decl; rc = single_resource_decl; T_END_RESOURCE;
  { { Syntax.name = Some n; Syntax.tasks = rc.tasks; Syntax.variables = vs; Syntax.programs = rc.programs } }

(* Return Syntax.resource_decl *)
let single_resource_decl :=
  | ts = list(task_config); pis = prog_config_list;
  { { Syntax.name = None; Syntax.tasks = ts; Syntax.variables = []; Syntax.programs = pis } }
  | pis = prog_config_list;
  { { Syntax.name = None; Syntax.tasks = []; Syntax.variables = []; Syntax.programs = pis } }

let resource_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

let access_decls :=
  | T_VAR_ACCESS; ~ = list(access_decl); T_END_VAR; <>

let access_decl :=
  | access_name; T_COLON; access_path; T_COLON; data_type_access; T_SEMICOLON;
  {  }
  | access_name; T_COLON; access_path; T_COLON; data_type_access; access_direction; T_SEMICOLON;
  {  }

let access_path :=
  | resource_name; T_DOT; direct_variable;
  {  }
  (* | dv = direct_variable
  {  } *)
  (* | resource_name; T_DOT; pn = prog_name; T_DOT fn = fb_name; T_DOT; v = symbolic_variable
  {  } *)
  | resource_name; T_DOT; prog_name; T_DOT; symbolic_variable;
  {  }
  (* | pn = prog_name; T_DOT fn = fb_name; T_DOT; v = symbolic_variable
  {  } *)
  | prog_name; T_DOT; symbolic_variable;
  {  }

let global_var_access :=
    | sv = global_var_name;
    { mk_var_use_sym sv }
    | separated_list(T_DOT, resource_name); sv = global_var_name;
    { mk_var_use_sym sv }
    (* | out = global_var_name; list(struct_elem_name); *)
    (* { Syntax.SymVar(out) }                                *)
    (* | separated_list(T_DOT, resource_name); out = global_var_name; list(struct_elem_name); *)
    (* { Syntax.SymVar(out) }                                                                      *)

let access_name :=
  | id = T_IDENTIFIER;
  { let name, _ = id in name }

let prog_output_access :=
  | p = prog_name; T_DOT; sv = symbolic_variable;
  {
    let var_use = mk_var_use_sym sv in
    let config_name = Syntax.ProgramConfig.get_name p in
    (config_name, var_use)
  }

let prog_name :=
  | id = T_IDENTIFIER;
  { let (name, ti) = id in Syntax.ProgramConfig.create name ti }

(* Helper rule for prog_name *)
let prog_name_qual :=
  | ~ = prog_name; <>
  | p = prog_name; T_RETAIN;
  { Syntax.ProgramConfig.set_qualifier p Syntax.ProgramConfig.QRetain }
  | p = prog_name; T_NON_RETAIN;
  { Syntax.ProgramConfig.set_qualifier p Syntax.ProgramConfig.QNonRetain }

let access_direction :=
  | T_READ_WRITE;
  {  }
  | T_READ_ONLY;
  {  }

let task_config :=
  | T_TASK; ~ = task_name; task_init; T_SEMICOLON; <>

let task_name :=
  | id = T_IDENTIFIER;
  { let (name, ti) = id in Syntax.Task.create name ti }

let task_init :=
  | T_LPAREN; T_SINGLE; s = data_source; T_COMMA; T_INTERVAL; i = data_source; T_COMMA; T_PRIORITY; T_ASSIGN; p = unsigned_int; T_RPAREN;
  { (Some s, Some i, Some p) }
  | T_LPAREN; T_SINGLE; T_ASSIGN; s = data_source; T_COMMA; T_PRIORITY; T_ASSIGN; p = unsigned_int; T_RPAREN;
  { (Some s, None, Some p) }
  | T_LPAREN; T_INTERVAL; T_ASSIGN; i = data_source; T_COMMA; T_PRIORITY; T_ASSIGN; p = unsigned_int; T_RPAREN;
  { (None, Some i, Some p) }
  | T_LPAREN; T_PRIORITY; T_ASSIGN; p = unsigned_int; T_RPAREN;
  { (None, None, Some p) }

let data_source :=
  | v = constant;
  {
    let c = Syntax.c_from_expr_exn v in
    Syntax.Task.DSConstant(c)
  }
  | ~ = global_var_access; <Syntax.Task.DSGlobalVar>
  | out = prog_output_access;
  {
    let (prog_name, var) = out in
    Syntax.Task.DSProgOutput(prog_name, var)
  }
  | dv = direct_variable;
  {
    let var_use = mk_var_use_dir dv in
    Syntax.Task.DSGlobalVar(var_use)
  }

let prog_config :=
  | T_PROGRAM; ~ = prog_name_qual; T_COLON; prog_type_access; <>
  | T_PROGRAM; pc = prog_name_qual; T_WITH; t = task_name; T_COLON; prog_type_name;
  { (Syntax.ProgramConfig.set_task pc t) }
  | T_PROGRAM; pc = prog_name_qual; T_WITH; t = task_name; T_COLON; prog_type_name; T_LBRACE; cvs = separated_list(T_COMMA, prog_conf_elem); T_RBRACE;
  {
    let pc = Syntax.ProgramConfig.set_conn_vars pc cvs in
    (Syntax.ProgramConfig.set_task pc t)
  }
  | T_PROGRAM; pc = prog_name_qual; T_COLON; prog_type_access; T_LBRACE; cvs = separated_list(T_COMMA, prog_conf_elem); T_RBRACE;
  { (Syntax.ProgramConfig.set_conn_vars pc cvs) }

(* Helper rule for prog_config *)
let prog_config_list :=
  | pc = prog_config; T_SEMICOLON;
  { [pc] }
  | pcs = prog_config_list; pc = prog_config; T_SEMICOLON;
  { pc :: pcs }

(* prog_conf_elems: *)

let prog_conf_elem :=
  (* | fb = fb_task
  { fb } *)
  | ~ = prog_cnxn; <>

(* fb_task:
    | fn = fb_name; T_WITH; tn = task_name
    {  } *)

(* This stmt assigns program inputs and outputs to IEC variable. *)
let prog_cnxn :=
  (* Input *)
  | sv = symbolic_variable; T_ASSIGN; prog_data_source;
  { mk_var_use_sym sv }
  (* Output *)
  (* | v = symbolic_variable; T_SENDTO; data_sink
  { v } *)

let prog_data_source :=
  | ~ = constant; <>
  (* | ~ = enumerated_value; <> *)
  (* | ~ = global_var_access; <> *)
  (* | ~ = direct_variable; <> *)

(* let data_sink :=              *)
(*   | ~ = global_var_access; <> *)
(*   | ~ = direct_variable; <>   *)

(* TODO: This is not complete *)
let config_inst_init :=
  | resource_name; T_DOT; prog_name; T_SEMICOLON;
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
let expression :=
  | ~ = xor_expr; <>
  | e1 = expression; T_OR; e2 = xor_expr;
  {
    let ti = Syntax.expr_get_ti e1 in
    Syntax.ExprBin(ti, e1, Syntax.OR, e2)
  }

(* According IEC61131-3 constant expression should be evaluated at compile-time.

   There are no examples or more formal rules for supported compile-time expressions
   in a Standard text, so I don't know what does this means.
*)
let constant_expr :=
  | ~ = constant; <>

let xor_expr :=
  | ~ = and_expr; <>
  | e1 = xor_expr; T_XOR; e2 = and_expr;
  {
    let ti = Syntax.expr_get_ti e1 in
    Syntax.ExprBin(ti, e1, Syntax.XOR, e2)
  }

let and_expr :=
  | ~ = compare_expr; <>
  | e1 = and_expr; T_AND; e2 = compare_expr;
  {
    let ti = Syntax.expr_get_ti e1 in
    Syntax.ExprBin(ti, e1, Syntax.AND, e2)
  }

let compare_expr :=
  | ~ = equ_expr; <>
  | e1 = compare_expr; T_EQ; e2 = equ_expr;
  {
    let ti = Syntax.expr_get_ti e1 in
    Syntax.ExprBin(ti, e1, Syntax.EQ, e2)
  }
  | e1 = compare_expr; T_NEQ; e2= equ_expr;
  {
    let ti = Syntax.expr_get_ti e1 in
    Syntax.ExprBin(ti, e1, Syntax.NEQ, e2)
  }

let equ_expr :=
  | ~ = add_expr; <>
  | e1 = equ_expr; op = compare_expr_operator; e2 = add_expr;
  {
    let ti = Syntax.expr_get_ti e1 in
    Syntax.ExprBin(ti, e1, op, e2)
  }

let add_expr :=
  | ~ = term; <>
  | e1 = add_expr; op = add_operator; e2 = term;
  {
    let ti = Syntax.expr_get_ti e1 in
    Syntax.ExprBin(ti, e1, op, e2)
  }

let term :=
  | ~ = power_expr; <>
  | e1 = term; op = multiply_operator; e2 = power_expr;
  {
    let ti = Syntax.expr_get_ti e1 in
    Syntax.ExprBin(ti, e1, op, e2)
  }

let power_expr :=
  | ~ = unary_expr; <>
  | e1 = power_expr; T_POW; e2 = unary_expr;
  {
    let ti = Syntax.expr_get_ti e1 in
    Syntax.ExprBin(ti, e1, Syntax.POW, e2)
  }

let unary_expr :=
  | op = unary_operator; e = primary_expr;
  {
    let ti = Syntax.expr_get_ti e in
    Syntax.ExprUn(ti, op, e)
  }
  | ~ = primary_expr; <>

let primary_expr :=
  | ~ = constant; <>
  | values = enum_value_use;
  {
    let (name, ti) = values in
    Syntax.ExprConstant(ti, Syntax.CEnumValue(ti, name))
  }
  | v = variable_access;
  {
    let ti = Syntax.VarUse.get_ti v in
    Syntax.ExprVariable(ti, v)
  }
  | fc = func_call;
  {
    let ti = Syntax.stmt_get_ti fc in
    Syntax.ExprFuncCall(ti, fc)
  }
  (* | ref_value {  } *)
  | T_LPAREN; ~ = expression; T_RPAREN; <>

(* Helper rule for primary_expr: "use" occurrence of enum element. Since we are not
   interested in the complete enum type specification here, we need only the name
   and token location.

   Returns (name, ti). *)
let enum_value_use :=
  | option(enum_value_opt); ~ = T_IDENTIFIER; <>

let variable_access :=
  | ~ = variable_expr; <>
  | ~ = variable_expr; multibit_part_access; <>
  (* Non-standard extenstion to handle IF ARR1[i] < 10 THEN *)
  | ~ = variable_expr; array_access; <>

let array_access :=
  | T_LBRACK; ~ = expression; T_RBRACK; <>

(* Helper rule for variable_access.
   This is required to avoid shift/reduce conflict with identifier from func_name rule. *)
let variable_expr :=
  | id = T_IDENTIFIER;
  {
    let (name, ti) = id in
    mk_var_use name ti
  }

(* Standard's BNF define this rule as:
     '.' (unsigned_int | '%' ('X'|'B'|'W'|'D'|'L') ? unsigned_int)
   I believe that this is a typo, because it will not work with dot-separated
   paths like this: "%X3.0.1.2".
   This behaviour is fixed in lexer.mll (see rules for T_DIR_VAR). *)
let multibit_part_access :=
  | T_DOT; ~ = T_DIR_VAR; <>

let func_call :=
  | f = func_access; T_LPAREN; stmts = separated_list(T_COMMA, param_assign); T_RPAREN;
  {
    let ti = Syntax.Function.get_ti f in
    Syntax.StmFuncCall(ti, f, stmts)
  }

let stmt_list :=
  | s = stmt; option(T_SEMICOLON);
  { [s] }
  | sl = stmt_list; s = stmt; option(T_SEMICOLON);
  { sl @ [s] }

let stmt :=
  | ~ = assign_stmt; <>
  | ~ = subprog_ctrl_stmt; <>
  | ~ = selection_stmt; <>
  | ~ = iteration_stmt; <>

let assign_stmt :=
  | v = variable; T_ASSIGN; e = expression;
  {
    let vti = Syntax.VarUse.get_ti v in
    let eti = Syntax.expr_get_ti e in
    Syntax.StmExpr(vti, Syntax.ExprBin(eti, Syntax.ExprVariable(vti, v), Syntax.ASSIGN, e))
  }
  | ~ = assignment_attempt; <>
  (* | ~ = ref_assign; <> *)

let assignment_attempt :=
  | name_ti = ref_name; T_ASSIGN_REF; rhs_expr = assignment_attempt_rhs;
  {
    let name, ti = name_ti in
    let var = mk_var_use name ti in
    Syntax.StmExpr(ti, Syntax.ExprBin(ti, Syntax.ExprVariable(ti, var), Syntax.ASSIGN_REF, rhs_expr))
  }

(* Helper rule for [assignment_attempt] *)
let assignment_attempt_rhs :=
  | name_ti = ref_name;
  {
    let name, ti = name_ti in
    let var = mk_var_use name ti in
    Syntax.ExprVariable(ti, var)
  }
  | ~ = ref_deref; <>
  | ref_val = ref_value;
  {
    let ti = match ref_val with
      | Syntax.RefNull | Syntax.RefFBInstance _ -> TI.create_dummy ()
      | Syntax.RefSymVar sv -> Syntax.SymVar.get_ti sv
    in
    Syntax.ExprConstant(ti, Syntax.CPointer(ti, ref_val))
  }

(* invocation: *)

let subprog_ctrl_stmt :=
  | ~ = func_call; <>
  (* | invocation
  {  } *)
  (* | T_SUPER T_LPAREN T_RPAREN
  {  } *)
  | ~ = T_RETURN; <Syntax.StmReturn>

let param_assign :=
  | vn = variable_name; T_ASSIGN; expr = expression;
  {
    (* Source *)
    let (name, ti) = vn in
    let src_var = mk_var_use name ti in
    let expr_var_src = Syntax.ExprVariable(ti, src_var) in

    let assign_expr = Syntax.ExprBin(ti, expr_var_src, Syntax.ASSIGN, expr) in
    { Syntax.name = Some(name); Syntax.stmt = Syntax.StmExpr(ti, assign_expr); Syntax.inverted = false }
  }
  | expr = expression;
  {
    let eti = Syntax.expr_get_ti expr in
    { Syntax.name = None; Syntax.stmt = Syntax.StmExpr(eti, expr); Syntax.inverted = false }
  }
  (* | ref_assign
  {  } *)
  (* Use assignment expression for "=>" tokens that assigns given parameter
     to a function output variable. *)
  | T_NOT; vn = variable_name; T_SENDTO; v = variable;
  {
    (* Source *)
    let (name, ti) = vn in
    let src_var = mk_var_use name ti in
    let expr_var_src = Syntax.ExprVariable(ti, v) in

    (* Destination *)
    let ti_dest = Syntax.VarUse.get_ti v in
    let expr_var_dest = Syntax.ExprVariable(ti_dest, v) in

    let sendto_expr = Syntax.ExprBin(ti, expr_var_src, Syntax.SENDTO, expr_var_dest) in
    { Syntax.name = Some(name); Syntax.stmt = Syntax.StmExpr(ti, sendto_expr); Syntax.inverted = true }
  }
  | vn = variable_name; T_SENDTO; v = variable;
  {
    (* Source *)
    let (name, ti) = vn in
    let src_var = mk_var_use name ti in
    let expr_var_src = Syntax.ExprVariable(ti, v) in

    (* Destination *)
    let ti_dest = Syntax.VarUse.get_ti v in
    let expr_var_dest = Syntax.ExprVariable(ti_dest, v) in

    let sendto_expr = Syntax.ExprBin(ti, expr_var_src, Syntax.SENDTO, expr_var_dest) in
    { Syntax.name = Some(name); Syntax.stmt = Syntax.StmExpr(ti, sendto_expr); Syntax.inverted = false }
  }

let selection_stmt :=
  | ~ = if_stmt; <>
  | ~ = case_stmt; <>

let if_stmt :=
  | ti = T_IF; e = expression; T_THEN; ifs = stmt_list; T_END_IF;
  {
    let eti = Syntax.expr_get_ti e in
    Syntax.StmIf(ti, Syntax.StmExpr(eti, e), ifs, [], [])
  }
  | ti = T_IF; e = expression; T_THEN; ifs = stmt_list; T_ELSE; elses = stmt_list; T_END_IF;
  {
    let eti = Syntax.expr_get_ti e in
    Syntax.StmIf(ti, Syntax.StmExpr(eti, e), ifs, [], elses)
  }
  | ti = T_IF; e = expression; T_THEN; ifs = stmt_list; elsifs = if_stmt_elsif_list; T_END_IF;
  {
    let eti = Syntax.expr_get_ti e in
    Syntax.StmIf(ti, Syntax.StmExpr(eti, e), ifs, elsifs, [])
  }
  | ti = T_IF; e = expression; T_THEN; ifs = stmt_list; elsifs = if_stmt_elsif_list; T_ELSE; elses = stmt_list; T_END_IF;
  {
    let eti = Syntax.expr_get_ti e in
    Syntax.StmIf(ti, Syntax.StmExpr(eti, e), ifs, elsifs, elses)
  }

(* Helper rule for if_stmt *)
let if_stmt_elsif_list :=
  | ti = T_ELSIF; cond_e = expression; T_THEN; stmts = stmt_list;
  {
    let eti = Syntax.expr_get_ti cond_e in
    [Syntax.StmElsif(ti, Syntax.StmExpr(eti, cond_e), stmts)]
  }
  | elsifs = if_stmt_elsif_list; ti = T_ELSIF; cond_e = expression; T_THEN; stmts = stmt_list;
  {
    let eti = Syntax.expr_get_ti cond_e in
    let elsif = Syntax.StmElsif(ti, Syntax.StmExpr(eti, cond_e), stmts) in
    elsifs @ [elsif]
  }

let case_stmt :=
  | ti = T_CASE; e = expression; T_OF; css = list(case_selection); T_ELSE; sl = stmt_list; T_END_CASE;
  {
    let eti = Syntax.expr_get_ti e in
    Syntax.StmCase(ti, Syntax.StmExpr(eti, e), css, sl)
  }
  | ti = T_CASE; e = expression; T_OF; css = list(case_selection); T_END_CASE;
  {
    let eti = Syntax.expr_get_ti e in
    Syntax.StmCase(ti, Syntax.StmExpr(eti, e), css, [])
  }

let case_selection :=
  | cl = case_list; T_COLON; sl = stmt_list;
  { { Syntax.case = cl; Syntax.body = sl } }

let case_list :=
  | ~ = separated_list(T_COMMA, case_list_elem); <>

let case_list_elem :=
  | specs = subrange;
  {
    let (ti, lb, ub) = specs in
    Syntax.StmExpr(ti, Syntax.ExprConstant(ti, Syntax.CRange(ti, lb, ub)))
  }
  | e = constant_expr;
  {
    let eti = Syntax.expr_get_ti e in
    Syntax.StmExpr(eti, e)
  }

let iteration_stmt :=
  | ~ = for_stmt; <>
  | ~ = while_stmt; <>
  | ~ = repeat_stmt; <>
  | ~ = T_EXIT; <Syntax.StmExit>
  | ~ = T_CONTINUE; <Syntax.StmContinue>

let for_stmt :=
  | ti = T_FOR; cv = control_variable; T_ASSIGN; fl = for_list; T_DO; sl = stmt_list; T_END_FOR;
  {
    let (e_start, e_end, e_step) = fl in
    let ctrl_assign_stmt =
      let vti = Syntax.SymVar.get_ti cv in
      let var = mk_var_use_sym cv in
      let assign_expr = Syntax.ExprBin(vti, Syntax.ExprVariable(vti, var), Syntax.ASSIGN, e_start) in
      Syntax.StmExpr(ti, assign_expr)
    in
    let ctrl = {
      Syntax.assign = ctrl_assign_stmt;
      Syntax.range_end = e_end;
      Syntax.range_step = e_step }
    in
    Syntax.StmFor(ti, ctrl, sl)
  }

let control_variable :=
  | id = T_IDENTIFIER;
  {
    let (n, ti) = id in
    Syntax.SymVar.create n ti
  }

let for_list :=
  | e1 = expression; T_TO; e2 = expression; T_BY; e3 = expression;
  { (e1, e2, e3) }
  | e1 = expression; T_TO; e2 = expression;
  {
    (* According 7.3.3.4.2 default STEP value is 1. *)
    let dti = TI.create_dummy () in
    (e1, e2, Syntax.ExprConstant(dti, Syntax.CInteger(dti, 1)))
  }

let while_stmt :=
  | ti = T_WHILE; e = expression; T_DO; sl = stmt_list; T_END_WHILE;
  {
    let eti = Syntax.expr_get_ti e in
    Syntax.StmWhile(ti, Syntax.StmExpr(eti, e), sl)
  }

let repeat_stmt :=
  | ti = T_REPEAT; sl = stmt_list; T_UNTIL; e = expression; T_END_REPEAT;
  {
    let eti = Syntax.expr_get_ti e in
    Syntax.StmRepeat(ti, sl, Syntax.StmExpr(eti, e))
  }
(* }}} *)

(* {{{ Table 73-76 -- Graphical languages *)
(* }}} *)

(* {{{ Generic variables declaration. *)
(** We don't want to handle standard violations in the static analyzer, so, we allow to use any type of variables
    in any type of POU. For example, Programs can contain global variables, which is forbidden in IEC 61131-3. *)
let var_decls :=
  | vds = list(var_decls1);
  { list_flatten vds }

let var_decls1 :=
  | T_VAR; option(qualifier); vars = var_decls_list; T_END_VAR;
  {
    List.rev vars
    |> List.map ~f:(fun v -> let attr = Syntax.VarDecl.Var(None) in Syntax.VarDecl.set_attr v attr)
  }
  | T_VAR_INPUT; option(qualifier); vars = var_decls_list; T_END_VAR;
  {
    List.rev vars
    |> List.map ~f:(fun v -> let attr = Syntax.VarDecl.VarIn(None) in Syntax.VarDecl.set_attr v attr)
  }
  | T_VAR_OUTPUT; option(qualifier); vars = var_decls_list; T_END_VAR;
  {
    List.rev vars
    |> List.map ~f:(fun v -> let attr = Syntax.VarDecl.VarOut(None) in Syntax.VarDecl.set_attr v attr)
  }
  | T_VAR_IN_OUT; option(qualifier); vars = var_decls_list; T_END_VAR;
  {
    List.rev vars
    |> List.map ~f:(fun v -> Syntax.VarDecl.set_attr v Syntax.VarDecl.VarInOut)
  }
  | T_VAR_EXTERNAL; option(qualifier); vars = var_decls_list; T_END_VAR;
  {
    List.rev vars
    |> List.map ~f:(fun v -> let attr = Syntax.VarDecl.VarExternal(None) in Syntax.VarDecl.set_attr v attr)
  }
  | ~ = var_global_decl; <>
  | T_VAR_TEMP; option(qualifier); vars = var_decls_list; T_END_VAR;
  {
    List.rev vars
    |> List.map ~f:(fun v -> Syntax.VarDecl.set_attr v Syntax.VarDecl.VarTemp)
  }
  | T_VAR_ACCESS; option(qualifier); vars = var_decls_list; T_END_VAR;
  {
    vars
    (* List.rev vars                                                              *)
    (* |> List.map ~f:(fun v -> Syntax.VarDecl.set_attr v Syntax.VarDecl.VarTemp) *)
  }
  | T_VAR_CONFIG; option(qualifier); vars = var_decls_list; T_END_VAR;
  {
    vars
    (* List.rev vars                                                              *)
    (* |> List.map ~f:(fun v -> Syntax.VarDecl.set_attr v Syntax.VarDecl.VarTemp) *)
  }

let var_global_decl :=
  | T_VAR_GLOBAL; option(qualifier); vars = var_decls_list; T_END_VAR;
  {
    List.rev vars
    |> List.map ~f:(fun v -> let attr = Syntax.VarDecl.VarGlobal(None) in Syntax.VarDecl.set_attr v attr)
  }

(** We don't care. *)
let qualifier :=
  | T_RETAIN; {}
  | T_NON_RETAIN; {}
  | T_CONSTANT; {}

let var_decls_list :=
  /* nothing */
  { [] }
  | ~ = var_decl; T_SEMICOLON; <>
  | ~ = var_ref_decl; T_SEMICOLON; <>
  | ~ = var_loc_decl; T_SEMICOLON; <>
  | ~ = var_access_decl; T_SEMICOLON; <>
  | vdl = var_decls_list; vl = var_decl; T_SEMICOLON;
  { List.append vdl vl  }
  | vdl = var_decls_list; vl = var_ref_decl; T_SEMICOLON;
  { List.append vdl vl  }
  | vdl = var_decls_list; vl = var_loc_decl; T_SEMICOLON;
  { List.append vdl vl  }
  | vdl = var_decls_list; vl = var_access_decl; T_SEMICOLON;
  { List.append vdl vl  }

let var_decl :=
  | vars = separated_nonempty_list(T_COMMA, variable_name); T_COLON; simple_spec;
  {
    List.map
      vars
      ~f:(fun (n, ti) -> let var = mk_var_use n ti in Syntax.VarDecl.create var None)
  }
  (* | vs = separated_nonempty_list(T_COMMA, variable_name); T_COLON; str_var_decl; *)
  (* | vs = separated_nonempty_list(T_COMMA, variable_name); T_COLON; array_var_decl; *)
  (* | vs = separated_nonempty_list(T_COMMA, variable_name); T_COLON; struct_var_decl; *)
  | var_names = separated_nonempty_list(T_COMMA, variable_name); T_COLON; ty_specs = simple_spec_init;
  {
    let (ty, inval_opt) = ty_specs in
    let spec = Syntax.DTyDeclSingleElement(ty, inval_opt) in
    List.map
      var_names
      ~f:(fun (n, ti) -> begin
        let var = mk_var_use n ti in
        Syntax.VarDecl.create var (Some(spec))
      end)
  }
  (* | var_names = separated_nonempty_list(T_COMMA, variable_name); T_COLON; str_var_decl; <> *)
  | var_names = separated_nonempty_list(T_COMMA, variable_name); T_COLON; ty_specs = ref_spec_init;
  {
    let (ref_level, ref_ty, inval_opt) = ty_specs in
    let spec = Syntax.DTyDeclRefType(ref_level, ref_ty, inval_opt) in
    List.map
      var_names
      ~f:(fun (n, ti) -> begin
        let var = mk_var_use n ti in
        Syntax.VarDecl.create var (Some(spec))
      end)
  }
  (* | var_names = separated_nonempty_list(T_COMMA, variable_name); T_COLON; struct_var_decl_init; <> *)
  (* | var_names = separated_nonempty_list(T_COMMA, variable_name); T_COLON; interface_spec_init; <> *)
  | ~ = array_var_decl_init; <>
  (* | ~ = struct_var_decl_init; <> *)
  (* | ~ = fb_decl_init; <> *)
  (* | ~ = interface_spec_init; <> *)

(* NOTE: Comma-separated list is incorrect. *)
let var_loc_decl :=
  | var_names = separated_nonempty_list(T_COMMA, variable_name); T_AT; dir_var = T_DIR_VAR; T_COLON; var_spec; optional_assign(constant_expr);
  {
    List.map var_names
      ~f:(fun (n, ti) -> begin
        let var = mk_var_use n ti in
        let v = Syntax.VarDecl.create var None in
        Syntax.VarDecl.set_located_at v dir_var
      end)
  }

(* NOTE: Comma-separated list is incorrect. *)
let var_access_decl :=
  | var_names = separated_nonempty_list(T_COMMA, variable_name); T_COLON; sv = symbolic_variable; T_COLON; data_type_access;
  {
    List.map var_names
      ~f:(fun (n, ti) -> begin
        let var = mk_var_use n ti in
        let v = Syntax.VarDecl.create var None in
        let attr = Syntax.VarDecl.VarAccess("") in
        Syntax.VarDecl.set_attr v attr
      end)
  }

let var_ref_decl :=
  | var_names = separated_nonempty_list(T_COMMA, variable_name); T_COLON; ty_specs = ref_spec;
  {
    let (ref_level, ref_ty) = ty_specs in
    let spec = Syntax.DTyDeclRefType(ref_level, ref_ty, None) in
    List.map
      var_names
      ~f:(fun (n, ti) -> begin
        let var = mk_var_use n ti in
        Syntax.VarDecl.create var (Some(spec))
      end)
  }

(* interface_var_decl: *)

let array_var_decl_init :=
  | var_names = separated_nonempty_list(T_COMMA, variable_name); T_COLON; ty_specs = array_spec_init;
  {
    let (subranges, ty, initializer_list) = ty_specs in
    let spec = Syntax.DTyDeclArrayType(subranges, ty, initializer_list) in
    List.map
      var_names
      ~f:(fun (n, ti) -> begin
        let var = mk_var_use n ti in
        Syntax.VarDecl.create var (Some(spec))
      end)
  }

(* array_conform_decl *)

(* fb_decl_no_init *)

(* array_var_decl: *)

(* struct_var_decl: *)

(* }}} *)

(* {{{ Other symbols
 * Helper rules and symbols which doesn't defined in standard grammar explicitly *)

(* Generic data types *)
let generic_type_name :=
  | T_ANY;
  { Syntax.ANY }
  | T_ANY_DERIVED;
  { Syntax.ANY_DERIVED }
  | T_ANY_ELEMENTARY;
  { Syntax.ANY_ELEMENTARY }
  | T_ANY_MAGNITUDE;
  { Syntax.ANY_MAGNITUDE }
  | T_ANY_NUM;
  { Syntax.ANY_NUM }
  | T_ANY_REAL;
  { Syntax.ANY_REAL }
  | T_ANY_INT;
  { Syntax.ANY_INT }
  | T_ANY_BIT;
  { Syntax.ANY_BIT }
  | T_ANY_STRING;
  { Syntax.ANY_STRING }
  | T_ANY_DATE;
  { Syntax.ANY_DATE }

(* let dir_var_location_prefix := *)
(*   | id = T_IDENTIFIER;         *)
(*   {                            *)
(*     let (id_str, _) = id in    *)
(*     get_dir_var_loc_exn id_str *)
(*   }                            *)

(* let dir_var_size_prefix :=      *)
(*   | id = T_IDENTIFIER;          *)
(*   {                             *)
(*     let (id_str, _) = id in     *)
(*     get_dir_var_size_exn id_str *)
(*   }                             *)

let compare_expr_operator :=
  | T_GT;
  { Syntax.GT }
  | T_LT;
  { Syntax.LT }
  | T_GE;
  { Syntax.GE }
  | T_LE;
  { Syntax.LE }

let add_operator :=
  | T_PLUS;
  { Syntax.ADD }
  | T_MINUS;
  { Syntax.SUB }

let multiply_operator :=
  | T_MUL;
  { Syntax.MUL }
  | T_DIV;
  { Syntax.DIV }
  | T_MOD;
  { Syntax.MOD }

let unary_operator :=
  | T_MINUS;
  { Syntax.NEG }
  | T_NOT;
  { Syntax.NEG }

optional_assign(X):
  | { None }
  | T_ASSIGN x = X { Some x }
(* }}} *)

(* vim: set foldmethod=marker foldlevel=0 foldenable tw=2 sw=2 tw=120 wrap : *)
