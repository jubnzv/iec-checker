(* Based on IEC61131-3 3rd edition ANNEX A - Formal specification of language elements. *)
{
  open Parser
  open Lexing
  open Core_kernel

  exception LexingError of string

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
      lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

let comment_beg  = "(*"
let comment_end  = "*)"

let whitespace = ['\r' '\t' ' ']

(* Letters, digits and identifiers *)
let letter	    = ['A'-'Z' 'a'-'z']
let digit		= ['0'-'9']
let octal_digit	= ['0'-'7']
let hex_digit   = digit | ['A'-'F']

let integer = digit+(('_')?digit)*
let bit = ['0' '1']
let binary_integer = '2' '#' (('_')?bit)+
let octal_integer = '8' '#' (('_')?octal_digit)+
let hex_integer = '1' '6' '#' (('_')?hex_digit)+

let bool_false = "FALSE" | "BOOL#FALSE" | "BOOL#0"
let bool_true = "TRUE" | "BOOL#TRUE" | "BOOL#1"

let exponent = 'E' (['+' '-'])? integer

(* The correct definition for real would be:
   let real = integer '.' integer (exponent)?

   But there is also definition of fix_point in Table 8 / Table 9:
   integer '.' integer

   This means that parser should be able to figure out what kind of token we
   have based on the current context.
 *)
let real = integer '.' integer exponent

let fix_point = integer '.' integer

(* Time literals *)
let fix_point_d = (integer | (integer '.' integer)) 'd'
let fix_point_h = (integer | (integer '.' integer)) 'h'
let fix_point_m = (integer | (integer '.' integer)) 'm'
let fix_point_s = (integer | (integer '.' integer)) 's'
let fix_point_ms = (integer | (integer '.' integer)) "ms"
let fix_point_us = (integer | (integer '.' integer)) "us"
let fix_point_ns = (integer | (integer '.' integer)) "ns"

let identifier  = letter | letter ['A'-'Z' 'a'-'z' '0'-'9' '_']*

rule initial tokinfo =
  parse
  (* {{{ Common *)
  | whitespace+          { initial tokinfo lexbuf }
  | '\n'                 { incr_linenum lexbuf; initial tokinfo lexbuf }
  | ":="                 { T_ASSIGN }
  | "=>"                 { T_SENDTO }
  | "."                  { T_DOT }
  | "^"                  { T_DEREF }
  | ">="                 { T_GE }
  | "<="                 { T_LE }
  | ">"                  { T_GT }
  | "-"                  { T_MINUS }
  | "+"                  { T_PLUS }
  | "<"                  { T_LT }
  | "<>"                 { T_NEQ }
  | "="                  { T_EQ }
  | "NOT"                { T_NOT }
  | "MOD"                { T_MOD }
  | "/"                  { T_DIV }
  | "**"                 { T_POW }
  | "*"                  { T_MUL }
  | ".."                 { T_RANGE }
  | "{"                  { T_LBRACE }
  | "}"                  { T_RBRACE }
  | "["                  { T_LBRACK }
  | "]"                  { T_RBRACK }
  | "("                  { T_LPAREN }
  | ")"                  { T_RPAREN }
  | ":"                  { T_COLON }
  | ","                  { T_COMMA }
  | "AT"                 { T_AT }
  | ";"                  { T_SEMICOLON }
  | "#"                  { T_SHARP }
  | "%"                  { T_PERCENT }
  | "WITH"               { T_WITH }
  | "RETAIN"             { T_RETAIN }
  | "NON_RETAIN"         { T_NON_RETAIN }
  | "PROGRAM"            { T_PROGRAM }
  | "END_PROGRAM"        { T_END_PROGRAM }
  | "FUNCTION"           { T_FUNCTION }
  | "END_FUNCTION"       { T_END_FUNCTION }
  | "FUNCTION_BLOCK"     { T_FUNCTION_BLOCK }
  | "END_FUNCTION_BLOCK" { T_END_FUNCTION_BLOCK }
  | "CONFIGURATION"      { T_CONFIGURATION }
  | "END_CONFIGURATION"  { T_END_CONFIGURATION }
  | "RESOURCE"           { T_RESOURCE }
  | "ON"                 { T_ON }
  | "END_RESOURCE"       { T_END_RESOURCE }
  | "SINGLE"             { T_SINGLE }
  | "INTERVAL"           { T_INTERVAL }
  | "PRIORITY"           { T_PRIORITY }
  | "READ_WRITE"         { T_READ_WRITE }
  | "READ_ONLY"          { T_READ_ONLY }
  | "TASK"               { T_TASK }
  | "CONSTANT"           { T_CONSTANT }
  | "VAR"                { T_VAR }
  | "VAR_INPUT"          { T_VAR_INPUT }
  | "VAR_OUTPUT"         { T_VAR_OUTPUT }
  | "VAR_IN_OUT"         { T_VAR_IN_OUT }
  | "VAR_TEMP"           { T_VAR_TEMP }
  | "VAR_EXTERNAL"       { T_VAR_EXTERNAL }
  | "VAR_ACCESS"         { T_VAR_ACCESS }
  | "VAR_CONFIG"         { T_VAR_CONFIG }
  | "VAR_GLOBAL"         { T_VAR_GLOBAL }
  | "END_VAR"            { T_END_VAR }
  | "TYPE"               { T_TYPE }
  | "END_TYPE"           { T_END_TYPE }
  (* }}} *)

  (* {{{ Helpers for datetime types *)
  | "T#"  { T_TSHARP }
  | "LT#" { T_LTSHARP }
  | "D#"  { T_DSHARP }
  | "LD#" { T_LDSHARP }
  (* }}} *)

  (* {{{ Elementary data types *)
  | "SINT"           { T_SINT }
  | "BYTE"           { T_BYTE }
  | "WORD"           { T_WORD }
  | "DWORD"          { T_DWORD }
  | "LWORD"          { T_LWORD }
  | "LREAL"          { T_LREAL }
  | "REAL"           { T_REAL }
  | "SINT"           { T_SINT }
  | "INT"            { T_INT }
  | "DINT"           { T_DINT }
  | "LINT"           { T_LINT }
  | "USINT"          { T_USINT }
  | "UINT"           { T_UINT }
  | "UDINT"          { T_UDINT }
  | "ULINT"          { T_ULINT }
  | "WSTRING"        { T_WSTRING }
  | "STRING"         { T_STRING }
  | "CHAR"           { T_CHAR }
  | "WCHAR"          { T_WCHAR }
  | "BOOL"           { T_BOOL }
  | "TIME"           { T_TIME }
  | "LTIME"          { T_LTIME }
  | "DATE"           { T_DATE }
  | "LDATE"          { T_LDATE }
  | "DATE_AND_TIME"  { T_DATE_AND_TIME }
  | "LDATE_AND_TIME" { T_LDATE_AND_TIME }
  | "DT"             { T_DT }
  | "LDT"            { T_LDT }
  | "TIME_OF_DAY"    { T_TIME_OF_DAY }
  | "LTIME_OF_DAY"   { T_LTIME_OF_DAY }
  | "TOD"            { T_TOD }
  | "LTOD"           { T_LTOD }
  (* }}} *)

  (* {{{ Generic data types *)
  | "ANY"            { T_ANY }
  | "ANY_DERIVED"    { T_ANY_DERIVED }
  | "ANY_ELEMENTARY" { T_ANY_ELEMENTARY }
  | "ANY_MAGNITUDE"  { T_ANY_MAGNITUDE }
  | "ANY_NUM"        { T_ANY_NUM }
  | "ANY_REAL"       { T_ANY_REAL }
  | "ANY_INT"        { T_ANY_INT }
  | "ANY_BIT"        { T_ANY_BIT }
  | "ANY_STRING"     { T_ANY_STRING }
  | "ANY_DATE"       { T_ANY_DATE }
  (* }}} *)

  (* {{{ ST operators *)
  | "OR"             { T_OR }
  | "XOR"            { T_XOR }
  | "AND" | "&"      { T_AND }
  | "EQU"            { T_EQU }
  (* }}} *)

(* {{{ ST control statements *)
  | "IF"             { let ti = tokinfo lexbuf in T_IF(ti) }
  | "THEN"           { let ti = tokinfo lexbuf in T_THEN(ti) }
  | "ELSIF"          { let ti = tokinfo lexbuf in T_ELSIF(ti) }
  | "ELSE"           { let ti = tokinfo lexbuf in T_ELSE(ti) }
  | "END_IF"         { let ti = tokinfo lexbuf in T_END_IF(ti) }
  | "CASE"           { let ti = tokinfo lexbuf in T_CASE(ti) }
  | "OF"             { let ti = tokinfo lexbuf in T_OF(ti) }
  | "END_CASE"       { let ti = tokinfo lexbuf in T_END_CASE(ti) }
  | "FOR"            { let ti = tokinfo lexbuf in T_FOR(ti) }
  | "TO"             { let ti = tokinfo lexbuf in T_TO(ti) }
  | "BY"             { let ti = tokinfo lexbuf in T_BY(ti) }
  | "DO"             { let ti = tokinfo lexbuf in T_DO(ti) }
  | "END_FOR"        { let ti = tokinfo lexbuf in T_END_FOR(ti) }
(* }}} *)

  (* {{{ Integer literals *)
  | integer as i
  {
      let v = int_of_string i in
      (* Printf.printf "INTEGER: %s -> %d\n" i v; *)
      let ti = tokinfo lexbuf in
      T_INTEGER(v, ti)
  }
  | binary_integer as i
  {
      let v = int_of_string ("0b" ^ (String.slice i 2 (String.length i))) in
      (* Printf.printf "BINARY_INTEGER: %s -> %d\n" i v; *)
      let ti = tokinfo lexbuf in
      T_BINARY_INTEGER(v, ti)
  }
  | octal_integer as i
  {
      let v = int_of_string ("0o" ^ (String.slice i 2 (String.length i))) in
      (* Printf.printf "OCTAL_INTEGER: %s -> %d\n" i v; *)
      let ti = tokinfo lexbuf in
      T_BINARY_INTEGER(v, ti)
  }
  | hex_integer as i
  {
      let v = int_of_string ("0x" ^ (String.slice i 3 (String.length i))) in
      (* Printf.printf "HEX_INTEGER: %s -> %d\n" i v; *)
      let ti = tokinfo lexbuf in
      T_BINARY_INTEGER(v, ti)
  }
  | bool_false
  {
      (* Printf.printf "BOOL_VALUE: false\n"; *)
      let ti = tokinfo lexbuf in
      T_BOOL_VALUE(false, ti)
  }
  | bool_true
  {
      (* Printf.printf "BOOL_VALUE: true\n"; *)
      let ti = tokinfo lexbuf in
      T_BOOL_VALUE(false, ti)
  }
  (* }}} *)

  (* {{{ Real literal *)
  | real as r
  {
      let v = float_of_string r in
      (* Printf.printf "REAL_VALUE: %s -> %f\n" r v; *)
      let ti = tokinfo lexbuf in
      T_REAL_VALUE(v, ti)
  }
  (* }}} *)

  (* {{{ Fixed point literal *)
  | fix_point as fp
  {
      (* Printf.printf "FIX_POINT_VALUE: %s\n" fp; *)
      let ti = tokinfo lexbuf in
      T_FIX_POINT_VALUE(fp, ti)
  }
  (* }}} *)

  (* {{{ Time intervals *)
  | fix_point_d as v
  {
      let vf = float_of_string (String.slice v 0 ((String.length v) - 1)) in
      (* Printf.printf "TIME_D: %s -> %f\n" v vf; *)
      let ti = tokinfo lexbuf in
      T_TIME_INTERVAL_D(vf, ti)
  }
  | fix_point_h as v
  {
      let vf = float_of_string (String.slice v 0 ((String.length v) - 1)) in
      (* Printf.printf "TIME_H: %s -> %f\n" v vf; *)
      let ti = tokinfo lexbuf in
      T_TIME_INTERVAL_H(vf, ti)
  }
  | fix_point_m as v
  {
      let vf = float_of_string (String.slice v 0 ((String.length v) - 1)) in
      (* Printf.printf "TIME_M: %s -> %f\n" v vf; *)
      let ti = tokinfo lexbuf in
      T_TIME_INTERVAL_M(vf, ti)
  }
  | fix_point_s as v
  {
      let vf = float_of_string (String.slice v 0 ((String.length v) - 1)) in
      (* Printf.printf "TIME_S: %s -> %f\n" v vf; *)
      let ti = tokinfo lexbuf in
      T_TIME_INTERVAL_S(vf, ti)
  }
  | fix_point_ms as v
  {
      let vf = float_of_string (String.slice v 0 ((String.length v) - 2)) in
      (* Printf.printf "TIME_MS: %s -> %f\n" v vf; *)
      let ti = tokinfo lexbuf in
      T_TIME_INTERVAL_MS(vf, ti)
  }
  | fix_point_us as v
  {
      let vf = float_of_string (String.slice v 0 ((String.length v) - 2)) in
      (* Printf.printf "TIME_US: %s -> %f\n" v vf; *)
      let ti = tokinfo lexbuf in
      T_TIME_INTERVAL_US(vf, ti)
  }
  | fix_point_ns as v
  {
      let vf = float_of_string (String.slice v 0 ((String.length v) - 2)) in
      (* Printf.printf "TIME_NS: %s -> %f\n" v vf; *)
      let ti = tokinfo lexbuf in
      T_TIME_INTERVAL_NS(vf, ti)
  }
  (* }}} *)

  (* {{{ Identifiers *)
  | identifier as id
  {
      (* Printf.printf "ID: %s\n" id; *)
      let ti = tokinfo lexbuf in
      T_IDENTIFIER(id, ti)
  }
  (* }}} *)

  (* {{{ etc. *)
  | eof              { T_EOF }
  | "(*" {comment tokinfo 1 lexbuf} (* start of a comment *)
  | _ { raise (LexingError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
and comment tokinfo depth  = parse
  | "(*" {comment tokinfo (depth + 1) lexbuf}
  | "*)" {if depth = 1 then initial tokinfo lexbuf else comment tokinfo (depth-1) lexbuf} (*Nested comments are allowed*)
  | '\n' {let () = new_line lexbuf in comment tokinfo depth lexbuf}
  | _ {comment tokinfo depth lexbuf}
  (* }}} *)

{
  let read_file parser (filename: string) =
  try
    let fh = open_in filename in
    let lex = Lexing.from_channel fh in
    lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname = filename};
    try
      let terms = parser lex in
      close_in fh;
      terms
    with err ->
      (* Close the file in case of any parsing errors. *)
      close_in fh;
      raise err
  with err ->
    (* Any errors when opening or closing a file are fatal. *)
    Printf.printf "Error while reading file\n" ;
    raise err
}

(* vim: set foldmethod=marker foldlevel=0 foldenable sw=2 tw=120 : *)
