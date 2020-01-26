(*
 Based on IEC61131-3 2nd edition ANNEX B - Formal specification of language elements.
 See also Table C.2 (ANNEX C) with complete list of keywords.
*)
{
  open Parser
  open Lexing

  exception SyntaxError of string

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

(* B.1.1 Letters, digits and identifiers *)
let letter	    = ['A'-'Z' 'a'-'z']
let digit		= ['0'-'9']
let octal_digit	= ['0'-'7']
let hex_digit   = digit | ['A'-'F']

let identifier  = letter | letter ['A'-'Z' 'a'-'z' '0'-'9' '_']*
(*TODO: _ syntax*)
let integer = digit+

rule initial tokinfo =
  parse
  | whitespace+          { initial tokinfo lexbuf }
  | '\n'                 { incr_linenum lexbuf; initial tokinfo lexbuf }
  | "NIL"                { T_NIL(tokinfo lexbuf) }
  | ":="                 { T_ASSIGN }
  | "=>"                 { T_SENDTO }
  | "."                  { T_DOT }
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
  | "I"                  { T_I }
  | "Q"                  { T_Q }
  | "M"                  { T_M }
  | "X"                  { T_X }
  | "B"                  { T_B }
  | "W"                  { T_W }
  | "D"                  { T_D }
  | "L"                  { T_L }
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
  (* B.1.3.1 - Elementary data types *)
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
  | "BOOL"           { T_BOOL }
  | "TIME"           { T_TIME }
  | "DATE"           { T_DATE }
  | "DATE_AND_TIME"  { T_DATE_AND_TIME }
  | "DT"             { T_DT }
  | "TIME_OF_DAY"    { T_TIME_OF_DAY }
  | "TOD"            { T_TOD }
  (* B.1.3.2 - Generic data types *)
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
  (* B.2.2 Operators *)
  | "OR"             { T_OR }
  | "XOR"            { T_XOR }
  | "AND" | "&"      { T_AND }
  | "EQU"            { T_EQU }
  (* B.1.3.3 - Derived data types *)
  | identifier as id
  {
      (* Printf.printf "ID: %s\n" id; *)
      let ti = tokinfo lexbuf in
      T_IDENTIFIER(id, ti)
  }
  | integer as i
  {
      (* Printf.printf "INTEGER: %s\n" i; *)
      let v = int_of_string i in
      let ti = tokinfo lexbuf in
      T_INTEGER(v, ti)
  }
  | eof              { T_EOF }
  | "(*" {comment tokinfo 1 lexbuf} (* start of a comment *)
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
and comment tokinfo depth  = parse
  | "(*" {comment tokinfo (depth + 1) lexbuf}
  | "*)" {if depth = 1 then initial tokinfo lexbuf else comment tokinfo (depth-1) lexbuf} (*Nested comments are allowed*)
  | '\n' {let () = new_line lexbuf in comment tokinfo depth lexbuf}
  | _ {comment tokinfo depth lexbuf}

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
