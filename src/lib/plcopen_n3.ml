open Core_kernel
module S = IECCheckerCore.Syntax
module TI = IECCheckerCore.Tok_info
module AU = IECCheckerCore.Ast_util
module Warn = IECCheckerCore.Warn

(** Keywords / reserved word list of IEC 61131-3 Ed.3 starting with a letter *)
let reserved_keywords =
  [
    "ABS";
    "END_IF";
    "ABSTRACT";
    "END_INTERFACE LEFT";
    "ACOS";
    "END_METHOD";
    "LEN";
    "ACTION";
    "END_NAMESPACE LIMIT";
    "ADD";
    "END_PROGRAM";
    "LINT";
    "AND";
    "END_REPEAT";
    "LN";
    "ARRAY";
    "END_RESOURCE LOG";
    "ASIN";
    "END_STEP";
    "LREAL";
    "AT";
    "END_STRUCT";
    "LT";
    "ATAN";
    "END_TRANSITION LTIME";
    "ATAN2";
    "END_TYPE";
    "LTIME_OF_DAY";
    "BOOL";
    "END_VAR";
    "LTOD";
    "BY";
    "END_WHILE";
    "LWORD";
    "BYTE";
    "EQ";
    "MAX";
    "CASE";
    "EXIT";
    "METHOD";
    "CHAR";
    "EXP";
    "MID";
    "CLASS";
    "EXPT";
    "MIN";
    "CONCAT";
    "EXTENDS";
    "MOD";
    "CONFIGURATION";
    "F_EDGE";
    "MOVE";
    "CONSTANT";
    "F_TRIG";
    "MUL";
    "CONTINUE";
    "FALSE";
    "MUX";
    "COS";
    "FINAL";
    "NAMESPACE";
    "CTD";
    "FIND";
    "NE";
    "CTU";
    "FOR";
    "NON_RETAIN";
    "CTUD";
    "FROM";
    "NOT";
    "DATE";
    "FUNCTION";
    "NULL";
    "DATE_AND_TIME";
    "FUNCTION_BLOCK OF";
    "DELETE";
    "GE";
    "ON";
    "DINT";
    "GT";
    "OR";
    "DIV";
    "IF";
    "OVERLAP";
    "DO";
    "IMPLEMENTS";
    "OVERRIDE";
    "DT";
    "INITIAL_STEP";
    "PRIORITY";
    "DWORD";
    "INSERT";
    "PRIVATE";
    "ELSE";
    "INT";
    "PROGRAM";
    "ELSIF";
    "INTERFACE";
    "PROTECTED";
    "END_ACTION";
    "INTERNAL";
    "PUBLIC";
    "END_CASE";
    "INTERVAL";
    "R_EDGE";
    "END_CLASS";
    "LD";
    "R_TRIG";
    "END_CONFIGURATION LDATE";
    "READ_ONLY";
    "END_FOR";
    "LDATE_AND_TIME READ_WRITE";
    "END_FUNCTION";
    "LDT";
    "REAL";
    "END_FUNCTION_BLOCK LE";
    "REF";
    "REF_TO";
    "REPEAT";
    "REPLACE";
    "RESOURCE";
    "RETAIN";
    "RETURN";
    "RIGHT";
    "ROL";
    "ROR";
    "RS";
    "SEL";
    "SHL";
    "SHR";
    "SIN";
    "SINGLE";
    "SINT";
    "SQRT";
    "SR";
    "STEP";
    "STRING";
    "STRING#";
    "STRUCT";
    "SUB";
    "SUPER";
    "T";
    "TAN";
    "TASK";
    "THEN";
    "THIS";
    "THIS";
    "TIME";
    "TIME_OF_DAY";
    "TO";
    "TOD";
    "TOF";
    "TON";
    "TP";
    "TRANSITION";
    "TRUE";
    "TRUNC";
    "TYPE";
    "UDINT";
  ]

let check_name var =
  let name = S.VarUse.get_name var in
  let ti = S.VarUse.get_ti var in
  let m = List.find reserved_keywords ~f:(fun k -> String.equal name k) in
  match m with
  | Some _ ->
    let msg = "IEC data types and standard library objects must be avoided" in
    let w = Warn.mk ti.linenr ti.col "PLCOPEN-N3" msg in
    Some w
  | None -> None

let do_check elems =
  let vardecls = List.fold_left
      elems
      ~init:[]
      ~f:(fun acc elem -> acc @ (AU.get_var_decls elem))
  in
  List.map
    vardecls
    ~f:(fun d -> begin
          let var = S.VarDecl.get_var d in
          check_name var
        end)
  |> List.filter ~f:(fun w -> match w with Some _ -> true | None -> false)
  |> List.map ~f:(fun w ->
      match w with Some w -> w | None -> assert false)
