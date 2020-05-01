open Core_kernel
open IECCheckerCore
open IECCheckerParser
open IECCheckerLib
open IECCheckerAnalysis
module S = Syntax
module Lib = CheckerLib
module TI = Tok_info
module W = Warn
module WO = Warn_output

let parse_with_error (lexbuf: Lexing.lexbuf) : (S.iec_library_element list * Warn.t list) =
  let tokinfo lexbuf = TI.create lexbuf in
  let l = Lexer.initial tokinfo in
  try (Parser.main l lexbuf), [] with
  | Lexer.LexingError msg ->
    [], [(W.mk_from_lexbuf lexbuf "LexingError" msg)]
  | Parser.Error ->
    [], [(W.mk_from_lexbuf lexbuf "ParserError" "")]
  | e ->
    [], [(W.mk_from_lexbuf lexbuf "UnknownError" (Exn.to_string e))]

let parse_file (filename : string) : (S.iec_library_element list * Warn.t list) =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let (elements, warns) = parse_with_error lexbuf in
  In_channel.close inx;
  (elements, warns)

let parse_stdin : (S.iec_library_element list * Warn.t list) =
  match In_channel.input_line stdin with
  | None -> ([], [W.mk_internal ~id:"Cancel" ""])
  | Some code -> (
      let lexbuf = Lexing.from_string code in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "stdin" };
      let (elements, warns) = parse_with_error lexbuf in
      (elements, warns))

let run_checker filename fmt create_dumps quiet =
  let (read_stdin : bool) = (String.equal "-" filename) || (String.is_empty filename) in
  if (not (Sys.file_exists filename) && not read_stdin) then
    let err = W.mk_internal ~id:"FileNotFoundError" (Printf.sprintf "File %s doesn't exists" filename) in
    WO.print_report [err] fmt;
    exit 127
  else
    let (elements, parser_warns) =
      if read_stdin then
        parse_stdin
      else
        parse_file filename
    in
    let envs = Ast_util.create_envs elements in
    let pou_cfgs = Cfg.create_cfgs elements in
    if create_dumps then
      Dump.create_dump elements envs pou_cfgs
      (if read_stdin then "stdin" else filename);
    let decl_warnings = Declaration_analysis.run elements envs in
    let flow_warnings = Control_flow_analysis.run pou_cfgs in
    let lib_warnings = Lib.run_all_checks elements envs quiet in
    WO.print_report (
      parser_warns @
      decl_warnings @
      flow_warnings @
      lib_warnings)
      fmt;
    let rc = if not (List.is_empty parser_warns) then 1 else 0 in
    exit rc

let command =
  Command.basic ~summary:"IEC61131-3 static analysis"
    Command.Let_syntax.(
      let%map_open
        output_format = flag "-output-format" (optional string) ~doc:"Output format"
      and
        create_dumps = flag "-dump" (optional bool) ~doc:"Generate AST dumps in JSON format"
      and
        quiet = flag "-quiet" (optional bool) ~doc:"Print only error messages."
      and
        files = anon (sequence ("filename" %: Core.Filename.arg_type))
      in
      fun () ->
        let fmt = match output_format with
          | Some s -> (
              if String.equal s "json" then
                WO.Json
              else if String.equal s "plain" then
                WO.Plain
              else (
                Printf.eprintf "Unknown output format '%s'!\n\n" s;
                Printf.eprintf "Supported formats:\n" ;
                Printf.eprintf "  plain\n" ;
                Printf.eprintf "  json\n" ;
                exit 22
              ))
          | None -> WO.Plain
        in
        let create_dumps = match create_dumps with
          | Some v -> v
          | None -> false
        in
        let quiet = match quiet with
          | Some v -> v
          | None -> false
        in
        match files with
        | [] -> (
            Printf.eprintf "No input files\n";
            exit 1)
        | _ -> List.iter files ~f:(fun f -> run_checker f fmt create_dumps quiet)
    )

let () =
  Core.Command.run command
