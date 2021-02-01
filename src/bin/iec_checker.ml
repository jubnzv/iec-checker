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

(** Format of files given to checker *)
type input_format_ty =
  | InputST (** StructuredText source code *)
  | InputXML (** PLCOpen schemas *)

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

let parse_stdin () : (S.iec_library_element list * Warn.t list) =
  match In_channel.input_line stdin with
  | None -> ([], [W.mk_internal ~id:"Cancel" ""])
  | Some code -> (
      let lexbuf = Lexing.from_string code in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "stdin" };
      let (elements, warns) = parse_with_error lexbuf in
      (elements, warns))

let parse_st_file (filename : string) : (S.iec_library_element list * Warn.t list) =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let (elements, warns) = parse_with_error lexbuf in
  In_channel.close inx;
  (elements, warns)

let parse_xml_file (filename : string) : (S.iec_library_element list * Warn.t list) =
  let inx = In_channel.create filename in
  let program = Plcopen.reconstruct_from_channel inx in
  In_channel.close inx;
  let lexbuf = Lexing.from_string program in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let (elements, warns) = parse_with_error lexbuf in
  (elements, warns)

(** Run the checker and return the error code. *)
let run_checker filename in_fmt out_fmt create_dumps verbose interactive =
  let (read_stdin : bool) = (String.equal "-" filename) || (String.is_empty filename) in
  if (not (Sys.file_exists filename) && not read_stdin) then
    let err = W.mk_internal ~id:"FileNotFoundError" (Printf.sprintf "File %s doesn't exists" filename) in
    WO.print_report [err] out_fmt;
    127
  else
    let (elements, parser_warns) =
      if read_stdin then begin
        if interactive then Printf.printf "> ";
        flush stdout;
        parse_stdin ()
      end else begin
        if verbose then Printf.printf "Parsing %s ...\n" filename;
        match in_fmt with
        | InputST -> parse_st_file filename
        | InputXML -> parse_xml_file filename
      end
    in
    let envs = Ast_util.create_envs elements in
    let cfgs = Cfg.create_cfgs elements in
    if create_dumps then
      Dump.create_dump elements envs cfgs
        (if read_stdin then "stdin" else filename);
    let decl_warns = Declaration_analysis.run elements envs in
    let unused_warns = Unused_variable.run elements in
    let ud_warns = Use_define.run elements in
    let lib_warns = Lib.run_all_checks elements envs cfgs (not verbose) in
    WO.print_report (
      parser_warns @
      decl_warns @
      unused_warns @
      ud_warns @
      lib_warns)
      out_fmt;

    if not (List.is_empty parser_warns) then 1 else 0

let () =
  Clap.description "Static analysis of IEC 61131-3 programs ";

  let in_str =
    Clap.default_string
      ~short: 'i'
      ~long: "input-format"
      ~description:
        "Format of the input files. Supported formats: 'st' and 'xml'."
      ~placeholder: "INPUT_FORMAT"
      "st"
  in
  let input_format = match in_str with
    | s when String.equal "st" s -> InputST
    | s when String.equal "xml" s -> InputXML
    | s -> begin
        Printf.eprintf "Unknown input format '%s'. Supported: 'st and 'xml.\n" s;
        exit 1
      end
  in

  let of_str =
    Clap.default_string
      ~short: 'o'
      ~long: "output-format"
      ~description:
        "Output format for the checker messages. Supported formats: 'plain' and 'json'."
      ~placeholder: "OUTPUT_FORMAT"
      "plain"
  in
  let output_format = match of_str with
    | s when String.equal "plain" s -> WO.Plain
    | s when String.equal "json" s -> WO.Json
    | s -> begin
        Printf.eprintf "Unknown output format '%s'. Supported: 'plain' and 'json'.\n" s;
        exit 1
      end
  in

  let d =
    Clap.flag
      ~set_short: 'd'
      ~set_long: "dump"
      ~description:
        "Create dump files of the processed files in json format.\
         These files will contain the structure of the processed source files and \
         can be used from plugins and external tools."
      false
  in

  let v =
    Clap.flag
      ~set_short: 'v'
      ~set_long: "verbose"
      ~unset_short: 'q'
      ~unset_long: "quiet"
      ~description: "Show additional messages from the checker."
      false
  in

  let i =
    Clap.flag
      ~set_short: 'I'
      ~set_long: "interactive"
      ~unset_long: "non-interactive"
      ~description: "Accept input from stdin."
      false
  in

  let files =
    Clap.list_string
      ~description:
        "List of source files to check."
      ~placeholder: "FILE"
      ()
  in

  Clap.close ();

  if List.is_empty files then begin
    Printf.eprintf "No input files!\n\n";
    Clap.help ();
    exit 1
  end

  else if phys_equal 0
      begin
        List.fold_left files
          ~f:(fun return_codes f -> return_codes @ [run_checker f input_format output_format d v i])
          ~init:[]
        |> List.filter ~f:(fun rc -> not (phys_equal rc 0))
        |> List.length
      end
  then exit 0 else exit 1
