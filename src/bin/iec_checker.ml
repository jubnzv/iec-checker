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
  | InputST     (** Structured Text source code *)
  | InputXML    (** PLCOpen schemas *)
  | InputSELXML (** Schweitzer Engineering Laboratories XML Format*)

type parse_results = S.iec_library_element list * Warn.t list

let parse_with_error (lexbuf: Lexing.lexbuf) : parse_results =
  let tokinfo lexbuf = TI.create lexbuf in
  let l = Lexer.initial tokinfo in
  try (Parser.main l lexbuf), [] with
  | Lexer.LexingError msg ->
    [], [(W.mk_from_lexbuf lexbuf "LexingError" msg)]
  | Parser.Error ->
    [], [(W.mk_from_lexbuf lexbuf "ParserError" "")]
  | e ->
    [], [(W.mk_from_lexbuf lexbuf "UnknownError" (Exn.to_string e))]

let parse_stdin () : parse_results option =
  match In_channel.input_line stdin with
  | None -> None
  | Some code -> begin
      let lexbuf = Lexing.from_string code in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "stdin" };
      Some(parse_with_error lexbuf)
    end

let parse_st_file (filename : string) : parse_results =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let (elements, warns) = parse_with_error lexbuf in
  In_channel.close inx;
  (elements, warns)

let parse_xml_file (filename : string) : parse_results =
  let inx = In_channel.create filename in
  let program = Plcopen.reconstruct_from_channel inx in
  In_channel.close inx;
  let lexbuf = Lexing.from_string program in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let (elements, warns) = parse_with_error lexbuf in
  (elements, warns)

(** Parses an XML file located on [filepath]. If the
    file contains the valid XML, it returns parse results, otherwise None. *)
let parse_sel_xml_file (filepath : string) : parse_results option =
  let inx = In_channel.create filepath in
  let program_opt = Sel.reconstruct_from_channel_opt inx in
  In_channel.close inx;
  match program_opt with
  | Some(program) -> begin
      let lexbuf = Lexing.from_string program in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filepath };
      Some(parse_with_error lexbuf)
    end
  | None -> None

let endswith s1 s2 =
  let len1 = String.length s1 and len2 = String.length s2 in
  if len1 < len2 then false
  else
    let sub = String.sub s1 ~pos:(len1 - len2) ~len:(len2) in
    String.equal sub s2

(** Returns a list of the files to be checked. *)
let get_files_to_check paths in_fmt =
  (** Recursively traverses [path] and returns absolute paths to the files with
      the given [suffix]. *)
  let walkthrough_directory path suffix =
    let rec aux result = function
      | f::fs when (Sys.file_exists f && Sys.is_directory f) -> begin
          Caml.Sys.readdir f
          |> Array.to_list
          |> List.map ~f:(Filename.concat f)
          |> List.filter ~f:(fun n -> endswith n suffix)
          |> List.append fs
          |> aux result
        end
      | f::fs -> aux (f::result) fs
      | []    -> result
    in
    aux [] [path]
  in
  let suffix = match in_fmt with
    | InputST                -> ".st"
    | InputXML | InputSELXML -> ".xml"
  in
  List.fold_left
    paths
    ~init:[]
    ~f:(fun acc p -> acc @ walkthrough_directory p suffix)

(** Determines which files need to will be checked or how does iec-checker
    should be run. *)
let prepare_paths paths in_fmt =
  if List.exists paths ~f:(fun p -> String.equal p "-") then
    ["-"]
  else
    get_files_to_check paths in_fmt

(** Starts the iec-checker REPL. *)
let start_repl interactive =
  if interactive then Printf.printf "> ";
  flush stdout;
  parse_stdin ()

(** Parse the file with the given path. *)
let parse_file path in_fmt verbose : parse_results option =
  if verbose then Printf.printf "Parsing %s ...\n" path;
  match in_fmt with
  | InputST -> Some(parse_st_file path)
  | InputXML -> Some(parse_xml_file path)
  | InputSELXML -> begin
      parse_sel_xml_file path
    end

(** Runs the checker on the file with [path] and returns the error code. *)
let run_checker path in_fmt out_fmt create_dumps verbose interactive =
  let (read_stdin : bool) = (String.equal "-" path) || (String.is_empty path) in
  if (not (Sys.file_exists path) && not read_stdin) then
    let err = W.mk_internal ~id:"FileNotFoundError" (Printf.sprintf "File %s doesn't exists" path) in
    WO.print_report [err] out_fmt;
    127
  else
    let results_opt =
      if read_stdin then start_repl interactive
      else parse_file path in_fmt verbose
    in
    match results_opt with
    | None -> 0
    | Some(elements, parser_warns) -> begin
        let envs = Ast_util.create_envs elements in
        let cfgs = Cfg.create_cfgs elements in
        if create_dumps then
          Dump.create_dump elements envs cfgs
            (if read_stdin then "stdin" else path);
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
      end

let () =
  Clap.description "Static analysis of IEC 61131-3 programs ";

  let in_str =
    Clap.default_string
      ~short: 'i'
      ~long: "input-format"
      ~description:
        "Format of the input files. Supported formats:
          + st - Structured Text source
          + xml - PLCOpen XML
          + selxml - Schweitzer Engineering Laboratories XML"
      ~placeholder: "INPUT_FORMAT"
      "st"
  in
  let input_format = match in_str with
    | s when String.equal "st" s -> InputST
    | s when String.equal "xml" s -> InputXML
    | s when String.equal "selxml" s -> InputSELXML
    | s -> begin
        Printf.eprintf "Unknown input format '%s'.\n" s;
        Printf.eprintf "Avaialble formats: 'st', 'xml' and 'selxml'\n";
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

  let paths =
    Clap.list_string
      ~description:
        "Paths to source files or directories to check."
      ~placeholder: "PATHS"
      ()
  in

  Clap.close ();

  if List.is_empty paths then begin
    Printf.eprintf "No input files!\n\n";
    Clap.help ();
    exit 1
  end

  else if phys_equal 0
      begin
        prepare_paths paths input_format
        |> List.fold_left
            ~f:(fun return_codes f -> return_codes @ [run_checker f input_format output_format d v i])
            ~init:[]
        |> List.filter ~f:(fun rc -> not (phys_equal rc 0))
        |> List.length
      end
  then exit 0 else exit 1
