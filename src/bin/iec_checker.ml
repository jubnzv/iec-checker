open Core
open IECCheckerCore
open IECCheckerParser
open IECCheckerLib
open IECCheckerAnalysis
module S = Syntax
module Lib = CheckerLib
module TI = Tok_info
module W = Warn
module WO = Warn_output

(** Format of files given to the checker *)
type input_format_ty =
  | InputST     (** Structured Text source code *)
  | InputXML    (** PLCOpen schemes *)
  | InputSELXML (** Schweitzer Engineering Laboratories XML Format *)

type parse_results = S.iec_library_element list * Warn.t list

(** The path of the temporary file created when the `-m` option is set. *)
let merged_file_path = "merged-input.st"

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
  match In_channel.input_line In_channel.stdin with
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

(** [parse_sel_xml_file] Parse an SEL XML file located on [filepath]. If the
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

(** [walkthrough_directory] Recursively traverse [path] and return absolute
    paths to the files with the given [suffix]. *)
let walkthrough_directory path suffix =
  let rec aux result = function
    | f::_ when (Caml.Sys.file_exists f &&
                 Caml.Sys.is_directory f) -> begin
        Caml.Sys.readdir f
        |> Array.to_list
        |> List.map ~f:(Filename.concat f)
        |> List.fold_left
          ~init:[]
          ~f:(fun acc p -> begin
                if Caml.Sys.is_directory p then
                  acc @ (aux result [p])
                else if endswith p suffix then
                  acc @ [p]
                else
                  acc
              end)
        |> aux result
      end
    | f::fs -> aux (f::result) fs
    | []    -> result
  in
  aux [] [path]

(** [get_files_to_check] Return a list of the files to be checked. *)
let get_files_to_check paths in_fmt =
  let suffix = match in_fmt with
    | InputST                -> ".st"
    | InputXML | InputSELXML -> ".xml"
  in
  List.fold_left
    paths
    ~init:[]
    ~f:(fun acc p -> acc @ walkthrough_directory p suffix)

(** Collects paths to files that should be parsed.
    If there are directories among [paths], this functions recursively
    traverses them and collects nested files there. *)
let collect_paths paths in_fmt =
  if List.exists paths ~f:(fun p -> String.equal p "-") then
    ["-"]
  else
    get_files_to_check paths in_fmt

(** [start_repl] Start the iec-checker REPL. *)
let start_repl interactive =
  if interactive then Printf.printf "> ";
  Out_channel.flush stdout;
  parse_stdin ()

(** [parse_file] Parse file with the given path. *)
let parse_file path in_fmt verbose : parse_results option =
  if verbose then Printf.printf "Parsing %s ...\n" path;
  match in_fmt with
  | InputST -> Some(parse_st_file path)
  | InputXML -> Some(parse_xml_file path)
  | InputSELXML -> begin
      parse_sel_xml_file path
    end

module ReturnCode = struct
  let ok        = 0
  let fail      = 1
  let not_found = 127
end

let remove_file path =
  if Caml.Sys.file_exists path then
    Caml.Sys.remove path

let cleanup out_path = remove_file out_path

(** Merges contents of files [paths] creating a temporary file [out_path]. *)
let merge_files paths out_path =
  remove_file out_path;
  let oc = Out_channel.create ~append:true ~fail_if_exists:true ~perm:0o755 out_path in
  List.iter paths ~f:(fun path ->
    Out_channel.output_string oc (In_channel.read_all path));
  Out_channel.close_no_err oc

(** [run_checker] Run program on the file with [path] and returns the
    error code. *)
let run_checker path in_fmt out_fmt create_dumps merged verbose (interactive : bool) : int =
  let (read_stdin : bool) = (String.equal "-" path) || (String.is_empty path) in
  if (not read_stdin && not (Caml.Sys.file_exists path)) then
    let err =
      W.mk_internal ~id:"FileNotFoundError"
        (Printf.sprintf "File %s doesn't exists" path)
    in
    WO.print_report [err] out_fmt;
    ReturnCode.not_found
  else
    let results_opt =
      if read_stdin then start_repl interactive
      else parse_file path in_fmt verbose
    in
    match results_opt with
    | None -> ReturnCode.ok
    | Some(elements, parser_warns) -> begin
        let envs = Ast_util.create_envs elements in
        let cfgs = Cfg.create_cfgs elements in
        if create_dumps then (
          let src_file = (if read_stdin then "stdin"
                          else if merged then Filename.basename path
                               else path)
          in
          let dst_file = Printf.sprintf "%s.dump.json" src_file in
          Dump.create_dump ~dst_file elements envs cfgs);
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
        if List.is_empty parser_warns then ReturnCode.ok else ReturnCode.fail
      end

let create_file path =
  Out_channel.create ~perm:0o755 path |> Out_channel.close_no_err

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
        Printf.eprintf "Available formats: 'st', 'xml' and 'selxml'\n";
        exit ReturnCode.fail
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
        exit ReturnCode.fail
      end
  in

  let d =
    Clap.flag
      ~set_short: 'd'
      ~set_long: "dump"
      ~description:
        (Printf.sprintf
        "Create dump files of the processed files in the JSON format. \
         These files will contain the structure of the processed source files and \
         can be used from plugins and external tools. \
         Note: The dump file path will always be `%s` if the `-m` option is enabled."
         merged_file_path)
      false
  in

  let m =
    Clap.flag
      ~set_short: 'm'
      ~set_long: "merge"
      ~description:
        "Merge input files in the single file before running the checker. \
         This option is useful if the project is split into several files that \
         represent the same program. \
         Note: name collisions in the input files are forbidden."
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
    exit ReturnCode.fail
  end

  else
      let paths' = collect_paths paths input_format in
      (* Disable the merge option if there is only one input file. *)
      let m = if m && phys_equal 1 (List.length paths') then false else m in
      let success =
        if m then (
          (* Merge all the input files to the single file and analyze it. *)
          remove_file merged_file_path;
          create_file merged_file_path;
          merge_files paths merged_file_path;
          let rc = run_checker merged_file_path input_format output_format d m v i in
          remove_file merged_file_path;
          phys_equal rc ReturnCode.ok)
        else (
          (* Run the checker for each file and collect all the warnings. *)
          List.fold_left paths'
            ~f:(fun return_codes f -> return_codes @ [run_checker f input_format output_format d m v i])
            ~init:[]
          |> List.for_all ~f:(phys_equal ReturnCode.ok))
      in
      if success
      then exit ReturnCode.ok else exit ReturnCode.fail
