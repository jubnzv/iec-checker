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

(** Where to fetch the original source text for error snippets. *)
type source_origin =
  | SrcFile of string
  | SrcString of string
  | SrcNone

(** Fetch a specific 1-indexed line from the parse source for snippet display. *)
let fetch_source_line origin line =
  if line <= 0 then None
  else match origin with
    | SrcNone -> None
    | SrcString s ->
      let lines = String.split_lines s in
      List.nth lines (line - 1)
    | SrcFile filename ->
      try
        let ic = In_channel.create filename in
        let rec nth n =
          match In_channel.input_line ic with
          | None -> None
          | Some l -> if n = 1 then Some l else nth (n - 1)
        in
        let result = nth line in
        In_channel.close ic;
        result
      with _ -> None

(** Render a source snippet with a caret pointing at [col_start .. col_end]
    (1-indexed, inclusive/exclusive). *)
let render_snippet ~line ~col_start ~col_end src =
  let col_start = Int.max 1 col_start in
  let col_end = Int.max (col_start + 1) col_end in
  let gutter = Printf.sprintf "%d" line in
  let gw = String.length gutter in
  let pad = String.make gw ' ' in
  let caret_pad = String.make (col_start - 1) ' ' in
  let carets = String.make (col_end - col_start) '^' in
  Printf.sprintf "  %s |\n  %s | %s\n  %s | %s%s"
    pad gutter src pad caret_pad carets

let snippet_from_lexbuf origin (lexbuf : Lexing.lexbuf) =
  let start_p = lexbuf.Lexing.lex_start_p in
  let curr_p = lexbuf.Lexing.lex_curr_p in
  let line = start_p.pos_lnum in
  let col_start = start_p.pos_cnum - start_p.pos_bol + 1 in
  let col_end = curr_p.pos_cnum - curr_p.pos_bol + 1 in
  match fetch_source_line origin line with
  | Some src -> render_snippet ~line ~col_start ~col_end src
  | None -> ""

let parser_error_message (lexbuf : Lexing.lexbuf) =
  let tok = Lexing.lexeme lexbuf in
  let tok_desc =
    if String.is_empty tok then "end of input"
    else Printf.sprintf "`%s`" tok
  in
  Printf.sprintf "unexpected token %s" tok_desc

let parse_with_error ?(origin=SrcNone) (lexbuf: Lexing.lexbuf) : parse_results =
  let tokinfo lexbuf = TI.create lexbuf in
  let l = Lexer.initial tokinfo in
  try (Parser.main l lexbuf), [] with
  | Lexer.LexingError msg ->
    let ctx = snippet_from_lexbuf origin lexbuf in
    [], [(W.mk_from_lexbuf ~context:ctx lexbuf "LexingError" msg)]
  | Parser.Error ->
    let ctx = snippet_from_lexbuf origin lexbuf in
    [], [(W.mk_from_lexbuf ~context:ctx lexbuf "ParserError"
            (parser_error_message lexbuf))]
  | e ->
    [], [(W.mk_from_lexbuf lexbuf "UnknownError" (Exn.to_string e))]

let parse_stdin () : parse_results option =
  match In_channel.input_line In_channel.stdin with
  | None -> None
  | Some code -> begin
      let lexbuf = Lexing.from_string code in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "stdin" };
      Some(parse_with_error ~origin:(SrcString code) lexbuf)
    end

let parse_st_file (filename : string) : parse_results =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let (elements, warns) = parse_with_error ~origin:(SrcFile filename) lexbuf in
  In_channel.close inx;
  (elements, warns)

let parse_xml_file (filename : string) : parse_results =
  let inx = In_channel.create filename in
  let program = Plcopen.reconstruct_from_channel inx in
  In_channel.close inx;
  let lexbuf = Lexing.from_string program in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let (elements, warns) = parse_with_error ~origin:(SrcString program) lexbuf in
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
      Some(parse_with_error ~origin:(SrcString program) lexbuf)
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
    | f::_ when (Stdlib.Sys.file_exists f &&
                 Stdlib.Sys.is_directory f) -> begin
        Stdlib.Sys.readdir f
        |> Array.to_list
        |> List.map ~f:(Filename.concat f)
        |> List.fold_left
          ~init:[]
          ~f:(fun acc p -> begin
                if Stdlib.Sys.is_directory p then
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

(** Apply [exclude_paths] globs from the configuration to filter out files. *)
let apply_exclude_paths paths =
  let cfg = Config.get () in
  match cfg.exclude_paths with
  | [] -> paths
  | pats ->
    let exclude_res = List.map pats ~f:(fun pat ->
      Re.compile (Re.Glob.glob pat)) in
    List.filter paths ~f:(fun p ->
      not (List.exists exclude_res ~f:(fun re -> Re.execp re p)))

(** Collects paths to files that should be parsed.
    If there are directories among [paths], this functions recursively
    traverses them and collects nested files there. *)
let collect_paths paths in_fmt =
  if List.exists paths ~f:(fun p -> String.equal p "-") then
    ["-"]
  else
    get_files_to_check paths in_fmt |> apply_exclude_paths

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
  if Stdlib.Sys.file_exists path then
    Stdlib.Sys.remove path

let cleanup out_path = remove_file out_path

(** Merges contents of files [paths] creating a temporary file [out_path]. *)
let merge_files paths out_path =
  remove_file out_path;
  let oc = Out_channel.create ~append:true ~fail_if_exists:true ~perm:0o755 out_path in
  List.iter paths ~f:(fun path ->
    Out_channel.output_string oc (In_channel.read_all path));
  Out_channel.close_no_err oc

(** Check whether an analysis pass is enabled in the current configuration. *)
let pass_enabled id =
  let cfg = Config.get () in
  match cfg.enabled_detectors with
  | _ :: _ as ids -> List.mem ids id ~equal:String.equal
  | [] -> not (List.mem cfg.disabled_detectors id ~equal:String.equal)

(** [run_checker] Run program on the file with [path] and returns the
    error code. *)
let doc_urls =
  List.map Lib.registered_detectors ~f:(fun d ->
    (d.Detector.id, d.Detector.doc_url))

let stamp_file path ws =
  List.map ws ~f:(fun w ->
    if String.is_empty w.W.file then { w with W.file = path } else w)

let run_checker path in_fmt out_fmt create_dumps merged verbose (interactive : bool) use_color : int =
  let (read_stdin : bool) = (String.equal "-" path) || (String.is_empty path) in
  if (not read_stdin && not (Stdlib.Sys.file_exists path)) then
    let err =
      W.mk_internal ~id:"FileNotFoundError"
        (Printf.sprintf "File %s doesn't exists" path)
    in
    WO.print_report ~use_color [err] out_fmt;
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
        let decl_warns =
          if pass_enabled "DeclarationAnalysis"
          then Declaration_analysis.run elements envs else [] in
        let unused_warns =
          if pass_enabled "UnusedVariable"
          then Unused_variable.run elements else [] in
        let ud_warns =
          if pass_enabled "UseDefine"
          then Use_define.run elements else [] in
        let lib_warns = Lib.run_all_checks elements envs cfgs (not verbose) in
        WO.print_report ~doc_urls ~use_color (
          stamp_file path parser_warns @
          stamp_file path decl_warns @
          stamp_file path unused_warns @
          stamp_file path ud_warns @
          stamp_file path lib_warns)
          out_fmt;
        if List.is_empty parser_warns then ReturnCode.ok else ReturnCode.fail
      end

let create_file path =
  Out_channel.create ~perm:0o755 path |> Out_channel.close_no_err

(** Built-in analysis passes (not in the detector registry). *)
let builtin_passes = [
  ("DeclarationAnalysis", "Check variable declarations");
  ("UnusedVariable",      "Detect unused local variables");
  ("UseDefine",           "Use-define chain analysis (array bounds)");
]

(** Print every registered detector to stdout, one per line, padded for
    readability. Used by [--list-checks]. *)
let print_list_checks () =
  let detectors = Lib.registered_detectors in
  let all_ids =
    List.map detectors ~f:(fun d -> (d.Detector.id, d.Detector.name))
    @ builtin_passes
  in
  let id_width =
    List.fold_left all_ids ~init:0 ~f:(fun acc (id, _) ->
      Int.max acc (String.length id))
  in
  List.iter all_ids ~f:(fun (id, name) ->
    Printf.printf "%-*s  %s\n" id_width id name);
  Printf.printf "\n%d check(s). See <https://iec-checker.github.io/docs/detectors/> for details.\n"
    (List.length all_ids)

(** Parse the input format string into [input_format_ty]. *)
let parse_input_format s =
  match s with
  | s when String.equal "st" s -> InputST
  | s when String.equal "xml" s -> InputXML
  | s when String.equal "selxml" s -> InputSELXML
  | s ->
    Printf.eprintf "Unknown input format '%s'.\n" s;
    Printf.eprintf "Available formats: 'st', 'xml' and 'selxml'\n";
    exit ReturnCode.fail

(** Parse the output format string into [WO.output_format]. *)
let parse_output_format s =
  match s with
  | s when String.equal "plain" s -> WO.Plain
  | s when String.equal "json" s -> WO.Json
  | s ->
    Printf.eprintf "Unknown output format '%s'. Supported: 'plain' and 'json'.\n" s;
    exit ReturnCode.fail

(** Load configuration from file (explicit path or auto-discovery) and merge
    with CLI overrides.  Returns the final [Config.t]. *)
let load_and_merge_config ~config_path ~cli_input_format ~cli_output_format
    ~cli_dump ~cli_merge ~cli_verbose ~cli_no_color =
  (* 1. Load base config *)
  let base = match config_path with
    | Some path -> begin
        match Config.load_file path with
        | Ok c -> c
        | Error msg ->
          Printf.eprintf "Error: %s\n" msg;
          exit ReturnCode.fail
      end
    | None -> begin
        match Config.find_config_file (Stdlib.Sys.getcwd ()) with
        | Some path -> begin
            match Config.load_file path with
            | Ok c -> c
            | Error msg ->
              Printf.eprintf "Error: %s\n" msg;
              exit ReturnCode.fail
          end
        | None -> Config.default
      end
  in
  (* 2. Override with explicitly-provided CLI args *)
  let c = match cli_input_format with
    | Some s -> { base with input_format = s }
    | None -> base
  in
  let c = match cli_output_format with
    | Some s -> { c with output_format = s }
    | None -> c
  in
  let c = if cli_dump then { c with dump = true } else c in
  let c = if cli_merge then { c with merge = true } else c in
  let c = if cli_verbose then { c with verbose = true } else c in
  let c = if cli_no_color then { c with use_color = false } else c in
  c

let () =
  Clap.description "Static analysis of IEC 61131-3 programs ";

  let cli_input_format =
    Clap.optional_string
      ~short: 'i'
      ~long: "input-format"
      ~description:
        "Format of the input files. Supported formats:
          + st - Structured Text source
          + xml - PLCOpen XML
          + selxml - Schweitzer Engineering Laboratories XML"
      ~placeholder: "INPUT_FORMAT"
      ()
  in

  let cli_output_format =
    Clap.optional_string
      ~short: 'o'
      ~long: "output-format"
      ~description:
        "Output format for the checker messages. Supported formats: 'plain' and 'json'."
      ~placeholder: "OUTPUT_FORMAT"
      ()
  in

  let config_path =
    Clap.optional_string
      ~short: 'c'
      ~long: "config"
      ~description:
        "Path to configuration file (iec_checker.json). \
         If not specified, iec-checker looks for iec_checker.json in the current \
         directory and its parents."
      ~placeholder: "CONFIG_FILE"
      ()
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

  let no_color =
    Clap.flag
      ~set_long: "no-color"
      ~description: "Disable colored output."
      false
  in

  let list_checks =
    Clap.flag
      ~set_long: "list-checks"
      ~description:
        "List every registered detector (id and human-readable name) and exit. \
         No input files are required."
      false
  in

  let dump_config =
    Clap.flag
      ~set_long: "dump-config"
      ~description:
        "Print the effective configuration as JSON and exit."
      false
  in

  let generate_config =
    Clap.flag
      ~set_long: "generate-config"
      ~description:
        "Write a default iec_checker.json to the current directory and exit."
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

  (* --generate-config: write defaults and exit *)
  if generate_config then begin
    let oc = Out_channel.create "iec_checker.json" in
    let json = Config.to_yojson Config.default in
    Yojson.Safe.pretty_to_channel oc json;
    Out_channel.output_char oc '\n';
    Out_channel.close oc;
    exit ReturnCode.ok
  end;

  (* --list-checks: print detectors and exit *)
  if list_checks then begin
    print_list_checks ();
    exit ReturnCode.ok
  end;

  (* Load and merge configuration *)
  let cfg = load_and_merge_config
    ~config_path
    ~cli_input_format
    ~cli_output_format
    ~cli_dump:d
    ~cli_merge:m
    ~cli_verbose:v
    ~cli_no_color:no_color
  in
  Config.set cfg;

  (* --dump-config: print effective config and exit *)
  if dump_config then begin
    Config.to_yojson (Config.get ())
    |> Yojson.Safe.pretty_to_string
    |> print_endline;
    exit ReturnCode.ok
  end;

  (* Resolve effective values from merged config *)
  let input_format = parse_input_format cfg.input_format in
  let output_format = parse_output_format cfg.output_format in
  let use_color = cfg.use_color in
  let create_dumps = cfg.dump in
  let verbose = cfg.verbose in
  let do_merge = cfg.merge in

  if List.is_empty paths then begin
    Printf.eprintf "No input files!\n\n";
    Clap.help ();
    exit ReturnCode.fail
  end

  else
      let paths' = collect_paths paths input_format in
      (* Disable the merge option if there is only one input file. *)
      let do_merge = if do_merge && phys_equal 1 (List.length paths') then false else do_merge in
      let success =
        if do_merge then (
          (* Merge all the input files to the single file and analyze it. *)
          remove_file merged_file_path;
          create_file merged_file_path;
          merge_files paths merged_file_path;
          let rc = run_checker merged_file_path input_format output_format create_dumps do_merge verbose i use_color in
          remove_file merged_file_path;
          phys_equal rc ReturnCode.ok)
        else (
          (* Run the checker for each file and collect all the warnings. *)
          List.fold_left paths'
            ~f:(fun return_codes f -> return_codes @ [run_checker f input_format output_format create_dumps do_merge verbose i use_color])
            ~init:[]
          |> List.for_all ~f:(phys_equal ReturnCode.ok))
      in
      if success
      then exit ReturnCode.ok else exit ReturnCode.fail
