open Core_kernel
open IECCheckerCore
open IECCheckerParser
module S = Syntax
module TI = Tok_info

module Driver : sig
  val parse_file : string -> S.iec_library_element list
  (** Parse content of a file with given path. *)

  val parse_string : string -> S.iec_library_element list
  (** Parse content of a given string. *)
end = struct
  (* Parse lexbuf using given Parser rule. *)
  let parse ?(parser_rule = Parser.main) lexbuf =
    let tokinfo lexbuf = TI.create lexbuf in
    let l = Lexer.initial tokinfo in
    parser_rule l lexbuf

  let parse_file fpath =
    let inx = In_channel.create fpath in
    let lexbuf = Lexing.from_channel inx in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fpath };
    let els = parse lexbuf in
    In_channel.close inx;
    els

  let parse_string str =
    let lexbuf = Lexing.from_string str in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "" };
    parse lexbuf
end

let test_program_declaration () =
  let programs1 =
    Driver.parse_string
      "PROGRAM program0 VAR f : DINT; END_VAR f := 42; END_PROGRAM"
  in
  let do_check els =
    Alcotest.(check int) "number of programs" 1 (List.length els);
    Alcotest.(check string)
      "name of program" "program0"
      ( match els with
      | e1 :: _ -> ( match e1 with S.IECProgram p0 -> p0.name | _ -> "error" )
      | _ -> "error" )
  in
  let rec do_all programs =
    match programs with
    | [] -> ()
    | h :: t ->
        do_check h;
        do_all t
  in
  do_all [ programs1 ]

let test_fb_declaration () =
  let fbs1 =
    Driver.parse_string
      "\n\
      \    FUNCTION_BLOCK fb0\n\
      \    END_FUNCTION_BLOCK\n\n\
      \    FUNCTION_BLOCK fb1\n\
      \      VAR\n\
      \        v1 : DINT;\n\
      \      END_VAR\n\
      \    END_FUNCTION_BLOCK\n\n\
      \    FUNCTION_BLOCK fb2\n\
      \      VAR\n\
      \        v1 : DINT;\n\
      \      END_VAR\n\
      \      v1 := 0;\n\
      \    END_FUNCTION_BLOCK\n\n\
      \    FUNCTION_BLOCK fb3\n\
      \      VAR_INPUT\n\
      \        vi1 : DINT;\n\
      \      END_VAR\n\
      \      VAR_OUTPUT\n\
      \        vo1 : DINT;\n\
      \      END_VAR\n\
      \      VAR_IN_OUT\n\
      \        vio1 : DINT;\n\
      \      END_VAR\n\
      \      VAR\n\
      \        v1 : DINT;\n\
      \      END_VAR\n\
      \      VAR_TEMP\n\
      \        vt1 : DINT;\n\
      \      END_VAR\n\
      \      VAR RETAIN\n\
      \        vr1 : DINT;\n\
      \      END_VAR\n\
      \      VAR NON_RETAIN\n\
      \        vnr1 : DINT;\n\
      \      END_VAR\n\
      \      v1 := 42;\n\
      \    END_FUNCTION_BLOCK\n\
      \    "
  in
  let do_check els =
    Alcotest.(check int) "number of fbs" 4 (List.length els);
    Alcotest.(check string)
      "name of first fb" "fb0"
      ( match els with
      | e1 :: _ -> (
          match e1 with
          | S.IECFunctionBlock fb0 -> S.FunctionBlock.get_name fb0.id
          | _ -> "error" )
      | _ -> "error" )
  in
  let rec do_all fbs =
    match fbs with
    | [] -> ()
    | h :: t ->
        do_check h;
        do_all t
  in
  do_all [ fbs1 ]

(** All ST demo programs in test/st directory should be checked without syntax
    errors. *)
let test_syntax_errors_in_demo_programs () =
  let ls dir =
    Sys.readdir dir |> Array.to_list |> List.map ~f:(Filename.concat dir)
  in
  let files = ls "../../../test/st/" in
  let rec check_all files =
    match files with
    | [] -> ()
    | h :: t ->
        let _ = Driver.parse_file h in
        ();
        check_all t
  in
  check_all files

let () =
  let open Alcotest in
  run "Parser"
    [
      ( "test-program-declaration",
        [ test_case " " `Quick test_program_declaration ] );
      ("test-fb-declaration", [ test_case " " `Quick test_fb_declaration ]);
      ( "test-syntax-errors-in-demo-programs",
        [ test_case " " `Quick test_syntax_errors_in_demo_programs ] );
    ]
