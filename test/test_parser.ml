open Core_kernel
open IECCheckerCore
open IECCheckerParser
module S = Syntax
module TI = Tok_info

module Driver = struct
  let print_position outx (lexbuf : Lexing.lexbuf) =
    let pos = lexbuf.lex_curr_p in
    Printf.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)

  let parse_with_error lexbuf =
    let tokinfo lexbuf = TI.create lexbuf in
    let l = Lexer.initial tokinfo in
    try Parser.main l lexbuf with
    | Lexer.SyntaxError msg ->
        fprintf stderr "%a: %s\n" print_position lexbuf msg;
        []
    | Parser.Error ->
        Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
        []
    | Failure msg ->
        Printf.fprintf stderr "%a: %s-n" print_position lexbuf msg;
        []

  let parse lexbuf =
    let tokinfo lexbuf = TI.create lexbuf in
    let l = Lexer.initial tokinfo in
    Parser.main l lexbuf

  let parse_and_print lexbuf : S.iec_library_element list = parse lexbuf

  let parse_file (filename : string) : S.iec_library_element list =
    let inx = In_channel.create filename in
    let lexbuf = Lexing.from_channel inx in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let els = parse_and_print lexbuf in
    In_channel.close inx;
    els

  let parse_string (text : string) : S.iec_library_element list =
    let lexbuf = Lexing.from_string text in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "" };
    parse_and_print lexbuf
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
    "
    FUNCTION_BLOCK fb0
    END_FUNCTION_BLOCK

    FUNCTION_BLOCK fb1
      VAR
        v1 : DINT;
      END_VAR
    END_FUNCTION_BLOCK

    FUNCTION_BLOCK fb2
      VAR
        v1 : DINT;
      END_VAR
      v1 := 0;
    END_FUNCTION_BLOCK

    FUNCTION_BLOCK fb3
      VAR_INPUT
        vi1 : DINT;
      END_VAR
      VAR_OUTPUT
        vo1 : DINT;
      END_VAR
      VAR_IN_OUT
        vio1 : DINT;
      END_VAR
      VAR
        v1 : DINT;
      END_VAR
      VAR_TEMP
        vt1 : DINT;
      END_VAR
      VAR RETAIN
        vr1 : DINT;
      END_VAR
      VAR NON_RETAIN
        vnr1 : DINT;
      END_VAR
      v1 := 42;
    END_FUNCTION_BLOCK
    "
  in
  let do_check els =
    Alcotest.(check int) "number of fbs" 4 (List.length els);
    Alcotest.(check string)
      "name of first fb" "fb0"
      ( match els with
      | e1 :: _ -> (
          match e1 with S.IECFunctionBlock fb0 -> (
              S.FunctionBlock.get_name fb0.id)
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

let () =
  let open Alcotest in
  run "Parser"
    [
      ( "test-program-declaration",
        [ test_case " " `Quick test_program_declaration ] );
      ( "test-fb-declaration",
        [ test_case " " `Quick test_fb_declaration ] );
    ]
