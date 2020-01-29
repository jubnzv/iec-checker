open Core_kernel
open IECCheckerCore
open IECCheckerParser
open IECCheckerLib

module S = Syntax
module C = CheckerLib
module TI = Tok_info

let read_all filename =
  let chan = open_in filename in
  try
    let res = really_input_string chan (in_channel_length chan) in
    let _ = close_in chan in
    res
  with e ->
    close_in_noerr chan;
    raise e

let print_position outx (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)

let parse_with_error lexbuf =
  let tokinfo lexbuf = TI.create lexbuf in
  let l = Lexer.initial tokinfo in
  try Parser.main l lexbuf with
  | Lexer.LexingError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg;
      []
  | Parser.Error ->
      Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
      []
  | Failure msg ->
      Printf.fprintf stderr "%a: %s-n" print_position lexbuf msg;
      []

let parse_elements lexbuf =
  let node = parse_with_error lexbuf in
  match node with elements -> elements

let parse_file (filename : string) : S.iec_library_element list =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let elements = parse_elements lexbuf in
  In_channel.close inx;
  elements

let _ =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Usage: %s <file>\n" Sys.argv.(0);
    exit 1 )
  else
    let filename = Sys.argv.(1) in
    if not (Sys.file_exists filename) then
      failwith ("File " ^ filename ^ " doesn't exists")
    else
      let elements = parse_file filename in
      C.run_all_checks elements
