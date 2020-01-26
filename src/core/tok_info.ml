open Core_kernel

type t = { id : int; linenr : int; col: int }

let next_id =
  let n = ref (-1) in
  fun () ->
    incr n;
    !n

let create lexbuf =
  let id = next_id () in
  let linenr = lexbuf.Lexing.lex_curr_p.pos_lnum in
  let col = lexbuf.Lexing.lex_curr_p.pos_bol in
  { id; linenr; col }

