(* This file contains variables with valid type definitions. *)
PROGRAM program0
  VAR
    (* character strings *)
    s : STRING;
    s1 : STRING[5];
    ws : WSTRING;
    ws1 : WSTRING[5];
    c : CHAR;
    wc : WCHAR;
  END_VAR

  a := 0;
  s := "foo";
  s1 := "bar";

END_PROGRAM
(* vim: set foldmethod=marker foldlevel=0 foldenable sw=2 tw=120 : *)
