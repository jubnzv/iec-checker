(* This file contains variables with valid type definitions. *)
TYPE
  (* Character strings *)
  S : STRING;
  S1 : STRING[5];
  WS : WSTRING;
  WS1 : WSTRING[5];
  WS1 : WSTRING[25] := 'spaces_disallowed';
  C : CHAR;
  WC : WCHAR;

  (* Subrange types *)
  ANALOG_DATA : INT (-4095 .. 4095);

END_TYPE
