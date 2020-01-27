(* Test lexing/parsing of integer literals defined in Table 5. *)
PROGRAM program0
  VAR
    a : DWORD;
  END_VAR

  (* integer *)
  a := 3;
  a := 3_000;
  a := 3_000_000;
  a := 3_00_0000;

  (* binary_integer *)
  a := 2#1;
  a := 2#010110;
  a := 2#11_011_0;

  (* octal_integer *)
  a := 8#7;
  a := 8#71_061_0;

  (* hex_integer *)
  a := 16#0;
  a := 16#10;
  a := 16#A;
  a := 16#A_B_C;

END_PROGRAM

