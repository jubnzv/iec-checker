(* This demo includes some declaration errors *)
PROGRAM program0
  VAR
    s1 : STRING[5] := 'foo';    (* OK *)
    s2 : STRING[5] := 'foobar'; (* Incorrect size *)
  END_VAR

  s1 := "bar";

END_PROGRAM
