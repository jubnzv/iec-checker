(** PLCOPEN-L17 – Each IF instruction should have an ELSE clause

    Reference: PLCopen Coding Guidelines 6.5.9. *)

PROGRAM program0
  VAR
    a : INT;
  END_VAR

  IF (a = 42) (* PLCOPEN-L17 *)
    THEN
      a := 0;
  END_IF;

  IF (a = 42) (* no warning *)
    THEN
      a := 0;
    ELSE
      a := 19;
  END_IF;

END_PROGRAM

