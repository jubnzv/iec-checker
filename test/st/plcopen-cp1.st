(** PLCOPEN-CP1 – Access to a member shall be by name *)

PROGRAM program0
  VAR
    head AT %B0 : INT;
  END_VAR
  %B0 := 42;
END_PROGRAM

