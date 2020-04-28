FUNCTION function0 : BOOL
  VAR
    LocalVar0 : DINT;
    LocalVar0_ : DINT;
  END_VAR
  LocalVar0 := LocalVar0 / 2;
END_FUNCTION

PROGRAM program0
  VAR_ACCESS
    acc : Var1 : DINT;
    acc : Var2 : DINT;
  END_VAR
  Var1 := 19 / 0;
  Var2 := Var1 / 1;
  Var2 := Var2 / 0;
END_PROGRAM

FUNCTION function1 : BOOL
  VAR
    LocalVar0 : DINT;
    LocalVar0_ : DINT;
  END_VAR
  LocalVar0 := LocalVar0 / 2;
END_FUNCTION

