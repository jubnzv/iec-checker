FUNCTION_BLOCK fb1
  VAR_INPUT
    vi1 : DINT;
  END_VAR
  v1 := 42;
END_FUNCTION_BLOCK

PROGRAM example
  VAR
    temp : DINT;
  END_VAR

  temp ?= temp^;
  temp ?= temp^^;
  temp ?= temp^^^;
  temp ?= NULL;
  temp ?= fb1;
END_PROGRAM
