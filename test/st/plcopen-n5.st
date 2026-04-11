PROGRAM prog0
  VAR
    local0 : INT;
  END_VAR
  local0 := 0;
END_PROGRAM

FUNCTION_BLOCK Example
  VAR
    MyCalculation : REAL;   (* PLCOPEN-N5 *)
  END_VAR
  MyCalculation := 0.0;
END_FUNCTION_BLOCK

CONFIGURATION config
  VAR_GLOBAL
    MyCalculation : REAL;
  END_VAR
  TASK task0(INTERVAL := T#20ms, PRIORITY := 0);
  PROGRAM instance0 WITH task0 : prog0;
END_CONFIGURATION
