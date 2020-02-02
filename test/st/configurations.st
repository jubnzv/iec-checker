(* Demo program with all possible TASK entries.
   Created with Beremiz IDE (https://beremiz.org/). *)
FUNCTION function0 : BOOL
  VAR
    LocalVar0 : DINT;
  END_VAR

  LocalVar0 := 42;
END_FUNCTION

PROGRAM program0
  VAR
    LocalVar0 : DINT;
    LocalVar1 : TOD;
  END_VAR

  LocalVar0 := 42;
END_PROGRAM

PROGRAM program1
  VAR
    LocalVar0 : DINT;
  END_VAR

  LocalVar0 := 0x42;
END_PROGRAM


CONFIGURATION config
  VAR_GLOBAL
    ConfVar0 : DINT;
    ConfVar1 : DINT;
    ConfVar2 : DINT;
  END_VAR

  RESOURCE resource1 ON PLC
    VAR_GLOBAL
      ResVar0 : DINT;
      ResVar1 : BOOL;
    END_VAR
    TASK task0(INTERVAL := T#20ms,PRIORITY := 0);
    TASK task1(SINGLE := ResVar1,PRIORITY := 0);
    TASK task2(INTERVAL := T#2h0m0s3ms,PRIORITY := 1);
    PROGRAM instance0 WITH task0 : program0;
    PROGRAM instance1 WITH task1 : program1;
    PROGRAM instance2 WITH task2 : program1;
  END_RESOURCE
END_CONFIGURATION
