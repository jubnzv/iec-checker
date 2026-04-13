CONFIGURATION config
  VAR_GLOBAL
    SharedCounter : INT;
    ReadOnlyVar : INT;
    MultiWriter : INT;
  END_VAR
  TASK task0(INTERVAL := T#20ms, PRIORITY := 0);
  PROGRAM instance0 WITH task0 : Program1;
END_CONFIGURATION

(* Case 1: SharedCounter written by Program1 and Program2 — violation in Program2 *)
PROGRAM Program1
  VAR_EXTERNAL SharedCounter : INT; END_VAR
  VAR_EXTERNAL ReadOnlyVar : INT; END_VAR
  VAR x : INT; END_VAR
  SharedCounter := SharedCounter + 1;
  x := ReadOnlyVar;
END_PROGRAM

PROGRAM Program2
  VAR_EXTERNAL SharedCounter : INT; END_VAR
  VAR_EXTERNAL ReadOnlyVar : INT; END_VAR
  VAR x : INT; END_VAR
  SharedCounter := 0;
  x := ReadOnlyVar;
END_PROGRAM

(* Case 2: MultiWriter written by Program3 and Program4 — violation in Program4 *)
PROGRAM Program3
  VAR_EXTERNAL MultiWriter : INT; END_VAR
  MultiWriter := 1;
END_PROGRAM

PROGRAM Program4
  VAR_EXTERNAL MultiWriter : INT; END_VAR
  MultiWriter := 2;
END_PROGRAM
