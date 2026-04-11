FUNCTION StartMotor : BOOL
  VAR
    goodVar : INT;
    BadVar  : INT;  (* PLCOPEN-N4 *)
  END_VAR
  StartMotor := TRUE;
END_FUNCTION

FUNCTION start_motor : BOOL    (* PLCOPEN-N4 *)
  start_motor := FALSE;
END_FUNCTION
