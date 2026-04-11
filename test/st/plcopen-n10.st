TYPE
  Color : (Red, Green, Blue);   (* PLCOPEN-N10 *)
END_TYPE

TYPE
  E_Direction : (North, South, East, West);
END_TYPE

FUNCTION_BLOCK Motor   (* PLCOPEN-N10 *)
  VAR
    speed : INT;
  END_VAR
  speed := 0;
END_FUNCTION_BLOCK

FUNCTION_BLOCK FB_Pump
  VAR
    flow : REAL;
  END_VAR
  flow := 0.0;
END_FUNCTION_BLOCK
