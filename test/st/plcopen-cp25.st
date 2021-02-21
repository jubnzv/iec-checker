PROGRAM demo
    VAR
        I : INT := 10;
        J : REAL := 0.55;
    END_VAR

  I := J; (* PLCOPEN-CP25 *)
  J := I; (* PLCOPEN-CP25 *)
  I := REAL_TO_INT(J);
  J := INT_TO_REAL(I);
END_PROGRAM
