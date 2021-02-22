PROGRAM l10
  VAR
    i : INT := 0;
    j : INT := 0;
    flag : INT := 1;
    counter : INT := 0;
  END_VAR

  FOR i := 0 TO 10 DO
    FOR j := 10 TO 100 BY 2 DO
      IF flag THEN
        EXIT; (* PLCOPEN-CP10 *)
      END_IF;
      counter := counter + 1;
      IF j = 10 THEN
        CONTINUE; (* PLCOPEN-CP10 *)
      END_IF;
      EXIT; (* PLCOPEN-CP10 *)
    END_FOR;
  END_FOR;
END_PROGRAM

