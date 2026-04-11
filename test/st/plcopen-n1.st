PROGRAM l10
  VAR
    a AT %MW10.2.4.1 : INT;
  END_VAR

   %MW10.2.4.1 := 42; (* PLCOPEN-N1 *)
   a := 42; (* ok *)
END_PROGRAM
