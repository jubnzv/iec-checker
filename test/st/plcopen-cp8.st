PROGRAM demo
    VAR
        x : REAL;
    END_VAR

    IF x = 0.0 THEN (* PLCOPEN CP-8 *)
      x := x + 1;
    END_IF;

    IF x <> 0.0 THEN (* PLCOPEN CP-8 *)
      x := x + 1;
    END_IF;

    IF 0.0 = x THEN (* PLCOPEN CP-8 *)
      x := x + 1;
    END_IF;

    IF 0.0 <> x THEN (* PLCOPEN CP-8 *)
      x := x + 1;
    END_IF;

    IF REAL_TO_INT(x) <> 0 THEN
      x := x + 1;
    END_IF;
END_PROGRAM

