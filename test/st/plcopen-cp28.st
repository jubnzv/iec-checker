PROGRAM demo
    VAR
        x : TIME;
    END_VAR

    IF x = T#100MS THEN (* PLCOPEN CP-28 *)
      x := x + 1;
    END_IF;

    IF x <> T#100MS THEN (* PLCOPEN CP-28 *)
      x := x + 1;
    END_IF;

    IF T#100MS = x THEN (* PLCOPEN CP-28 *)
      x := x + 1;
    END_IF;

    IF T#100MS <> x THEN (* PLCOPEN CP-28 *)
      x := x + 1;
    END_IF;

    IF REAL_TO_INT(x) <> 0 THEN
      x := x + 1;
    END_IF;
END_PROGRAM

