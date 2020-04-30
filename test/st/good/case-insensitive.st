(* Keywords and identifiers are case-insensitive *)
Program simple
  VaR
    v01 : DiNt;
  eND_var

  v01 := 92;
  V01 := 92;
END_PROGRAM

PRoGRAM square_root
  var
    a, b, c, d : REAL;
    x1, x2 : REaL;
    nroots : InT;
  END_VAR

  d := b*b - 4.0*a;
  d := b*b - 4.0*a*c;
  IF d < 0.0
  THEN nroots := 0;
    ELSIF d = 0.0
    THEN
      nroots := 1;
      X1 := -b/(2.0*A);
      X1 := -b;
    ELSE
      nroots := 2;
      X1 := (-b + SQRT(d))/(2.0*a);
      X2 := (-b - SQRT(d))/(2.0*a);
  END_IF;

ENd_ProGRAM
