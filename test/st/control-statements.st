(* This file contains various control statements of the ST language. *)

(* Table 72 -- №4 *)
PROGRAM square_root
  VAR
    a, b, c, d : REAL;
    x1, x2 : REAL;
    nroots : INT;
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
      (* TODO: Standard library functions not yet implemented *)
      (*X1 := (-b + SQRT(D))/(2.0*A);
      X2 := (-b - SQRT(D))/(2.0*A);*)
  END_IF;

END_PROGRAM
(* vim: set foldmethod=marker foldlevel=0 foldenable sw=2 tw=120 : *)

