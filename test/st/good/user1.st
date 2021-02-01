(* a simple ST program that call a user defined function, and executes a FOR loop *)
PROGRAM prog1
    
	VAR_TEMP RETAIN

		I, Z : INT ;
		INITIAL : BOOL := FALSE;
		X1 : INT := 10;
		Y1 : INT := 20;
		R1 : REAL := 3.0;
		A : BOOL := TRUE;
		DI1 : BOOL := FALSE;

		I_I1 AT %MW10.2.4.1 : INT;
		I_I2 AT %MW11 : INT;
		O_I1 AT %QW63.1 : INT;

	END_VAR

  (* currently we allow GLOBAL blocks inside PROGRAMS *)
	VAR_GLOBAL
		G1: BOOL;
	END_VAR
	
	IF INITIAL = FALSE THEN
		I_I2 := 100;
		INITIAL := TRUE;
	END_IF;
		
	Z := func1(X1, Y1); // user defined function
		
	A := A AND G1;
	
	G1 := NOT G1; // changes in each run
	
	Y1 := Z + 5;
				
	FOR I := 1 TO 3 DO
		R1 := SQRT(R1);
		X1 := I;
	END_FOR;
	
	Y1 := Z + 1;
	O_I1 := Y1;
	
	
END_PROGRAM
