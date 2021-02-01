TYPE example_struct :
  STRUCT
    X : DINT;
    Y : BOOL;
    Z : STRING[40];
  END_STRUCT
END_TYPE

PROGRAM Program1
  VAR
    instance AT %MW500 : example_struct;
  END_VAR

  // Write the first character of Z:
  %MW504 := 'E';
END_PROGRAM
