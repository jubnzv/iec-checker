(* This file contains variables with valid type definitions. *)
TYPE
  (* Character strings *)
  S : STRING;
  S1 : STRING[5];
  WS : WSTRING;
  WS1 : WSTRING[5];
  WS1 : WSTRING[25] := 'spaces_disallowed';
  C : CHAR;
  WC : WCHAR;

  (* Subrange types *)
  ANALOG_DATA : INT (-4095 .. 4095);

  (* Enum types *)
  Traffic_Light: (Red, Amber, Green);
  Painting_colors: (Red, Yellow, Blue) := Blue;

  (* Array types *)
  BITS: ARRAY [0..7] OF BOOL := [0,1,1,0,0,1,0,0];

  (* Struct types *)
  ANALOG_CHANNEL_CONFIGURATION: STRUCT
    RANGE: ANALOG_SIGNAL_RANGE;
    MIN_SCALE: INT := -4095;
    MAX_SCALE: INT := 4095;
  END_STRUCT;
  Cooler: STRUCT
    Temp: INT;
    Cooling: TOF;
  END_STRUCT;
  Com1_data: STRUCT
    head AT %B0 : INT;
    length AT %B2 : USINT := 26;
    flag1 AT %X3.0 : BOOL;
    end AT %B25 : BYTE;
  END_STRUCT;
END_TYPE

PROGRAM arr_demo
  VAR
    myAnalog_16: ARRAY [1..16] OF DINT;
    TBT1 : ARRAY [1..2, 1..3] OF INT;
    BITS: ARRAY [0..7] OF BOOL := [0,1,1,0,0,1,0,0];
  END_VAR
  myAnalog_16[0] := 3;
END_PROGRAM
