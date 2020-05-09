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
