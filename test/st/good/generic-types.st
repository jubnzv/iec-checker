(* Extended example from Beckhoff: https://infosys.beckhoff.com/english.php?content=../content/1033/tc3_plc_intro/18014401038986379.html&id= *)
FUNCTION F_AnyBitFunc : BOOL
VAR_INPUT
  value : ANY_BIT;
END_VAR
  value := 0;
END_FUNCTION

FUNCTION F_AnyDateFunc : BOOL
VAR_INPUT
    value : ANY_DATE;
END_VAR
  value := 0;
END_FUNCTION

FUNCTION F_AnyFunc : BOOL
VAR_INPUT
    value : ANY;
END_VAR
  value := 0;
END_FUNCTION

FUNCTION F_AnyIntFunc : BOOL
VAR_INPUT
    value : ANY_INT;
END_VAR
  value := 0;
END_FUNCTION

FUNCTION F_AnyNumFunc : BOOL
VAR_INPUT
    value : ANY_NUM;
END_VAR
  value := 0;
END_FUNCTION

FUNCTION F_AnyRealFunc : BOOL
VAR_INPUT
    value : ANY_REAL;
END_VAR
  value := 0;
END_FUNCTION

FUNCTION F_AnyStringFunc : BOOL
VAR_INPUT
    value : ANY_STRING;
END_VAR
  value := 0;
END_FUNCTION
