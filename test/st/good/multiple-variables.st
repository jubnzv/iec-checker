PROGRAM program0
  VAR_ACCESS
    dummy : va1 : DINT;
    dummy : va2 : DINT;
    dummy : va3 : DINT;
  END_VAR
  VAR_INPUT
    vi1, vi2 : DINT;
    vi3, vi4, vi5 : DINT;
    vi6 : DINT;
  END_VAR
  VAR_OUTPUT
    vo1, vo2 : DINT;
    vo3, vo4, vo5 : DINT;
    vo6 : DINT;
  END_VAR
  VAR_IN_OUT
    vio1, vio2 : DINT;
    vio3, vio4, vio5 : DINT;
    vio6 : DINT;
  END_VAR
  VAR
    v1, v2 : DINT;
    v3, v4, v5 : DINT;
    v6 : DINT;
  END_VAR
  VAR_EXTERNAL
    ve1 : DINT;
    ve2 : DINT;
    ve3 : DINT;
  END_VAR
  VAR_TEMP
    vt1 : DINT;
    vt2 : DINT;
    vt3 : DINT;
  END_VAR
  VAR
    vinc1 AT %Q* : INT;
    vinc2 AT %I* : INT;
    vinc3 AT %M* : INT;
  END_VAR
  VAR
    v1 : DINT;
    v2 AT %M* : INT;
  END_VAR

  vi1 := vi1 / 1;
END_PROGRAM
