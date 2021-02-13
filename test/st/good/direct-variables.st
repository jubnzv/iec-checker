(* Examples of directly represented variables from ch. 6.5.5 *)
PROGRAM simple
  VAR
    v01 AT %IW215 : DINT;
    v02 AT %QB7 : DINT;
    v03 AT %MD48 : DINT;
    v04 AT %IX1 : DINT;
    v05 AT %I1 : DINT;
    v06 AT %IB2 : DINT;
    v07 AT %IW3 : DINT;
    v08 AT %ID4 : DINT;
    v09 AT %IL5 : DINT;
    v10 AT %IB0 : DINT;
    v11 AT %QX7.5 : DINT;
    v12 AT %MW1.7.9 : DINT;
    v13 AT %M* : DINT;
    v14 AT %MW10.2.4.1 : INT;
		v15 AT %MW11 : INT;
		v16 AT %MW12: INT;
  END_VAR

  v01 := 92;
END_PROGRAM
