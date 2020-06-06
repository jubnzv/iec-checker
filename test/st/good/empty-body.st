(* Allow POU that contains only {} in their bodies. *)
FUNCTION EmptyFun : BOOL
  {}
END_FUNCTION

FUNCTION_BLOCK EmptyFB
  {}
END_FUNCTION_BLOCK

PROGRAM EmptyProgram
  {}
END_PROGRAM
