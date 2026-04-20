FUNCTION_BLOCK FB_CP17_Test
VAR_INPUT
    in_ok_read_only       : INT;   // Compliant: read at least once, never written
    in_violate_unused     : INT;   // Violation: never read
    in_violate_write_only : INT;   // Violation: written, not read
    in_violate_read_write : INT;   // Violation: read but also written

    idx_read_only           : INT;   // Compliant: only read as array index
    idx_violate_write_only  : INT;   // Violation: written, not read
END_VAR

VAR_OUTPUT
    out_ok_written        : INT;   // Compliant: written at least once
    out_violate_unused    : INT;   // Violation: never written
    out_violate_read_only : INT;   // Violation: read but not written
END_VAR

VAR_IN_OUT
    io_ok_read_only       : INT;   // Compliant: read only
    io_ok_write_only      : INT;   // Compliant: written only
    io_ok_read_write      : INT;   // Compliant: both read and written
    io_violate_unused     : INT;   // Violation: neither read nor written
END_VAR

VAR
    temp : INT;
    arr  : ARRAY[0..9] OF INT;
END_VAR


(* ===== VAR_INPUT tests ===== *)

(* Compliant: input parameter is read *)
temp := in_ok_read_only + 1;

(* Violation: in_violate_unused is never used *)

(* Violation: input parameter is written *)
in_violate_write_only := 100;

(* Violation: input parameter is both read and written *)
temp := in_violate_read_write;
in_violate_read_write := temp + 1;


(* ===== VAR_OUTPUT tests ===== *)

(* Compliant: output parameter is written *)
out_ok_written := temp;

(* Violation: out_violate_unused is never written *)

(* Violation: output parameter is read but not written *)
temp := out_violate_read_only;


(* ===== VAR_IN_OUT tests ===== *)

(* Compliant: read only *)
temp := temp + io_ok_read_only;

(* Compliant: written only *)
io_ok_write_only := 10;

(* Compliant: both read and written *)
io_ok_read_write := io_ok_read_write + 1;

(* Violation: io_violate_unused is never used *)

(* ===== special tests for LHS handling ===== *)

(* Important case:
   idx_read_only is used only to locate the write target arr[idx_read_only].
   It should be counted as READ, not WRITE. 
   However, *)
arr[idx_read_only] := 5;

(* Violation:
   idx_violate_write_only is written, but never read. *)
idx_violate_write_only := 3;

END_FUNCTION_BLOCK

(*
 * The reported result should be:
 * 1. in_violate_unused => should be read
 * 2. in_violate_write_only => should not be written, should be read (2 * warnings reported)
 * 3. in_violate_read_write => should not be written
 * 4. out_violate_unused => should be written
 * 5. out_violate_read_only => should be written
 * 6. io_violate_unused => should be written/read
 * 7. idx_violate_write_only => should not be written, should be read (2 * warnings reported)
*)