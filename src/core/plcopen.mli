(** Module to reconstruct IEC61131-3 source code from the PLCOpen XML schema. *)
module S = Syntax

val reconstruct_from_channel : in_channel -> string
(** [reconstruct_from_channel channel] Reconstruct source code from the the
    input [channel]. Return complete source code listing from parsed schema. *)
