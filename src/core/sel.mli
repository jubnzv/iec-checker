(** Module to reconstruct IEC61131-3 source code from the SEL XML files.
    See: https://github.com/jubnzv/iec-checker/issues/6 for the description of
    this format. *)

val reconstruct_from_channel_opt : in_channel -> string option
(** [reconstruct_from_channel channel] Tries to reconstruct the source code
    from the input [channel]. If the content of the given channel contains the
    valid IEC61131-3 source code, this function returns it. Otherwise it returns
    None. *)

