(** Configuration values including platform and implementation dependent options.

    When no [iec_checker.json] file is found, {!default} is used and the
    analyzer behaves identically to a build without configuration support. *)

(** The full configuration record. *)
type t = {
  disabled_detectors : string list;
  enabled_detectors  : string list;
  mccabe_complexity  : int;
  statements_count   : int;
  max_string_length  : int;
  output_format      : string;
  use_color          : bool;
  input_format       : string;
  merge              : bool;
  exclude_paths      : string list;
  dump               : bool;
  verbose            : bool;
}

val default : t
(** Default configuration — matches the original hardcoded values. *)

val set : t -> unit
(** Set the global configuration.  Must be called exactly once, before any
    analysis pass runs. *)

val get : unit -> t
(** Return the current global configuration. *)

(** {2 Backward-compatible accessors} *)

val max_string_len : unit -> int
(** Maximum size of STRING and WSTRING data types. *)

val mccabe_complexity_threshold : unit -> int
(** Threshold of McCabe complexity to generate warnings. *)

val statements_num_threshold : unit -> int
(** Threshold of maximum number of statements in POU to generate warnings. *)

(** {2 Config file I/O} *)

val load_file : string -> (t, string) result
(** [load_file path] reads [path] as JSON and merges it onto {!default}. *)

val find_config_file : string -> string option
(** [find_config_file dir] walks from [dir] up to the filesystem root looking
    for [iec_checker.json].  Returns [Some path] or [None]. *)

val to_yojson : t -> Yojson.Safe.t
(** Serialize a configuration to JSON (for [--dump-config] / [--generate-config]). *)
