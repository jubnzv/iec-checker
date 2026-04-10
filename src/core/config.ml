(** Configuration values including platform and implementation dependent options.

    When no [iec_checker.json] file is found, {!default} is used and the
    analyzer behaves identically to a build without configuration support. *)

open Core

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

let default = {
  disabled_detectors = [];
  enabled_detectors  = [];
  mccabe_complexity  = 15;
  statements_count   = 25;
  max_string_length  = 4096;
  output_format      = "plain";
  use_color          = true;
  input_format       = "st";
  merge              = false;
  exclude_paths      = [];
  dump               = false;
  verbose            = false;
}

(* Mutable global — set once in the binary entry point before analysis. *)
let current = ref default

let set c = current := c
let get () = !current

(* Backward-compatible accessors *)
let max_string_len () = (!current).max_string_length
let mccabe_complexity_threshold () = (!current).mccabe_complexity
let statements_num_threshold () = (!current).statements_count

(* ---------- JSON helpers ------------------------------------------------- *)

let string_list_of_yojson = function
  | `List items ->
    Ok (List.filter_map items ~f:(function `String s -> Some s | _ -> None))
  | `Null -> Ok []
  | _ -> Error "expected a JSON array of strings"

let member key json =
  match json with
  | `Assoc fields -> List.Assoc.find fields ~equal:String.equal key
  | _ -> None

let string_field json key ~default:d =
  match member key json with
  | Some (`String s) -> s
  | _ -> d

let int_field json key ~default:d =
  match member key json with
  | Some (`Int i) -> i
  | _ -> d

let bool_field json key ~default:d =
  match member key json with
  | Some (`Bool b) -> b
  | _ -> d

let string_list_field json key ~default:d =
  match member key json with
  | Some v -> (match string_list_of_yojson v with Ok l -> l | Error _ -> d)
  | None -> d

(* ---------- Deserialization ---------------------------------------------- *)

let of_yojson (json : Yojson.Safe.t) : (t, string) result =
  try
    let detectors  = Option.value (member "detectors"  json) ~default:`Null in
    let thresholds = Option.value (member "thresholds" json) ~default:`Null in
    let output     = Option.value (member "output"     json) ~default:`Null in
    let input      = Option.value (member "input"      json) ~default:`Null in
    let analysis   = Option.value (member "analysis"   json) ~default:`Null in
    Ok {
      disabled_detectors = string_list_field detectors "disabled"  ~default:default.disabled_detectors;
      enabled_detectors  = string_list_field detectors "enabled"   ~default:default.enabled_detectors;
      mccabe_complexity  = int_field thresholds "mccabe_complexity" ~default:default.mccabe_complexity;
      statements_count   = int_field thresholds "statements_count"  ~default:default.statements_count;
      max_string_length  = int_field thresholds "max_string_length" ~default:default.max_string_length;
      output_format      = string_field output "format"    ~default:default.output_format;
      use_color          = bool_field   output "color"     ~default:default.use_color;
      input_format       = string_field input  "format"    ~default:default.input_format;
      merge              = bool_field   input  "merge"     ~default:default.merge;
      exclude_paths      = string_list_field input "exclude_paths" ~default:default.exclude_paths;
      dump               = bool_field analysis "dump"    ~default:default.dump;
      verbose            = bool_field analysis "verbose" ~default:default.verbose;
    }
  with exn ->
    Error (Printf.sprintf "Failed to parse configuration: %s" (Exn.to_string exn))

(* ---------- Serialization ------------------------------------------------ *)

let to_yojson (c : t) : Yojson.Safe.t =
  `Assoc [
    "detectors", `Assoc [
      "disabled", `List (List.map c.disabled_detectors ~f:(fun s -> `String s));
      "enabled",  `List (List.map c.enabled_detectors  ~f:(fun s -> `String s));
    ];
    "thresholds", `Assoc [
      "mccabe_complexity", `Int c.mccabe_complexity;
      "statements_count",  `Int c.statements_count;
      "max_string_length", `Int c.max_string_length;
    ];
    "output", `Assoc [
      "format", `String c.output_format;
      "color",  `Bool   c.use_color;
    ];
    "input", `Assoc [
      "format",        `String c.input_format;
      "merge",         `Bool   c.merge;
      "exclude_paths", `List (List.map c.exclude_paths ~f:(fun s -> `String s));
    ];
    "analysis", `Assoc [
      "dump",    `Bool c.dump;
      "verbose", `Bool c.verbose;
    ];
  ]

(* ---------- File I/O ----------------------------------------------------- *)

let load_file path =
  try
    let json = Yojson.Safe.from_file path in
    of_yojson json
  with
  | Yojson.Json_error msg ->
    Error (Printf.sprintf "Failed to parse %s: %s" path msg)
  | Sys_error msg ->
    Error (Printf.sprintf "Cannot read %s: %s" path msg)

let config_filename = "iec_checker.json"

let find_config_file start_dir =
  let rec walk dir =
    let candidate = Filename.concat dir config_filename in
    if Stdlib.Sys.file_exists candidate then Some candidate
    else
      let parent = Filename.dirname dir in
      if String.equal parent dir then None  (* reached root *)
      else walk parent
  in
  walk start_dir
