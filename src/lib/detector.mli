(** Uniform interface for detectors.

    Every check in this library is described by a value of type {!t} that
    bundles the check function together with the runtime metadata needed to
    identify it (id, name, one-line summary) and a list of external
    references. *)

open IECCheckerCore

(** Everything a detector might need to inspect a translation unit. Each
    detector closes over only the fields it actually uses; the registry in
    {!IECCheckerLib.CheckerLib} builds a single [inputs] value once and
    threads it through every detector. *)
type inputs = {
  elements : Syntax.iec_library_element list;
  envs : Env.t list;
  cfgs : Cfg.t list;
}

(** A static check together with its runtime metadata. *)
type t = {
  id : string;
    (** Stable, machine-readable identifier (e.g. ["PLCOPEN-CP1"]). *)
  name : string;
    (** Short human-readable title (e.g. ["Access to a member shall be by
        name"]). *)
  summary : string;
    (** One-line summary suitable for tabular output such as
        [iec_checker --list-checks]. *)
  doc_url : string;
    (** Canonical URL of the detector's long-form documentation page on the
        published doc site. *)
  check : inputs -> Warn.t list;
    (** The analysis function. Receives the full {!inputs} record so that
        every detector has the same shape; each detector reads only the
        fields it needs. *)
}
