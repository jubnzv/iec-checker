open IECCheckerCore

type inputs = {
  elements : Syntax.iec_library_element list;
  envs : Env.t list;
  cfgs : Cfg.t list;
}

type t = {
  id : string;
  name : string;
  summary : string;
  doc_url : string;
  check : inputs -> Warn.t list;
}
