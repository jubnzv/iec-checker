open Core
module S = Syntax

type dump_scheme = {
  version: string; (** Scheme version *)
  functions: S.function_decl list;
  function_blocks: S.fb_decl list;
  programs: S.program_decl list;
  configurations: S.configuration_decl list;
  types: S.derived_ty_decl list;
  environments: Env.t list;
  cfgs: Cfg.t list;
} [@@deriving to_yojson]

let create_dump ~dst_file elements environments cfgs =
  let version = "0.1" in
  let functions =
    List.fold_left elements
      ~f:(fun acc e -> match e with S.IECFunction (_, f) -> acc @ [f] | _ -> acc)
      ~init:[]
  in
  let function_blocks =
    List.fold_left elements
      ~f:(fun acc e -> match e with S.IECFunctionBlock (_, fb) -> acc @ [fb] | _ -> acc)
      ~init:[]
  in
  let programs =
    List.fold_left elements
      ~f:(fun acc e -> match e with S.IECProgram (_, p) -> acc @ [p] | _ -> acc)
      ~init:[]
  in
  let configurations =
    List.fold_left elements
      ~f:(fun acc e -> match e with S.IECConfiguration (_, c) -> acc @ [c] | _ -> acc)
      ~init:[]
  in
  let types =
    List.fold_left elements
      ~f:(fun acc e -> match e with S.IECType (_, ty) -> acc @ [ty] | _ -> acc)
      ~init:[]
  in
  let scheme = {
    version;
    functions;
    function_blocks;
    programs;
    configurations;
    types;
    environments;
    cfgs;
  } in
  Yojson.Safe.to_file dst_file (dump_scheme_to_yojson scheme)
