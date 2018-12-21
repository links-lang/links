type t =
  {
    venv: Value.env; (* maps int identifiers to their values *)
    nenv: Ir.var Env.String.t; (* map string identifiers to int identifiers *)
    tyenv: FrontendTypeEnv.t; (* typing info, using string identifiers *)
    globals: Ir.binding list; (* Global bindings that the current program may use. Needed for the webserver *)
    external_files : string list
  }

let empty =
  {
    venv = Value.Env.empty;
    nenv = Env.String.empty;
    tyenv =  FrontendTypeEnv.empty_typing_environment;
    globals = [];
    external_files = [];
  }