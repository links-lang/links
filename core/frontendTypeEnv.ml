
  open Utility

  (* Maps variable names to their type and optionally to their original module, if the variable is a field of an opened module *)
  type qual_var_environment = (QualifiedName.t option * Types.datatype) Env.String.t [@@deriving show]

  (* Maps module names to their type and optionally to their original module, if the module is a sub-module of an opened module*)
  type qual_module_environment = (QualifiedName.t option * Types.module_t) Env.String.t [@@deriving show]
  type tycon_environment  = Types.tycon_spec Env.String.t [@@deriving show]
  type t = {  var_env   : qual_var_environment
           ; module_env : qual_module_environment
           ; tycon_env  : tycon_environment
           ; effect_row : Types.row } [@@deriving show]

  let empty_typing_environment = {
    var_env = Env.String.empty;
    module_env = Env.String.empty;
    tycon_env =  Env.String.empty;
    effect_row = Types.make_empty_closed_row ()  }

  let normalise_typing_environment env =
    let rec normalise_module_t mt =
    {
      Types.fields = StringMap.map Types.normalise_datatype mt.Types.fields;
      Types.modules = StringMap.map normalise_module_t mt.Types.modules;
    } in
  { env with
      var_env = Env.String.map (fun (open_path,t) -> open_path, Types.normalise_datatype t) env.var_env;
      module_env = Env.String.map (fun (open_path, mt) -> open_path, normalise_module_t mt) env.module_env;
      (* FIXME: normalise types in module types, too? *)
      (* what about tycon_env? *)
      effect_row = Types.normalise_row env.effect_row }

  (* Functions on environments *)
  let extend_typing_environment
      {var_env = l ; module_env = ml ; tycon_env = al ; effect_row = _  }
      {var_env = r ; module_env = mr ; tycon_env = ar ; effect_row = er } : t =
    {var_env = Env.String.extend l r ; module_env = Env.String.extend ml mr  ; tycon_env = Env.String.extend al ar ; effect_row = er }

  let string_of_environment = show_qual_var_environment

  let string_of_typing_environment { var_env = env; _ }
    = string_of_environment env


  let lookup_var_tyenv tyenv name = snd (Env.String.lookup tyenv.var_env name)
  let lookup_var_venv varenv name = snd (Env.String.lookup varenv name)

  (* Must not be used for binding vars that come from opening/importing a module *)
  let bind_var_tyenv tyenv (name, t) = Env.String.bind tyenv.var_env (name, (None, t))
  let bind_var_venv var_env (name, t) = Env.String.bind var_env (name, (None, t))