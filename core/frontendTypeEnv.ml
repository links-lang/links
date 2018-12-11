
  open Utility

  type var_environment   = Types.datatype Env.String.t
  and module_environment = Types.module_t Env.String.t
  and tycon_environment  = Types.tycon_spec Env.String.t
  and t = {  var_env    : var_environment
           ; module_env : module_environment
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
      var_env = Env.String.map Types.normalise_datatype env.var_env;
      module_env = Env.String.map normalise_module_t env.module_env;
      (* FIXME: normalise types in module types, too? *)
      (* what about tycon_env? *)
      effect_row = Types.normalise_row env.effect_row }

  (* Functions on environments *)
  let extend_typing_environment
      {var_env = l ; module_env = ml ; tycon_env = al ; effect_row = _  }
      {var_env = r ; module_env = mr ; tycon_env = ar ; effect_row = er } : t =
    {var_env = Env.String.extend l r ; module_env = Env.String.extend ml mr  ; tycon_env = Env.String.extend al ar ; effect_row = er }

  let string_of_environment = show_var_environment

  let string_of_typing_environment { var_env = env; _ }
    = string_of_environment env
