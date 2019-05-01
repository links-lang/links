
  open Utility

  (* Maps variable names to their type and optionally to their original module, if the variable is a field of an opened module *)
  type qual_var_environment = (QualifiedName.t option * Types.datatype) Env.String.t [@@deriving show]

  (* Maps module names to their type and optionally to their original module, if the module is a sub-module of an opened module*)
  type qual_module_environment = (QualifiedName.t option * Types.module_t) Env.String.t [@@deriving show]
  type tycon_environment  =  Types.tycon_spec Env.String.t [@@deriving show]
  type t = {  var_env   : qual_var_environment
           ; module_env : qual_module_environment
           ; tycon_env  : tycon_environment
           ; effect_row : Types.row } [@@deriving show]

  let empty_typing_environment = {
    var_env = Env.String.empty;
    module_env = Env.String.empty;
    tycon_env =  Env.String.empty;
    effect_row = Types.make_empty_closed_row () }


  let import_module module_name env =
    let _, imported_module = Env.String.lookup env.module_env module_name in
    let orig_path = Some (QualifiedName.of_name module_name) in
    let extend1 sm env = StringMap.fold (fun k v env -> Env.String.bind env (k,(orig_path,v))) sm env in
    let extend2 sm env = StringMap.fold (fun k v env -> Env.String.bind env (k, v)) sm env in
    { env with
        var_env = extend1 imported_module.Types.fields env.var_env;
        module_env = extend1 imported_module.Types.modules env.module_env;
        tycon_env = extend2 imported_module.Types.tycons env.tycon_env}

  let normalise_typing_environment env =
    let rec normalise_module_t mt =
    {
      (* TODO: do we really want to normalise tycons envs? *)
      Types.tycons = StringMap.map Types.normalise_tycon_spec mt.Types.tycons;
      Types.fields = StringMap.map Types.normalise_datatype mt.Types.fields;
      Types.modules = StringMap.map normalise_module_t mt.Types.modules;
    } in
  { env with
      var_env = Env.String.map (fun (open_path,t) -> open_path, Types.normalise_datatype t) env.var_env;
      module_env = Env.String.map (fun (open_path, mt) -> open_path, normalise_module_t mt) env.module_env;
      (* what about tycon_env? *)
      effect_row = Types.normalise_row env.effect_row }

  (* Functions on environments *)
  let extend_typing_environment ?effects
      {var_env = l ; module_env = ml ; tycon_env = al ; effect_row = _  }
      {var_env = r ; module_env = mr ; tycon_env = ar ; effect_row = er } : t =
    {var_env = Env.String.extend l r ;
     module_env = Env.String.extend ml mr ;
     tycon_env = Env.String.extend al ar ;
     effect_row = match effects with
                    | None -> er
                    | Some se -> se
    }

  let string_of_environment = show_qual_var_environment

  let string_of_typing_environment { var_env = env; _ }
    = string_of_environment env



exception TypeNotFound of QualifiedName.t
exception VariableNotFound of QualifiedName.t
exception ModuleNotFound of QualifiedName.t




let lookup_variable (type_env : t) qname : Types.datatype =
 let rec lookup prev_path_rev cur_module_env cur_var_env = function
    | `Ident x ->
       begin match StringMap.find_opt x cur_var_env with
         | Some t -> t
         | None -> raise (VariableNotFound qname)
       end
    | `Dot (moodule, remainder) ->
       begin match StringMap.find_opt moodule cur_module_env with
         | Some module_t ->
            lookup
              (moodule :: prev_path_rev)
              module_t.Types.modules
              module_t.Types.fields
              remainder
         | None ->
            let path = List.rev (moodule :: prev_path_rev) in
            raise (ModuleNotFound (QualifiedName.of_path path))
       end
  in
  match qname with
  | `Ident x ->
     begin match Env.String.find type_env.var_env x with
       | Some (_, t) -> t
       | None -> raise (TypeNotFound qname)
     end
  | `Dot (moodule, remainder) ->
     begin match Env.String.find type_env.module_env moodule with
       | Some ( _, module_t) ->
          lookup
            [moodule]
            module_t.Types.modules
            module_t.Types.fields
            remainder
       | None -> raise (ModuleNotFound (QualifiedName.of_name moodule))
     end

let find_variable (env : t) qname : Types.datatype option =
  try Some (lookup_variable env qname)
  with | VariableNotFound _ -> None
       | ModuleNotFound _ -> None


let lookup_type (type_env : t) qname : Types.tycon_spec =
 let rec lookup prev_path_rev cur_module_env cur_tycon_env = function
    | `Ident x ->
       begin match StringMap.find_opt x cur_tycon_env with
         | Some tspec -> tspec
         | None -> raise (TypeNotFound qname)
       end
    | `Dot (moodule, remainder) ->
       begin match StringMap.find_opt moodule cur_module_env with
         | Some module_t ->
            lookup
              (moodule :: prev_path_rev)
              module_t.Types.modules
              module_t.Types.tycons remainder
         | None ->
            let path = List.rev (moodule :: prev_path_rev) in
            raise (ModuleNotFound (QualifiedName.of_path path))
       end
  in
  match qname with
  | `Ident x ->
     begin match Env.String.find type_env.tycon_env x with
       | Some tspec -> tspec
       | None -> raise (TypeNotFound qname)
     end
  | `Dot (moodule, remainder) ->
     begin match Env.String.find type_env.module_env moodule with
       | Some (_, module_t) ->
          lookup
            [moodule]
            module_t.Types.modules
            module_t.Types.tycons
            remainder
       | None -> raise (ModuleNotFound (QualifiedName.of_name moodule))
     end


let find_type (env : t) qname : Types.tycon_spec option =
  try Some (lookup_type env qname)
  with | TypeNotFound _ -> None
       | ModuleNotFound _ -> None


  (* Must not be used for binding vars that come from opening/importing a module *)
  let bind_var (env : t) (name, t) =
    {env with var_env = Env.String.bind env.var_env (name, (None, t))}
  let bind_tycons (env : t) (name, tspec) =
    {env with tycon_env = Env.String.bind env.tycon_env (name,  tspec)}
  let bind_module (env : t) (name, module_t) =
    {env with module_env = Env.String.bind env.module_env (name, (None, module_t))}


(* Legacy, remove later *)
let var_env_bind_var (var_env : qual_var_environment) (name, t) =
   Env.String.bind var_env (name, (None, t))
