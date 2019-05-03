
  open Utility

  (* Maps variable names to their type and optionally to their original module, if the variable is a field of an opened module *)
  type qual_var_environment = (QualifiedName.t option * Types.datatype) Env.String.t [@@deriving show]

  (* Maps module names to their type and optionally to their original module, if the module is a sub-module of an opened module*)
  type qual_module_environment = (QualifiedName.t option * Types.module_t) Env.String.t [@@deriving show]
  type qual_tycon_environment  = (QualifiedName.t option * Types.tycon_spec) Env.String.t [@@deriving show]
  type t = {  var_env   : qual_var_environment
           ; module_env : qual_module_environment
           ; tycon_env  : qual_tycon_environment
           ; effect_row : Types.row } [@@deriving show]

  let empty_typing_environment = {
    var_env = Env.String.empty;
    module_env = Env.String.empty;
    tycon_env =  Env.String.empty;
    effect_row = Types.make_empty_closed_row () }


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



exception TyConsNotFound   of QualifiedName.t
exception VariableNotFound of QualifiedName.t
exception ModuleNotFound   of QualifiedName.t



type ('a, 'b) resolve_result =
  | RInEnv of 'a
  | RInModule of QualifiedName.t option * 'b
  | RNotFound

(**
  This is the generic resolver logic for qualified names.
  It should not be exported from this module.
  Used by the variable/type/module lookup functions

  env_extractor:
  How to handle a qname that is just an Ident?
  module_extractor:
  How to handle an Ident once we are in a module?

  raises ModuleNotFound if it can't traverse module
  path furter
  returns RNotFound if we get to the right module,
  but the Ident is unknown **)
let resolve_qualified_name
    (env : t)
    (qname : QualifiedName.t)
    (env_extractor : string -> t -> 'a option)
    (module_extractor : string -> Types.module_t -> 'b option)
      : ('a, 'b) resolve_result =
  let rec traverse_submodules
            orig_module
            count
            cur_qname
            (cur_module_t : Types.module_t) =
    match cur_qname with
      | QualifiedName.Ident name ->
         begin match module_extractor name cur_module_t with
           | None -> RNotFound
           | Some res -> RInModule (orig_module, res)
         end
      |  QualifiedName.Dot (mod_name, remainder) ->
         let next_module =
           StringMap.find_opt
             mod_name
             cur_module_t.Types.modules in
          match next_module with
            | None ->
               let module_path_until_failure =
                 QualifiedName.prefix (count+1) qname in
               raise (ModuleNotFound module_path_until_failure)
            | Some module_t ->
               traverse_submodules
                 orig_module
                 (count+1)
                 remainder
                 module_t
  in
  match qname with
    |  QualifiedName.Ident name ->
       begin match env_extractor name env with
         | None -> RNotFound
         | Some res -> RInEnv res
       end
    |  QualifiedName.Dot (first_module, remainder) ->
      begin match Env.String.find env.module_env first_module with
        | None ->
           raise (ModuleNotFound (QualifiedName.of_name first_module))
        | Some (orig_module, module_t) ->
           traverse_submodules orig_module 1 remainder module_t
      end


let lookup_variable (type_env : t) qname : Types.datatype =
  let var_from_env var (env : t) =
    Env.String.find env.var_env var in
  let var_from_module var (module_t : Types.module_t) =
    StringMap.lookup var module_t.Types.fields in
  let resolve_res =
    resolve_qualified_name
      type_env
      qname
      var_from_env
      var_from_module in
  match resolve_res with
    | RInEnv (_, typ) -> typ
    | RInModule (_, typ) -> typ
    | RNotFound -> raise (VariableNotFound qname)

let find_variable (env : t) qname : Types.datatype option =
  try Some (lookup_variable env qname)
  with | VariableNotFound _ -> None
       | ModuleNotFound _ -> None


let lookup_tycons_with_orig_path (type_env : t) qname
    : (QualifiedName.t option * Types.tycon_spec) =
 let type_from_env tycon (env : t) =
    Env.String.find env.tycon_env tycon in
  let type_from_module tycon (module_t : Types.module_t) =
    StringMap.lookup tycon module_t.Types.tycons in
  let resolve_res =
    resolve_qualified_name
      type_env
      qname
      type_from_env
      type_from_module in
  match resolve_res with
    | RInEnv (orig_path, tyspec) -> (orig_path, tyspec)
    | RInModule (orig_path, tyspec) -> (orig_path, tyspec)
    | RNotFound -> raise (TyConsNotFound qname)

let lookup_tycons (type_env : t) qname : Types.tycon_spec =
  snd (lookup_tycons_with_orig_path  type_env qname)

let find_tycons (env : t) qname : Types.tycon_spec option =
  try Some (snd (lookup_tycons_with_orig_path  env qname))
  with | TyConsNotFound  _ -> None
       | ModuleNotFound _ -> None


let lookup_module_with_orig_path
      (type_env : t) qname
    : (QualifiedName.t option*Types.module_t) =
  let module_from_env var (env : t) =
    Env.String.find env.module_env var in
  let module_from_module var (module_t : Types.module_t) =
    StringMap.lookup var module_t.Types.modules in
  let resolve_res =
    resolve_qualified_name
      type_env
      qname
      module_from_env
      module_from_module in
  match resolve_res with
    | RInEnv (orig_path, module_t) -> (orig_path, module_t)
    | RInModule (orig_path, module_t) -> (orig_path, module_t)
    | RNotFound -> raise (ModuleNotFound qname)

let lookup_module
   (type_env : t) qname : Types.module_t =
  snd (lookup_module_with_orig_path type_env qname)

let find_module (env : t) qname : Types.module_t option =
  try Some (snd (lookup_module_with_orig_path env qname))
  with | ModuleNotFound _ -> None



let open_module module_qname env_lookup_from env_add_to =
  let orig_path_opt, module_t = lookup_module_with_orig_path env_lookup_from module_qname in
  let orig_path = match orig_path_opt with
      | None -> Some module_qname
      | Some prefix_path ->
         Some (QualifiedName.append prefix_path module_qname) in
  let extend sm env = StringMap.fold
    (fun k v env -> Env.String.bind env (k,(orig_path,v))) sm env in
  { effect_row = env_add_to.effect_row;
    var_env = extend module_t.Types.fields env_add_to.var_env;
    module_env = extend module_t.Types.modules env_add_to.module_env;
    tycon_env = extend module_t.Types.tycons env_add_to.tycon_env}




  (* Must not be used for binding vars that come from opening/importing a module *)
  let bind_var (env : t) (name, t) =
    {env with var_env = Env.String.bind env.var_env (name, (None, t))}
  let bind_tycons (env : t) (name, tspec) =
    {env with tycon_env = Env.String.bind env.tycon_env (name, (None, tspec))}
  let bind_module (env : t) (name, module_t) =
    {env with module_env = Env.String.bind env.module_env (name, (None, module_t))}


(* Legacy, remove later *)
let var_env_bind_var (var_env : qual_var_environment) (name, t) =
   Env.String.bind var_env (name, (None, t))
