open Utility

module NEnv = Env.String
module TEnv = Env.Int

type datatype = Types.datatype

type nenv = Var.var NEnv.t
type tenv = datatype TEnv.t

type env =  {
  sugar_to_ir_names : nenv ;
  ir_type_env : tenv ;
  effects : Types.row
}

(* In the IR, variabled and modules share the same environments.
   To ditinguish them, we prefix module names with the following string,
    which is not a legal variable name in the source language *)
let module_variable_prefix = "#MODULE_"
let module_variable_prefix_regex = Str.regexp (module_variable_prefix ^ ".*")


let variable_name_of_module_name module_name = module_variable_prefix ^ module_name

let lookup_variable_name_and_type var_name env =
    if Str.string_match module_variable_prefix_regex var_name 0 then
        failwith "Trying to lookup variable name that looks like a module name";
    let var = NEnv.lookup env.sugar_to_ir_names var_name in
    var, TEnv.lookup env.ir_type_env var

let lookup_variable_name var_name env =
    if Str.string_match module_variable_prefix_regex var_name 0 then
        failwith "Trying to lookup variable name that looks like a module name";
    NEnv.lookup env.sugar_to_ir_names var_name

let lookup_effects env = env.effects

let lookup_module_name_and_type module_name env =
    let var = NEnv.lookup env.sugar_to_ir_names (variable_name_of_module_name module_name) in
    var, TEnv.lookup env.ir_type_env var

let bind_variable_name_and_type var_name var t env =
    if Str.string_match module_variable_prefix_regex var_name 0 then
        failwith "Trying to bind variable name that looks like a module name";
    {env with
        sugar_to_ir_names = NEnv.bind env.sugar_to_ir_names (var_name,var ) ;
        ir_type_env = TEnv.bind env.ir_type_env (var, t) }

let bind_module_name_and_type module_name var t env =
    {env with
        sugar_to_ir_names = NEnv.bind env.sugar_to_ir_names (variable_name_of_module_name module_name, var) ;
        ir_type_env = TEnv.bind env.ir_type_env (var, t) }

let bind_type var t env =
    {env with ir_type_env = TEnv.bind env.ir_type_env (var, t) }

let extend xs vs env =
    List.fold_left2
        (fun env x (v, t) ->
            bind_variable_name_and_type x v t env)
        env
        xs
        vs


let (++) env1 env2 = {
    sugar_to_ir_names = NEnv.extend env1.sugar_to_ir_names env2.sugar_to_ir_names ;
    ir_type_env = TEnv.extend env1.ir_type_env env2.ir_type_env ;
    effects = env2.effects
}


let make_empty effects = {
    sugar_to_ir_names = NEnv.empty ;
    ir_type_env = TEnv.empty ;
    effects = effects
}

  let rec ir_type_of_module_type module_type =
    let field_env = StringMap.fold (fun field t fenv ->
        StringMap.add field  t fenv
      ) module_type.Types.fields StringMap.empty in

    let module_env = StringMap.fold (fun module_name mt fenv ->
        StringMap.add module_name (ir_type_of_module_type mt) fenv
      ) module_type.Types.modules field_env in
    let record_row = Types.make_closed_row module_env in
    `Record record_row


(** Create a copy of a type environment mapping vars (= ints) to types
    instead of strings to types
*)
let varify_env (nenv, (frontend_env : FrontendTypeEnv.t)) : Types.datatype Env.Int.t =
  let with_vars =
    Env.String.fold
      (fun name (_, t) tenv ->
        Env.Int.bind tenv (Env.String.lookup nenv name, t))
      frontend_env.FrontendTypeEnv.var_env
      Env.Int.empty in

  let with_modules =
    Env.String.fold
      (fun module_name (_, module_t) tenv ->
        if module_name = Lib.BuiltinModules.lib then
          tenv
        else
          let t = ir_type_of_module_type module_t in
          Env.Int.bind tenv (Env.String.lookup nenv (variable_name_of_module_name module_name), t))
      frontend_env.FrontendTypeEnv.module_env
      with_vars in
   with_modules

