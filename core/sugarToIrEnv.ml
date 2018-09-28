module NEnv = Env.String
module TEnv = Env.Int

type datatype = Types.datatype

type nenv = Var.var NEnv.t
type tenv = datatype TEnv.t

type env =  {
  sugar_to_ir_names : nenv ;
  ir_type_env : tenv ;
  module_to_record : (Var.var * datatype) NEnv.t ;
  effects : Types.row
}

let lookup_name_and_type name env =
    let var = NEnv.lookup env.sugar_to_ir_names name in
    var, TEnv.lookup env.ir_type_env var

let lookup_name name env = NEnv.lookup env.sugar_to_ir_names name

let lookup_effects env = env.effects

let lookup_record_for_module name env =
    NEnv.lookup env.module_to_record name

let bind_name_and_type name var t env =
    {env with
        sugar_to_ir_names = NEnv.bind env.sugar_to_ir_names (name,var ) ;
        ir_type_env = TEnv.bind env.ir_type_env (var, t) }

let bind_type var t env =
    {env with ir_type_env = TEnv.bind env.ir_type_env (var, t) }
let extend xs vs env =
    List.fold_left2
        (fun env x (v, t) ->
            bind_name_and_type x v t env)
        env
        xs
        vs

let extend_module name var mtype env =
    {env with module_to_record = NEnv.bind env.module_to_record (name, (var, mtype)) }

let (++) env1 env2 = {
    sugar_to_ir_names = NEnv.extend env1.sugar_to_ir_names env2.sugar_to_ir_names ;
    ir_type_env = TEnv.extend env1.ir_type_env env2.ir_type_env ;
    module_to_record = NEnv.extend  env1.module_to_record env2.module_to_record ;
    effects = env2.effects
}


let make_empty effects = {
    sugar_to_ir_names = NEnv.empty ;
    ir_type_env = TEnv.empty ;
    module_to_record = NEnv.empty ;
    effects = effects
}