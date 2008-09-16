open Utility
open Types

(* debug flags *)
let show_generalisation = Settings.add_bool("show_generalisation", false, `User)
let show_recursion = Instantiate.show_recursion

type gen_kind = [`Rigid|`All]

(*
 get the free type variables of a datatype as type args
 
 (the first argument specifies type variables that should remain free)
*)
let rec get_type_args : gen_kind -> TypeVarSet.t -> datatype -> type_arg list = 
  fun kind bound_vars t ->
    let gt = get_type_args kind bound_vars in
      match t with
        | `Not_typed -> failwith "Internal error: Not_typed encountered in get_type_args"
        | `Primitive _ -> []
        | `MetaTypeVar point ->
            (match Unionfind.find point with
               | `Flexible var
               | `Rigid var when TypeVarSet.mem var bound_vars -> []
               | `Flexible _ when kind=`All -> [`Type (`MetaTypeVar point)]
               | `Flexible _ -> []
               | `Rigid _ -> [`Type (`MetaTypeVar point)]
               | `Recursive (var, body) ->
                   Debug.if_set (show_recursion) (fun () -> "rec (get_type_args): " ^(string_of_int var));
                   if TypeVarSet.mem var bound_vars then
                     []
                   else
                     get_type_args kind (TypeVarSet.add var bound_vars) body
               | `Body t -> gt t)
        | `Function (f, m, t) ->
            let from_gens = gt f
            and mailbox_gens = gt m
            and to_gens = gt t in
              from_gens @ mailbox_gens @ to_gens
        | `Record row
        | `Variant row -> get_row_type_args kind bound_vars row 
        | `Table (r, w) -> gt r @ gt w
        | `Alias ((_, ts), t) ->
            concat_map gt ts @ gt t
        | `ForAll (qs, t) -> 
            get_type_args kind (List.fold_right (Types.type_var_number ->- TypeVarSet.add) qs bound_vars) t
        | `Application (_, args) ->
            Utility.concat_map gt args

and get_row_var_type_args : gen_kind -> TypeVarSet.t -> row_var -> type_arg list =
  fun kind bound_vars row_var ->
    match Unionfind.find row_var with
      | `Closed -> []
      | `Flexible var
      | `Rigid var when TypeVarSet.mem var bound_vars -> []
      | `Flexible _ when kind=`All -> [`Row (StringMap.empty, row_var)]
      | `Flexible _ -> []
      | `Rigid _ -> [`Row (StringMap.empty, row_var)]
      | `Recursive (var, rec_row) ->
          Debug.if_set (show_recursion) (fun () -> "rec (get_row_var_type_args): " ^(string_of_int var));
          (if TypeVarSet.mem var bound_vars then
             []
           else
             get_row_type_args kind (TypeVarSet.add var bound_vars) rec_row)
      | `Body row -> get_row_type_args kind bound_vars row

and get_field_spec_type_args : gen_kind -> TypeVarSet.t -> field_spec -> type_arg list =
  fun kind bound_vars ->
    function
      | `Present t -> get_type_args kind bound_vars t
      | `Absent -> []

and get_row_type_args : gen_kind -> TypeVarSet.t -> row -> type_arg list =
  fun kind bound_vars (field_env, row_var) ->
    let field_vars = StringMap.fold
      (fun _ field_spec vars ->
         get_field_spec_type_args kind bound_vars field_spec @ vars
      ) field_env [] in
    let row_vars = get_row_var_type_args kind bound_vars (row_var:row_var)
    in
      field_vars @ row_vars

let get_type_args kind bound_vars t =
  unduplicate (fun l r ->
                 match l, r with
                   | `Type (`MetaTypeVar l), `Type (`MetaTypeVar r) -> Unionfind.equivalent l r
                   | `Row (_, l), `Row (_, r) -> Unionfind.equivalent l r
                   | _ -> false) (get_type_args kind bound_vars t)


let quantifiers_of_type_args =
  List.map
    (function
       | `Type (`MetaTypeVar point) ->
           begin
             match Unionfind.find point with
               | `Flexible var -> `TypeVar var
               | `Rigid var -> `RigidTypeVar var
               | _ -> assert false
           end
       | `Type _ -> assert false
       | `Row (fields, row_var) ->
           assert (StringMap.is_empty fields);
           begin
             match Unionfind.find row_var with
               | `Flexible var -> `RowVar var
               | `Rigid var -> `RigidRowVar var
               | _ -> assert false
           end)

let get_quantifiers bound_vars = quantifiers_of_type_args -<- (get_type_args `All bound_vars)

let env_type_vars (env : Types.environment) =
  TypeVarSet.union_all (List.map free_type_vars (Env.String.range env))

(** generalise: 
    Universally quantify any free type variables in the expression.
*)
let generalise : gen_kind -> environment -> datatype -> ((quantifier list * type_arg list) * datatype) =
  fun kind env t ->
    let vars_in_env = env_type_vars env in
    let type_args = get_type_args kind vars_in_env t in
    let quantifiers = quantifiers_of_type_args type_args in
    let quantified = Types.for_all (quantifiers, t) in 
      Debug.if_set show_generalisation (fun () -> "Generalised: " ^ string_of_datatype quantified);
      ((quantifiers, type_args), quantified)

(** only generalise rigid type variables *)
let generalise_rigid = generalise `Rigid

(** generalise both rigid and flexible type variables *)
let generalise = generalise `All

let get_quantifiers : environment -> datatype -> quantifier list =
  fun env t ->
    get_quantifiers (env_type_vars env) t
