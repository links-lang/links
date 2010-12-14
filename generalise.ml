open Utility
open Types

(* debug flags *)
let show_generalisation = Settings.add_bool("show_generalisation", false, `User)
let show_recursion = Instantiate.show_recursion

type gen_kind = [`Rigid|`All]

(** [get_type_args kind bound_vars t] gets the free type variables of
    the datatype [t] as type args. The [kind] parameter specifies
    whether to generalise all variables or just the rigid ones. The
    [bound_vars] parameter specifies type variables that should remain
    free.

    NOTE: the order of the type arguments can be important.
*)
let rec get_type_args : gen_kind -> TypeVarSet.t -> datatype -> type_arg list = 
  fun kind bound_vars t ->
    let gt = get_type_args kind bound_vars in
      match t with
        | `Not_typed -> failwith "Internal error: Not_typed encountered in get_type_args"
        | `Primitive _ -> []
        | `MetaTypeVar point ->
            begin
              match Unionfind.find point with
                | `Flexible (var, _)
                | `Rigid (var, _) when TypeVarSet.mem var bound_vars -> []
                | `Flexible _ when kind=`All -> [`Type (`MetaTypeVar point)]
                | `Flexible _ -> []
                | `Rigid _ -> [`Type (`MetaTypeVar point)]
                | `Recursive (var, body) ->
                    Debug.if_set (show_recursion) (fun () -> "rec (get_type_args): " ^(string_of_int var));
                    if TypeVarSet.mem var bound_vars then
                      []
                    else
                      get_type_args kind (TypeVarSet.add var bound_vars) body
                | `Body t -> gt t
            end
        | `Function (f, m, t) ->
            let from_gens = gt f
            and effect_gens = get_row_type_args kind bound_vars m
            and to_gens = gt t in
              from_gens @ effect_gens @ to_gens
        | `Record row
        | `Variant row -> get_row_type_args kind bound_vars row 
        | `Table (r, w, n) -> gt r @ gt w @ gt n
        | `Alias ((_, ts), t) ->
            concat_map (get_type_arg_type_args kind bound_vars) ts @ gt t
        | `ForAll (qsref, t) -> 
            (* 
               filter out flexible quantifiers
               
               WARNING: this behaviour might not always be what we want

                 1) Its imperative nature seems a bit dodgey.
                    Note that this function is called by the
                    get_type_variables function as well as by
                    generalise.

                 2) We might alternatively rigidify the quantifiers
                    during generalisation.

                 3) In any case, we might implement
                    instantiation as part of destructing a type. For
                    instance, this would allow function application
                    in the case where the function has a rigid polymorphic
                    type such as:
                    
                      forall a.(a) {}-> a
               
                 4) Furthermore, we can keep as much polymorphism as possible
                    by only instantiating those type variables that appear
                    free in the function argument.

            *)
            let qs = List.filter Types.is_rigid_quantifier (Types.unbox_quantifiers qsref) in
              qsref := qs;
              get_type_args kind (List.fold_right (Types.type_var_number ->- TypeVarSet.add) qs bound_vars) t
        | `Application (_, args) ->
            Utility.concat_map (get_type_arg_type_args kind bound_vars) args

and get_row_var_type_args : gen_kind -> TypeVarSet.t -> row_var -> type_arg list =
  fun kind bound_vars row_var ->
    match Unionfind.find row_var with
      | `Closed -> []
      | `Flexible (var, _)
      | `Rigid (var, _) when TypeVarSet.mem var bound_vars -> []
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
  fun kind bound_vars (f, t) ->
    get_presence_type_args kind bound_vars f @ get_type_args kind bound_vars t

and get_presence_type_args : gen_kind -> TypeVarSet.t -> presence_flag -> type_arg list =
  fun kind bound_vars ->
    function
      | `Present | `Absent -> []
      | `Var point ->
          begin
            match Unionfind.find point with
              | `Flexible var
              | `Rigid var when TypeVarSet.mem var bound_vars -> []
              | `Flexible _ when kind=`All -> [`Presence (`Var point)]
              | `Flexible _ -> []
              | `Rigid _ -> [`Presence (`Var point)]
              | `Body f -> get_presence_type_args kind bound_vars f
          end

and get_row_type_args : gen_kind -> TypeVarSet.t -> row -> type_arg list =
  fun kind bound_vars (field_env, row_var) ->
    let field_vars =
      StringMap.fold
        (fun _ field_spec vars ->
           vars @ get_field_spec_type_args kind bound_vars field_spec
        ) field_env [] in
    let row_vars = get_row_var_type_args kind bound_vars (row_var:row_var)
    in
      field_vars @ row_vars

and get_type_arg_type_args : gen_kind -> TypeVarSet.t -> type_arg -> type_arg list =
  fun kind bound_vars ->
    function
      | `Type t -> get_type_args kind bound_vars t
      | `Row r -> get_row_type_args kind bound_vars r
      | `Presence f -> get_presence_type_args kind bound_vars f

let remove_duplicates =
  unduplicate (fun l r ->
                 match l, r with
                   | `Type (`MetaTypeVar l), `Type (`MetaTypeVar r) -> Unionfind.equivalent l r
                   | `Row (_, l), `Row (_, r) -> Unionfind.equivalent l r
                   | `Presence (`Var l), `Presence (`Var r) -> Unionfind.equivalent l r
                   | _ -> false)

let get_type_args kind bound_vars t =
  remove_duplicates (get_type_args kind bound_vars t)

let get_row_var_type_args kind bound_vars row_var =
  remove_duplicates (get_row_var_type_args kind bound_vars row_var)

let get_presence_type_args kind bound_vars f =
  remove_duplicates (get_presence_type_args kind bound_vars f)

let type_variables_of_type_args =
  List.map
    (function
       | `Type (`MetaTypeVar point) ->
           begin
             match Unionfind.find point with
               | `Flexible (var, _) -> (var, `Flexible, `Type point)
               | `Rigid (var, _) -> (var, `Rigid, `Type point)
               | _ -> assert false
           end
       | `Type _ -> assert false
       | `Row (fields, row_var) ->
           assert (StringMap.is_empty fields);
           begin
             match Unionfind.find row_var with
               | `Flexible (var, _) -> (var, `Flexible, `Row row_var)
               | `Rigid (var, _) -> (var, `Rigid, `Row row_var)
               | _ -> assert false
           end
       | `Presence (`Var point) ->
           begin
             match Unionfind.find point with
               | `Flexible var -> (var, `Flexible, `Presence point)
               | `Rigid var -> (var, `Rigid, `Presence point)
               | _ -> assert false
           end
       | `Presence _ -> assert false)

let get_type_variables bound_vars = type_variables_of_type_args -<- (get_type_args `All bound_vars)
let get_quantifiers bound_vars = Types.quantifiers_of_type_args -<- (get_type_args `All bound_vars)
(* let get_row_var_quantifiers bound_vars = Types.quantifiers_of_type_args -<- (get_row_var_type_args `All bound_vars) *)
(* let get_presence_quantifiers bound_vars = Types.quantifiers_of_type_args -<- (get_presence_type_args `All bound_vars) *)

(* pull out all the type variables in the quantifiers, as quantifiers,
   e.g. if a quantifier gets instantiated as (a) -> b, then that
   results in two quantifiers: a and b.
*)
let extract_quantifiers quantifiers =
  let quantifier_type_args =
    function
      | `TypeVar (_, point) ->
          get_type_args `All TypeVarSet.empty (`MetaTypeVar point)
      | `RowVar (_, row_var) ->
          get_row_var_type_args `All TypeVarSet.empty row_var
      | `PresenceVar (_, point) ->
          get_presence_type_args `All TypeVarSet.empty (`Var point)
  in
    Types.quantifiers_of_type_args
      (remove_duplicates (concat_map quantifier_type_args quantifiers))

let env_type_vars (env : Types.environment) =
  TypeVarSet.union_all (List.map free_type_vars (Env.String.range env))

let rigidify_quantifier =
  function
    | `TypeVar (_, point) ->
        begin
          match Unionfind.find point with
            | `Flexible (var, subkind) -> Unionfind.change point (`Rigid (var, subkind))
            | `Rigid _ -> ()
            | _ -> assert false
        end
    | `RowVar (_, point) ->
        begin
          match Unionfind.find point with
            | `Flexible (var, subkind) -> Unionfind.change point (`Rigid (var, subkind))
            | `Rigid _ -> ()
            | _ -> assert false
        end
    | `PresenceVar (_, point) ->
        begin
          match Unionfind.find point with
            | `Flexible var -> Unionfind.change point (`Rigid var)
            | `Rigid _ -> ()
            | _ -> assert false
        end

(** generalise: 
    Universally quantify any free type variables in the expression.
*)
let generalise : gen_kind -> environment -> datatype -> ((quantifier list * type_arg list) * datatype) =
  fun kind env t ->
    let vars_in_env = env_type_vars env in
    let type_args = get_type_args kind vars_in_env t in
    let quantifiers = Types.quantifiers_of_type_args type_args in
    let () = List.iter rigidify_quantifier quantifiers in
    let quantified = Types.for_all (quantifiers, t) in
      (* Make sure that any existing quantifiers are accounted for.
         This can be necessary if we have type annotations involving
         explicit quantifiers.
      *)
    let quantifiers, type_args =
      begin
        let qs =
          match quantified with
            | `ForAll (qs, _) ->
                Types.unbox_quantifiers qs
            | _ -> []
        in
          if List.length qs <> List.length quantifiers then
            qs, (List.map Types.type_arg_of_quantifier qs)
          else
            quantifiers, type_args
      end
    in
      Debug.if_set show_generalisation (fun () -> "Generalised: " ^ string_of_datatype quantified);
      ((quantifiers, type_args), quantified)

(** only generalise rigid type variables *)
let generalise_rigid = generalise `Rigid

(** generalise both rigid and flexible type variables *)
let generalise = generalise `All

let get_type_variables : environment -> datatype -> type_variable list =
  fun env t ->
    get_type_variables (env_type_vars env) t

let get_quantifiers : environment -> datatype -> quantifier list =
  fun env t ->
    get_quantifiers (env_type_vars env) t
