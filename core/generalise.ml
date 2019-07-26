open Utility
open Types
open CommonTypes

(* debug flags *)
let show_generalisation = Basicsettings.Generalise.show_generalisation
let show_recursion = Instantiate.show_recursion

let internal_error message = Errors.internal_error ~filename:"generalise" ~message

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
        | `Not_typed -> raise (internal_error "Not_typed encountered in get_type_args")
        | `Primitive _ -> []
        | `MetaTypeVar point ->
            begin
              match Unionfind.find point with
                | `Var (var, _, _) when TypeVarSet.mem var bound_vars -> []
                | `Var (_, _, `Flexible) when kind=`All -> [`Type (`MetaTypeVar point)]
                | `Var (_, _, `Flexible) -> []
                | `Var (_, _, `Rigid) -> [`Type (`MetaTypeVar point)]
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
        | `Lolli (f, m, t) ->
            let from_gens = gt f
            and effect_gens = get_row_type_args kind bound_vars m
            and to_gens = gt t in
              from_gens @ effect_gens @ to_gens
        | `Record row
        | `Effect row
        | `Variant row -> get_row_type_args kind bound_vars row
        | `Table (r, w, n) -> gt r @ gt w @ gt n
        | `Lens _ -> []
        | `Alias ((_, _, ts), t) ->
            concat_map (get_type_arg_type_args kind bound_vars) ts @ gt t
        | `ForAll (qs, t) ->
           get_type_args kind (List.fold_right (Types.type_var_number ->- TypeVarSet.add) qs bound_vars) t
        | `Application (_, args) ->
            Utility.concat_map (get_type_arg_type_args kind bound_vars) args
        | `RecursiveApplication appl ->
            Utility.concat_map (get_type_arg_type_args kind bound_vars) appl.r_args
        | `Input (t, s)
        | `Output (t, s) -> gt t @ gt s
        | `Select fields -> get_row_type_args kind bound_vars fields
        | `Choice fields -> get_row_type_args kind bound_vars fields
        | `Dual s -> gt s
        | `End -> []

and get_row_var_type_args : gen_kind -> TypeVarSet.t -> row_var -> type_arg list =
  fun kind bound_vars row_var ->
    match Unionfind.find row_var with
      | `Closed -> []
      | `Var (var, _, _) when TypeVarSet.mem var bound_vars -> []
      | `Var (_, _, `Flexible) when kind=`All -> [`Row (StringMap.empty, row_var, false)]
      | `Var (_, _, `Flexible) -> []
      | `Var (_, _, `Rigid) -> [`Row (StringMap.empty, row_var, false)]
      | `Recursive (var, rec_row) ->
          Debug.if_set (show_recursion) (fun () -> "rec (get_row_var_type_args): " ^(string_of_int var));
          (if TypeVarSet.mem var bound_vars then
             []
           else
             get_row_type_args kind (TypeVarSet.add var bound_vars) rec_row)
      | `Body row -> get_row_type_args kind bound_vars row

and get_presence_type_args : gen_kind -> TypeVarSet.t -> field_spec -> type_arg list =
  fun kind bound_vars ->
    function
      | `Present t -> get_type_args kind bound_vars t
      | `Absent -> []
      | `Var point ->
          begin
            match Unionfind.find point with
              | `Var (var, _, _) when TypeVarSet.mem var bound_vars -> []
              | `Var (_, _, `Flexible) when kind=`All -> [`Presence (`Var point)]
              | `Var (_, _, `Flexible) -> []
              | `Var (_, _,`Rigid) -> [`Presence (`Var point)]
              | `Body f -> get_presence_type_args kind bound_vars f
          end

and get_row_type_args : gen_kind -> TypeVarSet.t -> row -> type_arg list =
  fun kind bound_vars (field_env, row_var, _) ->
    let field_vars =
      StringMap.fold
        (fun _ field_spec vars ->
           vars @ get_presence_type_args kind bound_vars field_spec
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

(** Determine if two points have the same quantifier.

   Whenever we use {!Types.type_arg_of_quantifier}, we get a fresh point, and so
   it is not safe to use {!Unionfind.equivalent}. *)
let equivalent_tyarg l r =
  match Unionfind.find l, Unionfind.find r with
  | `Var (v, _, _), `Var (v', _, _) -> v = v'
  | _ -> assert false

let remove_duplicates =
  unduplicate (fun l r ->
                 match l, r with
                   | `Type (`MetaTypeVar l), `Type (`MetaTypeVar r) -> equivalent_tyarg l r
                   | `Row (_, l, ld), `Row (_, r, rd) -> ld=rd && equivalent_tyarg l r
                   | `Presence (`Var l), `Presence (`Var r) -> equivalent_tyarg l r
                   | _ -> false)

let get_type_args kind bound_vars t =
  remove_duplicates (get_type_args kind bound_vars t)

let env_type_vars (env : Types.environment) =
  TypeVarSet.union_all (List.map free_type_vars (Env.String.range env))

let rigidify_type_arg : type_arg -> unit =
  let rigidify_point point =
    match Unionfind.find point with
    | `Var (var, subkind, `Flexible) -> Unionfind.change point (`Var (var, subkind, `Rigid))
    | `Var _ -> ()
    | _ -> assert false
  in
    function
    | `Type (`MetaTypeVar point) -> rigidify_point point
    | `Row (_, point, _)         -> rigidify_point point
    | `Presence (`Var point)    -> rigidify_point point
    | _ -> raise (internal_error "Not a type-variable argument.")

(** Only flexible type variables should have the mono restriction. When we
   quantify over such variables (and so rigidify them), we need to convert any
   latent Mono variables into the more general Any one. *)
let mono_type_args : type_arg -> unit =
  let check_sk point =
    match Unionfind.find point with
    | `Var (var, (lin, Restriction.Mono), `Flexible) ->
       Unionfind.change point (`Var (var, (lin, Restriction.Any), `Flexible))
    | _ -> ()
  in
  function
  | `Type (`MetaTypeVar point) -> check_sk point
  | `Row (_, point, _) -> check_sk point
  | `Presence (`Var point) -> check_sk point
  | _ -> ()

(** generalise:
    Universally quantify any free type variables in the expression.
*)
let generalise : gen_kind -> ?unwrap:bool -> environment -> datatype -> ((quantifier list * type_arg list) * datatype) =
  fun kind ?(unwrap=true) env t ->
    (* throw away any existing top-level quantifiers *)
    Debug.if_set show_generalisation (fun () -> "Generalising : " ^ string_of_datatype t);
    let t = match Types.concrete_type t with
      | `ForAll (_, t) when unwrap -> t
      | _ -> t in
    let vars_in_env = env_type_vars env in
    let type_args = get_type_args kind vars_in_env t in
    List.iter mono_type_args type_args;
    let quantifiers = Types.quantifiers_of_type_args type_args in
    List.iter rigidify_type_arg type_args;
    let quantified = Types.for_all (quantifiers, t) in

    (* The following code suffers from the problem that it may reorder
       quantifiers: unbound type variables that can be generalised
       come before any existing quantifiers regardless of the order in
       which they appear in the body of the type.

       Throwing away existing top-level quantifiers (as above) is an
       easy fix.

       Another alternative would be to disallow explicit
       quantification that fails to quantify over all generalisable
       type variables - GHC does something like this.
    *)

(*

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
      end in
*)
      Debug.if_set show_generalisation (fun () -> "Generalised: " ^ string_of_datatype quantified);
      ((quantifiers, type_args), quantified)

(** only generalise rigid type variables *)
let generalise_rigid = generalise `Rigid

(** generalise both rigid and flexible type variables *)
let generalise = generalise `All

let get_quantifiers_rigid (env : environment) (t : datatype) : quantifier list =
  get_type_args `Rigid (env_type_vars env) t |> Types.quantifiers_of_type_args
