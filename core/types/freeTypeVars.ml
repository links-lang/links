(* Detection of free variables in types *)
(* Do not use this module directly, but its export via FreeTypeVars.*)


open Types
open CommonTypes
open Utility

module FieldEnv = Utility.StringMap


type flavour = [`Rigid | `Flexible | `Recursive]
type kind    = PrimaryKind.t
type scope   = [`Free | `Bound]
type spec    = flavour * kind * int

type vars_list = (int * (flavour * kind * scope)) list
type names  = (int, string * spec) Hashtbl.t



let varspec_of_tyvar q =
  var_of_quantifier q, (`Rigid, primary_kind_of_quantifier q, `Bound)

(* find all free and bound type variables *)
let rec free_bound_type_vars : TypeVarSet.t -> datatype -> vars_list = fun bound_vars t ->
  let fbtv = free_bound_type_vars bound_vars in
    match t with
      | `Not_typed -> []
      | `Primitive _ -> []
      | `MetaTypeVar point ->
          begin
            match Unionfind.find point with
              | `Var (var, _, freedom) ->
                    [var, ((freedom :> flavour), pk_type, `Free)]
              | `Recursive (var, body) ->
                  if TypeVarSet.mem var bound_vars then
                    [var, (`Recursive, pk_type, `Bound)]
                  else
                    (var, (`Recursive, pk_type, `Bound))::(free_bound_type_vars (TypeVarSet.add var bound_vars) body)
              | `Body t -> fbtv t
          end
      | `Function (f, m, t) ->
          (fbtv f) @ (free_bound_row_type_vars bound_vars m) @ (fbtv t)
      | `Lolli (f, m, t) ->
          (fbtv f) @ (free_bound_row_type_vars bound_vars m) @ (fbtv t)
      | `Record row
      | `Variant row -> free_bound_row_type_vars bound_vars row
      | `Lens _ -> []
      | `Effect row -> free_bound_row_type_vars bound_vars row
      | `Table (r, w, n) -> (fbtv r) @ (fbtv w) @ (fbtv n)
      | `ForAll (tyvars, body) ->
          let bound_vars, vars =
            List.fold_left
              (fun (bound_vars, vars) tyvar ->
                 let var, spec = varspec_of_tyvar tyvar in
                   TypeVarSet.add var bound_vars, (var, spec)::vars)
              (bound_vars, [])
              tyvars
          in
            (List.rev vars) @ (free_bound_type_vars bound_vars body)
      | `Alias ((_, _, ts), _) ->
          concat_map (free_bound_tyarg_vars bound_vars) ts
      | `Application (_, tyargs) ->
          List.concat (List.map (free_bound_tyarg_vars bound_vars) tyargs)
      | `RecursiveApplication { r_args; _ } ->
          List.concat (List.map (free_bound_tyarg_vars bound_vars) r_args)
      | `Input (t, s)
      | `Output (t, s) ->
         free_bound_type_vars bound_vars t @ free_bound_type_vars bound_vars s
      | `Select row
      | `Choice row -> free_bound_row_type_vars bound_vars row
      | `Dual s -> free_bound_type_vars bound_vars s
      | `End -> []
and free_bound_field_spec_type_vars bound_vars =
  function
    | `Present t -> free_bound_type_vars bound_vars t
    | `Absent -> []
    | `Var point ->
        begin
          match Unionfind.find point with
            | `Var (var, _, freedom) ->
                  [var, ((freedom :> flavour), pk_presence, `Free)]
            | `Body f -> free_bound_field_spec_type_vars bound_vars f
        end
and free_bound_row_type_vars bound_vars (field_env, row_var, _) =
  let field_type_vars =
    FieldEnv.fold
      (fun _name f tvs ->
         tvs @ free_bound_field_spec_type_vars bound_vars f)
      field_env [] in
  let row_var = free_bound_row_var_vars bound_vars row_var in
    field_type_vars @ row_var
and free_bound_row_var_vars bound_vars row_var =
  match Unionfind.find row_var with
    | `Closed -> []
    | `Var (var, _, freedom) ->
          [var, ((freedom :> flavour), pk_row, `Free)]
    | `Recursive (var, row) ->
        if TypeVarSet.mem var bound_vars then
          [var, (`Recursive, pk_row, `Bound)]
        else
          (var, (`Recursive, pk_row, `Bound))::(free_bound_row_type_vars (TypeVarSet.add var bound_vars) row)
    | `Body row -> free_bound_row_type_vars bound_vars row
and free_bound_tyarg_vars bound_vars =
  function
    | `Type t -> free_bound_type_vars bound_vars t
    | `Row row -> free_bound_row_type_vars bound_vars row
    | `Presence f -> free_bound_field_spec_type_vars bound_vars f

let free_bound_quantifier_vars quant =
  let var, spec = varspec_of_tyvar quant in
  [(var, spec)]

let free_bound_tycon_vars bound_vars tycon_spec =
  let split_vars tyvars =
    let bound_vars, vars =
      List.fold_left
        (fun (bound_vars, vars) tyvar ->
           let var, spec = varspec_of_tyvar tyvar in
             (TypeVarSet.add var bound_vars, (var, spec)::vars)) (bound_vars, []) tyvars in
      (bound_vars, List.rev vars) in

  match tycon_spec with
    | `Alias (tyvars, body) ->
        let (bound_vars, vars) = split_vars tyvars in
        vars @ (free_bound_type_vars bound_vars body)
    | `Mutual (tyvars, _) -> snd (split_vars tyvars)
    | `Abstract _ -> []





let find      var tbl = fst (Hashtbl.find tbl var)
let find_spec var tbl =      Hashtbl.find tbl var



let free_type_vars, free_row_type_vars, free_tyarg_vars =
  let module S = TypeVarSet in
  let rec free_type_vars' : S.t -> datatype -> S.t = fun rec_vars ->
    function
      | `Not_typed               -> S.empty
      | `Primitive _             -> S.empty
      | `Function (f, m, t)      ->
         S.union_all [free_type_vars' rec_vars f; free_row_type_vars' rec_vars m; free_type_vars' rec_vars t]
      | `Lolli (f, m, t)         ->
         S.union_all [free_type_vars' rec_vars f; free_row_type_vars' rec_vars m; free_type_vars' rec_vars t]
      | `Effect row
      | `Record row
      | `Variant row             -> free_row_type_vars' rec_vars row
      | `Table (r, w, n)         ->
          S.union_all
            [free_type_vars' rec_vars r; free_type_vars' rec_vars w; free_type_vars' rec_vars n]
      | `Lens _          -> S.empty
      | `Alias ((_, _, ts), datatype) ->
          S.union (S.union_all (List.map (free_tyarg_vars' rec_vars) ts)) (free_type_vars' rec_vars datatype)
      | `Application (_, tyargs) -> S.union_all (List.map (free_tyarg_vars' rec_vars) tyargs)
      | `RecursiveApplication { r_args; _ } ->
          S.union_all (List.map (free_tyarg_vars' rec_vars) r_args)
      | `ForAll (tvars, body)    -> S.diff (free_type_vars' rec_vars body)
                                           (List.fold_right (S.add -<- type_var_number) tvars S.empty)
      | `MetaTypeVar point       ->
          begin
            match Unionfind.find point with
              | `Var (var, _, _) -> S.singleton(var)
              | `Recursive (var, body) ->
                  if S.mem var rec_vars then
                    S.empty
                  else
                    free_type_vars' (S.add var rec_vars) body
              | `Body t ->
                  free_type_vars' rec_vars t
          end
      | `Input (t, s)
      | `Output (t, s) -> S.union (free_type_vars' rec_vars t) (free_type_vars' rec_vars s)
      | `Select fields -> free_row_type_vars' rec_vars fields
      | `Choice fields -> free_row_type_vars' rec_vars fields
      | `Dual s -> free_type_vars' rec_vars s
      | `End -> S.empty
  and free_field_spec_type_vars' : S.t -> field_spec -> S.t =
    fun rec_vars ->
      function
        | `Present t -> free_type_vars' rec_vars t
        | `Absent -> S.empty
        | `Var point ->
            begin
              match Unionfind.find point with
                | `Var (var, _, _) -> S.singleton(var)
                | `Body f -> free_field_spec_type_vars' rec_vars f
            end
  and free_row_type_vars' : S.t -> row -> S.t =
    fun rec_vars (field_env, row_var, _) ->
      let field_vars =
        FieldEnv.fold
          (fun _ (f : field_spec) field_vars ->
             S.union field_vars (free_field_spec_type_vars' rec_vars f))
          field_env S.empty in
      let row_vars =
        match Unionfind.find row_var with
          | `Var (var, _, _) -> S.singleton(var)
          | `Recursive (var, body) ->
              if S.mem var rec_vars then
                S.empty
              else
                free_row_type_vars' (S.add var rec_vars) body
          | `Body row ->
              free_row_type_vars' rec_vars row
          | `Closed -> S.empty
      in
        S.union field_vars row_vars
  and free_tyarg_vars' : S.t -> type_arg -> S.t =
    fun rec_vars ->
      function
        | `Type t -> free_type_vars' rec_vars t
        | `Row row -> free_row_type_vars' rec_vars row
        | `Presence f -> free_field_spec_type_vars' rec_vars f
  in
    ((free_type_vars' S.empty),
     (free_row_type_vars' S.empty),
     (free_tyarg_vars' S.empty))


let free_bound_type_vars            = free_bound_type_vars TypeVarSet.empty
let free_bound_row_type_vars        = free_bound_row_type_vars TypeVarSet.empty
let free_bound_field_spec_type_vars = free_bound_field_spec_type_vars TypeVarSet.empty
let free_bound_type_arg_type_vars   = free_bound_tyarg_vars TypeVarSet.empty
let free_bound_row_var_vars         = free_bound_row_var_vars TypeVarSet.empty
let free_bound_quantifier_vars      = free_bound_quantifier_vars
let free_bound_tycon_type_vars      = free_bound_tycon_vars TypeVarSet.empty
