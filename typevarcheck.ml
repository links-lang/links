open Utility
open Types
open Instantiate

module FieldEnv = Utility.StringMap

let bind_quantifiers  = List.fold_right (Types.type_var_number ->- TypeVarSet.add)

(* return true if all free occurrences of a variable are guarded in a type *)
let rec is_guarded : TypeVarSet.t -> int -> datatype -> bool =
  fun bound_vars var t ->
    let isg = is_guarded bound_vars var in
    let isgr = is_guarded_row bound_vars var in
      match t with
        | `Not_typed -> true
        | `Primitive _ -> true
        | `MetaTypeVar point ->
            begin
              match Unionfind.find point with
                | `Flexible var'
                | `Rigid var' -> (var <> var')
                | `Recursive (var', t) ->
                    (var=var' || TypeVarSet.mem var' bound_vars) ||
                      is_guarded (TypeVarSet.add var' bound_vars) var t
                | `Body t -> isg t
            end
        | `Function (f, m, t) ->
            isg f && isg m && isg t
        | `ForAll (qs, t) -> 
            is_guarded (bind_quantifiers qs bound_vars) var t
        | `Record row -> isgr row
        | `Variant row -> isgr row
        | `Table (r, w) -> isg r && isg w
        | `Alias (_, t) -> is_guarded bound_vars var t
        | `Application _ -> true
            
and is_guarded_row : TypeVarSet.t -> int -> row -> bool =
  fun bound_vars var (field_env, row_var) ->
    is_guarded_row_var bound_vars var row_var
and is_guarded_row_var : TypeVarSet.t -> int -> row_var -> bool =
  fun bound_vars var row_var ->
    match Unionfind.find row_var with
      | `Closed -> true
      | `Flexible var'
      | `Rigid var' -> var <> var'
      | `Recursive (var', row) ->
          (var=var' || TypeVarSet.mem var' bound_vars) ||
            is_guarded_row (TypeVarSet.add var' bound_vars) var row
      | `Body row ->
          is_guarded_row bound_vars var row

(* return true if a variable occurs negatively in a type *)
let rec is_negative : TypeVarSet.t -> int -> datatype -> bool =
  fun bound_vars var t ->
    let isp = is_positive bound_vars var in
    let isn = is_negative bound_vars var in
    let isnr = is_negative_row bound_vars var in
      match t with
        | `Not_typed -> false
        | `Primitive _ -> false
        | `MetaTypeVar point ->
            begin
              match Unionfind.find point with
                | `Flexible _
                | `Rigid _ -> false
                | `Recursive (var', t) ->
                    not (TypeVarSet.mem var' bound_vars) &&
                      is_negative (TypeVarSet.add var' bound_vars) var t
                | `Body t -> isn t
            end
        | `Function (f, m, t) ->
            isp f || isp m || isn t
        | `ForAll (qs, t) -> is_negative (bind_quantifiers qs bound_vars) var t
        | `Record row -> isnr row
        | `Variant row -> isnr row
        | `Table (r, w) -> isn r || isn w
        | `Alias (_, t) -> isn t
        | `Application (_, ts) -> 
            List.exists isn ts
and is_negative_row : TypeVarSet.t -> int -> row -> bool =
  fun bound_vars var (field_env, row_var) ->
    is_negative_field_env bound_vars var field_env || is_negative_row_var bound_vars var row_var
and is_negative_field_env : TypeVarSet.t -> int -> field_spec_map -> bool =
  fun bound_vars var field_env ->
    FieldEnv.fold (fun _ spec result ->
                      match spec with
                        | `Absent -> result
                        | `Present t -> result || is_negative bound_vars var t
                   ) field_env false
and is_negative_row_var : TypeVarSet.t -> int -> row_var -> bool =
  fun bound_vars var row_var ->
    match Unionfind.find row_var with
      | `Closed
      | `Flexible _
      | `Rigid _ -> false
      | `Recursive (var', row) ->
          not (TypeVarSet.mem var' bound_vars) &&
            is_negative_row (TypeVarSet.add var' bound_vars) var row
      | `Body row ->
          is_negative_row bound_vars var row

and is_positive : TypeVarSet.t -> int -> datatype -> bool =
  fun bound_vars var t ->
    let isp = is_positive bound_vars var in
    let isn = is_negative bound_vars var in
    let ispr = is_positive_row bound_vars var in
      match t with
        | `Not_typed -> false
        | `Primitive _ -> false
        | `MetaTypeVar point ->
            begin
              match Unionfind.find point with
                | `Flexible _
                | `Rigid _ -> false
                | `Recursive (var', t) ->
                    not (TypeVarSet.mem var' bound_vars) &&
                      is_positive (TypeVarSet.add var' bound_vars) var t
                | `Body t -> isp t
            end
        | `Function (f, m, t) ->
            isn f || isn m || isp t
        | `ForAll (qs, t) -> is_positive (bind_quantifiers qs bound_vars) var t
        | `Record row -> ispr row
        | `Variant row -> ispr row
        | `Table (r, w) -> isp r || isp w
        | `Alias (_, t) -> isp t
        | `Application (s, ts) ->
            List.exists isp ts
and is_positive_row : TypeVarSet.t -> int -> row -> bool =
  fun bound_vars var (field_env, row_var) ->
    is_positive_field_env bound_vars var field_env || is_positive_row_var bound_vars var row_var
and is_positive_field_env : TypeVarSet.t -> int -> field_spec_map -> bool =
  fun bound_vars var field_env ->
    FieldEnv.fold (fun _ spec result ->
                      match spec with
                        | `Absent -> result
                        | `Present t -> result || is_positive bound_vars var t
                   ) field_env false
and is_positive_row_var : TypeVarSet.t -> int -> row_var -> bool =
  fun bound_vars var row_var ->
    match Unionfind.find row_var with
      | `Closed -> false
      | `Flexible var'
      | `Rigid var' -> var=var;
      | `Recursive (var', row) ->
          not (TypeVarSet.mem var' bound_vars) &&
            is_positive_row (TypeVarSet.add var' bound_vars) var row
      | `Body row ->
          is_positive_row bound_vars var row

let is_guarded = is_guarded TypeVarSet.empty
let is_guarded_row = is_guarded_row TypeVarSet.empty

let is_negative = is_negative TypeVarSet.empty
let is_negative_row = is_negative_row TypeVarSet.empty
let is_negative_field_env = is_negative_field_env TypeVarSet.empty
let is_negative_row_var = is_negative_row_var TypeVarSet.empty

let is_positive = is_positive TypeVarSet.empty
let is_positive_row = is_positive_row TypeVarSet.empty
let is_positive_field_env = is_positive_field_env TypeVarSet.empty
let is_positive_row_var = is_positive_row_var TypeVarSet.empty
