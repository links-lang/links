open Utility
open Types

module FieldEnv = Utility.StringMap

let bind_quantifiers  = List.fold_right (Types.type_var_number ->- TypeVarSet.add)

(* TODO

   - Actually make use of the bool argument to is_guarded_row.  We can't
   do this until we change Unify.unify_rows' to take an extra
   parameter as well (to distinguish between record rows and variant
   rows).

   - Variance annotations.
*)

(* return true if all free occurrences of a variable are guarded in a type

   Guarded means 'only occurs inside a field of a row'.

   In line with OCaml, we should change it to mean 'only occurs inside
   a variant constructor'. This will require making changes to row
   unification as described above.
*)
let rec is_guarded : TypeVarSet.t -> int -> datatype -> bool =
  fun bound_vars var t ->
    let isg = is_guarded bound_vars var in
    let isgr row = is_guarded_row false bound_vars var row in
    let isgv row = is_guarded_row false bound_vars var row in
      match t with
        | `Not_typed -> true
        | `Primitive _ -> true
        | `MetaTypeVar point ->
            begin
              match Unionfind.find point with
                | `Var (var', _, _) -> (var <> var')
                | `Recursive (var', t) ->
                    (var=var' || TypeVarSet.mem var' bound_vars) ||
                      is_guarded (TypeVarSet.add var' bound_vars) var t
                | `Body t -> isg t
            end
        | `Function (f, m, t) ->
            isg f && isgr m && isg t
        | `Lolli (f, m, t) ->
            isg f && isgr m && isg t
        | `ForAll (qs, t) ->
            is_guarded (bind_quantifiers (unbox_quantifiers qs) bound_vars) var t
        | `Record row ->
            begin
              (* HACK: silly 1-tuple test *)
              match row with
                | (fields, row_var, _dual)
                    when
                      (FieldEnv.mem "1" fields &&
                         FieldEnv.size fields = 1 &&
                          Unionfind.find row_var = `Closed) ->
                  begin
                    match FieldEnv.find "1" fields with
                    | `Present t         -> isg t
                    | (`Absent | `Var _) -> true
                  end
                | _ ->
                    isgr row
            end
        | `Effect row
        | `Variant row -> isgv row
        | `Table (f, d, r) -> isg f && isg d && isg r
        | `Alias (_, t) -> is_guarded bound_vars var t
        | `Application (_, ts) ->
            (* don't treat abstract type constructors as guards *)
            List.for_all (is_guarded_type_arg bound_vars var) ts
        | `Input (t, s)
        | `Output (t, s) -> isg t && isg s
        | `Select row
        | `Choice row -> isgv row
        | `Dual s -> isg s
        | `End -> true
and is_guarded_row : bool -> TypeVarSet.t -> int -> row -> bool =
  fun check_fields bound_vars var (fields, row_var, _dual) ->
    (if check_fields then
       (StringMap.fold
          (fun _ f b ->
            match f with
            | `Present t         -> b && is_guarded bound_vars var t
            | (`Absent | `Var _) -> b)
          fields
          true)
     else
       true) &&
      (is_guarded_row_var check_fields bound_vars var row_var)
and is_guarded_row_var : bool -> TypeVarSet.t -> int -> row_var -> bool =
  fun check_fields bound_vars var row_var ->
    match Unionfind.find row_var with
      | `Closed -> true
      | `Var (var', _, _) -> var <> var'
      | `Recursive (var', row) ->
          (var=var' || TypeVarSet.mem var' bound_vars) ||
            is_guarded_row check_fields (TypeVarSet.add var' bound_vars) var row
      | `Body row ->
          is_guarded_row check_fields bound_vars var row
and is_guarded_type_arg : TypeVarSet.t -> int -> type_arg -> bool =
  fun bound_vars var ->
    function
      | `Type t -> is_guarded bound_vars var t
      | `Row r -> is_guarded_row false bound_vars var r
      | `Presence _ -> true

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
                | `Var _ -> false
                | `Recursive (var', t) ->
                    not (TypeVarSet.mem var' bound_vars) &&
                      is_negative (TypeVarSet.add var' bound_vars) var t
                | `Body t -> isn t
            end
        | `Function (f, m, t)
        | `Lolli (f, m, t) ->
            isp f || isnr m || isn t
        | `ForAll (qs, t) -> is_negative (bind_quantifiers (unbox_quantifiers qs) bound_vars) var t
        | `Record row -> isnr row
        | `Effect row
        | `Variant row -> isnr row
        | `Table (f, d, r) -> isn f || isn d || isn r
        | `Alias (_, t) -> isn t
        | `Application (_, ts) ->
            List.exists (is_negative_type_arg bound_vars var) ts
        | `Input (t, s)
        | `Output (t, s) -> isn t && isn s
        | `Select row -> isnr row
        | `Choice row -> isnr row
	| `End -> false
        | `Dual s -> isn s
and is_negative_row : TypeVarSet.t -> int -> row -> bool =
  fun bound_vars var (field_env, row_var, _dual) ->
    is_negative_field_env bound_vars var field_env || is_negative_row_var bound_vars var row_var
and is_negative_field_env : TypeVarSet.t -> int -> field_spec_map -> bool =
  fun bound_vars var field_env ->
    FieldEnv.fold (fun _ spec result ->
                      match spec with
                        | `Present t -> result || is_negative bound_vars var t
                        | `Absent    -> false
                        | `Var _     -> false
                          (* TODO: shouldn't we handle this case somehow? *)
                   ) field_env false
and is_negative_row_var : TypeVarSet.t -> int -> row_var -> bool =
  fun bound_vars var row_var ->
    match Unionfind.find row_var with
      | `Closed
      | `Var _ -> false
      | `Recursive (var', row) ->
          not (TypeVarSet.mem var' bound_vars) &&
            is_negative_row (TypeVarSet.add var' bound_vars) var row
      | `Body row ->
          is_negative_row bound_vars var row
and is_negative_type_arg : TypeVarSet.t -> int -> type_arg -> bool =
  fun bound_vars var ->
    function
      | `Type t -> is_negative bound_vars var t
      | `Row r -> is_negative_row bound_vars var r
      | `Presence _ -> false

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
                | `Var _ -> false
                | `Recursive (var', t) ->
                    not (TypeVarSet.mem var' bound_vars) &&
                      is_positive (TypeVarSet.add var' bound_vars) var t
                | `Body t -> isp t
            end
        | `Function (f, m, t) ->
            isn f || ispr m || isp t
        | `Lolli (f, m, t) ->
            isn f || ispr m || isp t
        | `ForAll (qs, t) -> is_positive (bind_quantifiers (unbox_quantifiers qs) bound_vars) var t
        | `Record row -> ispr row
        | `Effect row
        | `Variant row -> ispr row
        | `Table (f, d, r) -> isp f || isp d || isp r
        | `Alias (_, t) -> isp t
        | `Application (_, ts) ->
            List.exists (is_positive_type_arg bound_vars var) ts
        | `Input (t, s)
        | `Output (t, s) -> isp t && isp s
        | `Select row -> ispr row
        | `Choice row -> ispr row
        | `End -> false
        | `Dual s -> isp s
and is_positive_row : TypeVarSet.t -> int -> row -> bool =
  fun bound_vars var (field_env, row_var, _dual) ->
    is_positive_field_env bound_vars var field_env || is_positive_row_var bound_vars var row_var
and is_positive_presence : TypeVarSet.t -> int -> field_spec -> bool =
  fun bound_vars var ->
    function
      | `Absent    -> false
      | `Present t -> is_positive bound_vars var t
      | `Var point ->
          begin
            match Unionfind.find point with
              | `Var (var', _, _) -> var = var'
              | `Body f -> is_positive_presence bound_vars var f
          end
and is_positive_field_env : TypeVarSet.t -> int -> field_spec_map -> bool =
  fun bound_vars var field_env ->
    FieldEnv.fold
      (fun _ f result ->
         result || is_positive_presence bound_vars var f)
      field_env false
and is_positive_row_var : TypeVarSet.t -> int -> row_var -> bool =
  fun bound_vars var row_var ->
    match Unionfind.find row_var with
      | `Closed -> false
      | `Var (var', _, _) -> var=var'
      | `Recursive (var', row) ->
          not (TypeVarSet.mem var' bound_vars) &&
            is_positive_row (TypeVarSet.add var' bound_vars) var row
      | `Body row ->
          is_positive_row bound_vars var row
and is_positive_type_arg : TypeVarSet.t -> int -> type_arg -> bool =
  fun bound_vars var ->
    function
      | `Type t -> is_positive bound_vars var t
      | `Row r -> is_positive_row bound_vars var r
      | `Presence f -> is_positive_presence bound_vars var f

let is_guarded = is_guarded TypeVarSet.empty
let is_guarded_row = is_guarded_row false TypeVarSet.empty

let is_negative = is_negative TypeVarSet.empty
let is_negative_row = is_negative_row TypeVarSet.empty
let is_negative_field_env = is_negative_field_env TypeVarSet.empty
let is_negative_row_var = is_negative_row_var TypeVarSet.empty

let is_positive = is_positive TypeVarSet.empty
let is_positive_row = is_positive_row TypeVarSet.empty
let is_positive_field_env = is_positive_field_env TypeVarSet.empty
let is_positive_row_var = is_positive_row_var TypeVarSet.empty
