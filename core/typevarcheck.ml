open Utility
open Types

module FieldEnv = Utility.StringMap

(* TODO

   - Actually make use of the bool argument to is_guarded_row.  We
   can't do this until we change Unify.unify_rows' to take an extra
   parameter as well (to distinguish between record rows and variant
   rows).

   (SL: is the above behaviour actually desirable anyway? I think the
   idea was to only allow recursion to be inferred through variants
   and not records.)

   - Variance annotations.  *)

(* return true if all free occurrences of a variable are guarded in a type

   Guarded means 'only occurs inside a field of a row'.

   In line with OCaml, we should change it to mean 'only occurs inside
   a variant constructor'. This will require making changes to row
   unification as described above.
*)
let rec is_guarded : TypeVarSet.t -> StringSet.t -> int -> datatype -> bool =
  fun bound_vars expanded_apps var t ->
    let isg = is_guarded bound_vars expanded_apps var in
    let isgr row = is_guarded_row false bound_vars expanded_apps var row in
      match t with
        (* Unspecified kind *)
        | Not_typed -> true
        | (Var _ | Recursive _) ->
           failwith ("freestanding Var / Recursive not implemented yet (must be inside Meta)")
        | Alias (_, _, t) -> isg t
        | Application (_, ts) ->
            (* don't treat abstract type constructors as guards *)
            List.for_all (is_guarded_type_arg bound_vars expanded_apps var) ts
        | RecursiveApplication { r_unique_name; r_args; r_unwind; r_dual; _ } ->
            List.for_all
              (is_guarded_type_arg bound_vars expanded_apps var) r_args &&
            ((StringSet.mem r_unique_name expanded_apps) ||
             let body = r_unwind r_args r_dual in
             is_guarded
               bound_vars (StringSet.add r_unique_name expanded_apps) var body)
        | Meta point ->
            begin
              match Unionfind.find point with
                | Var (var', _, _) -> (var <> var')
                | Recursive (var', _kind, t) ->
                    (var=var' || TypeVarSet.mem var' bound_vars) ||
                    is_guarded
                      (TypeVarSet.add var' bound_vars) expanded_apps var t
                | t -> isg t
            end
        (* Type *)
        | Primitive _ -> true
        | Function (f, m, t) ->
            isg f && isgr m && isg t
        | Lolli (f, m, t) ->
            isg f && isgr m && isg t
        | Record row ->
           begin
             (* HACK: silly 1-tuple test *)
             match row with
             | Row (fields, row_var, _dual)
                  when
                    (FieldEnv.mem "1" fields &&
                       FieldEnv.size fields = 1 &&
                         Unionfind.find row_var = Closed) ->
                begin
                  match FieldEnv.find "1" fields with
                  | Present t        -> isg t
                  | (Absent | Var _) -> true
                  | _ -> raise Types.tag_expectation_mismatch
                end
             | _ ->
                isgr row
           end
        | Variant row -> isgr row
        | Table (_, f, d, r) -> isg f && isg d && isg r
        | Lens _sort -> true (* does not contain type variables *)
        | ForAll (qs, t) ->
            is_guarded
              (TypeVarSet.add_quantifiers qs bound_vars)
              expanded_apps var t
        (* Effect *)
        | Effect row -> isgr row
        | Operation (f, t, _) ->
            isg f && isg t
        (* Row *)
        | Row (fields, row_var, _dual) ->
           let check_fields = false in
           (if check_fields then
              (StringMap.fold
                 (fun _ f b -> b && isg f)
                 fields
                 true)
            else
              true) &&
             (is_guarded_row_var check_fields bound_vars expanded_apps var row_var)
        | Closed -> true
        (* Presence *)
        | Present t -> isg t
        | Absent -> true
        (* Session *)
        | Input (t, s)
        | Output (t, s) -> isg t && isg s
        | Select row
        | Choice row -> isgr row
        | Dual s -> isg s
        | End -> true
and is_guarded_row : bool -> TypeVarSet.t -> StringSet.t -> int -> row -> bool =
  fun _check_fields bound_vars expanded_apps var row ->
  is_guarded bound_vars expanded_apps var row
and is_guarded_row_var : bool -> TypeVarSet.t -> StringSet.t -> int -> row_var -> bool =
  fun check_fields bound_vars expanded_apps var row_var ->
    match Unionfind.find row_var with
      | Closed -> true
      | Var (var', _, _) -> var <> var'
      | Recursive (var', _kind, row) ->
          (var = var' || TypeVarSet.mem var' bound_vars) ||
            is_guarded_row check_fields
              (TypeVarSet.add var' bound_vars) expanded_apps var row
      | row ->
         is_guarded_row check_fields bound_vars expanded_apps var row
and is_guarded_type_arg : TypeVarSet.t -> StringSet.t -> int -> type_arg -> bool =
  fun bounded_vars expanded_apss var (_pk, t) ->
  is_guarded bounded_vars expanded_apss var t

(* return true if a variable occurs negatively in a type *)
let rec is_negative : TypeVarSet.t -> StringSet.t -> int -> datatype -> bool =
  fun bound_vars expanded_apps var t ->
    let isp = is_positive bound_vars expanded_apps var in
    let isn = is_negative bound_vars expanded_apps var in
    let isnr = is_negative_row bound_vars expanded_apps var in
      match t with
        (* Unspecified kind *)
        | Not_typed -> false
        | (Var _ | Recursive _) ->
           failwith ("freestanding Var / Recursive not implemented yet (must be inside Meta)")
        | Alias (_, _, t) -> isn t
        | Application (_, ts) ->
            List.exists (is_negative_type_arg bound_vars expanded_apps var) ts
        | RecursiveApplication { r_unique_name; r_args; r_unwind; r_dual; _ } ->
            List.exists
              (is_negative_type_arg bound_vars expanded_apps var) r_args ||
            StringSet.mem r_unique_name expanded_apps ||
            let body = r_unwind r_args r_dual in
              is_negative
                bound_vars (StringSet.add r_unique_name expanded_apps)
                var body
        | Meta point ->
            begin
              match Unionfind.find point with
                | Var _ -> false
                | Recursive (var', _kind, t) ->
                    not (TypeVarSet.mem var' bound_vars) &&
                      is_negative
                        (TypeVarSet.add var' bound_vars) expanded_apps var t
                | t -> isn t
            end
        (* Type *)
        | Primitive _ -> false
        | Function (f, m, t)
        | Lolli (f, m, t) ->
            isp f || isnr m || isn t
        | Record row -> isnr row
        | Variant row -> isnr row
        | Table (_, f, d, r) -> isn f || isn d || isn r
        | Lens typ -> is_negative_lens_type bound_vars expanded_apps var typ
        | ForAll (qs, t) ->
            is_negative
              (TypeVarSet.add_quantifiers qs bound_vars)
              expanded_apps var t
        (* Effect *)
        | Effect row -> isnr row
        | Operation (f, t, _) ->
            isp f || isn t
        (* Row *)
        | Row (field_env, row_var, _dual) ->
           is_negative_field_env bound_vars expanded_apps var field_env
           || is_negative_row_var bound_vars expanded_apps var row_var
        | Closed -> false
        (* Presence *)
        | Present t -> isn t
        | Absent -> false
        (* Session *)
        | Input (t, s)
        | Output (t, s) -> isn t && isn s
        | Select row -> isnr row
        | Choice row -> isnr row
        | Dual s -> isn s
        | End -> false
and is_negative_row : TypeVarSet.t -> StringSet.t -> int -> row -> bool =
  fun bound_vars expanded_apps var row ->
  is_negative bound_vars expanded_apps var row
and is_negative_field_env : TypeVarSet.t -> StringSet.t -> int -> field_spec_map -> bool =
  fun bound_vars expanded_apps var field_env ->
    FieldEnv.fold
      (fun _ spec result -> result || is_negative bound_vars expanded_apps var spec)
      field_env
      false
and is_negative_row_var : TypeVarSet.t -> StringSet.t -> int -> row_var -> bool =
  fun bound_vars expanded_apps var row_var ->
    match Unionfind.find row_var with
      | Closed
      | Var _ -> false
      | Recursive (var', _kind, row) ->
          not (TypeVarSet.mem var' bound_vars) &&
            is_negative_row (TypeVarSet.add var' bound_vars) expanded_apps var row
      | row ->
          is_negative_row bound_vars expanded_apps var row
and is_negative_type_arg : TypeVarSet.t -> StringSet.t -> int -> type_arg -> bool =
  fun bound_vars expanded_apps var (_pk, t)->
  is_negative bound_vars expanded_apps var t
and is_negative_lens_type = (*: TypeVarSet.t -> StringSet.t -> int -> Lens.Type.t -> bool =*)
  fun _bound_vars _expanded_apps _var _typ ->
    false

and is_positive : TypeVarSet.t -> StringSet.t -> int -> datatype -> bool =
  fun bound_vars expanded_apps var t ->
    let isp = is_positive bound_vars expanded_apps var in
    let isn = is_negative bound_vars expanded_apps var in
    let ispr = is_positive_row bound_vars expanded_apps var in
      match t with
        (* Unspecified kind *)
        | Not_typed -> false
        | (Var _ | Recursive _) ->
           failwith ("freestanding Var / Recursive not implemented yet (must be inside Meta)")
        | Alias (_, _, t) -> isp t
        | Application (_, ts) ->
            List.exists (is_positive_type_arg bound_vars expanded_apps var) ts
        | RecursiveApplication { r_unique_name; r_args; r_unwind; r_dual; _ } ->
            List.exists
              (is_positive_type_arg bound_vars expanded_apps var) r_args ||
            StringSet.mem r_unique_name expanded_apps ||
            let body = r_unwind r_args r_dual in
              is_positive
                bound_vars (StringSet.add r_unique_name expanded_apps)
                var body
        | Meta point ->
            begin
              match Unionfind.find point with
                | Var (var', _, _) -> var = var'
                | Recursive (var', _kind, t) ->
                    not (TypeVarSet.mem var' bound_vars) &&
                      is_positive
                        (TypeVarSet.add var' bound_vars) expanded_apps var t
                | t -> isp t
            end
        (* Type *)
        | Primitive _ -> false
        | Function (f, m, t)
        | Lolli (f, m, t) ->
            isn f || ispr m || isp t
        | Record row -> ispr row
        | Variant row -> ispr row
        | Table (_, f, d, r) -> isp f || isp d || isp r
        | Lens typ -> is_positive_lens_type bound_vars expanded_apps var typ
        | ForAll (qs, t) ->
            is_positive
              (TypeVarSet.add_quantifiers qs bound_vars)
              expanded_apps var t
        (* Effect *)
        | Effect row -> ispr row
        | Operation (f, t, _) ->
            isn f || isp t
        (* Row *)
        | Row (field_env, row_var, _dual) ->
           is_positive_field_env bound_vars expanded_apps var field_env
           || is_positive_row_var bound_vars expanded_apps var row_var
        | Closed -> false
        (* Presence *)
        | Present t -> isp t
        | Absent -> false
        (* Session *)
        | Input (t, s)
        | Output (t, s) -> isp t && isp s
        | Select row -> ispr row
        | Choice row -> ispr row
        | Dual s -> isp s
        | End -> false
and is_positive_row : TypeVarSet.t -> StringSet.t -> int -> row -> bool =
  fun bound_vars expanded_apps var row ->
  is_positive bound_vars expanded_apps var row
and is_positive_field_env : TypeVarSet.t -> StringSet.t -> int -> field_spec_map -> bool =
  fun bound_vars expanded_apps var field_env ->
    FieldEnv.fold
      (fun _ spec result -> result || is_positive bound_vars expanded_apps var spec)
      field_env
      false
and is_positive_row_var : TypeVarSet.t -> StringSet.t -> int -> row_var -> bool =
  fun bound_vars expanded_apps var row_var ->
    match Unionfind.find row_var with
      | Closed -> false
      | Var (var', _, _) -> var = var'
      | Recursive (var', _kind, row) ->
          not (TypeVarSet.mem var' bound_vars) &&
            is_positive_row (TypeVarSet.add var' bound_vars) expanded_apps var row
      | row ->
          is_positive_row bound_vars expanded_apps var row
and is_positive_type_arg : TypeVarSet.t -> StringSet.t -> int -> type_arg -> bool =
  fun bound_vars expanded_apps var (_pk, t) ->
  is_positive bound_vars expanded_apps var t
and is_positive_lens_type = (*: TypeVarSet.t -> StringSet.t -> int -> Lens.Type.t -> bool =*)
  fun _bound_vars _expanded_apps _var _typ ->
    false

let is_guarded = is_guarded TypeVarSet.empty StringSet.empty
let is_guarded_row = is_guarded_row false TypeVarSet.empty StringSet.empty

let is_negative = is_negative TypeVarSet.empty StringSet.empty
let is_negative_row = is_negative_row TypeVarSet.empty StringSet.empty
let is_negative_field_env = is_negative_field_env TypeVarSet.empty StringSet.empty
let is_negative_row_var = is_negative_row_var TypeVarSet.empty StringSet.empty

let is_positive = is_positive TypeVarSet.empty StringSet.empty
let is_positive_row = is_positive_row TypeVarSet.empty StringSet.empty
let is_positive_field_env = is_positive_field_env TypeVarSet.empty StringSet.empty
let is_positive_row_var = is_positive_row_var TypeVarSet.empty StringSet.empty
