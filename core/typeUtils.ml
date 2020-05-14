open CommonTypes
open Utility
open Types

(** type destructors *)
exception TypeDestructionError of string

let error t = raise (TypeDestructionError t)


(** remove any top-level meta typevars and aliases from a type
    (perhaps we can use this version of concrete_type everywhere)
*)
let concrete_type t =
  let rec ct rec_names t : datatype =
    match t with
      | Alias (_, t) -> ct rec_names t
      | Meta point ->
          begin
            match Unionfind.find point with
              | Recursive (var, _kind, t) ->
                  if RecIdSet.mem (MuBoundId var) rec_names then
                    Meta point
                  else
                    ct (RecIdSet.add (MuBoundId var) rec_names) t
              | t -> ct rec_names t
          end
      | ForAll (qs, t) ->
          begin
            match ct rec_names t with
              | ForAll (qs', t') ->
                  ForAll (qs @ qs', t')
              | t ->
                  begin
                    match qs with
                      | [] -> t
                      | _ -> ForAll (qs, t)
                  end
          end
      | Dual s -> dual_type s
      | RecursiveApplication ({ r_unique_name; r_dual; r_args; r_unwind ; _ } as appl) ->
          if (RecIdSet.mem (NominalId r_unique_name) rec_names) then
            RecursiveApplication appl
          else
            let body = r_unwind r_args r_dual in
            ct (RecIdSet.add (NominalId r_unique_name) rec_names) body
      | _ -> t
  in
    ct RecIdSet.empty t

let extract_row t = match concrete_type t with
  | Effect row
  | Record row -> row
  | Variant row -> row
  | t ->
      error
        ("Internal error: attempt to extract a row from a datatype that is not a record or a variant: "
         ^ string_of_datatype t)


let extract_row_parts : Types.t -> Types.row' = function
    | Row parts -> parts
    | t -> error
             ("Internal error: attempt to extract row parts from a datatype that is not a row "
              ^ string_of_datatype t)


let split_row name row =
  let (field_env, row_var, dual) = fst (unwrap_row row) |> extract_row_parts in
  let t =
    if StringMap.mem name field_env then
      match (StringMap.find name field_env) with
        | Present t -> t
        | Absent ->
            error ("Attempt to split row "^string_of_row row ^" on absent field " ^ name)
        | Var _ ->
            error ("Attempt to split row "^string_of_row row ^" on var field " ^ name)
        | _ -> raise Types.tag_expectation_mismatch
    else
      error ("Attempt to split row "^string_of_row row ^" on absent field " ^ name)
  in
    t, Row (StringMap.remove name field_env, row_var, dual)

let rec variant_at ?(overstep_quantifiers=true) name t = match (concrete_type t, overstep_quantifiers) with
  | (ForAll (_, t), true) -> variant_at name t
  | (Variant row, _) ->
      let t, _ = split_row name row in t
  | (t, _) ->
      error ("Attempt to deconstruct non-variant type "^string_of_datatype t)

let rec split_variant_type name t = match concrete_type t with
  | ForAll (_, t) -> split_variant_type name t
  | Variant row ->
      let t, row = split_row name row in
        Variant (make_singleton_closed_row (name, Present t)), Variant row
  | t ->
      error ("Attempt to split non-variant type "^string_of_datatype t)

let rec project_type ?(overstep_quantifiers=true) name t = match (concrete_type t, overstep_quantifiers) with
  | (ForAll (_, t), true) -> project_type name t
  | (Record row, _) ->
      let t, _ = split_row name row in
        t
  | (t, _) ->
      error ("Attempt to project non-record type "^string_of_datatype t)

let rec select_type name t = match concrete_type t with
  | ForAll (_, t) -> select_type name t
  | Select row ->
    let t, _ = split_row name row in t
  | t ->
    error ("Attempt to select from non-selection type "^string_of_datatype (concrete_type t))

let rec split_choice_type name t = match concrete_type t with
  | ForAll (_, t) -> split_choice_type name t
  | Choice row ->
      let t, row = split_row name row in
        Choice (make_singleton_closed_row (name, Present t)), Choice row
  | t ->
      error ("Attempt to split non-choice type "^string_of_datatype t)

let rec choice_at name t = match concrete_type t with
  | ForAll (_, t) -> choice_at name t
  | Choice row ->
      let t, _ = split_row name row in t
  | t ->
      error ("Attempt to deconstruct non-choice type "^string_of_datatype t)


(*
  This returns the type obtained by removing a set of
  fields from a record.
*)
let rec erase_type ?(overstep_quantifiers=true) names t =
  match (concrete_type t, overstep_quantifiers) with
  | (ForAll (_, t), true) -> erase_type names t
  | (Record row, _) ->
    let closed = is_closed_row row in
      let (field_env, row_var, duality) = fst (unwrap_row row) |> extract_row_parts in
      let field_env =
        StringSet.fold
          (fun name field_env ->
            match StringMap.lookup name field_env with
            | Some (Present _) ->
              if closed then
                StringMap.remove name field_env
              else
                StringMap.add name Absent field_env
            | Some Absent ->
              error ("Attempt to remove absent field "^name^" from row "^string_of_row row)
            | Some (Var _) ->
              error ("Attempt to remove var field "^name^" from row "^string_of_row row)
            | Some _ ->
              raise Types.tag_expectation_mismatch
            | None ->
              error ("Attempt to remove absent field "^name^" from row "^string_of_row row))
          names
          field_env
      in
        Record (Row (field_env, row_var, duality))
  | (t, _) -> error ("Attempt to erase field from non-record type "^string_of_datatype t)

let rec return_type ?(overstep_quantifiers=true) t = match (concrete_type t, overstep_quantifiers)  with
  | (ForAll (_, t), true) -> return_type t
  | (Function (_, _, t), _) -> t
  | (Lolli (_, _, t), _) -> t
  | (t, _) ->
      error ("Attempt to take return type of non-function: " ^ string_of_datatype t)

let rec arg_types ?(overstep_quantifiers=true) t = match (concrete_type t, overstep_quantifiers) with
  | (ForAll (_, t), true) -> arg_types t
  | (Function (Record row, _, _), _) ->
      extract_tuple row
  | (Lolli (Record row, _, _), _) ->
     extract_tuple row
(*   | Function (t', _, _) when is_thunk_type t' -> [Types.unit_type] (\* THIS IS A HACK. TODO: Trace down cause of bug. At some point during the compilation process the formal parameter to (() {Op: a -> b} -> c) -> d gets unwrapped yielding a function type composed internally as *)
(*  Function ((Function (), {Op: a -> b}, c) *)
(*            , <empty effects>, d) *)
(*   which is wrong; the formal parameter should be wrapped inside a Record. *)
(* *\) (\*error ("arg_types: " ^ (string_of_datatype t') ^ ", ret: " ^ string_of_datatype t'')*\) *)
  | (t, _) ->
     error ("Attempt to take arg types of non-function: " ^ string_of_datatype t)

let rec effect_row ?(overstep_quantifiers=true) t = match (concrete_type t, overstep_quantifiers)  with
  | (ForAll (_, t), true) -> effect_row t
  | (Function (_, effects, _), _) -> effects
  | (Lolli (_, effects, _), _) -> effects
  | (t, _) ->
      error ("Attempt to take effects of non-function: " ^ string_of_datatype t)


let iter_row (iter_func : string -> field_spec -> unit) row  =
  let (field_spec_map, _, _) = fst (unwrap_row row) |> extract_row_parts in
  Utility.StringMap.iter iter_func field_spec_map

let is_function_type t = match concrete_type t with
  | Lolli (_, _, _)
  | Function (_, _, _) -> true
  | _ -> false

let is_thunk_type t =
  is_function_type t && arg_types t = []

let is_builtin_effect = function
  | "wild" | "hear" -> true
  | _ -> false

let rec element_type ?(overstep_quantifiers=true) t = match (concrete_type t, overstep_quantifiers) with
  | (ForAll (_, t), true) -> element_type t
  | Application (l, [t]), _
      when Types.Abstype.equal l Types.list -> t
  | (t, _) ->
      error ("Attempt to take element type of non-list: " ^ string_of_datatype t)

let rec table_read_type t = match concrete_type t with
  | ForAll (_, t) -> table_read_type t
  | Table (r, _, _) -> r
  | t ->
      error ("Attempt to take read type of non-table: " ^ string_of_datatype t)

let rec table_write_type t = match concrete_type t with
  | ForAll (_, t) -> table_write_type t
  | Table (_, w, _) -> w
  | t ->
      error ("Attempt to take write type of non-table: " ^ string_of_datatype t)

let rec table_needed_type t = match concrete_type t with
  | ForAll (_, t) -> table_needed_type t
  | Table (_, _, n) -> n
  | t ->
      error ("Attempt to take needed type of non-table: " ^ string_of_datatype t)

let inject_type name t =
  Variant (make_singleton_open_row (name, Present t) (lin_any, res_any))

let abs_type _ = assert false
let app_type _ _ = assert false

let quantifiers t = match concrete_type t with
  | ForAll (qs, _) -> qs
  | _ -> []


(* Given a type, return its list of toplevel quantifiers and the remaining non-quantified type.
   This merges adjacent ForAlls *)
let split_quantified_type qt = match concrete_type qt with
  | ForAll (qs, t) -> (qs, t)
  | t -> ([], t)

let record_without t names =
  match concrete_type t with
    | Record (Row (fields, row_var, dual) as row) ->
        if is_closed_row row then
          let fieldm = StringSet.fold (fun name fields -> StringMap.remove name fields) names fields
          in
          Record
            (Row
               (fieldm,
                row_var,
                dual))
        else
          let fieldm =
            StringMap.mapi
              (fun name f ->
                if StringSet.mem name names then
                  Absent
                else
                  f)
              fields
          in
          Record
            (Row
               (fieldm,
                row_var,
                dual))
    | _ -> assert false



let rec primary_kind_of_type t =
  match concrete_type t with
  | Not_typed ->
     failwith "Not_typed has no kind"
  | Var (_, kind, _) ->
     Kind.primary_kind kind
  | Recursive r ->
     failwith "Top-level Recursive should have been removed by concrete_type call"
  | Meta p ->
     primary_kind_of_type (Unionfind.find p)
  | Alias (_, d) ->
     primary_kind_of_type d
  | Primitive _
  | Function _
  | Lolli _
  | Record _
  | Variant _
  | Table _
  | ForAll _
  | Application _
  | RecursiveApplication _
  | Input _
  | Output _
  | Select _
  | Choice _
  | Dual _
  | Lens _
  | End ->
     pk_type
  | Present _
  | Absent ->
     pk_presence
  | Closed
  | Effect _
  | Row _ ->
     pk_row



(**
    Determines the primary kind of a type and checks the well-formedness
    of the type in the process.
    Note: This doesn't catch errors related to ill-formed session types.
*)
let check_type_wellformdness t : unit =
  let check_kind expected actual =
    if actual = expected then
      actual
    else
      raise tag_expectation_mismatch
  in
  let rec typ rec_env t =
    check_kind
      pk_type
      (main rec_env t)
  and row rec_env r =
    check_kind
      pk_row
      (main rec_env r)
  and field_spec rec_env fs =
    check_kind
      pk_presence
      (main rec_env fs)

  and meta rec_env ~allow_row_var p =
    (* row variables must be specifically allowed, because currently
       we only allow Row (_, _, \rho) but not standalone \rho *)
    match Unionfind.find p, allow_row_var with
    | Var (_, kind, _), false  when Kind.primary_kind kind = pk_row ->
       (* freestanding row variables not implemented yet (must be inside Row) *)
       raise tag_expectation_mismatch
    | Var (_, kind, _), _ -> Kind.primary_kind kind
    | Recursive (var, var_kind, body), _ ->
       let pk =
         if IntMap.mem var rec_env then
           IntMap.find var rec_env
         else
           let rec_env' =
             IntMap.add var (Kind.primary_kind var_kind) rec_env
           in
           main rec_env' body
       in
       if pk = pk_presence then
         (* recursive types of kind presence are not allowed right now *)
         raise tag_expectation_mismatch
       else
         pk
    | body, _ -> main rec_env body

  and compare_kinds rec_env k t =
    ignore (check_kind (Kind.primary_kind k) (main rec_env t))
  and main rec_env =
    let ityp t = ignore (typ rec_env t) in
    let irow r = ignore (row rec_env r) in
    let ifs fs = ignore (field_spec rec_env fs) in
    function
    | Not_typed ->
       (* Not_typed has no kind *)
       raise tag_expectation_mismatch
    | (Var _ | Recursive _) ->
       (* freestanding Var / Recursive not implemented yet (must be inside Meta) *)
       raise tag_expectation_mismatch
    | Primitive _  -> pk_type
    | Function (f, m, t)
    | Lolli (f, m, t) ->
       ityp f; irow m; ityp t;
       pk_type
    | Record row
    | Variant row ->
       irow row;
       pk_type
    | Effect row ->
       irow row;
       pk_row
    | Table (f, d, r) ->
       ityp f; ityp d; ityp r;
       pk_type
    | ForAll (qs, t) ->
       ityp t;
       pk_type
    | Meta p ->
       meta rec_env ~allow_row_var:false p
    | Alias ((name, qs, ts), d) ->
       List.iter2 (compare_kinds rec_env) qs ts;
       main rec_env d
    | Application (abs_type, args) ->
       List.iter2 (compare_kinds rec_env) (Types.Abstype.arity abs_type) args;
       pk_type
    | RecursiveApplication app ->
       List.iter2 (compare_kinds rec_env) app.r_quantifiers app.r_args;
       pk_type
      (* session stuff. would have to check sub-kinds for full well-formedness check *)
    | Input (t, s)
    | Output (t, s) ->
       ityp t; ityp s;
       pk_type
    | Select fields
    | Choice fields ->
       irow fields;
       pk_type
    | Dual s -> main rec_env s
    | Lens s ->
       (* todo?*)
       pk_type
    | End -> pk_type

    (* presence stuff*)
    | Present t ->
       ityp t;
       pk_presence
    | Absent -> pk_presence

    (* rows *)
    | Closed ->
       (* freestanding Closed not implemented yet (must be inside Row) *)
       raise tag_expectation_mismatch
    | Row (field_spec_map, row_var, dual) ->
       let handle_fs _label f =
         ifs f
       in
       StringMap.iter handle_fs field_spec_map;
       (* must deal with special constructs allowed only in row_var positon:
          - Closed is allowed in row_var position
          - Var with row variable allowed in row_var *)
       begin
       match Unionfind.find row_var with
         | Meta p -> meta rec_env ~allow_row_var:true p
         | Closed -> pk_row
         | body -> row rec_env body
       end

  in
  ignore (main IntMap.empty t)

let row_present_types t =
  extract_row t
   |> extract_row_parts
   |> fst3
   |> StringMap.filter_map
      (fun _ v ->
        match v with
          | Present t -> Some t
          | _ -> None)
