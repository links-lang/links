open Utility
open Types

module Env = Env.String

(** type destructors *)
exception TypeDestructionError of string

let error t = raise (TypeDestructionError t)


(** remove any top-level meta typevars and aliases from a type
    (perhaps we can use this version of concrete_type everywhere)
*)
let concrete_type t =
  let rec ct rec_names t : datatype =
    match t with
      | `Alias (_, t) -> ct rec_names t
      | `MetaTypeVar point ->
          begin
            match Unionfind.find point with
              | `Body t -> ct rec_names t
              | `Recursive (var, t) ->
                  if IntSet.mem var rec_names then
                    `MetaTypeVar point
                  else
                    ct (IntSet.add var rec_names) t
              | _ -> t
          end
      | `ForAll (qs, t) ->
          begin
            match ct rec_names t with
              | `ForAll (qs', t') ->
                  `ForAll (box_quantifiers (unbox_quantifiers qs @ unbox_quantifiers qs'), t')
              | t ->
                  begin
                    match unbox_quantifiers qs with
                      | [] -> t
                      | _ -> `ForAll (qs, t)
                  end
          end
      | `Dual s -> dual_type s
      | _ -> t
  in
    ct (IntSet.empty) t

let extract_row t = match concrete_type t with
  | `Record row -> row
  | `Variant row -> row
  | t ->
      failwith
        ("Internal error: attempt to extract a row from a datatype that is not a record or a variant: "
         ^ string_of_datatype t)

let split_row name row =
  let (field_env, row_var, dual) = fst (unwrap_row row) in
  let t =
    if StringMap.mem name field_env then
      match (StringMap.find name field_env) with
        | `Present t -> t
        | `Absent ->
            error ("Attempt to split row "^string_of_row row ^" on absent field" ^ name)
        | `Var _ ->
            error ("Attempt to split row "^string_of_row row ^" on var field" ^ name)
    else
      error ("Attempt to split row "^string_of_row row ^" on absent field" ^ name)
  in
    t, (StringMap.remove name field_env, row_var, dual)

let rec variant_at name t = match concrete_type t with
  | `ForAll (_, t) -> variant_at name t
  | `Variant row ->
      let t, _ = split_row name row in t
  | t ->
      error ("Attempt to deconstruct non-variant type "^string_of_datatype t)

let rec split_variant_type name t = match concrete_type t with
  | `ForAll (_, t) -> split_variant_type name t
  | `Variant row ->
      let t, row = split_row name row in
        `Variant (make_singleton_closed_row (name, `Present t)), `Variant row
  | t ->
      error ("Attempt to split non-variant type "^string_of_datatype t)

let rec project_type name t = match concrete_type t with
  | `ForAll (_, t) -> project_type name t
  | `Record row ->
      let t, _ = split_row name row in
        t
  | t ->
      error ("Attempt to project non-record type "^string_of_datatype t)

let rec select_type name t = match concrete_type t with
  | `ForAll (_, t) -> select_type name t
  | `Select row ->
    let t, _ = split_row name row in t
  | t ->
    error ("Attempt to select from non-selection type "^string_of_datatype (concrete_type t))

let rec split_choice_type name t = match concrete_type t with
  | `ForAll (_, t) -> split_choice_type name t
  | `Choice row ->
      let t, row = split_row name row in
        `Choice (make_singleton_closed_row (name, `Present t)), `Choice row
  | t ->
      error ("Attempt to split non-choice type "^string_of_datatype t)

let rec choice_at name t = match concrete_type t with
  | `ForAll (_, t) -> choice_at name t
  | `Choice row ->
      let t, _ = split_row name row in t
  | t ->
      error ("Attempt to deconstruct non-choice type "^string_of_datatype t)


(*
  This returns the type obtained by removing a set of
  fields from a record.
*)
let rec erase_type names t =
  match concrete_type t with
  | `ForAll (_, t) -> erase_type names t
  | `Record row ->
    let closed = is_closed_row row in
      let (field_env, row_var, duality) = fst (unwrap_row row) in
      let field_env =
        StringSet.fold
          (fun name field_env ->
            match StringMap.lookup name field_env with
            | Some (`Present t) ->
              if closed then
                StringMap.remove name field_env
              else
                StringMap.add name `Absent field_env
            | Some `Absent ->
              error ("Attempt to remove absent field "^name^" from row "^string_of_row row)
            | Some (`Var _) ->
              error ("Attempt to remove var field "^name^" from row "^string_of_row row)
            | None ->
              error ("Attempt to remove absent field "^name^" from row "^string_of_row row))
          names
          field_env
      in
        `Record (field_env, row_var, duality)
  | t -> error ("Attempt to erase field from non-record type "^string_of_datatype t)

let rec return_type t = match concrete_type t with
  | `ForAll (_, t) -> return_type t
  | `Function (_, _, t) -> t
  | `Lolli (_, _, t) -> t
  | t ->
      error ("Attempt to take return type of non-function: " ^ string_of_datatype t)

let rec arg_types t = match concrete_type t with
  | `ForAll (_, t) -> arg_types t
  | `Function (`Record row, _, _) ->
      extract_tuple row
  | `Lolli (`Record row, _, _) ->
      extract_tuple row
  | t ->
      error ("Attempt to take arg types of non-function: " ^ string_of_datatype t)

let rec effect_row t = match concrete_type t with
  | `ForAll (_, t) -> effect_row t
  | `Function (_, effects, _) -> effects
  | `Lolli (_, effects, _) -> effects
  | t ->
      error ("Attempt to take effects of non-function: " ^ string_of_datatype t)

let rec element_type t = match concrete_type t with
  | `ForAll (_, t) -> element_type t
  | `Application (l, [`Type t])
      when Types.Abstype.Eq_t.eq l Types.list -> t
  | t ->
      error ("Attempt to take element type of non-list: " ^ string_of_datatype t)

let rec table_read_type t = match concrete_type t with
  | `ForAll (_, t) -> table_read_type t
  | `Table (r, _, _) -> r
  | t ->
      error ("Attempt to take read type of non-table: " ^ string_of_datatype t)

let rec table_write_type t = match concrete_type t with
  | `ForAll (_, t) -> table_write_type t
  | `Table (_, w, _) -> w
  | t ->
      error ("Attempt to take write type of non-table: " ^ string_of_datatype t)

let rec table_needed_type t = match concrete_type t with
  | `ForAll (_, t) -> table_needed_type t
  | `Table (_, _, n) -> n
  | t ->
      error ("Attempt to take needed type of non-table: " ^ string_of_datatype t)

let inject_type name t =
  `Variant (make_singleton_open_row (name, `Present t) (`Any, `Any))

let abs_type _ = assert false
let app_type _ _ = assert false

let quantifiers t = match concrete_type t with
  | `ForAll (qs, _) -> Types.unbox_quantifiers qs
  | _ -> []

let record_without t names =
  match concrete_type t with
    | `Record ((fields, row_var, dual) as row) ->
        if is_closed_row row then
          `Record
            (StringSet.fold (fun name fields -> StringMap.remove name fields) names fields,
             row_var,
             dual)
        else
          `Record
            (StringMap.mapi
               (fun name f ->
                  if StringSet.mem name names then
                    `Absent
                  else
                    f)
               fields,
             row_var,
             dual)
    | _ -> assert false
