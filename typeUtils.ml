open Utility
open Types

module Env = Env.String

(** remove any top-level `MetaTypeVars from a type and
    expand top-level aliases using a user-supplied alias environment.
*)
let expand_aliases ~aenv t =
  let rec ct rec_names aenv t : datatype =
    match t with
      | `MetaTypeVar point ->
          begin
            match Unionfind.find point with
              | `Body t -> ct rec_names aenv t
              | `Recursive (_, t) ->
                  ct rec_names aenv t
              | _ -> t
          end
      | `Application (name, ts) when Env.has aenv name ->
          if Env.Dom.mem name rec_names then
            failwith ("Cannot instantiate unguarded recursive alias" ^ name)
          else
            let t' = lookup_alias (name, ts) aenv in
              begin
                match t' with
                  | `ForAll (_, `Primitive `Abstract)
                  | `Primitive `Abstract -> t
                  | _ ->
                      ct (Env.Dom.add name rec_names) aenv (Instantiate.alias t' ts)
              end
(*
      | `Application (name, ts) ->
          Debug.print ("Unbound alias: " ^ name);
          t
*)
      | _ -> t
  in
    ct (StringSet.empty) aenv t

(** type destructors *)
exception TypeDestructionError of string

let split_row name row =
  let (field_env, row_var) = fst (unwrap_row row) in
  let t =
    if StringMap.mem name field_env then
      match (StringMap.find name field_env) with
        | `Present t -> t
        | `Absent ->
            raise (TypeDestructionError
                     ("Attempt to split row "^string_of_row row ^" on absent field" ^ name))
    else
      raise (TypeDestructionError
               ("Attempt to split row "^string_of_row row ^" on absent field" ^ name))
  in
    t, (StringMap.remove name field_env, row_var)

let rec split_variant_type ?(aenv=Env.empty) name =
  (expand_aliases ~aenv:aenv) ->-
    (function
       | `ForAll (_, t) -> split_variant_type ~aenv:aenv name t
       | `Variant row ->
           let t, row = split_row name row in
             `Variant (make_singleton_closed_row (name, `Present t)), `Variant row
       | t ->
           raise (TypeDestructionError
                    ("Attempt to split non-variant type "^string_of_datatype t)))

let rec project_type ?(aenv=Env.empty) name =
  (expand_aliases ~aenv:aenv) ->-
    (function
       | `ForAll (_, t) -> project_type ~aenv:aenv name t
       | `Record row ->
           let t, _ = split_row name row in
             t
       | t -> 
           raise (TypeDestructionError
                    ("Attempt to project non-record type "^string_of_datatype t)))

let rec erase_type ?(aenv=Env.empty) name =
  (expand_aliases ~aenv:aenv) ->-
    (function
       | `ForAll (_, t) -> erase_type ~aenv:aenv name t
       | `Record row ->
           let t, row = split_row name row in
             `Record row
       | t ->
           raise (TypeDestructionError
                    ("Attempt to erase field from non-record type "^string_of_datatype t)))

let rec return_type ?(aenv=Env.empty) =
  (expand_aliases ~aenv:aenv) ->-
    (function
       | `ForAll (_, t) -> return_type ~aenv:aenv t
       | `Function (_, _, t) -> t
       | t -> 
           raise (TypeDestructionError
                    ("Attempt to take return type of non-function: " ^ string_of_datatype t)))

let rec arg_types ?(aenv=Env.empty) =
  (expand_aliases ~aenv:aenv) ->-
    (function
       | `ForAll (_, t) -> arg_types ~aenv:aenv t
       | `Function (`Record row, _, _) ->
           extract_tuple row
       | t ->
           raise (TypeDestructionError
                    ("Attempt to take arg types of non-function: " ^ string_of_datatype t)))

let rec element_type ?(aenv=Env.empty) =
  (expand_aliases ~aenv:aenv) ->-
    (function
       | `ForAll (_, t) -> element_type ~aenv:aenv t
       | `Application ("List", [t]) -> t
       | t ->
           raise (TypeDestructionError
                    ("Attempt to take element type of non-list: " ^ string_of_datatype t)))

let inject_type ?(aenv=Env.empty) name t =
  `Variant (make_singleton_open_row (name, `Present t))

let abs_type ?(aenv=Env.empty) _ = assert false
let app_type ?(aenv=Env.empty) _ _ = assert false
