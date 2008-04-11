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
              | `Recursive (_, t) ->
                  ct rec_names t
              | _ -> t
          end
      | _ -> t
  in
    ct (StringSet.empty) t

let split_row name row =
  let (field_env, row_var) = fst (unwrap_row row) in
  let t =
    if StringMap.mem name field_env then
      match (StringMap.find name field_env) with
        | `Present t -> t
        | `Absent -> 
            error ("Attempt to split row "^string_of_row row ^" on absent field" ^ name)
    else
      error ("Attempt to split row "^string_of_row row ^" on absent field" ^ name)
  in
    t, (StringMap.remove name field_env, row_var)

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
    
let rec erase_type name t = match concrete_type t with
  | `ForAll (_, t) -> erase_type name t
  | `Record row ->
      let t, row = split_row name row in
        `Record row
  | t -> error ("Attempt to erase field from non-record type "^string_of_datatype t)

let rec return_type t = match concrete_type t with
  | `ForAll (_, t) -> return_type t
  | `Function (_, _, t) -> t
  | t -> 
      error ("Attempt to take return type of non-function: " ^ string_of_datatype t)

let rec arg_types t = match concrete_type t with
  | `ForAll (_, t) -> arg_types t
  | `Function (`Record row, _, _) ->
      extract_tuple row
  | t ->
      error ("Attempt to take arg types of non-function: " ^ string_of_datatype t)

let rec element_type t = match concrete_type t with
  | `ForAll (_, t) -> element_type t
  | `Application (l, [t])
      when Abstype.Eq_t.eq l Types.list -> t
  | t ->
      error ("Attempt to take element type of non-list: " ^ string_of_datatype t)

let inject_type name t =
  `Variant (make_singleton_open_row (name, `Present t))

let abs_type _ = assert false
let app_type _ _ = assert false

let rec expand_abstract_types context recvars t = 
  let find_abstype (abstype : Abstype.t) : tycon_spec option =
    try Some (snd (List.find ((=) 0 -<- Abstype.compare abstype -<- fst) context))
    with NotFound _ -> None in
  let expand  t = expand_abstract_types context recvars t 
  and expandr r = expand_abstract_types_in_row context recvars r in
    match t with
      | `Not_typed
      | `Primitive _ as t -> t
      | `Function (t1, t2, t3) -> `Function (expand t1, expand t2, expand t3)
      | `Record r -> `Record (expandr r)
      | `Variant r -> `Variant (expandr r)
      | `Table (t1, t2) -> `Table (expand t1, expand t2)
      | `Alias ((tc, ts), t) -> `Alias ((tc, List.map expand ts), expand t)
      | `Application (abstype, ts) ->
          let ts = List.map expand ts in
          begin match find_abstype abstype with
            | Some (`Abstract abstype') -> `Application (abstype', ts)
            | Some (`Alias (vars, body))  -> 
                let tenv = List.fold_right2 IntMap.add vars ts IntMap.empty in
                  `Alias ((Abstype.name abstype, ts),
                          Instantiate.datatype
                            (tenv, IntMap.empty) 
                            (expand (freshen_mailboxes body)))
            | None -> `Application (abstype, ts)
          end
      | `MetaTypeVar mt as m ->
          begin
            match Unionfind.find mt with
              | `Flexible _
              | `Rigid _ as v -> m
              | `Recursive (var, body) as r ->
                  if TypeVarSet.mem var recvars then
                    m
                  else
                    `MetaTypeVar
                      (Unionfind.fresh
                         (`Recursive (var, expand_abstract_types
                                        context
                                        (TypeVarSet.add var recvars)
                                        body)))
              | `Body t -> expand t
          end
      | `ForAll (qs, t) -> `ForAll (qs, expand t)
and expand_abstract_types_in_row context recvars (field_env, row_var) =
  let expand  t = expand_abstract_types context recvars t in
  (StringMap.map (fun t ->
                   match t with
                     | `Present t -> `Present (expand t)
                     | `Absent    -> `Absent) field_env,
   expand_abstract_types_in_rowvar context recvars row_var)
and expand_abstract_types_in_rowvar context recvars rv = 
  match Unionfind.find rv with
    | `Closed
    | `Flexible _
    | `Rigid _ -> rv
    | `Recursive (var, row) ->
        if TypeVarSet.mem var recvars then
          rv
        else
          Unionfind.fresh
            (`Recursive (var,
                         expand_abstract_types_in_row context  (TypeVarSet.add var recvars) row))
    | `Body row ->
        Unionfind.fresh
          (`Body (expand_abstract_types_in_row context recvars row))

  
let expand_abstract_types context t = expand_abstract_types context TypeVarSet.empty t
