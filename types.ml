(*pp deriving *)
open Utility

module FieldEnv = Utility.StringMap
type 'a stringmap = 'a Utility.stringmap
type 'a field_env = 'a stringmap deriving (Eq, Pickle, Typeable, Show, Shelve)

(* type var sets *)
module TypeVarSet = Utility.IntSet

(* points *)
type 'a point = 'a Unionfind.point deriving (Eq, Typeable, Shelve, Show)

(* module Show_point (S : Show.Show) = Show.Show_unprintable(struct type a = S.a point end) *)
module Pickle_point (S : Pickle.Pickle) = Pickle.Pickle_unpicklable(struct type a = S.a point let tname = "point" end)

type primitive = [ `Bool | `Int | `Char | `Float | `XmlItem | `DB | `NativeString ]
    deriving (Eq, Typeable, Show, Pickle, Shelve)

type 't meta_type_var_basis =
    [ `Flexible of int
    | `Rigid of int
    | `Recursive of (int * 't)
    | `Body of 't ]
      deriving (Eq, Show, Pickle, Typeable, Shelve)

type 'r meta_row_var_basis =
    [ 'r meta_type_var_basis | `Closed ]
      deriving (Eq, Show, Pickle, Typeable, Shelve)

type type_variable =
    [ `TypeVar of int | `RigidTypeVar of int
    | `RowVar of int | `RigidRowVar of int ]
      deriving (Eq, Typeable, Show, Pickle, Shelve)

type quantifier = type_variable
    deriving (Eq, Typeable, Show, Pickle, Shelve)

module Abstype =
struct
  type t = { id    : string ;
             name  : string ;
             arity : int }
      deriving (Eq, Show, Pickle, Typeable, Shelve)
  let make name arity = 
    let id = Utility.gensym ~prefix:"abstype:" () in
      { id    = id ;
        name  = name ;
        arity = arity }
  let arity { arity = arity } = arity
  let name  { name  = name  } = name
  let compare l r = String.compare l.id r.id
end

let mailbox  = {
  Abstype.id = "Mailbox" ;
  name       = "Mailbox" ;
  arity      = 1 ;
}
let list     = {
  Abstype.id = "List" ;
  name       = "List" ;
  arity      = 1 ;
}

let event    = {
  Abstype.id = "Event" ;
  name       = "Event" ;
  arity      = 0 ;
}
let dom_node = {
  Abstype.id = "DomNode" ;
  name       = "DomNode" ;
  arity      = 0 ;
}

type datatype =
    [ `Not_typed
    | `Primitive of primitive
    | `Function of (datatype * datatype * datatype)
    | `Record of row
    | `Variant of row
    | `Table of datatype * datatype
    | `Alias of ((string * datatype list) * datatype)
    | `Application of (Abstype.t * datatype list)
    | `MetaTypeVar of meta_type_var 
    | `ForAll of (quantifier list * datatype)]
and field_spec     = [ `Present of datatype | `Absent ]
and field_spec_map = field_spec field_env
and row_var        = meta_row_var
and row            = field_spec_map * row_var
and meta_type_var  = (datatype meta_type_var_basis) point
and meta_row_var   = (row meta_row_var_basis) point
    deriving (Eq, Show, Pickle, Typeable, Shelve)

(* useful for debugging: types tend to be too big to read *)
(*
module Show_datatype = Show_unprintable (struct type a = datatype end)
module Show_field_spec = Show_unprintable (struct type a = field_spec end)
module Show_field_spec_map = Show_unprintable (struct type a = field_spec_map end)
module Show_row_var = Show_unprintable (struct type a = row_var end)
module Show_row = Show_unprintable (struct type a = row end)
module Show_meta_type_var = Show_unprintable (struct type a = meta_type_var end)
module Show_meta_row_var = Show_unprintable (struct type a = meta_row_var end)
*)

type type_arg = 
    [ `Type of datatype | `Row of row ]
      deriving (Eq, Typeable, Show, Pickle, Shelve)

let for_all : quantifier list * datatype -> datatype = function
  | [], t -> t
  | qs, t ->
      let qs = List.map
        (function
           | `TypeVar x | `RigidTypeVar x -> `RigidTypeVar x
           | `RowVar x | `RigidRowVar x -> `RigidRowVar x) qs
      in
        match t with
          | `ForAll (qs', t) -> `ForAll (qs @ qs', t)
          | _ -> `ForAll (qs, t)

let type_var_number = function
  | `TypeVar x
  | `RigidTypeVar x
  | `RowVar x
  | `RigidRowVar x -> x

module Env = Env.String

type tycon_spec = [`Alias of int list * datatype | `Abstract of Abstype.t]
    deriving (Show)

type environment        = datatype Env.t
 and tycon_environment  = tycon_spec Env.t
 and typing_environment = { var_env   : environment ;
                            tycon_env : tycon_environment }
    deriving (Show)

(* Functions on environments *)
let extend_typing_environment 
    {var_env = l ; tycon_env = al }
    {var_env = r ; tycon_env = ar} : typing_environment = 
  {var_env = Env.extend l r ; tycon_env = Env.extend al ar }

(* Generation of fresh type variables *)
let type_variable_counter = ref 0

let fresh_raw_variable : unit -> int =
  function () -> 
    incr type_variable_counter; !type_variable_counter

let bump_variable_counter i = type_variable_counter := !type_variable_counter+i

(* Caveat: Map.fold behaves differently between Ocaml 3.08.3 and 3.08.4

let map_fold_increasing = ocaml_version_atleast [3; 8; 4]
*)
(*
  NOTE:
  
  We use Map.fold and Set.fold too often to support OCaml versions prior to 3.08.4
*)
let _ =
  if not (ocaml_version_atleast [3; 8; 4]) then
    failwith ("Links requires OCaml version 3.08.4 or later")
  else
    ()

(* type ops stuff *)
  let empty_field_env = FieldEnv.empty
  let closed_row_var = Unionfind.fresh `Closed

  let make_type_variable var = `MetaTypeVar (Unionfind.fresh (`Flexible var))
  let make_rigid_type_variable var = `MetaTypeVar (Unionfind.fresh (`Rigid var))
  let make_row_variable var = Unionfind.fresh (`Flexible var)
  let make_rigid_row_variable var = Unionfind.fresh (`Rigid var)

  let is_closed_row =
    let rec is_closed rec_vars =
      function
        | (_, row_var) ->
            begin
              match Unionfind.find row_var with
                | `Closed -> true
                | `Rigid _
                | `Flexible _ -> false
                | `Recursive (var, row) ->
                    ((TypeVarSet.mem var rec_vars)
                     or (is_closed (TypeVarSet.add var rec_vars) row))
                | `Body row ->
                    is_closed rec_vars row
            end
    in
      is_closed TypeVarSet.empty

  let get_row_var : row -> int option = fun (_, row_var) ->
    let rec get_row_var' = fun rec_vars -> function
      | `Closed -> None
      | `Flexible var
      | `Rigid var -> Some var
      | `Recursive (var, (_, row_var')) ->
          if TypeVarSet.mem var rec_vars then
            None
          else
            get_row_var' (TypeVarSet.add var rec_vars) (Unionfind.find row_var')
      | `Body (_, row_var') ->
          get_row_var' rec_vars (Unionfind.find row_var')
    in
      get_row_var' TypeVarSet.empty (Unionfind.find row_var)

  let fresh_type_variable = make_type_variable -<- fresh_raw_variable
  let fresh_rigid_type_variable = make_rigid_type_variable -<- fresh_raw_variable
  let fresh_row_variable = make_row_variable -<- fresh_raw_variable
  let fresh_rigid_row_variable = make_rigid_row_variable -<- fresh_raw_variable

  let fresh_type_quantifier () =
    let var = fresh_raw_variable () in
      `RigidTypeVar var, make_rigid_type_variable var
        
  let fresh_row_quantifier () =
    let var = fresh_raw_variable () in
      `RigidRowVar var, make_rigid_row_variable var
        
  let fresh_flexible_type_quantifier () =
    let var = fresh_raw_variable () in
      `TypeVar var, make_type_variable var
        
  let fresh_flexible_row_quantifier () =
    let var = fresh_raw_variable () in
      `RowVar var, make_row_variable var      

  let make_empty_closed_row () = empty_field_env, closed_row_var
  let make_empty_open_row () = empty_field_env, fresh_row_variable ()

  let make_singleton_closed_row (label, field_spec) =
    FieldEnv.add label field_spec empty_field_env, closed_row_var
  let make_singleton_open_row (label, field_spec) =
    FieldEnv.add label field_spec empty_field_env, fresh_row_variable ()

  let is_absent_from_row label (field_env, _ as row) =
    if FieldEnv.mem label field_env then
      FieldEnv.find label field_env = `Absent
    else
      is_closed_row row

  let row_with (label, f : string * field_spec) (field_env, row_var : field_spec_map * row_var) =
    FieldEnv.add label f field_env, row_var

(*** end of type_basis ***)

(** remove any top-level `MetaTypeVars from a type. *)
let concrete_type t =
  let rec ct rec_names t : datatype =
    match t with
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

let free_type_vars, free_row_type_vars =
  let module S = TypeVarSet in
  let rec free_type_vars' : S.t -> datatype -> S.t = fun rec_vars ->
    function
      | `Not_typed               -> S.empty
      | `Primitive _             -> S.empty
      | `Function (f, m, t)      ->
          S.union_all [free_type_vars' rec_vars f; free_type_vars' rec_vars m; free_type_vars' rec_vars t]
      | `Record row
      | `Variant row             -> free_row_type_vars' rec_vars row
      | `Table (r, w)            -> S.union (free_type_vars' rec_vars r) (free_type_vars' rec_vars w)
      | `Alias ((_, ts), datatype) ->
          S.union (S.union_all (List.map (free_type_vars' rec_vars) ts)) (free_type_vars' rec_vars datatype)
      | `Application (_, datatypes) -> S.union_all (List.map (free_type_vars' rec_vars) datatypes)
      | `ForAll (tvars, body)    -> S.diff (free_type_vars' rec_vars body) 
                                           (List.fold_right (S.add -<- type_var_number) tvars S.empty)
      | `MetaTypeVar point       ->
          begin
            match Unionfind.find point with
              | `Flexible var
              | `Rigid var -> S.singleton(var)
              | `Recursive (var, body) ->
                  if S.mem var rec_vars then
                    S.empty
                  else
                    free_type_vars' (S.add var rec_vars) body
              | `Body t ->
                  free_type_vars' rec_vars t
          end
  and free_row_type_vars' : S.t -> row -> S.t = 
    fun rec_vars (field_env, row_var) ->
      let field_vars =
        FieldEnv.fold (fun _ t field_vars ->
                         match t with
                           | `Present t ->
                               S.union field_vars (free_type_vars' rec_vars t)
                           | `Absent ->
                               field_vars) field_env S.empty in
      let row_vars =
        match Unionfind.find row_var with
          | `Flexible var
          | `Rigid var -> S.singleton(var)
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
  in
    ((fun t -> free_type_vars' S.empty t),
     (fun t -> free_row_type_vars' S.empty t))

type inference_type_map =
    ((datatype Unionfind.point) IntMap.t ref *
       (row Unionfind.point) IntMap.t ref)

let field_env_union : (field_spec_map * field_spec_map) -> field_spec_map =
  fun (env1, env2) ->
    FieldEnv.fold (fun label field_spec env' ->
                     FieldEnv.add label field_spec env') env1 env2

let contains_present_fields field_env =
  FieldEnv.fold
    (fun _ field_spec present ->
       match field_spec with
         | `Present _ -> true
         | `Absent -> present
    ) field_env false

let is_canonical_row_var row_var =
  match Unionfind.find row_var with
    | `Closed
    | `Flexible _
    | `Rigid _ -> true
    | `Recursive _
    | `Body _ -> false

let is_rigid_row : row -> bool =
  let rec is_rigid rec_vars (_, row_var) =
    match Unionfind.find row_var with
      | `Closed
      | `Rigid _ -> true
      | `Flexible _ -> false
      | `Recursive (var, row) ->
          ((TypeVarSet.mem var rec_vars) or (is_rigid (TypeVarSet.add var rec_vars) row))
      | `Body row ->
          is_rigid rec_vars row
  in
    is_rigid TypeVarSet.empty

(* is_rigid_row_with_var var row
   returns true if row is rigid and has var as its row var
*)
let is_rigid_row_with_var : int -> row -> bool =
  fun var ->
    let rec is_rigid rec_vars (_, row_var) =
      match Unionfind.find row_var with
        | `Closed
        | `Flexible _ -> false
        | `Rigid var' -> var=var'
        | `Recursive (var', row) ->
            ((TypeVarSet.mem var' rec_vars) or (is_rigid (TypeVarSet.add var' rec_vars) row))
        | `Body row ->
            is_rigid rec_vars row
    in
      is_rigid TypeVarSet.empty


let is_flattened_row : row -> bool =
  let rec is_flattened =
    fun rec_vars (_, row_var) ->
      match Unionfind.find row_var with
        | `Closed
        | `Flexible _
        | `Rigid _ -> true
        | `Body _ -> false
        | `Recursive (var, rec_row) ->
            if TypeVarSet.mem var rec_vars then true
            else is_flattened (TypeVarSet.add var rec_vars) rec_row
  in
    is_flattened TypeVarSet.empty

let is_empty_row : row -> bool =
  let rec is_empty = fun rec_vars -> fun (field_env, row_var) ->
    FieldEnv.is_empty field_env &&
      begin
        match Unionfind.find row_var with
          | `Closed
          | `Rigid _
          | `Flexible _ -> true
          | `Recursive (var, _) when TypeVarSet.mem var rec_vars -> true
          | `Recursive (var, rec_row) -> is_empty (TypeVarSet.add var rec_vars) rec_row
          | `Body row -> is_empty rec_vars row
      end
  in
    is_empty TypeVarSet.empty

(* 
 convert a row to the form (field_env, row_var)
 where Unionfind.find row_var is of the form:
    `Closed
  | `Rigid var
  | `Flexible var
  | `Recursive (var, body)
 *)
let flatten_row : row -> row =
  let rec flatten_row' : meta_row_var IntMap.t -> row -> row =
    fun rec_env ((field_env, row_var) as row) ->
      let row' =
        match Unionfind.find row_var with
          | `Closed
          | `Flexible _
          | `Rigid _ -> row
          | `Recursive (var, rec_row) ->
              if IntMap.mem var rec_env then
                row
              else
                (let row_var' =
                   Unionfind.fresh (`Recursive (var, (FieldEnv.empty,
                                                      Unionfind.fresh (`Flexible var)))) in
                 let rec_row' = flatten_row' (IntMap.add var row_var' rec_env) rec_row in
                   Unionfind.change row_var' (`Recursive (var, rec_row'));
                    field_env, row_var')                 
          | `Body row' ->
              let field_env', row_var' = flatten_row' rec_env row' in
                field_env_union (field_env, field_env'), row_var'
      in
        assert (is_flattened_row row');
        row'
  in
    flatten_row' IntMap.empty


(*
 As flatten_row except if the flattened row_var is of the form:

  `Recursive (var, body)

then it is unwrapped. This ensures that all the fields are exposed
in field_env.
 *)
let unwrap_row : row -> (row * row_var option) =
  let rec unwrap_row' : meta_row_var IntMap.t -> row -> (row * row_var option) =
    fun rec_env ((field_env, row_var) as row) ->
      let row' =
        match Unionfind.find row_var with
          | `Closed
          | `Flexible _
          | `Rigid _ -> row, None
          | `Recursive (var, body) ->
              if IntMap.mem var rec_env then
                row, Some row_var
              else
                begin
                  let point =
                    Unionfind.fresh (`Recursive (var, body)) in
                  let unwrapped_body, _ = unwrap_row' (IntMap.add var point rec_env) body in
                    Unionfind.change point (`Recursive (var, unwrapped_body));
                    let field_env', row_var' = unwrapped_body in
                      (field_env_union (field_env, field_env'), row_var'), Some point
                end
          | `Body row' ->
              let (field_env', row_var'), rec_row = unwrap_row' rec_env row' in
                (field_env_union (field_env, field_env'), row_var'), rec_row
      in
        assert (is_flattened_row (fst row'));
        row'
  in
    unwrap_row' IntMap.empty

(* useful types *)
let unit_type = `Record (make_empty_closed_row ())
let string_type = `Alias (("String", []), (`Application (list, [`Primitive `Char])))
let char_type = `Primitive `Char
let bool_type = `Primitive `Bool
let int_type = `Primitive `Int
let float_type = `Primitive `Float
let xml_type = `Alias (("Xml", []), `Application (list, [`Primitive `XmlItem]))
let database_type = `Primitive `DB
let native_string_type = `Primitive `NativeString

(*
let empty_var_maps : unit -> inference_type_map =
  fun () ->
    let type_var_map : (datatype Unionfind.point) IntMap.t ref = ref IntMap.empty in
    let row_var_map : (row Unionfind.point) IntMap.t ref = ref IntMap.empty in
      (type_var_map, row_var_map)
*)  

(* skeleton for performing a fold over (inference) datatypes *)
let rec datatype_skeleton :  TypeVarSet.t -> datatype -> datatype = fun rec_vars ->
  function
    | `Not_typed -> `Not_typed
    | `Primitive p -> `Primitive p
    | `Function (f, m, t) ->
        `Function (datatype_skeleton rec_vars f, datatype_skeleton rec_vars m, datatype_skeleton rec_vars t)
    | `Record row -> `Record (row_skeleton rec_vars row)
    | `Variant row -> `Variant (row_skeleton rec_vars row)
    | `Table (r, w) -> `Table (datatype_skeleton rec_vars r, datatype_skeleton rec_vars w)
    | `Alias _ -> assert false
    | `Application (s, ts) -> `Application (s, List.map (datatype_skeleton rec_vars) ts)
    | `ForAll _ -> assert false
    | `MetaTypeVar point ->
        `MetaTypeVar
          (Unionfind.fresh
             (match Unionfind.find point with
                | `Flexible var -> `Flexible var
                | `Rigid var -> `Rigid var
                | `Recursive (var, t) ->
                    if TypeVarSet.mem var rec_vars then
                      `Recursive (var, t)
                    else
                      `Recursive (var, datatype_skeleton (TypeVarSet.add var rec_vars) t)
                | `Body t -> `Body (datatype_skeleton rec_vars t)))
and field_spec_skeleton = fun rec_vars ->
  function
    | `Present t -> `Present (datatype_skeleton rec_vars t)
    | `Absent -> `Absent
and field_spec_map_skeleton = fun rec_vars field_env ->
  FieldEnv.map (field_spec_skeleton rec_vars) field_env
and row_skeleton = fun rec_vars row ->
  let field_env, row_var = row in (*flatten_row row in*)
  let field_env' = field_spec_map_skeleton rec_vars field_env in
  let row_var' =
    Unionfind.fresh
      (match Unionfind.find row_var with
         | `Closed -> `Closed
         | `Flexible var -> `Flexible var
         | `Rigid var -> `Rigid var
         | `Recursive (var, rec_row) ->
             if TypeVarSet.mem var rec_vars then
               `Recursive (var, rec_row)
             else
               `Recursive (var, row_skeleton (TypeVarSet.add var rec_vars) rec_row)
         | `Body row ->
             `Body (row_skeleton rec_vars row))
  in
    field_env', row_var'

let rec is_mailbox_free rec_vars (t : datatype) =
  let imb = is_mailbox_free rec_vars in
  let imbr = is_mailbox_free_row rec_vars in
    match t with
      | `Not_typed -> true
      | `Primitive _ -> true
      | `Function (f, m, t) -> imb f && imb m && imb t
      | `Record row
      | `Variant row -> imbr row
      | `Table (r, w) -> imb r && imb w
      | `Alias ((_, ts), dt) -> List.for_all imb ts && imb dt
      | `Application (t, _) when t.Abstype.id = mailbox.Abstype.id -> false
      | `Application (_, ts) -> List.for_all imb ts
      | `ForAll (_, _) -> assert false
      | `MetaTypeVar point ->
          begin
            match Unionfind.find point with
              | `Flexible _
              | `Rigid _ -> true
              | `Recursive (var, t) ->
                  (TypeVarSet.mem var rec_vars) ||
                    (is_mailbox_free (TypeVarSet.add var rec_vars) t)
              | `Body t -> imb t
          end
and is_mailbox_free_field_spec rec_vars =
  function
    | `Present t -> is_mailbox_free rec_vars t
    | `Absent -> true
and is_mailbox_free_field_spec_map rec_vars field_env =
  FieldEnv.fold (fun _ field_spec b ->
                   b && is_mailbox_free_field_spec rec_vars field_spec)
    field_env true
and is_mailbox_free_row rec_vars row =
  let field_env, row_var = row
  in
    (is_mailbox_free_field_spec_map rec_vars field_env) &&
      (match Unionfind.find row_var with
         | `Closed
         | `Flexible _
         | `Rigid _ -> true
         | `Recursive (var, rec_row) ->
             (TypeVarSet.mem var rec_vars) ||
               (is_mailbox_free_row (TypeVarSet.add var rec_vars) rec_row)
         | `Body row ->
             is_mailbox_free_row rec_vars row)


(* interface *)
let is_mailbox_free : datatype -> bool = is_mailbox_free TypeVarSet.empty
let is_mailbox_free_row = is_mailbox_free_row TypeVarSet.empty

(* precondition: the row is unwrapped *)
let is_tuple ?(allow_onetuples=false) (field_env, rowvar) =
  match Unionfind.find rowvar with
    | `Closed ->
        let n = StringMap.size field_env in
        let b =
          n = 0
          ||
          (List.for_all
             (fun i ->
                let name = string_of_int i in
                  FieldEnv.mem name field_env
                  && (match FieldEnv.find (string_of_int i) field_env with
                        | `Present _ -> true
                        | `Absent -> false))
             (fromTo 1 n))
        in
          (* 0/1-tuples are displayed as records *)
          b && (allow_onetuples || n <> 1)
    | _ -> false

let extract_tuple (field_env, _) =
  FieldEnv.to_list (fun _ ->
                      function
                        | `Present t -> t
                        | `Absent -> assert false) field_env
    
(* whether to display mailbox annotations on arrow types
   [NOTE]
      unused mailbox parameters are never shown
 *)
let show_mailbox_annotations = Settings.add_bool("show_mailbox_annotations", true, `User)

(* pretty-print type vars as raw numbers rather than letters *)
let show_raw_type_vars = Settings.add_bool("show_raw_type_vars", false, `User)

(* Type printers *)

exception Not_tuple

let string_of_primitive : primitive -> string = function
  | `Bool -> "Bool"  | `Int -> "Int"  | `Char -> "Char"  | `Float   -> "Float"  
  | `XmlItem -> "XmlItem" | `DB -> "Database" | `NativeString -> "NativeString"

let string_of_quantifier' : string IntMap.t -> quantifier -> string =
  fun vars ->
    function
      | `TypeVar var
      | `RowVar var -> "'" ^ IntMap.find var vars
      | `RigidTypeVar var
      | `RigidRowVar var -> IntMap.find var vars

let rec string_of_datatype' : TypeVarSet.t -> string IntMap.t -> datatype -> string =
  fun rec_vars vars datatype ->
    let sd = string_of_datatype' rec_vars vars in

    let string_of_mailbox_arrow mailbox_type =
      begin
        if Settings.get_value(show_mailbox_annotations) then
          "-{" ^ sd mailbox_type ^ "}->"
        else
          "->"
      end in

    let unwrap = fst -<- unwrap_row in
    (* precondition: the row is unwrapped *)
    let string_of_tuple (field_env, row_var) =
      let tuple_env =
        FieldEnv.fold
          (fun i t tuple_env ->
             match t with
               | `Present t -> IntMap.add (int_of_string i) t tuple_env
               | `Absent -> assert false) field_env IntMap.empty in
      let ss = List.rev (IntMap.fold (fun _ t ss -> (sd t) :: ss) tuple_env [])
      in
        "(" ^ String.concat ", " ss ^  ")"
    in
      match datatype with
        | `Not_typed       -> "not typed"
        | `Primitive p     -> string_of_primitive p
        | `MetaTypeVar point ->
            begin             
              match Unionfind.find point with
                | `Flexible var -> (* Debug.print ("flexi var: " ^ IntMap.find var vars); *) IntMap.find var vars
                | `Rigid var -> (* Debug.print ("rigid var: " ^ IntMap.find var vars); *) IntMap.find var vars
                | `Recursive (var, body) ->
(*                    Debug.print ("rec var: " ^ IntMap.find var vars);*)
                    if TypeVarSet.mem var rec_vars then
                      IntMap.find var vars
                    else
                      "mu " ^ IntMap.find var vars ^ " . " ^
                        string_of_datatype' (TypeVarSet.add var rec_vars) vars body
                | `Body t -> (* Debug.print ("body"); *) sd t
            end
        | `Function (args, mailbox_type, t) ->
            let arrow =
              match concrete_type mailbox_type with
                | `Application (mb, [t]) when mb.Abstype.id = mailbox.Abstype.id->
                    string_of_mailbox_arrow (t)
                | _ -> "->"
            in begin match concrete_type args with
              | `Record row when is_tuple ~allow_onetuples:true row ->
                  string_of_tuple row ^ " " ^arrow ^ " " ^ sd t
              | t' ->     "*" ^ sd t' ^ " " ^arrow ^ " " ^ sd t
              end
        | `Record row      ->
            let row = unwrap row in
              (if is_tuple row then string_of_tuple row
               else "(" ^ string_of_row' "," rec_vars vars row ^ ")")
        | `Variant row    -> "[|" ^ string_of_row' "|" rec_vars vars row ^ "|]"
        | `ForAll (tvars, body) -> 
            "forall "^ mapstrcat "," (string_of_quantifier' vars) tvars ^"."^ sd body
        | `Table (r, w)   ->
            "TableHandle(" ^
              string_of_datatype' rec_vars vars r ^ "," ^
              string_of_datatype' rec_vars vars w ^ ")"
            (*
              [QUESTION]
              How should we render the types [Char] and [XmlItem]?

              It isn't clear what the right thing to do here is.

              Option 1 - as lists
              Then
              ['a', 'b', 'c] : [Char]
              but
              "abc" ++ "def" : [Char]

              Option 2 - as typenames
              Then
              "abc" ++ "def" : String
              but
              ['a', 'b', 'c] : String

              What do GHCi and SML/NJ Do?
            *) 
            (*
              | `Application ("List", [`Primitive `Char]) -> "String"
              | `Application ("List", [`Primitive `XmlItem]) -> "Xml"
            *)

(*        | `Alias ((s,[]), t) ->  "{"^s^"}"^ sd t*)
        | `Alias ((s,[]), t) ->  s
        | `Alias ((s,ts), _) ->  s ^ " ("^ String.concat "," (List.map sd ts) ^")"
        | `Application (l, [elems]) when Abstype.Eq_t.eq l list ->  "["^ sd elems ^"]"
        | `Application (s, []) -> Abstype.name s
        | `Application (s, ts) -> Abstype.name s ^ " ("^ String.concat "," (List.map sd ts) ^")"

and string_of_row' sep rec_vars vars (field_env, row_var) =
  let show_absent =
    if is_closed_row (field_env, row_var) then
      (fun _ x -> x) (* don't show absent fields in closed rows *)
    else
      (fun label (present_strings, absent_strings) -> present_strings, (label ^ "- ") :: absent_strings) in
   
  let present_strings, absent_strings =
    FieldEnv.fold (fun label t (present_strings, absent_strings) ->
                     match t with
                       | `Present t ->
                           (label ^ ":" ^ string_of_datatype' rec_vars vars t) :: present_strings, absent_strings
                       | `Absent ->
                           show_absent label (present_strings, absent_strings)) field_env ([], []) in

  let row_var_string = string_of_row_var' sep rec_vars vars row_var in
    (String.concat sep (List.rev (present_strings) @ List.rev (absent_strings))) ^
      (match row_var_string with
         | None -> ""
         | Some s -> "|"^s)
and string_of_row_var' sep rec_vars vars row_var =
  match Unionfind.find row_var with
    | `Closed -> None
    | `Flexible var
    | `Rigid var ->
        Some (IntMap.find var vars)
    | `Recursive (var, row) ->
        if TypeVarSet.mem var rec_vars then
          Some (IntMap.find var vars)
        else
          Some ("(mu " ^ IntMap.find var vars ^ " . " ^
                  string_of_row' sep (TypeVarSet.add var rec_vars) vars row ^ ")")
    | `Body row -> Some (string_of_row' sep rec_vars vars row)

let make_names vars =
  if Settings.get_value show_raw_type_vars then
    TypeVarSet.fold (fun var (name_map) -> IntMap.add var (string_of_int var) name_map) vars IntMap.empty
  else
    begin
      let first_letter = int_of_char 'a' in
      let last_letter = int_of_char 'z' in
      let num_letters = last_letter - first_letter + 1 in
        
      let string_of_ascii n = Char.escaped (char_of_int n) in
        
      let rec num_to_letters n =
        let letter = string_of_ascii (first_letter + (n mod num_letters)) in
          letter ^
            (if n >= num_letters then (num_to_letters (n / num_letters))
             else "")
      in
        
      let (_, name_map) = 
        TypeVarSet.fold (fun var (n, name_map) -> (n+1, IntMap.add var (num_to_letters n) name_map)) vars (0, IntMap.empty)
      in
        name_map
    end


(*
  find all the flexible type variables in a type
 *)
let rec flexible_type_vars : TypeVarSet.t -> datatype -> TypeVarSet.t = fun bound_vars t ->
  let ftv = flexible_type_vars bound_vars in
    match t with
      | `Not_typed  
      | `Primitive _ -> TypeVarSet.empty
      | `MetaTypeVar point ->
          begin
            match Unionfind.find point with
              | `Flexible var when TypeVarSet.mem var bound_vars -> TypeVarSet.empty
              | `Flexible var -> TypeVarSet.singleton var
              | `Rigid _ -> TypeVarSet.empty
              | `Recursive (var, body) ->
                  if TypeVarSet.mem var bound_vars then
                    TypeVarSet.empty
                  else
                    flexible_type_vars (TypeVarSet.add var bound_vars) body
              | `Body t -> ftv t
          end
      | `Function (f, m, t) ->
          TypeVarSet.union_all [ftv f; ftv m; ftv t]
      | `Record row -> row_flexible_type_vars bound_vars row
      | `ForAll (tvars, body) ->
          failwith "Not implemented flexible_type_vars for `ForAll yet"
(*`ForAll (tvars, ftv body)*)
      | `Variant row -> row_flexible_type_vars bound_vars row
      | `Table (r, w) -> TypeVarSet.union (ftv r) (ftv w)
      | `Alias ((name, ts), d) -> TypeVarSet.union_all ((ftv d)::(List.map ftv ts))
      | `Application (name, datatypes) -> TypeVarSet.union_all (List.map ftv datatypes)
and row_flexible_type_vars bound_vars (field_env, row_var) =
  TypeVarSet.union
    (FieldEnv.fold (fun _ t ftvs ->
                      match t with
                        | `Present t ->
                            TypeVarSet.union (flexible_type_vars bound_vars t) ftvs
                        | `Absent ->
                            ftvs) field_env TypeVarSet.empty)
    (row_var_flexible_type_vars bound_vars row_var)
and row_var_flexible_type_vars bound_vars row_var = 
  match Unionfind.find row_var with
    | `Closed -> TypeVarSet.empty
    | `Flexible var when TypeVarSet.mem var bound_vars -> TypeVarSet.empty
    | `Flexible var -> TypeVarSet.singleton var
    | `Rigid _ -> TypeVarSet.empty
    | `Recursive (var, row) ->
        if TypeVarSet.mem var bound_vars then
          TypeVarSet.empty
        else
          row_flexible_type_vars (TypeVarSet.add var bound_vars) row
    | `Body row ->
        row_flexible_type_vars bound_vars row

(* find all free and bound type variables in a  *)
(*
  BUG: include_aliases probably shouldn't be an option here - it should always be true
*)
let rec free_bound_type_vars : include_aliases:bool -> TypeVarSet.t -> datatype -> TypeVarSet.t = fun ~include_aliases rec_vars t ->
  let fbtv = free_bound_type_vars ~include_aliases rec_vars in
    match t with
      | `Not_typed               -> TypeVarSet.empty
      | `Primitive _             -> TypeVarSet.empty
      | `MetaTypeVar point ->
          begin
            match Unionfind.find point with
              | `Flexible var
              | `Rigid var -> TypeVarSet.singleton var
              | `Recursive (var, body) ->
                  if TypeVarSet.mem var rec_vars then
                    TypeVarSet.empty
                  else
                    TypeVarSet.add var (free_bound_type_vars ~include_aliases (TypeVarSet.add var rec_vars) body)
              | `Body t -> fbtv t
          end
      | `Function (f, m, t)      ->
          TypeVarSet.union
            (TypeVarSet.union (fbtv f) (fbtv t))
            (fbtv m)
      | `Record row
      | `Variant row -> free_bound_row_type_vars ~include_aliases rec_vars row
      | `Table (r, w) -> TypeVarSet.union (fbtv r) (fbtv w)
      | `ForAll (tvars, body) -> List.fold_right (TypeVarSet.add -<- type_var_number) tvars (fbtv body)
      | `Alias ((_,ts), d) when include_aliases -> TypeVarSet.union_all (fbtv d :: List.map fbtv ts)
      | `Alias (_, d) -> fbtv d
      | `Application (_, datatypes) -> List.fold_right TypeVarSet.union (List.map fbtv datatypes) TypeVarSet.empty
and free_bound_row_type_vars ~include_aliases rec_vars (field_env, row_var) =
  let field_type_vars =
    FieldEnv.fold (fun _ t tvs ->
                     match t with
                       | `Present t ->
                           TypeVarSet.union tvs (free_bound_type_vars ~include_aliases rec_vars t)
                       | `Absent ->
                           tvs) field_env TypeVarSet.empty in
  let row_var = free_bound_row_var_vars ~include_aliases rec_vars row_var in
    TypeVarSet.union field_type_vars row_var  
and free_bound_row_var_vars ~include_aliases rec_vars row_var = 
  match Unionfind.find row_var with
    | `Closed -> TypeVarSet.empty
    | `Flexible var
    | `Rigid var ->
        TypeVarSet.singleton var
    | `Recursive (var, row) ->
        if TypeVarSet.mem var rec_vars then
          TypeVarSet.empty
        else
          TypeVarSet.add var
            (free_bound_row_type_vars ~include_aliases (TypeVarSet.add var rec_vars) row)
    | `Body row -> free_bound_row_type_vars ~include_aliases rec_vars row

let free_bound_type_vars ?(include_aliases=true) = free_bound_type_vars ~include_aliases TypeVarSet.empty
let free_bound_row_type_vars ?(include_aliases=true) = free_bound_row_type_vars ~include_aliases TypeVarSet.empty
let free_bound_row_var_vars ?(include_aliases=true) = free_bound_row_var_vars ~include_aliases TypeVarSet.empty

(* string conversions *)
let string_of_datatype (datatype : datatype) = 
    string_of_datatype' TypeVarSet.empty (make_names (free_bound_type_vars ~include_aliases:true datatype)) datatype

let string_of_datatype_raw datatype = 
  string_of_datatype' TypeVarSet.empty (TypeVarSet.fold
                     (fun var name_map -> IntMap.add var (string_of_int var) name_map)
                     (free_bound_type_vars ~include_aliases:true datatype) IntMap.empty) datatype

let string_of_row row = 
  string_of_row' "," TypeVarSet.empty (make_names (free_bound_row_type_vars ~include_aliases:true row)) row

let string_of_row_var row_var =
  match string_of_row_var' "," TypeVarSet.empty (make_names (free_bound_row_var_vars ~include_aliases:true row_var)) row_var with
    | None -> ""
    | Some s -> s

let string_of_environment env =
  let module M = Env.Show_t(Show.ShowDefaults(struct
                                                type a = datatype
                                                let format fmt a = 
                                                  Format.pp_print_string fmt (string_of_datatype a)
                                              end)) in
    M.show env

let string_of_typing_environment {var_env=env} = string_of_environment env

let make_fresh_envs : datatype -> datatype IntMap.t * row_var IntMap.t =
  let module M = IntMap in
  let empties = M.empty, M.empty in
  let union2 a b = M.fold M.add a b in
  let union2both (l,r) (ll,rr) = (union2 l ll, union2 r rr) in
  let union = List.fold_left union2both empties in
  let rec makeEnv recvars = function
      | `Not_typed
      | `Primitive _             -> empties
      | `Function (f, m, t)      -> union [makeEnv recvars f; makeEnv recvars m; makeEnv recvars t]
      | `Record row              
      | `Variant row             -> makeEnvR recvars row
      | `Table (l,r)             -> union [makeEnv recvars l; makeEnv recvars r]
      | `Alias ((name, ts), d)   -> union (List.map (makeEnv recvars) ts @ [makeEnv recvars d])
      | `Application (_, ds)     -> union (List.map (makeEnv recvars) ds)
      | `ForAll _                -> assert false
      | `MetaTypeVar point       ->
          begin
            match Unionfind.find point with
              | `Rigid var -> let l,r = empties in
                  (M.add var (fresh_rigid_type_variable ()) l, r)
              | `Flexible var -> let l, r = empties in
                  (M.add var (fresh_type_variable ()) l, r)
              | `Recursive (l, _) when List.mem l recvars -> empties
              | `Recursive (l, b) -> makeEnv (l::recvars) b
              | `Body t -> makeEnv recvars t
          end
  and makeEnvR recvars ((field_env, row_var):row) =
    let field_vars = 
      FieldEnv.fold (fun _ t envs ->
                       match t with 
                           `Present t -> union [envs; makeEnv recvars t]
                         | `Absent -> envs) field_env empties
    and row_vars = 
      match Unionfind.find row_var with
        | `Closed -> empties
        | `Flexible var -> let l, r = empties in
            (l, M.add var (fresh_row_variable ()) r)
        | `Rigid var -> let l, r = empties in
            (l, M.add var (fresh_rigid_row_variable ()) r)
        | `Recursive (l, _) when List.mem l recvars -> empties
        | `Recursive (l, row) -> makeEnvR (l::recvars) row
        | `Body row -> makeEnvR recvars row
    in union [field_vars; row_vars]
  in makeEnv []

let make_rigid_envs datatype : datatype IntMap.t * row_var IntMap.t =
  let tenv, renv = make_fresh_envs datatype in
    (IntMap.map (fun _ -> fresh_rigid_type_variable ()) tenv,
     IntMap.map (fun _ -> fresh_rigid_row_variable ()) renv)

let make_wobbly_envs datatype : datatype IntMap.t * row_var IntMap.t =
  let tenv, renv = make_fresh_envs datatype in
    (IntMap.map (fun _ -> fresh_type_variable ()) tenv,
     IntMap.map (fun _ -> fresh_row_variable ()) renv)


(* subtyping *)
let is_sub_type, is_sub_row =
  let module S = TypeVarSet in
  let rec is_sub_type = fun rec_vars (t, t') ->
    match t, t' with
      | `Not_typed, `Not_typed -> true
      | `Primitive p, `Primitive q -> p=q
      | `Function (f, m, t), `Function (f', m', t') ->
          is_sub_type rec_vars (f', f)
          && is_sub_type rec_vars (m, m')
          && is_sub_type rec_vars (t, t')
      | `Record row', `Record row
      | `Variant row, `Variant row' ->
          let lrow, _ = unwrap_row row
          and rrow, _ = unwrap_row row' in
            is_sub_row rec_vars (lrow, rrow)
      | `Table _, `Table _ -> failwith "not implemented subtyping on tables yet"
      | `Application (labs, lts), `Application (rabs, rts) ->
          (* WARNING:

             This assumes that abstract type parameters are all covariant -
             which happens to be true for all the built-in abstract types we
             currently support.
          *)
          (* TODO: implement variance annotations *)
          labs = rabs &&
              List.for_all2 (fun t t' -> is_sub_type rec_vars (t, t')) lts rts
      | `MetaTypeVar point, `MetaTypeVar point' ->
          begin
            match Unionfind.find point, Unionfind.find point' with
              | `Rigid var, `Rigid var'
              | `Flexible var, `Flexible var' -> var=var'
              | `Body t, _ -> is_sub_type rec_vars (t, t')
              | _, `Body t -> is_sub_type rec_vars (t, t')
              | `Recursive (var, t), `Recursive (var', t') ->
                  failwith "not implemented subtyping on recursive types yet"
              | _, _ -> false
          end
      | `MetaTypeVar point, _ ->
          begin
            match Unionfind.find point with
              | `Rigid _
              | `Flexible _
              | `Recursive _ -> false
              | `Body t -> is_sub_type rec_vars (t, t')
          end
      | _, `MetaTypeVar point ->
          begin
            match Unionfind.find point with
              | `Rigid _
              | `Flexible _
              | `Recursive _ -> false
              | `Body t' -> is_sub_type rec_vars (t, t')
          end
      | (`Alias (_, t)), t'
      | t, (`Alias (_, t')) -> is_sub_type rec_vars (t, t')
      | `ForAll (qs, t), `ForAll (qs', t') ->
          failwith "not implemented subtyping on forall types yet"          
      | _, _ -> false
  and is_sub_row =
    fun rec_vars ((lfield_env, lrow_var as lrow), (rfield_env, rrow_var as rrow)) ->
      let sub_fields =
        FieldEnv.fold (fun name t b ->
                         match t with
                           | `Present t ->
                               if FieldEnv.mem name rfield_env then
                                 match FieldEnv.find name rfield_env with
                                   | `Present t' ->
                                       is_sub_type rec_vars (t, t')
                                   | `Absent -> false
                               else
                                 false
                           | `Absent ->
                               true) lfield_env true in
      let sub_row_vars =
        match Unionfind.find lrow_var, Unionfind.find rrow_var with
          | `Flexible var, `Flexible var'
          | `Rigid var, `Rigid var' -> var=var'
          | `Closed, _ -> true
          | `Body lrow, _ -> is_sub_row rec_vars (lrow, rrow)
          | _, `Body rrow -> is_sub_row rec_vars (lrow, rrow)
          | `Recursive (lvar, lrow), `Recursive (rvar, rrow) ->
              failwith "not implemented subtyping on recursive rows yet"              
          | _, _ -> false
      in
        sub_fields && sub_row_vars
  in
    ((fun t -> is_sub_type S.empty t),
     (fun row -> is_sub_row S.empty row))


let make_tuple_type (ts : datatype list) : datatype =
  `Record 
    (snd 
       (List.fold_left
          (fun (n, row) t -> n+1, row_with (string_of_int n, `Present t) row)
          (1, make_empty_closed_row ())
          ts))

let make_list_type t = `Application (list, [t])
let make_mailbox_type t = `Application (mailbox, [t])

let extend_row fields (fields', row_var) =
  (FieldEnv.fold
     (fun name t fields -> FieldEnv.add name (`Present t) fields)
     fields
     fields',
   row_var)

let make_closed_row fields =
  (FieldEnv.map (fun t -> `Present t) fields), closed_row_var

let make_record_type ts = `Record (make_closed_row ts)
let make_variant_type ts = `Variant (make_closed_row ts)

let make_table_type (r, w) = `Table (r, w)

