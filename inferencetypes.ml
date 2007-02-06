(*pp deriving *)
open Utility

module FieldEnv = Utility.StringMap
type 'a stringmap = 'a Utility.stringmap
type 'a field_env = 'a stringmap

module Typeable_field_env = Typeable_stringmap
module Show_field_env = Show_stringmap
module Pickle_field_env = Pickle_stringmap

(* type var sets *)
module TypeVarSet = Utility.IntSet
type type_var_set = TypeVarSet.t


(* points *)
type 'a point = 'a Unionfind.point 

module Show_point (S : Show.Show) = Show.Show_unprintable(struct type a = S.a point end)
module Pickle_point (S : Pickle.Pickle) = Pickle.Pickle_unpicklable(struct type a = S.a point let tname = "point" end)


type primitive = [ `Bool | `Int | `Char | `Float | `XmlItem | `DB
                 | `Abstract ]
    deriving (Typeable, Show, Pickle)

(* this is what we should replace meta type vars with *)
type 't meta_type_var =
    [ `Flexible of int
    | `Rigid of int
    | `Recursive of (int * 't)
    | `Body of 't ]

(* this is what we should replace meta row vars with *)
type 't meta_row_var =
    [ 't meta_type_var
    | `Closed ]

type datatype =
    [ `Not_typed
    | `Primitive of primitive
    | `TypeVar of int
    | `RigidTypeVar of int
    | `Function of (datatype * datatype * datatype)
    | `Record of row
    | `Variant of row
    | `Table of row
    | `Recursive of (int * datatype)
    | `Application of (string * datatype list)
    | `MetaTypeVar of datatype point ]
and field_spec = [ `Present of datatype | `Absent ]
and field_spec_map = (field_spec) field_env
and row = field_spec_map * row_var
and row_var =
    [ `RowVar of int option
    | `RecRowVar of int * row
    | `MetaRowVar of row point
    | `RigidRowVar of int ]
      deriving (Show, Pickle)

type type_variable = [`TypeVar of int | `RigidTypeVar of int | `RowVar of int]
    deriving (Typeable, Show, Pickle)
type quantifier = type_variable
    deriving (Typeable, Show, Pickle)

type assumption = ((quantifier list) * datatype)
    deriving (Show, Pickle)
type environment = ((string * assumption) list)
    deriving (Show, Pickle)
type alias_environment = assumption stringmap
    deriving (Show, Pickle)
type typing_environment = environment * alias_environment
    deriving (Show, Pickle)

(* Functions on environments *)
let environment_values = fun env -> snd (List.split env)
let lookup = fun x -> List.assoc x

let concat_environment
      ((types1, aliases1) : typing_environment)
      (types2, aliases2) : typing_environment = 
    (types1 @ types2, superimpose aliases1 aliases2)

(* Generation of fresh type variables *)
let type_variable_counter = ref 0

let fresh_raw_variable : unit -> int =
  function () -> 
    incr type_variable_counter; !type_variable_counter


(* Caveat: Map.fold behaves differently between Ocaml 3.08.3 and 3.08.4

let map_fold_increasing = ocaml_version_atleast [3; 8; 4]
*)
(*
  [NOTE]
  
  We use Map.fold and Set.fold too often to support OCaml versions prior to 3.08.4
*)
let _ =
  if not (ocaml_version_atleast [3; 8; 4]) then
    failwith ("Links requires OCaml version 3.08.4 or later")
  else
    ()

(* type operations *)
module type TYPEOPS =
sig
  (* type variable construction *)
  val make_type_variable : int -> datatype
  val make_rigid_type_variable : int -> datatype
  val make_row_variable : int -> row_var

  (* fresh type variable generation *)
  val fresh_type_variable : unit -> datatype
  val fresh_rigid_type_variable : unit -> datatype
  val fresh_row_variable : unit -> row_var

  (* empty row constructors *)
  val make_empty_closed_row : unit -> row
  val make_empty_open_row : unit -> row
  val make_empty_open_row_with_var : int -> row

  (* singleton row constructors *)
  val make_singleton_closed_row : (string * field_spec) -> row
  val make_singleton_open_row : (string * field_spec) -> row
  val make_singleton_open_row_with_var : (string * field_spec) -> int -> row

  (* row predicates *)
  val is_closed_row : row -> bool
  val is_absent_from_row : string -> row -> bool

  (* row_var retrieval *)
  val get_row_var : row -> int option

  (* row update *)
  val set_field : (string * field_spec) -> row -> row

  (* constants *)
  val empty_field_env : field_spec_map
  val closed_row_var : row_var
end

module InferenceTypeOps : TYPEOPS
=
struct
  let empty_field_env = FieldEnv.empty
  let closed_row_var = `RowVar None

  let make_type_variable var = `MetaTypeVar (Unionfind.fresh (`TypeVar var))
  let make_rigid_type_variable var = `MetaTypeVar (Unionfind.fresh (`RigidTypeVar var))
  let make_row_variable var = `MetaRowVar (Unionfind.fresh (empty_field_env, `RowVar (Some var)))

  let is_closed_row =
    let rec is_closed rec_vars =
      function
        | (_, `RowVar None) -> true
        | (_, `MetaRowVar point) ->
            begin
              match Unionfind.find point with
                | (_, `RowVar None) -> true
                | (_, `RigidRowVar _)
                | (_, `RowVar (Some _)) -> false
                | (_, `RecRowVar (var, row)) ->
                    ((TypeVarSet.mem var rec_vars) or (is_closed (TypeVarSet.add var rec_vars) row))
                | (_, `MetaRowVar _) as row ->
                    is_closed rec_vars row
            end
        | _ -> assert false
    in
      is_closed TypeVarSet.empty

  let get_row_var = fun (_, row_var) ->
    let rec get_row_var' = fun rec_vars -> function
      | `RowVar None -> None
      | `RowVar (Some r)
      | `RigidRowVar r -> Some r
      | `RecRowVar (var, (_, row_var')) ->
          if TypeVarSet.mem var rec_vars then
            None
          else
            get_row_var' (TypeVarSet.add var rec_vars) row_var'
      | `MetaRowVar point ->
          get_row_var' rec_vars (snd (Unionfind.find point))
    in
      get_row_var' TypeVarSet.empty row_var

  let fresh_type_variable = make_type_variable -<- fresh_raw_variable
  let fresh_rigid_type_variable = make_rigid_type_variable -<- fresh_raw_variable
  let fresh_row_variable = make_row_variable -<- fresh_raw_variable

  let make_empty_closed_row () = empty_field_env, closed_row_var
  let make_empty_open_row () = empty_field_env, fresh_row_variable ()
  let make_empty_open_row_with_var var = empty_field_env, make_row_variable var

  let make_singleton_closed_row (label, field_spec) =
    FieldEnv.add label field_spec empty_field_env, closed_row_var
  let make_singleton_open_row (label, field_spec) =
    FieldEnv.add label field_spec empty_field_env, fresh_row_variable ()
  let make_singleton_open_row_with_var (label, field_spec) var =
    FieldEnv.add label field_spec empty_field_env, make_row_variable var

  let is_absent_from_row label (field_env, _ as row) =
    if FieldEnv.mem label field_env then
      FieldEnv.find label field_env = `Absent
    else
      is_closed_row row

  let set_field (label, f) (field_env, row_var) =
    FieldEnv.add label f field_env, row_var
end



(*** end of type_basis ***)


(* remove any top-level `MetaTypeVars from a type *)
let rec concrete_type = function
  | `MetaTypeVar point ->
      concrete_type (Unionfind.find point)
  | t -> t

(* [TODO]
      change the return type of these functions to be TypeVarSet.t
*)
let
    free_type_vars, free_row_type_vars =
  let rec free_type_vars' : type_var_set -> datatype -> int list = fun rec_vars ->
    function
      | `Not_typed               -> []
      | `Primitive _             -> []
      | `RigidTypeVar var        -> [var]
      | `TypeVar var             -> [var]
      | `Function (f, m, t)      ->
          free_type_vars' rec_vars f @ free_type_vars' rec_vars m @ free_type_vars' rec_vars t
      | `Record row
      | `Variant row
      | `Table row               -> free_row_type_vars' rec_vars row
      | `Recursive (var, body)   ->
          if TypeVarSet.mem var rec_vars then
            []
          else
            free_type_vars' (TypeVarSet.add var rec_vars) body
      | `Application (_, datatypes) -> Utility.concat_map (free_type_vars' rec_vars) datatypes
      | `MetaTypeVar point       -> free_type_vars' rec_vars (Unionfind.find point)
  and free_row_type_vars' : type_var_set -> row -> int list = 
    fun rec_vars (field_env, row_var) ->
      let field_vars =
        FieldEnv.fold (fun _ t field_vars ->
                         match t with
                           | `Present t ->
                               field_vars @ (free_type_vars' rec_vars t)
                           | `Absent ->
                               field_vars) field_env [] in
      let row_vars =
        match row_var with
          | `RigidRowVar var
          | `RowVar (Some var) -> [var]
          | `RowVar None -> []
          | `RecRowVar (var, row) -> if TypeVarSet.mem var rec_vars then
              []
            else
              (free_row_type_vars' (TypeVarSet.add var rec_vars) row)
          | `MetaRowVar point -> free_row_type_vars' rec_vars (Unionfind.find point)
      in
        field_vars @ row_vars
  in
    ((fun t -> Utility.unduplicate (=) (free_type_vars' TypeVarSet.empty t)),
     (fun t -> Utility.unduplicate (=) (free_row_type_vars' TypeVarSet.empty t)))


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

let is_canonical_row_var =
  function
    | `RowVar None -> true
    | `MetaRowVar point ->
        begin
          match snd (Unionfind.find point) with
            | `RigidRowVar _
            | `RowVar (Some _) -> true
            | `RowVar None
            | `RecRowVar _
            | `MetaRowVar _ -> false
        end
    | `RowVar (Some _)
    | `RigidRowVar _
    | `RecRowVar _ -> assert false

let is_rigid_row : row -> bool =
  let rec is_rigid rec_vars =
    function
      | (_, `RowVar None) -> true
      | (_, `MetaRowVar point) ->
          begin
            match Unionfind.find point with
              | (_, `RowVar (Some _)) -> false
              | (_, `RowVar None)
              | (_, `RigidRowVar _)  -> true
              | (_, `RecRowVar (var, row)) ->
                  ((TypeVarSet.mem var rec_vars) or (is_rigid (TypeVarSet.add var rec_vars) row))
              | (_, `MetaRowVar _) as row ->
                  is_rigid rec_vars row
          end
      | _ -> assert false
  in
    is_rigid TypeVarSet.empty

(* is_rigid_row_with_var var row
     returns true if row is rigid and has var as its row var
 *)
let is_rigid_row_with_var : int -> row -> bool =
  fun var ->
    let rec is_rigid var rec_vars =
      function
        | (_, `RowVar None) -> false
        | (_, `MetaRowVar point) ->
            begin
              match Unionfind.find point with
                | (_, `RowVar None)
                | (_, `RowVar (Some _)) -> false
                | (_, `RigidRowVar var') -> var=var'
                | (_, `RecRowVar (var, row)) ->
                    ((TypeVarSet.mem var rec_vars) or (is_rigid var (TypeVarSet.add var rec_vars) row))
                | (_, `MetaRowVar _) as row ->
                    is_rigid var rec_vars row
            end
        | _ -> assert false
    in
      is_rigid var TypeVarSet.empty


let is_flattened_row : row -> bool =
  let rec is_flattened = fun rec_vars -> function
    | (_, `MetaRowVar point) ->
        (match Unionfind.find point with 
           | (_, `RowVar None) -> false
           | (field_env', `RigidRowVar _)
           | (field_env', `RowVar (Some _)) ->
               assert(not (contains_present_fields field_env')); true
           | (field_env', `RecRowVar (var, rec_row)) ->
               assert(not (contains_present_fields field_env'));
               if TypeVarSet.mem var rec_vars then true
               else is_flattened (TypeVarSet.add var rec_vars) rec_row
           | (_ , `MetaRowVar _) -> false)
    | (_, `RigidRowVar _)
    | (_, `RowVar None) -> true
    | (_, `RowVar (Some _ ))
    | (_, `RecRowVar (_, _)) -> assert false
  in
    is_flattened TypeVarSet.empty

let is_empty_row : row -> bool =
  let rec is_empty = fun rec_vars -> fun (field_env, row_var) ->
    FieldEnv.is_empty field_env &&
      begin
        match row_var with
          | `MetaRowVar point ->
              let (field_env, row_var) = Unionfind.find point
              in
                FieldEnv.is_empty field_env &&
                  begin
                    match row_var with
                      | `RigidRowVar _
                      | `RowVar _ -> true
                      | `RecRowVar (var, _) when TypeVarSet.mem var rec_vars -> true
                      | `RecRowVar (var, rec_row) -> is_empty (TypeVarSet.add var rec_vars) rec_row
                      | `MetaRowVar point -> is_empty rec_vars (Unionfind.find point)
                  end
          | `RowVar None -> true
          | `RigidRowVar _
          | `RowVar (Some _)
          | `RecRowVar (_, _) -> assert false
      end
  in
    is_empty TypeVarSet.empty

(* 
 convert a row to the form (field_env, row_var)
 where row_var is of the form:
    `RowVar None
  | `MetaRowVar (empty, `RigidRowVar var)
  | `MetaRowVar (empty, `RowVar (Some var))
  | `MetaRowVar (empty, `RecRowVar (var, rec_row))
 *)
let flatten_row : row -> row =
  let rec flatten_row' : (row Unionfind.point) IntMap.t -> row -> row =
    fun rec_env row ->
      let row' =
        match row with
          | (field_env, `MetaRowVar point) ->
              let row' = Unionfind.find point in
                (match row' with
                     (field_env', `RowVar (Some _))
                   | (field_env', `RigidRowVar _)
                   | (field_env', `RecRowVar _) when (contains_present_fields field_env')
                       -> assert false
                   | (field_env', (`RowVar None as r)) ->
                       field_env_union (field_env, field_env'), r
                   | (_, `RigidRowVar _)
                   | (_, `RowVar (Some _)) -> row
                   | (_, `RecRowVar (var, rec_row)) ->
                       if IntMap.mem var rec_env then
                         field_env, `MetaRowVar (IntMap.find var rec_env)
                       else
                         (let point' = Unionfind.fresh (FieldEnv.empty, `RecRowVar (var, (FieldEnv.empty, `RowVar (Some var)))) in
                          let rec_row' = flatten_row' (IntMap.add var point' rec_env) rec_row in
                            Unionfind.change point' (FieldEnv.empty, `RecRowVar (var, rec_row'));
                            field_env, `MetaRowVar point')
                   | (_, `MetaRowVar _) ->
                       let field_env', row_var' = flatten_row' rec_env row' in
                         field_env_union (field_env, field_env'), row_var')

          | (_, `RowVar None) -> row
          | _ -> assert false
      in
        assert (is_flattened_row row');
        row'
  in
    flatten_row' IntMap.empty


(*
 As flatten_row except if the flattened row_var is of the form:

  `MetaRowVar (`RecRowVar (var, rec_row))

then it is unwrapped. This ensures that all the fields are exposed
in field_env.
 *)
let unwrap_row : row -> (row * (int * row) option) =
  let rec unwrap_row' : (row Unionfind.point) IntMap.t -> row -> (row * (int * row) option) =
    fun rec_env -> function
      | (field_env, `MetaRowVar point) as row ->
          (match Unionfind.find point with
             | (field_env', `RowVar (Some _))
             | (field_env', `RigidRowVar _)
             | (field_env', `RecRowVar _) when (contains_present_fields field_env')
                 -> assert false
             | (field_env', `RowVar None) ->
                 (field_env_union (field_env, field_env'), `RowVar None), None
             | (_, `RowVar (Some _))
             | (_, `RigidRowVar _) ->
                 row, None
             | (_, `RecRowVar ((var, body) as rec_row)) ->
                 if IntMap.mem var rec_env then
                   (field_env, `MetaRowVar (IntMap.find var rec_env)), Some rec_row
                 else
                   (let point' = Unionfind.fresh (FieldEnv.empty, `RecRowVar (var, (FieldEnv.empty, `RowVar (Some var)))) in
                    let unwrapped_body, _ = unwrap_row' (IntMap.add var point' rec_env) body in
                      Unionfind.change point' (FieldEnv.empty, `RecRowVar (var, unwrapped_body));
                      let field_env', row_var' = unwrapped_body in
                        (field_env_union (field_env, field_env'), row_var')), Some rec_row
             | (_, `MetaRowVar _) as row' ->
                 let (field_env', row_var'), rec_row = unwrap_row' rec_env row' in
                   (field_env_union (field_env, field_env'), row_var'), rec_row)
      | (_, `RowVar None) as row -> row, None
      | _ -> assert false
  in
    fun row ->
      let unwrapped_row, rec_row = unwrap_row' IntMap.empty row
      in
        assert (is_flattened_row unwrapped_row);
        unwrapped_row, rec_row  

(* useful types *)
let unit_type = `Record (InferenceTypeOps.make_empty_closed_row ())
let string_type = `Application ("String", [])
let xml_type = `Application ("Xml", [])

(*
let empty_var_maps : unit -> inference_type_map =
  fun () ->
    let type_var_map : (datatype Unionfind.point) IntMap.t ref = ref IntMap.empty in
    let row_var_map : (row Unionfind.point) IntMap.t ref = ref IntMap.empty in
      (type_var_map, row_var_map)
*)  

(* skeleton for performing a fold over (inference) datatypes *)
let rec datatype_skeleton :  type_var_set -> datatype -> datatype = fun rec_vars ->
  function
    | `Not_typed -> `Not_typed
    | `Primitive p -> `Primitive p
    | `Function (f, m, t) ->
        `Function (datatype_skeleton rec_vars f, datatype_skeleton rec_vars m, datatype_skeleton rec_vars t)
    | `Record row -> `Record (row_skeleton rec_vars row)
    | `Variant row -> `Variant (row_skeleton rec_vars row)
    | `Table row -> `Table (row_skeleton rec_vars row)
    | `Application (s, ts) -> `Application (s, List.map (datatype_skeleton rec_vars) ts)
    | `MetaTypeVar point ->
        (match Unionfind.find point with
           | `RigidTypeVar var -> `RigidTypeVar var
           | `TypeVar var -> `TypeVar var
           | `Recursive (var, t) ->
               if TypeVarSet.mem var rec_vars then
                 `Recursive (var, t)
               else
                 `Recursive (var, datatype_skeleton (TypeVarSet.add var rec_vars) t)
           | `MetaTypeVar _ -> assert false
           | t -> `MetaTypeVar (Unionfind.fresh (datatype_skeleton rec_vars t)))
    | `Recursive _
    | `RigidTypeVar _
    | `TypeVar _ -> assert false
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
    match row_var with
      | `RowVar None -> `RowVar None
      | `MetaRowVar point ->
          let (field_env, row_var) = Unionfind.find point in
            assert(not (contains_present_fields field_env));
            (match row_var with
               | `RowVar None -> `RowVar None
               | `RowVar (Some var)
               | `RigidRowVar var -> `RowVar (Some var)
               | `RecRowVar (var, rec_row) ->
                   if TypeVarSet.mem var rec_vars then
                     `RowVar (Some var)
                   else
                     `RecRowVar (var, row_skeleton (TypeVarSet.add var rec_vars) rec_row)
               | `MetaRowVar _ -> assert false)
      | `RigidRowVar _
      | `RowVar (Some _)
      | `RecRowVar (_, _) -> assert false
  in
    field_env', row_var'


(* check for undefined aliases *)
exception UndefinedAlias of string

let rec free_alias_check alias_env = fun rec_vars ->
  let fac = free_alias_check alias_env in
    function
      | `Not_typed -> ()
      | `Primitive p -> ()
      | `Function (f, m, t) -> fac rec_vars f; fac rec_vars m; fac rec_vars t
      | `Record row -> free_alias_check_row alias_env rec_vars row
      | `Variant row -> free_alias_check_row alias_env rec_vars row
      | `Table row -> free_alias_check_row alias_env rec_vars row
      | `Application (s, ts) ->
          if StringMap.mem s alias_env then
            List.iter (fac rec_vars) ts
          else
            raise (UndefinedAlias ("Unbound alias: "^s))
      | `MetaTypeVar point ->
          (match Unionfind.find point with
             | `RigidTypeVar var
             | `TypeVar var -> ()
             | `Recursive (var, t) ->
                 if TypeVarSet.mem var rec_vars then
                   ()
                 else
                   fac (TypeVarSet.add var rec_vars) t
             | `MetaTypeVar _ -> assert false
             | t -> fac rec_vars t)
      | `Recursive _
      | `RigidTypeVar _
      | `TypeVar _ -> assert false
and free_alias_check_field_spec alias_env = fun rec_vars ->
  function
    | `Present t -> free_alias_check alias_env rec_vars t
    | `Absent -> ()
and free_alias_check_field_spec_map alias_env = fun rec_vars field_env ->
  FieldEnv.iter (fun _ -> free_alias_check_field_spec alias_env rec_vars) field_env
and free_alias_check_row alias_env = fun rec_vars row ->
  let field_env, row_var = row
  in
    free_alias_check_field_spec_map alias_env rec_vars field_env;
    match row_var with
      | `RowVar None -> ()
      | `MetaRowVar point ->
          let (field_env, row_var) = Unionfind.find point in
            assert(not (contains_present_fields field_env));
            (match row_var with
               | `RowVar None -> ()
               | `RowVar (Some var)
               | `RigidRowVar var -> ()
               | `RecRowVar (var, rec_row) ->
                   if TypeVarSet.mem var rec_vars then
                     ()
                   else
                     free_alias_check_row alias_env (TypeVarSet.add var rec_vars) rec_row
               | `MetaRowVar _ -> assert false)
      | `RigidRowVar _
      | `RowVar (Some _)
      | `RecRowVar (_, _) -> assert false

(* interface *)
let free_alias_check alias_env = free_alias_check alias_env TypeVarSet.empty
let free_alias_check_row alias_env = free_alias_check_row alias_env TypeVarSet.empty


(* whether to display mailbox annotations on arrow types
   [NOTE]
      unused mailbox parameters are never shown
 *)
let show_mailbox_annotations = Settings.add_bool("show_mailbox_annotations", true, `User)

(* pretty-print type vars as raw numbers rather than letters *)
let show_raw_type_vars = Settings.add_bool("show_raw_type_vars", false, `User)

(*
  [HACK]
  used to temporarily disable mailbox typing for two-pass type-checking
*)
let use_mailbox_typing = ref true

(* return true if mailbox typing is currently switched on *)
let using_mailbox_typing () = !use_mailbox_typing

(* call f()
   with mailbox typing switched on or off according to
   the value of b
   (guarantees that the state of mailbox typing is
   restored afterwards)
*)
let with_mailbox_typing b f =
  let oldb = using_mailbox_typing ()
  in
    try
      use_mailbox_typing := b;
      let result = f() in
	use_mailbox_typing := oldb;
	result
    with e ->
      use_mailbox_typing := oldb;
      raise e


(* Type printers *)

exception Not_tuple

let string_of_primitive : primitive -> string = function
  | `Bool -> "Bool"  | `Int -> "Int"  | `Char -> "Char"  | `Float   -> "Float"  
  | `XmlItem -> "XmlItem" | `DB -> "Database" | `Abstract -> "(abstract)"

let rec string_of_datatype' : type_var_set -> string IntMap.t -> datatype -> string =
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
    let is_tuple (field_env, _) =
      let b, i =
        FieldEnv.fold (fun label t (b, i) ->
                         match t with
                           | `Present _ ->
	                       (* check that the labels are numbers 1..n *)
                               b && (String.compare (string_of_int i) label)=0, i+1
                           | `Absent -> false, i+1) field_env (true, 1)
      in
	(* 0/1-tuples are displayed as records *)
        b && i > 2 in
    (* precondition: the row is unwrapped *)
    let string_of_tuple (field_env, row_var) =
      let row_var_string =
        match row_var with
          | `RowVar None -> ""
          | `MetaRowVar point ->
              begin
                let _, row_var = Unionfind.find point in
                  match row_var with 
                    | `RowVar (Some var)
                    | `RigidRowVar var -> " | "
                    | `RowVar None
                    | `RecRowVar _
                    | `MetaRowVar _ -> assert false
              end
          | `RowVar (Some _) | `RigidRowVar _ | `RecRowVar _ -> assert false in
      let ss = List.rev
        (FieldEnv.fold (fun _ t ss ->
                          match t with
                            | `Present t -> (sd t) :: ss
                            | `Absent -> assert false) field_env [])
      in
          "(" ^ (String.concat ", " ss) ^ row_var_string ^ ")"
    in
      match datatype with
        | `Not_typed       -> "not typed"
        | `Primitive p     -> string_of_primitive p
        | `MetaTypeVar point ->
            begin
              match Unionfind.find point with
                | `TypeVar var
                | `RigidTypeVar var -> IntMap.find var vars
                | `Recursive (var, body) ->
                    if TypeVarSet.mem var rec_vars then
                      IntMap.find var vars
                    else
	              "mu " ^ IntMap.find var vars ^ " . " ^
                        string_of_datatype' (TypeVarSet.add var rec_vars) vars body
                | t -> sd t
            end
        | `Function (f, mailbox_type, t) when using_mailbox_typing () ->
	    let arrow =
	      match concrete_type mailbox_type with
	        | `Application ("Mailbox", [t]) ->
		    string_of_mailbox_arrow (t)
	        | _ -> "->"
	    in
	      (match concrete_type f with
	         | `Record _ as f ->
		     sd f ^ " " ^arrow ^
		       " " ^ sd t
	         | _ ->
		     "(" ^ sd f ^ ") "^ arrow ^
		       " " ^ sd t)
        | `Function (f, _, t) ->
	    (match concrete_type f with
	       | `Record _ ->
	           sd f ^ " -> " ^ sd t
	       | _ ->
	           "(" ^ sd f ^ ") -> " ^ sd t)
        | `Record row      ->
            let row = unwrap row in
              (if is_tuple row then string_of_tuple row
	       else "(" ^ string_of_row' "," rec_vars vars row ^ ")")
        | `Variant row    -> "[|" ^ string_of_row' " | " rec_vars vars row ^ "|]"
        | `Table row      -> "TableHandle(" ^ string_of_row' "," rec_vars vars row ^ ")"
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
        | `Application ("List", [elems])              ->  "["^ sd elems ^"]"
        | `Application (s, []) -> s
        | `Application (s, ts) ->  s ^ " ("^ String.concat "," (List.map sd ts) ^")"
        | `TypeVar _
        | `RigidTypeVar _
        | `Recursive _ -> assert false

and string_of_row' sep rec_vars vars (field_env, row_var) =
  let present_strings, absent_strings =
    FieldEnv.fold (fun label t (present_strings, absent_strings) ->
                     match t with
                       | `Present t ->
                           (label ^ ":" ^ string_of_datatype' rec_vars vars t) :: present_strings, absent_strings
                       | `Absent ->
                           present_strings, (label ^ " -") :: absent_strings) field_env ([], [])  in
  let row_var_string = string_of_row_var' sep rec_vars vars row_var in
    (String.concat sep (List.rev (present_strings) @ List.rev (absent_strings))) ^
      (match row_var_string with
	 | None -> ""
	 | Some s -> "|"^s)
and string_of_row_var' sep rec_vars vars = function
  | `RowVar None -> None
  | `MetaRowVar point ->
      begin
        match Unionfind.find point with
          | (field_env, `RowVar (Some var))
          | (field_env, `RigidRowVar var) ->
              assert(not (contains_present_fields field_env));
              Some (IntMap.find var vars)
          | (field_env, `RecRowVar (var, row)) ->
              assert(not (contains_present_fields field_env));
              if TypeVarSet.mem var rec_vars then
                Some (IntMap.find var vars)
              else
                Some ("(mu " ^ IntMap.find var vars ^ " . " ^
                        string_of_row' sep (TypeVarSet.add var rec_vars) vars row ^ ")")
          | row -> Some (string_of_row' sep rec_vars vars row)
      end
  | `RowVar (Some _) | `RigidRowVar _ | `RecRowVar _ -> assert false

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

let rec free_bound_type_vars : type_var_set -> datatype -> TypeVarSet.t = fun rec_vars t ->
  let fbtv = free_bound_type_vars rec_vars in
    match t with
      | `Not_typed               -> TypeVarSet.empty
      | `Primitive _             -> TypeVarSet.empty
      | `MetaTypeVar point ->
          begin
            match Unionfind.find point with
              | `TypeVar var
              | `RigidTypeVar var -> TypeVarSet.singleton var
              | `Recursive (var, body) ->
                  if TypeVarSet.mem var rec_vars then
                    TypeVarSet.empty
                  else
                    TypeVarSet.add var (free_bound_type_vars (TypeVarSet.add var rec_vars) body)
              | t -> fbtv t
          end
      | `Function (f, m, t)      ->
          TypeVarSet.union
            (TypeVarSet.union (fbtv f) (fbtv t))
            (fbtv m)
      | `Record row
      | `Variant row
      | `Table row               -> free_bound_row_type_vars rec_vars row
      | `Application (_, datatypes) -> List.fold_right TypeVarSet.union (List.map fbtv datatypes) TypeVarSet.empty
      | `TypeVar _
      | `RigidTypeVar _
      | `Recursive _ -> assert false
and free_bound_row_type_vars rec_vars (field_env, row_var) =
  let field_type_vars =
    FieldEnv.fold (fun _ t tvs ->
                     match t with
                       | `Present t ->
                           TypeVarSet.union tvs (free_bound_type_vars rec_vars t)
                       | `Absent ->
                           tvs) field_env TypeVarSet.empty in
  let row_var = free_bound_row_var_vars rec_vars row_var in
    TypeVarSet.union field_type_vars row_var  
and free_bound_row_var_vars rec_vars row_var = 
  match row_var with
    | `RowVar None -> TypeVarSet.empty
    | `MetaRowVar point ->
        begin
          match Unionfind.find point with
            | (field_env, `RowVar (Some var))
            | (field_env, `RigidRowVar var)  ->
                assert(not (contains_present_fields field_env));
                TypeVarSet.singleton var
            | (field_env, `RecRowVar (var, row)) ->
                assert(not (contains_present_fields field_env));
                if TypeVarSet.mem var rec_vars then
                  TypeVarSet.empty
                else
                  TypeVarSet.add var
                    (free_bound_row_type_vars (TypeVarSet.add var rec_vars) row)
            | row -> free_bound_row_type_vars rec_vars row
        end
    | `RowVar (Some _) | `RigidRowVar _ | `RecRowVar _ -> assert false

let free_bound_type_vars = free_bound_type_vars TypeVarSet.empty
let free_bound_row_type_vars = free_bound_row_type_vars TypeVarSet.empty
let free_bound_row_var_vars = free_bound_row_var_vars TypeVarSet.empty

let rec type_aliases : type_var_set -> datatype -> StringSet.t = fun rec_vars t ->
  let tas = type_aliases rec_vars in
    match t with
      | `Not_typed
      | `Primitive _ -> StringSet.empty
      | `MetaTypeVar point ->
          begin
            match Unionfind.find point with
              | `TypeVar var
              | `RigidTypeVar var -> StringSet.empty
              | `Recursive (var, body) ->
                  if TypeVarSet.mem var rec_vars then
                    StringSet.empty
                  else
                    type_aliases (TypeVarSet.add var rec_vars) body
              | t -> tas t
          end
      | `TypeVar _
      | `RigidTypeVar _        -> StringSet.empty
      | `Function (f, m, t)      ->
          StringSet.union
            (StringSet.union (tas f) (tas t))
            (tas m)
      | `Record row
      | `Variant row
      | `Table row               -> row_type_aliases rec_vars row
      | `Application (alias, datatypes) -> List.fold_right StringSet.union (List.map tas datatypes) (StringSet.singleton alias)
      | `TypeVar _ | `RigidTypeVar _ | `Recursive _ -> assert false
and row_type_aliases rec_vars (field_env, row_var) =
  let field_type_aliases = 
    FieldEnv.fold (fun _ t ftas ->
                     match t with
                       | `Present t ->
                           StringSet.union ftas (type_aliases rec_vars t)
                       | `Absent ->
                           ftas) field_env StringSet.empty in
  let row_var = row_var_type_aliases rec_vars row_var in
    StringSet.union field_type_aliases row_var  
and row_var_type_aliases rec_vars row_var = 
  match row_var with
    | `RowVar None -> StringSet.empty
    | `MetaRowVar point ->
        begin
          match Unionfind.find point with
            | (field_env, `RowVar _)
            | (field_env, `RigidRowVar _) ->
                assert(not (contains_present_fields field_env));
                StringSet.empty
            | (field_env, `RecRowVar (var, row)) ->
                assert(not (contains_present_fields field_env));
                if TypeVarSet.mem var rec_vars then
                  StringSet.empty
                else
                  row_type_aliases (TypeVarSet.add var rec_vars) row
            | row -> row_type_aliases rec_vars row
        end
    | `RowVar (Some _) | `RigidRowVar _ | `RecRowVar _ -> assert false


(* type aliases *)
type type_alias_set = Utility.StringSet.t

let type_aliases = type_aliases TypeVarSet.empty
let row_type_aliases = row_type_aliases TypeVarSet.empty


(* string conversions *)
let string_of_datatype (datatype : datatype) = 
  string_of_datatype' TypeVarSet.empty (make_names (free_bound_type_vars datatype)) datatype

let string_of_datatype_raw datatype = 
  string_of_datatype' TypeVarSet.empty (TypeVarSet.fold
		     (fun var name_map -> IntMap.add var (string_of_int var) name_map)
		     (free_bound_type_vars datatype) IntMap.empty) datatype

let string_of_row row = 
  string_of_row' "," TypeVarSet.empty (make_names (free_bound_row_type_vars row)) row

let string_of_row_var row_var =
  match string_of_row_var' "," TypeVarSet.empty (make_names (free_bound_row_var_vars row_var)) row_var with
    | None -> ""
    | Some s -> s

let string_of_quantifier = function
  | `TypeVar var -> string_of_int var
  | `RigidTypeVar var -> string_of_int var
  | `RowVar var -> "'" ^ string_of_int var
let string_of_assumption = function
  | [], datatype -> string_of_datatype datatype
  | assums, datatype -> "forall " ^ (String.concat ", " (List.map string_of_quantifier assums)) ^" . "^ string_of_datatype datatype
let string_of_environment env =
  "{ " ^ (String.concat " ; " (List.map (fun (f, s) -> f ^" : " ^ string_of_assumption s) env)) ^" }"

(** check for well-foundedness **)

(* return true if a variable occurs negatively in a type *)
let rec is_negative : TypeVarSet.t -> int -> datatype -> bool =
  fun rec_vars var t ->
    let isp = is_positive rec_vars var in
    let isn = is_negative rec_vars var in
    let isnr = is_negative_row rec_vars var in
      match t with
        | `Not_typed -> false
        | `Primitive _ -> false
        | `RigidTypeVar _
        | `TypeVar _ -> false
        | `MetaTypeVar point ->
            isn (Unionfind.find point)
        | `Function (f, m, t) ->
            isp f || isp m || isn t
        | `Record row -> isnr row
        | `Variant row -> isnr row
        | `Table row -> isnr row
        | `Application (_, ts) -> List.exists isn ts (* is this right? -jdy *)
        | `Recursive (var', t) ->
            not (TypeVarSet.mem var' rec_vars) &&
              is_negative (TypeVarSet.add var' rec_vars) var t
and is_negative_row : TypeVarSet.t -> int -> row -> bool =
  fun rec_vars var (field_env, row_var) ->
    is_negative_field_env rec_vars var field_env || is_negative_row_var rec_vars var row_var
and is_negative_field_env : TypeVarSet.t -> int -> field_spec_map -> bool =
  fun rec_vars var field_env ->
    FieldEnv.fold (fun _ spec result ->
                      match spec with
                        | `Absent -> result
                        | `Present t -> result || is_negative rec_vars var t
                   ) field_env false
and is_negative_row_var : TypeVarSet.t -> int -> row_var -> bool =
  fun rec_vars var -> function
    | `RigidRowVar _
    | `RowVar _ -> false
    | `RecRowVar (var', row) ->
        not (TypeVarSet.mem var' rec_vars) &&
          is_negative_row (TypeVarSet.add var' rec_vars) var row
    | `MetaRowVar point ->
        is_negative_row rec_vars var (Unionfind.find point)

and is_positive : TypeVarSet.t -> int -> datatype -> bool =
  fun rec_vars var t ->
    let isp = is_positive rec_vars var in
    let isn = is_negative rec_vars var in
    let ispr = is_positive_row rec_vars var in
      match t with
        | `Not_typed -> false
        | `Primitive _ -> false
        | `RigidTypeVar var'
        | `TypeVar var' -> var = var'
        | `MetaTypeVar point ->
            isp (Unionfind.find point)
        | `Function (f, m, t) ->
            isn f || isn m || isp t
        | `Record row -> ispr row
        | `Variant row -> ispr row
        | `Table row -> ispr row
        | `Application (_,ts) -> List.exists isp ts (* is this right? -jdy *)
        | `Recursive (var', t) ->
            not (TypeVarSet.mem var' rec_vars) &&
              is_positive (TypeVarSet.add var' rec_vars) var t
and is_positive_row : TypeVarSet.t -> int -> row -> bool =
  fun rec_vars var (field_env, row_var) ->
    is_positive_field_env rec_vars var field_env || is_positive_row_var rec_vars var row_var
and is_positive_field_env : TypeVarSet.t -> int -> field_spec_map -> bool =
  fun rec_vars var field_env ->
    FieldEnv.fold (fun _ spec result ->
                      match spec with
                        | `Absent -> result
                        | `Present t -> result || is_positive rec_vars var t
                   ) field_env false
and is_positive_row_var : TypeVarSet.t -> int -> row_var -> bool =
  fun rec_vars var -> function
    | `RowVar None -> false
    | `RigidRowVar var'
    | `RowVar (Some var') -> var = var'
    | `RecRowVar (var', row) ->
        not (TypeVarSet.mem var' rec_vars) &&
          is_positive_row (TypeVarSet.add var' rec_vars) var row
    | `MetaRowVar point ->
        is_positive_row rec_vars var (Unionfind.find point)

let is_negative = is_negative TypeVarSet.empty
let is_negative_row = is_negative_row TypeVarSet.empty
let is_negative_field_env = is_negative_field_env TypeVarSet.empty
let is_negative_row_var = is_negative_row_var TypeVarSet.empty

let is_positive = is_positive TypeVarSet.empty
let is_positive_row = is_positive_row TypeVarSet.empty
let is_positive_field_env = is_positive_field_env TypeVarSet.empty
let is_positive_row_var = is_positive_row_var TypeVarSet.empty
