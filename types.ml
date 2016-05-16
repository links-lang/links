(*pp deriving *)
open Utility

module FieldEnv = Utility.StringMap
type 'a stringmap = 'a Utility.stringmap
type 'a field_env = 'a stringmap deriving (Show)

(* type var sets *)
module TypeVarSet = Utility.IntSet

(* type var sets *)
module TypeVarMap = Utility.IntMap

(* points *)
type 'a point = 'a Unionfind.point deriving (Show)

type primitive = [ `Bool | `Int | `Char | `Float | `XmlItem | `DB | `String]
    deriving (Show)

type linearity   = [ `Any | `Unl ]
    deriving (Eq, Show)
type restriction = [ `Any | `Base | `Session ]
    deriving (Eq, Show)

type subkind = linearity * restriction
    deriving (Eq, Show)

type freedom = [`Rigid | `Flexible]
    deriving (Eq, Show)

type primary_kind = [ `Type | `Row | `Presence ]
    deriving (Eq, Show)

type kind = primary_kind * subkind
    deriving (Eq, Show)

type 't meta_type_var_non_rec_basis =
    [ `Var of (int * subkind * freedom)
    | `Body of 't ]
      deriving (Show)

type 't meta_type_var_basis =
    [ 't meta_type_var_non_rec_basis
    | `Recursive of (int * 't) ]
      deriving (Show)

type 'r meta_row_var_basis =
    [ 'r meta_type_var_basis | `Closed ]
      deriving (Show)

type 't meta_presence_var_basis =
    [ 't meta_type_var_non_rec_basis ]
      deriving (Show)

(* this subsumes all of the other meta_X_basis thingies *)
type 't meta_max_basis = 't meta_row_var_basis

type istring = string deriving (Show)
module Eq_istring = Deriving_Eq.Eq_immutable(struct type a = istring end)

module Abstype =
struct
  type t = { id    : istring ;
             name  : istring ;
             arity : kind list }
      deriving (Eq, Show)
  let make name arity =
    let id = Utility.gensym ~prefix:"abstype:" () in
      { id    = id ;
        name  = name ;
        arity = arity }
  let arity { arity = arity } = arity
  let name  { name  = name  } = name
  let compare l r = String.compare l.id r.id
end

let process  = {
  Abstype.id = "Process" ;
  name       = "Process" ;
  arity      = [`Row, (`Any, `Any)] ;
}

(* Lists are currently unlimited because the only deconstructors are
   hd and tl. There's nothing to stop us rolling our own potentially
   linear lists, though. *)
let list     = {
  Abstype.id = "List" ;
  name       = "List" ;
  arity      = [`Type, (`Unl, `Any)] ;
}

let event    = {
  Abstype.id = "Event" ;
  name       = "Event" ;
  arity      = [] ;
}
let dom_node = {
  Abstype.id = "DomNode" ;
  name       = "DomNode" ;
  arity      = [] ;
}

let access_point = {
  Abstype.id = "AP" ;
  name       = "AP" ;
  arity      = [`Type, (`Any, `Session)] ;
}

let socket = {
  Abstype.id = "Socket";
  name = "Socket";
  arity = []
}

type ('t, 'r) session_type_basis =
    [ `Input of 't * 't
    | `Output of 't * 't
    | `Select of 'r
    | `Choice of 'r
    | `Dual of 't
    | `End ]
      deriving (Show)

type typ =
    [ `Not_typed
    | `Primitive of primitive
    | `Function of (typ * row * typ)
    | `Lolli of (typ * row * typ)
    | `Record of row
    | `Variant of row
    | `Table of typ * typ * typ
    | `Alias of ((string * type_arg list) * typ)
    | `Application of (Abstype.t * type_arg list)
    | `MetaTypeVar of meta_type_var
    | `ForAll of (quantifier list ref * typ)
    | (typ, row) session_type_basis ]
and field_spec     = [ `Present of typ | `Absent | `Var of meta_presence_var ]
and field_spec_map = field_spec field_env
and row_var        = meta_row_var
and row            = field_spec_map * row_var * bool (* true if the row variable is dualised *)
and meta_type_var  = (typ meta_type_var_basis) point
and meta_row_var   = (row meta_row_var_basis) point
and meta_presence_var = (field_spec meta_presence_var_basis) point
and meta_var = [ `Type of meta_type_var | `Row of meta_row_var | `Presence of meta_presence_var ]
and quantifier = int * subkind * meta_var
and type_arg =
    [ `Type of typ | `Row of row | `Presence of field_spec ]
      deriving (Show)

type session_type = (typ, row) session_type_basis
  deriving (Show)

let dummy_type = `Primitive `Int

let is_present =
  function
  | `Present _           -> true
  | (`Absent | `Var _) -> false

type tycon_spec = [`Alias of quantifier list * typ | `Abstract of Abstype.t]

(* TODO: consider abstracting some of this subkind manipulation code *)

(* base type stuff *)
let rec is_base_type : typ -> bool =
  function
    | `Primitive ((`Bool | `Int | `Char | `Float | `String)) -> true
    | `Alias (_, t) -> is_base_type t
    | `MetaTypeVar point ->
        begin
          match Unionfind.find point with
            | `Var (_, (_, `Base), _) -> true
            | `Var _ -> false
            | `Body t -> is_base_type t
            | `Recursive _ -> false
        end
    | _ -> false

let rec is_base_row (fields, row_var, _) =
  let base_row_var =
    match Unionfind.find row_var with
      | `Closed
      | `Var (_, (_, `Base), _) -> true
      | `Var _ -> false
      | `Body row -> is_base_row row
      | `Recursive _ -> false in
  let base_fields =
    FieldEnv.fold
      (fun _ f b ->
        match f with
        | `Present t -> b && is_base_type t
        | (`Absent | `Var _) -> b)
      fields
      true
  in
    base_row_var && base_fields

let rec is_baseable_type : typ -> bool =
  function
    | `Primitive ((`Bool | `Int | `Char | `Float | `String)) -> true
    | `Alias (_, t) -> is_baseable_type t
    | `MetaTypeVar point ->
        begin
          match Unionfind.find point with
            | `Var (_, (_, `Base), `Rigid)
            | `Var (_, _, `Flexible) -> true
            | `Var (_, _, `Rigid) -> false
            | `Body t -> is_baseable_type t
            | `Recursive _ -> false
        end
    | _ -> false

let rec is_baseable_row (fields, row_var, _) =
  let base_row_var =
    match Unionfind.find row_var with
      | `Closed
      | `Var (_, (_, `Base), `Rigid)
      | `Var (_, _, `Flexible) -> true
      | `Var (_, _, `Rigid) -> false
      | `Body row -> is_baseable_row row
      | `Recursive _ -> false in
  let base_fields =
    FieldEnv.fold
      (fun _ f b ->
        match f with
        | `Present t -> b && is_baseable_type t
        | (`Absent | `Var _) -> b)
      fields
      true
  in
    base_row_var && base_fields

let rec basify_type : typ -> unit =
  function
    | `Primitive ((`Bool | `Int | `Char | `Float | `String)) -> ()
    | `Alias (_, t) -> basify_type t
    | `MetaTypeVar point ->
        begin
          match Unionfind.find point with
            | `Var (_, (_, `Base), _) -> ()
            | `Var (_, _, `Rigid) -> assert false
            | `Var (var, (lin, `Any), `Flexible) -> Unionfind.change point (`Var (var, (lin, `Base), `Flexible))
            | `Var (_, _, `Flexible) -> assert false
            | `Body t -> basify_type t
            | `Recursive _ -> assert false
        end
    | _ -> assert false

let rec basify_row (fields, row_var, _) =
  begin
    match Unionfind.find row_var with
      | `Closed
      | `Var (_, (_, `Base), _) -> ()
      | `Var (var, (lin, `Any), `Flexible) -> Unionfind.change row_var (`Var (var, (lin, `Base), `Flexible))
      | `Var _ -> assert false
      | `Body row -> basify_row row
      | `Recursive _ -> assert false
  end;
  FieldEnv.fold
    (fun _ f () ->
      match f with
      | `Present t         -> basify_type t
      | (`Absent | `Var _) -> ())
    fields
    ()

(* Var depends on Types so we use int instead of Var.var here... *)

(* set of recursive variables seen so far *)
type var_set = TypeVarSet.t

(* Check if the recursive variable has been seen and return a default
   value or continue accordingly *)
let check_rec : int -> var_set -> 'a -> (var_set -> 'a) -> 'a =
  fun var rec_vars default k ->
    if IntSet.mem var rec_vars then
      default
    else
      k (IntSet.add var rec_vars)

let var_of_quantifier : quantifier -> int =
  function
    | var, _, _ -> var

let kind_of_quantifier : quantifier -> kind =
  function
  | _, sk, `Type _     -> `Type, sk
  | _, sk, `Row _      -> `Row, sk
  | _, sk, `Presence _ -> `Presence, sk

let type_arg_of_quantifier : quantifier -> type_arg =
  function
    | _, _, `Type point     -> `Type (`MetaTypeVar point)
    | _, _, `Row row_var    -> `Row (FieldEnv.empty, row_var, false)
    | _, _, `Presence point -> `Presence (`Var point)

let primary_kind_of_quantifier : quantifier -> primary_kind =
  function
  | _, _, `Type _     -> `Type
  | _, _, `Row _      -> `Row
  | _, _, `Presence _ -> `Presence

let primary_kind_of_type_arg : type_arg -> primary_kind =
  function
  | `Type _     -> `Type
  | `Row _      -> `Row
  | `Presence _ -> `Presence

let add_quantified_vars qs vars =
  List.fold_right IntSet.add (List.map var_of_quantifier qs) vars


(* unl type stuff *)

let is_unl_point =
  fun f (rec_vars, quant_vars) point ->
    begin
      match Unionfind.find point with
      | `Closed -> true
      | `Var (var, (lin, _), _) -> IntSet.mem var quant_vars || lin=`Unl
      | `Body t -> f (rec_vars, quant_vars) t
      | `Recursive (var, t) ->
        check_rec var rec_vars true (fun rec_vars' -> f (rec_vars', quant_vars) t)
    end

let rec is_unl_type : (var_set * var_set) -> typ -> bool =
  fun (rec_vars, quant_vars) ->
    let iut t = is_unl_type (rec_vars, quant_vars) t in
      function
      | `Not_typed -> assert false
      | `Primitive _
      | `Function _ -> true
      | `Lolli _ -> false
      | `Record r
      | `Variant r -> is_unl_row (rec_vars, quant_vars) r
      | `Table _ -> true
      | `Alias (_, t) -> iut t
      (* We might support linear lists like this...
         but we'd need to replace hd and tl with a split operation. *)
      (* | `Application ({Abstype.id="List"}, [`Type t]) -> is_unl_type (rec_vars, quant_vars) t  *)
      | `Application _ -> true (* TODO: change this if we add linear abstract types *)
      | `MetaTypeVar point -> is_unl_point is_unl_type (rec_vars, quant_vars) point
      | `ForAll (qs, t) -> is_unl_type (rec_vars, add_quantified_vars !qs quant_vars) t
      | `Dual s -> is_unl_type (rec_vars, quant_vars) s
      | `End -> true
      | #session_type -> false
and is_unl_field vars =
  function
  | `Absent -> true
  | `Present t ->
    is_unl_type vars t
  | `Var point -> is_unl_point is_unl_field vars point
and is_unl_row vars (fields, row_var, _) =
  let unl_row_var = is_unl_point is_unl_row vars row_var in
  let unl_fields =
    FieldEnv.fold
      (fun _ f b -> b && is_unl_field vars f)
      fields
      true in
  unl_row_var && unl_fields

let point_can_be_unl =
  fun f ((rec_vars, quant_vars) as vars) point ->
    begin
        match Unionfind.find point with
        | `Closed -> true
        | `Var (v, (lin, _), `Rigid) -> IntSet.mem v quant_vars || lin=`Unl
        | `Var (_, _, `Flexible)     -> true
        | `Body t -> f vars t
        | `Recursive (var, t) ->
          check_rec var rec_vars true (fun rec_vars' -> f (rec_vars', quant_vars) t)
      end

let rec type_can_be_unl : var_set * var_set -> typ -> bool =
  fun ((rec_vars, quant_vars) as vars) ->
    let tcu t = type_can_be_unl vars t in
    function
    | `Not_typed -> assert false
    | `Primitive _
    | `Function _ -> true
    | `Lolli _ -> false
    | `Record r
    | `Variant r -> row_can_be_unl vars r
    | `Table _ -> true
    | `Alias (_, t) -> tcu t
    (* We might support linear lists like this...
         but we'd need to replace hd and tl with a split operation. *)
    (* | `Application ({Abstype.id="List"}, [`Type t]) -> tcu t *)
    | `Application _ -> true (* TODO: change this if we add linear abstract types *)
    | `MetaTypeVar point -> point_can_be_unl type_can_be_unl vars point
    | `ForAll (qs, t) -> type_can_be_unl (rec_vars, add_quantified_vars !qs quant_vars) t
    | `Dual s -> type_can_be_unl vars s
    | `End -> true
    | #session_type -> false
and field_can_be_unl vars =
  function
  | `Absent    -> true
  | `Present t -> type_can_be_unl vars t
  | `Var point -> point_can_be_unl field_can_be_unl vars point
and row_can_be_unl vars (fields, row_var, _) =
  let unl_row_var = point_can_be_unl row_can_be_unl vars row_var in
  let unl_fields =
    FieldEnv.fold
      (fun _ f b -> b && field_can_be_unl vars f)
      fields
      true in
  unl_row_var && unl_fields

let is_unl_type = is_unl_type (IntSet.empty, IntSet.empty)
let is_unl_row = is_unl_row (IntSet.empty, IntSet.empty)

let type_can_be_unl = type_can_be_unl (IntSet.empty, IntSet.empty)
let row_can_be_unl = row_can_be_unl (IntSet.empty, IntSet.empty)

let make_point_unl : ((var_set * var_set) -> 'a -> unit) -> (var_set * var_set) -> [< 'a meta_max_basis] point -> unit =
  fun f ((rec_vars, quant_vars) as vars) point ->
    match Unionfind.find point with
    | `Closed -> ()
    | `Var (v, (lin, _), `Rigid)       -> if IntSet.mem v quant_vars || lin = `Unl then () else assert false
    | `Var (var, (_, rest), `Flexible) -> Unionfind.change point (`Var (var, (`Unl, rest), `Flexible))
    | `Body t -> f vars t
    | `Recursive (var, t) ->
      check_rec var rec_vars () (fun rec_vars' -> f (rec_vars', quant_vars) t)

let rec make_type_unl : var_set * var_set -> typ -> unit =
  fun ((rec_vars, quant_vars) as vars) ->
    function
    | `Not_typed -> assert false
    | `Primitive _ | `Function _ | `Table _ | `End | `Application _ -> ()
    | `Record r | `Variant r -> make_row_unl vars r
    | `Alias (_, t) -> make_type_unl vars t
    | `ForAll (qs, t) -> make_type_unl (rec_vars, add_quantified_vars !qs quant_vars) t
    | `MetaTypeVar point -> make_point_unl make_type_unl vars point
    | `Dual s -> make_type_unl vars s
    | _ -> assert false
and make_field_unl vars =
  function
  | `Absent -> ()
  | `Present t -> make_type_unl vars t
  | `Var point -> make_point_unl make_field_unl vars point
and make_row_unl vars (fields, row_var, _) =
  make_point_unl make_row_unl vars row_var;
  FieldEnv.iter (fun _name -> make_field_unl vars) fields

let make_type_unl = make_type_unl (IntSet.empty, IntSet.empty)
let make_row_unl = make_row_unl (IntSet.empty, IntSet.empty)

(* session kind stuff *)

let is_session_point : (var_set -> 'a -> bool) -> var_set -> [< 'a meta_max_basis] point -> bool =
  fun f rec_vars point ->
    match Unionfind.find point with
    | `Closed
    | `Var (_, (_, `Session), _) -> true
    | `Var _ -> false
    | `Body t -> f rec_vars t
    | `Recursive (var, t) ->
      check_rec var rec_vars true (flip f t)

let rec is_session_type : var_set -> typ -> bool =
  fun rec_vars ->
    function
    | #session_type -> true
    | `Alias (_, t) -> is_session_type rec_vars t
    | `MetaTypeVar point -> is_session_point is_session_type rec_vars point
    | _ -> false

let rec is_session_field rec_vars =
  function
  | `Absent -> true
  | `Present t -> is_session_type rec_vars t
  | `Var point -> is_session_point is_session_field rec_vars point

let rec is_session_row rec_vars (fields, row_var, _) =
  let session_row_var = is_session_point is_session_row rec_vars row_var in
  let session_fields =
    FieldEnv.fold
      (fun _ f b -> b && is_session_field rec_vars f)
      fields
      true
  in
    session_row_var && session_fields


let is_sessionable_point : (var_set -> 'a -> bool) -> var_set -> [< 'a meta_max_basis] point -> bool =
  fun f rec_vars point ->
    match Unionfind.find point with
    | `Closed
    | `Var (_, (_, `Session), _)
    | `Var (_, (_, `Any),     `Flexible) -> true
    | `Var (_, (_, `Base),    `Rigid)
    | `Var (_, (_, `Any),     `Rigid)
    | `Var (_, (_, `Base),    `Flexible) -> false
    | `Body t -> f rec_vars t
    | `Recursive (var, t) ->
      check_rec var rec_vars true (flip f t)

let rec is_sessionable_type : var_set -> typ -> bool =
  fun rec_vars ->
    function
    | #session_type -> true
    | `Alias (_, t) -> is_sessionable_type rec_vars t
    | `MetaTypeVar point -> is_sessionable_point is_sessionable_type rec_vars point
    | _ -> false

let rec is_sessionable_field rec_vars =
  function
  | `Absent -> true
  | `Present t -> is_sessionable_type rec_vars t
  | `Var point -> is_sessionable_point is_sessionable_field rec_vars point

let rec is_sessionable_row rec_vars (fields, row_var, _) =
  let session_row_var = is_sessionable_point is_sessionable_row rec_vars row_var in
  let session_fields =
    FieldEnv.fold
      (fun _ f b -> b && is_sessionable_field rec_vars f)
      fields
      true
  in
    session_row_var && session_fields

(* precondition: point is sessionable *)
let sessionify_point : (var_set -> 'a -> unit) -> var_set -> [< 'a meta_max_basis] point -> unit =
  fun f rec_vars point ->
    match Unionfind.find point with
    | `Closed
    | `Var (_,   (_,   `Session), _)     -> ()
    | `Var (var, (lin, `Any), `Flexible) -> Unionfind.change point (`Var (var, (lin, `Session), `Flexible))
    | `Var _                             -> assert false
    | `Body t                            -> f rec_vars t
    | `Recursive (var, t)                -> check_rec var rec_vars () (flip f t)

let rec sessionify_type : var_set -> typ -> unit =
  fun rec_vars ->
    function
    | #session_type -> ()
    | `Alias (_, t) -> sessionify_type rec_vars t
    | `MetaTypeVar point -> sessionify_point sessionify_type rec_vars point
    | _ -> assert false

let rec sessionify_field rec_vars =
  function
  | `Absent -> ()
  | `Present t -> sessionify_type rec_vars t
  | `Var point -> sessionify_point sessionify_field rec_vars point

let rec sessionify_row rec_vars (fields, row_var, _) =
  sessionify_point sessionify_row rec_vars row_var;
  FieldEnv.iter
    (fun _ f -> sessionify_field rec_vars f)
    fields

let is_session_type = is_session_type IntSet.empty
let is_session_row = is_session_row IntSet.empty

let is_sessionable_type = is_sessionable_type IntSet.empty
let is_sessionable_row = is_sessionable_row IntSet.empty

let sessionify_type = sessionify_type IntSet.empty
let sessionify_row = sessionify_row IntSet.empty

type tyvar_wrapper_contents = [`Type of meta_type_var | `Row of meta_row_var | `Presence of meta_presence_var]
      deriving (Show)

type tyvar_wrapper = int * freedom * tyvar_wrapper_contents
      deriving (Show)

type datatype = typ

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

let type_var_number = var_of_quantifier

module Env = Env.String

(* Generation of fresh type variables *)
let type_variable_counter = ref 0
let fresh_raw_variable : unit -> int =
  function () ->
    incr type_variable_counter; !type_variable_counter

let get_variable_counter () = !type_variable_counter
let bump_variable_counter i = type_variable_counter := !type_variable_counter+i

(* type ops stuff *)
  let empty_field_env = FieldEnv.empty
  let closed_row_var = Unionfind.fresh `Closed

  let build_type_variable freedom var subkind = Unionfind.fresh (`Var (var, subkind, freedom))
  let make_type_variable var subkind = `MetaTypeVar (build_type_variable `Flexible var subkind)
  let make_rigid_type_variable var subkind = `MetaTypeVar (build_type_variable `Rigid var subkind)
  let make_row_variable = build_type_variable `Flexible
  let make_rigid_row_variable = build_type_variable `Rigid
  let make_presence_variable var subkind = `Var (build_type_variable `Flexible var subkind)
  let make_rigid_presence_variable var subkind = `Var (build_type_variable `Rigid var subkind)

  let is_closed_row : row -> bool =
    let rec is_closed rec_vars =
      function
        | (_, row_var, _) ->
            begin
              match Unionfind.find row_var with
                | `Closed -> true
                | `Var _ -> false
                | `Recursive (var, row) ->
                    ((TypeVarSet.mem var rec_vars)
                        || (is_closed (TypeVarSet.add var rec_vars) row))
                | `Body row ->
                    is_closed rec_vars row
            end
    in
      is_closed TypeVarSet.empty

  let is_empty_row ((fields, row_var, _) as row) =
    is_closed_row row && FieldEnv.is_empty fields

  let get_row_var : row -> int option = fun (_, row_var, _) ->
    let rec get_row_var' = fun rec_vars -> function
      | `Closed -> None
      | `Var (var, _, _) -> Some var
      | `Recursive (var, (_, row_var', _)) ->
          if TypeVarSet.mem var rec_vars then
            None
          else
            get_row_var' (TypeVarSet.add var rec_vars) (Unionfind.find row_var')
      | `Body (_, row_var', _) ->
          get_row_var' rec_vars (Unionfind.find row_var')
    in
      get_row_var' TypeVarSet.empty (Unionfind.find row_var)

  let fresh_type_variable subkind = make_type_variable (fresh_raw_variable ()) subkind
  let fresh_rigid_type_variable subkind = make_rigid_type_variable (fresh_raw_variable ()) subkind
  let fresh_row_variable subkind = make_row_variable (fresh_raw_variable ()) subkind
  let fresh_rigid_row_variable subkind = make_rigid_row_variable (fresh_raw_variable ()) subkind
  let fresh_session_variable linearity = make_type_variable (fresh_raw_variable ()) (linearity, `Session)

  let fresh_presence_variable subkind = make_presence_variable (fresh_raw_variable ()) subkind
  let fresh_rigid_presence_variable subkind = make_rigid_presence_variable (fresh_raw_variable ()) subkind

  let fresh_type_quantifier subkind : quantifier * datatype =
    let var = fresh_raw_variable () in
    let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
      (var, subkind, `Type point), `MetaTypeVar point

  let fresh_row_quantifier subkind : quantifier * row =
    let var = fresh_raw_variable () in
    let point = make_rigid_row_variable var subkind in
      (var, subkind, `Row point), (FieldEnv.empty, point, false)

  let fresh_presence_quantifier subkind : quantifier * field_spec =
    let var = fresh_raw_variable () in
    let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
      (var, subkind, `Presence point), `Var point

  let fresh_flexible_type_quantifier subkind : quantifier * datatype =
    let var = fresh_raw_variable () in
    let point = Unionfind.fresh (`Var (var, subkind, `Flexible)) in
      (var, subkind, `Type point), `MetaTypeVar point

  let fresh_flexible_row_quantifier subkind : quantifier * row =
    let var = fresh_raw_variable () in
    let point = make_row_variable var subkind in
      (var, subkind, `Row point), (FieldEnv.empty, point, false)

  let fresh_flexible_presence_quantifier subkind : quantifier * field_spec =
    let var = fresh_raw_variable () in
    let point = Unionfind.fresh (`Var (var, subkind, `Flexible)) in
      (var, subkind, `Presence point), `Var point

let freshen_quantifier =
  function
    | (_, subkind, `Type _) ->
        let q, t = fresh_type_quantifier subkind in
          q, `Type t
    | (_, subkind, `Row _) ->
        let q, row = fresh_row_quantifier subkind in
          q, `Row row
    | (_, subkind, `Presence _) ->
        let q, f = fresh_presence_quantifier subkind in
          q, `Presence f

let freshen_quantifier_flexible =
  function
    | (_, subkind, `Type _) ->
        let q, t = fresh_flexible_type_quantifier subkind in
          q, `Type t
    | (_, subkind, `Row _) ->
        let q, row = fresh_flexible_row_quantifier subkind in
          q, `Row row
    | (_, subkind, `Presence _) ->
        let q, f = fresh_flexible_presence_quantifier subkind in
          q, `Presence f

let make_empty_closed_row () = empty_field_env, closed_row_var, false
let make_empty_open_row subkind = empty_field_env, fresh_row_variable subkind, false

let make_singleton_closed_row (label, field_spec) =
  FieldEnv.add label field_spec empty_field_env, closed_row_var, false
let make_singleton_open_row (label, field_spec) subkind =
  FieldEnv.add label field_spec empty_field_env, fresh_row_variable subkind, false

let is_absent_from_row label (field_env, _, _ as row) =
  if FieldEnv.mem label field_env then
    FieldEnv.find label field_env = `Absent
  else
    is_closed_row row

let row_with (label, f : string * field_spec) (field_env, row_var, dual : row) =
  FieldEnv.add label f field_env, row_var, dual

(*** end of type_basis ***)

let unbox_quantifiers = (!)
let box_quantifiers = ref

(** Remove any redundant top-level `MetaTypeVars from a type.
    Additionally, collapse adjacent quantifiers. *)
let concrete_type rec_names t =
  let rec ct rec_names t : datatype =
    match t with
      | `MetaTypeVar point ->
          begin
            match Unionfind.find point with
              | `Body t ->
                  ct rec_names t
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
      | _ -> t
  in
    ct rec_names t

(** Hoist all top-level quantifiers to the top-level list, e.g.:

    forall a.forall b.forall c.(a) -> (b) -> c
    -->
    forall a,b,c.(a) -> (b) -> c
*)
let hoist_quantifiers =
  function
    | `ForAll (qsref, t) ->
        let rec hq =
          function
            | `MetaTypeVar point ->
                begin
                  match Unionfind.find point with
                    | `Body t -> hq t
                    | _ -> []
                end
            | `ForAll (qsref, t) ->
                let qs = !qsref in
                  qsref := [];
                  qs :: hq t
            | _ -> [] in

        let qss = hq t in
          qsref := List.concat (!qsref :: qss)
    | _ -> ()

(** remove any redundant top-level `Vars from a presence flag. *)
let rec concrete_field_spec f =
  match f with
    | `Var point ->
        begin
          match Unionfind.find point with
            | `Var _ -> f
            | `Body f -> concrete_field_spec f
        end
    | _ -> f

let free_type_vars, free_row_type_vars =
  let module S = TypeVarSet in
  let rec free_type_vars' : S.t -> datatype -> S.t = fun rec_vars ->
    function
      | `Not_typed               -> S.empty
      | `Primitive _             -> S.empty
      | `Function (f, m, t)      ->
          S.union_all [free_type_vars' rec_vars f; free_row_type_vars' rec_vars m; free_type_vars' rec_vars t]
      | `Lolli (f, m, t)         ->
          S.union_all [free_type_vars' rec_vars f; free_row_type_vars' rec_vars m; free_type_vars' rec_vars t]
      | `Record row
      | `Variant row             -> free_row_type_vars' rec_vars row
      | `Table (r, w, n)         ->
          S.union_all
            [free_type_vars' rec_vars r; free_type_vars' rec_vars w; free_type_vars' rec_vars n]
      | `Alias ((_, ts), datatype) ->
          S.union (S.union_all (List.map (free_tyarg_vars' rec_vars) ts)) (free_type_vars' rec_vars datatype)
      | `Application (_, datatypes) -> S.union_all (List.map (free_tyarg_vars' rec_vars) datatypes)
      | `ForAll (tvars, body)    -> S.diff (free_type_vars' rec_vars body)
                                           (List.fold_right (S.add -<- type_var_number) (unbox_quantifiers tvars) S.empty)
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
    ((fun t -> free_type_vars' S.empty t),
     (fun t -> free_row_type_vars' S.empty t))

type inference_type_map =
    ((datatype Unionfind.point) IntMap.t ref *
       (row Unionfind.point) IntMap.t ref)

let field_env_union : (field_spec_map * field_spec_map) -> field_spec_map =
  fun (env1, env2) ->
    FieldEnv.fold (fun label field_spec env' ->
                     FieldEnv.add label field_spec env') env1 env2

(* let contains_present_fields field_env = *)
(*   FieldEnv.fold *)
(*     (fun _ field_spec present -> *)
(*        match field_spec with *)
(*          | `Present, _ -> true *)
(*          | `Absent, _ -> present *)
(*     ) field_env false *)

let is_canonical_row_var row_var =
  match Unionfind.find row_var with
    | `Closed
    | `Var _ -> true
    | `Recursive _
    | `Body _ -> false

let is_rigid_row : row -> bool =
  let rec is_rigid rec_vars (_, row_var, _) =
    match Unionfind.find row_var with
      | `Closed
      | `Var (_, _, `Rigid) -> true
      | `Var (_, _, `Flexible) -> false
      | `Recursive (var, row) ->
          ((TypeVarSet.mem var rec_vars) || (is_rigid (TypeVarSet.add var rec_vars) row))
      | `Body row ->
          is_rigid rec_vars row
  in
    is_rigid TypeVarSet.empty

(* is_rigid_row_with_var var row
   returns true if row is rigid and has var as its row var
*)
let is_rigid_row_with_var : int -> row -> bool =
  fun var ->
    let rec is_rigid rec_vars (_, row_var, _) =
      match Unionfind.find row_var with
        | `Closed
        | `Var (_, _, `Flexible) -> false
        | `Var (var', _, `Rigid) -> var=var'
        | `Recursive (var', row) ->
            ((TypeVarSet.mem var' rec_vars) || (is_rigid (TypeVarSet.add var' rec_vars) row))
        | `Body row ->
            is_rigid rec_vars row
    in
      is_rigid TypeVarSet.empty


let is_flattened_row : row -> bool =
  let rec is_flattened =
    fun rec_vars (_, row_var, _) ->
      match Unionfind.find row_var with
        | `Closed
        | `Var _ -> true
        | `Body _ -> false
        | `Recursive (var, rec_row) ->
            if TypeVarSet.mem var rec_vars then true
            else is_flattened (TypeVarSet.add var rec_vars) rec_row
  in
    is_flattened TypeVarSet.empty

let is_empty_row : row -> bool =
  let rec is_empty = fun rec_vars -> fun (field_env, row_var, _) ->
    FieldEnv.is_empty field_env &&
      begin
        match Unionfind.find row_var with
          | `Closed
          | `Var _ -> true
          | `Recursive (var, _) when TypeVarSet.mem var rec_vars -> true
          | `Recursive (var, rec_row) -> is_empty (TypeVarSet.add var rec_vars) rec_row
          | `Body row -> is_empty rec_vars row
      end
  in
    is_empty TypeVarSet.empty

(*
  get rid of any `Body constructors inside a presence flag
*)
let normalise_field_spec = concrete_field_spec

let normalise_fields =
  FieldEnv.map normalise_field_spec

type var_map = (bool * meta_type_var) TypeVarMap.t 

let rec dual_type : var_map -> datatype -> datatype =
  fun rec_points ->
    let dt s = dual_type rec_points s in
    let sdt t = subst_dual_type rec_points t in
      function
      | `Input (t, s) -> `Output (sdt t, dt s)
      | `Output (t, s) -> `Input (sdt t, dt s)
      | `Select row -> `Choice (dual_row rec_points row)
      | `Choice row -> `Select (dual_row rec_points row)
      | `MetaTypeVar point ->
        begin
          match Unionfind.find point with
          | `Var _ -> `Dual (`MetaTypeVar point)
          | `Recursive (var, t) ->
            if TypeVarMap.mem var rec_points then
              `MetaTypeVar (snd (TypeVarMap.find var rec_points))
            else
              let var' = fresh_raw_variable () in
              let point = Unionfind.fresh (`Recursive (var', dummy_type)) in
                Unionfind.change point (`Recursive (var', dual_type (TypeVarMap.add var (true, point) rec_points) t));
                `MetaTypeVar point
          | `Body s -> dt s
        end
      | `Dual s ->
        (* TODO: is this correct? *)
        sdt s
      | `End -> `End
      (* it sometimes seems tempting to preserve aliases here, but it
         won't always work - e.g. when we use dual_type to expose a
         concrete type *)
      (* | `Alias _ as t         -> `Dual t *)
      (* Still, we might hope to find a way of preserving 'dual
         aliases' in order to simplify the pretty-printing of types... *)
      | `Alias (_, t) -> dt t
      | t -> raise (Invalid_argument ("Attempt to dualise non-session type: " ^ Show_typ.show t))
and dual_row : var_map -> row -> row =
  fun rec_points row ->
    let (fields, row_var, dual) = fst (unwrap_row row) in
    let fields' =
      StringMap.map (function
      | `Absent -> `Absent
      | `Present t ->
        `Present (dual_type rec_points t)
      | `Var _ -> (* TODO: what should happen here? *) assert false) fields
    in
      (fields', row_var, not dual)

and subst_dual_type : var_map -> datatype -> datatype =
  fun rec_points ->
    let sdt t = subst_dual_type rec_points t in
    let sdr r = subst_dual_row rec_points r in
      fun t ->
        match t with
        | `Not_typed
        | `Primitive _ -> t
        | `Function (f, m, t) -> `Function (sdt f, sdr m, sdt t)
        | `Lolli (f, m, t) -> `Lolli (sdt f, sdr m, sdt t)
        | `Record row -> `Record (sdr row)
        | `Variant row -> `Variant (sdr row)
        | `Table (r, w, n) -> `Table (sdt r, sdt w, sdt n)
        (* TODO: we could do a check to see if we can preserve aliases here *)
        | `Alias (_, t) -> sdt t
        | `Application (abs, ts) -> `Application (abs, List.map (subst_dual_type_arg rec_points) ts)
        | `ForAll (qs, body) -> `ForAll (qs, sdt body)
        | `MetaTypeVar point ->
          begin
            match Unionfind.find point with
            | `Var _ -> `MetaTypeVar point
            | `Recursive (var, t) ->
              if TypeVarMap.mem var rec_points then
                let (dual, point) = TypeVarMap.find var rec_points in
                  if dual then `Dual (`MetaTypeVar point)
                  else `MetaTypeVar point
              else
                let var' = fresh_raw_variable () in
                let point = Unionfind.fresh (`Recursive (var', dummy_type)) in
                  Unionfind.change point (`Recursive (var', subst_dual_type (TypeVarMap.add var (false, point) rec_points) t));
                  `MetaTypeVar point
            | `Body s -> sdt s
          end
        | `Input (t, s) -> `Input (sdt t, sdt s)
        | `Output (t, s) -> `Output (sdt t, sdt s)
        | `Select row -> `Select (sdr row)
        | `Choice row -> `Choice (sdr row)
        | `Dual s ->
          begin
            match sdt s with
            | `Dual s' -> s'
            | s' -> `Dual s'
          end
        | `End                  -> `End
and subst_dual_row : var_map -> row -> row =
  fun rec_points row ->
    let (fields, row_var, dual) = fst (unwrap_row row) in
    let fields' =
      StringMap.map
        (subst_dual_field_spec rec_points)
        fields
    in
      (fields', row_var, dual)
and subst_dual_field_spec : var_map -> field_spec -> field_spec =
  fun rec_points ->
    function
    | `Absent -> `Absent
    | `Present t -> `Present (subst_dual_type rec_points t)
    | `Var _ -> (* TODO: what should happen here? *) assert false
and subst_dual_type_arg : var_map -> type_arg -> type_arg =
  fun rec_points ->
    function
    | `Type t -> `Type (subst_dual_type rec_points t)
    | `Row row -> `Row (subst_dual_row rec_points row)
    | `Presence f -> `Presence (subst_dual_field_spec rec_points f)

(*
 convert a row to the form (field_env, row_var)
 where Unionfind.find row_var is of the form:
    `Closed
  | `Var var
  | `Recursive (var, body)
 *)
and flatten_row : row -> row = fun (field_env, row_var, dual) ->
  let dual_if r = if dual then dual_row TypeVarMap.empty r else r in
  let rec flatten_row' : meta_row_var IntMap.t -> row -> row =
    fun rec_env ((field_env, row_var, dual) as row) ->
      let row' =
        match Unionfind.find row_var with
          | `Closed
          | `Var _ -> row
          | `Recursive (var, rec_row) ->
              if IntMap.mem var rec_env then
                row
              else
                (let row_var' =
                   Unionfind.fresh (`Recursive (var, (FieldEnv.empty,
                                                      Unionfind.fresh (`Var (var, (`Any, `Any), `Flexible)),
                                                      false))) in
                 let rec_row' = flatten_row' (IntMap.add var row_var' rec_env) rec_row in
                   Unionfind.change row_var' (`Recursive (var, rec_row'));
                    field_env, row_var', dual)
          | `Body row' ->
              let field_env', row_var', dual = flatten_row' rec_env (dual_if row') in
                field_env_union (field_env, field_env'), row_var', dual
      in
        assert (is_flattened_row row');
        row' in
  let field_env, row_var, dual = flatten_row' IntMap.empty (field_env, row_var, dual) in
  let field_env = normalise_fields field_env in
    field_env, row_var, dual

(*
 As flatten_row except if the flattened row_var is of the form:

  `Recursive (var, body)

then it is unwrapped. This ensures that all the fields are exposed
in field_env.
 *)
and unwrap_row : row -> (row * row_var option) = fun (field_env, row_var, dual) ->
  let dual_if r = if dual then dual_row TypeVarMap.empty r else r in
  let rec unwrap_row' : meta_row_var IntMap.t -> row -> (row * row_var option) =
    fun rec_env ((field_env, row_var, dual) as row) ->
      let row' =
        match Unionfind.find row_var with
          | `Closed
          | `Var _ -> row, None
          | `Recursive (var, body) ->
              if IntMap.mem var rec_env then
                row, Some row_var
              else
                begin
                  let point =
                    Unionfind.fresh (`Recursive (var, body)) in
                  let unwrapped_body, _ = unwrap_row' (IntMap.add var point rec_env) body in
                    Unionfind.change point (`Recursive (var, unwrapped_body));
                    let field_env', row_var', dual' = unwrapped_body in
                      (field_env_union (field_env, field_env'), row_var', dual'), Some point
                end
          | `Body row' ->
              let (field_env', row_var', dual), rec_row = unwrap_row' rec_env (dual_if row') in
                (field_env_union (field_env, field_env'), row_var', dual), rec_row
      in
        assert (is_flattened_row (fst row'));
        row' in
  let (field_env, row_var, dual), rec_row = unwrap_row' IntMap.empty (field_env, row_var, dual) in
  let field_env = normalise_fields field_env in
    (field_env, row_var, dual), rec_row


let dual_type = dual_type TypeVarMap.empty
let dual_row = dual_row TypeVarMap.empty


(* TODO: tidy up all this normalisation / concretisation code *)
let rec normalise_datatype rec_names t =
  let nt = normalise_datatype rec_names in
  let nr = normalise_row rec_names in
    hoist_quantifiers t;
    match t with
      | `Not_typed
      | `Primitive _             -> t
      | `Function (f, m, t)      ->
          `Function (nt f, nr m, nt t)
      | `Lolli (f, m, t)         ->
           `Lolli (nt f, nr m, nt t)
      | `Record row              -> `Record (nr row)
      | `Variant row             -> `Variant (nr row)
      | `Table (r, w, n)         ->
          `Table (nt r, nt w, nt n)
      | `Alias ((name, ts), datatype) ->
          `Alias ((name, ts), nt datatype)
      | `Application (abs, datatypes) ->
          `Application (abs, List.map (normalise_type_arg rec_names) datatypes)
      | `ForAll (qs, body)    ->
          begin
            match unbox_quantifiers qs with
              | [] -> nt body
              | _ -> `ForAll (qs, nt body)
          end
      | `MetaTypeVar point       ->
          begin
            match Unionfind.find point with
              | `Var _ -> t
              | `Recursive (var, body) ->
                  if IntSet.mem var rec_names then
                    t
                  else
                    let body = normalise_datatype (IntSet.add var rec_names) body in
                      Unionfind.change point (`Recursive (var, body));
                      `MetaTypeVar point
              | `Body t -> nt t
          end
      | `Input (t, s)         -> `Input (nt t, nt s)
      | `Output (t, s)        -> `Output (nt t, nt s)
      | `Select r             -> `Select (nr r)
      | `Choice r             -> `Choice (nr r)
      | `Dual s               -> dual_type (nt s)
      | `End                  -> `End

and normalise_row rec_names row =
  (* WARNING:

     We cannot use unwrap_row here, as that would lead to
     non-termination.
  *)
  let fields, row_var, dual = flatten_row row in
  let fields =
    FieldEnv.map
      (fun f -> normalise_field_spec f)
      fields
  in
    (fields, row_var, dual)
and normalise_type_arg rec_names type_arg =
  match type_arg with
    | `Type t -> `Type (normalise_datatype rec_names t)
    | `Row row -> `Row (normalise_row rec_names row)
    | `Presence f -> `Presence (normalise_field_spec f)

let concrete_type = concrete_type IntSet.empty

let normalise_datatype = normalise_datatype IntSet.empty
let normalise_row = normalise_row IntSet.empty

(** building quantified types *)

let quantifiers_of_type_args =
  List.map
    (function
       | `Type (`MetaTypeVar point) ->
           begin
             match Unionfind.find point with
               | `Var (var, subkind, _) -> (var, subkind, `Type point)
               | _ -> assert false
           end
       | `Type _ -> assert false
       | `Row (fields, row_var, dual) ->
           assert (StringMap.is_empty fields);
           begin
             match Unionfind.find row_var with
               | `Var (var, subkind, _) -> (var, subkind, `Row row_var)
               | _ -> assert false
           end
       | `Presence (`Var point) ->
           begin
             match Unionfind.find point with
               | `Var (var, subkind, _) -> (var, subkind, `Presence point)
               | _ -> assert false
           end
       | `Presence _ -> assert false)

(* TODO: need to unwind `Body constructors *)
let is_rigid_quantifier q =
  let rigid point =
    match Unionfind.find point with
      | `Var (_, _, `Rigid) -> true
      | _ -> false
  in
    match q with
      | _, _, `Type     point -> rigid point
      | _, _, `Row      point -> rigid point
      | _, _, `Presence point -> rigid point

let is_instantiated_quantifier q =
  let is_concrete_type t =
    match concrete_type t with
      | `MetaTypeVar point ->
          begin
            match Unionfind.find point with
              | `Recursive _ -> true
              | _ -> false
          end
      | _ -> true in

  let is_concrete_row row =
    let (field_env, row_var, _) = flatten_row row in
      FieldEnv.is_empty field_env &&
        match Unionfind.find row_var with
          | `Recursive _
          | `Closed -> true
          | _ -> false in

  let is_concrete_field_spec f =
    match concrete_field_spec f with
      | `Var _ -> false
      | _ -> true
  in
    match q with
      | _, _, `Type point ->
          begin
            match Unionfind.find point with
              | `Recursive _ -> true
              | `Body t -> is_concrete_type t
              | _ -> false
          end
      | _, _, `Row point ->
          begin
            match Unionfind.find point with
              | `Recursive _ -> true
              | `Body row -> is_concrete_row row
              | _ -> false
          end
      | _, _, `Presence point ->
          begin
            match Unionfind.find point with
              | `Body f -> is_concrete_field_spec f
              | _ -> false
          end

(* update a quantifier with any changes to its point *)
let normalise_quantifier = fun q ->
  match q with
    | _, _, `Type point ->
        begin
          match Unionfind.find point with
            | `Var (var, subkind, _) -> (var, subkind, `Type point)
            | _ -> (* TODO: shouldn't this be an error? *) q
        end
    | _, _, `Row point ->
        begin
          match Unionfind.find point with
            | `Var (var, subkind, _) -> (var, subkind, `Row point)
            | _ -> (* TODO: shouldn't this be an error? *) q
        end
    | _, _, `Presence point ->
        begin
          match Unionfind.find point with
            | `Var (var, subkind, _) -> (var, subkind, `Presence point)
            | _ -> (* TODO: shouldn't this be an error? *) q
        end

let rec flexible_of_type t =
  match concrete_type t with
    | `MetaTypeVar point as t ->
        begin
          match Unionfind.find point with
            | `Var (_, _, `Flexible) -> Some t
            | _ -> None
        end
    | `ForAll (qs, t) when
        List.for_all (fun q -> not (is_rigid_quantifier q)) (unbox_quantifiers qs) ->
          begin
            match flexible_of_type t with
              | Some t ->
                  (* WARNING: side-effect! *)
                  qs := [];
                  Some t
              | None -> None
          end
    | _ -> None


let for_all : quantifier list * datatype -> datatype = fun (qs, t) ->
  concrete_type (`ForAll (box_quantifiers qs, t))

(* useful types *)
let unit_type = `Record (make_empty_closed_row ())
(* let string_type = `Alias (("String", []), (`Application (list, [`Type (`Primitive `Char)]))) *)
let string_type = `Primitive `String
let keys_type = `Application (list, [`Type (`Application (list, [`Type (string_type)]))])
let char_type = `Primitive `Char
let bool_type = `Primitive `Bool
let int_type = `Primitive `Int
let float_type = `Primitive `Float
let xml_type = `Alias (("Xml", []), `Application (list, [`Type (`Primitive `XmlItem)]))
let database_type = `Primitive `DB

(* precondition: the row is unwrapped *)
let is_tuple ?(allow_onetuples=false) (field_env, rowvar, _) =
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
                        | `Absent    -> false
                        | `Var _     -> false))
             (fromTo 1 n))
        in
          (* 0/1-tuples are displayed as records *)
          b && (allow_onetuples || n <> 1)
    | _ -> false

let extract_tuple (field_env, _, _) =
  FieldEnv.to_list (fun _ ->
                      function
                        | `Present t -> t
                        | `Absent
                        | `Var _ -> assert false) field_env

(* whether to display mailbox annotations on arrow types
   [NOTE]
      unused mailbox parameters are never shown
 *)
let show_mailbox_annotations = Settings.add_bool("show_mailbox_annotations", true, `User)

(* pretty-print type vars as raw numbers rather than letters *)
let show_raw_type_vars = Settings.add_bool("show_raw_type_vars", false, `User)




module Vars =
struct
  type flavour = [`Rigid | `Flexible | `Recursive]
  type kind = primary_kind
  type scope = [`Free | `Bound]
  type spec = flavour * kind * int

  type vars_list = (int * (flavour * kind * scope)) list

(*   let add var spec vars = *)
(*     match V.lookup var vars with *)
(*       | None -> V.add var spec vars *)
(*       | Some (flavour, kind, count) -> *)
(*           begin *)
(*             let (flavour', kind', count') = spec in *)
(* (\*              assert (flavour = flavour'); *)
(*               assert (kind = kind'); *)
(* *\)              V.add var (flavour, kind, count+count') vars *)
(*           end *)

(*   let union vars vars' = V.fold add vars' vars *)
(*   let union_all varss = List.fold_right union varss V.empty *)

  let varspec_of_tyvar q =
    let flavour = if is_rigid_quantifier q then
      `Rigid
    else
      `Flexible
    in
      var_of_quantifier q, (flavour, primary_kind_of_quantifier q, `Bound)

  (* find all free and bound type variables *)
  (*
    BUG: include_aliases probably shouldn't be an option here - it should always be true
  *)
  let rec free_bound_type_vars : include_aliases:bool -> TypeVarSet.t -> datatype -> vars_list = fun ~include_aliases bound_vars t ->
    let fbtv = free_bound_type_vars ~include_aliases bound_vars in
      match t with
        | `Not_typed -> []
        | `Primitive _ -> []
        | `MetaTypeVar point ->
            begin
              match Unionfind.find point with
                | `Var (var, _, freedom) ->
                      [var, ((freedom :> flavour), `Type, `Free)]
                | `Recursive (var, body) ->
                    if TypeVarSet.mem var bound_vars then
                      [var, (`Recursive, `Type, `Bound)]
                    else
                      (var, (`Recursive, `Type, `Bound))::(free_bound_type_vars ~include_aliases (TypeVarSet.add var bound_vars) body)
                | `Body t -> fbtv t
            end
        | `Function (f, m, t) ->
            (fbtv f) @ (free_bound_row_type_vars ~include_aliases bound_vars m) @ (fbtv t)
        | `Lolli (f, m, t) ->
            (fbtv f) @ (free_bound_row_type_vars ~include_aliases bound_vars m) @ (fbtv t)
        | `Record row
        | `Variant row -> free_bound_row_type_vars ~include_aliases bound_vars row
        | `Table (r, w, n) -> (fbtv r) @ (fbtv w) @ (fbtv n)
        | `ForAll (tyvars, body) ->
            let bound_vars, vars =
              List.fold_left
                (fun (bound_vars, vars) tyvar ->
                   let var, spec = varspec_of_tyvar tyvar in
                     TypeVarSet.add var bound_vars, (var, spec)::vars)
                (bound_vars, [])
                (unbox_quantifiers tyvars)
            in
              (List.rev vars) @ (free_bound_type_vars ~include_aliases bound_vars body)
        | `Alias ((_,ts), d) when include_aliases ->
            List.concat
              (List.map (free_bound_tyarg_vars ~include_aliases bound_vars) ts) @ (fbtv d)
        | `Alias (_, d) -> fbtv d
        | `Application (_, datatypes) -> List.concat (List.map (free_bound_tyarg_vars ~include_aliases bound_vars) datatypes)
        | `Input (t, s)
        | `Output (t, s) ->
           free_bound_type_vars ~include_aliases bound_vars t @ free_bound_type_vars ~include_aliases bound_vars s
        | `Select row
        | `Choice row -> free_bound_row_type_vars ~include_aliases bound_vars row
        | `Dual s -> free_bound_type_vars ~include_aliases bound_vars s
        | `End -> []
  and free_bound_field_spec_type_vars ~include_aliases bound_vars =
    function
      | `Present t -> free_bound_type_vars ~include_aliases bound_vars t
      | `Absent -> []
      | `Var point ->
          begin
            match Unionfind.find point with
              | `Var (var, _, freedom) ->
                    [var, ((freedom :> flavour), `Presence, `Free)]
              | `Body f -> free_bound_field_spec_type_vars ~include_aliases bound_vars f
          end
  and free_bound_row_type_vars ~include_aliases bound_vars (field_env, row_var, _) =
    let field_type_vars =
      FieldEnv.fold
        (fun name f tvs ->
           tvs @ free_bound_field_spec_type_vars ~include_aliases bound_vars f)
        field_env [] in
    let row_var = free_bound_row_var_vars ~include_aliases bound_vars row_var in
      field_type_vars @ row_var
  and free_bound_row_var_vars ~include_aliases bound_vars row_var =
    match Unionfind.find row_var with
      | `Closed -> []
      | `Var (var, _, freedom) ->
            [var, ((freedom :> flavour), `Row, `Free)]
      | `Recursive (var, row) ->
          if TypeVarSet.mem var bound_vars then
            [var, (`Recursive, `Row, `Bound)]
          else
            (var, (`Recursive, `Row, `Bound))::(free_bound_row_type_vars ~include_aliases (TypeVarSet.add var bound_vars) row)
      | `Body row -> free_bound_row_type_vars ~include_aliases bound_vars row
  and free_bound_tyarg_vars ~include_aliases bound_vars =
    function
      | `Type t -> free_bound_type_vars ~include_aliases bound_vars t
      | `Row row -> free_bound_row_type_vars ~include_aliases bound_vars row
      | `Presence f -> free_bound_field_spec_type_vars ~include_aliases bound_vars f

  let free_bound_tycon_vars ~include_aliases bound_vars tycon_spec =
    match tycon_spec with
      | `Alias (tyvars, body) ->
          let bound_vars, vars =
            List.fold_left
              (fun (bound_vars, vars) tyvar ->
                 let var, spec = varspec_of_tyvar tyvar in
                   (TypeVarSet.add var bound_vars, (var, spec)::vars)) (bound_vars, []) tyvars
          in
            (List.rev vars) @ (free_bound_type_vars ~include_aliases bound_vars body)
      | `Abstract _ -> []



(*   let varset_of_vars vars = *)
(*     V.fold (fun var _ varset -> TypeVarSet.add var varset) vars TypeVarSet.empty *)

  let init (var, (flavour, kind, scope)) name =
    match scope with
      | `Free -> (name, (flavour, kind, 1))
      | `Bound -> (name, (flavour, kind, 0))

  let combine (name, (flavour, kind, count)) (flavour', kind', scope) =
(*     assert (flavour = flavour'); *)
(*     assert (kind = kind'); *)
    match scope with
      | `Free -> (name, (flavour, kind, count+1))
      | `Bound -> (name, (flavour, kind, count))

  let make_names vars =
    if Settings.get_value show_raw_type_vars then
      List.fold_left
        (fun name_map (var, spec) ->
           match IntMap.lookup var name_map with
             | None -> IntMap.add var (init (var, spec) (string_of_int var)) name_map
             | Some (name, spec') -> IntMap.add var (combine (name, spec') spec) name_map)
        IntMap.empty vars
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
               else "") in

        let (_, name_map) =
          List.fold_left
            (fun (n, name_map) (var, spec) ->
               match IntMap.lookup var name_map with
                 | None -> (n+1, IntMap.add var (init (var, spec) (num_to_letters n)) name_map)
                 | Some (name, spec') -> (n, IntMap.add var (combine (name, spec') spec) name_map))
            (0, IntMap.empty) vars
        in
          name_map
      end

  let find var = fst -<- (IntMap.find var)

  let find_spec = IntMap.find
end

(** Type printers *)

module Print =
struct
  let show_quantifiers = Settings.add_bool ("show_quantifiers", false, `User)
  let show_flavours = Settings.add_bool ("show_flavours", false, `User)
  let show_kinds = Settings.add_string ("show_kinds", "default", `User)
  let hide_fresh_type_vars = Settings.add_bool ("hide_fresh_type_vars", true, `User)

  (* Set the quantifiers to be true to display any outer quantifiers.
     Set flavours to be true to distinguish flexible type variables
     from rigid type variables. *)
  type policy = {quantifiers:bool; flavours:bool; hide_fresh:bool; kinds:string}
  type names = (string * Vars.spec) IntMap.t

  let default_policy () =
    {quantifiers=Settings.get_value show_quantifiers;
     flavours=Settings.get_value show_flavours;
     hide_fresh=Settings.get_value hide_fresh_type_vars;
     kinds=Settings.get_value show_kinds}

  let primitive : primitive -> string = function
    | `Bool -> "Bool"  | `Int -> "Int"  | `Char -> "Char"  | `Float   -> "Float"
    | `XmlItem -> "XmlItem" | `DB -> "Database" | `String -> "String"

  let has_kind =
    function
    | "" -> ""
    | s -> "::" ^ s

  let subkind : (policy * names) -> subkind -> string =
    let linearity = function
      | `Any -> "Any"
      | `Unl -> "Unl" in
    let restriction = function
      | `Any -> "Any"
      | `Base -> "Base"
      | `Session -> "Session" in
    let full (l, r) = "(" ^ linearity l ^ "," ^ restriction r ^ ")" in

    fun (policy, _vars) ->
    if policy.kinds = "full" then
      full
    else if policy.kinds = "hide" then
      function (_, _) -> ""
    else
      function
      | (`Unl, `Any) -> ""
      | (`Any, `Any) -> "Any"
      | (`Unl, `Base) -> restriction `Base
      | (`Any, `Session) -> restriction `Session
      | (l, r) -> full (l, r)

  let kind : (policy * names) -> kind -> string =
    let primary_kind = function
      | `Type -> "Type"
      | `Row -> "Row"
      | `Presence -> "Presence" in
    let restriction = function
      | `Any -> "Any"
      | `Base -> "Base"
      | `Session -> "Session" in
    let full (policy, _vars) (k, sk) =
      primary_kind k ^ subkind (policy, _vars) sk in
    fun (policy, _vars) (k, sk) ->
    if policy.kinds = "full" then
      full (policy, _vars) (k, sk)
    else if policy.kinds = "hide" then
      primary_kind k
    else
      match (k, sk) with
      | `Type, (`Unl, `Any) -> ""
      | `Type, (`Unl, `Base) -> restriction `Base
      | `Type, (`Any, `Session) -> restriction `Session
      | `Type, sk -> subkind ({policy with kinds="full"}, _vars) sk
      | `Row, (`Unl, `Any) -> primary_kind `Row
      | `Presence, (`Unl, `Any) -> primary_kind `Presence
      | `Row, _
      | `Presence, _ -> full ({policy with kinds="full"}, _vars) (k, sk)

  let quantifier : (policy * names) -> quantifier -> string =
    fun (policy, vars) q ->
      let k = kind_of_quantifier q in
      let prefix =
        if not(policy.flavours) || is_rigid_quantifier q then
          ""
        else
          "%"
      in
        prefix ^ Vars.find (var_of_quantifier q) vars ^ has_kind (kind (policy, vars) k)

  let rec datatype : TypeVarSet.t -> policy * names -> datatype -> string =
    fun bound_vars ((policy, vars) as p) t ->
      let sd = datatype bound_vars p in
      let sk k = has_kind (subkind p k) in

      let hide_fresh_check var (flavour, _, count) =
        policy.hide_fresh && count = 1 &&
        ((flavour = `Flexible && not (policy.flavours)) || not (IntSet.mem var bound_vars)) in

      let unwrap = fst -<- unwrap_row in
        (* precondition: the row is unwrapped *)
      let string_of_tuple (field_env, row_var, _) =
        let tuple_env =
          FieldEnv.fold
            (fun i f tuple_env ->
               match f with
                 | `Present t         -> IntMap.add (int_of_string i) t tuple_env
                 | (`Absent | `Var _) -> assert false)
            field_env
            IntMap.empty in
        let ss = List.rev (IntMap.fold (fun _ t ss -> (sd t) :: ss) tuple_env []) in
          "(" ^ String.concat ", " ss ^  ")"
      in
        match t with
          | `Not_typed       -> "not typed"
          | `Primitive p     -> primitive p
          | `MetaTypeVar point ->
              begin
                match Unionfind.find point with
                  | `Var (var, k, `Flexible) when policy.flavours ->
                      let name, spec = Vars.find_spec var vars in
                        (if hide_fresh_check var spec then
                           "%"
                         else
                           "%" ^ name) ^ sk k
                  | `Var (var, k, _) ->
                      let name, spec = Vars.find_spec var vars in
                        (if hide_fresh_check var spec then
                           "_"
                         else
                           name) ^ sk k
                  | `Recursive (var, body) ->
                      if TypeVarSet.mem var bound_vars then
                        Vars.find var vars
                      else
                        "mu " ^ Vars.find var vars ^ " . " ^
                          datatype (TypeVarSet.add var bound_vars) p body
                  | `Body t -> sd t
              end
          | `Function (args, effects, t) ->
              let arrow =
                let (fields, row_var, false) = unwrap effects in
                  if FieldEnv.is_empty fields then
                    match Unionfind.find row_var with
                      | `Closed -> "{}->"
                      | `Var (var, _, `Flexible) when policy.flavours ->
                          let name, spec = Vars.find_spec var vars in
                            if hide_fresh_check var spec then
                              "-%->"
                            else
                              "-%" ^ name ^ "->"
                      | `Var (var, _, _) ->
                          let name, spec = Vars.find_spec var vars in
                            if hide_fresh_check var spec then
                              "->"
                            else
                              "-" ^ name ^ "->"
                      | `Recursive _ -> assert false
                      | `Body t' ->
                          sd (`Function (args, t', t))
                  else if
                    (FieldEnv.mem "wild" fields &&
                       is_present (FieldEnv.find "wild" fields) &&
                        FieldEnv.size fields = 1)
                  then
                    match Unionfind.find row_var with
                      | `Closed -> "{}~>"
                      | `Var (var, _, `Flexible) when policy.flavours ->
                          let name, spec = Vars.find_spec var vars in
                            if hide_fresh_check var spec then
                              "~%~>"
                            else
                              "~%" ^ name ^ "~>"
                      | `Var (var, _, _) ->
                          let name, spec = Vars.find_spec var vars in
                            if hide_fresh_check var spec then
                              "~>"
                            else
                              "~" ^ name ^ "~>"
                      | `Recursive _ -> assert false
                      | `Body t' ->
                          sd (`Function (args, t', t))
                  else if
                    (FieldEnv.mem "hear" fields &&
                       FieldEnv.mem "wild" fields &&
                       is_present (FieldEnv.find "hear" fields) &&
                        is_present (FieldEnv.find "wild" fields) &&
                        FieldEnv.size fields = 2)
                  then
                    let ht =
                      match FieldEnv.find "hear" fields with
                      | `Present t -> sd t
                      | _          -> assert false
                    in
                      match Unionfind.find row_var with
                        | `Closed ->
                            "{:" ^ ht ^ "}~>"
                        | `Var (var, _, `Flexible) when policy.flavours ->
                            let name, spec = Vars.find_spec var vars in
                              if hide_fresh_check var spec then
                                "{:" ^ ht ^ "|%}~>"
                              else
                                "{:" ^ ht ^ "|%" ^ name ^ "}~>"
                        | `Var (var, _, _) ->
                            let name, spec = Vars.find_spec var vars in
                              if hide_fresh_check var spec then
                                "{:" ^ ht ^ "|_}~>"
                              else
                                "{:" ^ ht ^ "|" ^ name ^ "}~>"
                        | `Recursive _ -> assert false
                        | `Body t' ->
                            sd (`Function (args, t', t))
                  else
                      (* to guarantee termination it's crucial that we
                         invoke row on the original wrapped version of
                         the effect row *)
                      "{" ^ row "," bound_vars p effects ^ "}->"
              in
                begin match concrete_type args with
                  | `Record row when is_tuple ~allow_onetuples:true row ->
                    string_of_tuple row ^ " " ^arrow ^ " " ^ sd t
                  | t' -> assert false (* "*" ^ sd t' ^ " " ^arrow ^ " " ^ sd t *)
                end
          | `Lolli (args, effects, t) ->
              let arrow =
                let (fields, row_var, false) = unwrap effects in
                  if FieldEnv.is_empty fields then
                    match Unionfind.find row_var with
                      | `Closed -> "{}-@"
                      | `Var (var, _, `Flexible) when policy.flavours ->
                          let name, spec = Vars.find_spec var vars in
                            if hide_fresh_check var spec then
                              "-%-@"
                            else
                              "-%" ^ name ^ "-@"
                      | `Var (var, _, _) ->
                          let name, spec = Vars.find_spec var vars in
                            if hide_fresh_check var spec then
                              "-@"
                            else
                              "-" ^ name ^ "-@"
                      | `Recursive _ -> assert false
                      | `Body t' ->
                          sd (`Lolli (args, t', t))
                  else if
                    (FieldEnv.mem "wild" fields &&
                       is_present (FieldEnv.find "wild" fields) &&
                        FieldEnv.size fields = 1)
                  then
                    match Unionfind.find row_var with
                      | `Closed -> "{}~@"
                      | `Var (var, _, `Flexible) when policy.flavours ->
                          let name, spec = Vars.find_spec var vars in
                            if hide_fresh_check var spec then
                              "~%~@"
                            else
                              "~%" ^ name ^ "~@"
                      | `Var (var, _, _) ->
                          let name, spec = Vars.find_spec var vars in
                            if hide_fresh_check var spec then
                              "~@"
                            else
                              "~" ^ name ^ "~@"
                      | `Recursive _ -> assert false
                      | `Body t' ->
                          sd (`Lolli (args, t', t))
                  else if
                    (FieldEnv.mem "hear" fields &&
                       FieldEnv.mem "wild" fields &&
                       is_present (FieldEnv.find "hear" fields) &&
                        is_present (FieldEnv.find "wild" fields) &&
                        FieldEnv.size fields = 2)
                  then
                    let ht = sd
                      (match FieldEnv.find "hear" fields with
                      | `Present t -> t
                      | (`Absent | `Var _) -> assert false) in
                      match Unionfind.find row_var with
                        | `Closed ->
                            "{:" ^ ht ^ "}~@"
                        | `Var (var, _, `Flexible) when policy.flavours ->
                            let name, spec = Vars.find_spec var vars in
                              if hide_fresh_check var spec then
                                "{:" ^ ht ^ "|%}~@"
                              else
                                "{:" ^ ht ^ "|%" ^ name ^ "}~@"
                        | `Var (var, _, _) ->
                            let name, spec = Vars.find_spec var vars in
                              if hide_fresh_check var spec then
                                "{:" ^ ht ^ "|_}~@"
                              else
                                "{:" ^ ht ^ "|" ^ name ^ "}~@"
                        | `Recursive _ -> assert false
                        | `Body t' ->
                            sd (`Lolli (args, t', t))
                  else
                      "{" ^ row "," bound_vars p effects ^ "}-@"
              in
                begin match concrete_type args with
                  | `Record row when is_tuple ~allow_onetuples:true row ->
                    string_of_tuple row ^ " " ^arrow ^ " " ^ sd t
                  | t' -> assert false (* "*" ^ sd t' ^ " " ^arrow ^ " " ^ sd t *)
                end
          | `Record r ->
              let ur = unwrap r in
                (if is_tuple ur then string_of_tuple r
                 else "(" ^ row "," bound_vars p r ^ ")")
          | `Variant r -> "[|" ^ row "|" bound_vars p r ^ "|]"
          | `ForAll (tyvars, body) ->
              let tyvars = unbox_quantifiers tyvars in
              let bound_vars =
                List.fold_left
                  (fun bound_vars tyvar ->
                     TypeVarSet.add (var_of_quantifier tyvar) bound_vars)
                  bound_vars tyvars
              in
                if not (policy.flavours) then
                  let tyvars = List.filter is_rigid_quantifier tyvars in
                    match tyvars with
                      | [] -> datatype bound_vars p body
                      | _ ->
                          "forall "^ mapstrcat "," (quantifier p) tyvars ^"."^ datatype bound_vars p body
                else
                  "forall "^ mapstrcat "," (quantifier p) tyvars ^"."^ datatype bound_vars p body
          | `Input (t, s) -> "?(" ^ datatype bound_vars p t ^ ")." ^ datatype bound_vars p s
          | `Output (t, s) -> "!(" ^ datatype bound_vars p t ^ ")." ^ datatype bound_vars p s
          | `Select bs -> "[+|" ^ row "," bound_vars p bs ^ "|+]"
          | `Choice bs -> "[&|" ^ row "," bound_vars p bs ^ "|&]"
          | `Dual s -> "~" ^ datatype bound_vars p s
          | `End -> "End"
          | `Table (r, w, n)   ->
             (* TODO:

                pretty-print this using constraints?
             *)
             "TableHandle(" ^
               datatype bound_vars p r ^ "," ^
               datatype bound_vars p w ^ "," ^
               datatype bound_vars p n ^ ")"
                (*
                  QUESTION:

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
          | `Alias ((s,ts), _) ->  s ^ " ("^ String.concat "," (List.map (type_arg bound_vars p) ts) ^")"
          | `Application (l, [elems]) when Abstype.Eq_t.eq l list ->  "["^ (type_arg bound_vars p) elems ^"]"
          | `Application (s, []) -> Abstype.name s
          | `Application (s, ts) -> Abstype.name s ^ " ("^ String.concat "," (List.map (type_arg bound_vars p) ts) ^")"

  and presence bound_vars ((policy, vars) as p) =
    function
      | `Present t ->
        begin
          match concrete_type t with
          | `Record row when is_empty_row row -> ""
          | _                                 -> ":" ^ datatype bound_vars p t
        end
      | `Absent -> "-"
      | `Var point ->
          begin
            match Unionfind.find point with
              | `Var (var, _, `Flexible) when policy.flavours ->
                  let name, (_, _, count) = Vars.find_spec var vars in
                    if policy.hide_fresh && count = 1 && not (IntSet.mem var bound_vars) then "{%}"
                    else "{%" ^ name ^ "}"
              | `Var (var, _, _) ->
                  let name, (_, _, count) = Vars.find_spec var vars in
                    if policy.hide_fresh && count = 1 && not (IntSet.mem var bound_vars) then "{_}"
                    else "{" ^ name ^ "}"
              | `Body f ->
                  presence bound_vars p f
          end

  and row sep bound_vars ((policy, vars) as p) (field_env, rv, dual) =
    (* FIXME:

       should quote labels when necessary, i.e., when they
       contain non alpha-numeric characters
    *)
    let field_strings =
      FieldEnv.fold
        (fun label f field_strings ->
          (label ^ presence bound_vars p f) :: field_strings)
        field_env [] in

    let row_var_string = row_var sep bound_vars p rv in
      String.concat sep (List.rev (field_strings)) ^
        begin
          match row_var_string with
            | None -> ""
            | Some s -> "|"^ (if dual then "~" else "") ^ s
        end
  and row_var sep bound_vars ((policy, vars) as p) rv =
    match Unionfind.find rv with
      | `Closed -> None
      | `Var (var, k, `Flexible) when policy.flavours ->
          let name, (_, _, count) = Vars.find_spec var vars in
            Some
              ((if policy.hide_fresh && count = 1 && not (IntSet.mem var bound_vars) then "%"
                else ("%" ^ name)) ^ has_kind (subkind p k))
      | `Var (var, k, _) ->
          let name, (_, _, count) = Vars.find_spec var vars in
            Some
              ((if policy.hide_fresh && count = 1 && not (IntSet.mem var bound_vars) then "_"
                else name) ^ has_kind (subkind p k))
      | `Recursive (var, r) ->
          if TypeVarSet.mem var bound_vars then
            Some (Vars.find var vars)
          else
            Some ("(mu " ^ Vars.find var vars ^ " . " ^
                    row sep (TypeVarSet.add var bound_vars) p r ^ ")")
      | `Body r -> Some (row sep bound_vars p r)

  and type_arg bound_vars p =
    function
      | `Type t -> datatype bound_vars p t
      | `Row r -> "{ " ^ row "," bound_vars p r ^ " }"
      | `Presence f -> "::Presence (" ^ presence bound_vars p f ^ ")"

  let tycon_spec bound_vars p =
    function
      | `Alias (tyvars, body) ->
          let bound_vars =
            List.fold_left
              (fun bound_vars tyvar ->
                 TypeVarSet.add (var_of_quantifier tyvar) bound_vars)
              bound_vars tyvars
          in
            begin
              match tyvars with
                | [] -> datatype bound_vars p body
                | _ -> mapstrcat "," (quantifier p) tyvars ^"."^ datatype bound_vars p body
            end
      | `Abstract _ -> "abstract"

  let strip_quantifiers =
    function
      | `ForAll (_, t)
      | t -> t
end

(*
  find all the flexible type variables in a type
 *)
let rec flexible_type_vars : TypeVarSet.t -> datatype -> quantifier TypeVarMap.t = fun bound_vars t ->
  let ftv = flexible_type_vars bound_vars in
    match t with
      | `Not_typed
      | `Primitive _ -> TypeVarMap.empty
      | `MetaTypeVar point ->
          begin
            match Unionfind.find point with
              | `Var (var, _, `Flexible) when TypeVarSet.mem var bound_vars -> TypeVarMap.empty
              | `Var (var, subkind, `Flexible) -> TypeVarMap.singleton var (var, subkind, `Type point)
              | `Var (_, _, `Rigid) -> TypeVarMap.empty
              | `Recursive (var, body) ->
                  if TypeVarSet.mem var bound_vars then
                    TypeVarMap.empty
                  else
                    flexible_type_vars (TypeVarSet.add var bound_vars) body
              | `Body t -> ftv t
          end
      | `Function (f, m, t) ->
          TypeVarMap.union_all [ftv f; row_flexible_type_vars bound_vars m; ftv t]
      | `Lolli (f, m, t) ->
          TypeVarMap.union_all [ftv f; row_flexible_type_vars bound_vars m; ftv t]
      | `Record row -> row_flexible_type_vars bound_vars row
      | `ForAll (tyvars, body) ->
          let bound_vars =
            List.fold_left
              (fun bound_vars tyvar ->
                 let var = var_of_quantifier tyvar in
                   TypeVarSet.add var bound_vars)
                bound_vars
                (unbox_quantifiers tyvars)
          in
            flexible_type_vars bound_vars body
      | `Variant row -> row_flexible_type_vars bound_vars row
      | `Table (r, w, n) -> TypeVarMap.union_all [ftv r; ftv w; ftv n]
      | `Alias ((name, ts), d) ->
          TypeVarMap.union_all
            ((ftv d)::(List.map (tyarg_flexible_type_vars bound_vars) ts))
      | `Application (name, datatypes) ->
          TypeVarMap.union_all (List.map (tyarg_flexible_type_vars bound_vars) datatypes)
      | `Input (t, s)
      | `Output (t, s) -> TypeVarMap.union_all [flexible_type_vars bound_vars t; flexible_type_vars bound_vars s]
      | `Select row
      | `Choice row -> row_flexible_type_vars bound_vars row
      | `Dual s -> flexible_type_vars bound_vars s
      | `End -> TypeVarMap.empty
and presence_flexible_type_vars bound_vars =
  function
    | `Present t -> flexible_type_vars bound_vars t
    | `Absent -> TypeVarMap.empty
    | `Var point ->
        begin
          match Unionfind.find point with
            | `Var (var, _, `Flexible) when TypeVarSet.mem var bound_vars -> TypeVarMap.empty
            | `Var (var, subkind, `Flexible) -> TypeVarMap.singleton var (var, subkind, `Presence point)
            | `Var (var, _, `Rigid) -> TypeVarMap.empty
            | `Body f -> presence_flexible_type_vars bound_vars f
        end

and row_flexible_type_vars bound_vars (field_env, row_var, _) =
  TypeVarMap.superimpose
    (FieldEnv.fold
       (fun _ f ftvs ->
          TypeVarMap.union_all
            [presence_flexible_type_vars bound_vars f;
             ftvs])
       field_env TypeVarMap.empty)
    (row_var_flexible_type_vars bound_vars row_var)
and row_var_flexible_type_vars bound_vars row_var =
  match Unionfind.find row_var with
    | `Closed -> TypeVarMap.empty
    | `Var (var, _, `Flexible) when TypeVarSet.mem var bound_vars -> TypeVarMap.empty
    | `Var (var, subkind, `Flexible) -> TypeVarMap.singleton var (var, subkind, `Row row_var)
    | `Var (_, _, `Rigid) -> TypeVarMap.empty
    | `Recursive (var, row) ->
        if TypeVarSet.mem var bound_vars then
          TypeVarMap.empty
        else
          row_flexible_type_vars (TypeVarSet.add var bound_vars) row
    | `Body row ->
        row_flexible_type_vars bound_vars row
and tyarg_flexible_type_vars bound_vars =
  function
    | `Type t -> flexible_type_vars bound_vars t
    | `Row row -> row_flexible_type_vars bound_vars row
    | `Presence f -> presence_flexible_type_vars bound_vars f

let free_bound_type_vars ?(include_aliases=true) = Vars.free_bound_type_vars ~include_aliases TypeVarSet.empty
let free_bound_row_type_vars ?(include_aliases=true) = Vars.free_bound_row_type_vars ~include_aliases TypeVarSet.empty
let free_bound_field_spec_type_vars ?(include_aliases=true) = Vars.free_bound_field_spec_type_vars ~include_aliases TypeVarSet.empty
let free_bound_type_arg_type_vars ?(include_aliases=true) = Vars.free_bound_tyarg_vars ~include_aliases TypeVarSet.empty
let free_bound_row_var_vars ?(include_aliases=true) = Vars.free_bound_row_var_vars ~include_aliases TypeVarSet.empty

let free_bound_tycon_type_vars ?(include_aliases=true) = Vars.free_bound_tycon_vars ~include_aliases TypeVarSet.empty

(* string conversions *)
let string_of_datatype ?(policy=Print.default_policy) (t : datatype) =
  let policy = policy () in
  let t =
    if policy.Print.quantifiers then t
    else Print.strip_quantifiers t
  in
    Print.datatype
      TypeVarSet.empty
      (policy, Vars.make_names (free_bound_type_vars ~include_aliases:true t))
      t

let string_of_row ?(policy=Print.default_policy) row =
  Print.row "," TypeVarSet.empty
    (policy (), Vars.make_names (free_bound_row_type_vars ~include_aliases:true row))
    row

let string_of_presence ?(policy=Print.default_policy) (f : field_spec) =
  let policy = policy () in
    Print.presence
      TypeVarSet.empty
      (policy, Vars.make_names (free_bound_field_spec_type_vars ~include_aliases:true f))
      f

let string_of_type_arg ?(policy=Print.default_policy) (arg : type_arg) =
  let policy = policy () in
    Print.type_arg
      TypeVarSet.empty
      (policy, Vars.make_names (free_bound_type_arg_type_vars ~include_aliases:true arg))
      arg

let string_of_row_var ?(policy=Print.default_policy) row_var =
  match
    Print.row_var "," TypeVarSet.empty
      (policy (), Vars.make_names (free_bound_row_var_vars ~include_aliases:true row_var))
      row_var
  with
      | None -> ""
      | Some s -> s

let string_of_tycon_spec ?(policy=Print.default_policy) (tycon : tycon_spec) =
  let policy = policy () in
    Print.tycon_spec
      TypeVarSet.empty
      (policy, Vars.make_names (free_bound_tycon_type_vars ~include_aliases:true tycon))
      tycon

(* TODO: we need a way of choosing consistent names for a collection
   of types (and rows, type_args, etc.) in order to give adequate
   feedback to the programmer.

   Some options:

     - update a global mapping on demand

     - use a monad

     - build a message data structure and traverse in one go to
       generate a consistent set of names
*)


   (* HACK:

   Just use the default policy. At some point we might want to export
   the printing policy in types.mli.
 *)
let string_of_datatype t = string_of_datatype t
let string_of_row r = string_of_row r
let string_of_presence f = string_of_presence f
let string_of_type_arg arg = string_of_type_arg arg
let string_of_row_var r = string_of_row_var r
let string_of_tycon_spec s = string_of_tycon_spec s

module Show_datatype =
  Deriving_Show.Defaults
    (struct
      type a = datatype
      let format fmt a =
        Format.pp_print_string fmt (string_of_datatype a)
     end)

module Show_tycon_spec =
  Deriving_Show.Defaults
    (struct
      type a = tycon_spec
      let format fmt a =
        Format.pp_print_string fmt (string_of_tycon_spec a)
     end)

type environment        = datatype Env.t
and tycon_environment  = tycon_spec Env.t
and typing_environment = { var_env   : environment ;
                           tycon_env : tycon_environment ;
                           effect_row : row }
    deriving (Show)

let normalise_typing_environment env =
  { env with
      var_env = Env.map normalise_datatype env.var_env;
      (* what about tycon_env? *)
      effect_row = normalise_row env.effect_row }

(* Functions on environments *)
let extend_typing_environment
    {var_env = l ; tycon_env = al ; effect_row = _el }
    {var_env = r ; tycon_env = ar ; effect_row = er } : typing_environment =
  {var_env = Env.extend l r ; tycon_env = Env.extend al ar ; effect_row = er }

let string_of_environment = Show_environment.show

let string_of_typing_environment {var_env=env} = string_of_environment env

let make_fresh_envs : datatype -> datatype IntMap.t * row IntMap.t * field_spec IntMap.t =
  let module S = IntSet in
  let module M = IntMap in
  let empties = M.empty, M.empty, M.empty in
  let union =
    let union_one a b = M.fold M.add a b in
    let union_three (x, y, z) (x', y', z') = (union_one x x', union_one y y', union_one z z') in
      List.fold_left union_three empties in
  let rec make_env boundvars = function
      | `Not_typed
      | `Primitive _             -> empties
      | `Function (f, m, t)      -> union [make_env boundvars f; make_env_r boundvars m; make_env boundvars t]
      | `Lolli (f, m, t)         -> union [make_env boundvars f; make_env_r boundvars m; make_env boundvars t]
      | `Record row
      | `Variant row             -> make_env_r boundvars row
      | `Table (r, w, n)         -> union [make_env boundvars r; make_env boundvars w; make_env boundvars n]
      | `Alias ((name, ts), d)   -> union (List.map (make_env_ta boundvars) ts @ [make_env boundvars d])
      | `Application (_, ds)     -> union (List.map (make_env_ta boundvars) ds)
      | `ForAll (qs, t)          ->
          make_env
            (List.fold_right
               (fun q boundvars -> S.add (var_of_quantifier q) boundvars)
               (unbox_quantifiers qs)
               boundvars)
            t
      | `MetaTypeVar point       ->
          begin
            match Unionfind.find point with
              | `Var (var, subkind, `Flexible) ->
                  let tenv, renv, penv = empties in
                    (M.add var (fresh_type_variable subkind) tenv, renv, penv)
              | `Var (var, subkind, `Rigid) ->
                  let tenv, renv, penv = empties in
                    (M.add var (fresh_rigid_type_variable subkind) tenv, renv, penv)
              | `Recursive (l, _) when S.mem l boundvars -> empties
              | `Recursive (l, b) -> make_env (S.add l boundvars) b
              | `Body t -> make_env boundvars t
          end
      | `Input (t, s)
      | `Output (t, s) -> union [make_env boundvars t; make_env boundvars s]
      | `Select row
      | `Choice row    -> make_env_r boundvars row
      | `Dual s        -> make_env boundvars s
      | `End           -> empties
  and make_env_f boundvars =
    function
      | `Present t -> make_env boundvars t
      | `Absent -> empties
      | `Var point ->
          begin
            match Unionfind.find point with
              | `Var (var, subkind, `Flexible) ->
                  let tenv, renv, penv = empties in
                    (tenv, renv, M.add var (fresh_presence_variable subkind) penv)
              | `Var (var, subkind, `Rigid) ->
                  let tenv, renv, penv = empties in
                    (tenv, renv, M.add var (fresh_rigid_presence_variable subkind) penv)
              | `Body f -> make_env_f boundvars f
          end
  and make_env_r boundvars ((field_env, row_var, _):row) =
    let field_vars =
      FieldEnv.fold
        (fun _ f envs ->
           union [make_env_f boundvars f; envs])
        field_env empties
    and row_vars =
      match Unionfind.find row_var with
        | `Closed -> empties
        | `Var (var, subkind, `Flexible) ->
            let tenv, renv, penv = empties in
              (tenv, M.add var (StringMap.empty, fresh_row_variable subkind, false) renv, penv)
        | `Var (var, subkind, `Rigid) ->
            let tenv, renv, penv = empties in
              (tenv, M.add var (StringMap.empty, fresh_rigid_row_variable subkind, false) renv, penv)
        | `Recursive (l, _) when S.mem l boundvars -> empties
        | `Recursive (l, row) -> make_env_r (S.add l boundvars) row
        | `Body row -> make_env_r boundvars row
    in union [field_vars; row_vars]
  and make_env_ta boundvars =
    function
      | `Type t -> make_env boundvars t
      | `Row row -> make_env_r boundvars row
      | `Presence f -> make_env_f boundvars f
  in make_env S.empty

let make_rigid_envs datatype : datatype IntMap.t * row IntMap.t * field_spec Utility.IntMap.t =
  let tenv, renv, penv = make_fresh_envs datatype in
    (IntMap.map (fun _ -> fresh_rigid_type_variable (`Any, `Any)) tenv,
     IntMap.map (fun _ -> (StringMap.empty, fresh_rigid_row_variable (`Any, `Any), false)) renv,
     IntMap.map (fun _ -> fresh_rigid_presence_variable (`Any, `Any)) penv)

let make_wobbly_envs datatype : datatype IntMap.t * row IntMap.t * field_spec Utility.IntMap.t =
  let tenv, renv, penv = make_fresh_envs datatype in
    (IntMap.map (fun _ -> fresh_type_variable (`Any, `Any)) tenv,
     IntMap.map (fun _ -> (StringMap.empty, fresh_row_variable (`Any, `Any), false)) renv,
     IntMap.map (fun _ -> fresh_presence_variable (`Any, `Any)) penv)


(* subtyping *)
let is_sub_type, is_sub_row =
  let module S = TypeVarSet in
(*   let rec is_sub_eff (eff, eff') = *)
(* (\*     Debug.print ("mbt: "^string_of_datatype t); *\) *)
(* (\*     Debug.print ("mbt': "^string_of_datatype t'); *\) *)
(*     match eff, eff' with *)
(* (\*       | `MetaTypeVar point, `MetaTypeVar point' -> *\) *)
(* (\*           begin *\) *)
(* (\*             match Unionfind.find point, Unionfind.find point' with *\) *)
(* (\*               | `Rigid var, `Rigid var' *\) *)
(* (\*               | `Flexible var, `Flexible var' -> var=var' *\) *)
(* (\*               | `Body t, _ -> is_sub_mb (t, t') *\) *)
(* (\*               | _, `Body t -> is_sub_mb (t, t') *\) *)
(* (\*               | _, _ -> false *\) *)
(* (\*           end *\) *)
(* (\*       | `MetaTypeVar point, _ -> *\) *)
(* (\*           begin *\) *)
(* (\*             match Unionfind.find point with *\) *)
(* (\*               | `Rigid _ *\) *)
(* (\*               | `Flexible _ *\) *)
(* (\*               | `Recursive _ -> false *\) *)
(* (\*               | `Body t -> is_sub_mb (t, t') *\) *)
(* (\*           end *\) *)
(* (\*       | `Application (mb, [_ (\\*`Alias (("O", _), _)*\\)]), _ *\) *)
(* (\*           when mb.Abstype.id = mailbox.Abstype.id *\) *)
(* (\*           -> true (\\* HACK *\\) *\) *)
(* (\*       | _, `MetaTypeVar point -> *\) *)
(* (\*           begin *\) *)
(* (\*             match Unionfind.find point with *\) *)
(* (\*               | `Rigid _ *\) *)
(* (\*               | `Flexible _ *\) *)
(* (\*               | `Recursive _ -> false *\) *)
(* (\*               | `Body t' -> is_sub_mb (t, t') *\) *)
(* (\*           end *\) *)
(*       | _, _ -> false in *)
  let rec is_sub_type = fun rec_vars (t, t') ->
    match t, t' with
      | `Not_typed, `Not_typed -> true
      | `Primitive p, `Primitive q -> p=q
      | `Function (f, eff, t), `Function (f', eff', t') ->
          is_sub_type rec_vars (f', f)
          && is_sub_eff rec_vars (eff, eff')
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
          labs = rabs && assert false (* TODO: is_sub_type_tyarg *)
(*              List.for_all2 (fun t t' -> is_sub_type rec_vars (t, t')) lts rts*)
      | `MetaTypeVar point, `MetaTypeVar point' ->
          begin
            match Unionfind.find point, Unionfind.find point' with
              | `Var (var, _, _), `Var (var', _, _) -> var=var'
              | `Body t, _ -> is_sub_type rec_vars (t, t')
              | _, `Body t -> is_sub_type rec_vars (t, t')
              | `Recursive (var, t), `Recursive (var', t') ->
                  failwith "not implemented subtyping on recursive types yet"
              | _, _ -> false
          end
      | `MetaTypeVar point, _ ->
          begin
            match Unionfind.find point with
              | `Var _
              | `Recursive _ -> false
              | `Body t -> is_sub_type rec_vars (t, t')
          end
      | _, `MetaTypeVar point ->
          begin
            match Unionfind.find point with
              | `Var _
              | `Recursive _ -> false
              | `Body t' -> is_sub_type rec_vars (t, t')
          end
      | `Alias ((name, []), _), `Alias ((name', []), _) when name=name' -> true
      | (`Alias (_, t)), t'
      | t, (`Alias (_, t')) -> is_sub_type rec_vars (t, t')
      | `ForAll (qs, t), `ForAll (qs', t') ->
          failwith "not implemented subtyping on forall types yet"
      | _, _ -> false
  (* This is like standard row sub-typing, but the field types must be invariant.
     Ultimately we might want more flexibility. For instance, we might expect
     contravariance in the type of heard messages (the 'hear' effect is only
     associated with input).
  *)
  and is_sub_eff =
    fun rec_vars ((lfield_env, lrow_var, false as lrow), (rfield_env, rrow_var, false as rrow)) ->
      let sub_fields =
        FieldEnv.fold (fun name f b ->
                         match f with
                           | `Present t ->
                               if FieldEnv.mem name rfield_env then
                                 match FieldEnv.find name rfield_env with
                                   | `Present t' ->
                                       (is_sub_type rec_vars (t, t') &&
                                          is_sub_type rec_vars (t', t))
                                   | `Absent
                                   | `Var _ -> false
                               else
                                 false
                           | `Absent -> true
                           | `Var _ -> assert false (* TODO *)) lfield_env true in
      let sub_row_vars =
        match Unionfind.find lrow_var, Unionfind.find rrow_var with
          | `Var (var, _, _), `Var (var', _, _) -> var=var'
          | `Closed, _ -> true
          | `Body lrow, _ -> is_sub_eff rec_vars (lrow, rrow)
          | _, `Body rrow -> is_sub_eff rec_vars (lrow, rrow)
          | `Recursive _, `Recursive _ ->
              assert false
          | _, _ -> false
      in
        sub_fields && sub_row_vars
  and is_sub_row =
    fun rec_vars ((lfield_env, lrow_var, ldual as lrow), (rfield_env, rrow_var, rdual as rrow)) ->
      let sub_fields =
        FieldEnv.fold (fun name f b ->
                         match f with
                           | `Present t ->
                               if FieldEnv.mem name rfield_env then
                                 match FieldEnv.find name rfield_env with
                                   | `Present t' ->
                                       is_sub_type rec_vars (t, t')
                                   | `Absent
                                   | `Var _ -> false
                               else
                                 false
                           | `Absent ->
                               true
                           | `Var _ -> assert false (* TODO *)) lfield_env true in
      let sub_row_vars =
        let dual_if b r = if b then dual_row r else r in
        match Unionfind.find lrow_var, Unionfind.find rrow_var with
          | `Var (var, _, _), `Var (var', _, _) -> ldual=rdual && var=var'
          | `Closed, _ -> true
          | `Body lrow, _ -> is_sub_row rec_vars (dual_if ldual lrow, rrow)
          | _, `Body rrow -> is_sub_row rec_vars (lrow, dual_if rdual rrow)
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

let make_list_type t = `Application (list, [`Type t])
let make_process_type r = `Application (process, [`Row r])

let extend_row fields (fields', row_var, dual) =
  (FieldEnv.fold
     (fun name t fields -> FieldEnv.add name (`Present t) fields)
     fields
     fields',
   row_var, dual)

let make_closed_row : datatype field_env -> row = fun fields ->
  (FieldEnv.map (fun t -> `Present t) fields), closed_row_var, false

let make_record_type ts = `Record (make_closed_row ts)
let make_variant_type ts = `Variant (make_closed_row ts)

let make_table_type (r, w, n) = `Table (r, w, n)
let make_endbang_type : datatype = `Alias (("EndBang", []), `Output (unit_type, `End))
