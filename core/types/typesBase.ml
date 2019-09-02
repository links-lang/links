(* Do not use this file directly, but use the Types module instead *)

open Utility
open CommonTypes

[@@@ocaml.warning "-32"] (** disable warnings about unused functions in this module**)

let internal_error message =
  Errors.internal_error ~filename:"types.ml" ~message

module FieldEnv = Utility.StringMap
type 'a stringmap = 'a Utility.stringmap [@@deriving show]
type 'a field_env = 'a stringmap [@@deriving show]

(* type var sets *)
module TypeVarSet = Utility.IntSet
let intset_of_typevarset x = x


(* type var sets *)
module TypeVarMap = Utility.IntMap

(* points *)
type 'a point = 'a Unionfind.point [@@deriving show]

type kind = PrimaryKind.t * subkind
    [@@deriving eq,show]

type 't meta_type_var_non_rec_basis =
    [ `Var of (int * subkind * freedom)
    | `Body of 't ]
      [@@deriving show]

type 't meta_type_var_basis =
    [ 't meta_type_var_non_rec_basis
    | `Recursive of (int * 't) ]
      [@@deriving show]

type 'r meta_row_var_basis =
    [ 'r meta_type_var_basis | `Closed ]
      [@@deriving show]

type 't meta_presence_var_basis = 't meta_type_var_non_rec_basis
      [@@deriving show]

(* this subsumes all of the other meta_X_basis thingies *)
type 't meta_max_basis = 't meta_row_var_basis

type istring = string [@@deriving show,eq]

module Abstype =
struct
  type t = { id    : istring ;
             name  : istring ;
             arity : kind list }
      [@@deriving eq,show]
  let make name arity =
    let id = Utility.gensym ~prefix:"abstype:" () in
      { id; name; arity }
  let arity { arity; _ } = arity
  let name  { name ; _ } = name
  let compare l r = String.compare l.id r.id
end

let process  = {
  Abstype.id = "Process" ;
  name       = "Process" ;
  arity      = [pk_row, (lin_any, res_effect)] ;
}

(* Lists are currently unlimited because the only deconstructors are
   hd and tl. There's nothing to stop us rolling our own potentially
   linear lists, though. *)
let list     = {
  Abstype.id = "List" ;
  name       = "List" ;
  arity      = [pk_type, (lin_unl, res_any)] ;
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
  arity      = [pk_type, (lin_any, res_session)] ;
}

let socket = {
  Abstype.id = "Socket";
  name = "Socket";
  arity = []
}

let spawn_location = {
  Abstype.id = "Location";
  name = "Location";
  arity = []
}

type ('t, 'r) session_type_basis =
    [ `Input of 't * 't
    | `Output of 't * 't
    | `Select of 'r
    | `Choice of 'r
    | `Dual of 't
    | `End ]
      [@@deriving show]

(* When unifying, we need to keep track of both mu-bound recursive variables
 * and names of recursive type applications. The `rec_id` type allows us to
 * abstract this and keep both in the same environment. *)
type rec_id =
  | MuBoundId of int
  | NominalId of string [@@deriving show]

module RecId = struct
  type t = rec_id [@@deriving show]
  let compare = compare
end
module type RECIDMAP = Utility.Map with type key = rec_id
module RecIdMap = Map.Make(RecId)

module type RECIDSET = Utility.Set with type elt = rec_id
module RecIdSet : RECIDSET = Set.Make(RecId)


type tygroup = {
  id: int;
  type_map: ((quantifier list * typ) Utility.StringMap.t);
  linearity_map: bool Utility.StringMap.t
}

(* Types *)
and rec_appl = {
  r_name: string;
  r_dual: bool;
  r_unique_name: string;
  r_quantifiers : kind list;
  r_args: type_arg list;
  r_unwind: type_arg list -> bool -> typ;
  r_linear: unit -> bool option
}
and typ =
    [ `Not_typed
    | `Primitive of Primitive.t
    | `Function of (typ * row * typ)
    | `Lolli of (typ * row * typ)
    | `Record of row
    | `Variant of row
    | `Effect of row
    | `Table of typ * typ * typ
    | `Lens of Lens.Type.t
    | `Alias of ((string * kind list * type_arg list) * typ)
    | `Application of (Abstype.t * type_arg list)
    | `RecursiveApplication of rec_appl
    | `MetaTypeVar of meta_type_var
    | `ForAll of (quantifier list * typ)
    | (typ, row) session_type_basis ]
and field_spec     = [ `Present of typ | `Absent | `Var of meta_presence_var ]
and field_spec_map = field_spec field_env
and row_var        = meta_row_var
and row            = field_spec_map * row_var * bool (* true if the row variable is dualised *)
and meta_type_var  = (typ meta_type_var_basis) point
and meta_row_var   = (row meta_row_var_basis) point
and meta_presence_var = (field_spec meta_presence_var_basis) point
and meta_var = [ `Type of meta_type_var | `Row of meta_row_var | `Presence of meta_presence_var ]
and quantifier = int * kind
and type_arg =
    [ `Type of typ | `Row of row | `Presence of field_spec ]
      [@@deriving show]

type session_type = (typ, row) session_type_basis
  [@@deriving show]

let dummy_type = `Primitive Primitive.Int

let is_present =
  function
  | `Present _           -> true
  | (`Absent | `Var _) -> false

type alias_type = quantifier list * typ [@@deriving show]

type tycon_spec = [
  | `Alias of alias_type
  | `Abstract of Abstype.t
  | `Mutual of (quantifier list * tygroup ref) (* Type in same recursive group *)
] [@@deriving show]


(* Generation of fresh type variables *)
let type_variable_counter = ref 0
let fresh_raw_variable () : int =
  incr type_variable_counter;
  !type_variable_counter

module type TYPE_VISITOR =
sig
  class visitor :
  object ('self_type)

    method remove_rec_row_binding : int -> 'self_type
    method remove_rec_type_binding : int ->'self_type

    method primitive : Primitive.t -> (Primitive.t * 'self_type)
    method typ : typ -> (typ * 'self_type)
    method row : row -> (row * 'self_type)
    method row_var : row_var -> (row_var * 'self_type)
    method meta_type_var : meta_type_var -> (meta_type_var * 'self_type)
    method meta_row_var : meta_row_var -> (meta_row_var * 'self_type)
    method meta_var : meta_var -> (meta_var * 'self_type)
    method meta_presence_var : meta_presence_var -> (meta_presence_var * 'self_type)
    method field_spec : field_spec -> (field_spec * 'self_type)
    method field_spec_map : field_spec_map -> (field_spec_map * 'self_type)
    method quantifier : quantifier -> (quantifier * 'self_type)
    method type_arg : type_arg -> (type_arg * 'self_type)
  end
end



module Transform : TYPE_VISITOR =
struct
  class visitor  =
  object ((o : 'self_type))

    val rec_vars : (meta_type_var) IntMap.t * (meta_row_var) IntMap.t = (IntMap.empty, IntMap.empty)
    method remove_rec_type_binding id =
      let (rec_types, rec_rows) = rec_vars in
      {< rec_vars = (IntMap.remove id rec_types, rec_rows) >}

    method remove_rec_row_binding id =
      let (rec_types, rec_rows) = rec_vars in
      {< rec_vars = (rec_types, IntMap.remove id rec_rows) >}



    method primitive p = (p,o)
    method row :  row -> (row * 'self_type) = fun (fsp, rv, d) ->
      let (fsp', o) = o#field_spec_map fsp in
      let (rv', o) = o#row_var rv in
        ((fsp', rv', d), o)


    method meta_type_var : meta_type_var -> (meta_type_var * 'self_type) = fun point ->
      match Unionfind.find point with
        | `Recursive (var, t) ->
          let (rec_types, rec_rows) = rec_vars in
          if IntMap.mem var rec_types then
            (IntMap.find var rec_types), o
          else
            let var' = fresh_raw_variable () in
            let point' : meta_type_var = Unionfind.fresh (`Var (var', (lin_any, res_any), `Flexible)) in
            let rec_types' : (meta_type_var) IntMap.t = IntMap.add var point' rec_types in
            let o_extended_rec_env = {< rec_vars = (rec_types', rec_rows) >} in
            let (t', o') = o_extended_rec_env#typ t in
            let o'_reduced_rec_env = o'#remove_rec_type_binding var in
            Unionfind.change point' (`Recursive (var', t'));
            (point', o'_reduced_rec_env)
        | `Body t ->
            let (t', o) = o#typ t in Unionfind.fresh (`Body t'), o
        | `Var _  -> point, o

    method meta_row_var : meta_row_var -> (meta_row_var * 'self_type) = fun point ->
      match Unionfind.find point with
        | `Closed -> point, o
        | `Recursive (var, r) ->
          let (rec_types, rec_rows) = rec_vars in
          if IntMap.mem var rec_rows then
            (IntMap.find var rec_rows), o
          else
            let var' = fresh_raw_variable () in
            let point' = Unionfind.fresh (`Var (var', (lin_any, res_any), `Flexible)) in
            let rec_rows' = IntMap.add var point' rec_rows in
            let o_extended_rec_env = {< rec_vars = (rec_types, rec_rows') >} in
            let (r', o') = o_extended_rec_env#row r in
            let o'_reduced_rec_env = o'#remove_rec_row_binding var in
            Unionfind.change point' (`Recursive (var', r'));
            (point', o'_reduced_rec_env)
        | `Body r ->
            let (t', o) = o#row r in Unionfind.fresh (`Body t'), o
        | `Var _  -> point, o

    method row_var : row_var -> (row_var * 'self_type) = o#meta_row_var

    method meta_var : meta_var -> (meta_var * 'self_type) = function
      | `Type  mtv -> let (mtv', o) = o#meta_type_var mtv in (`Type mtv', o)
      | `Row mrv -> let (mrv', o) =  o#meta_row_var mrv in (`Row mrv', o)
      | `Presence mpv -> let (mpv', o) = o#meta_presence_var mpv in (`Presence mpv', o)

    method meta_presence_var :  meta_presence_var -> (meta_presence_var * 'self_type) = fun point ->
      let (newVar, o) = begin match Unionfind.find point with
                                  | `Body fs ->
                                     let (fs',o) = o#field_spec fs in (`Body fs', o)
                                  | `Var (id,sk,fd) -> (`Var (id,sk,fd), o) end in
                                (Unionfind.fresh newVar, o)
    method field_spec :  field_spec -> (field_spec * 'self_type) = function
      | `Present t ->
         let (t',o) = o#typ t in
           (`Present t', o)
      | `Absent -> (`Absent, o)
      | `Var mpv ->
         let (mpv', o) = o#meta_presence_var mpv in
           (`Var mpv', o)

    method field_spec_map :  field_spec_map -> (field_spec_map * 'self_type) = fun map ->
      Utility.StringMap.fold (fun k v (new_map, o) ->
        let (fs, o) = o#field_spec v in
          (StringMap.add k fs new_map, o)
       ) map (StringMap.empty, o)

   method quantifier (i, k) = ((i, k), o)

   method type_arg = function
     | `Type t ->
        let (t', o) = o#typ t in (`Type t', o)
     | `Row r ->
        let (r', o) = o#row r in (`Row r', o)
     | `Presence p ->
        let (p', o) = o#field_spec p in (`Presence p', o)

   method typ = function
     | `Not_typed ->
        (`Not_typed, o)
     | `Primitive p ->
        let (p', o) = o#primitive p in (`Primitive p', o)
     | `Function (at, r, rt) ->
        let (at', o) = o#typ at in
        let (r', o) = o#row r in
        let (rt', o) = o#typ rt in
          (`Function (at', r', rt'), o)
     | `Lolli (at, r, rt) ->
        let (at', o) = o#typ at in
        let (r', o) = o#row r in
        let (rt', o) = o#typ rt in
          (`Lolli (at', r', rt'), o)
     | `Record r ->
        let (r', o) = o#row r in
          (`Record r', o)
     | `Variant r ->
        let (r', o) = o#row r in
          (`Variant r', o)
     | `Effect r ->
        let (r', o) = o#row r in
          (`Effect r', o)
     | `Table (t1, t2, t3) ->
        let (t1', o) = o#typ t1 in
        let (t2', o) = o#typ t2 in
        let (t3', o) = o#typ t3 in
          (`Table (t1', t2', t3'), o)
     | `Lens sort ->
          (`Lens sort, o)
     | `Alias ((name, qs, args), t) ->
        let (args', o) = List.fold_right (fun arg (acc_args, o) ->
            let (arg', o) = o#type_arg arg in
            (arg' :: acc_args, o)
          ) args ([],o) in
        let (t',o) = o#typ t in
          (`Alias ((name, qs, args'), t'), o)
     | `Application (abst, args) ->
        let (args', o) = List.fold_right (fun arg (acc_args, o) ->
            let (arg', o) = o#type_arg arg in
            (arg' :: acc_args, o)
          ) args ([], o) in
          (`Application (abst, args'), o)
     | `RecursiveApplication appl ->
        let (args', o) = List.fold_right (fun arg (acc_args, o) ->
            let (arg', o) = o#type_arg arg in
            (arg' :: acc_args, o)
          ) appl.r_args ([], o) in
          (`RecursiveApplication { appl with r_args = args' }, o)
     | `MetaTypeVar mtv ->
        let (mtv', o) = o#meta_type_var mtv in
          (`MetaTypeVar mtv', o)
     | `ForAll (rqs,t) ->
         let (qs', o) = List.fold_right (fun q (acc_qs, o) ->
            let (q',o) = o#quantifier q in
            (q' :: acc_qs,o)
          ) rqs ([],o) in
         let (t', o) = o#typ t in
         (`ForAll (qs', t'), o)
     | `Input (t1, t2) ->
         let (t1', o) = o#typ t1 in
         let (t2', o) = o#typ t2 in
         (`Input (t1', t2'), o)
     | `Output (t1, t2) ->
         let (t1', o) = o#typ t1 in
         let (t2', o) = o#typ t2 in
         (`Output (t1', t2'), o)
     | `Dual t ->
        let (t', o) = o#typ t in
        (`Dual t', o)
     | `Select r ->
         let (r', o) = o#row r in
        (`Select r', o)
     | `Choice r ->
         let (r', o) = o#row r in
        (`Select r', o)
     | `End -> (`End, o)


  end
end


module ElimRecursiveTypeCyclesTransform : TYPE_VISITOR =
struct
  class visitor =
      object (o)
        inherit Transform.visitor as super


        val mu_vars =  Utility.IntSet.empty

        method! meta_type_var point = match Unionfind.find point with
          | `Recursive (id, t) ->
             if Utility.IntSet.mem id mu_vars then
               let newvar = `Var (id, (lin_any, res_any), `Rigid) in
               (* Debug.print (Printf.sprintf "Saw rec  var %d" id); *)
               (Unionfind.fresh newvar, o)
             else
               let new_mu_vars = Utility.IntSet.add id mu_vars in
               let o' =  {< mu_vars=new_mu_vars >} in
               (* Debug.print (Printf.sprintf "Added rec  var %d" id); *)
               let (t', _) = o'#typ t in (Unionfind.fresh (`Recursive (id, t')), o)
          | _ -> super#meta_type_var point

        method! meta_row_var point = match Unionfind.find point with
          | `Recursive (id, t) ->
             if Utility.IntSet.mem id mu_vars then
               let newvar = `Var (id, (lin_any, res_any), `Rigid) in
               (* Debug.print (Printf.sprintf "Saw rec  var %d" id); *)
               (Unionfind.fresh newvar, o)
             else
               let new_mu_vars = Utility.IntSet.add id mu_vars in
               let o' =  {< mu_vars=new_mu_vars >} in
               (* Debug.print (Printf.sprintf "Added rec  var %d" id); *)
               let (t', _) = o'#row t in (Unionfind.fresh (`Recursive (id, t')), o)
          | _ -> super#meta_row_var point


      end
end



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
    | var, _ -> var

let kind_of_quantifier : quantifier -> kind =
  fun (_, k) -> k

let primary_kind_of_quantifier : quantifier -> PrimaryKind.t =
  fun (_, (pk, _)) -> pk

let subkind_of_quantifier : quantifier -> subkind
  = fun q ->
    snd (kind_of_quantifier q)

let primary_kind_of_type_arg : type_arg -> PrimaryKind.t =
  function
  | `Type _     -> pk_type
  | `Row _      -> pk_row
  | `Presence _ -> pk_presence

let add_quantified_vars qs vars =
  List.fold_right IntSet.add (List.map var_of_quantifier qs) vars


(** A constraint provides a way of ensuring a type (or row) satisfies the
   requirements of some subkind. *)
module type Constraint = sig
  (** Does this type satisfy the requirements of this subkind? *)
  val type_satisfies : typ -> bool
  val row_satisfies : row -> bool

  (** Can this type be modified to satisfy the requirements of the subkind?

      This effectively asks if there are flexible type variables whose subkinds
     can be modified so that {!is_type} returns true. *)
  val can_type_be : typ -> bool
  val can_row_be : row -> bool

  (** Attempt to convert any flexible variables within this type so the
     appropriate subkind, so that {!is_type} will return true. *)
  val make_type : typ -> unit
  val make_row : row -> unit
end

(** A context for the various type visitors ({!type_predicate} and {!type_iter})

  This contains:

   - The set of applications to [typename]s currently being visited.
   - The set of in-scope recursive type variables.
   - The set of in-scope forall quantified variables.

  We do not impose the same restrictions on forall quantified variables as we do
  on free ones, hence the need to track quantified variables. *)
type visit_context = StringSet.t * var_set * var_set

(** A predicate on types, which can be used to limit the range of types a
    {!Constraint} allows.

   By default, this visits the entire type, and returns true iff all child nodes
   of the type satisfy the predicate. *)
class virtual type_predicate = object(self)
  method var_satisfies : (int * subkind * freedom) -> bool = fun _ -> true

  method point_satisfies : 'a 'c . (visit_context -> 'a -> bool) -> visit_context -> ([< 'a meta_max_basis] as 'c) point -> bool
    = fun f ((rec_appl, rec_vars, quant_vars) as vars) point ->
    match Unionfind.find point with
    | `Closed -> true
    | `Var ((id, _, _) as var) -> IntSet.mem id quant_vars || self#var_satisfies var
    | `Body t -> f vars t
    | `Recursive (var, t) -> check_rec var rec_vars true (fun rec_vars' -> f (rec_appl, rec_vars', quant_vars) t)

  method type_satisfies ((rec_appl, rec_vars, quant_vars) as vars) : typ -> bool = function
    | `Not_typed -> assert false
    | `Primitive _ -> true
    | `Function (a, e, r) | `Lolli (a, e, r) -> self#type_satisfies vars a && self#row_satisfies vars e && self#type_satisfies vars r
    | `Record r | `Effect r | `Variant r -> self#row_satisfies vars r
    | `Table _ -> true
    | `Lens _ -> true
    | `Alias (_, t) -> self#type_satisfies vars t
    | `MetaTypeVar point -> self#point_satisfies self#type_satisfies vars point
    | `ForAll (qs, t) -> self#type_satisfies (rec_appl, rec_vars, add_quantified_vars qs quant_vars) t
    | `Application (_, ts) ->
       (* This does assume that all abstract types satisfy the predicate. *)
       List.for_all (self#type_satisfies_arg vars) ts
    | `RecursiveApplication { r_unique_name; r_args; r_dual; r_unwind; _ } ->
       if StringSet.mem r_unique_name rec_appl then
         List.for_all (self#type_satisfies_arg vars) r_args
       else
         let body = r_unwind r_args r_dual in
         self#type_satisfies (StringSet.add r_unique_name rec_appl, rec_vars, quant_vars) body
    | `Select r | `Choice r -> self#row_satisfies vars r
    | `Input (a, b) | `Output (a, b) -> self#type_satisfies vars a && self#type_satisfies vars b
    | `Dual s -> self#type_satisfies vars s
    | `End -> true

  method field_satisfies vars = function
    | `Absent -> true
    | `Present t -> self#type_satisfies vars t
    | `Var point -> self#point_satisfies self#field_satisfies vars point

  method row_satisfies vars (fields, row_var, _) =
    let row_var = self#point_satisfies self#row_satisfies vars row_var in
    let fields = FieldEnv.for_all (fun _ f -> self#field_satisfies vars f) fields in
    row_var && fields

  method type_satisfies_arg vars (arg : type_arg) =
    match arg with
    | `Type t -> self#type_satisfies vars t
    | `Row r -> self#row_satisfies vars r
    | `Presence f -> self#field_satisfies vars f

  method predicates =
    (self#type_satisfies (StringSet.empty, IntSet.empty, IntSet.empty),
     self#row_satisfies (StringSet.empty, IntSet.empty, IntSet.empty))
end

(** Iterate over every node in a type.

    By default this does nothing. However, it can be extended by {!Constraint}s
    to mutate various flexible type variables. *)
class virtual type_iter = object(self)
  method visit_var : 'a 'c. ([< 'a meta_max_basis > `Var] as 'c) point -> (int * subkind * freedom) -> unit = fun _ _ -> ()

  method visit_point : 'a 'c . (visit_context -> 'a -> unit) -> visit_context -> ([< 'a meta_max_basis > `Var] as 'c) point -> unit
    = fun f ((rec_appl, rec_vars, quant_vars) as vars) point ->
    match Unionfind.find point with
    | `Closed -> ()
    | `Var ((id, _, _) as var) -> if not (IntSet.mem id quant_vars) then self#visit_var point var
    | `Body t -> f vars t
    | `Recursive (var, t) -> check_rec var rec_vars () (fun rec_vars' -> f (rec_appl, rec_vars', quant_vars) t)

  method visit_type ((rec_appl, rec_vars, quant_vars) as vars) : typ -> unit = function
    | `Not_typed -> assert false
    | `Primitive _ -> ()
    | `Function (a, e, r) | `Lolli (a, e, r) -> self#visit_type vars a; self#visit_row vars e; self#visit_type vars r
    | `Record r | `Effect r | `Variant r -> self#visit_row vars r
    | `Table _ -> ()
    | `Lens _ -> ()
    | `Alias (_, t) -> self#visit_type vars t
    | `MetaTypeVar point -> self#visit_point self#visit_type vars point
    | `ForAll (qs, t) -> self#visit_type (rec_appl, rec_vars, add_quantified_vars qs quant_vars) t
    | `Application (_, ts) -> List.iter (self#visit_type_arg vars) ts
    | `RecursiveApplication { r_args; _ } -> List.iter (self#visit_type_arg vars) r_args
    | `Select r | `Choice r -> self#visit_row vars r
    | `Input (a, b) | `Output (a, b) -> self#visit_type vars a; self#visit_type vars b
    | `Dual s -> self#visit_type vars s
    | `End -> ()

  method visit_field vars = function
    | `Absent -> ()
    | `Present t -> self#visit_type vars t
    | `Var point -> self#visit_point self#visit_field vars point

  method visit_row vars (fields, row_var, _) =
    self#visit_point self#visit_row vars row_var;
    FieldEnv.iter (fun _ f -> self#visit_field vars f) fields

  method visit_type_arg vars (arg : type_arg) =
    match arg with
    | `Type t -> self#visit_type vars t
    | `Row r -> self#visit_row vars r
    | `Presence f -> self#visit_field vars f

  method visitors =
    (self#visit_type (StringSet.empty, IntSet.empty, IntSet.empty),
     self#visit_row (StringSet.empty, IntSet.empty, IntSet.empty))
end

module type TypePredicate = sig class klass : type_predicate end

(** Make a basic restriction predicate from a more general type predicate.

   This simply requires that all visited type variables has the correct subkind.
   If [flexibles] is true, then flexible type variables which can be converted
   to have the correct subkind are permitted. *)
let make_restriction_predicate (klass : (module TypePredicate)) restr flexibles =
  let module M = (val klass) in
  (object
     inherit M.klass

     method! var_satisfies = function
       | (_, (_, sk), _) when sk = restr -> true
       | (_, _, `Rigid) -> false
       | (_, (_, sk), `Flexible) ->
          flexibles &&
            match Restriction.min sk restr with
            | Some sk -> sk = restr
            | _ -> false
   end)#predicates

(** Make a basic restriction transformation. This attempts to transform any
   flexible type variable to one with a compatible subkind.

   If a type variable cannot be made compatible, and [ensure] is true, then an
   error is thrown. *)
let make_restriction_transform ?(ensure=false) subkind =
  (object
     inherit type_iter

     method! visit_var point = function
       | (_, (_, sk), _) when sk = subkind -> ()
       | (v, (l, sk), `Flexible) ->
          begin
            match Restriction.min sk subkind with
            | Some sk when sk = subkind -> Unionfind.change point (`Var (v, (l, sk), `Flexible))
            | _ -> assert ensure
          end
       | (_, _, `Rigid) -> assert ensure
   end)#visitors

module Base : Constraint = struct
  open Restriction
  open Primitive

  module BasePredicate = struct
    class klass = object
      inherit type_predicate as super

      method! point_satisfies f vars point =
        match Unionfind.find point with
        | `Recursive _ -> false
        | _ -> super#point_satisfies f vars point

      method! type_satisfies vars = function
        | `Primitive (Bool | Int | Char | Float | String) -> true
        | (`Alias _ | `MetaTypeVar _) as t  -> super#type_satisfies vars t
        | _ -> false
    end
  end

  let type_satisfies, row_satisfies = make_restriction_predicate (module BasePredicate) Base false
  let can_type_be, can_row_be = make_restriction_predicate (module BasePredicate) Base true
  let make_type, make_row = make_restriction_transform Base
end

(* unl type stuff *)
module Unl : Constraint = struct
  class unl_predicate = object
    inherit type_predicate as super

    method! type_satisfies vars = function
      | `Not_typed -> assert false
      | `Effect _ | `Primitive _ | `Function _ -> true
      | `Lolli _ -> false
      | (`Record _ | `Variant _ | `Alias _ | `MetaTypeVar _ | `ForAll _ | `Dual _) as t
        -> super#type_satisfies vars t
      | `Table _ -> true
      | `Lens _sort -> true
        (* We might support linear lists like this...
           but we'd need to replace hd and tl with a split operation. *)
        (* | `Application ({Abstype.id="List"}, [`Type t]) -> Unl.satisfies_type (rec_vars, quant_vars) t  *)
      | `Application _ -> true (* TODO: change this if we add linear abstract types *)
      | `RecursiveApplication { r_linear ; _ } ->
            (* An application is linear if the type it refers to is
             * also linear. We calculate this information in two stages.
             * The first pass (if r_linear () returns None) calculates linearity
             * *up to recursive applications*, under the assumption that every type in the
             * block is unrestricted. With this in hand, we can calculate
             * linearity information, meaning that (r_linear ()) will return (Some lin). *)
            OptionUtils.opt_app not true (r_linear ())
      | `End -> false
      | #session_type -> false
  end

  let type_satisfies, row_satisfies =
    (object
      inherit unl_predicate
      method! var_satisfies = function
        | (_, (Linearity.Unl, _), _) -> true
        | _ -> false
    end)#predicates

  let can_type_be, can_row_be =
    (object
      inherit unl_predicate
      method! var_satisfies = function
        | (_, (Linearity.Unl, _), _) -> true
        | (_, _, `Flexible) -> true
        | (_, _, `Rigid) -> false
    end)#predicates

  let make_type, make_row = (object
     inherit type_iter as super

     method! visit_type vars = function
       | `Not_typed -> assert false
       | `Effect _ | `Primitive _ | `Function _ | `Table _ | `Lens _ | `Application _ -> ()
       | (`Record _ | `Variant _ | `Alias _ | `MetaTypeVar _ | `ForAll _ | `Dual _) as t
         -> super#visit_type vars t
       | `RecursiveApplication _ -> ()
       | _ -> assert false

     method! visit_var point = function
       | (_, (Linearity.Unl, _), _) -> ()
       | (v, (_, sk), `Flexible) -> Unionfind.change point (`Var (v, (lin_unl, sk), `Flexible))
       | (_, _, `Rigid) -> assert false
   end)#visitors
end

module Session : Constraint = struct
  open Restriction

  module SessionPredicate = struct
    class klass = object
      inherit type_predicate as super

      method! type_satisfies ((rec_appls, _, _) as vars) = function
        | #session_type -> true
        | (`Alias _ | `MetaTypeVar _) as t -> super#type_satisfies vars t
        | (`RecursiveApplication { r_unique_name; _ }) as t ->
           if StringSet.mem r_unique_name rec_appls then
             false
           else
             super#type_satisfies vars t
        | _ -> false
    end
  end

  let type_satisfies, row_satisfies = make_restriction_predicate (module SessionPredicate) Session false
  let can_type_be, can_row_be = make_restriction_predicate (module SessionPredicate) Session true
  let make_type, make_row =
    (object
       inherit type_iter as super

       method! visit_var point = function
         | (_, (_, Session), _) -> ()
         | (v, (l, sk), `Flexible) ->
            begin
              match Restriction.min sk Session with
              | Some Session -> Unionfind.change point (`Var (v, (l, Session), `Flexible))
              | _ -> assert false
            end
         | (_, _, `Rigid) -> assert false

       method! visit_type vars = function
         | #session_type -> ()
         | ty -> super#visit_type vars ty
     end)#visitors
end

module Mono : Constraint = struct
  open Restriction

     module MonoPredicate = struct
       class klass = object
         inherit type_predicate as super

         method! type_satisfies vars = function
           | `ForAll _ -> false
           | t -> super#type_satisfies vars t
       end
     end

  let type_satisfies, row_satisfies = make_restriction_predicate (module MonoPredicate) Session false

  let can_type_be, can_row_be =
    (object
       inherit MonoPredicate.klass

       method! var_satisfies = function
         | (_, (_, Mono), _) -> true
         | (_, _, `Rigid) -> true
         | (_, (_, sk), `Flexible) ->
              (* Mono is substantially more lax - we just require that we can unify with any subkind *)
              match Restriction.min sk Mono with
              | Some _ -> true
              | None -> false
     end)#predicates

  let make_type, make_row = make_restriction_transform ~ensure:true Mono
end

let get_restriction_constraint : Restriction.t -> (module Constraint) option =
  let open Restriction in
  function
  | Any | Effect -> None
  | Base -> Some (module Base)
  | Session -> Some (module Session)
  | Mono -> Some (module Mono)

type datatype = typ [@@deriving show]

(* useful for debugging: types tend to be too big to read *)
(*
*)
(*let pp_datatype = fun f _ -> Utility.format_omission f
let pp_field_spec = fun f _ -> Utility.format_omission f
let pp_field_spec_map = fun f _ -> Utility.format_omission f
let pp_row_var = fun f _ -> Utility.format_omission f
let pp_row = fun f _ -> Utility.format_omission f
let pp_meta_type_var = fun f _ -> Utility.format_omission f
let pp_meta_row_var = fun f _ -> Utility.format_omission f*)

let type_var_number = var_of_quantifier

module Env = Env.String


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

  let type_arg_of_quantifier : quantifier -> type_arg =
    fun (var, (pk, sk)) ->
    let open PrimaryKind in
    match pk with
    | Type     -> `Type (make_rigid_type_variable var sk)
    | Row      -> `Row (StringMap.empty, make_rigid_row_variable var sk, false)
    | Presence -> `Presence (make_rigid_presence_variable var sk)

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
  let fresh_session_variable linearity = make_type_variable (fresh_raw_variable ()) (linearity, res_session)

  let fresh_presence_variable subkind = make_presence_variable (fresh_raw_variable ()) subkind
  let fresh_rigid_presence_variable subkind = make_rigid_presence_variable (fresh_raw_variable ()) subkind

  let fresh_type_quantifier subkind : quantifier * datatype =
    let var = fresh_raw_variable () in
    let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
      (var, (PrimaryKind.Type, subkind)), `MetaTypeVar point

  let fresh_row_quantifier subkind : quantifier * row =
    let var = fresh_raw_variable () in
    let point = make_rigid_row_variable var subkind in
      (var, (PrimaryKind.Row, subkind)), (FieldEnv.empty, point, false)

  let fresh_presence_quantifier subkind : quantifier * field_spec =
    let var = fresh_raw_variable () in
    let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
      (var, (PrimaryKind.Presence, subkind)), `Var point

  let fresh_quantifier =
    let open CommonTypes.PrimaryKind in
    function
    | (Type, sk) ->
       let q, t = fresh_type_quantifier sk in
       q, `Type t
    | (Row , sk) ->
       let q, r = fresh_row_quantifier sk in
       q, `Row r
    | (Presence, sk) ->
       let q, p = fresh_presence_quantifier sk in
       q, `Presence p

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
                  `ForAll (qs @ qs', t')
              | t ->
                  begin
                    match qs with
                      | [] -> t
                      | _ -> `ForAll (qs, t)
                  end
          end
      | _ -> t
  in
    ct rec_names t

(** remove any redundant top-level `Vars from a presence flag. *)
let rec concrete_field_spec f =
  match f with
    | `Var point ->
        begin
          match Unionfind.find point with
            | `Var _ -> f
            | `Body f -> concrete_field_spec f
        end
    (* The following may be tempting, but can lead to an infinite loop *)
    (* | `Present t -> `Present (concrete_type IntSet.empty t) *)
    | _ -> f

let concrete_fields =
  FieldEnv.map concrete_field_spec

let free_type_vars, free_row_type_vars, free_tyarg_vars =
  let module S = TypeVarSet in
  let rec free_type_vars' : S.t -> datatype -> S.t = fun rec_vars ->
    function
      | `Not_typed               -> S.empty
      | `Primitive _             -> S.empty
      | `Function (f, m, t)      ->
         S.union_all [free_type_vars' rec_vars f; free_row_type_vars' rec_vars m; free_type_vars' rec_vars t]
      | `Lolli (f, m, t)         ->
         S.union_all [free_type_vars' rec_vars f; free_row_type_vars' rec_vars m; free_type_vars' rec_vars t]
      | `Effect row
      | `Record row
      | `Variant row             -> free_row_type_vars' rec_vars row
      | `Table (r, w, n)         ->
          S.union_all
            [free_type_vars' rec_vars r; free_type_vars' rec_vars w; free_type_vars' rec_vars n]
      | `Lens _          -> S.empty
      | `Alias ((_, _, ts), datatype) ->
          S.union (S.union_all (List.map (free_tyarg_vars' rec_vars) ts)) (free_type_vars' rec_vars datatype)
      | `Application (_, tyargs) -> S.union_all (List.map (free_tyarg_vars' rec_vars) tyargs)
      | `RecursiveApplication { r_args; _ } ->
          S.union_all (List.map (free_tyarg_vars' rec_vars) r_args)
      | `ForAll (tvars, body)    -> S.diff (free_type_vars' rec_vars body)
                                           (List.fold_right (S.add -<- type_var_number) tvars S.empty)
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
    ((free_type_vars' S.empty),
     (free_row_type_vars' S.empty),
     (free_tyarg_vars' S.empty))

type inference_type_map =
    ((datatype Unionfind.point) IntMap.t ref *
       (row Unionfind.point) IntMap.t ref)

let field_env_union : (field_spec_map * field_spec_map) -> field_spec_map =
  fun (env1, env2) ->
    FieldEnv.fold (fun label field_spec env' ->
                     FieldEnv.add label field_spec env') env1 env2

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
      | `RecursiveApplication appl ->
          `RecursiveApplication { appl with r_dual = (not appl.r_dual) }
      | `End -> `End
      (* it sometimes seems tempting to preserve aliases here, but it
         won't always work - e.g. when we use dual_type to expose a
         concrete type *)
      (* | `Alias _ as t         -> `Dual t *)
      (* Still, we might hope to find a way of preserving 'dual
         aliases' in order to simplify the pretty-printing of types... *)
      | `Alias (_, t) -> dt t
      | t -> raise (Invalid_argument ("Attempt to dualise non-session type: " ^ show_datatype t))
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
        | `Effect row -> `Effect (sdr row)
        | `Table (r, w, n) -> `Table (sdt r, sdt w, sdt n)
        | `Lens _sort -> t
        (* TODO: we could do a check to see if we can preserve aliases here *)
        | `Alias (_, t) -> sdt t
        | `Application (abs, ts) -> `Application (abs, List.map (subst_dual_type_arg rec_points) ts)
        | `RecursiveApplication app ->
            (* I don't think we need to do anything with the dualisation flag
             * here -- this should be sorted by `dual_type` above. *)
            `RecursiveApplication { app with r_args =
              List.map (subst_dual_type_arg rec_points) app.r_args }
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
                  Unionfind.change point (`Recursive (var',
                    subst_dual_type
                      (TypeVarMap.add var (false, point) rec_points) t));
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
                                                      Unionfind.fresh (`Var (var, (lin_any, res_any), `Flexible)),
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
  let field_env = concrete_fields field_env in
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
    fun rec_env ((field_env, row_var, _dual) as row) ->
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
  let field_env = concrete_fields field_env in
    (field_env, row_var, dual), rec_row




(* TODO: tidy up all this normalisation / concretisation code *)
and normalise_datatype rec_names t =
  let nt = normalise_datatype rec_names in
  let nr = normalise_row rec_names in
    match t with
      | `Not_typed
      | `Primitive _             -> t
      | `Function (f, m, t)      ->
         `Function (nt f, nr m, nt t)
      | `Lolli (f, m, t)         ->
           `Lolli (nt f, nr m, nt t)
      | `Record row              -> `Record (nr row)
      | `Variant row             -> `Variant (nr row)
      | `Effect row              -> `Effect (nr row)
      | `Table (r, w, n)         ->
          `Table (nt r, nt w, nt n)
      | `Lens sort                ->
          `Lens sort
      | `Alias ((name, qs, ts), datatype) ->
          `Alias ((name, qs, ts), nt datatype)
      | `Application (abs, tyargs) ->
          `Application (abs, List.map (normalise_type_arg rec_names) tyargs)
      | `RecursiveApplication app ->
          `RecursiveApplication { app with r_args =
            List.map (normalise_type_arg rec_names) app.r_args }
      | `ForAll ([], body) ->
         nt body
      | `ForAll (qs, body)    ->
         begin
           match nt body with
           | `ForAll (qs', body) -> `ForAll (qs @ qs', body)
           | body -> `ForAll (qs, body)
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
      | `Dual s               -> dual_type TypeVarMap.empty (nt s)
      | `End                  -> `End

and normalise_row rec_names row =
  (* WARNING:

     We cannot use unwrap_row here, as that would lead to
     non-termination.
  *)
  let fields, row_var, dual = flatten_row row in
  let closed = is_closed_row (fields, row_var, dual) in
  let fields =
    FieldEnv.fold
      (fun l f fields ->
        match f with
        (* strip absent fields from closed rows *)
        | `Absent when closed -> fields
        | _ -> FieldEnv.add l (normalise_field_spec rec_names f) fields)
      fields
      FieldEnv.empty
  in
    (fields, row_var, dual)
and normalise_type_arg rec_names type_arg =
  match type_arg with
    | `Type t -> `Type (normalise_datatype rec_names t)
    | `Row row -> `Row (normalise_row rec_names row)
    | `Presence f -> `Presence (normalise_field_spec rec_names f)

(*
  get rid of any `Body constructors inside a presence flag
*)
and normalise_field_spec rec_names f =
  match f with
    | `Var point ->
        begin
          match Unionfind.find point with
            | `Var _ -> f
            | `Body f -> normalise_field_spec rec_names f
        end
    | `Present t -> `Present (normalise_datatype rec_names t)
    | _ -> f

and normalise_fields rec_names =
  FieldEnv.map (normalise_field_spec rec_names)


let dual_type = dual_type TypeVarMap.empty
let dual_row = dual_row TypeVarMap.empty


let concrete_type = concrete_type IntSet.empty

let normalise_datatype = normalise_datatype IntSet.empty
let normalise_row = normalise_row IntSet.empty

(** building quantified types *)

let quantifier_of_type_arg =
  let open PrimaryKind in
  function
  | `Type (`MetaTypeVar point) ->
     begin
       match Unionfind.find point with
       | `Var (var, subkind, _) -> (var, (Type, subkind))
       | _ -> assert false
     end
  | `Row (fields, row_var, _dual) ->
     assert (StringMap.is_empty fields);
     begin
       match Unionfind.find row_var with
       | `Var (var, subkind, _) -> (var, (Row, subkind))
       | _ -> assert false
     end
  | `Presence (`Var point) ->
     begin
       match Unionfind.find point with
       | `Var (var, subkind, _) -> (var, (Presence, subkind))
       | _ -> assert false
     end
  | _ -> assert false

let quantifiers_of_type_args = List.map quantifier_of_type_arg

let for_all : quantifier list * datatype -> datatype = fun (qs, t) ->
  concrete_type (`ForAll (qs, t))

(* useful types *)
let unit_type     = `Record (make_empty_closed_row ())
let string_type   = `Primitive Primitive.String
let keys_type     = `Application (list, [`Type (`Application (list, [`Type string_type]))])
let char_type     = `Primitive Primitive.Char
let bool_type     = `Primitive Primitive.Bool
let int_type      = `Primitive Primitive.Int
let float_type    = `Primitive Primitive.Float
let xml_type      = `Alias (("Xml", [], []), `Application (list, [`Type (`Primitive Primitive.XmlItem)]))
let database_type = `Primitive Primitive.DB
(* Empty type, used for exceptions *)
let empty_type = `Variant (make_empty_closed_row ())


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
let show_mailbox_annotations = Basicsettings.Types.show_mailbox_annotations



type environment        = datatype Env.t
                            [@@deriving show]
type tycon_environment  = tycon_spec Env.t
                            [@@deriving show]
type typing_environment = { var_env    : environment ;
                            rec_vars   : StringSet.t ;
                            tycon_env  : tycon_environment ;
                            effect_row : row;
                            desugared  : bool }
                            [@@deriving show]

let empty_typing_environment = { var_env = Env.empty;
                                 rec_vars = StringSet.empty;
                                 tycon_env =  Env.empty;
                                 effect_row = make_empty_closed_row ();
                                 desugared = false }

let normalise_typing_environment env =
  { env with
      var_env = Env.map normalise_datatype env.var_env;
      (* what about tycon_env? *)
      effect_row = normalise_row env.effect_row }

(* Functions on environments *)
let extend_typing_environment
    {var_env = l; rec_vars = lvars; tycon_env = al; effect_row = _; desugared = _;  }
    {var_env = r; rec_vars = rvars; tycon_env = ar; effect_row = er; desugared = dr } : typing_environment =
  { var_env    = Env.extend l r
  ; rec_vars   = StringSet.union lvars rvars
  ; tycon_env  = Env.extend al ar
  ; effect_row = er
  ; desugared  = dr }



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
      | `Effect row
      | `Record row
      | `Variant row             -> make_env_r boundvars row
      | `Table (r, w, n)         -> union [make_env boundvars r; make_env boundvars w; make_env boundvars n]
      | `Lens _                  -> empties
      | `Alias ((_, _, ts), d)   -> union (List.map (make_env_ta boundvars) ts @ [make_env boundvars d])
      | `Application (_, ds)     -> union (List.map (make_env_ta boundvars) ds)
      | `RecursiveApplication { r_args ; _ } -> union (List.map (make_env_ta boundvars) r_args)
      | `ForAll (qs, t)          ->
          make_env
            (List.fold_right
               (fun q boundvars -> S.add (var_of_quantifier q) boundvars)
               qs
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
    (IntMap.map (fun _ -> fresh_rigid_type_variable (lin_any, res_any)) tenv,
     IntMap.map (fun _ -> (StringMap.empty, fresh_rigid_row_variable (lin_any, res_any), false)) renv,
     IntMap.map (fun _ -> fresh_rigid_presence_variable (lin_any, res_any)) penv)

let make_wobbly_envs datatype : datatype IntMap.t * row IntMap.t * field_spec Utility.IntMap.t =
  let tenv, renv, penv = make_fresh_envs datatype in
    (IntMap.map (fun _ -> fresh_type_variable (lin_any, res_any)) tenv,
     IntMap.map (fun _ -> (StringMap.empty, fresh_row_variable (lin_any, res_any), false)) renv,
     IntMap.map (fun _ -> fresh_presence_variable (lin_any, res_any)) penv)


(* subtyping *)
let is_sub_type, is_sub_row =
  let module S = TypeVarSet in
  let rec is_sub_type = fun rec_vars (t, t') ->
    match t, t' with
      | `Not_typed, `Not_typed -> true
      | `Primitive p, `Primitive q -> p=q
      | `Function (f, eff, t), `Function (f', eff', t') ->
          is_sub_type rec_vars (f', f)
          && is_sub_eff rec_vars (eff, eff')
          && is_sub_type rec_vars (t, t')
      | `Effect row', `Effect row
      | `Record row', `Record row
      | `Variant row, `Variant row' ->
          let lrow, _ = unwrap_row row
          and rrow, _ = unwrap_row row' in
            is_sub_row rec_vars (lrow, rrow)
      | `Table _, `Table _ -> raise (internal_error "not implemented subtyping on tables yet")
      | `Application (labs, _), `Application (rabs, _) ->
          (* WARNING:

             This assumes that abstract type parameters are all covariant -
             which happens to be true for all the built-in abstract types we
             currently support.
          *)
          (* TODO: implement variance annotations *)
          labs = rabs && assert false (* TODO: is_sub_type_tyarg *)
      | `MetaTypeVar point, `MetaTypeVar point' ->
          begin
            match Unionfind.find point, Unionfind.find point' with
              | `Var (var, _, _), `Var (var', _, _) -> var=var'
              | `Body t, _ -> is_sub_type rec_vars (t, t')
              | _, `Body t -> is_sub_type rec_vars (t, t')
              | `Recursive _, `Recursive _ ->
                  raise (internal_error "not implemented subtyping on recursive types yet")
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
      | `Alias ((name, [], []), _), `Alias ((name', [], []), _) when name=name' -> true
      | (`Alias (_, t)), t'
      | t, (`Alias (_, t')) -> is_sub_type rec_vars (t, t')
      | `ForAll _, `ForAll _ ->
          raise (internal_error "not implemented subtyping on forall types yet")
      | _, _ -> false
  (* This is like standard row sub-typing, but the field types must be invariant.
     Ultimately we might want more flexibility. For instance, we might expect
     contravariance in the type of heard messages (the 'hear' effect is only
     associated with input).
  *)
  and is_sub_eff =
    fun rec_vars ((lfield_env, lrow_var, ldual as lrow), (rfield_env, rrow_var, rdual as rrow)) ->
      assert (not ldual);
      assert (not rdual);
      let sub_fields =
        FieldEnv.fold (fun name f _ ->
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
        FieldEnv.fold (fun name f _ ->
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
          | `Recursive _, `Recursive _ ->
              raise (internal_error "not implemented subtyping on recursive rows yet")
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

let extend_row_check_duplicates fields (fields', row_var, dual) =
  let (unified_fields, has_duplicates) =
    FieldEnv.fold
      (fun name t (fields, has_duplicates) ->
        (FieldEnv.add name (`Present t) fields), has_duplicates && FieldEnv.mem name fields)
      fields
      (fields', false) in
  (unified_fields,row_var, dual), has_duplicates

let extend_row_safe fields row =
  match extend_row_check_duplicates fields row with
  | (_, true) -> None
  | (row', false) -> Some row'
let extend_row fields row =
  fst (extend_row_check_duplicates fields row)

let make_closed_row : datatype field_env -> row = fun fields ->
  (FieldEnv.map (fun t -> `Present t) fields), closed_row_var, false

let make_record_type ts = `Record (make_closed_row ts)
let make_variant_type ts = `Variant (make_closed_row ts)

let make_table_type (r, w, n) = `Table (r, w, n)
let make_endbang_type : datatype = `Alias (("EndBang", [], []), `Output (unit_type, `End))

let make_function_type : ?linear:bool -> datatype list -> row -> datatype -> datatype
  = fun ?(linear=false) args effs range ->
  if linear then
    `Lolli (make_tuple_type args, effs, range)
  else
    `Function (make_tuple_type args, effs, range)

let make_pure_function_type : datatype list -> datatype -> datatype
  = fun domain range -> make_function_type domain (make_empty_closed_row ()) range

let make_thunk_type : row -> datatype -> datatype
  = fun effs rtype ->
  make_function_type [] effs rtype



let unwrap_list_type = function
  | `Application ({Abstype.id = "List"; _}, [`Type t]) -> t
  | _ -> assert false

(* We export some of the auto-generated printers, but only
   for the types module to use them. *)

let raw_show_datatype = show_datatype
let raw_show_row = show_row
let raw_pp_tycon_spec = pp_tycon_spec
let raw_pp_datatype = pp_datatype
let raw_pp_row = pp_row
