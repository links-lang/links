open Utility
open CommonTypes

[@@@ocaml.warning "-32"] (** disable warnings about unused functions in this module**)

let internal_error message =
  Errors.internal_error ~filename:"types.ml" ~message

let tag_expectation_mismatch =
  internal_error "Type tag expectation mismatch"

module FieldEnv = Utility.StringMap
type 'a stringmap = 'a Utility.stringmap [@@deriving show]
type 'a field_env = 'a stringmap [@@deriving show]

(* type var sets *)
module TypeVarSet = struct
  include Utility.IntSet

  let add_quantifiers : Quantifier.t list -> t -> t = fun qs vars ->
    List.fold_right IntSet.add (List.map Quantifier.to_var qs) vars
end

(* type var sets *)
module TypeVarMap = Utility.IntMap

(* points *)
type 'a point = 'a Unionfind.point [@@deriving show]

type istring = string [@@deriving show,eq]

module Abstype =
struct
  type t = { id    : istring ;
             name  : istring ;
             arity : Kind.t list }
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
  type_map: ((Quantifier.t list * typ) Utility.StringMap.t);
  linearity_map: bool Utility.StringMap.t
}

(* Types *)
and rec_appl = {
  r_name: string;
  r_dual: bool;
  r_unique_name: string;
  r_quantifiers : Kind.t list;
  r_args: type_arg list;
  r_unwind: type_arg list -> bool -> typ;
  r_linear: unit -> bool option
  }
and tid = int
and typ =
  (* Unspecified kind *)
  | Not_typed
  | Var of (tid * Kind.t * Freedom.t)
  | Recursive of (tid * Kind.t * typ)
  | Alias of ((string * Kind.t list * type_arg list * bool) * typ)
  | Application of (Abstype.t * type_arg list)
  | RecursiveApplication of rec_appl
  | Meta of typ point
  (* Type *)
  | Primitive of Primitive.t
  | Function of (typ * row * typ)
  | Lolli of (typ * row * typ)
  | Record of row
  | Variant of row
  | Table of (typ * typ * typ)
  | Lens of Lens.Type.t
  | ForAll of (Quantifier.t list * typ)
  (* Effect *)
  | Effect of row
  (* Row *)
  | Row of (field_spec_map * row_var * bool)
  | Closed
  (* Presence *)
  | Absent
  | Present of typ
  (* Session *)
  | Input of (typ * session_type)
  | Output of (typ * session_type)
  | Select of row
  | Choice of row
  | Dual of typ
  | End
and t = typ
and session_type = typ
and datatype = typ
and type_arg = PrimaryKind.t * typ
and field_spec = typ
and field_spec_map = field_spec Utility.StringMap.t
and meta_type_var = typ point
and meta_row_var = row point
and meta_presence_var = typ point
and row = typ
and row' = field_spec_map * row_var * bool
and row_var = meta_row_var
   [@@deriving show]

let dummy_type = Not_typed

let is_present = function
  | Present _         -> true
  | (Absent | Meta _) -> false
  | _ ->
     failwith "Expected presence constructor."

type alias_type = Quantifier.t list * typ [@@deriving show]

type tycon_spec = [
  | `Alias of alias_type
  | `Abstract of Abstype.t
  | `Mutual of (Quantifier.t list * tygroup ref) (* Type in same recursive group *)
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
    method set_rec_vars : (meta_type_var) Utility.IntMap.t -> 'self_type

    method primitive : Primitive.t -> ('self_type * Primitive.t)
    method list : ('self_type -> 'a -> 'self_type * 'b ) -> 'a list -> ('self_type * 'b list)
    method type_args : type_arg list -> ('self_type * type_arg list)
    method typ : typ -> ('self_type * typ)
    method row : row -> ('self_type * row)
    method row_var : row_var -> ('self_type * row_var)
    method meta_type_var : meta_type_var -> ('self_type * meta_type_var)
    method meta_row_var : meta_row_var -> ('self_type * meta_row_var)
    method meta_presence_var : meta_presence_var -> ('self_type * meta_presence_var)
    method field_spec : field_spec -> ('self_type * field_spec)
    method field_spec_map : field_spec_map -> ('self_type * field_spec_map)
    method quantifier : Quantifier.t -> ('self_type * Quantifier.t)
    method type_arg : type_arg -> ('self_type * type_arg)
  end
end

(* FIXME: these will probably go when we relax the constraint that
   Var, Recursive, and Closed constructors can only appear inside a
   Meta constructor *)

(* HACK: check that this type could be the body of a typ *)
let is_type_body = function
  | (Var _ | Recursive _ | Closed) -> false
  | _ -> true
let is_row_body = is_type_body
let is_field_spec_body = is_type_body

module Transform : TYPE_VISITOR =
struct
  class visitor = object ((o : 'self_type))
    val rec_vars : (meta_type_var) IntMap.t = IntMap.empty

    method set_rec_vars rec_vars = {< rec_vars = rec_vars >}

    method primitive : Primitive.t -> ('self_type * Primitive.t) = fun p -> (o,p)
    method row : row -> ('self_type * row) =
      fun row -> o#typ row

    method meta_type_var : meta_type_var -> ('self_type * meta_type_var) =
      fun point ->
      match Unionfind.find point with
        | Recursive (var, kind, t) ->
          if IntMap.mem var rec_vars then
            o, (IntMap.find var rec_vars)
          else
            (* FIXME: seems unnecessary to freshen type variables here! *)
            let var' = fresh_raw_variable () in
            let point' = Unionfind.fresh (Var (var', kind, `Flexible)) in
            let rec_vars' = IntMap.add var point' rec_vars in
            let o = {< rec_vars = rec_vars' >} in
            let (o, t') = o#typ t in
            let o = o#set_rec_vars rec_vars in
            Unionfind.change point' (Recursive (var', kind, t'));
            (o, point')
        | Var _  -> o, point
        | Closed -> o, point
        | t ->
            let (o, t') = o#typ t in
            o, Unionfind.fresh t'

    method meta_row_var : meta_row_var -> ('self_type * meta_row_var) =
      o#meta_type_var

    method row_var : row_var -> ('self_type * row_var) = o#meta_row_var

    method meta_presence_var :  meta_presence_var -> ('self_type * meta_presence_var) =
      o#meta_type_var
    method field_spec :  field_spec -> ('self_type * field_spec) =
      fun field_spec -> o#typ field_spec

    method field_spec_map :  field_spec_map -> ('self_type * field_spec_map) =
      fun fsmap ->
      StringMap.fold
        (fun lbl fs (o, fsmap') ->
          let (o, fs) = o#field_spec fs in
          (o, StringMap.add lbl fs fsmap'))
        fsmap (o, StringMap.empty)

    method quantifier : Quantifier.t -> ('self_type * Quantifier.t) =
      fun q -> (o, q)

    method type_arg : type_arg -> ('self_type * type_arg) =
      fun (pk, t) ->
      let open PrimaryKind in
      match pk with
      | Type ->
         let (o, t') = o#typ t in (o, (Type,  t'))
      | Row ->
         let (o, r') = o#row t in (o, (Row, r'))
      | Presence ->
         let (o, p') = o#field_spec t in (o, (Presence, p'))

    method list : 'a 'b. ('self_type -> 'a -> 'self_type * 'b) -> 'a list -> ('self_type * 'b list)
      = fun f xs ->
      List.fold_right
        (fun x (o, xs') ->
          let (o, x') = f o x in
          (o, x' :: xs'))
        xs (o, [])

    method type_args : type_arg list -> ('self_type * type_arg list) =
      fun ts -> o#list (fun o -> o#type_arg) ts

    method typ : typ -> ('self_type * typ) = function
      (* Unspecified kind *)
      | Not_typed ->
         (o, Not_typed)
      | (Var _ | Recursive _ | Closed) ->
         failwith ("[0] freestanding Var / Recursive / Closed not implemented yet (must be inside Meta)")
      | Alias ((name, params, args, is_dual), t) ->
         let (o, args') = o#type_args args in
         let (o, t') = o#typ t in
         (o, Alias ((name, params, args', is_dual), t'))
      | Application (con, args) ->
         let (o, args') = o#type_args args in
         (o, Application (con, args'))
      | RecursiveApplication payload ->
         let (o, r_args) = o#type_args payload.r_args in
         (o, RecursiveApplication { payload with r_args })
      | Meta point ->
         (* FIXME: Currently the meta_type_var method handles Var,
            Recursive, and closed. Ulimately these constructors should
            be handled in this method. *)
         let (o, point') = o#meta_type_var point in
         (o, Meta point')
      (* Type *)
      | Primitive prim ->
         let (o, prim') = o#primitive prim in
         (o, Primitive prim')
      | Function (dom, eff, cod) ->
         let (o, dom') = o#typ dom in
         let (o, eff') = o#row eff in
         let (o, cod') = o#typ cod in
         (o, Function (dom', eff', cod'))
      | Lolli (dom, eff, cod) ->
         let (o, dom') = o#typ dom in
         let (o, eff') = o#row eff in
         let (o, cod') = o#typ cod in
         (o, Lolli (dom', eff', cod'))
      | Record row ->
         let (o, row') = o#row row in
         (o, Record row')
      | Variant row ->
         let (o, row') = o#row row in
         (o, Variant row')
      | Table (read, write, needed) ->
         let (o, read') = o#typ read in
         let (o, write') = o#typ write in
         let (o, needed') = o#typ needed in
         (o, Table (read', write', needed'))
      | Lens _ -> assert false (* TODO FIXME *)
      | ForAll (names, body) ->
         let (o, names') = o#list (fun o -> o#quantifier) names in
         let (o, body') = o#typ body in
         (o, ForAll (names', body'))
      (* Effect *)
      | Effect row ->
         let (o, row') = o#row row in
         (o, Effect row')
      (* Row *)
      | Row (fsp, rv, d) ->
         let (o, fsp') = o#field_spec_map fsp in
         let (o, rv') = o#row_var rv in
         (o, Row (fsp', rv', d))
      (* Presence *)
      | Present t ->
         let (o, t') = o#typ t in
         (o, Present t')
      | Absent -> (o, Absent)
      (* FIXME: change some typs to session_types *)
      | Input (t, s) ->
         let (o, t') = o#typ t in
         let (o, s') = o#typ s in
         (o, Input (t', s'))
      | Output (t, s) ->
         let (o, t') = o#typ t in
         let (o, s') = o#typ s in
         (o, Output (t', s'))
      | Dual s ->
         let (o, s') = o#typ s in
         (o, Dual s')
      | Select r ->
         let (o, r') = o#row r in
         (o, Select r')
      | Choice r ->
         let (o, r') = o#row r in
         (o, Choice r')
      | End -> (o, End)
  end
end


module ElimRecursiveTypeCyclesTransform : TYPE_VISITOR =
struct
  class visitor =
      object (o)
        inherit Transform.visitor as super


        val mu_vars =  Utility.IntSet.empty

        method! meta_type_var point =
          match Unionfind.find point with
          | Recursive (id, kind, t) ->
             if Utility.IntSet.mem id mu_vars then
               let newvar = Var (id, kind, `Rigid) in
               (* Debug.print (Printf.sprintf "Saw rec  var %d" id); *)
               (o, Unionfind.fresh newvar)
             else
               let new_mu_vars = Utility.IntSet.add id mu_vars in
               let o' =  {< mu_vars=new_mu_vars >} in
               (* Debug.print (Printf.sprintf "Added rec  var %d" id); *)
               let (_, t') = o'#typ t in (o, Unionfind.fresh (Recursive (id, kind, t')))
          | _ -> super#meta_type_var point

        method! meta_row_var point =
          match Unionfind.find point with
          | Recursive (id, kind, t) ->
             if Utility.IntSet.mem id mu_vars then
               let newvar = Var (id, kind, `Rigid) in
               (* Debug.print (Printf.sprintf "Saw rec  var %d" id); *)
               (o, Unionfind.fresh newvar)
             else
               let new_mu_vars = Utility.IntSet.add id mu_vars in
               let o' =  {< mu_vars=new_mu_vars >} in
               (* Debug.print (Printf.sprintf "Added rec  var %d" id); *)
               let (_, t') = o'#row t in (o, Unionfind.fresh (Recursive (id, kind, t')))
          | _ -> super#meta_row_var point


      end
end


module GetRecursiveApplications =
struct
  class visitor =
    object(o)
      inherit Transform.visitor as super
      val rec_appls = StringSet.empty

      method get_applications = rec_appls

      method! typ = function
        | Alias _ as a ->
            (* Don't expand aliases -- RecursiveApplications to previous type
             * groups are not of interest in this pass *)
            (o, a)
        | RecursiveApplication { r_name; r_args; _ } as ra ->
            let apps =
              List.fold_left (fun acc x ->
                let (o, _) = o#type_arg x in
                let apps = o#get_applications in
                StringSet.union acc apps) StringSet.empty r_args in
            let apps = StringSet.(union apps (singleton r_name)) in
            ({< rec_appls = apps >}, ra)
        | x -> super#typ x
    end
end


module DecycleTypes  =
struct
  let elim_recursive_type_cycles_visitor = new ElimRecursiveTypeCyclesTransform.visitor

  let datatype t = snd (elim_recursive_type_cycles_visitor#typ t)
  let row r = snd (elim_recursive_type_cycles_visitor#row r)
  let field_spec p = snd (elim_recursive_type_cycles_visitor#field_spec p)
  let type_arg ta = snd (elim_recursive_type_cycles_visitor#type_arg ta)
  let row_var rv = snd (elim_recursive_type_cycles_visitor#row_var rv)
  let quantifier q = snd (elim_recursive_type_cycles_visitor#quantifier q)

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

let primary_kind_of_type_arg : type_arg -> PrimaryKind.t = fst

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
  method var_satisfies : (int * Kind.t * Freedom.t) -> bool = fun _ -> true

  method point_satisfies : (visit_context -> typ -> bool) -> visit_context -> t point -> bool
    = fun f ((rec_appl, rec_vars, quant_vars) as vars) point ->
    match Unionfind.find point with
    | Closed -> true
    | Var ((id, _, _) as var) ->
       IntSet.mem id quant_vars || self#var_satisfies var
    | Recursive (var, _kind, t) ->
       check_rec var rec_vars true (fun rec_vars' -> f (rec_appl, rec_vars', quant_vars) t)
    | t -> f vars t

  method type_satisfies : visit_context -> typ -> bool =
    fun ((rec_appl, rec_vars, quant_vars) as vars) typ ->
    match typ with
    | Not_typed -> assert false
    | Var _ | Recursive _ | Closed ->
       failwith ("[1] freestanding Var / Recursive / Closed not implemented yet (must be inside Meta)")
    | Alias (_, t) -> self#type_satisfies vars t
    | Application (_, ts) ->
       (* This does assume that all abstract types satisfy the predicate. *)
       List.for_all (self#type_satisfies_arg vars) ts
    | RecursiveApplication { r_unique_name; r_args; r_dual; r_unwind; _ } ->
       if StringSet.mem r_unique_name rec_appl then
         List.for_all (self#type_satisfies_arg vars) r_args
       else
         let body = r_unwind r_args r_dual in
         self#type_satisfies (StringSet.add r_unique_name rec_appl, rec_vars, quant_vars) body
    | Meta point -> self#point_satisfies self#type_satisfies vars point
    | Primitive _ -> true
    | Function (a, e, r) | Lolli (a, e, r) -> self#type_satisfies vars a && self#row_satisfies vars e && self#type_satisfies vars r
    | Record r | Effect r | Variant r -> self#row_satisfies vars r
    | Table _ -> true
    | Lens _ -> true
    | ForAll (qs, t) -> self#type_satisfies (rec_appl, rec_vars, TypeVarSet.add_quantifiers qs quant_vars) t
    | Row (fields, row_var, _) ->
       let row_var = self#point_satisfies self#row_satisfies vars row_var in
       let fields = FieldEnv.for_all (fun _ f -> self#field_satisfies vars f) fields in
       row_var && fields
    | Absent -> true
    | Present t -> self#type_satisfies vars t
    | Select r | Choice r -> self#row_satisfies vars r
    | Input (a, b) | Output (a, b) -> self#type_satisfies vars a && self#type_satisfies vars b
    | Dual s -> self#type_satisfies vars s
    | End -> true

  method field_satisfies : visit_context -> field_spec -> bool =
    fun vars field_spec -> self#type_satisfies vars field_spec

  method row_satisfies : visit_context -> row -> bool =
    fun vars row -> self#type_satisfies vars row

  method type_satisfies_arg : visit_context -> type_arg -> bool =
    fun vars (pk, t) ->
    let open PrimaryKind in
    match pk with
    | Type -> self#type_satisfies vars t
    | Row -> self#row_satisfies vars t
    | Presence -> self#field_satisfies vars t

  method predicates : ((typ -> bool) * (row -> bool)) =
    (self#type_satisfies (StringSet.empty, IntSet.empty, IntSet.empty),
     self#row_satisfies (StringSet.empty, IntSet.empty, IntSet.empty))
end

(** Iterate over every node in a type.

    By default this does nothing. However, it can be extended by {!Constraint}s
    to mutate various flexible type variables. *)
class virtual type_iter = object(self)
  method visit_var : typ point -> (int * Kind.t * Freedom.t) -> unit = fun _ _ -> ()

  method visit_point : (visit_context -> typ -> unit) -> visit_context -> typ point -> unit
    = fun f ((rec_appl, rec_vars, quant_vars) as vars) point ->
    match Unionfind.find point with
    | Closed -> ()
    | Var ((id, _, _) as var) ->
       if not (IntSet.mem id quant_vars)
       then self#visit_var point var
    | Recursive (var, _kind, t) ->
       check_rec var rec_vars () (fun rec_vars' -> f (rec_appl, rec_vars', quant_vars) t)
    | t -> f vars t

  method visit_type : visit_context -> typ -> unit =
    fun ((rec_appl, rec_vars, quant_vars) as vars) typ ->
    match typ with
    (* Unspecified kind *)
    | Not_typed -> assert false
    | Var _ | Recursive _ | Closed ->
       failwith ("[2] freestanding Var / Recursive / Closed not implemented yet (must be inside Meta)")
    | Alias (_, t) -> self#visit_type vars t
    | Application (_, ts) -> List.iter (self#visit_type_arg vars) ts
    | RecursiveApplication { r_args; _ } -> List.iter (self#visit_type_arg vars) r_args
    | Meta point -> self#visit_point self#visit_type vars point
    (* Type *)
    | Primitive _ -> ()
    | Function (a, e, r) | Lolli (a, e, r) ->
       self#visit_type vars a; self#visit_row vars e; self#visit_type vars r
    | Record r | Variant r -> self#visit_row vars r
    | Table _ -> ()
    | Lens _ -> ()
    | ForAll (qs, t) ->
       self#visit_type (rec_appl, rec_vars, TypeVarSet.add_quantifiers qs quant_vars) t
    (* Effect *)
    | Effect r -> self#visit_row vars r
    (* Row *)
    | Row (fields, row_var, _dual) ->
       self#visit_point self#visit_row vars row_var;
       FieldEnv.iter (fun _ f -> self#visit_field vars f) fields
    (* Presence *)
    | Absent -> ()
    | Present t ->
       self#visit_type vars t
    (* Session *)
    | Input (a, b) | Output (a, b) -> self#visit_type vars a; self#visit_type vars b
    | Select r | Choice r -> self#visit_row vars r
    | Dual s -> self#visit_type vars s
    | End -> ()

  method visit_field : visit_context -> field_spec -> unit =
    fun vars field_spec -> self#visit_type vars field_spec

  method visit_row : visit_context -> row -> unit =
    fun vars row -> self#visit_type vars row

  method visit_type_arg : visit_context -> type_arg -> unit =
    fun vars (_pk, t) ->
    self#visit_type vars t

  method visitors : ((typ -> unit) * (row -> unit)) =
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
       | (_, (_, (_, sk)), _) when sk = restr -> true
       | (_, (_, _), `Rigid) -> false
       | (_, (_, (_, sk)), `Flexible) ->
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
       | (_, (_, (_, sk)), _) when sk = subkind -> ()
       | (v, (pk, (l, sk)), `Flexible) ->
          begin
            match Restriction.min sk subkind with
            | Some sk when sk = subkind -> Unionfind.change point (Var (v, (pk, (l, sk)), `Flexible))
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
        | Recursive _ -> false
        | _ -> super#point_satisfies f vars point

      method! type_satisfies vars = function
        (* Unspecified kind *)
        | Not_typed -> assert false
        | Var _ | Recursive _ | Closed ->
           failwith ("[3] freestanding Var / Recursive / Closed not implemented yet (must be inside Meta)")
        | Alias _  as t  -> super#type_satisfies vars t
        | (Application _ | RecursiveApplication _) -> false
        | Meta _ as t  -> super#type_satisfies vars t
        (* Type *)
        | Primitive (Bool | Int | Char | Float | String) -> true
        | Primitive _ -> false
        | (Function _ | Lolli _ | Record _ | Variant _ | Table _ | Lens _ | ForAll (_::_, _)) -> false
        | ForAll ([], t) -> super#type_satisfies vars t
        (* Effect *)
        | Effect _ as t -> super#type_satisfies vars t
        (* Row *)
        | Row _ as t -> super#type_satisfies vars t
        (* Presence *)
        | Absent -> true
        | Present _ as t -> super#type_satisfies vars t
        (* Session *)
        | Input _ | Output _ | Select _ | Choice _ | Dual _ | End -> false
    end
  end

  let type_satisfies, row_satisfies = make_restriction_predicate (module BasePredicate) Base false
  let can_type_be, can_row_be = make_restriction_predicate (module BasePredicate) Base true
  let make_type, make_row = make_restriction_transform Base
end

(* unl type stuff *)
module Unl : Constraint = struct
  class unl_predicate = object(o)
    inherit type_predicate as super

    method! type_satisfies vars = function
      (* Unspecified kind *)
      | Not_typed -> assert false
      | Var _ | Recursive _ | Closed ->
         failwith ("[4] freestanding Var / Recursive / Closed not implemented yet (must be inside Meta)")
      | Alias _ as t -> super#type_satisfies vars t
      (* We might support linear lists like this...
         but we'd need to replace hd and tl with a split operation. *)
      (* | `Application ({Abstype.id="List"}, [`Type t]) -> Unl.satisfies_type (rec_vars, quant_vars) t  *)
      | Application _ -> true (* TODO: change this if we add linear abstract types *)
      | RecursiveApplication { r_linear ; _ } ->
            (* An application is linear if the type it refers to is
             * also linear. We calculate this information in two stages.
             * The first pass (if r_linear () returns None) calculates linearity
             * *up to recursive applications*, under the assumption that every type in the
             * block is unrestricted. With this in hand, we can calculate
             * linearity information, meaning that (r_linear ()) will return (Some lin). *)
         OptionUtils.opt_app not true (r_linear ())
      | Meta _ as t -> super#type_satisfies vars t
      (* Type *)
      | Primitive _ | Function _ -> true
      | Lolli _ -> false
      | (Record _ | Variant _) as t -> super#type_satisfies vars t
      | (Table _ | Lens _) -> true
      | ForAll _ as t -> super#type_satisfies vars t
      (* Effect *)
      | Effect _  -> true
      (* Row *)
      | Row _ as t -> super#type_satisfies vars t
      (* Presence *)
      | Absent -> true
      | Present t -> o#type_satisfies vars t
      (* Session *)
      | Input _ | Output _ | Select _ | Choice _ | Dual _ | End -> false
  end

  let type_satisfies, row_satisfies =
    (object
      inherit unl_predicate
      method! var_satisfies = function
        | (_, (_, (Linearity.Unl, _)), _) -> true
        | _ -> false
    end)#predicates

  let can_type_be, can_row_be =
    (object
      inherit unl_predicate
      method! var_satisfies = function
        | (_, (_, (Linearity.Unl, _)), _) -> true
        | (_, _, `Flexible) -> true
        | (_, _, `Rigid) -> false
    end)#predicates

  let make_type, make_row = (object
     inherit type_iter as super

     method! visit_type vars = function
       (* Unspecified kind *)
       | Not_typed -> assert false
       | Var _ | Recursive _ | Closed ->
          failwith ("[5] freestanding Var / Recursive / Closed not implemented yet (must be inside Meta)")
       | Alias _ as t -> super#visit_type vars t
       | Application _ -> ()
       | RecursiveApplication _ -> ()
       | Meta _ as t -> super#visit_type vars t
       (* Type *)
       | Primitive _ -> ()
       | Function _ -> ()
       | Lolli _ -> assert false
       | (Record _ | Variant _) as t -> super#visit_type vars t
       | Table _ | Lens _ -> ()
       | ForAll _ as t -> super#visit_type vars t
       (* Effect *)
       | Effect _ -> ()
       (* Row *)
       | Row _ as t -> super#visit_type vars t
       (* Presence *)
       | Absent -> ()
       | Present _ as t -> super#visit_type vars t
       (* Session *)
       (* FIXME: the following line is patently wrong, but was present
          in the previous code and appears to be being relied upon *)
       | Dual _ as t -> super#visit_type vars t
       | (Input _ | Output _ | Select _ | Choice _ | End) -> assert false

     method! visit_var point = function
       | (_, (_, (Linearity.Unl, _)), _) -> ()
       | (v, (pk, (_, sk)), `Flexible) -> Unionfind.change point (Var (v, (pk, (lin_unl, sk)), `Flexible))
       | (_, _, `Rigid) -> assert false
   end)#visitors
end

module Session : Constraint = struct
  open Restriction

  module SessionPredicate = struct
    class klass = object
      inherit type_predicate as super

      method! type_satisfies ((rec_appls, _, _) as vars) = function
        | Not_typed -> assert false
        | Var _ | Recursive _ | Closed ->
           failwith ("[6] freestanding Var / Recursive / Closed not implemented yet (must be inside Meta)")
        | Input _ | Output _ | Dual _ | Choice _ | Select _ | End -> true
        | (Alias _ | Meta _) as t -> super#type_satisfies vars t
        | (RecursiveApplication { r_unique_name; _ }) as t ->
           if StringSet.mem r_unique_name rec_appls then
             false
           else
             super#type_satisfies vars t
        (* Row *)
        | Row _ as t -> super#type_satisfies vars t
        (* Present *)
        | Absent -> true
        | Present _ as t -> super#type_satisfies vars t
        (* Unspecified kind *)
        | Application _ -> false (* FIXME: we assume that abstract types cannot have session kind *)
        (* Type but not Session *)
        | Primitive _ | Function _ | Lolli _ | Record _ | Variant _ | Table _ | Lens _ | ForAll _ -> false
        (* Effect *)
        | Effect _  -> false
    end
  end

  let type_satisfies, row_satisfies = make_restriction_predicate (module SessionPredicate) Session false
  let can_type_be, can_row_be = make_restriction_predicate (module SessionPredicate) Session true
  let make_type, make_row =
    (object
       inherit type_iter as super

       method! visit_var point = function
         | (_, (_, (_, Session)), _) -> ()
         | (v, (pk, (l, sk)), `Flexible) ->
            begin
              match Restriction.min sk Session with
              | Some Session -> Unionfind.change point (Var (v, (pk, (l, Session)), `Flexible))
              | _ -> assert false
            end
         | (_, _, `Rigid) -> assert false

       method! visit_type vars = function
         | Input _ | Output _ | Choice _ | Select _ | Dual _ | End -> ()
         | ty -> super#visit_type vars ty
     end)#visitors
end

module Mono : Constraint = struct
  open Restriction

     module MonoPredicate = struct
       class klass = object
         inherit type_predicate as super

         method! type_satisfies vars = function
           | ForAll _ -> false
           | t -> super#type_satisfies vars t
       end
     end

  let type_satisfies, row_satisfies = make_restriction_predicate (module MonoPredicate) Session false

  let can_type_be, can_row_be =
    (object
       inherit MonoPredicate.klass

       method! var_satisfies = function
         | (_, (_, (_, Mono)), _) -> true
         | (_, _, `Rigid) -> true
         | (_, (_, (_, sk)), `Flexible) ->
              (* Mono is substantially more lax - we just require that we can unify with any subkind *)
              match Restriction.min sk Mono with
              | Some _ -> true
              | None -> false
     end)#predicates

  let make_type, make_row = make_restriction_transform ~ensure:true Mono
end

let get_restriction_constraint : Restriction.t -> (module Constraint) option =
  let open Restriction in function
  | Any | Effect -> None
  | Base -> Some (module Base)
  | Session -> Some (module Session)
  | Mono -> Some (module Mono)

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

module Env = Env.String

(* type ops stuff *)
  let empty_field_env = FieldEnv.empty
  let closed_row_var = Unionfind.fresh Closed

  let build_type_variable freedom var kind =
    Unionfind.fresh (Var (var, kind, freedom))
  let make_type_variable var subkind =
    Meta (build_type_variable `Flexible var (PrimaryKind.Type, subkind))
  let make_rigid_type_variable var subkind =
    Meta (build_type_variable `Rigid var (PrimaryKind.Type, subkind))
  let make_row_variable var subkind = build_type_variable `Flexible var (PrimaryKind.Row, subkind)
  let make_rigid_row_variable var subkind = build_type_variable `Rigid var (PrimaryKind.Row, subkind)
  let make_presence_variable var subkind =
    Meta (build_type_variable `Flexible var (PrimaryKind.Presence, subkind))
  let make_rigid_presence_variable var subkind =
    Meta (build_type_variable `Rigid var (PrimaryKind.Presence, subkind))
  let make_rigid_variable var kind =
    Meta (build_type_variable `Rigid var kind)

  let type_arg_of_quantifier : Quantifier.t -> type_arg =
    fun (var, (pk, sk)) ->
    let open PrimaryKind in
    match pk with
    | Type     -> (Type, make_rigid_type_variable var sk)
    | Row      -> (Row, Row (StringMap.empty, make_rigid_row_variable var sk, false))
    | Presence -> (Presence, make_rigid_presence_variable var sk)

  let is_closed_row : row -> bool =
    let rec is_closed rec_vars =
      function
        | Row (_, row_var, _) ->
            begin
              match Unionfind.find row_var with
                | Closed -> true
                | Var _ -> false
                | Recursive (var, _, row) ->
                   ((TypeVarSet.mem var rec_vars)
                    || (is_closed (TypeVarSet.add var rec_vars) row))
                | row ->
                   is_closed rec_vars row
            end
        | _ -> raise tag_expectation_mismatch
    in
      is_closed TypeVarSet.empty

  let get_row_var : row -> int option =
    fun row ->
    let row_var = match row with
      | Row (_, row_var, _) -> row_var
      | _ -> raise tag_expectation_mismatch
    in
    let rec get_row_var' rec_vars = function
      | Closed -> None
      | Var (var, _, _) -> Some var
      | Recursive (var, _, Row (_, row_var', _)) ->
         if TypeVarSet.mem var rec_vars
         then None
         else get_row_var' (TypeVarSet.add var rec_vars) (Unionfind.find row_var')
      | Row (_, row_var', _) ->
         get_row_var' rec_vars (Unionfind.find row_var')
      | _ -> raise tag_expectation_mismatch
    in
    get_row_var' TypeVarSet.empty (Unionfind.find row_var)

  let fresh_type_variable subkind = make_type_variable (fresh_raw_variable ()) subkind
  let fresh_rigid_type_variable subkind = make_rigid_type_variable (fresh_raw_variable ()) subkind
  let fresh_row_variable subkind = make_row_variable (fresh_raw_variable ()) subkind
  let fresh_rigid_row_variable subkind = make_rigid_row_variable (fresh_raw_variable ()) subkind
  let fresh_session_variable linearity = make_type_variable (fresh_raw_variable ()) (linearity, res_session)

  let fresh_presence_variable subkind = make_presence_variable (fresh_raw_variable ()) subkind
  let fresh_rigid_presence_variable subkind = make_rigid_presence_variable (fresh_raw_variable ()) subkind

  let fresh_type_quantifier subkind : Quantifier.t * datatype =
    let var = fresh_raw_variable () in
    let kind = (PrimaryKind.Type, subkind) in
    let point = Unionfind.fresh (Var (var, kind, `Rigid)) in
      (var, kind), Meta point

  let fresh_row_quantifier subkind : Quantifier.t * row =
    let var = fresh_raw_variable () in
    let point = make_rigid_row_variable var subkind in
      (var, (PrimaryKind.Row, subkind)), Row (FieldEnv.empty, point, false)

  let fresh_presence_quantifier subkind : Quantifier.t * field_spec =
    let var = fresh_raw_variable () in
    let kind = (PrimaryKind.Presence, subkind) in
    let point = Unionfind.fresh (Var (var, kind, `Rigid)) in
      (var, kind), Meta point

  let fresh_quantifier =
    let open PrimaryKind in
    fun (pk, sk) ->
    match pk with
    | Type     -> let q, t = fresh_type_quantifier sk in q, (Type, t)
    | Row      -> let q, r = fresh_row_quantifier sk in q, (Row, r)
    | Presence -> let q, p = fresh_presence_quantifier sk in q, (Presence, p)
    (* | (PrimaryKind.Type, sk) -> fresh_type_quantifier sk
     * | (PrimaryKind.Row , sk) -> fresh_row_quantifier sk
     * | (PrimaryKind.Presence, sk) -> fresh_presence_quantifier sk *)

let make_empty_closed_row () = Row (empty_field_env, closed_row_var, false)
let make_empty_open_row subkind = Row (empty_field_env, fresh_row_variable subkind, false)

let make_singleton_closed_row (label, field_spec) =
  Row (FieldEnv.add label field_spec empty_field_env, closed_row_var, false)
let make_singleton_open_row (label, field_spec) subkind =
  Row (FieldEnv.add label field_spec empty_field_env, fresh_row_variable subkind, false)

let is_absent_from_row label row (* (field_env, _, _ as row) *) =
  let field_env = match row with
    | Row (field_env, _, _) -> field_env
    | _ -> raise tag_expectation_mismatch
  in
  if FieldEnv.mem label field_env
  then FieldEnv.find label field_env = Absent
  else is_closed_row row

let row_with (label, f : string * field_spec) = function
  | Row (field_env, row_var, dual) ->
     Row (FieldEnv.add label f field_env, row_var, dual)
  | _ -> raise tag_expectation_mismatch

(*** end of type_basis ***)


(** Remove any redundant top-level `MetaTypeVars from a type.
    Additionally, collapse adjacent quantifiers. *)
let concrete_type rec_names t =
  let rec ct rec_names t : datatype =
    match t with
      | Meta point ->
          begin
            match Unionfind.find point with
            | t when is_type_body t -> ct rec_names t
            | Recursive (var, _kind, t) ->
               if IntSet.mem var rec_names
               then Meta point
               else ct (IntSet.add var rec_names) t
            | _ -> t
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
      | _ -> t
  in
  ct rec_names t

(** remove any redundant top-level 'Meta's from a presence flag. *)
let rec concrete_field_spec f =
  match f with
    | Meta point ->
        begin
          match Unionfind.find point with
            | Var _ -> f
            | f -> concrete_field_spec f
        end
    (* The following may be tempting, but can lead to an infinite loop *)
    (* | `Present t -> `Present (concrete_type IntSet.empty t) *)
    | _ -> f

let concrete_fields =
  FieldEnv.map concrete_field_spec

let free_type_vars, free_row_type_vars, free_tyarg_vars =
  let module S = TypeVarSet in
  let rec free_type_vars' : S.t -> datatype -> S.t = fun rec_vars t ->
    match t with
    | Not_typed               -> S.empty
    | Var _ | Recursive _ | Closed ->
       failwith ("[7] freestanding Var / Recursive / Closed not implemented yet (must be inside Meta)")
    | Primitive _             -> S.empty
    | Function (f, m, t)      ->
       S.union_all [free_type_vars' rec_vars f; free_row_type_vars' rec_vars m; free_type_vars' rec_vars t]
    | Lolli (f, m, t)         ->
       S.union_all [free_type_vars' rec_vars f; free_row_type_vars' rec_vars m; free_type_vars' rec_vars t]
    | Effect row | Record row | Variant row -> free_row_type_vars' rec_vars row
    | Table (r, w, n)         ->
       S.union_all
         [free_type_vars' rec_vars r; free_type_vars' rec_vars w; free_type_vars' rec_vars n]
    | Lens _          -> S.empty
    | Alias ((_, _, ts, _), datatype) ->
       S.union (S.union_all (List.map (free_tyarg_vars' rec_vars) ts)) (free_type_vars' rec_vars datatype)
    | Application (_, tyargs) -> S.union_all (List.map (free_tyarg_vars' rec_vars) tyargs)
    | RecursiveApplication { r_args; _ } ->
       S.union_all (List.map (free_tyarg_vars' rec_vars) r_args)
    | ForAll (tvars, body)    -> S.diff (free_type_vars' rec_vars body)
                                   (TypeVarSet.add_quantifiers tvars S.empty)
    | Meta point       ->
       begin
         match Unionfind.find point with
         | Closed -> S.empty
         | Var (var, _, _) -> S.singleton(var)
         | Recursive (var, _kind, body) ->
            if S.mem var rec_vars
            then S.empty
            else free_type_vars' (S.add var rec_vars) body
         | t ->
            free_type_vars' rec_vars t
       end
    | Row (field_env, row_var, _) ->
       let free_field_type_vars =
         FieldEnv.fold
           (fun _ t free_type_vars ->
             S.union (free_type_vars' rec_vars t) free_type_vars)
           field_env S.empty
       in
       S.union (free_type_vars' rec_vars (Meta row_var)) free_field_type_vars
    | Absent -> S.empty
    | Present t -> free_type_vars' rec_vars t
    | Input (t, s) | Output (t, s) -> S.union (free_type_vars' rec_vars t) (free_type_vars' rec_vars s)
    | Select fields | Choice fields -> free_row_type_vars' rec_vars fields
    | Dual s -> free_type_vars' rec_vars s
    | End -> S.empty
  (* and free_field_spec_type_vars' : S.t -> field_spec -> S.t =
   *   fun rec_vars field_spec -> free_type_vars' rec_vars field_spec *)
  and free_row_type_vars' : S.t -> row -> S.t =
    fun rec_vars row -> free_type_vars' rec_vars row
  and free_tyarg_vars' : S.t -> type_arg -> S.t =
    fun rec_vars (_, t) -> free_type_vars' rec_vars t
  in
    ((free_type_vars' S.empty),
     (free_row_type_vars' S.empty),
     (free_tyarg_vars' S.empty))

type inference_type_map =
    ((datatype Unionfind.point) IntMap.t ref *
       (row Unionfind.point) IntMap.t ref)

let field_env_union : (field_spec_map * field_spec_map) -> field_spec_map =
  fun (env1, env2) ->
  FieldEnv.fold
    (fun label field_spec env' ->
      FieldEnv.add label field_spec env')
    env1 env2

let is_canonical_row_var row_var =
  match Unionfind.find row_var with
    | Closed
    | Var _ -> true
    | _ -> false

let is_rigid_row : row -> bool =
  fun row ->
  let rec is_rigid rec_vars = function
    | Row (_, row_var, _) ->
       begin match Unionfind.find row_var with
       | Closed | Var (_, _, `Rigid) -> true
       | Var (_, _, `Flexible) -> false
       | Recursive (var, _kind, row) ->
          ((TypeVarSet.mem var rec_vars) || (is_rigid (TypeVarSet.add var rec_vars) row))
       | row ->
          is_rigid rec_vars row
       end
    | _ -> raise tag_expectation_mismatch
  in
  is_rigid TypeVarSet.empty row

(* is_rigid_row_with_var var row
   returns true if row is rigid and has var as its row var
*)
let is_rigid_row_with_var : int -> row -> bool =
  fun var row ->
  let rec is_rigid rec_vars = function
    | Row (_, row_var, _) ->
       begin match Unionfind.find row_var with
       | Closed | Var (_, _, `Flexible) -> false
       | Var (var', _, `Rigid) -> var = var'
       | Recursive (var', _kind, row) ->
          ((TypeVarSet.mem var' rec_vars) || (is_rigid (TypeVarSet.add var' rec_vars) row))
       | row ->
          is_rigid rec_vars row
       end
    | _ -> raise tag_expectation_mismatch
  in
  is_rigid TypeVarSet.empty row


let is_flattened_row : row -> bool =
  fun row ->
  let rec is_flattened rec_vars = function
    | Row (_, row_var, _) ->
      begin match Unionfind.find row_var with
        | Closed | Var _ -> true
        | Recursive (var, _kind, rec_row) ->
            if TypeVarSet.mem var rec_vars then true
            else is_flattened (TypeVarSet.add var rec_vars) rec_row
        | _ -> false
      end
    | _ -> raise tag_expectation_mismatch
  in
  is_flattened TypeVarSet.empty row

let is_empty_row : row -> bool =
  fun row ->
  let rec is_empty rec_vars = function
    | Row (field_env, row_var, _) ->
       FieldEnv.is_empty field_env &&
         begin
           match Unionfind.find row_var with
           | Closed | Var _ -> true
           | Recursive (var, _kind, _) when TypeVarSet.mem var rec_vars -> true
           | Recursive (var, _kind, rec_row) -> is_empty (TypeVarSet.add var rec_vars) rec_row
           | row -> is_empty rec_vars row
         end
    | _ -> raise tag_expectation_mismatch
  in
  is_empty TypeVarSet.empty row



type var_map = (bool * meta_type_var) TypeVarMap.t

let rec dual_type : var_map -> datatype -> datatype =
  fun rec_points t ->
  let dt s = dual_type rec_points s in
  let sdt t = subst_dual_type rec_points t in
  match t with
  | Input (t, s) -> Output (sdt t, dt s)
  | Output (t, s) -> Input (sdt t, dt s)
  | Select row -> Choice (dual_row rec_points row)
  | Choice row -> Select (dual_row rec_points row)
  | Meta point ->
     begin
       match Unionfind.find point with
       | Closed -> assert false (* TODO: check that this can never happen *)
       | Var _ -> Dual (Meta point)
       | Recursive (var, kind, t) ->
          if TypeVarMap.mem var rec_points
          then Meta (snd (TypeVarMap.find var rec_points))
          else let var' = fresh_raw_variable () in
               let point = Unionfind.fresh (Recursive (var', kind, dummy_type)) in
               Unionfind.change point (Recursive (var', kind, dual_type (TypeVarMap.add var (true, point) rec_points) t));
               Meta point
       | s -> dt s
     end
  | Dual s ->
     (* TODO: is this correct? *)
     sdt s
  | RecursiveApplication appl ->
     RecursiveApplication { appl with r_dual = (not appl.r_dual) }
  | End -> End
  | Alias ((f,ks,args,isdual),t)         -> Alias ((f,ks,args,not(isdual)),dt t)
  | t -> raise (Invalid_argument ("Attempt to dualise non-session type: " ^ show_datatype @@ DecycleTypes.datatype t))
and dual_row : var_map -> row -> row =
  fun rec_points row ->
  match fst (unwrap_row row) with
  | Row (fields, row_var, dual) ->
     let fields' =
       StringMap.map
         (function
          | Absent -> Absent
          | Present t ->
             Present (dual_type rec_points t)
          | Meta _ -> assert false (* TODO: what should happen here? *)
          | _ -> raise tag_expectation_mismatch)
         fields
     in
     Row (fields', row_var, not dual)
  | _ -> raise tag_expectation_mismatch

and subst_dual_type : var_map -> datatype -> datatype =
  fun rec_points t ->
  let sdt t = subst_dual_type rec_points t in
  let sdr r = subst_dual_row rec_points r in
  match t with
  | Not_typed | Primitive _ -> t
  | Var _ | Recursive _ | Closed ->
     failwith ("[8] freestanding Var / Recursive / Closed not implemented yet (must be inside Meta)")
  | Function (f, m, t) -> Function (sdt f, sdr m, sdt t)
  | Lolli (f, m, t) -> Lolli (sdt f, sdr m, sdt t)
  | Record row -> Record (sdr row)
  | Variant row -> Variant (sdr row)
  | Effect row -> Effect (sdr row)
  | Table (r, w, n) -> Table (sdt r, sdt w, sdt n)
  | Lens _sort -> t
  (* TODO: we could do a check to see if we can preserve aliases here *)
  | Alias (_, t) -> sdt t
  | Application (abs, ts) -> Application (abs, List.map (subst_dual_type_arg rec_points) ts)
  | RecursiveApplication app ->
     (* I don't think we need to do anything with the dualisation flag
      * here -- this should be sorted by `dual_type` above. *)
     RecursiveApplication { app with r_args =
                                       List.map (subst_dual_type_arg rec_points) app.r_args }
  | ForAll (qs, body) -> ForAll (qs, sdt body)
  | Meta point ->
     begin
       match Unionfind.find point with
       | Closed -> t
       | Var _ -> t
       | Recursive (var, kind, t) ->
          if TypeVarMap.mem var rec_points then
            let (dual, point) = TypeVarMap.find var rec_points in
            if dual then Dual (Meta point)
            else Meta point
          else
            let var' = fresh_raw_variable () in
            let point = Unionfind.fresh (Recursive (var', kind, dummy_type)) in
            Unionfind.change point (Recursive (var', kind,
                                               subst_dual_type
                                                 (TypeVarMap.add var (false, point) rec_points) t));
            Meta point
       | s -> sdt s
     end
  | (Row _) as row -> subst_dual_row rec_points row
  | (Present _ | Absent) as t -> subst_dual_field_spec rec_points t
  | Input (t, s) -> Input (sdt t, sdt s)
  | Output (t, s) -> Output (sdt t, sdt s)
  | Select row -> Select (sdr row)
  | Choice row -> Choice (sdr row)
  | Dual s ->
     begin
       match sdt s with
       | Dual s' -> s'
       | s' -> Dual s'
     end
  | End                  -> End
and subst_dual_row : var_map -> row -> row =
  (* TODO: check that it's OK to not bother traversing the row_var *)
  fun rec_points row ->
  match fst (unwrap_row row) with
  | Row (fields, row_var, dual) ->
     let fields' =
       StringMap.map
         (subst_dual_field_spec rec_points)
         fields
     in
     Row (fields', row_var, dual)
  | _ -> raise tag_expectation_mismatch
and subst_dual_field_spec : var_map -> field_spec -> field_spec =
  fun rec_points field_spec ->
  match field_spec with
  | Absent -> Absent
  | Present t -> Present (subst_dual_type rec_points t)
  | Meta _ -> (* TODO: what should happen here? *) assert false
  | _ -> raise tag_expectation_mismatch
and subst_dual_type_arg : var_map -> type_arg -> type_arg =
  fun rec_points (pk, t) ->
  let open PrimaryKind in
  match pk with
  | Type     -> (Type, subst_dual_type rec_points t)
  | Row      -> (Row, subst_dual_row rec_points t)
  | Presence -> (Presence, subst_dual_field_spec rec_points t)

and flatten_row : row -> row = fun row ->
  let row =
    match row with
    | Row _ -> row
    (* HACK: this probably shouldn't happen! *)
    | Meta row_var -> Row (StringMap.empty, row_var, false)
    | _ -> assert false in
  let dual_if =
    match row with
    | Row (_, _, dual) ->
       fun r -> if dual then dual_row TypeVarMap.empty r else r
    | _ ->
       Debug.print ("row: " ^ show_row row);
       raise tag_expectation_mismatch
  in
  let rec flatten_row' : meta_row_var IntMap.t -> row -> row =
    fun rec_env row ->
    match row with
    | Row (field_env, row_var, dual) ->
       let row' =
         match Unionfind.find row_var with
         | Closed | Var _ -> row
         | Recursive (var, kind, rec_row) ->
            if IntMap.mem var rec_env
            then row
            else let row_var' =
                   let fresh_var = Unionfind.fresh (Var (var, kind, `Flexible)) in
                   Unionfind.fresh (Recursive (var, kind, Row (FieldEnv.empty, fresh_var, false)))
                 in
                 let rec_row' =
                   flatten_row' (IntMap.add var row_var' rec_env) rec_row
                 in
                 Unionfind.change row_var' (Recursive (var, kind, rec_row'));
                 Row (field_env, row_var', dual)
         | row' ->
            let field_env', row_var', dual =
              match flatten_row' rec_env (dual_if row') with
              | Row (field_env, row_var, dual) -> field_env, row_var, dual
              | _ -> raise tag_expectation_mismatch
            in
            Row (field_env_union (field_env, field_env'), row_var', dual)
       in
       assert (is_flattened_row row');
       row'
    | _ -> raise tag_expectation_mismatch
  in
  let field_env, row_var, dual =
    match flatten_row' IntMap.empty row with
    | Row (field_env, row_var, dual) ->
       field_env, row_var, dual
    | _ -> raise tag_expectation_mismatch
  in
  let field_env = concrete_fields field_env in
  Row (field_env, row_var, dual)

(*
 As flatten_row except if the flattened row_var is of the form:

  `Recursive (var, body)

then it is unwrapped. This ensures that all the fields are exposed
in field_env.
 *)
and unwrap_row : row -> (row * row_var option) = function
  | Row (field_env, row_var, dual) ->
     let dual_if r = if dual then dual_row TypeVarMap.empty r else r in
     let rec unwrap_row' : meta_row_var IntMap.t -> row -> (row * row_var option) =
       fun rec_env row ->
       let field_env, row_var, _ =
         match row with
         | Row (field_env, row_var, dual) ->
            field_env, row_var, dual
         | _ -> raise tag_expectation_mismatch
       in
       let row' =
         match Unionfind.find row_var with
         | Closed | Var _ -> row, None
         | Recursive (var, kind, body) ->
            if IntMap.mem var rec_env
            then row, Some row_var
            else let point =
                   Unionfind.fresh (Recursive (var, kind, body))
                 in
                 let unwrapped_body, _ = unwrap_row' (IntMap.add var point rec_env) body in
                 Unionfind.change point (Recursive (var, kind, unwrapped_body));
                 let field_env', row_var', dual' =
                   match unwrapped_body with
                   | Row (field_env', row_var', dual') ->
                      field_env', row_var', dual'
                   | _ -> raise tag_expectation_mismatch
                 in
                 Row (field_env_union (field_env, field_env'), row_var', dual'), Some point
         | row' ->
            let (field_env', row_var', dual), rec_row =
              match unwrap_row' rec_env (dual_if row') with
              | Row (field_env', row_var', dual), rec_row ->
                 (field_env', row_var', dual), rec_row
              | _ -> raise tag_expectation_mismatch
            in
            Row (field_env_union (field_env, field_env'), row_var', dual), rec_row
       in
       assert (is_flattened_row (fst row'));
       row'
     in
     let (field_env, row_var, dual), rec_row =
       match unwrap_row' IntMap.empty (Row (field_env, row_var, dual)) with
       | Row (field_env, row_var, dual), rec_row ->
          (field_env, row_var, dual), rec_row
       | _ -> raise tag_expectation_mismatch
     in
     let field_env = concrete_fields field_env in
     Row (field_env, row_var, dual), rec_row
  | _ -> raise tag_expectation_mismatch




(* TODO: tidy up all this normalisation / concretisation code *)
and normalise_datatype rec_names t =
  let nt = normalise_datatype rec_names in
  let nr = normalise_row rec_names in
  match t with
  | Not_typed -> t
  | Var _ | Recursive _ | Closed ->
     failwith ("[9] freestanding Var / Recursive / Closed not implemented yet (must be inside Meta)")
  | Primitive _             -> t
  | Function (f, m, t)      ->
     Function (nt f, nr m, nt t)
  | Lolli (f, m, t)         ->
     Lolli (nt f, nr m, nt t)
  | Record row              -> Record (nr row)
  | Variant row             -> Variant (nr row)
  | Effect row              -> Effect (nr row)
  | Table (r, w, n)         ->
     Table (nt r, nt w, nt n)
  | Lens sort               -> Lens sort
  | Alias ((name, qs, ts, is_dual), datatype) ->
     Alias ((name, qs, ts, is_dual), nt datatype)
  | Application (abs, tyargs) ->
     Application (abs, List.map (normalise_type_arg rec_names) tyargs)
  | RecursiveApplication app ->
     RecursiveApplication { app with r_args =
                                       List.map (normalise_type_arg rec_names) app.r_args }
  | ForAll ([], body) ->
     nt body
  | ForAll (qs, body)    ->
     begin
       match nt body with
       | ForAll (qs', body) -> ForAll (qs @ qs', body)
       | body -> ForAll (qs, body)
     end
  | Meta point       ->
     begin
       match Unionfind.find point with
       | Closed -> t
       | Var _ -> t
       | Recursive (var, kind, body) ->
          if IntSet.mem var rec_names
          then t
          else let body = normalise_datatype (IntSet.add var rec_names) body in
               Unionfind.change point (Recursive (var, kind, body));
               Meta point
       | t -> nt t
     end
  | (Row _) as row ->
     (* WARNING: We cannot use unwrap_row here, as that would lead
        to non-termination.  *)
     let fields, row_var, dual =
       match flatten_row row with
       | Row (fields, row_var, dual) -> fields, row_var, dual
       | _ -> raise tag_expectation_mismatch
     in
     let closed = is_closed_row (Row (fields, row_var, dual)) in
     let fields =
       FieldEnv.fold
         (fun l f fields ->
           match f with
           (* strip absent fields from closed rows *)
           | Absent when closed -> fields
           | _ -> FieldEnv.add l (normalise_field_spec rec_names f) fields)
         fields
         FieldEnv.empty
     in
     Row (fields, row_var, dual)
  | Present t -> Present (nt t)
  | Absent -> Absent
  | Input (t, s)         -> Input (nt t, nt s)
  | Output (t, s)        -> Output (nt t, nt s)
  | Select r             -> Select (nr r)
  | Choice r             -> Choice (nr r)
  | Dual s               -> dual_type TypeVarMap.empty (nt s)
  | End                  -> End

and normalise_row rec_names row = normalise_datatype rec_names row

and normalise_type_arg rec_names (pk, t) =
let open PrimaryKind in
match pk with
  | Type     -> (Type,     normalise_datatype rec_names t)
  | Row      -> (Row,      normalise_row rec_names t)
  | Presence -> (Presence, normalise_field_spec rec_names t)

and normalise_field_spec rec_names f = normalise_datatype rec_names f

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
  let quantifier_of_point point = match Unionfind.find point with
    | Var (var, kind, _) -> (var, kind)
    | _ -> assert false in
  function
  | Type, Meta point -> quantifier_of_point point
  | Row, Row (fields, point, _dual) ->
     assert (StringMap.is_empty fields);
     quantifier_of_point point
  | Presence, Meta point -> quantifier_of_point point
  (* HACK: this probably shouldn't happen *)
  | Row, Meta point -> quantifier_of_point point
  | _, _ -> assert false

let quantifiers_of_type_args = List.map quantifier_of_type_arg

let for_all : Quantifier.t list * datatype -> datatype =
  fun (qs, t) -> concrete_type (ForAll (qs, t))

(* useful types *)
let unit_type     = Record (make_empty_closed_row ())
let string_type   = Primitive Primitive.String
let keys_type     = Application (list, [(PrimaryKind.Type, Application (list, [(PrimaryKind.Type, string_type)]))])
let char_type     = Primitive Primitive.Char
let bool_type     = Primitive Primitive.Bool
let int_type      = Primitive Primitive.Int
let float_type    = Primitive Primitive.Float
let xml_type      = Alias (("Xml", [], [], false), Application (list, [(PrimaryKind.Type, Primitive Primitive.XmlItem)]))
let database_type = Primitive Primitive.DB
(* Empty type, used for exceptions *)
let empty_type    = Variant (make_empty_closed_row ())
let wild = "wild"
let hear = "hear"
let wild_present   = (wild, Present unit_type)
let hear_present t = (hear, Present t)


(* precondition: the row is unwrapped *)
let is_tuple ?(allow_onetuples=false) row =
  let field_env, row_var =
    match row with
    | Row (field_env, row_var, _) ->
       field_env, row_var
    | _ -> raise tag_expectation_mismatch
  in
  match Unionfind.find row_var with
  | Closed ->
     let n = StringMap.size field_env in
     let b =
       n = 0
       || (List.for_all
             (fun i ->
               let name = string_of_int i in
               FieldEnv.mem name field_env
               && (match FieldEnv.find (string_of_int i) field_env with
                   | Present _ -> true
                   | Absent    -> false
                   | Meta _    -> false
                   | _ -> raise tag_expectation_mismatch))
             (fromTo 1 (n+1))) (* need to go up to n *)
     (* (Samo) I think there was a bug here, calling (fromTo 1 n):
        (dis)allowing one-tuples is handled below, but here we need to make sure
        that the object is a tuple at all; if there was only one field, it
        wasn't checked for int *)
     in
     (* 0/1-tuples are displayed as records *)
     b && (allow_onetuples || n <> 1)
  | _ -> false

let extract_tuple = function
  | Row (field_env, _, _) ->
     FieldEnv.to_list (fun _ -> function
         | Present t -> t
         | Absent | Meta _ -> assert false
         | _ -> raise tag_expectation_mismatch) field_env
  | _ -> raise tag_expectation_mismatch

exception TypeDestructionError of string

(** remove any top-level meta typevars and aliases from a type
    (perhaps we can use this version of concrete_type everywhere)
 *)
let concrete_type' t =
  let rec ct rec_names t : datatype =
    match t with
    | Alias (_, t) -> ct rec_names t
    | Meta point ->
       begin
         match Unionfind.find point with
         | Var _ -> t
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
    | Dual s -> dual_type (ct rec_names s)
    | RecursiveApplication ({ r_unique_name; r_dual; r_args; r_unwind ; _ } as appl) ->
       if (RecIdSet.mem (NominalId r_unique_name) rec_names) then
         RecursiveApplication appl
       else
         let body = r_unwind r_args r_dual in
         ct (RecIdSet.add (NominalId r_unique_name) rec_names) body
    | _ -> t
  in
  ct RecIdSet.empty t

let extract_row t =
  match concrete_type' t with
  | Effect row | Record row | Variant row | Select row | Choice row -> row
  | t ->
     raise @@ TypeDestructionError
                ("Internal error: attempt to extract a row from a datatype that is not a row container: "
                 ^ show_datatype @@ DecycleTypes.datatype t)

let extract_row_parts : t -> row' = function
    | Row parts -> parts
    | t -> raise @@ TypeDestructionError
                      ("Internal error: attempt to extract row parts from a datatype that is not a row "
                       ^ show_datatype @@ DecycleTypes.datatype t)

let strip_quantifiers = function
  | ForAll (_, t) | t -> t

let show_raw_type_vars
  = Settings.(flag "show_raw_type_vars"
              |> synopsis "Print type variables as raw numbers rather than letters"
              |> convert parse_bool
              |> sync)



module Vars =
struct
  type flavour = [`Rigid | `Flexible | `Recursive]
  type kind    = PrimaryKind.t
  type scope   = [`Free | `Bound]
  type spec    = flavour * kind * int

  type vars_list = (int * (flavour * kind * scope)) list

  (* See Note [Variable names in error messages] *)
  (* We don't really care much about size of the hash table.  20 should be a
     reasonable default since error message is unlikely to have more type
     variables.  And even if it does we don't care about performance penalty
     because we're printing the error message and thus stopping the compilation
     anyway. *)
  (* TODO check this - mem leak?
     (Samo: I think it's fine, because when tyvarnames are being rebuilt in
     build_tyvar_names (see below), this hashtable gets reset *)
  let tyvar_name_map = Hashtbl.create 20
  let tyvar_name_counter = ref 0

  let varspec_of_tyvar q =
    Quantifier.to_var q, (`Rigid, Quantifier.to_primary_kind q, `Bound)

  (* find all free and bound type variables *)
  let rec free_bound_type_vars : TypeVarSet.t -> datatype -> vars_list =
    fun bound_vars t ->
    let fbtv = free_bound_type_vars bound_vars in
    match t with
    (* Unspecified kind *)
    | Not_typed -> []
    | Var _ | Recursive _ | Closed ->
       failwith ("[10] freestanding Var / Recursive / Closed not implemented yet (must be inside Meta)")
    | Alias ((_, _, ts, _), _) ->
       concat_map (free_bound_tyarg_vars bound_vars) ts
    | Application (_, tyargs) ->
       List.concat (List.map (free_bound_tyarg_vars bound_vars) tyargs)
    | RecursiveApplication { r_args; _ } ->
       List.concat (List.map (free_bound_tyarg_vars bound_vars) r_args)
    | Meta point ->
       begin
         match Unionfind.find point with
         | Closed -> []
         | Var (var, (primary_kind, _), freedom) ->
            [var, ((freedom :> flavour), primary_kind, `Free)]
         | Recursive (var, (primary_kind, _), body) ->
            if TypeVarSet.mem var bound_vars
            then [var, (`Recursive, primary_kind, `Bound)]
            else (var, (`Recursive, primary_kind, `Bound))::(free_bound_type_vars (TypeVarSet.add var bound_vars) body)
         | t -> fbtv t
       end
    (* Type *)
    | Primitive _ -> []
    | Function (f, m, t) ->
       (fbtv f) @ (free_bound_row_type_vars bound_vars m) @ (fbtv t)
    | Lolli (f, m, t) ->
       (fbtv f) @ (free_bound_row_type_vars bound_vars m) @ (fbtv t)
    | Record row
      | Variant row -> free_bound_row_type_vars bound_vars row
    | Table (r, w, n) -> (fbtv r) @ (fbtv w) @ (fbtv n)
    | Lens _ -> []
    | ForAll (tyvars, body) ->
       let bound_vars, vars =
         List.fold_left
           (fun (bound_vars, vars) tyvar ->
             let var, spec = varspec_of_tyvar tyvar in
             (TypeVarSet.add var bound_vars, (var, spec)::vars))
           (bound_vars, [])
           tyvars in
       (List.rev vars) @ (free_bound_type_vars bound_vars body)
    (* Effect *)
    | Effect row -> free_bound_row_type_vars bound_vars row
    (* Row *)
    | Row (field_env, row_var, _) ->
       let field_type_vars =
         FieldEnv.fold
           (fun _name f tvs ->
             tvs @ free_bound_field_spec_type_vars bound_vars f)
           field_env [] in
       let row_var = free_bound_row_var_vars bound_vars row_var in
       field_type_vars @ row_var
    (* Presence *)
    | Present t -> free_bound_type_vars bound_vars t
    | Absent -> []
    (* Session *)
    | Input (t, s) | Output (t, s) ->
       free_bound_type_vars bound_vars t @ free_bound_type_vars bound_vars s
    | Select row | Choice row -> free_bound_row_type_vars bound_vars row
    | Dual s -> free_bound_type_vars bound_vars s
    | End -> []
  and free_bound_field_spec_type_vars bound_vars =
    free_bound_type_vars bound_vars
  and free_bound_row_type_vars bound_vars row =
    free_bound_type_vars bound_vars row
  and free_bound_row_var_vars bound_vars row_var =
    free_bound_type_vars bound_vars (Meta row_var)
  and free_bound_tyarg_vars bound_vars (pk, t) =
    let open PrimaryKind in
    match pk with
    | Type     -> free_bound_type_vars bound_vars t
    | Row      -> free_bound_row_type_vars bound_vars t
    | Presence -> free_bound_field_spec_type_vars bound_vars t

  let free_bound_quantifier_vars quant =
    let var, spec = varspec_of_tyvar quant in
    [(var, spec)]

  let free_bound_tycon_vars bound_vars tycon_spec =
    let split_vars tyvars =
      let bound_vars, vars =
        List.fold_left
          (fun (bound_vars, vars) tyvar ->
            let var, spec = varspec_of_tyvar tyvar in
            (TypeVarSet.add var bound_vars, (var, spec)::vars)) (bound_vars, []) tyvars in
      (bound_vars, List.rev vars) in
    match tycon_spec with
      | `Alias (tyvars, body) ->
          let (bound_vars, vars) = split_vars tyvars in
          vars @ (free_bound_type_vars bound_vars body)
      | `Mutual (tyvars, _) -> snd (split_vars tyvars)
      | `Abstract _ -> []

  let init (flavour, kind, scope) name =
    match scope with
      | `Free  -> (name, (flavour, kind, 1))
      | `Bound -> (name, (flavour, kind, 0))

  let combine (name, (flavour, kind, count)) (flavour', kind', scope) =
    assert (flavour = flavour');
    assert (kind    = kind'   );
    match scope with
      | `Free  -> (name, (flavour, kind, count+1))
      | `Bound -> (name, (flavour, kind, count))

  (* Generates next letter to use as a type variable name.  Uses side effects to
     increment variable counter.  This ensures that the next call generates next
     letter of the alphabet. *)
  let next_letter : unit -> string = function _ ->
    let first_letter      = int_of_char 'a' in
    let last_letter       = int_of_char 'z' in
    let num_letters       = last_letter - first_letter + 1 in
    let string_of_ascii n = Char.escaped (char_of_int n) in
    let letter n = string_of_ascii (first_letter + (n mod num_letters)) in
    let rec num_to_letters n =
      letter n ^ (if n >= num_letters
                  then (num_to_letters (n / num_letters))
                  else "") in
    let n = !tyvar_name_counter in
    begin
      incr tyvar_name_counter;
      num_to_letters n
    end

  (* Assigns names to type variables and adds them to a hash table storing type
     variables.  Both folds work by side-effecting on the hash table, which is
     then returned to be used freely outside of this module. *)
  let make_names (vars:vars_list) =
    if Settings.get show_raw_type_vars then
      let _ = List.fold_left
        (fun _ (var, spec) ->
           match Hashtbl.lookup tyvar_name_map var with
             | None ->
                Hashtbl.add tyvar_name_map var (init spec (string_of_int var))
             | Some (name, spec') ->
                Hashtbl.add tyvar_name_map var (combine (name, spec') spec))
        () vars
        in tyvar_name_map
    else
      begin
        let _ = List.fold_left
          (fun _ (var, spec) ->
            match Hashtbl.lookup tyvar_name_map var with
            | None -> Hashtbl.add tyvar_name_map var (init spec (next_letter ()))
            | Some (name, spec') ->
               Hashtbl.add tyvar_name_map var (combine (name, spec') spec))
          () vars
        in tyvar_name_map
      end

  let find      var tbl = fst (Hashtbl.find tbl var)
  let find_spec var tbl =      Hashtbl.find tbl var
end

(** Type printers *)

module Policy = struct
  (* Set the quantifiers to be true to display any outer quantifiers.
     Set flavours to be true to distinguish flexible type variables
     from rigid type variables. *)
  let show_quantifiers
    = Settings.(flag "show_quantifiers"
                |> synopsis "Toggles whether to display outer quantifiers (non-toplevel quantifiers will still be shown)."
                |> convert parse_bool
                |> sync)

  let show_flavours
    = Settings.(flag "show_flavours"
                |> synopsis "Toggles whether to show flavours."
                |> convert parse_bool
                |> sync)

  type kind_policy = Default | Full | Hide
  let show_kinds
    = let parse_kp v =
        match String.lowercase_ascii v with
        | "default" -> Some Default
        | "full"    -> Some Full
        | "hide"    -> Some Hide
        | _ -> raise (Invalid_argument "accepted values: default | full | hide")
      in
      let string_of_kp = function
        | None         -> "<none>"
        | Some Default -> "default"
        | Some Full    -> "full"
        | Some Hide    -> "hide"
      in
      Settings.(option ~default:(Some Default) "show_kinds"
                |> synopsis "Sets the policy for showing kinds."
                |> hint "<default|full|hide>"
                |> to_string string_of_kp
                |> convert parse_kp
                |> sync)

  let hide_fresh_type_vars
    = Settings.(flag ~default:true "hide_fresh_type_vars"
                |> synopsis "Toggles whether to hide fresh type variables."
                |> convert parse_bool
                |> sync)

  let effect_sugar
  = Settings.(flag "effect_sugar"
              |> synopsis "Toggles the effect sugar in pretty printer."
              |> convert parse_bool
              |> sync)

  type t = { quantifiers  : bool
           ; flavours     : bool
           ; hide_fresh   : bool
           ; kinds        : kind_policy
           ; effect_sugar : bool }

  let default_policy : unit -> t =
    fun () ->
    let kp = match Settings.get show_kinds with
      | None -> failwith "Invalid value of setting show_kinds."
      | Some s -> s
    in
    { quantifiers  = Settings.get show_quantifiers
    ; flavours     = Settings.get show_flavours
    ; hide_fresh   = Settings.get hide_fresh_type_vars
    ; kinds        = kp
    ; effect_sugar = Settings.get effect_sugar }

  let quantifiers : t -> bool
    = fun p -> p.quantifiers
  let flavours : t -> bool
    = fun p -> p.flavours
  let hide_fresh : t -> bool
    = fun p -> p.hide_fresh
  let kinds : t -> kind_policy
    = fun p -> p.kinds
  let effect_sugar : t -> bool
    = fun p -> p.effect_sugar

  let set_quantifiers : bool -> t -> t
    = fun v p -> { p with quantifiers = v }
  let set_flavours : bool -> t -> t
    = fun v p -> { p with flavours = v }
  let set_hide_fresh : bool -> t -> t
    = fun v p -> { p with hide_fresh = v }
  let set_kinds : kind_policy -> t -> t
    = fun v p -> { p with kinds = v }
  let set_effect_sugar : bool -> t -> t
    = fun v p -> { p with effect_sugar = v }
end

type names = (tid, string * Vars.spec) Hashtbl.t

module type PRETTY_PRINTER = sig
  val string_of_datatype : Policy.t -> names -> datatype -> string
  val string_of_type_arg : Policy.t -> names -> type_arg -> string
  val string_of_row_var  : Policy.t -> names -> row_var -> string
  val string_of_tycon_spec : Policy.t -> names -> tycon_spec -> string
  val string_of_quantifier : Policy.t -> names -> Quantifier.t -> string
  val string_of_presence : Policy.t -> names -> field_spec -> string
end

module Print : PRETTY_PRINTER =
struct
  module BS = Basicsettings

  type policy = Policy.t
  type context = { bound_vars: TypeVarSet.t; shared_effect: int option }

  let empty_context = { bound_vars = TypeVarSet.empty; shared_effect = None }

  let has_kind =
    function
    | "" -> ""
    | s -> "::" ^ s

  (** Checks that a field environment contains exactly the values passed in a
     list *)
  let fields_present_in fields values =
    FieldEnv.size fields = List.length values &&
      List.for_all (fun v -> FieldEnv.mem v fields
                             && is_present (FieldEnv.find v fields))
        values

  (** If this type may contain a shared effect. *)
  let maybe_shared_effect = function
    | Function _ | Lolli _ -> true
    | Alias ((_, qs, _, _), _) | RecursiveApplication { r_quantifiers = qs; _ } ->
       begin match ListUtils.last_opt qs with
       | Some (PrimaryKind.Row, (_, Restriction.Effect)) -> true
       | _ -> false
       end
    | _ -> false

  let context_with_shared_effect policy visit =
    let find_row_var r =
      let r =
        match fst (unwrap_row r) with
        | Row (_, r, _) -> r
        | _ -> raise tag_expectation_mismatch
      in
      begin match Unionfind.find r with
      | Var (var, _, _) -> Some var
      | _ -> None
      end
    in
    (* Find a shared effect variable from the right most arrow or type alias. *)
    let rec find_shared_var t =
      match t with
      | Function (_, _, r) | Lolli (_, _, r) when maybe_shared_effect r -> find_shared_var r
      | Function (_, e, _) | Lolli (_, e, _) -> find_row_var e
      | Alias ((_, _, ts, _), _) | RecursiveApplication { r_args = ts; _ } when maybe_shared_effect t ->
         begin match ListUtils.last ts with
         | (PrimaryKind.Row, (Row _ as r)) -> find_row_var r
         | _ -> None
         end
      | _ -> None
    in
    let obj =
      object (self)
        inherit Transform.visitor as super

        val var = None
        method var = var

        method! typ typ =
          match self#var with
          | None ->
             begin match find_shared_var typ with
             | Some v -> {<var = Some v>}, typ
             | None -> super#typ typ
             end
          | Some _ -> self, typ
      end
    in
    if Policy.effect_sugar policy then
      let (obj, _) = visit obj in
      { empty_context with shared_effect = obj#var }
    else
      empty_context

  let subkind : (policy * names) -> Subkind.t -> string =
    let full (l, r) = "(" ^ Linearity.to_string l ^ "," ^
                        Restriction.to_string r ^ ")"
    in
    fun (policy, _vars) ->
    match Policy.kinds policy with
    | Policy.Full -> full
    | Policy.Hide -> (fun _ -> "")
    | Policy.Default ->
       function
       | (Linearity.Unl, Restriction.Any)     -> ""
       | (Linearity.Any, Restriction.Any)     -> "Any"
       | (Linearity.Unl, Restriction.Base)    -> Restriction.to_string res_base
       | (Linearity.Any, Restriction.Session) -> Restriction.to_string res_session
       | (Linearity.Unl, Restriction.Effect)  -> Restriction.to_string res_effect
       | (l, r) -> full (l, r)

  let kind : (policy * names) -> Kind.t -> string =
    let full (policy, vars) (k, sk) =
      PrimaryKind.to_string k ^ subkind (policy, vars) sk in
    fun (policy, vars) (k, sk) ->
    match Policy.kinds policy with
    | Policy.Full -> full (policy, vars) (k, sk)
    | Policy.Hide -> PrimaryKind.to_string k
    | Policy.Default ->
      match (k, sk) with
      | PrimaryKind.Type, (Linearity.Unl, Restriction.Any) -> ""
      | PrimaryKind.Type, (Linearity.Unl, Restriction.Base) ->
         Restriction.to_string res_base
      | PrimaryKind.Type, (Linearity.Any, Restriction.Session) ->
         Restriction.to_string res_session
      | PrimaryKind.Type, sk ->
         subkind Policy.({policy with kinds = Full}, vars) sk
      | PrimaryKind.Row, (Linearity.Unl, Restriction.Any) ->
         PrimaryKind.to_string pk_row
      | PrimaryKind.Row, (Linearity.Unl, Restriction.Effect) ->
         PrimaryKind.to_string pk_row
      | PrimaryKind.Presence, (Linearity.Unl, Restriction.Any) ->
         PrimaryKind.to_string pk_presence
      | PrimaryKind.Row, _ | PrimaryKind.Presence, _ ->
         full Policy.({policy with kinds = Full}, vars) (k, sk)

  let quantifier : (policy * names) -> Quantifier.t -> string =
    fun (policy, vars) q ->
      let k = Quantifier.to_kind q in
      Vars.find (Quantifier.to_var q) vars ^ has_kind (kind (policy, vars) k)

  (** If type variable names are hidden return a generic name n1. Otherwise
     pass name of type variable to n2 so that it can construct a name. *)
  let name_of_type_plain { bound_vars; _ } (policy, vars : policy * names) var n1 n2 =
    let name, (flavour, _, count) = Vars.find_spec var vars in
    if Policy.hide_fresh policy && count = 1
       && ((flavour = `Flexible && not (Policy.flavours policy)) || not (IntSet.mem var bound_vars))
    then n1
    else n2 name

  let name_of_type context p var k n1 n2 =
    name_of_type_plain context p var n1 n2 ^ has_kind (subkind p k)

  let rec is_row_var known (_, rv, _) =
    match Unionfind.find rv with
    | Var (var, _, _) when var = known -> true
    | Row b -> is_row_var known b
    | _ -> false

  let rec datatype : context -> policy * names -> datatype -> string =
    fun ({ bound_vars; _ } as context) ((policy, vars) as p) t ->
      let sd = datatype context p in

      let unwrap = fst -<- unwrap_row in
        (* precondition: the row is unwrapped *)
      let string_of_tuple context (field_env, _, _) =
        let tuple_env =
          FieldEnv.fold
            (fun i f tuple_env ->
               match f with
                 | Present t        -> IntMap.add (int_of_string i) t tuple_env
                 | (Absent | Meta _) -> assert false
                 | _ -> raise tag_expectation_mismatch)
            field_env
            IntMap.empty in
        let ss = List.rev (IntMap.fold (fun _ t ss -> (datatype context p t) :: ss) tuple_env []) in
          "(" ^ String.concat ", " ss ^  ")" in

      let name_of_type = name_of_type context (policy, vars) in

      let name_of_eff_var ~allows_shared var _ nh nv =
        match context.shared_effect with
        | None -> name_of_type_plain context (policy, vars) var nh nv
        | Some v ->
           if allows_shared then
             (* If we're in a context with the shared variable, try to use it
                otherwise explicitly name it. *)
             if v = var then nh
               else
                 let name, _ = Vars.find_spec var vars in
                 nv name
           else
             (* Otherwise the shared effect variable must be explicitly referred to as "_". *)
             if v = var then nv "_" else name_of_type_plain context (policy, vars) var nh nv
      in

      (* Pretty-prints an arrow effect variable *)
      let ppr_eff_var ~args ~allows_shared to_match closed
            (flex_name_hidden, flex_name)
            (name_hidden, name) =
        match Unionfind.find to_match with
        | Var (var, k, `Flexible) when Policy.flavours policy ->
           name_of_eff_var ~allows_shared var k flex_name_hidden flex_name
        | Var (var, k, _) ->
           name_of_eff_var ~allows_shared var k name_hidden name
        | Closed      -> closed
        | Recursive _ -> assert false
        | t'     -> datatype context p (Function (args, t', t))
      in

      (* Pretty-prints function spaces.
         `ah` argument stands for "arrow head", either ">" (for normal function
              space) or "@" (for linear types' space). *)
      let ppr_function_type args effects t ah ht =
        let (fields, row_var, dual) =
          match unwrap effects with
          | Row (fields, row_var, dual) ->
             (fields, row_var, dual)
          | _ -> raise tag_expectation_mismatch
        in
       assert (not dual);

       let fields_present = fields_present_in fields in
       let allows_shared = not (maybe_shared_effect t) in

       let sd = datatype context p in

       let ppr_arrow () =
         if fields_present [] then
           ppr_eff_var ~args ~allows_shared row_var ("{}-" ^ ah)
               ("-%-" ^ ah, fun name -> "-%" ^ name ^ "-" ^ ah)
               ("-" ^ ah,   fun name -> "-"  ^ name ^ "-" ^ ah)
         else if fields_present [wild]
         then
           ppr_eff_var ~args ~allows_shared row_var ("{}~" ^ ah)
               ("~%~" ^ ah, fun name -> "~%" ^ name ^ "~" ^ ah)
               ("~" ^ ah,   fun name -> "~"  ^ name ^ "~" ^ ah)
         else if fields_present [hear; wild]
         then
           let ht' = ht fields in
           ppr_eff_var ~args ~allows_shared row_var ("{:" ^ ht' ^ "}~" ^ ah)
               ("{:" ^ ht' ^ "|%}~" ^ ah, fun name -> "{:" ^ ht' ^ "|%" ^ name ^ "}~" ^ ah)
               ("{:" ^ ht' ^ "|_}~" ^ ah, fun name -> "{:" ^ ht' ^ "|"  ^ name ^ "}~" ^ ah)
         else
             (* to guarantee termination it's crucial that we
                invoke row on the original wrapped version of
                the effect row *)
           let row = row ~name:(fun _ _ -> name_of_eff_var ~allows_shared) in
           if FieldEnv.mem wild fields &&
             is_present (FieldEnv.find wild fields) then
             "{" ^ row ~strip_wild:true "," context p effects ^ "}~" ^ ah
           else
             "{" ^ row "," context p effects ^ "}-" ^ ah
         in begin match concrete_type args with
            | Record (Row row) when is_tuple ~allow_onetuples:true (Row row) ->
               (* Let bindings are needed here to ensure left-to-right
                  generation of type variable names.
                  See Note [Variable names in error messages] *)
               let row_str   = string_of_tuple context row in
               let arrow_str = ppr_arrow () in
               let sd_str    = sd t in
               row_str ^ " " ^ arrow_str ^ " " ^ sd_str
            | _ -> assert false
            end
      in match t with
         (* Unspecified kind *)
         | Not_typed       -> "not typed"
         | Var _ | Recursive _ | Closed ->
            failwith ("[11] freestanding Var / Recursive / Closed not implemented yet (must be inside Meta)")
         | Alias ((s, _, ts, is_dual), _) | RecursiveApplication { r_name = s; r_args = ts; r_dual = is_dual; _ } ->
            let ts =
              match ListUtils.unsnoc_opt ts, context.shared_effect with
              | Some (ts, (PrimaryKind.Row, (Row r as r'))), Some v when maybe_shared_effect t && is_row_var v r ->
                 let ts = List.map (type_arg context p) ts in
                 let fields =
                   match fst (unwrap_row r') with
                   | Row (fields, _, _) -> fields
                   | _ -> raise tag_expectation_mismatch
                 in
                 if StringMap.is_empty fields then
                   ts
                 else
                   let r = row ~name:(fun _ _ -> name_of_eff_var ~allows_shared:true) "," context p r' in
                   ts @ ["{" ^ r ^ "}"]
              | _ -> List.map (type_arg context p) ts
            in
            begin match ts with
            | [] -> Module_hacks.Name.prettify s
            | _ ->
               Printf.sprintf "%s%s (%s)"
                 (if is_dual then "~" else "")
                 (Module_hacks.Name.prettify s)
                 (String.concat "," ts)
            end
         | Application (l, [elems]) when Abstype.equal l list ->  "["^ (type_arg context p) elems ^"]"
         | Application (s, []) -> Abstype.name s
         | Application (s, ts) ->
            let vars = String.concat "," (List.map (type_arg context p) ts) in
            Printf.sprintf "%s (%s)" (Abstype.name s) vars
         | Meta point ->
            begin
              match Unionfind.find point with
              | Closed -> ""
              | Var (var, k, `Flexible) when Policy.flavours policy ->
                 (name_of_type var (Kind.subkind k) "%" (fun name -> "%" ^ name))
              | Var (var, k, _) ->
                 (name_of_type var (Kind.subkind k) "_" (fun name -> name))
              | Recursive (var, _kind, body) ->
                 if TypeVarSet.mem var bound_vars then
                   Vars.find var vars
                 else
                   "mu " ^ Vars.find var vars ^ " . " ^
                     datatype { context with bound_vars = TypeVarSet.add var bound_vars } p body
              | t -> sd t
            end
         (* Types *)
         | Primitive p     -> Primitive.to_string p
         | Function (args, effects, t) ->
            let ht fields =
              match FieldEnv.find hear fields with
              | Present t -> sd t
              | _          -> assert false in
            ppr_function_type args effects t ">" ht
         | Lolli    (args, effects, t) ->
            let ht fields =
              sd (match FieldEnv.find hear fields with
                  | Present t -> t
                  | _          -> assert false)
            in ppr_function_type args effects t "@" ht
         | Record r ->
            let ur = unwrap r in
            let r = match r with
              | Row (fields, row_var, dual) ->
                 fields, row_var, dual
              | _ -> raise tag_expectation_mismatch
            in
            (if is_tuple ur then string_of_tuple context r
             else "(" ^ row "," context p (Row r) ^ ")")
         | Variant r -> "[|" ^ row "|" context p r ^ "|]"
         | Table (r, w, n)   ->
            (* TODO: pretty-print this using constraints? *)
            "TableHandle(" ^
              sd r ^ "," ^
                sd w ^ "," ^
                  sd n ^ ")"
         | Lens _typ ->
            let open Lens in
            let sort = Type.sort _typ in
            let cols = Sort.present_colset sort |> Column.Set.elements in
            let fds = Sort.fds sort in
            let predicate =
              Sort.predicate sort
           |> OptionUtils.from_option (Phrase.Constant.bool true) in
            let pp_col f col =
              Format.fprintf f "%s : %a"
                (Lens.Column.alias col)
                Lens.Phrase.Type.pp_pretty (Lens.Column.typ col) in
            if Lens.Type.is_abstract _typ
            then
              if Lens.Type.is_checked _typ
              then
                Format.asprintf "LensChecked((%a), { %a })"
                  (Lens.Utility.Format.pp_comma_list pp_col) cols
                  Lens.Fun_dep.Set.pp_pretty fds
              else
                Format.asprintf "LensUnchecked((%a), { %a })"
                  (Lens.Utility.Format.pp_comma_list pp_col) cols
                  Lens.Fun_dep.Set.pp_pretty fds
            else
              Format.asprintf "Lens((%a), %a, { %a })"
                (Lens.Utility.Format.pp_comma_list pp_col) cols
                Lens.Database.fmt_phrase_dummy predicate
                Lens.Fun_dep.Set.pp_pretty fds
         | ForAll (tyvars, body) ->
            let bound_vars =
              List.fold_left
                (fun bound_vars tyvar ->
                  TypeVarSet.add (Quantifier.to_var tyvar) bound_vars)
                bound_vars tyvars
            in
            if not (Policy.flavours policy) then
              match tyvars with
              | [] -> datatype { context with bound_vars } p body
              | _ ->
                 "forall "^ mapstrcat "," (quantifier p) tyvars ^"."^ datatype { context with bound_vars } p body
            else
              "forall "^ mapstrcat "," (quantifier p) tyvars ^"."^ datatype { context with bound_vars } p body
         (* Effect *)
         | Effect r -> "{" ^ row "," context p r ^ "}"
         (* Row *)
         | Row _ as t -> "{" ^ row "," context p t ^ "}"
         (* Presence *)
         | (Present _ | Absent) as t -> presence context p t
         (* Session *)
         | Input  (t, s) -> "?(" ^ sd t ^ ")." ^ sd s
         | Output (t, s) -> "!(" ^ sd t ^ ")." ^ sd s
         | Select bs -> "[+|" ^ row "," context p bs ^ "|+]"
         | Choice bs -> "[&|" ^ row "," context p bs ^ "|&]"
         | Dual s -> "~" ^ sd s
         | End -> "End"
  and presence ({ bound_vars; _ } as context) ((policy, vars) as p) = function
      | Present t ->
        begin
          match concrete_type t with
          | Record row when is_empty_row row -> ""
          | _                                -> ":" ^ datatype context p t
        end
      | Absent -> "-"
      | Meta point ->
          begin
            let name_of_type var n1 n2 =
              let name, (_, _, count) = Vars.find_spec var vars in
              if Policy.hide_fresh policy && count = 1 && not (IntSet.mem var bound_vars) then n1
              else (n2 name) in
            match Unionfind.find point with
              | Var (var, _, `Flexible) when Policy.flavours policy ->
                 name_of_type var "{%}" (fun name -> "{%" ^ name ^ "}")
              | Var (var, _, _) ->
                 name_of_type var "{_}" (fun name -> "{" ^ name ^ "}")
              | f ->
                  presence context p f
          end
      | _ -> raise tag_expectation_mismatch

  and row ?(name=name_of_type) ?(strip_wild=false) sep context p = function
    | Row (field_env, rv, dual) ->
       (* FIXME: should quote labels when necessary, i.e., when they
          contain non alpha-numeric characters *)
       let field_strings =
         FieldEnv.fold
           (fun label f field_strings ->
             if strip_wild && label = wild then
               field_strings
             else
               (label ^ presence context p f) :: field_strings)
           field_env []
       in
       let row_var_string = row_var name sep context p rv in
       String.concat sep (List.rev (field_strings)) ^
         begin
           match row_var_string with
           | None -> ""
           | Some s -> "|"^ (if dual then "~" else "") ^ s
         end
    (* FIXME: this shouldn't happen *)
    | Meta rv ->
       Debug.print ("Row variable where row expected:"^show_datatype (Meta rv));
       row sep context ~name:name ~strip_wild:strip_wild p (Row (StringMap.empty, rv, false))
    | t ->
       failwith ("Illformed row:"^show_datatype t)
       (* raise tag_expectation_mismatch *)
  and row_var name_of_type sep ({ bound_vars; _ } as context) ((policy, vars) as p) rv =
    match Unionfind.find rv with
      | Closed -> None
      | Var (var, k, `Flexible) when Policy.flavours policy ->
         Some (name_of_type context (policy, vars) var (Kind.subkind k) "%" (fun name -> "%" ^ name))
      | Var (var, k, _) ->
         Some (name_of_type context (policy, vars) var (Kind.subkind k) "_" (fun name -> name))
      | Recursive (var, _kind, r) ->
          if TypeVarSet.mem var bound_vars then
            Some (Vars.find var vars)
          else
            Some ("(mu " ^ Vars.find var vars ^ " . " ^
                    row sep { context with bound_vars = TypeVarSet.add var bound_vars } p r ^ ")")
      | r -> Some (row sep context p r)

  and type_arg context p (pk, t) =
    let open PrimaryKind in
    match pk with
    | Type     -> datatype context p t
    | Row      -> "{ " ^ row "," context p t ^ " }"
    | Presence -> "::Presence (" ^ presence context p t ^ ")"

  let tycon_spec ({ bound_vars; _ } as context) p =
    let bound_vars tyvars =
      List.fold_left
        (fun bound_vars tyvar ->
           TypeVarSet.add (Quantifier.to_var tyvar) bound_vars)
        bound_vars tyvars
    in function
    | `Alias (tyvars, body) ->
       let ctx = { context with bound_vars = bound_vars tyvars } in
       begin
         match tyvars with
         | [] -> datatype ctx p body
         | _ -> mapstrcat "," (quantifier p) tyvars ^"."^ datatype ctx p body
       end
    | `Mutual _ -> "mutual"
    | `Abstract _ -> "abstract"

  let string_of_datatype policy names ty =
    let ctxt = context_with_shared_effect policy (fun o -> o#typ ty) in
    datatype ctxt (policy, names) ty

  let string_of_row_var policy names rvar =
    match row_var name_of_type "," empty_context (policy, names) rvar with
    | Some s -> s
    | None -> ""

  let string_of_type_arg policy names tyarg =
    let ctxt = context_with_shared_effect policy (fun o -> o#type_arg tyarg) in
    type_arg ctxt (policy, names) tyarg

  let string_of_tycon_spec policy names tycon =
    tycon_spec empty_context (policy, names) tycon

  let string_of_quantifier policy names q =
    quantifier (policy, names) q

  let string_of_presence policy names pre =
    presence empty_context (policy, names) pre
end

(* New type pretty printer (Samo) *)
module RoundtripPrinter : PRETTY_PRINTER = struct
  module Context = struct

    type ambient = Toplevel
                 | Function
                 | Linfun
                 | Presence
                 | Tuple
                 | Variant
                 | Effect
                 | Row
                 | RowVar of [`Variant | `NonVariant]
                 | Binder
                 | Type_arg
                 [@@deriving show]

    type t = { policy: Policy.t
             ; bound_vars: TypeVarSet.t
             ; tyvar_names: names
             ; ambient: ambient
             ; shared_effect: int option }

    let empty () = { policy        = Policy.default_policy ()
                   ; bound_vars    = TypeVarSet.empty
                   ; tyvar_names   = Hashtbl.create 0
                   ; ambient       = Toplevel
                   ; shared_effect = None }


    let with_policy : Policy.t -> t -> t
      =  fun policy ctxt -> { ctxt with policy }

    let with_tyvar_names : names -> t -> t
      = fun tyvar_names ctxt -> { ctxt with tyvar_names }

    let bound_vars : t -> TypeVarSet.t
      = fun { bound_vars; _ } -> bound_vars

    let bind_tyvar : tid -> t -> t
      = fun ident ({ bound_vars; _ } as ctxt) ->
      { ctxt with bound_vars = TypeVarSet.add ident bound_vars }

    let bind_tyvars : tid list -> t -> t
      = fun lst ctx ->
      List.fold_left (fun c' v' -> bind_tyvar v' c') ctx lst

    let is_tyvar_bound : tid -> t -> bool
      = fun ident ctx ->
      TypeVarSet.mem ident (bound_vars ctx)

    let tyvar_names : t -> names
      = fun { tyvar_names; _ } -> tyvar_names

    let policy : t -> Policy.t
      = fun { policy; _ } -> policy

    let ambient : t -> ambient
      = fun { ambient; _ } -> ambient

    let set_ambient : ambient -> t -> t
      = fun ambient ctxt -> { ctxt with ambient }

    let toplevel : t -> t
      = set_ambient Toplevel

    (* generator for these below *)
    let is_ambient_toplevel : t -> bool
      = fun { ambient ; _ } -> ambient = Toplevel
    let is_ambient_function : t -> bool
      = fun { ambient ; _ } -> ambient = Function
    let is_ambient_linfun : t -> bool
      = fun { ambient ; _ } -> ambient = Linfun
    let is_ambient_presence : t -> bool
      = fun { ambient ; _ } -> ambient = Presence
    let is_ambient_tuple : t -> bool
      = fun { ambient ; _ } -> ambient = Tuple
    let is_ambient_variant : t -> bool
      = fun { ambient ; _ } -> ambient = Variant
    let is_ambient_effect : t -> bool
      = fun { ambient ; _ } -> ambient = Effect
    let is_ambient_row : t -> bool
      = fun { ambient ; _ } -> ambient = Row
    let is_ambient_rowvar : t -> bool
      = fun { ambient ; _ } -> match ambient with
                               | RowVar _ -> true
                               | _ -> false
    let is_ambient_variant_rowvar : t -> bool
      = fun { ambient ; _ } -> match ambient with
                               | RowVar `Variant -> true
                               | _ -> false
    let is_ambient_binder : t -> bool
      = fun { ambient ; _ } -> ambient = Binder

    let is_ambient_type_arg : t -> bool
      = fun { ambient ; _ } -> ambient = Type_arg
  end

  module StringBuffer = struct
    type t = Buffer.t

    let create : int -> t
      = fun size -> Buffer.create size

    let to_string : t -> string
      = fun buf -> Buffer.contents buf

    let write : t -> string -> unit
      = fun buf s -> Buffer.add_string buf s
  end

  module Printer = struct
    type 'a t = Printer of (Context.t -> 'a -> StringBuffer.t -> unit)
              | Empty

    let apply : 'a t -> Context.t -> 'a -> StringBuffer.t -> unit
      = fun pr ctx thing buf ->
      match pr with
      | Empty -> ()
      | Printer pr -> pr ctx thing buf

    let concat_items : sep:string -> 'a t -> 'a list -> Context.t -> StringBuffer.t -> unit
      = fun ~sep pr items ctx buf ->
      match pr with
      | Empty -> ()
      | Printer _ ->
         let rec loop
           = fun sep pr items ctx buf ->
           begin
             match items with
             | [] -> ()
             | [last] -> apply pr ctx last buf
             | not_last :: rest ->
                apply pr ctx not_last buf;
                StringBuffer.write buf sep;
                loop sep pr rest ctx buf
           end
         in
         loop sep pr items ctx buf

    let concat_printers : sep:string -> unit t list -> Context.t -> StringBuffer.t -> unit
      = fun ~sep prs ctx buf ->
      let rec loop sep ctx buf = function
        | [] -> ()
        | [pr] -> apply pr ctx () buf
        | pr :: prs ->
           apply pr ctx () buf;
           StringBuffer.write buf sep;
           loop sep ctx buf prs
      in
      loop sep ctx buf (List.filter (function Empty -> false | _ -> true) prs)

    let seq : sep:string -> ('a t * 'b t) -> ('a * 'b) -> Context.t -> StringBuffer.t -> unit
      = fun ~sep (lp, rp) (x, y) ctx buf ->
      match lp, rp with
      | Printer _, Printer _ ->
         apply lp ctx x buf;
         StringBuffer.write buf sep;
         apply rp ctx y buf
      | Printer _, Empty ->
         apply lp ctx x buf
      | Empty, Printer _ ->
         apply rp ctx y buf
      | Empty, Empty -> ()

    let constant : string -> unit t
      = fun s ->
      Printer (fun _ctx () buf -> StringBuffer.write buf s)

    let with_value : 'a t -> 'a -> unit t
      = fun pr v ->
      Printer (fun ctx () buf -> apply pr ctx v buf)

    let with_ambient : Context.ambient -> 'a t -> 'a -> unit t
      = fun amb pr v ->
      Printer (fun ctx () buf ->
          let inner_ctx = Context.set_ambient amb ctx in
          apply pr inner_ctx v buf)

    let generate_string : 'a t -> Context.t -> 'a -> string
      = fun pr ctx v ->
      let buf = StringBuffer.create 280 in
      apply pr ctx v buf;
      StringBuffer.to_string buf
  end

  type 'a printer = 'a Printer.t

  (* For correct printing of subkinds, need to know the subkind in advance:
   * see line (1): that has to be Empty so that the :: is not printed *)
  let subkind_name : Policy.t -> Subkind.t -> unit printer
    = fun policy (lin, res) ->
    let open Printer in
    let full_name : unit printer
      = Printer (fun _ctx () buf ->
            StringBuffer.write buf "(";
            StringBuffer.write buf (Linearity.to_string lin);
            StringBuffer.write buf ",";
            StringBuffer.write buf (Restriction.to_string res);
            StringBuffer.write buf ")"
          )
    in
    match Policy.kinds policy with
    | Policy.Full -> full_name
    | Policy.Hide -> Empty
    | _ ->
       let module L = Linearity in
       let module R = Restriction in
       match (lin, res) with
       | (L.Unl, R.Any)     -> Empty (* (1) see above *)
       | (L.Any, R.Any)     -> constant "Any"
       | (L.Unl, R.Base)    -> constant @@ R.to_string res_base
       | (L.Any, R.Session) -> constant @@ R.to_string res_session
       | (L.Unl, R.Effect)  -> constant @@ R.to_string res_effect
       | _ -> full_name


  let kind_name : Context.t -> Kind.t -> unit printer
    = let open Printer in
      let module P = PrimaryKind in
      let module L = Linearity in
      let module R = Restriction in

      fun ctx ((primary, subknd) as knd) ->

      let full_name : unit printer
        = Printer (fun ctxt () buf ->
              StringBuffer.write buf (P.to_string primary);
              Printer.apply (subkind_name (Context.policy ctxt) subknd) ctxt () buf)
      in
      let policy = Context.policy ctx in
      match Policy.kinds policy, knd with
      | Policy.Full, _ -> full_name
      | Policy.Hide, _ -> constant (P.to_string primary)
      | _, (PrimaryKind.Type, (L.Unl, R.Any)) -> Empty
      | _ ->
         Printer (fun ctx () buf ->
             (* do simple cases first, match the other stuff later *)
             match primary with
             | P.Type -> begin
                 match subknd with
                 | L.Unl, R.Any     -> assert false
                 | L.Unl, R.Base    -> StringBuffer.write buf (R.to_string res_base)
                 | L.Any, R.Session -> StringBuffer.write buf (R.to_string res_session)
                 | subknd ->
                    let policy = Policy.set_kinds Policy.Full (Context.policy ctx) in
                    Printer.apply (subkind_name policy subknd) ctx () buf
               end
             | PrimaryKind.Row -> begin
                 match subknd with
                 | L.Unl, R.Any | L.Unl, R.Effect ->
                    StringBuffer.write buf (P.to_string pk_row)
                 | _ ->
                    let ctx' = Context.(with_policy Policy.(set_kinds Full (policy ctx)) ctx) in
                    Printer.apply full_name ctx' () buf
               end
             | PrimaryKind.Presence -> begin
                 match subknd with
                 | L.Unl, R.Any ->
                    StringBuffer.write buf (P.to_string pk_presence)
                 | _ ->
                    let ctx' = Context.(with_policy Policy.(set_kinds Full (policy ctx)) ctx) in
                    Printer.apply full_name ctx' () buf
               end)

  let rec strip_quantifiers : typ -> typ =
    function
    | ForAll (_, t) -> strip_quantifiers t (* recurses to strip all toplevel quantifiers (if they happen to be separate) *)
    | t -> t

  let primitive : Primitive.t printer
    = let open Printer in
      Printer (fun _ctxt prim buf ->
          StringBuffer.write buf (Primitive.to_string prim))

  let is_var_anonymous : Context.t -> tid -> bool =
    fun ctxt vid ->
    let _, (_, _, count) = Vars.find_spec vid (Context.tyvar_names ctxt) in
    (count = 1 && (Policy.hide_fresh Context.(policy ctxt))) (* we want to hide it *)
    && not (Context.is_tyvar_bound vid ctxt) (* and it is not bound (if it is bound, it has to show up *)

  let rec var : (tid * Kind.t) printer
    = let open Printer in
      Printer (fun ctx (vid, knd) buf ->
          let subknd = Kind.subkind knd in
          let var_name, (flavour, _, _) = Vars.find_spec vid (Context.tyvar_names ctx) in
          let in_binder = Context.is_ambient_binder ctx in
          (* Rules of printing vars:
           * 1) If var only appears once (count = 1) & (policy.hide_fresh = true) => only as don't-care "_" [is_unique]
           * 2) But also if the var is bound (in context.bound_vars) it has to appear (by name -> overrides (1))! [is_bound]
           * 3) If a var is flexible (and policy.flavours = true), then it gets a prefix "%" [show_flexible]
           * 4) Don't-care simplifies: "%_" => "%"
           * 5) Within Presence (Meta (Var ...)), gets surrounded by "{ ... }"
           *    (or "{% ... }" if Flexible, but the % will be contributed by rule (3)) [is_presence]
           *)

          let print_var : string printer =
            Printer (fun ctx var_name buf ->
                let is_anonymous = (not in_binder) && is_var_anonymous ctx vid in
                let show_flexible = (Policy.flavours Context.(policy ctx)) && flavour = `Flexible in
                let is_presence = (PrimaryKind.Presence = Kind.primary_kind knd) in

                (if is_presence && not (Context.is_ambient_type_arg ctx) then StringBuffer.write buf "{");
                (if show_flexible then StringBuffer.write buf "%");
                (match is_anonymous, show_flexible with
                 | true, true  -> ()
                 | true, false -> StringBuffer.write buf "_"
                 | _, _        -> StringBuffer.write buf var_name);
                (if is_presence && not (Context.is_ambient_type_arg ctx) then StringBuffer.write buf "}"))
          in
          if not in_binder
          then seq ~sep:"::" (print_var, subkind_name (Context.policy ctx) subknd) (var_name, ()) ctx buf
          else seq ~sep:"::" (print_var, kind_name ctx knd) (var_name, ()) ctx buf)

  and recursive : (tid * Kind.t * typ) printer
    = let open Printer in
      Printer (fun ctx (binder, knd, tp) buf ->
          (* assumes that the recursive variable itself shouldn't display kind information,
           * because the definition of the type itself is right there *)
          let binder_ctx = Context.with_policy Policy.(set_kinds Hide (Context.policy ctx)) ctx in
          if Context.is_tyvar_bound binder ctx
          then (* this recursive was already seen -> just need the variable name *)
            Printer.apply var binder_ctx (binder, knd) buf
          else (* this the first occurence of this mu -> print the whole type *)
            begin
              let inner_context = Context.bind_tyvar binder ctx in
              let want_parens = (Context.is_ambient_rowvar ctx) || (Context.is_ambient_variant_rowvar ctx) in
              (if want_parens then StringBuffer.write buf "(");
              StringBuffer.write buf "mu ";
              Printer.apply var binder_ctx (binder, knd) buf;
              StringBuffer.write buf ".";
              Printer.apply datatype inner_context tp buf;
              (if want_parens then StringBuffer.write buf ")");
            end)

  and alias_recapp : (string * type_arg list * bool) printer
    = let open Printer in
      Printer (fun ctx (name, arg_types, is_dual) buf ->
          (if is_dual then StringBuffer.write buf "~");
          StringBuffer.write buf (Module_hacks.Name.prettify name);
          (match arg_types with
           | [] -> ()
           | _ -> StringBuffer.write buf " (";
                  Printer.concat_items ~sep:"," type_arg arg_types ctx buf;
                  StringBuffer.write buf ")"))
  (* TODO Ignoring shared effects for now (original printer does special stuff for them in aliases/recapp) *)

  and row_parts : row' printer
    = let open Printer in
      Printer (fun ctx (rfields, rvar, rdual) buf ->
          let hide_primitive_labels = Context.is_ambient_effect ctx in

          let field_printer lbl pre printers =
            (* this allows function types like this:
             *   absent wild|hear:
             *     (a) {wild-,hear-}-> b
             *   polymorphic in the presence of wild|hear (note the arrow is tame here,
             *   wild information is available in the fields):
             *     (a) {wild{_},head{_}}-> b *)
              if lbl = wild && is_present pre && hide_primitive_labels
              then printers (* do not print wild:() *)
              else if lbl = hear && is_present pre && hide_primitive_labels
              then (Printer (fun ctx () buf -> Printer.apply presence ctx pre buf)) :: printers
              else if Context.is_ambient_tuple ctx
              then with_value presence pre :: printers
              else (Printer (fun ctx () buf ->
                        StringBuffer.write buf lbl;
                        let pre = match pre with
                          | Meta point -> Unionfind.find point
                          | _ -> pre
                        in
                        match pre with
                        | Var (v,knd,_) when Kind.primary_kind knd = PrimaryKind.Presence ->
                           Printer.apply var ctx (v, knd) buf
                        | Present _ | Absent ->
                           Printer.apply presence ctx pre buf
                        | t -> raise (internal_error ("Not present: " ^ show_datatype (DecycleTypes.datatype t)))
                   )) :: printers
          in

          let printers = List.rev (FieldEnv.fold field_printer rfields []) in
          let sep =
            let open Context in
            match ambient ctx with
            | Variant | RowVar `Variant -> "|"
            | Tuple -> ", " (* tuples require a space as well *)
            | _ -> ","
          in
          Printer.concat_printers ~sep printers ctx buf;
          match meta rvar with
          | Empty -> ()
          | (Printer _) as pr ->
             begin
               if List.length printers = 0 then
                 begin
                   match Context.ambient ctx with
                   | Context.Effect | Context.Row -> StringBuffer.write buf " |"
                   | Context.Variant -> () (* variants don't get pipe *)
                   | _ -> StringBuffer.write buf "|"
                 end
               else StringBuffer.write buf "|";
               let ctx = Context.set_ambient (if Context.is_ambient_variant ctx
                                              then Context.RowVar `Variant
                                              else Context.RowVar `NonVariant) ctx
               in
               (if rdual then StringBuffer.write buf "~");
               Printer.apply pr ctx () buf
             end)

  and row : typ printer
    = let open Printer in
      Printer (fun ctx r buf ->
          let r', before, after, new_ctx =
            let module C = Context in
            match r with
            | Record r' ->
               let unrolled =
                 match r with
                 | Row _ -> r
                 | _ -> fst (unwrap_row (extract_row r))
               in
               let is_tuple = (C.is_ambient_tuple ctx) || (is_tuple unrolled) (* not allowing onetuples by default *)
               in           r',       "(",   ")",   (if is_tuple then (C.set_ambient C.Tuple ctx) else (C.toplevel ctx))
            | Variant r  -> r,        "[|",  "|]",  (C.set_ambient C.Variant ctx)
            | Effect r   -> r,        "{",   "}",   (C.set_ambient C.Effect ctx)
            | Select r   -> r,        "[+|", "|+]", ctx (* TODO ambient Session? *)
            | Choice r   -> r,        "[&|", "|&]", ctx (* TODO ^ *)
            | Row _ as r -> r,        "",    "",    ctx
            | _ -> failwith ("[*R] Invalid row:\n" ^ show_datatype @@ DecycleTypes.datatype r)
          in
          StringBuffer.write buf before;
          (* this row will be flattened, because sometimes rows of form (e.g. in the case of
             a record) Record(Row( | Row(l_j : P_j | rho))) appear, and unflattened they
             would get printed as: (|l_j:P_j|rho) because a Row itself gets no parentheses
             (it would be ambiguous which ones to get, because this can appear in any
             row-based type, nonetheless the case of Row in a Row-based type does occur in
             the internal representation, and so it must be allowed *)
          Printer.apply row_parts new_ctx (extract_row_parts (flatten_row r')) buf;
          StringBuffer.write buf after
        )

  (* returns the presence, possibly without the colon *)
  and presence : typ printer
    = let open Printer in
      Printer (fun ctx tp buf ->
          (match tp with
           | Absent ->
              StringBuffer.write buf "-"
           | Present tp ->
              (* Nullary variant payloads do not get printed. *)
              let is_nullary = concrete_type tp = unit_type in
              let inside_variant =
                Context.is_ambient_variant ctx (* plain variant *)
                || Context.is_ambient_variant_rowvar ctx (* recursive row variable in a variant row. *)
              in
              if not (is_nullary && inside_variant)
              then ((if not (Context.is_ambient_tuple ctx) then StringBuffer.write buf ":");
                    Printer.apply datatype (Context.set_ambient Context.Presence ctx) tp buf)
           | Meta pt ->
              let () =
                (* We need to emit the colon if the point is a
                   concrete type. Here Present is a special case as it
                   will be handled above by the call to `presence`
                   inside `meta`. *)
                match Unionfind.find pt with
                | Var _ | Recursive _ | Absent | Present _ -> ()
                | _ -> StringBuffer.write buf ":"
              in
              let ctx' =
                if Context.is_ambient_type_arg ctx then ctx
                else Context.(set_ambient Presence ctx)
              in
              Printer.apply (meta pt) ctx'  () buf
           | _ -> raise tag_expectation_mismatch))

  and meta : typ point -> unit printer
    = let open Printer in
      fun pt ->
      match Unionfind.find pt with
      | Closed -> (* nothing happens; TODO (future) but maybe something should *sometimes* happen *)
         Empty
      | Var (id, knd, _) -> with_value var (id, knd)
      | Recursive r -> with_value recursive r
      | t -> with_value datatype t

  and type_arg : type_arg printer
    = let open Printer in
      Printer (fun ctx (pknd, r) buf ->
          let module P = PrimaryKind in
          match pknd with
          | P.Type -> Printer.apply datatype ctx r buf
          | P.Row -> begin
              StringBuffer.write buf "{";
              let ctx = Context.set_ambient Context.Row ctx in
              (match r with
               | Row rp  -> Printer.apply row_parts ctx rp buf
               | Meta pt -> Printer.apply (meta pt) ctx () buf
               | _ -> raise tag_expectation_mismatch);
              StringBuffer.write buf "}";
            end
          | P.Presence ->
             let ctx' = Context.(set_ambient Type_arg ctx) in
             StringBuffer.write buf "{";
             Printer.apply presence ctx' r buf;
             StringBuffer.write buf "}";)

  and application : (Abstype.t * type_arg list) printer
    = let open Printer in
      Printer (fun ctx p buf ->
          match p with
          | (abstp, [el]) when Abstype.equal abstp list ->
             StringBuffer.write buf "[";
             Printer.apply type_arg ctx el buf;
             StringBuffer.write buf "]"
          | (abstp, []) ->
             StringBuffer.write buf (Abstype.name abstp)
          | (abstp, args) ->
             StringBuffer.write buf (Abstype.name abstp);
             StringBuffer.write buf " (";
             Printer.concat_items ~sep:"," type_arg args ctx buf;
             StringBuffer.write buf ")"
        )

  and func : (typ * row * typ) printer
    = let open Printer in
      let func_arrow : row printer
        = let is_field_present fields lbl =
            (* The row will NOT be unrolled, which means only the immediately visible wild will have
               any effect on the function arrow. Any wilds hidden in recursive row variables will be
               visible there, this is intentional: the printed type will more accurately represent
               what the internal type is. *)
            match FieldEnv.lookup lbl fields with
            | Some (Present _) -> true
            | None | Some Absent | Some (Meta _) -> false
            | _ -> raise tag_expectation_mismatch
          in
          Printer (fun ctx r buf ->
              let is_lolli = Context.is_ambient_linfun ctx in
              (* flatten here in case there are nested row variables,
                 e.g. {  | { wild:() } } *)
              let (fields, rvar, _) as r' = extract_row_parts (flatten_row r) in
              let is_wild = is_field_present fields wild in
              let visible_fields = (FieldEnv.size fields) - (if is_wild then 1 else 0) in
              let row_var_exists =
                match meta rvar with
                | Empty -> false
                | _ -> true
              in
              if visible_fields = 0
              then if not row_var_exists
                   then StringBuffer.write buf "{}" (* empty closed row *)
                   else (* empty open row use the abbreviated notation
                           -a- or ~a~ unless it's anonymous in which
                           case we skip it entirely *)
                     match Unionfind.find rvar with
                     | Var (vid, knd, _) ->
                        if is_var_anonymous ctx vid
                        then () (* skip printing it entirely *)
                        else begin
                            (if is_wild
                             then StringBuffer.write buf "~"
                             else StringBuffer.write buf "-");
                            let ctx =
                              Context.(set_ambient Effect
                                         (with_policy Policy.(set_kinds Hide (policy ctx)) ctx))
                            in
                            Printer.apply var ctx (vid, knd) buf
                          end
                     | _ ->
                        begin (* special case, construct row syntax, but only call the inside *)
                          StringBuffer.write buf "{";
                          Printer.apply row_parts (Context.set_ambient Context.Effect ctx) r' buf;
                          StringBuffer.write buf "}"
                        end
              else begin (* need the full effect row, but only construct the inside of it *)
                StringBuffer.write buf "{";
                Printer.apply row_parts (Context.set_ambient Context.Effect ctx) r' buf;
                StringBuffer.write buf "}";
                end;
              (if is_wild then StringBuffer.write buf "~" else StringBuffer.write buf "-");
              (* add the arrowhead/lollipop *)
              (if is_lolli then StringBuffer.write buf "@" else StringBuffer.write buf ">")
            )
      in

      (* func starts here *)
      Printer (fun ctx (domain, effects, range) buf ->
          (* build up the function type string: domain, arrow with effects, range *)
          Printer.apply row (Context.set_ambient Context.Tuple ctx) domain buf; (* function domain is always a Record *)
          StringBuffer.write buf " ";
          Printer.apply func_arrow ctx effects buf;
          StringBuffer.write buf " ";
          Printer.apply datatype ctx range buf;
        )

  and session_io : typ printer
    = let open Printer in
      Printer (fun ctx tp buf ->
          let t_char = match tp with
            | Input _ -> "?"
            | Output _ -> "!"
            | _ -> raise tag_expectation_mismatch (* this will never happen, because the function session_io
                                                   *  will only ever be called for Input | Output *)
          in
          match tp with
          | Input (tp, session_tp) | Output (tp, session_tp) ->
             StringBuffer.write buf t_char;
             StringBuffer.write buf "(";
             Printer.apply datatype ctx tp buf;
             StringBuffer.write buf ").";
             Printer.apply datatype ctx session_tp buf
          | _ -> () (* same as above, no error here because it would have already failed before *)
        )

  and session_dual : typ printer
    = let open Printer in
      Printer (fun ctx tp buf ->
          let dtype = with_value datatype tp in
          StringBuffer.write buf "~";
          match tp with
          | Input _ | Output _ | Select _ | Choice _ ->
             StringBuffer.write buf "(";
             Printer.apply dtype ctx () buf;
             StringBuffer.write buf ")"
          | _ -> Printer.apply dtype ctx () buf
        )

  and quantifier : Quantifier.t printer
    = let open Printer in
      Printer (fun ctx qr buf ->
          let vid = Quantifier.to_var qr in
          let knd = Quantifier.to_kind qr in
          Printer.apply var ctx (vid, knd) buf)

  and forall : (Quantifier.t list * typ) printer
    = let open Printer in
      Printer (fun ctx (binding, tp) buf ->
          (* toplevel quantifiers were already stripped by the calling string_of_datatype (provided we started there; if we
             started in another string_of_X, then it's not toplevel anyway and quantifiers should stay) *)
          let binder_ctx = Context.set_ambient Context.Binder ctx in
          let inner_ctx = Context.bind_tyvars (List.map Quantifier.to_var binding) ctx in
          StringBuffer.write buf "forall ";
          Printer.concat_items ~sep:"," quantifier binding binder_ctx buf;
          StringBuffer.write buf ".";
          Printer.apply datatype inner_ctx tp buf)

  (* code for printing relational lenses taken verbatim from the original printer *)
  and lens : Lens.Type.t printer
    = let open Printer in
      Printer (fun _ctx _typ buf ->
          let open Lens in
          let sort = Type.sort _typ in
          let cols = Sort.present_colset sort |> Column.Set.elements in
          let fds = Sort.fds sort in
          let predicate =
            Sort.predicate sort
            |> OptionUtils.from_option (Phrase.Constant.bool true) in
          let pp_col f col =
            Format.fprintf f "%s : %a"
              (Lens.Column.alias col)
              Lens.Phrase.Type.pp_pretty (Lens.Column.typ col) in
          let ret =
            if Lens.Type.is_abstract _typ
            then
              if Lens.Type.is_checked _typ
              then
                Format.asprintf "LensChecked((%a), { %a })"
                  (Lens.Utility.Format.pp_comma_list pp_col) cols
                  Lens.Fun_dep.Set.pp_pretty fds
              else
                Format.asprintf "LensUnchecked((%a), { %a })"
                  (Lens.Utility.Format.pp_comma_list pp_col) cols
                  Lens.Fun_dep.Set.pp_pretty fds
            else
              Format.asprintf "Lens((%a), %a, { %a })"
                (Lens.Utility.Format.pp_comma_list pp_col) cols
                Lens.Database.fmt_phrase_dummy predicate
                Lens.Fun_dep.Set.pp_pretty fds
          in
          StringBuffer.write buf ret)

  and table : (typ * typ * typ) printer
    = let open Printer in
      Printer (fun ctx (r, w, n) buf ->
          let ctx = Context.toplevel ctx in
          StringBuffer.write buf "TableHandle(";
          Printer.concat_items ~sep:"," datatype [ r ;  w ;  n ] ctx buf;
          StringBuffer.write buf ")")

  and datatype : datatype printer
    = let open Printer in
      Printer (fun ctx tp buf ->
          let printer =
            match tp with
            (* keeping this Not_typed in case we ever need to print some intermediate steps *)
            | Not_typed          -> constant "Not typed"

            | Var (vid, knd, _)  -> with_value var (vid, knd)
            | Recursive v        -> with_value recursive v
            | Application a      -> with_value application a
            | Alias ((name, _, arg_types, is_dual), _)
              | RecursiveApplication { r_name = name; r_args = arg_types; r_dual = is_dual; _ }
              -> with_value alias_recapp (name, arg_types, is_dual)

            | Meta pt            -> meta pt
            | Present t          -> with_value presence t
            | Primitive t        -> with_value primitive t

            | Function f         -> with_value func f
            | Lolli f            -> with_ambient Context.Linfun func f

            | Table tab          -> with_value table tab
            | Lens tp            -> with_value lens tp
            | ForAll fa          -> with_value forall fa

            | Input _ | Output _ -> with_value session_io tp
            | Dual tp            -> with_value session_dual tp
            | End                -> constant "End"

            | Record _ | Variant _ | Effect _ | Row _ | Select _ | Choice _
              -> with_value row tp

            | _ -> raise (internal_error ("Printer for this type not implemented:\n" ^ show_datatype @@ DecycleTypes.datatype tp))
          in
          Printer.apply printer ctx () buf)

  let string_of_datatype : Policy.t -> names -> datatype -> string
    = fun policy' names ty ->
    let ctxt = Context.(with_policy policy' (with_tyvar_names names (empty ()))) in
    Printer.generate_string datatype ctxt ty

  let string_of_row_var : Policy.t -> names -> row_var -> string
    = fun policy' names rvar ->
    let ctxt = Context.(with_policy policy' (with_tyvar_names names (empty ()))) in
    match meta rvar with
    | Printer.Empty -> ""
    | (Printer.Printer _) as pr -> Printer.generate_string pr ctxt ()

  let string_of_type_arg : Policy.t -> names -> type_arg -> string
    = fun policy' names tyarg ->
    let ctxt = Context.(with_policy policy' (with_tyvar_names names (empty ()))) in
    Printer.generate_string type_arg ctxt tyarg

  let string_of_quantifier : Policy.t -> names -> Quantifier.t -> string
    = fun policy' names q ->
    let ctxt = Context.(with_policy policy' (with_tyvar_names names (empty ()))) in
    Printer.generate_string quantifier ctxt q

  let tycon_spec : tycon_spec printer
    = let open Printer in
      Printer (fun ctx v buf ->
          match v with
          | `Alias (tyvars, body) ->
             let ctx = Context.bind_tyvars (List.map Quantifier.to_var tyvars) ctx in
             begin
               match tyvars with
               | [] -> Printer.apply datatype ctx body buf
               | _ -> Printer.concat_items ~sep:"," quantifier tyvars ctx buf;
                      StringBuffer.write buf ".";
                      Printer.apply datatype ctx body buf
             end
          | `Mutual _ -> StringBuffer.write buf "mutual"
          | `Abstract _ -> StringBuffer.write buf "abstract")

  let string_of_tycon_spec : Policy.t -> names -> tycon_spec -> string
    = fun policy' names tycon ->
    let ctxt = Context.(with_policy policy' (with_tyvar_names names (empty ()))) in
    Printer.generate_string tycon_spec ctxt tycon

  let string_of_presence : Policy.t -> names -> field_spec -> string
    = fun policy' names pre ->
    let ctxt = Context.(with_policy policy' (with_tyvar_names names (empty ()))) in
    Printer.generate_string presence ctxt pre
end

module DerivedPrinter : PRETTY_PRINTER = struct
  let string_of_datatype : Policy.t -> names -> datatype -> string
    = fun _policy _names ty ->
    show_datatype (DecycleTypes.datatype ty)

  let string_of_row_var : Policy.t -> names -> row_var -> string
    = fun _policy _names rvar ->
    show_row_var (DecycleTypes.row_var rvar)

  let string_of_type_arg : Policy.t -> names -> type_arg -> string
    = fun _policy _names tyarg ->
    show_type_arg (DecycleTypes.type_arg tyarg)

  let string_of_quantifier : Policy.t -> names -> Quantifier.t -> string
    = fun _policy _names q ->
    Quantifier.show (DecycleTypes.quantifier q)

  let string_of_tycon_spec : Policy.t -> names -> tycon_spec -> string
    = fun _policy _names tycon ->
    let decycle_tycon_spec = function
      | `Alias (qlist, ty) -> `Alias (List.map DecycleTypes.quantifier qlist, DecycleTypes.datatype ty)
      | other -> other
    in
    show_tycon_spec (decycle_tycon_spec tycon)

  let string_of_presence : Policy.t -> names -> field_spec -> string
    = fun _policy _names pre ->
    show_field_spec (DecycleTypes.field_spec pre)
end


let free_bound_type_vars            = Vars.free_bound_type_vars TypeVarSet.empty
let free_bound_row_type_vars        = Vars.free_bound_row_type_vars TypeVarSet.empty
let free_bound_field_spec_type_vars = Vars.free_bound_field_spec_type_vars TypeVarSet.empty
let free_bound_type_arg_type_vars   = Vars.free_bound_tyarg_vars TypeVarSet.empty
let free_bound_row_var_vars         = Vars.free_bound_row_var_vars TypeVarSet.empty
let free_bound_quantifier_vars      = Vars.free_bound_quantifier_vars
let free_bound_tycon_type_vars      = Vars.free_bound_tycon_vars TypeVarSet.empty

(** Generates new variable names for things in the list, adding them to already
    existing pool of type variable names.
 *)
let add_tyvar_names (f : 'a -> Vars.vars_list) (tys : 'a list) =
  List.iter (fun t -> let _ = Vars.make_names (f t) in ()) tys

(** Builds a fresh set of type variable names for a given list of things.  This
    function is called:

    * when pretty-printing a type.  It then builds type variable names for a
      single thing that is being printed.

    * when printing error messages.  It then builds a consistent set of variable
      names for several different types appearing in the error message.
 *)
let build_tyvar_names ~refresh_tyvar_names (f : 'a -> Vars.vars_list)
      (tys : 'a list) =
  if refresh_tyvar_names then
    begin Vars.tyvar_name_counter := 0; Hashtbl.reset Vars.tyvar_name_map; end;
  add_tyvar_names f tys

(*

Note [Refreshing type variable names]
=====================================

Optional argument refresh_tyvar_names passed to string_of_* pretty-printing
functions determines whether the set of variable names should be refreshed
(default) or re-used.  The latter is used for printing error messages, where we
want consistent type variable names across several calls to pretty-printing
functions.

See Note [Variable names in error messages].

 *)

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

(* Which printer to use *)
type pretty_printer_engine = Old | Roundtrip | Derived

let string_of_engine = function
  | Derived -> "derived"
  | Old -> "old"
  | Roundtrip -> "roundtrip"

let print_types_pretty_engine
  = let parse_engine v =
      match String.lowercase_ascii v with
      | "derived" -> Derived
      | "old" -> Old
      | "roundtrip" -> Roundtrip
      | _ -> raise (Invalid_argument "accepted values: derived | old | roundtrip")
    in
    let parse_engines v =
      List.map parse_engine (Settings.parse_paths v)
    in
    let string_of_engines es =
      let es' = List.map string_of_engine es in
      Settings.string_of_paths es' (* TODO(dhil): find a new appropriate name for `string_of_paths`. *)
    in
    Settings.(multi_option ~default:[Roundtrip] "types_pretty_printer_engine"
              |> synopsis "Selects the engine(s) used by the pretty printer."
              |> hint "<derived|old|roundtrip>"
              |> to_string string_of_engines
              |> convert parse_engines
              |> sync)

let print_types_pretty
  = Settings.(flag ~default:true "print_types_pretty"
              |> synopsis "Toggles whether to use the pretty printer or derived printer for printing types"
              |> convert parse_bool
              |> sync)

(* TODO (future) most of the functions below can be merged, as they do
   essentially the same thing, and also the functions they use for building
   tyvar names are aliases of the same things (in most cases) *)

(* string conversions *)
let printers : (pretty_printer_engine * (module PRETTY_PRINTER)) list
  = [ (Roundtrip, (module RoundtripPrinter))
    ; (Old      , (module Print))
    ; (Derived  , (module DerivedPrinter)) ]

let generate_string : Policy.t -> names -> ((module PRETTY_PRINTER) -> Policy.t -> names -> 'a -> string) -> 'a -> string
  = fun policy names selector obj ->
  let engines = Settings.get print_types_pretty_engine in
  let rec loop : pretty_printer_engine list -> (pretty_printer_engine * string) list = function
    | [] -> []
    | engine :: engines ->
       let (module Printer : PRETTY_PRINTER) = List.assoc engine printers in
       let str = selector (module Printer) policy names obj in
       (engine, str) :: loop engines
  in
  match loop engines with
  | [] -> ""
  | [(_, str)] -> str
  | results ->
     let results' =
       List.map (fun (engine, str) -> Printf.sprintf "%s: %s" (string_of_engine engine) str) results
     in
     String.concat "\n" ("Pretty printer results:" :: results')

let string_of_datatype : ?policy:(unit -> Policy.t) -> ?refresh_tyvar_names:bool -> datatype -> string
  = fun ?(policy=Policy.default_policy) ?(refresh_tyvar_names=true) ty ->
  let policy = policy () in
  let ty' =
    (* If we want to hide toplevel quantifiers, they get removed
       entirely here. This influences the names of type variables: the
       one seen first (i.e. getting the name "a") may no longer be the
       one that is bound by the quantifier. Tests expect this.  TODO
       (possibly; future) is this the right way to do things? Maybe
       this logic should be decided in the printer? *)
    if Policy.quantifiers policy then ty
    else strip_quantifiers ty
  in
  build_tyvar_names ~refresh_tyvar_names free_bound_type_vars [ty'];
  generate_string policy Vars.tyvar_name_map (fun (module Printer : PRETTY_PRINTER) -> Printer.string_of_datatype) ty'


let string_of_row : ?policy:(unit -> Policy.t) -> ?refresh_tyvar_names:bool -> row -> string
  = string_of_datatype

let string_of_presence : ?policy:(unit -> Policy.t) -> ?refresh_tyvar_names:bool -> field_spec -> string
  = fun ?(policy=Policy.default_policy) ?(refresh_tyvar_names=true) pre ->
  let policy = policy () in
  build_tyvar_names ~refresh_tyvar_names free_bound_field_spec_type_vars [pre];
  generate_string policy Vars.tyvar_name_map (fun (module Printer : PRETTY_PRINTER) -> Printer.string_of_presence) pre

let string_of_type_arg : ?policy:(unit -> Policy.t) -> ?refresh_tyvar_names:bool -> type_arg -> string
  = fun ?(policy=Policy.default_policy) ?(refresh_tyvar_names=true) tyarg ->
  let policy = policy () in
  build_tyvar_names ~refresh_tyvar_names free_bound_type_arg_type_vars [tyarg];
  generate_string policy Vars.tyvar_name_map (fun (module Printer : PRETTY_PRINTER) -> Printer.string_of_type_arg) tyarg

let string_of_row_var : ?policy:(unit -> Policy.t) -> ?refresh_tyvar_names:bool -> row_var -> string
  = fun ?(policy=Policy.default_policy) ?(refresh_tyvar_names=true) rvar ->
  let policy = policy () in
  build_tyvar_names ~refresh_tyvar_names free_bound_row_var_vars [rvar];
  generate_string policy Vars.tyvar_name_map (fun (module Printer : PRETTY_PRINTER) -> Printer.string_of_row_var) rvar

let string_of_tycon_spec : ?policy:(unit -> Policy.t) -> ?refresh_tyvar_names:bool -> tycon_spec -> string
  = fun ?(policy=Policy.default_policy) ?(refresh_tyvar_names=true) tycon ->
  let policy = policy () in
  build_tyvar_names ~refresh_tyvar_names free_bound_tycon_type_vars [tycon];
  generate_string policy Vars.tyvar_name_map (fun (module Printer : PRETTY_PRINTER) -> Printer.string_of_tycon_spec) tycon

let string_of_quantifier : ?policy:(unit -> Policy.t) -> ?refresh_tyvar_names:bool -> Quantifier.t -> string
  = fun ?(policy=Policy.default_policy) ?(refresh_tyvar_names=true) q ->
  let policy = policy () in
  build_tyvar_names ~refresh_tyvar_names free_bound_quantifier_vars [q];
  generate_string policy Vars.tyvar_name_map (fun (module Printer : PRETTY_PRINTER) -> Printer.string_of_quantifier) q

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

let string_of_environment env = show_environment (Env.map DecycleTypes.datatype env)

let string_of_typing_environment { var_env = env; _ }
  = string_of_environment env

let make_fresh_envs : datatype -> datatype IntMap.t * row IntMap.t * field_spec IntMap.t =
  let module S = IntSet in
  let module M = IntMap in
  let empties = M.empty, M.empty, M.empty in
  let union =
    let union_one a b = M.fold M.add a b in
    let union_three (x, y, z) (x', y', z') = (union_one x x', union_one y y', union_one z z') in
    List.fold_left union_three empties in
  let rec make_env boundvars =
    function
    | Not_typed -> empties
    | Var _ | Recursive _ | Closed ->
       failwith ("[12] freestanding Var / Recursive not implemented yet (must be inside Meta)")
    | Primitive _             -> empties
    | Function (f, m, t)      -> union [make_env boundvars f; make_env boundvars m; make_env boundvars t]
    | Lolli (f, m, t)         -> union [make_env boundvars f; make_env boundvars m; make_env boundvars t]
    | Effect row | Record row | Variant row -> make_env boundvars row
    | Table (r, w, n)         -> union [make_env boundvars r; make_env boundvars w; make_env boundvars n]
    | Lens _                  -> empties
    | Alias ((_, _, ts, _), d) -> union (List.map (make_env_ta boundvars) ts @ [make_env boundvars d])
    | Application (_, ds)     -> union (List.map (make_env_ta boundvars) ds)
    | RecursiveApplication { r_args ; _ } -> union (List.map (make_env_ta boundvars) r_args)
    | ForAll (qs, t)          ->
       make_env (TypeVarSet.add_quantifiers qs boundvars) t
    | Meta point ->
       begin
         match Unionfind.find point with
         | Var (var, kind, `Flexible) ->
            let tenv, renv, penv = empties in
            (M.add var (fresh_type_variable (Kind.subkind kind)) tenv, renv, penv)
         | Var (var, kind, `Rigid) ->
            let tenv, renv, penv = empties in
            (M.add var (fresh_rigid_type_variable (Kind.subkind kind)) tenv, renv, penv)
         | Recursive (l, _, _) when S.mem l boundvars -> empties
         | Recursive (l, _, b) -> make_env (S.add l boundvars) b
         | t -> make_env boundvars t
       end
    | Row (field_env, row_var, _dual) ->
       let field_vars =
         FieldEnv.fold
           (fun _ f envs ->
             union [make_env boundvars f; envs])
           field_env empties
       in
       let row_vars =
         match Unionfind.find row_var with
         | Closed -> empties
         | Var (var, kind, `Flexible) ->
            let tenv, renv, penv = empties in
            (tenv, M.add var (Row (StringMap.empty, fresh_row_variable (Kind.subkind kind), false)) renv, penv)
         | Var (var, kind, `Rigid) ->
            let tenv, renv, penv = empties in
            (tenv, M.add var (Row (StringMap.empty, fresh_rigid_row_variable (Kind.subkind kind), false)) renv, penv)
         | Recursive (l, _, _) when S.mem l boundvars -> empties
         | Recursive (l, _, row) -> make_env (S.add l boundvars) row
         | row -> make_env boundvars row
       in
       union [field_vars; row_vars]
    | Present t -> make_env boundvars t
    | Absent -> empties
    | Input (t, s) | Output (t, s) -> union [make_env boundvars t; make_env boundvars s]
    | Select row | Choice row    -> make_env boundvars row
    | Dual s        -> make_env boundvars s
    | End           -> empties
  and make_env_r boundvars t = make_env boundvars t
  and make_env_f boundvars t = make_env boundvars t
  and make_env_ta boundvars (pk, t) =
    let open PrimaryKind in
    match pk with
    | Type     -> make_env boundvars t
    | Row      -> make_env_r boundvars t
    | Presence -> make_env_f boundvars t in
  make_env S.empty

let make_rigid_envs datatype : datatype IntMap.t * row IntMap.t * field_spec Utility.IntMap.t =
  let tenv, renv, penv = make_fresh_envs datatype in
    (IntMap.map (fun _ -> fresh_rigid_type_variable (lin_any, res_any)) tenv,
     IntMap.map (fun _ -> Row (StringMap.empty, fresh_rigid_row_variable (lin_any, res_any), false)) renv,
     IntMap.map (fun _ -> fresh_rigid_presence_variable (lin_any, res_any)) penv)

let make_wobbly_envs datatype : datatype IntMap.t * row IntMap.t * field_spec Utility.IntMap.t =
  let tenv, renv, penv = make_fresh_envs datatype in
    (IntMap.map (fun _ -> fresh_type_variable (lin_any, res_any)) tenv,
     IntMap.map (fun _ -> Row (StringMap.empty, fresh_row_variable (lin_any, res_any), false)) renv,
     IntMap.map (fun _ -> fresh_presence_variable (lin_any, res_any)) penv)

let combine_per_kind_envs : datatype IntMap.t * row IntMap.t * field_spec IntMap.t -> type_arg IntMap.t =
  fun (tenv, renv, penv) ->
  let envs = [ IntMap.map (fun t -> PrimaryKind.Type, t) tenv
             ; IntMap.map (fun r -> PrimaryKind.Row, r) renv
             ; IntMap.map (fun p -> PrimaryKind.Presence, p) penv] in
  let union = List.fold_left (IntMap.fold IntMap.add) IntMap.empty in
  union envs


(* subtyping *)
let is_sub_type, is_sub_row =
  let module S = TypeVarSet in
  let rec is_sub_type = fun rec_vars (t, t') ->
    match t, t' with
      | Not_typed, Not_typed -> true
      | (Var _ | Recursive _ | Closed), _
      | _, (Var _ | Recursive _ | Closed) ->
         failwith ("[13] freestanding Var / Recursive / Closed not implemented yet (must be inside Meta)")
      | Primitive p, Primitive q -> p=q
      | Function (f, eff, t), Function (f', eff', t') ->
          is_sub_type rec_vars (f', f)
          && is_sub_eff rec_vars (eff, eff')
          && is_sub_type rec_vars (t, t')
      | Effect row', Effect row
      | Record row', Record row
      | Variant row, Variant row' ->
          let lrow, _ = unwrap_row row
          and rrow, _ = unwrap_row row' in
            is_sub_row rec_vars (lrow, rrow)
      | Table _, Table _ -> raise (internal_error "not implemented subtyping on tables yet")
      | Application (labs, _), Application (rabs, _) ->
          (* WARNING:

             This assumes that abstract type parameters are all covariant -
             which happens to be true for all the built-in abstract types we
             currently support.
          *)
          (* TODO: implement variance annotations *)
          labs = rabs && assert false (* TODO: is_sub_type_tyarg *)
      | Meta point, Meta point' ->
          begin
            match Unionfind.find point, Unionfind.find point' with
            | Closed, _ | _, Closed -> assert false
            | Var (var, _, _), Var (var', _, _) -> var=var'
            | t, _  when is_type_body t  -> is_sub_type rec_vars (t, t')
            | _, t' when is_type_body t' -> is_sub_type rec_vars (t, t')
            | Recursive _, Recursive _ ->
               raise (internal_error "not implemented subtyping on recursive types yet")
            | _, _ -> false
          end
      | Meta point, _ ->
          begin
            match Unionfind.find point with
              | Var _
              | Recursive _ -> false
              | t -> is_sub_type rec_vars (t, t')
          end
      | _, Meta point ->
          begin
            match Unionfind.find point with
              | Var _
              | Recursive _ -> false
              | t' -> is_sub_type rec_vars (t, t')
          end
      | Alias ((name, [], [], is_dual), _), Alias ((name', [], [], is_dual'), _)
        when name=name' && is_dual=is_dual' -> true
      | (Alias (_, t)), t'
      | t, (Alias (_, t')) -> is_sub_type rec_vars (t, t')
      | ForAll _, ForAll _ ->
          raise (internal_error "not implemented subtyping on forall types yet")
      | _, _ -> false
  (* This is like standard row sub-typing, but the field types must be invariant.
     Ultimately we might want more flexibility. For instance, we might expect
     contravariance in the type of heard messages (the 'hear' effect is only
     associated with input).
  *)
  and is_sub_eff =
    fun rec_vars (lrow, rrow) ->
    match lrow, rrow with
    | Row (lfield_env, lrow_var, ldual), Row (rfield_env, rrow_var, rdual) ->
       assert (not ldual);
       assert (not rdual);
       let sub_fields =
         FieldEnv.fold (fun name f _ ->
             match f with
             | Present t ->
                if FieldEnv.mem name rfield_env then
                  match FieldEnv.find name rfield_env with
                  | Present t' ->
                     (is_sub_type rec_vars (t, t') &&
                        is_sub_type rec_vars (t', t))
                  | Absent
                  | Meta _ -> false
                  | _ -> raise tag_expectation_mismatch
                else
                  false
             | Absent -> true
             | Meta _ -> assert false (* TODO *)
             | _ -> raise tag_expectation_mismatch
           ) lfield_env true in
       let sub_row_vars =
         match Unionfind.find lrow_var, Unionfind.find rrow_var with
         | Var (var, _, _), Var (var', _, _) -> var = var'
         | Closed, _ -> true
         | lrow, _ when is_row_body lrow -> is_sub_row rec_vars (lrow, rrow)
         | _, rrow when is_row_body rrow -> is_sub_row rec_vars (lrow, rrow)
         | Recursive _, Recursive _ ->
            raise (internal_error "not implemented subtyping on recursive rows yet")
         | _, _ -> false in
       sub_fields && sub_row_vars
    | _ -> raise tag_expectation_mismatch
  and is_sub_row =
    fun rec_vars (lrow, rrow) ->
    match lrow, rrow with
    | Row (lfield_env, lrow_var, ldual), Row (rfield_env, rrow_var, rdual) ->
      let sub_fields =
        FieldEnv.fold (fun name f _ ->
                         match f with
                           | Present t ->
                               if FieldEnv.mem name rfield_env then
                                 match FieldEnv.find name rfield_env with
                                   | Present t' ->
                                       is_sub_type rec_vars (t, t')
                                   | Absent | Meta _ -> false
                                   | _ -> raise tag_expectation_mismatch
                               else
                                 false
                           | Absent -> true
                           | Meta _ -> assert false (* TODO *)
                           | _ -> raise tag_expectation_mismatch
          ) lfield_env true
      in
      let sub_row_vars =
        let dual_if b r = if b then dual_row r else r in
        match Unionfind.find lrow_var, Unionfind.find rrow_var with
          | Var (var, _, _), Var (var', _, _) -> ldual=rdual && var=var'
          | Closed, _ -> true
          | lrow, _ when is_row_body lrow -> is_sub_row rec_vars (dual_if ldual lrow, rrow)
          | _, rrow when is_row_body rrow -> is_sub_row rec_vars (lrow, dual_if rdual rrow)
          | Recursive _, Recursive _ ->
             raise (internal_error "not implemented subtyping on recursive rows yet")
          | _, _ -> false
      in
      sub_fields && sub_row_vars
    | _ -> raise tag_expectation_mismatch
  in
  ((fun t -> is_sub_type S.empty t),
   (fun row -> is_sub_row S.empty row))


let make_tuple_type (ts : datatype list) : datatype =
  Record
    (snd
       (List.fold_left
          (fun (n, row) t -> n+1, row_with (string_of_int n, Present t) row)
          (1, make_empty_closed_row ())
          ts))

let make_list_type t = Application (list, [PrimaryKind.Type, t])
let make_process_type r = Application (process, [PrimaryKind.Row, r])

let extend_row_check_duplicates fields row =
  match row with
  | Row (fields', row_var, dual) ->
     let (unified_fields, has_duplicates) =
       FieldEnv.fold
         (fun name t (fields, has_duplicates) ->
           (FieldEnv.add name (Present t) fields), has_duplicates && FieldEnv.mem name fields)
         fields
         (fields', false) in
     Row (unified_fields,row_var, dual), has_duplicates
  | _ -> raise tag_expectation_mismatch

let extend_row_safe fields row =
  match extend_row_check_duplicates fields row with
  | (_, true) -> None
  | (row', false) -> Some row'
let extend_row fields row =
  fst (extend_row_check_duplicates fields row)

let open_row subkind = function
  | Row (fieldenv, rho, dual) when rho = closed_row_var ->
     Row (fieldenv, fresh_row_variable subkind, dual)
  | Row _ -> raise (internal_error "attempt to open an already open row")
  | _ -> raise tag_expectation_mismatch

let close_row = function
  | Row (fieldenv, rho, dual) when rho <> closed_row_var ->
     Row (fieldenv, closed_row_var, dual)
  | Row _ -> raise (internal_error "attempt to close an already closed row")
  | _ -> raise tag_expectation_mismatch

let closed_wild_row = make_singleton_closed_row wild_present

let remove_field : ?idempotent:bool -> Label.t -> row -> row
  = fun ?(idempotent=true) lbl row ->
  match row with
  | Row (fieldenv, var, dual) ->
     if idempotent || StringMap.mem lbl fieldenv
     then Row (StringMap.remove lbl fieldenv, var, dual)
     else raise (internal_error "attempt to remove non-existent field")
  | _ -> raise tag_expectation_mismatch



let make_closed_row : datatype field_env -> row =
  fun fields ->
  Row ((FieldEnv.map (fun t -> Present t) fields), closed_row_var, false)

let make_record_type ts = Record (make_closed_row ts)
let make_variant_type ts = Variant (make_closed_row ts)

let make_table_type (r, w, n) = Table (r, w, n)
let make_endbang_type : datatype = Alias (("EndBang", [], [], false), Output (unit_type, End))

let make_function_type : ?linear:bool -> datatype list -> row -> datatype -> datatype
  = fun ?(linear=false) args effs range ->
  if linear then
    Lolli (make_tuple_type args, effs, range)
  else
    Function (make_tuple_type args, effs, range)

let make_pure_function_type : datatype list -> datatype -> datatype
  = fun domain range -> make_function_type domain (make_empty_closed_row ()) range

let make_thunk_type : row -> datatype -> datatype
  = fun effs rtype ->
  make_function_type [] effs rtype

let recursive_applications t =
  let o = new GetRecursiveApplications.visitor in
  let (o, _) = o#typ t in
  o#get_applications |> StringSet.elements


(* We replace some of the generated printing functions here such that
   they may use our own printing functions instead. If the generated functions are
   to be used, we remove potential cycles arising from recursive types/rows first.
   They are here because they are needed
   by the generated code for printing the IR, do not call them yourself.
   Use string_of_* instead
   Make sure that none of the printing functions generated for types
   possibly containg cycles escape this module without having one
   a version below that removes the cycles first! *)
let pp : Format.formatter -> t -> unit = fun fmt t ->
  if Settings.get print_types_pretty then
    Format.pp_print_string fmt (string_of_datatype t)
  else
    pp fmt (DecycleTypes.datatype t)

let pp_row = pp
let pp_datatype = pp
let pp_field_spec = pp

let pp_meta_type_var : Format.formatter -> meta_type_var -> unit = fun fmt p ->
  if Settings.get print_types_pretty then
    Format.pp_print_string fmt (string_of_datatype (Meta p))
  else
    pp_typ fmt (DecycleTypes.row (Meta p))

let pp_row' : Format.formatter -> row' -> unit = fun fmt t ->
  if Settings.get print_types_pretty then
    Format.pp_print_string fmt (string_of_row (Row t))
  else
    pp_row fmt (DecycleTypes.row (Row t))

let pp_type_arg : Format.formatter -> type_arg -> unit = fun fmt t ->
  if Settings.get print_types_pretty then
    Format.pp_print_string fmt (string_of_type_arg t)
  else
    pp_type_arg fmt (DecycleTypes.type_arg t)

let pp_tycon_spec : Format.formatter -> tycon_spec -> unit = fun fmt t ->
  let decycle_tycon_spec = function
    | `Alias (qlist, ty) -> `Alias (List.map DecycleTypes.quantifier qlist, DecycleTypes.datatype ty)
    | other -> other in

  if Settings.get print_types_pretty then
    Format.pp_print_string fmt (string_of_tycon_spec t)
  else
    pp_tycon_spec fmt (decycle_tycon_spec t)


let unwrap_list_type = function
  | Application ({Abstype.id = "List"; _}, [PrimaryKind.Type, t]) -> t
  | _ -> assert false
