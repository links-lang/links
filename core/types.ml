let dpr' : string -> unit  =
  fun _ ->
  begin
    (* disabling debug printing for tests; to enable, uncomment the print *)
    (* Printf.printf "%s\n" x;
     * flush_all () *)

    (* it does flushing because at one point I thought my prints weren't actually showing up;
     * that issue is now resolved but I kept it just in case *)
  end

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
     (* TODO I think this was a bug: (dis)allowing one-tuples is
        handled below, but here we need to make sure that the object
        is a tuple at all; if there was only one field, it wasn't
        checked for int *)
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

(* Some helper functions from typeUtils.ml, to prevent cyclical dependencies *)
exception TypeDestructionError of string

let extract_row t = match concrete_type t with
  | Effect row
  | Record row -> row
  | Variant row -> row
  | t ->
      raise @@ TypeDestructionError
                 ("Internal error: attempt to extract a row from a datatype that is not a record or a variant: "
                  ^ show_datatype @@ DecycleTypes.datatype t)

let extract_row_parts : t -> row' = function
    | Row parts -> parts
    | t -> raise @@ TypeDestructionError
                      ("Internal error: attempt to extract row parts from a datatype that is not a row "
                       ^ show_datatype @@ DecycleTypes.datatype t)

(* END of the typeUtils.ml stuff *)

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
  (* TODO check this - mem leak? *)
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

let effect_sugar
  = Settings.(flag "effect_sugar"
              |> convert parse_bool
              |> sync)

(** Type printers *)

type pp_policy = {quantifiers:bool; flavours:bool; hide_fresh:bool; kinds:string; effect_sugar:bool}

(* TODO: check over the pretty-printer in light of the types refactoring *)
module Print =
struct
  module BS = Basicsettings
  let show_quantifiers
    = Settings.(flag "show_quantifiers"
                |> convert parse_bool
                |> sync)

  let show_flavours
    = Settings.(flag "show_flavours"
                |> convert parse_bool
                |> sync)

  let show_kinds
    = Settings.(option ~default:(Some "default") "show_kinds"
                |> to_string from_string_option
                |> convert Utility.some
                |> sync)

  let hide_fresh_type_vars
    = Settings.(flag ~default:true "hide_fresh_type_vars"
                |> convert parse_bool
                |> sync)

  (* Set the quantifiers to be true to display any outer quantifiers.
     Set flavours to be true to distinguish flexible type variables
     from rigid type variables. *)
  type policy = pp_policy
  (* {quantifiers:bool; flavours:bool; hide_fresh:bool; kinds:string; effect_sugar:bool} *)
  type names  = (int, string * Vars.spec) Hashtbl.t
  type context = { bound_vars: TypeVarSet.t; shared_effect: int option }

  let default_policy : unit -> policy = fun () ->
    {quantifiers=Settings.get show_quantifiers;
     flavours=Settings.get show_flavours;
     hide_fresh=Settings.get hide_fresh_type_vars;
     kinds=val_of (Settings.get show_kinds);
     effect_sugar=Settings.get effect_sugar}

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
    if policy.effect_sugar then
      let (obj, _) = visit obj in
      { empty_context with shared_effect = obj#var }
    else
      empty_context

  let subkind : (policy * names) -> Subkind.t -> string =
    let full (l, r) = "(" ^ Linearity.to_string l ^ "," ^
                        Restriction.to_string r ^ ")"
    in
    fun (policy, _vars) ->
    if policy.kinds = "full"
    then full
    else if policy.kinds = "hide"
    then function (_, _) -> ""
    else function
      | (Linearity.Unl, Restriction.Any)     -> ""
      | (Linearity.Any, Restriction.Any)     -> "Any"
      | (Linearity.Unl, Restriction.Base)    -> Restriction.to_string res_base
      | (Linearity.Any, Restriction.Session) -> Restriction.to_string res_session
      | (Linearity.Unl, Restriction.Effect)  -> Restriction.to_string res_effect
      | (l, r) -> full (l, r)

  let kind : (policy * names) -> Kind.t -> string =
    let full (policy, _vars) (k, sk) =
      PrimaryKind.to_string k ^ subkind (policy, _vars) sk in
    fun (policy, _vars) (k, sk) ->
    if policy.kinds = "full" then
      full (policy, _vars) (k, sk)
    else if policy.kinds = "hide" then
      PrimaryKind.to_string k
    else
      match (k, sk) with
      | PrimaryKind.Type, (Linearity.Unl, Restriction.Any) -> ""
      | PrimaryKind.Type, (Linearity.Unl, Restriction.Base) ->
         Restriction.to_string res_base
      | PrimaryKind.Type, (Linearity.Any, Restriction.Session) ->
         Restriction.to_string res_session
      | PrimaryKind.Type, sk ->
         subkind ({policy with kinds="full"}, _vars) sk
      | PrimaryKind.Row, (Linearity.Unl, Restriction.Any) ->
         PrimaryKind.to_string pk_row
      | PrimaryKind.Row, (Linearity.Unl, Restriction.Effect) ->
         PrimaryKind.to_string pk_row
      | PrimaryKind.Presence, (Linearity.Unl, Restriction.Any) ->
         PrimaryKind.to_string pk_presence
      | PrimaryKind.Row, _ | PrimaryKind.Presence, _ ->
         full ({policy with kinds="full"}, _vars) (k, sk)

  let quantifier : (policy * names) -> Quantifier.t -> string =
    fun (policy, vars) q ->
      let k = Quantifier.to_kind q in
      Vars.find (Quantifier.to_var q) vars ^ has_kind (kind (policy, vars) k)

  (** If type variable names are hidden return a generic name n1. Otherwise
     pass name of type variable to n2 so that it can construct a name. *)
  let name_of_type_plain { bound_vars; _ } (policy, vars : policy * names) var n1 n2 =
    let name, (flavour, _, count) = Vars.find_spec var vars in
    if policy.hide_fresh && count = 1
       && ((flavour = `Flexible && not (policy.flavours)) || not (IntSet.mem var bound_vars))
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
        | Var (var, k, `Flexible) when policy.flavours ->
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
         else if fields_present ["wild"]
         then
           ppr_eff_var ~args ~allows_shared row_var ("{}~" ^ ah)
               ("~%~" ^ ah, fun name -> "~%" ^ name ^ "~" ^ ah)
               ("~" ^ ah,   fun name -> "~"  ^ name ^ "~" ^ ah)
         else if fields_present ["hear"; "wild"]
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
           if FieldEnv.mem "wild" fields &&
             is_present (FieldEnv.find "wild" fields) then
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
              | Var (var, k, `Flexible) when policy.flavours ->
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
              match FieldEnv.find "hear" fields with
              | Present t -> sd t
              | _          -> assert false in
            ppr_function_type args effects t ">" ht
         | Lolli    (args, effects, t) ->
            let ht fields =
              sd (match FieldEnv.find "hear" fields with
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
            (* TODO *)
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
            if not (policy.flavours) then
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
              (* decycling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~vvvvvvvvvvvvvvvvvvvvvvvvv *)
              if policy.hide_fresh && count = 1 && not (IntSet.mem var bound_vars) then n1
              else (n2 name) in
            match Unionfind.find point with
              | Var (var, _, `Flexible) when policy.flavours ->
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
             if strip_wild && label = "wild" then
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
      | Var (var, k, `Flexible) when policy.flavours ->
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

  let strip_quantifiers = function
    | ForAll (_, t) | t -> t
end

type buffer_container = { buffer: Buffer.t;
                          write: (string -> unit);
                          concat: (?sep:string -> string list -> unit);
                          add_buffer: (Buffer.t -> unit);
                          concat_buffers: (?sep:string -> Buffer.t list -> unit);
                          read: (unit -> string) }
let create_buffer : ?init_size:int -> unit -> buffer_container =
  fun ?(init_size=16) () ->
  let buf = Buffer.create init_size in
  let write' s = Buffer.add_string buf s in
  let add_buffer' b = Buffer.add_buffer buf b in
  let rec concat' ?(sep="") =
    function
    | [] -> ()
    | [last] -> write' last
    | not_last :: ((_ :: _) as rest) -> begin write' not_last;
                                              write' sep;
                                              concat' ~sep:sep rest
                                        end
  in
  let rec concat_buffers' ?(sep="") =
    function
    | [] -> ()
    | [last] -> add_buffer' last
    | not_last :: ((_ :: _) as rest) -> begin add_buffer' not_last;
                                              write' sep;
                                              concat_buffers' ~sep:sep rest
                                        end
  in
  let read' () = Buffer.contents buf in
  { buffer = buf; write = write'; concat = concat'; add_buffer = add_buffer'; concat_buffers=concat_buffers'; read = read' }

let wrap_buffer : string -> Buffer.t =
  fun s ->
  let len = String.length s in
  let { buffer; write; _ } = create_buffer ~init_size:len () in
  write s;
  buffer

let concat : ?sep:string -> string list -> string =
  fun ?(sep="") lst ->
  let { concat; read; _ } = create_buffer () in
  concat ~sep:sep lst;
  read ()

(* New type pretty printer (Samo) *)
module NewPrint = struct
  (* TODO maybe it can all be done by having "inner" functions that just pass a Buffer around,
     and wrapping those in "outer" functions that return just a string to match the interface signature? *)

  (* settings (tied to original Print for the time being) *)
  let show_quantifiers = Print.show_quantifiers
    (* = Settings.(flag "show_quantifiers"
     *             |> convert parse_bool
     *             |> sync) *)

  let show_flavours = Print.show_flavours
    (* = Settings.(flag "show_flavours"
     *             |> convert parse_bool
     *             |> sync) *)

  let show_kinds = Print.show_kinds
    (* = Settings.(option ~default:(Some "default") "show_kinds"
     *             |> to_string from_string_option
     *             |> convert Utility.some
     *             |> sync) *)

  let hide_fresh_type_vars = Print.hide_fresh_type_vars
    (* = Settings.(flag ~default:true "hide_fresh_type_vars"
     *             |> convert parse_bool
     *             |> sync) *)


  module Context = struct
    type policy = pp_policy

    type names  = (int, string * Vars.spec) Hashtbl.t

    type ambient = Toplevel
                 | Function
                 | Linfun
                 | Presence
                 | Tuple
                 | Variant
                 | Effect
                 | Row

    type t = { policy: policy
             ; bound_vars: TypeVarSet.t
             ; tyvar_names: names
             ; ambient: ambient
             ; shared_effect: int option }

    let default_policy : unit -> policy
      = fun () ->
      { quantifiers = Settings.get show_quantifiers
      ; flavours    = Settings.get show_flavours
      ; hide_fresh  = Settings.get hide_fresh_type_vars
      ; kinds       = val_of (Settings.get show_kinds)
      ; effect_sugar= Settings.get effect_sugar }

    let empty () = { policy        = default_policy ()
                   ; bound_vars    = TypeVarSet.empty
                   ; tyvar_names   = Hashtbl.create 0
                   ; ambient       = Toplevel
                   ; shared_effect = None }

    (* let context = NewPrint.Context.setup policy Vars.tyvar_name_map (fun o -> o#typ t) in *)
    let setup : policy -> names (* -> <?> visitor needed for shared effects TODO *) -> t
      = fun policy tyvar_names -> { policy
                                  ; bound_vars    = TypeVarSet.empty
                                  ; tyvar_names
                                  ; ambient       = Toplevel
                                  ; shared_effect = None
                                  }

    let bound_vars : t -> TypeVarSet.t
      = fun { bound_vars; _ } -> bound_vars

    let bind_tyvar : tid -> t -> t
      = fun ident ({ bound_vars; _ } as ctxt) ->
      { ctxt with bound_vars = TypeVarSet.add ident bound_vars }

    let bind_tyvars : tid list -> t -> t
      = fun lst ctx ->
      List.fold_left (fun c' v' -> bind_tyvar v' c') ctx lst

    let tyvar_names : t -> names
      = fun { tyvar_names; _ } -> tyvar_names

    let policy : t -> policy
      = fun { policy; _ } -> policy

    let set_policy : policy -> t -> t
      = fun policy ctxt -> { ctxt with policy }

    let ambient : t -> ambient
      = fun { ambient; _ } -> ambient

    let set_ambient : ambient -> t -> t
      = fun ambient ctxt -> { ctxt with ambient }

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

    (*
       (defun insert-ambient-helpers ()
         (interactive)
         (let ((original-point (point)))
           (search-backward "type ambient =")
           (mark-sexp)
           (let* ((amb-def
                   (buffer-substring-no-properties (region-beginning) (region-end)))
                  (amb-cases
                   (mapcar #'string-trim (split-string (cadr (split-string amb-def "=")) "|"))))
             (goto-char original-point)
             (dolist (amb-name amb-cases)
               (insert (concat "let is_ambient_" (downcase amb-name) " : t -> bool\n"
                               "= fun { ambient ; _ } -> ambient = " amb-name "\n")))
             (set-mark (point))
             (goto-char original-point)
             (indent-region (region-beginning) (region-end)))))

       ;; eval the sexp above and use command insert-ambient-helpers above this definition
       ;; TODO we can throw this away, but it's useful while changing the ambients
     *)
  end

  (* TODO(dhil): Encapsulate inside a StringBuffer structure. *)
  module StringBuffer = struct
    type t = Buffer.t
    type 'a printer = Printer of (Context.t -> 'a -> t -> unit)
                    | Empty

    let write : t -> string -> unit
      = fun buf s -> Buffer.add_string buf s

    let concat : sep:string -> 'a printer -> 'a list -> Context.t -> t -> unit
      = fun ~sep pr items ctx buf ->
      match pr with
      | Empty -> ()
      | Printer pr ->
         let rec loop
           = fun sep pr items ctx buf ->
           begin
             match items with
             | [] -> ()
             | [last] -> pr ctx last buf
             | not_last :: rest ->
                pr ctx not_last buf;
                write buf sep;
                loop sep pr rest ctx buf
           end
         in
         loop sep pr items ctx buf

    let apply : 'a printer -> Context.t -> 'a -> t -> unit
      = fun pr ctx thing buf ->
      match pr with
      | Empty -> ()
      | Printer pr -> pr ctx thing buf

    let rec concat' : sep:string -> unit printer list -> Context.t -> t -> unit
      = fun ~sep prs ctx buf ->
      match prs with
      | [] -> ()
      | [last] -> apply last ctx () buf
      | x :: (y :: rest) ->
         begin (* TODO maybe with a filter *)
           match x, y with
           | Empty, Empty -> concat' ~sep rest ctx buf
           | Empty, _ -> concat' ~sep (y :: rest) ctx buf
           | Printer _, Empty -> concat' ~sep (x :: rest) ctx buf
           | (Printer _) as pr, _ -> apply pr ctx () buf;
                                     write buf sep;
                                     concat' ~sep (y :: rest) ctx buf
         end

    let seq : sep:string -> ('a printer * 'b printer) -> ('a * 'b) -> Context.t -> t -> unit
      = fun ~sep (lp, rp) (x, y) ctx buf ->
      match lp, rp with
      | Printer l, Printer r ->
         l ctx x buf;
         write buf sep;
         r ctx y buf
      | Printer p, Empty ->
         p ctx x buf
      | Empty, Printer p ->
         p ctx y buf
      | Empty, Empty -> ()

    let constant : string -> unit printer
      = fun s ->
      Printer (fun _ctx () buf -> write buf s)

    let wrap : 'a printer -> 'a -> unit printer
      = fun pr v ->
      Printer (fun ctx () buf -> apply pr ctx v buf)

    let wrap_ambient : Context.ambient -> 'a printer -> 'a -> unit printer
      = fun amb pr v ->
      Printer (fun ctx () buf ->
          let inner_ctx = Context.set_ambient amb ctx in
          apply pr inner_ctx v buf)

    let eval : 'a printer -> Context.t -> 'a -> string
      = fun pr ctx v ->
      let buf = Buffer.create 10 in (* initial size is an arbitrary value here (TODO) *)
      apply pr ctx v buf;
      Buffer.contents buf

    let concat_strs : sep:string -> string list -> string
      = let ctx_dont_care = Context.empty() in (* won't care about the context, just here to reuse the infrastructrue *)
        fun ~sep lst ->
        let buf = Buffer.create 10 in
        let str_pr : string printer = Printer (fun _ s buf -> write buf s) in
        concat ~sep str_pr lst ctx_dont_care buf;
        Buffer.contents buf
  end

  type 'a printer = 'a StringBuffer.printer

  module BS = Basicsettings (* TODO is this ever used? *)

  (* Set the quantifiers to be true to display any outer quantifiers.
     Set flavours to be true to distinguish flexible type variables
     from rigid type variables. *)


  (* For correct printing of subkinds, need to know the subkind in advance:
   * see line (1): that has to be Empty so that the :: is not printed *)
  let subkind_name : Context.policy -> Subkind.t -> unit printer
    = fun pol (lin, res) ->
    let open StringBuffer in
    let full_name : unit printer
      = Printer (
            fun _ctx () buf ->
            write buf "(";
            write buf (Linearity.to_string lin);
            write buf ",";
            write buf (Restriction.to_string res);
            write buf ")"
          )
    in
    let constant : string -> unit printer
      = fun c -> Printer (fun _ _ buf ->  write buf c)
    in
    match pol.kinds with
    | "full" -> full_name
    | "hide" -> Empty
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


  let kind_name : Kind.t printer
    = StringBuffer.Printer (
          fun ctxt ((primary, subknd) as knd) buf ->
          let open StringBuffer in
          let full_name : Context.t -> Kind.t -> StringBuffer.t -> unit
            = fun ctxt (k, sk) buf ->
            write buf (PrimaryKind.to_string k);
            StringBuffer.apply (subkind_name (Context.policy ctxt) sk) ctxt () buf
          in
          let policy = Context.policy ctxt in
          match policy.kinds with
          | "full" -> full_name ctxt knd buf
          | "hide" -> write buf (PrimaryKind.to_string primary)
          | _ ->
             let module P = PrimaryKind in
             let module L = Linearity in
             let module R = Restriction in
             (* do simple cases first, match the other stuff later *)
             match primary with
             | P.Type -> begin
                 match subknd with
                 | L.Unl, R.Any -> ()
                 | L.Unl, R.Base -> write buf (R.to_string res_base)
                 | L.Any, R.Session -> write buf (R.to_string res_session)
                 | subknd ->
                    let pol = { (Context.policy ctxt) with kinds = "full" } in
                    StringBuffer.apply (subkind_name pol subknd) ctxt () buf
               end
             | PrimaryKind.Row -> begin
                 match subknd with
                 | L.Unl, R.Any | L.Unl, R.Effect ->
                    write buf (P.to_string pk_row)
                 | _ ->
                    let ctxt' = Context.set_policy { policy with kinds = "full" } ctxt in
                    full_name ctxt' knd buf
               end
             | PrimaryKind.Presence -> begin
                 match subknd with
                 | L.Unl, R.Any -> write buf (P.to_string pk_presence)
                 | _ ->
                    let ctxt' = Context.set_policy { policy with kinds = "full" } ctxt in
                    full_name ctxt' knd buf
               end)

  let strip_quantifiers : typ -> typ =
    function
    | ForAll (_, t) | t -> t

  let primitive : Primitive.t printer
    = let open StringBuffer in
      Printer (fun _ctxt prim buf -> write buf (Primitive.to_string prim))

  let is_var_anonymous : Context.t -> tid -> bool =
    fun ctxt vid ->
    let _, (_, _, count) = Vars.find_spec vid (Context.tyvar_names ctxt) in
    count = 1 && (Context.policy ctxt).hide_fresh && not (IntSet.mem vid (Context.bound_vars ctxt))

  (* string_of_var : context -> policy * names -> VAR -> string *)
  let rec var : (tid * Kind.t) printer
    = let open StringBuffer in
      Printer (fun ctx (vid, knd) buf ->
          let subknd = Kind.subkind knd in
          let var_name, (flavour, _ (* kind already known *), _) = Vars.find_spec vid (Context.tyvar_names ctx) in
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
                let is_anonymous = is_var_anonymous ctx vid in
                let show_flexible = (Context.policy ctx).flavours && flavour = `Flexible in
                let is_presence = (PrimaryKind.Presence = Kind.primary_kind knd) in

                (if is_presence then write buf "{");
                (if show_flexible then write buf "%");
                (match is_anonymous, show_flexible with
                 | true, true  -> ()
                 | true, false -> write buf "_"
                 | _, _        -> write buf var_name);
                (if is_presence then write buf "}"))
          in
          seq ~sep:"::" (print_var, subkind_name (Context.policy ctx) subknd) (var_name, ()) ctx buf)

  and recursive : (tid * Kind.t * typ) printer
    = let open StringBuffer in
      Printer (fun ctx (binder, knd, tp) buf ->
          dpr' "recursive";
          (* assumes that the recursive variable itself shouldn't display kind information,
           * because the definition of the type itself is right there *)
          let rec_var_p = { (Context.policy ctx) with kinds = "hide" } in
          let ctx' = Context.set_policy rec_var_p ctx in
          if IntSet.mem binder (Context.bound_vars ctx)
          then (* this recursive was already seen -> just need the variable name *)
            begin
              dpr' "Already seen mu";
              apply var ctx' (binder, knd) buf
            end
          else (* this the first occurence of this mu -> print the whole type *)
            begin
              dpr' "New mu";
              let inner_context = Context.bind_tyvar binder ctx in
              let inner_context' = Context.bind_tyvar binder ctx' in
              let want_parens =
                match Context.ambient ctx with
                | Context.Toplevel -> false
                | _ -> true
              in
              (if want_parens then write buf "(");
              write buf "mu ";
              apply var inner_context' (binder, knd) buf;
              write buf " . ";
              apply datatype inner_context tp buf; (* TODO maybe change ambient? *)
              (if want_parens then write buf ")");
            end)

  and alias_recapp : (string * type_arg list * bool) printer
    = let open StringBuffer in
      Printer (fun ctx (name, arg_types, is_dual) buf ->
          (if is_dual then write buf "~");
          write buf (Module_hacks.Name.prettify name);
          (match arg_types with
           | [] -> ()
           | _ -> write buf " (";
                  concat ~sep:"," type_arg arg_types ctx buf;
                  write buf ")"))

  (* TODO Ignoring shared effects for now *)

  (* and row_fields : context -> policy * names -> strip_wild:bool -> concise_hear:bool -> ?hide_units:bool -> ?sep:string->
   *                  ?is_tuple:bool -> field_spec_map -> (Buffer.t * int) = (\* TODO count displayed fields for sugaring purposes *\)
   *   fun ctx p ~strip_wild ~concise_hear ?(hide_units=false) ->
   *   fun ?(sep=",") ?(is_tuple=false) fs_map ->
   *   (\* dpr' "row_fields"; *\)
   *   let { buffer=buf; concat_buffers; _ } = create_buffer () in
   *   let fold : string -> typ -> (Buffer.t list * int) -> (Buffer.t list * int) =
   *     fun label fld (accumulator, counter) ->
   *     match label with
   *     | "wild" when strip_wild -> accumulator, counter
   *     | "hear" when concise_hear ->
   *        let { buffer=buf'; write=write'; _ } = create_buffer () in
   *        dpr' "concise_hear --> presence_type with colon";
   *        write' (presence_type ~want_colon:true ctx p fld);
   *        accumulator @ [buf'], counter + 1
   *     | _ ->
   *        let { buffer=buf'; write=write'; _ } = create_buffer () in
   *        (if is_tuple then () else write' label);
   *        write' (presence_type ~want_colon:(not is_tuple) ~hide_units ctx p fld);
   *        accumulator @ [buf'], counter + 1
   *   in
   *   let field_buf_list, count = FieldEnv.fold fold fs_map ([], 0) in
   *   (\* List.iter (fun x -> dpr' (Buffer.contents x)) field_buf_list; *\)
   *   concat_buffers ~sep:sep field_buf_list;
   *   buf, count
   *
   * and row_var : context -> policy * names -> row_var -> istring option =
   *   fun ctx p rv ->
   *   (\* dpr' "row_var"; *\)
   *   let v = Unionfind.find rv in
   *   match v with
   *   | Closed -> None
   *   | Var (id, knd, _) -> Some (var ctx p id knd)
   *   | Recursive v -> Some (recursive ctx p v ~want_parens:true)
   *   | _ -> Some (datatype ctx p v)
   *
   * and row :
   *       (\* ?name not used, it's just to keep the signature compatible for now *\)
   *       ?maybe_tuple:bool option -> (\* ?maybe_tuple is ternary: None => maybe, Some true => yes, Some false => no *\)
   *       ?hide_units:bool -> (\* for variants *\)
   *       ?strip_wild:bool -> ?concise_hear:bool -> ?space_row_var:bool ->
   *       string -> context -> policy * names -> row -> string =
   *
   *   fun ?(maybe_tuple=(Some false)) ?(hide_units=false) ?(strip_wild=false)
   *       ?(concise_hear=false) ?(space_row_var=false) sep ctx p r ->
   *   dpr' "row";
   *   let r, is_tuple =
   *     match maybe_tuple with
   *     | None -> (\* need to decide if it's a tuple or not - it could be *\)
   *        let r, _ = unwrap_row r in
   *        (r, is_tuple ~allow_onetuples:false r) (\* onetuple still has to print as (1=...) to round trip *\)
   *     | Some x -> r, x (\* definitely it is (not) a tuple *\)
   *   in
   *   let fields = row_fields ctx p ~strip_wild ~concise_hear ~is_tuple ~hide_units
   *                  ~sep:(if is_tuple then concat [sep; " "] else sep) (\* sugar: tuples have spacing in fields *\)
   *   in
   *   let { write; add_buffer; read; _ } = create_buffer () in
   *   match r with
   *   | Row (r_fields, r_var, is_dual) ->
   *      let fields_string, fields_shown = fields r_fields in
   *      add_buffer fields_string;
   *      (\* dpr' @@ concat ["Fields (if not tuple):" ; Buffer.contents @@ row_fields ctx p strip_wild ~is_tuple:false ~sep:sep r_fields]; *\)
   *      let var = row_var ctx p r_var in
   *      begin
   *        match var with
   *        | Some s -> (if space_row_var && fields_shown = 0
   *                     then write " |"
   *                     else write "|");
   *                    dpr' "Has row variable";
   *                    (if is_dual then write "~" else ());
   *                    write s
   *        | None -> ()
   *      end;
   *      read ()
   *   | Meta pt -> dpr' "Meta in row!!";
   *                meta ctx p pt (\* TODO This happens in tests/typed_ir.tests/Generalisation obeys value restriction (#871)
   *                               * not entirely sure where this Meta is coming through, I though it could be via type_arg *\)
   *   | _ -> failwith ("Invalid row: " ^ show_datatype @@ DecycleTypes.datatype r) *)

  and row_parts : row' printer
    = let open StringBuffer in
      Printer (
          fun ctx (rfields, rvar, rdual) buf ->
          let hide_primitive_labels = Context.is_ambient_effect ctx in
          let fold = fun label fld printers ->
            match label with
            | "wild" when hide_primitive_labels -> printers (* skip wild *) (* TODO make sure it's present *)
            | "hear" when hide_primitive_labels ->
               (Printer (fun ctx () buf -> apply presence ctx fld buf)) :: printers
            | _ ->
               if Context.is_ambient_tuple ctx
               then (Printer (fun ctx () buf -> apply presence ctx fld buf)) :: printers
               else Printer (
                        fun ctx () buf ->
                        write buf label;
                        match concrete_type fld with
                        | Var (v,knd,_) when Kind.primary_kind knd = PrimaryKind.Presence ->
                           apply var ctx (v, knd) buf
                        | _ ->
                           apply presence ctx fld buf (* TODO label into presence *)
                      ) :: printers
          in
          let printers = List.rev (FieldEnv.fold fold rfields []) in
          let sep =
            let open Context in
            match ambient ctx with
            | Variant -> "|"
            | Tuple -> ", "
            | _ -> ","
          in
          concat' ~sep printers ctx buf;
          match meta rvar with
          | Empty -> ()
          | (Printer _) as pr ->
             begin
               (if List.length printers = 0 && (Context.is_ambient_effect ctx || Context.is_ambient_row ctx)
                                                 (* starts with {, want to avoid \{\| *)
                then write buf " |"
                else write buf "|");
               (if rdual then write buf "~");
               apply pr ctx () buf
             end)

  and row' : typ printer
    = let open StringBuffer in
      Printer (
          fun ctx r buf ->
          let unrolled =
            (match r with
             | Row _ -> r
             | _ -> extract_row r)
            |> unwrap_row |> fst in
          let r', before, after, new_ctx =
            match r with
            | Record r ->
               let is_tuple = (Context.is_ambient_tuple ctx) || (is_tuple unrolled) in (* not allowing onetuples by default *)
               r, "(", ")", (if is_tuple then (Context.set_ambient Context.Tuple ctx) else ctx)
            | Variant r -> r, "[|", "|]", (Context.set_ambient Context.Variant ctx)
            | Effect r -> r, "{", "}", (Context.set_ambient Context.Effect ctx)
            | Select r -> r, "[+|", "|+]", ctx (* TODO *)
            | Choice r -> r, "[&|", "|&]", ctx (* TODO *)
            | _ -> failwith ("[*R] Invalid row:\n" ^ show_datatype @@ DecycleTypes.datatype r)
          in
          write buf before;
          apply row_parts new_ctx (extract_row_parts r') buf;
          write buf after
        )

  (* returns the presence, but possibly without the colon *)
  (* TODO maybe have this return a Buffer, if that would be more efficient *)
  and presence : typ printer
    = let open StringBuffer in
      Printer (
          fun ctx tp buf ->
          dpr' "presence_type";
          (match concrete_type tp with
           | Absent -> write buf "-"
           | Present tp ->
              if not (concrete_type tp = unit_type && (Context.is_ambient_variant ctx))
              then begin
                  (if not (Context.is_ambient_tuple ctx) then write buf ":");
                  apply datatype ctx tp buf
                end
           | Meta pt -> apply (meta pt) (Context.set_ambient Context.Presence ctx) () buf
           | _ -> failwith "[*p] Type not implemented"))

  and meta : typ point -> unit printer
    = let open StringBuffer in
      fun pt ->
      match Unionfind.find pt with
      | Closed -> (* nothing happens; TODO but maybe something should *sometimes* happen *)
         dpr' "Closed Meta";
         Empty
      | Var (id, knd, _) -> Printer (fun ctx () buf -> apply var ctx (id, knd) buf)
      | Recursive r -> Printer (fun ctx () buf -> apply recursive ctx r buf)
      | t -> dpr' ("Meta - other type:\n" ^ show_datatype @@ DecycleTypes.datatype t);
             Printer (fun ctx () buf -> apply datatype ctx t buf)

  and type_arg : type_arg printer
    = let open StringBuffer in
      Printer (
          fun ctx (pknd, r) buf ->
          let module P = PrimaryKind in
          match pknd with
          | P.Type -> apply datatype ctx r buf
          | P.Row -> begin
              (* TODO maybe call concrete_type here *)
              write buf "{";
              let ctx = Context.set_ambient Context.Row ctx in
              (match r with
               | Row rp -> apply row_parts ctx rp buf
               | Meta pt -> apply (meta pt) ctx () buf
               (* TODO can a Var ever be here directly? (without Meta) *)
               | _ -> failwith ("Non-(row|meta) in type_arg:\n" ^ show_datatype @@ DecycleTypes.datatype r));
              write buf "}";
            end
          | P.Presence -> failwith "No way to round-trip type variable::Presence")

  and application : (Abstype.t * type_arg list) printer
    = let open StringBuffer in
      Printer (fun ctx p buf ->
          match p with
          | (abstp, [el]) when Abstype.equal abstp list ->
             write buf "[";
             apply type_arg ctx el buf;
             write buf "]"
          | (abstp, []) ->
             write buf (Abstype.name abstp)
          | (abstp, args) ->
             write buf (Abstype.name abstp);
             write buf " (";
             concat ~sep:"," type_arg args ctx buf;
             write buf ")"
        )

  and func : (typ * row * typ) printer
    (* and func : ?is_lolli:bool -> context -> policy * names -> typ -> row -> typ -> Buffer.t = *)
    = let open StringBuffer in

      let func_arrow : row printer
        (* let func_arrow : ?is_lolli:bool -> context -> policy * names -> row -> Buffer.t = *)
        = let is_field_present fields fld =
            match FieldEnv.lookup fld fields with
            | None -> false
            | Some (Present _) -> true
            | Some Absent | Some (Meta _) -> false
            | _ -> failwith "Unexpected field presence value."
          in
          Printer (
              fun ctx r buf ->
              let is_lolli = Context.is_ambient_linfun ctx in
              match r with
              | Row (fields, rvar, _) as r' ->
                 let is_wild = is_field_present fields "wild" in
                 let number_of_visible_fields = (FieldEnv.size fields) - (if is_wild then 1 else 0) in
                 let row_var_exists =
                   match meta rvar with
                   | Empty -> false
                   | _ -> true
                 in
                 dpr' @@ "Visible row fields: " ^ string_of_int number_of_visible_fields;
                 dpr' @@ "And a row var exists: " ^ string_of_bool row_var_exists;
                 begin
                   match number_of_visible_fields, row_var_exists with
                   | 0, false -> write buf "{}" (* no fields but closed row var *)
                   | 0, true -> (* there is a row variable, but no fields
                                 * => use the abbreviated notation -a- or ~a~
                                 * BUT if it's anonymous => skip entirely *)
                      begin
                        match Unionfind.find (extract_row_parts r |> snd3) with (* TODO temporary solution *)
                        | Var (vid, knd, _) ->
                           if is_var_anonymous ctx vid
                           then (dpr' "SKIP";
                                 ()) (* skip printing it entirely *)
                           else begin
                               dpr' "NO SKIP";
                               (if is_wild
                                then write buf "~"
                                else write buf "-");
                               apply var ctx (vid, knd) buf
                             end
                        | _ -> failwith "[*1] Shoudln't happen?" (* TODO *)
                      (* let varname = datatype ctx p v in
                       * dpr' "HERE";
                       * dpr' varname;
                       * (if is_wild
                       *  then write "~"
                       *  else write "-");
                       * write varname *)
                      end
                   | _ -> (* need the full effect row *)
                      apply row' ctx (Effect r') buf
                 end;

                 (if is_wild then write buf "~" else write buf "-");
                 (* add the arrowhead/lollipop *)
                 (if is_lolli then write buf "@" else write buf ">");
              | _ -> failwith ("Illformed effect:\n" ^ string_of_datatype ctx r)
            )
      in

      (* func starts here *)
      Printer (
          fun ctx (domain, effects, range) buf ->
          let effects, _ = unwrap_row effects in
          (* build up the function type string: domain, arrow with effects, range *)
          apply row' (Context.set_ambient Context.Tuple ctx) domain buf;
          write buf " ";
          apply func_arrow ctx effects buf;
          write buf " ";
          apply datatype ctx range buf;
        )

  and session_io : typ printer
    = let open StringBuffer in
      Printer (
          fun ctx tp buf ->
          let t_char = match tp with
            | Input _ -> "?"
            | Output _ -> "!"
            | _ -> failwith "Invalid session I/O type" (* this will never happen, because the function session_io
                                                        *  will only ever be called for Input | Output *)
          in
          match tp with
          | Input (tp, session_tp) | Output (tp, session_tp) ->
             write buf t_char;
             write buf "(";
             apply datatype ctx tp buf;
             write buf ").";
             apply datatype ctx session_tp buf
          | _ -> () (* same as above, no error here because it would have already failed before *)
        )

  and session_dual : typ printer
    = let open StringBuffer in
      Printer (
          fun ctx tp buf ->
          let dtype = wrap datatype tp in
          write buf "~";
          match tp with
          | Input _ | Output _ | Select _ | Choice _ ->
             write buf "(";
             apply dtype ctx () buf;
             write buf ")"
          | _ -> apply dtype ctx () buf
        )

  and quantifier : Quantifier.t printer
    = let open StringBuffer in
      Printer (
          fun ctx qr buf ->
          (* dpr' "quantifier"; *)
          let var = Quantifier.to_var qr in
          let var_name = Vars.find var (Context.tyvar_names ctx) in
          let knd = Quantifier.to_kind qr in
          write buf var_name;
          apply kind_name ctx knd buf)

  and forall : (Quantifier.t list * typ) printer
    = let open StringBuffer in
      Printer (
          fun ctx (binding, tp) buf ->
          let inner_ctx = Context.bind_tyvars (List.map Quantifier.to_var binding) ctx in
          write buf "forall ";
          concat ~sep:"," quantifier binding ctx buf;
          apply datatype inner_ctx tp buf
        )

  (* code for printing relational lenses taken verbatim from the original printer *)
  and lens : Lens.Type.t printer
    = let open StringBuffer in
      Printer (
          fun _ctx _typ buf ->
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
          write buf ret)

  and table : (typ * typ * typ) printer
    = let open StringBuffer in
      Printer (fun ctx (r, w, n) buf ->
          write buf "TableHandle(";
          concat ~sep:"," datatype [ r ;  w ;  n ] ctx buf;
          write buf ")")

  and datatype : datatype printer
    (* and datatype : context -> policy * names -> datatype -> string = *)
    = let open StringBuffer in
      Printer (
          fun ctx tp buf ->
          let printer =
            match tp with
            (* keeping this Not_typed in case we ever need to print some intermediate steps *)
            | Not_typed          -> constant "Not typed"

            | Var (vid, knd, _)  -> wrap var (vid, knd)
            | Recursive v        -> wrap recursive v
            | Application a      -> wrap application a
            | Alias ((name, _, arg_types, is_dual), _)
              | RecursiveApplication { r_name = name; r_args = arg_types; r_dual = is_dual; _ }
              -> wrap alias_recapp (name, arg_types, is_dual)

            | Meta pt            -> meta pt
            | Present t          -> wrap presence t
            | Primitive t        -> wrap primitive t

            | Function f         -> wrap func f
            | Lolli f            -> wrap_ambient Context.Linfun func f

            | Table tab          -> wrap table tab
            | Lens tp            -> wrap lens tp
            | ForAll fa          -> wrap forall fa

            | Input _ | Output _ -> wrap session_io tp
            | Dual tp            -> wrap session_dual tp
            | End                -> constant "End"

            | Record _ | Variant _ | Effect _ | Row _ | Select _ | Choice _
              -> wrap row' tp

            | _ -> failwith ("Printer for this type not implemented:\n" ^ show_datatype @@ DecycleTypes.datatype tp)
          in
          apply printer ctx () buf)

  (* outside interface functions *)
  and string_of_datatype : Context.t -> datatype -> string
    = fun ctx v -> StringBuffer.eval datatype ctx v

  let string_of_row : Context.t -> row -> string
    = fun ctx v -> StringBuffer.eval row' ctx v

  let string_of_presence : Context.t -> typ -> string
    = fun ctx v -> StringBuffer.eval presence ctx v

  let string_of_row_var : Context.t -> row_var -> string option
    = fun ctx v ->
    let open StringBuffer in
    let m' = meta v in
    match m' with
    | Empty -> None
    | Printer _ -> Some (StringBuffer.eval m' ctx ())

  let string_of_type_arg : Context.t -> type_arg -> string
    = fun ctx v -> StringBuffer.eval type_arg ctx v

  let string_of_quantifier : Context.t -> Quantifier.t -> string
    = fun ctx v -> StringBuffer.eval quantifier ctx v

  (* let context_with_shared_effect :
   *   policy ->
   *   (( (\* TODO *\)
   *     < field_spec : row -> 'c * row;
   *     field_spec_map : field_spec_map -> 'c * field_spec_map;
   *     list : 'a 'b. ('c -> 'a -> 'c * 'b) -> 'a list -> 'c * 'b list;
   *     meta_presence_var : meta_presence_var -> 'c * meta_presence_var;
   *     meta_row_var : row_var -> 'c * row_var;
   *     meta_type_var : meta_type_var -> 'c * meta_type_var;
   *     primitive : Primitive.t -> 'c * Primitive.t;
   *     quantifier : Quantifier.t -> 'c * Quantifier.t;
   *     row : row -> 'c * row; row_var : row_var -> 'c * row_var;
   *     set_rec_vars : meta_type_var intmap -> 'c; typ : row -> 'c * row;
   *     type_arg : type_arg -> 'c * type_arg;
   *     type_args : type_arg list -> 'c * type_arg list; var : tid option >
   *                                                            as 'c) ->
   *    < var : tid option; .. > * 'd) ->
   *   context = *)

  (* TODO copied from original to satisfy interface *)
  (* let maybe_shared_effect = function
   *   | Function _ | Lolli _ -> true
   *   | Alias ((_, qs, _, _), _) | RecursiveApplication { r_quantifiers = qs; _ } ->
   *      begin match ListUtils.last_opt qs with
   *      | Some (PrimaryKind.Row, (_, Restriction.Effect)) -> true
   *      | _ -> false
   *      end
   *   | _ -> false *)

  (* TODO see above *)
  (* let context_with_shared_effect policy visit =
   *   let find_row_var r =
   *     let r =
   *       match fst (unwrap_row r) with
   *       | Row (_, r, _) -> r
   *       | _ -> raise tag_expectation_mismatch
   *     in
   *     begin match Unionfind.find r with
   *     | Var (var, _, _) -> Some var
   *     | _ -> None
   *     end
   *   in
   *   (\* Find a shared effect variable from the right most arrow or type alias. *\)
   *   let rec find_shared_var t =
   *     match t with
   *     | Function (_, _, r) | Lolli (_, _, r) when maybe_shared_effect r -> find_shared_var r
   *     | Function (_, e, _) | Lolli (_, e, _) -> find_row_var e
   *     | Alias ((_, _, ts, _), _) | RecursiveApplication { r_args = ts; _ } when maybe_shared_effect t ->
   *        begin match ListUtils.last ts with
   *        | (PrimaryKind.Row, (Row _ as r)) -> find_row_var r
   *        | _ -> None
   *        end
   *     | _ -> None
   *   in
   *   let obj =
   *     object (self)
   *       inherit Transform.visitor as super
   *
   *       val var = None
   *       method var = var
   *
   *       method! typ typ =
   *         match self#var with
   *         | None ->
   *            begin match find_shared_var typ with
   *            | Some v -> {<var = Some v>}, typ
   *            | None -> super#typ typ
   *            end
   *         | Some _ -> self, typ
   *     end
   *   in
   *   if policy.effect_sugar then
   *     let (obj, _) = visit obj in
   *     { empty_context with shared_effect = obj#var }
   *   else
   *     empty_context *)

  (* TODO for now just copied from original to make it work *)
  let tycon_spec : [< `Abstract of 'a | `Alias of 'b list * 'c | `Mutual of 'd] printer
    = let open StringBuffer in
      Printer (
          fun ctx v buf ->
          match v with
          | `Alias (tyvars, body) ->
             (* let ctx = { context with bound_vars = bound_vars tyvars } in *)
             let ctx = Context.bind_tyvars (List.map Quantifier.to_var tyvars) ctx in
             begin
               match tyvars with
               | [] -> apply datatype ctx body buf
               | _ -> concat ~sep:"," quantifier tyvars ctx buf;
                      write buf ".";
                      apply datatype ctx body buf
             end
          | `Mutual _ -> write buf "mutual"
          | `Abstract _ -> write buf "abstract")

  let string_of_tycon_spec : Context.t -> 'a -> string
    = fun ctx t -> StringBuffer.eval tycon_spec ctx t

end

let default_pp_policy : unit -> pp_policy =
  fun () ->
  {quantifiers=Settings.get Print.show_quantifiers;
   flavours=Settings.get Print.show_flavours;
   hide_fresh=Settings.get Print.hide_fresh_type_vars;
   kinds=val_of (Settings.get Print.show_kinds);
   effect_sugar=Settings.get effect_sugar}



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

(* Pretty print types or use generated printer? *)
let print_types_pretty
  = Settings.(flag ~default:true "print_types_pretty"
              |> synopsis "Toggles whether to use the pretty printer or derived printer for printing types"
              |> convert parse_bool
              |> sync)

(* Use new pretty printer for types (Samo) *)
let use_new_type_pp
  = Settings.(flag ~default:false "use_new_type_pretty_printer"
              |> synopsis ("Toggles whether to use the new pretty printer for types"
                           ^ "(this is a temporary dev option).")
              |> convert parse_bool
              |> CLI.(add (short 'n' <&> long "use_new_type_pretty_printer"))
              |> sync)

let print_old_new
  = Settings.(flag ~default:false "pp_old_new"
              |> synopsis "(When using the new pretty printer) Print the output of the old pretty printer too"
              |> convert parse_bool
              |> sync)

let pp_test_roundtrip
  = Settings.(flag ~default:false "type_pp_roundtrip"
              |> synopsis "Test whether pretty-printed types are round-trippable."
              |> convert parse_bool
              |> CLI.(add (short 't' <&> long "pretty-types-roundtrip"))
              |> sync)

let test_type_roundtrip : datatype (* -> tycon_environment *) -> string -> bool =
  fun _ (* aliases *) _ ->
  (* let open Parse in
   * let dt =
   *   (\* try Some (DesugarDatatypes.read ~aliases str) *\)
   *   (\* TODO do the full read (including aliases), so that types can be compared *\)
   *   try Some (parse_string ~in_context:(LinksLexer.fresh_context ())
   *               datatype str)
   *   with Errors.RichSyntaxError _ -> None
   * in
   * match dt with
   * | None -> false
   * | Some _ ->
   *    (\* TODO compare if the parsed type is isomorphic to t *\)
   *    true *)
  (* TODO dependency cycle; wanna use DesugarDatatypes.read or parts of it *)
  true

(* string conversions *)
let rec string_of_datatype ?(policy=default_pp_policy) ?(refresh_tyvar_names=true)
          (t : datatype) =
  if Settings.get print_types_pretty then
    begin
      if Settings.get use_new_type_pp then
        (* preserve the original arguments so old printer can be invoked as well *)
        let policy' = policy in
        let t' = t in

        let policy = policy () in
        let t = if policy.quantifiers then t
                else NewPrint.strip_quantifiers t in
        build_tyvar_names ~refresh_tyvar_names free_bound_type_vars [t];
        (* let context = NewPrint.context_with_shared_effect policy (fun o -> o#typ t) in
         * let new_type = (NewPrint.string_of_datatype context (policy, Vars.tyvar_name_map) t) in *)
        let context = NewPrint.Context.setup policy Vars.tyvar_name_map (* (fun o -> o#typ t) TODO for shared effects *) in
        let new_type = NewPrint.string_of_datatype context t in
        let old_type = begin
            if Settings.get print_old_new then
              begin
                Settings.set use_new_type_pp false;
                let s = string_of_datatype ~policy:policy' ~refresh_tyvar_names t' in
                Settings.set use_new_type_pp true;
                ["The old printer would say:" ; s ;
                 "New and old pretty types agree:" ;
                 string_of_bool (new_type = s) ]
              end
            else
              []
          end in
        let rt = if Settings.get pp_test_roundtrip then
                   [ "Type roundtrips:" ;
                     string_of_bool (test_type_roundtrip t new_type) ]
                 else []
        in
        let module SB = NewPrint.StringBuffer in
        SB.concat_strs ~sep:"\n" (new_type :: old_type @ rt)

      else
        begin
          let policy = policy () in
          let t = if policy.quantifiers then t
                  else Print.strip_quantifiers t in
          build_tyvar_names ~refresh_tyvar_names free_bound_type_vars [t];
          let context = Print.context_with_shared_effect policy (fun o -> o#typ t) in
          let dt = Print.datatype context (policy, Vars.tyvar_name_map) t in
          (if Settings.get pp_test_roundtrip then
             Printf.printf "Type roundtrips: %s\n" (string_of_bool @@ test_type_roundtrip t dt)
           else ());
          dt
        end
    end
  else
    show_datatype (DecycleTypes.datatype t)

let string_of_row ?(policy=default_pp_policy) ?(refresh_tyvar_names=true) row =
  if Settings.get print_types_pretty then
    let policy = policy () in
    build_tyvar_names ~refresh_tyvar_names free_bound_row_type_vars [row];
    begin
      if Settings.get use_new_type_pp then
        (* let context = NewPrint.context_with_shared_effect policy (fun o -> o#row row) in
         * NewPrint.row' context (policy, Vars.tyvar_name_map) row |> fst3 *)
        let context = NewPrint.Context.setup policy Vars.tyvar_name_map (* (fun o -> o#typ t) TODO for shared effects *) in
        NewPrint.string_of_row context row
      else
        let context = Print.context_with_shared_effect policy (fun o -> o#row row) in
        Print.row "," context (policy, Vars.tyvar_name_map) row
    end
  else
    show_row (DecycleTypes.row row)

let string_of_presence ?(policy=default_pp_policy) ?(refresh_tyvar_names=true)
                       (f : field_spec) =
  build_tyvar_names ~refresh_tyvar_names free_bound_field_spec_type_vars [f];
  if Settings.get use_new_type_pp then
    (* NewPrint.presence NewPrint.empty_context (policy (), Vars.tyvar_name_map) f *)
    let context = NewPrint.Context.setup (policy ()) Vars.tyvar_name_map (* (fun o -> o#typ t) TODO for shared effects *) in
    NewPrint.string_of_presence context f
  else
    Print.presence Print.empty_context (policy (), Vars.tyvar_name_map) f

let string_of_type_arg ?(policy=default_pp_policy) ?(refresh_tyvar_names=true)
                       (arg : type_arg) =
  let policy = policy () in
  build_tyvar_names ~refresh_tyvar_names free_bound_type_arg_type_vars [arg];
  if Settings.get use_new_type_pp then
    (* let context = NewPrint.context_with_shared_effect policy (fun o -> o#type_arg arg) in
     * NewPrint.type_arg context (policy, Vars.tyvar_name_map) arg *)
    let context = NewPrint.Context.setup policy Vars.tyvar_name_map (* (fun o -> o#typ t) TODO for shared effects *) in
    NewPrint.string_of_type_arg context arg
  else
    let context = Print.context_with_shared_effect policy (fun o -> o#type_arg arg) in
    Print.type_arg context (policy, Vars.tyvar_name_map) arg

let string_of_row_var ?(policy=default_pp_policy) ?(refresh_tyvar_names=true) row_var =
  build_tyvar_names ~refresh_tyvar_names free_bound_row_var_vars [row_var];
  match
    begin
      if Settings.get use_new_type_pp then
        (* NewPrint.row_var NewPrint.empty_context (policy (), Vars.tyvar_name_map) row_var *)
        let module C = NewPrint.Context in
        let context = C.setup (policy ()) Vars.tyvar_name_map (* (fun o -> o#typ t) TODO for shared effects *) in
        NewPrint.string_of_row_var (C.set_ambient C.Row context) row_var
      else
        Print.row_var Print.name_of_type "," Print.empty_context (policy (), Vars.tyvar_name_map) row_var
    end
  with | None -> ""
       | Some s -> s

let string_of_tycon_spec ?(policy=default_pp_policy) ?(refresh_tyvar_names=true) (tycon : tycon_spec) =
  build_tyvar_names ~refresh_tyvar_names free_bound_tycon_type_vars [tycon];
  if Settings.get use_new_type_pp then
    let context = NewPrint.Context.setup (policy ()) Vars.tyvar_name_map (* (fun o -> o#typ t) TODO for shared effects *) in
    NewPrint.string_of_tycon_spec context tycon
  else
    Print.tycon_spec Print.empty_context (policy (), Vars.tyvar_name_map) tycon

let string_of_quantifier ?(policy=default_pp_policy) ?(refresh_tyvar_names=true) (quant : Quantifier.t) =
  build_tyvar_names ~refresh_tyvar_names free_bound_quantifier_vars [quant];
  if Settings.get use_new_type_pp then
    (* NewPrint.quantifier (policy (), Vars.tyvar_name_map) quant *)
    let context = NewPrint.Context.setup (policy ()) Vars.tyvar_name_map (* (fun o -> o#typ t) TODO for shared effects *) in
    NewPrint.string_of_quantifier context quant
  else
    Print.quantifier (policy (), Vars.tyvar_name_map) quant


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
