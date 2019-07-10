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
  arity      = [pk_row, (lin_any, res_any)] ;
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
  let compare = Pervasives.compare
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
    | `Alias of ((string * type_arg list) * typ)
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
     | `Alias ((name, args), t) ->
        let (args', o) = List.fold_right (fun arg (acc_args, o) ->
            let (arg', o) = o#type_arg arg in
            (arg' :: acc_args, o)
          ) args ([],o) in
        let (t',o) = o#typ t in
          (`Alias ((name, args'), t'), o)
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

module GetRecursiveApplications =
struct
  class visitor =
    object(o)
      inherit Transform.visitor as super
      val rec_appls = StringSet.empty

      method get_applications = rec_appls

      method! typ = function
        | `Alias _ as a ->
            (* Don't expand aliases -- RecursiveApplications to previous type
             * groups are not of interest in this pass *)
            (a, o)
        | `RecursiveApplication { r_name; r_args; _ } as ra ->
            let apps =
              List.fold_left (fun acc x ->
                let (_, o) = o#type_arg x in
                let apps = o#get_applications in
                StringSet.union acc apps) StringSet.empty r_args in
            let apps = StringSet.(union apps (singleton r_name)) in
            (ra, {< rec_appls = apps >})
        | x -> super#typ x
    end
end


module DecycleTypes  =
struct
  let elim_recursive_type_cycles_visitor = new ElimRecursiveTypeCyclesTransform.visitor

  let datatype t = fst (elim_recursive_type_cycles_visitor#typ t)
  let row r = fst (elim_recursive_type_cycles_visitor#row r)
  let field_spec p = fst (elim_recursive_type_cycles_visitor#field_spec p)
  let type_arg ta = fst (elim_recursive_type_cycles_visitor#type_arg ta)
  let row_var rv = fst (elim_recursive_type_cycles_visitor#row_var rv)
  let quantifier q = fst (elim_recursive_type_cycles_visitor#quantifier q)

end



(* TODO: consider abstracting some of this subkind manipulation code *)

(* base type stuff *)
let rec is_base_type : typ -> bool =
  function
    | `Primitive (Primitive.Bool | Primitive.Int | Primitive.Char |
                  Primitive.Float | Primitive.String) -> true
    | `Alias (_, t) -> is_base_type t
    | `MetaTypeVar point ->
        begin
          match Unionfind.find point with
            | `Var (_, (_, Restriction.Base), _) -> true
            | `Var _ -> false
            | `Body t -> is_base_type t
            | `Recursive _ -> false
        end
    | _ -> false

let rec is_base_row (fields, row_var, _) =
  let base_row_var =
    match Unionfind.find row_var with
      | `Closed
      | `Var (_, (_, Restriction.Base), _) -> true
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
    | `Primitive (Primitive.Bool | Primitive.Int | Primitive.Char |
                  Primitive.Float | Primitive.String) -> true
    | `Alias (_, t) -> is_baseable_type t
    | `MetaTypeVar point ->
        begin
          match Unionfind.find point with
            | `Var (_, (_, Restriction.Base), `Rigid)
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
      | `Var (_, (_, Restriction.Base), `Rigid)
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
    | `Primitive (Primitive.Bool | Primitive.Int | Primitive.Char |
                  Primitive.Float | Primitive.String) -> ()
    | `Alias (_, t) -> basify_type t
    | `MetaTypeVar point ->
        begin
          match Unionfind.find point with
            | `Var (_, (_, Restriction.Base), _) -> ()
            | `Var (_, _, `Rigid) -> assert false
            | `Var (var, (lin, Restriction.Any), `Flexible) -> Unionfind.change point (`Var (var, (lin, res_base), `Flexible))
            | `Var (_, _, `Flexible) -> assert false
            | `Body t -> basify_type t
            | `Recursive _ -> assert false
        end
    | _ -> assert false

let rec basify_row (fields, row_var, _) =
  begin
    match Unionfind.find row_var with
      | `Closed
      | `Var (_, (_, Restriction.Base), _) -> ()
      | `Var (var, (lin, Restriction.Any), `Flexible) -> Unionfind.change row_var (`Var (var, (lin, res_base), `Flexible))
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

(* unl type stuff *)

let is_unl_point =
  fun f (rec_vars, quant_vars) point ->
    begin
      match Unionfind.find point with
      | `Closed -> true
      | `Var (var, (lin, _), _) -> IntSet.mem var quant_vars || Linearity.is_nonlinear lin
      | `Body t -> f (rec_vars, quant_vars) t
      | `Recursive (var, t) ->
        check_rec var rec_vars true (fun rec_vars' -> f (rec_vars', quant_vars) t)
    end

let rec is_unl_type : (var_set * var_set) -> typ -> bool =
  fun (rec_vars, quant_vars) ->
    let iut t = is_unl_type (rec_vars, quant_vars) t in
      function
      | `Not_typed -> assert false
      | `Effect _
      | `Primitive _
      | `Function _ -> true
      | `Lolli _ -> false
      | `Record r
      | `Variant r -> is_unl_row (rec_vars, quant_vars) r
      | `Table _ -> true
      | `Lens _sort -> true
      | `Alias (_, t) -> iut t
      (* We might support linear lists like this...
         but we'd need to replace hd and tl with a split operation. *)
      (* | `Application ({Abstype.id="List"}, [`Type t]) -> is_unl_type (rec_vars, quant_vars) t  *)
      | `Application _ -> true (* TODO: change this if we add linear abstract types *)
      | `RecursiveApplication { r_linear ; _ } ->
          (* An application is linear if the type it refers to is
           * also linear. We calculate this information in two stages.
           * The first pass (if r_linear () returns None) calculates linearity
           * *up to recursive applications*, under the assumption that every type in the
           * block is unrestricted. With this in hand, we can calculate
           * linearity information, meaning that (r_linear ()) will return (Some lin). *)
          OptionUtils.opt_app (not) true (r_linear ())
      | `MetaTypeVar point -> is_unl_point is_unl_type (rec_vars, quant_vars) point
      | `ForAll (qs, t) -> is_unl_type (rec_vars, add_quantified_vars qs quant_vars) t
      | `Dual s -> is_unl_type (rec_vars, quant_vars) s
      | `End -> false
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
        | `Var (v, (lin, _), `Rigid) -> IntSet.mem v quant_vars || Linearity.is_nonlinear lin
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
    | `Effect _
    | `Primitive _
    | `Function _ -> true
    | `Lolli _ -> false
    | `Record r
    | `Variant r -> row_can_be_unl vars r
    | `Table _ -> true
    | `Lens _ -> true
    | `Alias (_, t) -> tcu t
    (* We might support linear lists like this...
         but we'd need to replace hd and tl with a split operation. *)
    (* | `Application ({Abstype.id="List"}, [`Type t]) -> tcu t *)
    | `Application _ -> true (* TODO: change this if we add linear abstract types *)
    | `RecursiveApplication { r_linear; _ } ->
        (* This will have been set during desugaring, far
         * before `type_can_be_unl` is called *)
        not (OptionUtils.val_of (r_linear ()))
    | `MetaTypeVar point -> point_can_be_unl type_can_be_unl vars point
    | `ForAll (qs, t) -> type_can_be_unl (rec_vars, add_quantified_vars qs quant_vars) t
    | `Dual s -> type_can_be_unl vars s
    | `End -> false
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
    | `Var (v, (lin, _), `Rigid)       -> if IntSet.mem v quant_vars || Linearity.is_nonlinear lin then () else assert false
    | `Var (var, (_, rest), `Flexible) -> Unionfind.change point (`Var (var, (lin_unl, rest), `Flexible))
    | `Body t -> f vars t
    | `Recursive (var, t) ->
      check_rec var rec_vars () (fun rec_vars' -> f (rec_vars', quant_vars) t)

let rec make_type_unl : var_set * var_set -> typ -> unit =
  fun ((rec_vars, quant_vars) as vars) ->
    function
    | `Not_typed -> assert false
    | `Primitive _ | `Function _ | `Table _ | `End | `Application _ | `Effect _ | `Lens _ -> ()
    | `RecursiveApplication _ -> ()
    | `Record r | `Variant r -> make_row_unl vars r
    | `Alias (_, t) -> make_type_unl vars t
    | `ForAll (qs, t) -> make_type_unl (rec_vars, add_quantified_vars qs quant_vars) t
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
    | `Var (_, (_, Restriction.Session), _) -> true
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
    | `Var (_, (_, Restriction.Session), _)
    | `Var (_, (_, Restriction.Any),     `Flexible) -> true
    | `Var (_, (_, Restriction.Base),    `Rigid)
    | `Var (_, (_, Restriction.Any),     `Rigid)
    | `Var (_, (_, Restriction.Effect),  `Rigid)
    | `Var (_, (_, Restriction.Effect),  `Flexible)
    | `Var (_, (_, Restriction.Base),    `Flexible) -> false
    | `Body t -> f rec_vars t
    | `Recursive (var, t) ->
      check_rec var rec_vars true (flip f t)

let rec is_sessionable_type : StringSet.t -> var_set -> typ -> bool =
  fun rec_appls rec_vars ->
    function
    | #session_type -> true
    | `Alias (_, t) -> is_sessionable_type rec_appls rec_vars t
    | `RecursiveApplication { r_unique_name; r_args; r_dual; r_unwind; _ } ->
        if StringSet.mem r_unique_name rec_appls  then
          false
        else
          let body = r_unwind r_args r_dual in
          is_sessionable_type (StringSet.add r_unique_name rec_appls) rec_vars body
    | `MetaTypeVar point ->
        is_sessionable_point (is_sessionable_type rec_appls) rec_vars point
    | _ -> false

let rec is_sessionable_field rec_vars =
  function
  | `Absent -> true
  | `Present t -> is_sessionable_type StringSet.empty rec_vars t
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
    | `Var (_,   (_,   Restriction.Session), _)     -> ()
    | `Var (var, (lin, Restriction.Any), `Flexible) ->
       Unionfind.change point (`Var (var, (lin, Restriction.Session), `Flexible))
    | `Var _                             -> assert false
    | `Body t                            -> f rec_vars t
    | `Recursive (var, t)                -> check_rec var rec_vars () (flip f t)

let rec sessionify_type : var_set -> typ -> unit =
  fun rec_vars ->
    function
    | #session_type -> ()
    | `RecursiveApplication _ -> ()
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

let is_sessionable_type = is_sessionable_type StringSet.empty IntSet.empty
let is_sessionable_row = is_sessionable_row IntSet.empty

let sessionify_type = sessionify_type IntSet.empty
let sessionify_row = sessionify_row IntSet.empty

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
      | `Alias ((_, ts), datatype) ->
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
      | `Alias ((name, ts), datatype) ->
          `Alias ((name, ts), nt datatype)
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
           | `ForAll (qs', body) ->
              `ForAll (qs @ qs', body)
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

let quantifiers_of_type_args =
  let open PrimaryKind in
  List.map
    (function
       | `Type (`MetaTypeVar point) ->
           begin
             match Unionfind.find point with
               | `Var (var, subkind, _) -> (var, (Type, subkind))
               | _ -> assert false
           end
       | `Type _ -> assert false
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
       | `Presence _ -> assert false)

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
let xml_type      = `Alias (("Xml", []), `Application (list, [`Type (`Primitive Primitive.XmlItem)]))
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

(* pretty-print type vars as raw numbers rather than letters *)
let show_raw_type_vars = Basicsettings.Types.show_raw_type_vars


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
  let tyvar_name_map = Hashtbl.create 20
  let tyvar_name_counter = ref 0

  let varspec_of_tyvar q =
    var_of_quantifier q, (`Rigid, primary_kind_of_quantifier q, `Bound)

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
                      [var, ((freedom :> flavour), pk_type, `Free)]
                | `Recursive (var, body) ->
                    if TypeVarSet.mem var bound_vars then
                      [var, (`Recursive, pk_type, `Bound)]
                    else
                      (var, (`Recursive, pk_type, `Bound))::(free_bound_type_vars ~include_aliases (TypeVarSet.add var bound_vars) body)
                | `Body t -> fbtv t
            end
        | `Function (f, m, t) ->
            (fbtv f) @ (free_bound_row_type_vars ~include_aliases bound_vars m) @ (fbtv t)
        | `Lolli (f, m, t) ->
            (fbtv f) @ (free_bound_row_type_vars ~include_aliases bound_vars m) @ (fbtv t)
        | `Record row
        | `Variant row -> free_bound_row_type_vars ~include_aliases bound_vars row
        | `Lens _ -> []
        | `Effect row -> free_bound_row_type_vars ~include_aliases bound_vars row
        | `Table (r, w, n) -> (fbtv r) @ (fbtv w) @ (fbtv n)
        | `ForAll (tyvars, body) ->
            let bound_vars, vars =
              List.fold_left
                (fun (bound_vars, vars) tyvar ->
                   let var, spec = varspec_of_tyvar tyvar in
                     TypeVarSet.add var bound_vars, (var, spec)::vars)
                (bound_vars, [])
                tyvars
            in
              (List.rev vars) @ (free_bound_type_vars ~include_aliases bound_vars body)
        | `Alias ((_,ts), d) when include_aliases ->
            List.concat
              (List.map (free_bound_tyarg_vars ~include_aliases bound_vars) ts) @ (fbtv d)
        | `Alias (_, d) -> fbtv d
        | `Application (_, tyargs) ->
            List.concat (List.map (free_bound_tyarg_vars ~include_aliases bound_vars) tyargs)
        | `RecursiveApplication { r_args; _ } ->
            List.concat (List.map (free_bound_tyarg_vars ~include_aliases bound_vars) r_args)
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
                    [var, ((freedom :> flavour), pk_presence, `Free)]
              | `Body f -> free_bound_field_spec_type_vars ~include_aliases bound_vars f
          end
  and free_bound_row_type_vars ~include_aliases bound_vars (field_env, row_var, _) =
    let field_type_vars =
      FieldEnv.fold
        (fun _name f tvs ->
           tvs @ free_bound_field_spec_type_vars ~include_aliases bound_vars f)
        field_env [] in
    let row_var = free_bound_row_var_vars ~include_aliases bound_vars row_var in
      field_type_vars @ row_var
  and free_bound_row_var_vars ~include_aliases bound_vars row_var =
    match Unionfind.find row_var with
      | `Closed -> []
      | `Var (var, _, freedom) ->
            [var, ((freedom :> flavour), pk_row, `Free)]
      | `Recursive (var, row) ->
          if TypeVarSet.mem var bound_vars then
            [var, (`Recursive, pk_row, `Bound)]
          else
            (var, (`Recursive, pk_row, `Bound))::(free_bound_row_type_vars ~include_aliases (TypeVarSet.add var bound_vars) row)
      | `Body row -> free_bound_row_type_vars ~include_aliases bound_vars row
  and free_bound_tyarg_vars ~include_aliases bound_vars =
    function
      | `Type t -> free_bound_type_vars ~include_aliases bound_vars t
      | `Row row -> free_bound_row_type_vars ~include_aliases bound_vars row
      | `Presence f -> free_bound_field_spec_type_vars ~include_aliases bound_vars f

  let free_bound_quantifier_vars quant =
    let var, spec = varspec_of_tyvar quant in
    [(var, spec)]

  let free_bound_tycon_vars ~include_aliases bound_vars tycon_spec =
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
          vars @ (free_bound_type_vars ~include_aliases bound_vars body)
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
    if Settings.get_value show_raw_type_vars then
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

module Print =
struct
  module BS = Basicsettings
  let show_quantifiers     = BS.Types.Print.show_quantifiers
  let show_flavours        = BS.Types.Print.show_flavours
  let show_kinds           = BS.Types.Print.show_kinds
  let hide_fresh_type_vars = BS.Types.Print.hide_fresh_type_vars
  let effect_sugar         = BS.Types.effect_sugar

  (* Set the quantifiers to be true to display any outer quantifiers.
     Set flavours to be true to distinguish flexible type variables
     from rigid type variables. *)
  type policy = {quantifiers:bool; flavours:bool; hide_fresh:bool; kinds:string; effect_sugar:bool}
  type names  = (int, string * Vars.spec) Hashtbl.t
  type shared_effect = Unknown | Shared of int | Distinct
  type context = { bound_vars: TypeVarSet.t; shared_effect: shared_effect }

  let default_policy () =
    {quantifiers=Settings.get_value show_quantifiers;
     flavours=Settings.get_value show_flavours;
     hide_fresh=Settings.get_value hide_fresh_type_vars;
     kinds=Settings.get_value show_kinds;
     effect_sugar=Settings.get_value effect_sugar}

  let empty_context = { bound_vars = TypeVarSet.empty; shared_effect = Unknown }

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

  let find_shared_effect { bound_vars; shared_effect } vars args fields row_var ret =
    let var_eq known resolve=
      match Unionfind.find resolve with
      | `Var (var, _, _) when var = known -> true
      | _ -> false
    in
    match shared_effect with
    | Distinct -> Distinct
    | Shared var ->
       (* If we encounter a row variable, it /must/ be equivalent to our shared one. *)
       assert (var_eq var row_var);
       Shared var
    | Unknown -> (
      match Unionfind.find row_var with
      | `Var (var, _, _) when not (TypeVarSet.mem var bound_vars) ->
          let obj =
            object (self)
              inherit Transform.visitor as super

              val all_same = true
              val count = 1
              method all_same = all_same
              method count = count

              method! typ =
                function
                | typ when not all_same -> (typ, self)
                | (`Function (args, effects, ret) | `Lolli (args, effects, ret)) as typ ->
                    let fields, row_var, _ = unwrap_row effects |> fst in
                    if
                      (fields_present_in fields [] || fields_present_in fields [ "wild" ])
                      && var_eq var row_var
                    then
                      let (_, self) = self#typ args in
                      let (_, self) = self#typ ret in
                      let (_, self) = self#field_spec_map fields in
                      (typ, {<count = count + 1; all_same = self#all_same>})
                    else (
                      (typ, {<all_same = false>})
                    )
                | `Alias ((_, args), _) as ty ->
                    let self = List.fold_left (fun o arg -> o#type_arg arg |> snd) self args in
                    (ty, self)
                | typ -> super#typ typ

              method! row_var row_var =
                if var_eq var row_var then
                  (row_var, {<all_same = false>})
                else
                  super#row_var row_var
            end
          in
          let (_, obj) = obj#typ args in
          let (_, obj) = obj#typ ret in
          let (_, obj) = obj#field_spec_map fields in
          let _, (_, _, count) = Vars.find_spec var vars in
          if obj#all_same && obj#count = count then Shared var else Distinct
      | _ -> Distinct )

  let subkind : (policy * names) -> subkind -> string =
    let full (l, r) = "(" ^ Linearity.to_string l ^ "," ^
                        Restriction.to_string r ^ ")" in

    fun (policy, _vars) ->
    if policy.kinds = "full" then
      full
    else if policy.kinds = "hide" then
      function (_, _) -> ""
    else
      function
      | (Linearity.Unl, Restriction.Any)     -> ""
      | (Linearity.Any, Restriction.Any)     -> "Any"
      | (Linearity.Unl, Restriction.Base)    -> Restriction.to_string res_base
      | (Linearity.Any, Restriction.Session) -> Restriction.to_string res_session
      | (Linearity.Unl, Restriction.Effect)  -> Restriction.to_string res_effect
      | (l, r) -> full (l, r)

  let kind : (policy * names) -> kind -> string =
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

  let quantifier : (policy * names) -> quantifier -> string =
    fun (policy, vars) q ->
      let k = kind_of_quantifier q in
      Vars.find (var_of_quantifier q) vars ^ has_kind (kind (policy, vars) k)

  let rec datatype : context -> policy * names -> datatype -> string =
    fun ({ bound_vars; _ } as context) ((policy, vars) as p) t ->
      let sd = datatype context p in
      let sk k = has_kind (subkind p k) in

      let hide_fresh_check var (flavour, _, count) =
        policy.hide_fresh && count = 1 &&
        ((flavour = `Flexible && not (policy.flavours)) || not (IntSet.mem var bound_vars)) in

      let unwrap = fst -<- unwrap_row in
        (* precondition: the row is unwrapped *)
      let string_of_tuple context (field_env, _, _) =
        let tuple_env =
          FieldEnv.fold
            (fun i f tuple_env ->
               match f with
                 | `Present t         -> IntMap.add (int_of_string i) t tuple_env
                 | (`Absent | `Var _) -> assert false)
            field_env
            IntMap.empty in
        let ss = List.rev (IntMap.fold (fun _ t ss -> (datatype context p t) :: ss) tuple_env []) in
          "(" ^ String.concat ", " ss ^  ")" in

      (* If type variable names are hidden return a generic name n1.
         Otherwise pass name of type variable to n2 so that it can construct a
         name. *)
      let name_of_type var n1 n2 =
        let name, spec = Vars.find_spec var vars in
        if hide_fresh_check var spec then n1 else (n2 name) in

      (* Pretty-prints a row variable *)
      let ppr_row_var context args to_match closed
            (flex_name_hidden, flex_name)
            (name_hidden, name) =
        match Unionfind.find to_match with
        | `Var (var, _, `Flexible) when policy.flavours ->
           name_of_type var flex_name_hidden flex_name
        | `Var (var, _, _) ->
           name_of_type var name_hidden name
        | `Closed      -> closed
        | `Body t'     -> datatype context p (`Function (args, t', t))
        | `Recursive _ -> assert false in

      (* Pretty-prints function spaces.
         `ah` argument stands for "arrow head", either ">" (for normal function
              space) or "@" (for linear types' space). *)
      let ppr_function_type args effects t ah ht =
       let (fields, row_var, dual) = unwrap effects in
       assert (not dual);

       let fields_present = fields_present_in fields in

       let context =
         if policy.effect_sugar then
           let shared_effect = find_shared_effect context vars args fields row_var t in
           { context with shared_effect }
         else context in
       let is_shared =
         match context.shared_effect with
         | Shared _ -> true
         | _ -> false in
       let hidden line =
         if policy.effect_sugar then line ^ "_" ^ line ^ ah else line ^ ah in
       let sd = datatype context p in

       let ppr_arrow () =
         if fields_present [] then
           if policy.hide_fresh && is_shared then "-" ^ ah else
           ppr_row_var context args row_var ("{}-" ^ ah)
               ("-%-" ^ ah, fun name -> "-%" ^ name ^ "-" ^ ah)
               (hidden "-", fun name -> "-"  ^ name ^ "-" ^ ah)
         else if fields_present ["wild"]
         then
           if policy.hide_fresh && is_shared then "~" ^ ah else
           ppr_row_var context args row_var ("{}~" ^ ah)
               ("~%~" ^ ah, fun name -> "~%" ^ name ^ "~" ^ ah)
               (hidden "~", fun name -> "~"  ^ name ^ "~" ^ ah)
         else if fields_present ["hear"; "wild"]
         then
           let ht' = ht fields in
           ppr_row_var context args row_var ("{:" ^ ht' ^ "}~" ^ ah)
               ("{:" ^ ht' ^ "|%}~" ^ ah, fun name -> "{:" ^ ht' ^ "|%" ^ name ^ "}~" ^ ah)
               ("{:" ^ ht' ^ "|_}~" ^ ah, fun name -> "{:" ^ ht' ^ "|"  ^ name ^ "}~" ^ ah)
         else
             (* to guarantee termination it's crucial that we
                invoke row on the original wrapped version of
                the effect row *)
           if FieldEnv.mem "wild" fields &&
             is_present (FieldEnv.find "wild" fields) then
             "{" ^ row ~strip_wild:true "," context p effects ^ "}~" ^ ah
           else
             "{" ^ row "," context p effects ^ "}-" ^ ah
         in begin match concrete_type args with
            | `Record row when is_tuple ~allow_onetuples:true row ->
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
          | `Not_typed       -> "not typed"
          | `Primitive p     -> Primitive.to_string p
          | `MetaTypeVar point ->
              begin
                match Unionfind.find point with
                  | `Var (var, k, `Flexible) when policy.flavours ->
                      (name_of_type var "%" (fun name -> "%" ^ name)) ^ sk k
                  | `Var (var, k, _) ->
                      (name_of_type var "_" (fun name -> name)) ^ sk k
                  | `Recursive (var, body) ->
                      if TypeVarSet.mem var bound_vars then
                        Vars.find var vars
                      else
                        "mu " ^ Vars.find var vars ^ " . " ^
                          datatype { context with bound_vars = TypeVarSet.add var bound_vars } p body
                  | `Body t -> sd t
              end
          | `Function (args, effects, t) ->
             let ht fields =
             match FieldEnv.find "hear" fields with
             | `Present t -> sd t
             | _          -> assert false in
             ppr_function_type args effects t ">" ht
          | `Lolli    (args, effects, t) ->
             let ht fields =
             sd (match FieldEnv.find "hear" fields with
                 | `Present t -> t
                 | _          -> assert false)
             in ppr_function_type args effects t "@" ht
          | `Record r ->
              let ur = unwrap r in
                (if is_tuple ur then string_of_tuple context r
                 else "(" ^ row "," context p r ^ ")")
          | `Variant r -> "[|" ^ row "|" context p r ^ "|]"
          | `Effect r -> "{" ^ row "," context p r ^ "}"
          | `ForAll (tyvars, body) ->
              let bound_vars =
                List.fold_left
                  (fun bound_vars tyvar ->
                     TypeVarSet.add (var_of_quantifier tyvar) bound_vars)
                  bound_vars tyvars
              in
                if not (policy.flavours) then
                  match tyvars with
                  | [] -> datatype { context with bound_vars } p body
                  | _ ->
                     "forall "^ mapstrcat "," (quantifier p) tyvars ^"."^ datatype { context with bound_vars } p body
                else
                  "forall "^ mapstrcat "," (quantifier p) tyvars ^"."^ datatype { context with bound_vars } p body
          | `Input  (t, s) -> "?(" ^ sd t ^ ")." ^ sd s
          | `Output (t, s) -> "!(" ^ sd t ^ ")." ^ sd s
          | `Select bs -> "[+|" ^ row "," context p bs ^ "|+]"
          | `Choice bs -> "[&|" ^ row "," context p bs ^ "|&]"
          | `Dual s -> "~" ^ sd s
          | `End -> "End"
          | `Table (r, w, n)   ->
             (* TODO: pretty-print this using constraints? *)
             "TableHandle(" ^
               sd r ^ "," ^
               sd w ^ "," ^
               sd n ^ ")"
          | `Lens typ ->
            let sort = Lens.Type.sort typ in
            let cols = Lens.Sort.cols sort in
            let pp_col f col =
              Format.fprintf f "%s : %a"
                (Lens.Column.alias col)
                Lens.Phrase.Type.pp (Lens.Column.typ col) in
            Format.asprintf "Lens(%a)"
              (Lens.Utility.Format.pp_comma_list pp_col) cols
          | `Alias ((s,[]), _) ->  Module_hacks.Name.prettify s
          | `Alias ((s,ts), _) ->
             Printf.sprintf "%s (%s)"
               (Module_hacks.Name.prettify s)
               (String.concat "," (List.map (type_arg context p) ts))
          | `Application (l, [elems]) when Abstype.equal l list ->  "["^ (type_arg context p) elems ^"]"
          | `Application (s, []) -> Abstype.name s
          | `Application (s, ts) ->
              let vars = String.concat "," (List.map (type_arg context p) ts) in
              Printf.sprintf "%s (%s)" (Abstype.name s) vars
          | `RecursiveApplication { r_name; r_args; _ } when r_args = [] -> Module_hacks.Name.prettify r_name
          | `RecursiveApplication { r_name; r_args; _ } ->
             Printf.sprintf "%s (%s)"
               (Module_hacks.Name.prettify r_name)
               (String.concat "," (List.map (type_arg context p) r_args))
  and presence ({ bound_vars; _ } as context) ((policy, vars) as p) =
    function
      | `Present t ->
        begin
          match concrete_type t with
          | `Record row when is_empty_row row -> ""
          | _                                 -> ":" ^ datatype context p t
        end
      | `Absent -> "-"
      | `Var point ->
          begin
            let name_of_type var n1 n2 =
              let name, (_, _, count) = Vars.find_spec var vars in
              if policy.hide_fresh && count = 1 && not (IntSet.mem var bound_vars) then n1
              else (n2 name) in
            match Unionfind.find point with
              | `Var (var, _, `Flexible) when policy.flavours ->
                 name_of_type var "{%}" (fun name -> "{%" ^ name ^ "}")
              | `Var (var, _, _) ->
                 name_of_type var "{_}" (fun name -> "{" ^ name ^ "}")
              | `Body f ->
                  presence context p f
          end

  and row ?(strip_wild=false) sep context p (field_env, rv, dual) =
    (* FIXME:

       should quote labels when necessary, i.e., when they
       contain non alpha-numeric characters
    *)
    let field_strings =
      FieldEnv.fold
        (fun label f field_strings ->
          if strip_wild && label = "wild" then
            field_strings
          else
            (label ^ presence context p f) :: field_strings)
        field_env [] in

    let row_var_string = row_var sep context p rv in
      String.concat sep (List.rev (field_strings)) ^
        begin
          match row_var_string with
            | None -> ""
            | Some s -> "|"^ (if dual then "~" else "") ^ s
        end
  and row_var sep ({ bound_vars; _ } as context) ((policy, vars) as p) rv =
    let name_of_type var k n1 n2 =
     let name, (_, _, count) = Vars.find_spec var vars in
     Some ((if policy.hide_fresh && count = 1 && not (IntSet.mem var bound_vars)
            then n1
            else (n2 name))
          ^ has_kind (subkind p k)) in
    match Unionfind.find rv with
      | `Closed -> None
      | `Var (var, k, `Flexible) when policy.flavours ->
         name_of_type var k "%" (fun name -> "%" ^ name)
      | `Var (var, k, _) ->
         name_of_type var k "_" (fun name -> name)
      | `Recursive (var, r) ->
          if TypeVarSet.mem var bound_vars then
            Some (Vars.find var vars)
          else
            Some ("(mu " ^ Vars.find var vars ^ " . " ^
                    row sep { context with bound_vars = TypeVarSet.add var bound_vars } p r ^ ")")
      | `Body r -> Some (row sep context p r)

  and type_arg context p =
    function
      | `Type t -> datatype context p t
      | `Row r -> "{ " ^ row "," context p r ^ " }"
      | `Presence f -> "::Presence (" ^ presence context p f ^ ")"

  let tycon_spec ({ bound_vars; _ } as context) p =
    let bound_vars tyvars =
      List.fold_left
        (fun bound_vars tyvar ->
           TypeVarSet.add (var_of_quantifier tyvar) bound_vars)
        bound_vars tyvars in

    function
      | `Alias (tyvars, body) ->
          let ctx = { context with bound_vars = bound_vars tyvars } in
          begin
            match tyvars with
              | [] -> datatype ctx p body
              | _ -> mapstrcat "," (quantifier p) tyvars ^"."^ datatype ctx p body
          end
      | `Mutual _ -> "mutual"
      | `Abstract _ -> "abstract"

  let strip_quantifiers =
    function
      | `ForAll (_, t)
      | t -> t
end


let free_bound_type_vars ?(include_aliases=true) = Vars.free_bound_type_vars ~include_aliases TypeVarSet.empty
let free_bound_row_type_vars ?(include_aliases=true) = Vars.free_bound_row_type_vars ~include_aliases TypeVarSet.empty
let free_bound_field_spec_type_vars ?(include_aliases=true) = Vars.free_bound_field_spec_type_vars ~include_aliases TypeVarSet.empty
let free_bound_type_arg_type_vars ?(include_aliases=true) = Vars.free_bound_tyarg_vars ~include_aliases TypeVarSet.empty
let free_bound_row_var_vars ?(include_aliases=true) = Vars.free_bound_row_var_vars ~include_aliases TypeVarSet.empty
let free_bound_quantifier_vars = Vars.free_bound_quantifier_vars
let free_bound_tycon_type_vars ?(include_aliases=true) = Vars.free_bound_tycon_vars ~include_aliases TypeVarSet.empty

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
let build_tyvar_names (f : 'a -> Vars.vars_list) (tys : 'a list) =
  Vars.tyvar_name_counter := 0;
  Hashtbl.reset Vars.tyvar_name_map;
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

(* string conversions *)
let string_of_datatype ?(policy=Print.default_policy) ?(refresh_tyvar_names=true)
                       (t : datatype) =
  if Settings.get_value Basicsettings.print_types_pretty then
    let policy = policy () in
    let t = if policy.Print.quantifiers then t
            else Print.strip_quantifiers t in
    if refresh_tyvar_names then build_tyvar_names (fun x -> free_bound_type_vars x) [t];
    Print.datatype Print.empty_context (policy, Vars.tyvar_name_map) t
  else
    show_datatype (DecycleTypes.datatype t)

let string_of_row ?(policy=Print.default_policy) ?(refresh_tyvar_names=true) row =
  if Settings.get_value Basicsettings.print_types_pretty then
    begin
    if refresh_tyvar_names then build_tyvar_names (fun x -> free_bound_row_type_vars x) [row];
    Print.row "," Print.empty_context (policy (), Vars.tyvar_name_map) row
    end
  else
    show_row (DecycleTypes.row row)

let string_of_presence ?(policy=Print.default_policy) ?(refresh_tyvar_names=true)
                       (f : field_spec) =
  if refresh_tyvar_names then
    build_tyvar_names (fun x -> free_bound_field_spec_type_vars x) [f];
  Print.presence Print.empty_context (policy (), Vars.tyvar_name_map) f

let string_of_type_arg ?(policy=Print.default_policy) ?(refresh_tyvar_names=true)
                       (arg : type_arg) =
  if refresh_tyvar_names then
    build_tyvar_names (fun x -> free_bound_type_arg_type_vars x) [arg];
  Print.type_arg Print.empty_context (policy (), Vars.tyvar_name_map) arg

let string_of_row_var ?(policy=Print.default_policy) ?(refresh_tyvar_names=true) row_var =
  if refresh_tyvar_names then
    build_tyvar_names (fun x -> free_bound_row_var_vars x) [row_var];
  match Print.row_var "," Print.empty_context (policy (), Vars.tyvar_name_map) row_var
  with | None -> ""
       | Some s -> s

let string_of_tycon_spec ?(policy=Print.default_policy) ?(refresh_tyvar_names=true) (tycon : tycon_spec) =
  if refresh_tyvar_names then
    build_tyvar_names (fun x -> free_bound_tycon_type_vars x) [tycon];
  Print.tycon_spec Print.empty_context (policy (), Vars.tyvar_name_map) tycon

let string_of_quantifier ?(policy=Print.default_policy) ?(refresh_tyvar_names=true) (quant : quantifier) =
  if refresh_tyvar_names then
    build_tyvar_names (fun x -> free_bound_quantifier_vars x) [quant];
  Print.quantifier (policy (), Vars.tyvar_name_map) quant


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

let string_of_environment = show_environment

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
      | `Alias ((_name, ts), d)  -> union (List.map (make_env_ta boundvars) ts @ [make_env boundvars d])
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
      | `Alias ((name, []), _), `Alias ((name', []), _) when name=name' -> true
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
let make_endbang_type : datatype = `Alias (("EndBang", []), `Output (unit_type, `End))

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

let recursive_applications t =
  let o = new GetRecursiveApplications.visitor in
  let (_, o) = o#typ t in
  o#get_applications |> StringSet.elements

(* We replace some of the generated printing functions here such that
   they may use our own printing functions instead. If the generated functions are
   to be used, we remove potential cycles arising from recursive types/rows first.
   They are here because they are needed
   by the generated code for printing the IR, do not call them yourself.
   Use string_of_* instead *)
let pp_datatype : Format.formatter -> datatype -> unit = fun fmt t ->
  if Settings.get_value Basicsettings.print_types_pretty then
    Format.pp_print_string fmt (string_of_datatype t)
  else
    pp_datatype fmt (DecycleTypes.datatype t)
let pp_quantifier : Format.formatter -> quantifier -> unit = fun fmt t ->
  if Settings.get_value Basicsettings.print_types_pretty then
    Format.pp_print_string fmt (string_of_quantifier t)
  else
    pp_quantifier fmt (DecycleTypes.quantifier t)
let show_quantifier : quantifier -> string = (fun x -> Format.asprintf "%a" pp_quantifier x)
let pp_type_arg : Format.formatter -> type_arg -> unit = fun fmt t ->
  if Settings.get_value Basicsettings.print_types_pretty then
    Format.pp_print_string fmt (string_of_type_arg t)
  else
    pp_type_arg fmt (DecycleTypes.type_arg t)
let pp_tycon_spec : Format.formatter -> tycon_spec -> unit = fun fmt t ->
  let decycle_tycon_spec = function
    | `Alias (qlist, ty) -> `Alias (List.map DecycleTypes.quantifier qlist, DecycleTypes.datatype ty)
    | other -> other in

  if Settings.get_value Basicsettings.print_types_pretty then
    Format.pp_print_string fmt (string_of_tycon_spec t)
  else
    pp_tycon_spec fmt (decycle_tycon_spec t)
let pp_row : Format.formatter -> row -> unit = fun fmt t ->
  if Settings.get_value Basicsettings.print_types_pretty then
    Format.pp_print_string fmt (string_of_row t)
  else
    pp_row fmt (DecycleTypes.row t)

let unwrap_list_type = function
  | `Application ({Abstype.id = "List"; _}, [`Type t]) -> t
  | _ -> assert false
