open Utility
open CommonTypes
open Sugartypes

(*

TODO: after this, the presence variables inserted for fixing effect rows
may violate the uniquuness coniditon for type variables,
because we may insert the same variable twice, but they are scoped differently (TODO: rethink ?)

*)



(* Name used to indicate that a certain (originally anonymous) row variable
   should be replaced by a shared row variabled later on. *)
let shared_effect_var_name = "$eff"

let has_effect_sugar () = Settings.get Types.effect_sugar

let internal_error message =
  Errors.internal_error ~filename:"desugarEffects.ml" ~message

let found_non_var_meta_var =
  internal_error "Every meta_*_var in a SugarTypeVar must be a `Var at this point"

let unpack_var_id = function
  | `Var (id, subkind, _) -> id, subkind
  | _ -> raise found_non_var_meta_var


module SEnv = Env.String

type tycon_info = Quantifier.t list * bool

type simple_tycon_env = tycon_info SEnv.t


let simplify_tycon_env = failwith "123"

let is_anon stv =
  let (name, _, _) = SugarTypeVar.get_unresolved_exn stv in
  name.[0] = '$'


let make_anon_point sk freedom =
     let var = Types.fresh_raw_variable () in
     Unionfind.fresh (`Var (var, DesugarTypeVariables.concrete_subkind sk, freedom))

(* A map with SugarTypeVar as keys, use for associating the former
   with information about what

*)

module type ROW_VAR_MAP =
sig

  type key = SugarTypeVar.t
  type 'a t

  val empty : 'a t

  val add : key -> 'a -> 'a t -> 'a t
  val find_opt : key -> 'a t -> 'a option
  val map : ('a -> 'b) -> 'a t -> 'b t
  val remove : key -> 'a t -> 'a t





  (* Predicate telling you if a given sugar variable should/can be
     handled by this map *)
  val is_relevant : SugarTypeVar.t -> bool

  (* like remove, but remove an entry by using the int id in the quantifier *)
  val remove_by_quantifier : SugarQuantifier.t -> 'a t -> 'a t
  val update_by_quantifier : SugarQuantifier.t -> ('a option -> 'a option) -> 'a t -> 'a t
  val find_opt_by_quantifier : SugarQuantifier.t -> 'a t -> 'a option

end

module RowVarMap : ROW_VAR_MAP =
struct

  type key = SugarTypeVar.t

  (* internal representation, hidden *)
  type 'a t = 'a IntMap.t

  let is_relevant : SugarTypeVar.t -> bool =
    let open SugarTypeVar in function
    | TUnresolved (name, _, _) when name <> shared_effect_var_name -> false
    | _ -> true


  (* helpers *)
  let get_var = let open SugarTypeVar in function
        | TUnresolved       (name, _, _) when name = shared_effect_var_name ->
           (* magic number specially used for $eff *)
           -1
        | TUnresolved       (_, _, _) ->
           raise (internal_error ("must only use SugarTypeVarMap on resoled SugarTypeVars OR the special unresolved one
                                  named " ^ shared_effect_var_name))
        | TResolvedType     mtv ->
           fst (unpack_var_id (Unionfind.find mtv))
        | TResolvedRow      mrv ->
           fst( unpack_var_id (Unionfind.find mrv))
        | TResolvedPresence mpv ->
           fst (unpack_var_id (Unionfind.find mpv))

  let var_id_from_quantifier =
    let open SugarQuantifier in
    function
      | QResolved (var, _) -> var
      | QUnresolved (_, _, _) -> raise
          (internal_error "must not call *_by_quantifier functions on unresolved quantifiers")

  (* functions using SugarTypeVar.t as key *)
  let find_opt : key -> 'a t -> 'a option = fun k m ->
    let var = get_var k in
    IntMap.find_opt var m

  let add : key -> 'a -> 'a t -> 'a t = fun k v m ->
    let var = get_var k in
    IntMap.add var v m

  let empty = IntMap.empty

  let map : ('a -> 'b) -> 'a t -> 'b t = fun f m ->
    IntMap.map f m

  let remove : key -> 'a t -> 'a t = fun k m ->
    let var = get_var k in
    IntMap.remove var m


  (* functions using SugarQuantifier.t as key *)
  let update_by_quantifier : SugarQuantifier.t -> ('a option -> 'a option) -> 'a t -> 'a t = fun q f m ->
    let var = var_id_from_quantifier q in
    IntMap.update var f m

  let find_opt_by_quantifier : SugarQuantifier.t -> 'a t -> 'a option = fun q m ->
    let var = var_id_from_quantifier q in
    IntMap.find_opt var m

  let remove_by_quantifier q map =
    let var = var_id_from_quantifier q in
    IntMap.remove var map


end

let raise_unbound_tycon name pos =
  raise (Errors.UnboundTyCon (pos, name))

(** Whether this type may have an shared effect variable appear within it.
 We insert the shared effect variable at the right most candidate on any
   function. This will either be the arrow itself, or a type application to a
   type which accepts an effect in the last place. *)
let may_have_shared_eff (tycon_env : simple_tycon_env) dt =
  let open Datatype in
  let node = SourceCode.WithPos.node dt in
  match node with
  | Function _ | Lolli _ -> true
  | TypeApplication (tycon, _) ->
     let _, has_implicit_effect = SEnv.find tycon tycon_env in
     has_implicit_effect
  (* TODO: in the original version, this was true for every tycon with a Row var with restriction effect as the last param *)
  | _ -> false


(** Perform some initial desugaring of effect rows, to make them more amenable
   to later analysis.
  - Elaborate operations in effect rows, converting from the various sugared
     forms to the canonical one.
  - Remap anonymous effect variables to the correct version.
    - If effect sugar is disabled, all anonymous effect variables become "$".
   - If we've an unnamed effect in a non-tail position (i.e. not the far
      right of an arrow/typename chain) then remap to "$". For instance,
      `(a) -> (b) -> c` becomes `(a) -$-> (b) -$-> c`.
   - If we're an anonymous variable in a row, remap to "$". (For instance,
      ` -_->` becomes `-$eff->`. *)
let cleanup_effects tycon_env =
  let has_effect_sugar = has_effect_sugar () in
  (object(self)
     inherit SugarTraversals.map as super

    method! datatype dt =
       let open Datatype in
       let open SourceCode.WithPos in
       let {pos; node=t} = dt in
       let do_fun a e r =
          let a = self#list (fun o -> o#datatype) a in
          let has_shared = may_have_shared_eff tycon_env r in
          let e = self#effect_row ~allow_shared:(not has_shared) e in
          let r = self#datatype r in
          (a, e, r)
       in
       let res_t = match t with
         | Function (a, e, r) -> let a, e, r = do_fun a e r in Function (a, e, r)
         | Lolli (a, e, r) -> let a, e, r = do_fun a e r in Lolli (a, e, r)
         | TypeApplication (name, ts) ->
            let tycon_info = SEnv.find_opt name tycon_env in
            let rec go =
               (* We don't know if the arities match up yet (nor the final arities
                  of the definitions), so we handle mismatches, assuming spare rows
                  are effects.
                  This is slightly dodgy: Processing the extra argument in the
                  wrong way may lead to a type error being shown to the user,
                  even though the actual error was the arity missmatch. *)
              function
              | _, [] -> []
              | [], Row t :: ts  ->
                 Row (self#effect_row ~allow_shared:false t) :: go ([], ts)
              | (_, (PrimaryKind.Row, (_, Restriction.Effect))) :: qs, Row t :: ts ->
                 Row (self#effect_row ~allow_shared:false t) :: go (qs, ts)
              | ([] as qs | _ :: qs), t :: ts -> self#type_arg t  :: go (qs, ts)
            in
            let ts =
              match tycon_info with
              | Some (qs, _) -> go (qs, ts)
              | None -> raise (Errors.UnboundTyCon (pos, name))
            in
            TypeApplication (name, ts)
         | _ -> super#datatypenode t
       in
       SourceCode.WithPos.with_node dt res_t

    method! type_variable x = x



    method effect_row ~allow_shared (fields, var) =
       let open Datatype in
       let open SourceCode.WithPos in
       let fields =
         List.map (function
             | (name, Present { node = Function (domain, (fields, rv), codomain); pos }) as op
                  when not (TypeUtils.is_builtin_effect name) -> (
               (* Elaborates `Op : a -> b' to `Op : a {}-> b' *)
               match (rv, fields) with
               | Closed, [] -> op
               | Open _, []
               | Recursive _, [] ->
                  (* might need an extra check on recursive rows *)
                  (name, Present (SourceCode.WithPos.make ~pos (Function (domain, ([], Closed), codomain))))
               | _, _ ->
                  raise
                    (Errors.Type_error
                       ( pos,
                         "The abstract operation " ^ name ^ " has unexpected "
                         ^ "effects in its signature. The effect signature on an "
                         ^ "abstract operation arrow is always supposed to be empty, "
                         ^ "since any effects it might have are ultimately conferred by its handler."
             )) )
             | name, Present node when not (TypeUtils.is_builtin_effect name) ->
                (* Elaborates `Op : a' to `Op : () {}-> a' *)
                name, Present (SourceCode.WithPos.make ~pos:node.pos (Function ([], ([], Closed), node)))
             | x -> x)
           fields
       in
      let gue = SugarTypeVar.get_unresolved_exn in
       let var = match var with
         | Datatype.Open stv
              when (not allow_shared || not has_effect_sugar)
                   && (not (SugarTypeVar.is_resolved stv))
                   && SugarTypeVar.get_unresolved_name_exn stv = shared_effect_var_name ->
            let (_, sk, fr) = gue stv in
            let stv' = SugarTypeVar.mk_unresolved "$" sk fr in
            Datatype.Open stv'
         | Datatype.Open stv when allow_shared && has_effect_sugar
                                  && (not (SugarTypeVar.is_resolved stv))
                                  && gue stv = ("$", None, `Rigid) ->
            let stv' = SugarTypeVar.mk_unresolved "$eff" None `Rigid in
            Datatype.Open stv'
         | _ -> var
       in
       self#row (fields, var)
   end)#datatype


   (** Gathers some information about type names, used for later analysis.
       Precondition: cleanup_effects ran on this type. *)
   let gather_mutual_info (tycon_env : simple_tycon_env) =
     (object(o)
        inherit SugarTraversals.fold as super

        val has_implicit = false
        val used_types = StringSet.empty

        (** Determine if this type contains the shared effect variable ("$eff").

            In order for this to be accurate, it should be run after
           {!cleanup_effects} - otherwise we may have superfluous "$eff"s. *)
        method has_implicit = has_implicit

        (** Any types that this typename consumes. This is used
           to propagate implicit effectiness and linearity of definitions. *)
        method used_types = used_types

        method with_implicit = {<has_implicit = true>}
        method with_used_type ty = {<used_types = StringSet.add ty used_types>}

        method! type_variable _x = o

        method! datatype dt =
          let open Datatype in
          let open SourceCode.WithPos in
          let pos, t = dt.pos, dt.node in
          let self = super#datatypenode t in
          match t with
          | Function (_, (_, eff_var), _) | Lolli (_, (_, eff_var), _) ->
             begin match eff_var with
             | Datatype.Open stv
                  when (not (SugarTypeVar.is_resolved stv))
                       && SugarTypeVar.get_unresolved_exn stv = ("$eff", None, `Rigid) ->
                self#with_implicit
             | _ -> self
             end
          | TypeApplication (name, ts) ->
             let tycon_info = SEnv.find_opt name tycon_env in
             let self = self#list(fun o ta -> o#type_arg ta) ts in
             begin
               match tycon_info with
               | Some (_, other_has_implicit) when other_has_implicit ->
                  self#with_implicit#with_used_type name
               | Some _->
                  self#with_used_type name
               | None ->
                  raise (Errors.UnboundTyCon (pos, name))
             end
             | _ -> self
     end)#datatype

   (** Gather information about which operations are used with which row
      variables.
      Precondition: cleanup_effects ran on this type *)
   let gather_operations (tycon_env : simple_tycon_env) allow_fresh dt =
     let o =
       object(self)
         inherit SugarTraversals.fold as super

         val operations = RowVarMap.empty
         method operations = operations

         method replace quantifier map =
           let ubq = RowVarMap.update_by_quantifier in
           let fobq = RowVarMap.find_opt_by_quantifier in
           {<operations = ubq quantifier (fun _ -> fobq quantifier map) operations>}

         method quantified action (qs : SugarQuantifier.t list) =
           let mask_operations =
             List.fold_left
               (fun o sq -> RowVarMap.remove_by_quantifier sq o)
               operations
               qs
           in
           let o = action {<operations = mask_operations>} in
           List.fold_left
             (fun o sq -> o#replace sq operations)
             o
             qs

         method! type_variable _x = self

         method add (var : SugarTypeVar.t) op =
           if TypeUtils.is_builtin_effect op || not (RowVarMap.is_relevant var) then
             self
           else
             let ops =
               match RowVarMap.find_opt var operations with
               | None -> StringSet.singleton op
               | Some t -> StringSet.add op t
             in
             {<operations = RowVarMap.add var ops operations>}

         method! datatype dt =
           let open Datatype in
           let open SourceCode.WithPos in
           let {pos; node=t} = dt in
           match t with
           | Function (a, e, t) | Lolli (a, e, t) ->
              let o = self#list (fun o -> o#datatype) a in
              let o = o#effect_row e in
              let o = o#datatype t in
              o
           | TypeApplication (name, ts) ->
              let tycon_info = SEnv.find_opt name tycon_env in
              let rec go o =
                (* We don't know if the arities match up yet, so we handle
                      mismatches, assuming spare rows are effects. *)
                function
                | _, [] -> o
                | (_, (PrimaryKind.Row, (_, Restriction.Effect))) :: qs, Row t :: ts ->
                   go (o#effect_row t) (qs, ts)
                | ([] as qs | _ :: qs), t :: ts -> go (o#type_arg t) (qs, ts)
              in
              begin
                match tycon_info with
                | Some (qs, _has_implict_eff) ->
                   go self (qs, ts)
                | None ->
                   raise (Errors.UnboundTyCon (pos, name))
              end
           | Mu (v, t) ->
              let mtv = SugarTypeVar.get_resolved_type_exn v in
              let var, sk = unpack_var_id (Unionfind.find mtv) in
              let q : Quantifier.t = var, (pk_type, sk) in
              let sq = SugarQuantifier.mk_resolved q in
              self#quantified (fun o -> o#datatype t) [sq]
           | Forall (qs, t) -> self#quantified (fun o -> o#datatype t) qs
           | _ -> super#datatype dt



         method! row_var =
           let open Datatype in
           function
           | Closed | Open _ -> self
           | Recursive (v, r) ->
              let mtv = SugarTypeVar.get_resolved_type_exn v in
              let var, sk = unpack_var_id (Unionfind.find mtv) in
              let q : Quantifier.t = var, (pk_row, sk) in
              let sq = SugarQuantifier.mk_resolved q in
              self#quantified (fun o -> o#row r) [sq]


         method effect_row ((fields, var) : Datatype.row) =
           let self =
             match var with
             | Datatype.Open stv ->

                List.fold_left (fun o (op, _) -> o#add stv op) self fields
             | _ -> self
           in
           self#row (fields, var)
       end
     in
     if allow_fresh && has_effect_sugar () then
       (o#datatype dt)#operations
       |> RowVarMap.map (fun v ->
              StringSet.fold
                (fun op m ->
                  let point =
                    lazy begin
                        let var = Types.fresh_raw_variable () in
                        Unionfind.fresh (`Var (var, default_subkind, `Rigid))
                      end
                  in
                  StringMap.add op point m) v StringMap.empty)
     else
       RowVarMap.empty




let preprocess_type (dt : Datatype.with_pos) tycon_env allow_fresh shared_effect =
  let dt = cleanup_effects tycon_env dt in
  let row_operations = gather_operations tycon_env allow_fresh dt in
  let shared_effect = match shared_effect with
    | None when allow_fresh && has_effect_sugar () ->
       let point =
         lazy begin
             let var = Types.fresh_raw_variable () in
             Unionfind.fresh (`Var (var, (lin_unl, res_any), `Rigid))
           end
       in
       Some point
    | _ ->
       (* If the shared_effect variable was already created, for instance
          by the typename logic, we don't have to create one *)
       shared_effect
  in
  dt, row_operations, shared_effect


class main_traversal =
  object (o : 'self_type)
    inherit SugarTraversals.fold_map as super


    (** True if we are visiting a type at the moment.
       Using this, whenever we vistit a datatype we can ditinguish
       between this node being a child node of another type or not.
       Some special processing must be done at the "top" of each type node,
       but not on its children. *)
    val inside_type = false


    (** The active shared effect variable, if allowed to be used. *)
    val shared_effect : Types.meta_row_var Lazy.t option = None

    (** Allow implicitly bound type/row/presence variables in the current context?
       This does not effect the special, implictly bound effect variable
       whose appearance is controled by {!shared_effect} being None *)
    val allow_implictly_bound_vars = true


    val tycon_env : simple_tycon_env = SEnv.empty



    (** Map of effect variables to all mentioned operations, and their
        corresponding effect variables. *)
    val row_operations : Types.meta_presence_var Lazy.t StringMap.t RowVarMap.t = RowVarMap.empty

    method set_inside_type inside_type = {< inside_type >}

    method set_tycon_env tycon_env = {< tycon_env>}

    method set_allow_implictly_bound_vars allow_implictly_bound_vars = {< allow_implictly_bound_vars>}

    method set_shared_effect shared_effect = {< shared_effect >}

    method disallow_shared_effect = {< shared_effect = None >}


   method! phrasenode =
     let open Sugartypes in
     function
     | Block (_bs, _p) as b ->
        (* aliases bound in `bs'
           should not escape the scope of the block *)
        let o, b = super#phrasenode b in
        o#set_tycon_env tycon_env, b

    | p -> super#phrasenode p



   method! datatypenode =
     let open Datatype in
     let do_fun a e r =
       let o, a = o#list (fun o -> o#datatype) a in
       let o, e = o#effect_row  e in
       let o, r = o#datatype r in
       o, a, e, r
     in
     function
     | Function (a, e, r) ->
        let o, a, e, r = do_fun a e r in
        o, Function (a, e, r)
     | Lolli (a, e, r) ->
        let o, a, e, r = do_fun a e r in
        o, Lolli (a, e, r)
     | TypeVar stv ->
        if (not (SugarTypeVar.is_resolved stv)) then
          raise (internal_error "All type variables (of kind Type) must have been resolved at this point");
        o, TypeVar stv
     | TypeApplication (tycon, ts) ->
        (* We need to process the arguments for a type constructor.
           For rows as arguments, we need the expected kinding info to tell
           us whether to process as normal row or effect row.
           Finally, for types expecting a new implicit effect var,
           we add it as the final argument.
           We have to report certain cases of arity/kinding mismatch errors
           here, but we are not exhaustively catching all problems with
           type applications. This must be done in later passes. *)
        let pos = SourceCode.Position.dummy in
        begin
          match SEnv.find_opt tycon tycon_env with
          | None -> raise (Errors.UnboundTyCon (pos, tycon))
          | Some (qs, has_implicit_eff) ->
             let qn = List.length qs in
             let tn = List.length ts in
             let arity_err () =
               raise (Errors.TypeApplicationArityMismatch { pos; name = tycon; expected = qn; provided = tn })
             in
             let module PK = PrimaryKind in
             let process_type_arg i : (Quantifier.t * type_arg) -> Datatype.type_arg = function
               | (_, (PK.Row, (_, Restriction.Effect))), Row r ->
                  let _o, erow = o#effect_row r in
                  Row erow
               | (_, (PK.Row, _)) , Row r ->
                  let _o, row = o#row r in
                  Row row
               | q, Row _ ->
                  (* Here we don't know how to transform the row.
                     Transforming it the wrong way may lead to type errors
                     distracting the user from the actual error: the kind missmatch.
                     Hence, we must report a proper error here. *)
                  let q_kind = Quantifier.to_primary_kind q in
                  raise
                    (Errors.TypeApplicationKindMismatch
                       { pos;
                         name = tycon;
                         tyarg_number = i;
                         expected = PrimaryKind.to_string q_kind;
                         provided = PrimaryKind.to_string pk_row
                       })
               | _, ta ->
                  snd (o#type_arg ta)
             in
             let rec match_args_to_params index = function
               | q :: qs, ta :: tas ->
                  process_type_arg index (q, ta) :: (match_args_to_params (index+1) (qs, tas))
               | _q :: _qs, [] ->
                  (* this *may* be an arity mismatch, but not handling it
                     here doesn't cause trouble. *)
                  []
               | [], _ta :: _tas ->
                  (* As above, must report proper error here to avoid confusion *)
                  arity_err ()
               | [], [] -> []
             in
             let ts = match_args_to_params 1 (qs, ts) in
             let ts = match has_implicit_eff, shared_effect with
               | true, Some lazy_eff ->
                  (* insert shared effect as final argument *)

                  (* Looking for this gives us the operations associcated with
                  the $eff var. The kind and freedom info are ignored for the lookup *)
                  let eff_sugar_var = SugarTypeVar.mk_unresolved shared_effect_var_name None `Rigid in

                  let fields = match RowVarMap.find_opt eff_sugar_var row_operations with
                    | None -> []
                    | Some ops ->
                       StringMap.fold
                         (fun op p fields ->
                           let concern =
                             (Errors.Type_error
                                (pos,
                                 "The effect sugar requires inserting an effect row as a type argument here. "
                                  ^ "This effect row uses an implicitly bound presence variable for the effect "
                                  ^ op ^ ". However, in the current context, implictly bound (presence) variables are disallowed")) in
                           let mpv : Types.meta_presence_var = Lazy.force p in
                           let fieldspec = Datatype.Var (SugarTypeVar.mk_resolved_presence mpv) in
                           if not allow_implictly_bound_vars then
                             raise concern;
                           (op, fieldspec) :: fields)
                         ops
                         []
                  in
                  let row_var = Datatype.Open (Lazy.force lazy_eff |> SugarTypeVar.mk_resolved_row) in
                  let eff : Datatype.row = (fields, row_var) in
                  ts @ [ Row eff ]
               | true, None ->
                  let concern =
                    Errors.Type_error
                      (pos,
                       "The effect sugar requires inserting an open effect row as a type argument here. " ^
                       "However, we are not allowed to introduce an implicitly bound (effect) variable in the current context.")
                  in
                  raise concern
               | false, _ -> ts
             in
             o, TypeApplication (tycon, ts)
        end

     | Forall (qs, t) ->
        let o = o#set_allow_implictly_bound_vars false in
        let o = o#disallow_shared_effect in
        let o, t = o#datatype t in
        let o = o#set_allow_implictly_bound_vars allow_implictly_bound_vars in
        let o = o#set_shared_effect shared_effect in
        o, Forall (qs, t)
     | t -> super#datatypenode t


   method! row (fields, rv) =
        let dpos = SourceCode.Position.dummy in
        let module D = Datatype in
        let o, rv = match rv with
          | D.Closed -> o, rv
          | D.Open stv when
                 (not (SugarTypeVar.is_resolved stv))
                 && SugarTypeVar.get_unresolved_name_exn stv = shared_effect_var_name ->
             begin
               match shared_effect with
               | None ->
                  raise
                    (Errors.Type_error
                       (dpos, "Trying to use (shared) effect variable that is implicitly bound in in context where no such implictly binding of type variables is allowed." ))
               | Some s -> o, D.Open (Lazy.force s |> SugarTypeVar.mk_resolved_row)
             end
          | D.Open stv when (not (SugarTypeVar.is_resolved stv)) && is_anon stv ->
             if (not allow_implictly_bound_vars) then
               raise (DesugarTypeVariables.free_type_variable dpos);

             let (_name, sk, freedom) = SugarTypeVar.get_unresolved_exn stv in
             let mtv = make_anon_point sk freedom in
             let rtv = SugarTypeVar.mk_resolved_row mtv in
             o, D.Open rtv
          | D.Open srv when (not (SugarTypeVar.is_resolved srv)) ->
             raise (internal_error "Encountered non-anonymous, unresolved effect variable. All such variables must have been resolved by earlier transformations.")
          | D.Open _ ->
             o, rv
          | D.Recursive (stv, r) ->
             let o, r = o#row r in
             o, D.Recursive (stv, r)
         in
         let o, fields =
           o#list
             (fun o (name, fs) -> let o, fs = o#fieldspec fs in o, (name, fs))
             fields
         in
         o, (fields, rv)



     method effect_row ((fields, rv) : Datatype.row) =
       let o, (fields, rv) = o#row (fields, rv) in
       let dpos = SourceCode.Position.dummy in
       let fields =
         match rv with
         | Datatype.Open stv when RowVarMap.is_relevant stv ->
            begin match RowVarMap.find_opt stv row_operations with
            | Some ops ->
               let ops_to_add = List.fold_left (fun ops (op, _) -> StringMap.remove op ops) ops fields in
               let add_op op pres_var fields =
                 if not allow_implictly_bound_vars then
                   (* Alternatively, we could just decide not to touch the row and let the type checker
                      complain about the incompatible rows? *)
                   raise
                     (Errors.Type_error
                                (dpos,
                                 "To fix the kinds of the effect variable, need to insert operation " ^ op ^
                                   "here, and make it polymorphic in its presence.
                                    However, in the current context, implictly bound (presence) variables are disallowed."));
                 let rpv = SugarTypeVar.mk_resolved_presence (Lazy.force pres_var)  in
                 (op, Datatype.Var rpv) :: fields
               in
               StringMap.fold add_op ops_to_add fields
            | None -> fields
            end
         | _ -> fields
       in
       o, (fields, rv)



     method! bindingnode = function
       | Typenames ts ->
          let open SourceCode.WithPos in
          let tycon_env =
          List.fold_left
            (fun alias_env {node=(t, args, _); _} ->
              let qs = List.map (SugarQuantifier.get_resolved_exn) args in
              SEnv.bind t (qs, false) alias_env)
            tycon_env
            ts
        in

        (* First determine which types require an implicit effect variable. *)
        let (implicits, dep_graph) =
          List.fold_left (fun (implicits, dep_graph) {node=(t, _, (d, _)); _} ->
              let d = cleanup_effects tycon_env d in
              let eff = gather_mutual_info tycon_env d in
              let has_imp = eff#has_implicit in
              let implicits = StringMap.add t has_imp implicits in
              let dep_graph = StringMap.add t (StringSet.elements eff#used_types) dep_graph in
              implicits, dep_graph)
            (StringMap.empty, StringMap.empty) ts
        in
        (* We also gather a dependency graph of our mutually recursive types. Group this into SCCs
           and toposort it. We can then trivially propagate the implicit effect requirement - if
           anyone in our component has an implicit effect, or depends on a type which does, then we
           must too! *)
        let has_implicit implicits name =
          StringMap.find name implicits
          || List.exists (flip StringMap.find implicits) (StringMap.find name dep_graph)
        in
        let sorted_graph = Graph.topo_sort_sccs (StringMap.bindings dep_graph) in
        let implicits =
          List.fold_left (fun implicits scc ->
              let scc_imp = List.exists (has_implicit implicits) scc in
              List.fold_left (fun acc x -> StringMap.add x scc_imp acc) implicits scc)
            implicits sorted_graph
        in
        (* Now patch up the types to include this effect variable. *)
        let patch_type_param_list (tycon_env, shared_var_env, ts) ({node=(t, args, (d, _)); pos} as tn) =
          if StringMap.find t implicits then
            let var = Types.fresh_raw_variable () in
            let q = (var, (PrimaryKind.Row, (lin_unl, res_effect))) in
            (* Add the new quantifier to the argument list and rebind. *)
            (* let qs = List.map (snd ->- OptionUtils.val_of) args @ [q] in *)
            let args = args @ [SugarQuantifier.mk_resolved q] in
            let tycon_env = SEnv.bind t (List.map SugarQuantifier.get_resolved_exn args, true) tycon_env in
            let shared_effect_var : Types.meta_row_var Lazy.t  =
              lazy (Unionfind.fresh (`Var (var, (lin_unl, res_effect), `Rigid)))
            in
            let shared_var_env =
              StringMap.add t (Some shared_effect_var) shared_var_env in
            (tycon_env, shared_var_env, SourceCode.WithPos.make ~pos (t, args, (d, None)) :: ts)
          else
            (* Note that we initially set the has-implict flag to
               false, so there is nothing to do here *)
            let shared_var_env = StringMap.add t None shared_var_env in
            (tycon_env, shared_var_env, tn :: ts)
        in
        let (tycon_env, shared_eff_vars, ts) =
          List.fold_left patch_type_param_list (tycon_env, StringMap.empty, []) ts
        in

        let traverse_body {node=(name, args, dt); pos} =
          if inside_type then
            raise (internal_error "a type definition should never be a child-node of a type");
          let shared_effect = StringMap.find name shared_eff_vars in
          let o = {< tycon_env; shared_effect; allow_implictly_bound_vars = false >} in

          (* TODO: no info to flow back out? *)
          let _o, dt' = o#datatype' dt in

          let (t, dt) =
            match dt' with
            | (t, Some dt) -> (t, dt)
            | _ -> assert false in
          SourceCode.WithPos.make ~pos (name, args, (t, Some dt))
        in

        let ts' = List.map traverse_body ts in
        ({< tycon_env >}, Typenames ts')

       | b -> o#bindingnode b


    method super_datatype = o#datatype

    method! datatype dt =
      let pos = SourceCode.WithPos.pos dt in
      let dt, o =
        if not inside_type then
          let dt, row_operations, shared_effect =
            preprocess_type
              dt
              tycon_env
              allow_implictly_bound_vars
              shared_effect
          in
          dt, {< row_operations; shared_effect >}
        else
          dt, o
      in
      let o = o#set_inside_type true in
      let o, dt =
        Errors.rethrow_errors_if_better_position pos o#super_datatype dt in
      let o = o#set_inside_type inside_type in
      o, dt

  end
