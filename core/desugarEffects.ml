open Utility
open CommonTypes
open Sugartypes

(**

This pass handles effect variables and the "effect sugar" (if enabled).

This pass assumes that all type variables have been resolved expect
anonymous row variables. All non-anonymous effect variables must have been
resolved such that different variables use Var Unionfind points whose integer
ids are different.

The following steps are always performed:

  1. Elaborate operators in effect rows, converting the various short forms
     [{Op:Int}], [{Op:()->Int}] into the "correct" [{Op:() {}-> Int}] form.

  2. Resolve anonymous effect variables, by translating from TUnresolved
     (from {Sugartypes.SugarTypeVar.t} to TResolvedRow. Further, report
     error about such variables occuring in a positon where they are not
     permitted.

The following steps are only performed when effect_sugar is enabled:

  1. Try a simple form of kind inference: For each effect variable, track
     which fields occur in the row it is used (see {gather_operations}).
     Then, for each usage of the variable, insert the missing fields as
     being polymorphic in their presence.

  2. Treat various anonymous effect variables occuring in the same type
     as being the same variable (this is called the "implicit effect variable")

  3. Typenames whose body contains an anonymous effect variable are
     parameterized over an additional effect variable.
     Similar, make provide the implicit effect variable as an additional
     argument in type applications that satisfy the following conditions:

     - The applied type constructor has an effect as the last parameter
     - The type application misses exactly one argument.

 *)


(* Name used to indicate that a certain (originally anonymous) row variable
   should be replaced by a shared row variabled later on. *)
let shared_effect_var_name = "$eff"

let has_effect_sugar () = Types.Policy.effect_sugar (Types.Policy.default_policy ())
let final_arrow_shares_with_alias () =
  let open Types.Policy in
  let policy = default_policy () in
  let es_policy = es_policy policy in
  EffectSugar.final_arrow_shares_with_alias es_policy
let all_implicit_arrows_share () =
  let open Types.Policy in
  let policy = default_policy () in
  let es_policy = es_policy policy in
  EffectSugar.all_implicit_arrows_share es_policy

let internal_error message =
  Errors.internal_error ~filename:"desugarEffects.ml" ~message

let found_non_var_meta_var =
  internal_error
    "Every meta_*_var in a SugarTypeVar must be a Var at this point"

let cannot_insert_presence_var pos op =
  Errors.Type_error
    ( pos,
      "To fix the kinds of the effect variable, need to insert operation "
      ^ op
      ^ " here, and make it polymorphic in its presence. However, in the \
         current context, implictly bound (presence) variables are disallowed."
    )

let cannot_insert_presence_var2 pos op =
  Errors.Type_error
    ( pos,
      "The effect sugar requires inserting an effect row as a type argument \
       here. This effect row uses an implicitly bound presence variable for \
       the effect "
      ^ op
      ^ ". However, in the current context, implictly bound (presence) \
         variables are disallowed" )

(* let unexpected_effects_on_abstract_op pos name = *)
(*   Errors.Type_error *)
(*     ( pos, *)
(*       "The abstract operation " *)
(*       ^ name *)
(*       ^ " has unexpected effects in its signature. The effect signature on an \ *)
(*          abstract operation arrow is always supposed to be empty, since any \ *)
(*          effects it might have are ultimately conferred by its handler." ) *)

let shared_effect_forbidden_here pos =
  Errors.Type_error
    ( pos,
      "Trying to use (shared) effect variable that is implicitly bound in in \
       context where no such implictly binding of type variables is allowed." )

let unpack_var_id = function
  | Types.Var (id, kind, _) -> (id, kind)
  | _ -> raise found_non_var_meta_var

module SEnv = Env.String

(* tycon_info stores information about a type alias;
   - Kind.t list = list of kinds of type arguments
   - bool = whether tycon has implicit shared effect
   - Types.typ option = the actual type inside the alias, if it exists (None for abstract types)
     - this is used to propagate operations, sometimes the type behind an alias will have some
       operations labels hidden inside it *)
type tycon_info = Kind.t list * bool * Types.typ option

type simple_tycon_env = tycon_info SEnv.t

let simplify_tycon_env (tycon_env : Types.tycon_environment) : simple_tycon_env
    =
  let simplify_tycon name tycon simpl_env =
    let param_kinds, internal_type =
      match tycon with
      | `Alias (_, qs, tp) -> List.map Quantifier.to_kind qs, Some tp
      | `Abstract abs -> Types.Abstype.arity abs, None
      | `Mutual _ -> raise (internal_error "Found `Mutual in global tycon env")
    in
    SEnv.bind name (param_kinds, false, internal_type) simpl_env
  in
  SEnv.fold simplify_tycon tycon_env SEnv.empty

let make_anon_point ?(is_eff=true) k sk freedom =
  let var = Types.fresh_raw_variable () in
  Unionfind.fresh
    (Types.Var (var, (k, DesugarTypeVariables.concrete_subkind ~is_effect:is_eff sk), freedom))

(** A map with SugarTypeVar as keys, use for associating the former
   with information about what
*)
module type ROW_VAR_MAP = sig
  type key = SugarTypeVar.t
  type 'a t

  val empty : 'a t

  val add : key -> 'a -> 'a t -> 'a t
  (* TODO renaming to something more obvious *)
  val add_raw : int -> 'a -> 'a t -> 'a t
  val find_raw : int -> 'a t -> 'a
  val find_raw_opt : int -> 'a t -> 'a option
  val update : int -> ('a option -> 'a option) -> 'a t -> 'a t
  val find_opt : key -> 'a t -> 'a option
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold : (int -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

  (* Predicate telling you if a given sugar variable should/can be
     handled by this map *)
  val is_relevant : SugarTypeVar.t -> bool

  (* like remove, but remove an entry by using the int id in the quantifier *)
  val remove_by_quantifier : SugarQuantifier.t -> 'a t -> 'a t
  val update_by_quantifier :
    SugarQuantifier.t -> ('a option -> 'a option) -> 'a t -> 'a t
  val find_opt_by_quantifier : SugarQuantifier.t -> 'a t -> 'a option
end

module RowVarMap : ROW_VAR_MAP = struct
  type key = SugarTypeVar.t

  (* internal representation, hidden *)
  type 'a t = 'a IntMap.t

  let is_relevant : SugarTypeVar.t -> bool =
    let open SugarTypeVar in
    function
    | TUnresolved (name, _, _) when name <> shared_effect_var_name -> false
    | _ -> true

  (* helpers *)
  let get_var =
    let open SugarTypeVar in
    function
    | TUnresolved (name, _, _) when name = shared_effect_var_name ->
        (* magic number specially used for $eff *)
        -1
    | TUnresolved (_, _, _) ->
        raise
          (internal_error
             ( "must only use SugarTypeVarMap on resoled SugarTypeVars OR the \
                special unresolved one named "
             ^ shared_effect_var_name ))
    | TResolvedType mtv -> fst (unpack_var_id (Unionfind.find mtv))
    | TResolvedRow mrv -> fst (unpack_var_id (Unionfind.find mrv))
    | TResolvedPresence mpv -> fst (unpack_var_id (Unionfind.find mpv))

  let var_id_from_quantifier =
    let open SugarQuantifier in
    function
    | QResolved (var, _) -> var
    | QUnresolved (_, _, _) ->
        raise
          (internal_error
             "must not call on unresolved quantifiers")

  (* functions using SugarTypeVar.t as key *)
  let find_opt : key -> 'a t -> 'a option =
   fun k m ->
    let var = get_var k in
    IntMap.find_opt var m

  let add : key -> 'a -> 'a t -> 'a t =
   fun k v m ->
    let var = get_var k in
    IntMap.add var v m

  let add_raw k v m = IntMap.add k v m
  let update k upd m = IntMap.update k upd m
  let find_raw k m = IntMap.find k m
  let find_raw_opt k m = IntMap.find_opt k m

  let empty = IntMap.empty

  let map : ('a -> 'b) -> 'a t -> 'b t = fun f m -> IntMap.map f m

  let fold : (int -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
    = fun f m acc -> IntMap.fold f m acc

  (* functions using SugarQuantifier.t as key *)
  let update_by_quantifier :
      SugarQuantifier.t -> ('a option -> 'a option) -> 'a t -> 'a t =
   fun q f m ->
    let var = var_id_from_quantifier q in
    IntMap.update var f m

  let find_opt_by_quantifier : SugarQuantifier.t -> 'a t -> 'a option =
   fun q m ->
    let var = var_id_from_quantifier q in
    IntMap.find_opt var m

  let remove_by_quantifier q map =
    let var = var_id_from_quantifier q in
    IntMap.remove var map
end

(** Whether this type may have an shared effect variable appear within it.
 We insert the shared effect variable at the right most candidate on any
   function. This will either be the arrow itself, or a type application to a
   type which accepts an effect in the last place. *)
let may_have_shared_eff (tycon_env : simple_tycon_env) dt =
  let open Datatype in
  let node = SourceCode.WithPos.node dt in
  match node with
  | Function _
  | Lolli _ ->
     Some `Arrow
  | TypeApplication (tycon, _) -> (
    let param_kinds, _has_implicit_effect, _internal_type =
      try
        SEnv.find tycon tycon_env
      with NotFound _ -> raise (Errors.unbound_tycon (SourceCode.WithPos.pos dt) tycon)
    in
    match ListUtils.last_opt param_kinds with
    | Some (PrimaryKind.Row, (_, Restriction.Effect)) -> Some `Alias
    | _ -> None )
  (* TODO: in the original version, this was true for every tycon with a Row var with restriction effect as the last param *)
  | _ -> None

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
  (object (self)
     inherit SugarTraversals.map as super

     method! datatype dt =
       let open Datatype in
       let open SourceCode.WithPos in
       let { pos; node = t } = dt in
       let do_fun a e r =
         let a = self#list (fun o -> o#datatype) a in
         let allow_shared = match may_have_shared_eff tycon_env r with
           (* range is irrelevant - this arrow can share effect *)
           | None -> `Allow

           (* range is another arrow and this is a collector in a
              curried function => must be fresh, unless we want to share all arrows *)
           | Some `Arrow ->
              if all_implicit_arrows_share ()
              then `Allow
              else `Disallow

           (* range is an alias, this is a rightmost arrow, effect
              sugar is active => decide based on policies *)
           | Some `Alias ->
              if all_implicit_arrows_share ()
              then `Allow
              else if final_arrow_shares_with_alias ()
              then `Infer
              else `Disallow
         in
         let e = self#effect_row ~allow_shared e in
         let r = self#datatype r in
         (a, e, r)
       in
       let res_t =
         match t with
         | Function (a, e, r) ->
             let a, e, r = do_fun a e r in
             Function (a, e, r)
         | Lolli (a, e, r) ->
             let a, e, r = do_fun a e r in
             Lolli (a, e, r)
         | TypeApplication (name, ts) ->
            let tycon_info =
              try
                SEnv.find_opt name tycon_env
              with NotFound _ -> raise (Errors.unbound_tycon pos name)
            in
            let rec go =
               (* We don't know if the arities match up yet (nor the final arities
                  of the definitions), so we handle mismatches, assuming spare rows
                  are effects.
                  This is slightly dodgy: Processing the extra argument in the
                  wrong way may lead to a type error being shown to the user,
                  even though the actual error was the arity missmatch. *)
               function
               | _, [] -> []
               | [], Row t :: ts ->
                   Row (self#effect_row ~allow_shared:`Disallow t) :: go ([], ts)
               | (PrimaryKind.Row, (_, Restriction.Effect)) :: qs, Row t :: ts
                 ->
                   Row (self#effect_row ~allow_shared:`Disallow t) :: go (qs, ts)
               | (([] as qs) | _ :: qs), t :: ts ->
                   self#type_arg t :: go (qs, ts)
             in
             let ts =
               match tycon_info with
               | Some (params, _, _) -> go (params, ts)
               | None -> raise (Errors.unbound_tycon pos name)
             in
             TypeApplication (name, ts)
         | Effect e ->
            let e = self#effect_row ~allow_shared:`Disallow e in
            Effect e
         | _ -> super#datatypenode t
       in
       SourceCode.WithPos.with_node dt res_t

     method effect_row ~allow_shared (fields, var) =
       let open Datatype in
       let open SourceCode in
       let open WithPos in
       (* Elaborates `Op : a' to `Op : () {}-> a' *)
       let rec elaborate_op dt =
         let { node ; pos } = dt in
         WithPos.make ~pos (match node with
             | Datatype.Operation _     -> node
             | Datatype.Forall (qs, dt) -> Datatype.Forall (qs, elaborate_op dt)
             | _                        -> Datatype.Operation ([], dt, DeclaredLinearity.Unl))
             (* nullary operations without =@ are unlimited *)
       in
       let fields = List.map (function
           | name, Present dt when not (TypeUtils.is_builtin_effect name) ->
             ( name, Present (elaborate_op dt) )
           | x -> x
         ) fields
       in
       let gue = SugarTypeVar.get_unresolved_exn in
       let var =
         match var with
         | Datatype.Open stv ->
            if not (SugarTypeVar.is_resolved stv)
            then begin
                let gen_unresolved_eff () =
                  SugarTypeVar.mk_unresolved shared_effect_var_name ~is_eff:true None `Rigid
                in
                let to_unresolved_general sk fr =
                  SugarTypeVar.mk_unresolved "$" ~is_eff:true sk fr
                in
                let gen_resolved_flex () =
                  SugarTypeVar.mk_resolved_row
                    (let var = Types.fresh_raw_variable () in
                     Unionfind.fresh
                       (Types.Var (var, (PrimaryKind.Row, (default_effect_lin, res_effect)), `Flexible)))
                in
                let name, (_, sk), fr = gue stv in
                if has_effect_sugar
                then
                  begin
                    if (name = "$" || name = shared_effect_var_name)
                       && sk = None && fr = `Rigid (* TODO need sk,fr? *)
                    then let stv' = match allow_shared with
                           | `Allow -> gen_unresolved_eff ()
                           | `Infer -> gen_resolved_flex ()
                           | `Disallow -> to_unresolved_general sk fr
                         in
                         Datatype.Open stv'
                    else var
                  end
                else
                  begin
                    if name = shared_effect_var_name
                    then let stv' = to_unresolved_general sk fr in
                         Datatype.Open stv'
                    else var
                  end
              end
            else var
         | _ -> var
       in
       self#row (fields, var)
  end)
    #datatype

(** Gathers some information about type names, used for later analysis.
    Precondition: cleanup_effects ran on this type. *)
let gather_mutual_info (tycon_env : simple_tycon_env) =
  (object
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

     method! datatype dt =
       let open Datatype in
       let open SourceCode.WithPos in
       let pos, t = (dt.pos, dt.node) in
       let self = super#datatypenode t in
       match t with
       | Function (_, (_, eff_var), _)
       | Lolli (_, (_, eff_var), _) -> (
           match eff_var with
           | Datatype.Open stv
             when (not (SugarTypeVar.is_resolved stv))
                  && SugarTypeVar.get_unresolved_exn stv = ("$eff", (true, None), `Rigid)
             ->
               self#with_implicit
           | _ -> self )
       | TypeApplication (name, ts) -> (
           let tycon_info = SEnv.find_opt name tycon_env in
           let self = self#list (fun o ta -> o#type_arg ta) ts in
           match tycon_info with
           | Some (param_kinds, _other_has_implicit, _internal_type)
             when List.length param_kinds = List.length ts + 1 ->
               let poss_with_implicit =
                 match ListUtils.last param_kinds with
                 | PrimaryKind.Row, (_, Restriction.Effect) ->
                     self#with_implicit
                 | _ -> self
               in
               poss_with_implicit#with_used_type name
           | Some _ -> self#with_used_type name
           | None -> raise (Errors.unbound_tycon pos name) )
       | _ -> self
  end)
    #datatype

let gather_operation_of_type tp
  = let open Types in
    let module FieldEnv = Utility.StringMap in
    let is_effect_row_kind : Kind.t -> bool
      = fun (primary, (_, restriction)) ->
      primary = PrimaryKind.Row && restriction = Restriction.Effect
    in
    let o =
      object (o : 'self_type)
        inherit Types.Transform.visitor as super

        val seen_recapps : stringset = StringSet.empty
        method add_seen_recapp name =
          let seen_recapps = StringSet.add name seen_recapps in
          {< seen_recapps >}

        val operations : stringset RowVarMap.t = RowVarMap.empty
        method operations = operations

        val implicit_shared_var : int option = None
        method implicit_shared_var = implicit_shared_var
        method set_implicit_shared_var : int -> 'self_type
          = fun vid ->
          {< implicit_shared_var = Some vid >}

        method with_label vid label =
          let operations = RowVarMap.update vid
                             (function
                              | None -> Some (StringSet.singleton label)
                              | Some sset -> Some (StringSet.add label sset))
                             operations
          in
          {< operations >}

        method effect_row : maybe_main_var:bool -> row -> 'self_type * row
          = fun ~maybe_main_var r ->
          let (fields, rvar, _) = flatten_row r (* |> fst *) |> extract_row_parts in
          let rvar = Unionfind.find rvar in
          let o = match rvar with
            | Var (vid,_,_)  ->
               (* this is an open effect row, collect its operations *)
               (* FieldEnv.fold (fold_fields vid) fields o *)
               let o = if maybe_main_var
                       then o#set_implicit_shared_var vid
                       else o
               in
               FieldEnv.fold
                 (fun label _field acc ->
                   acc#with_label vid label)
                 fields o
            | _ -> o (* not an open effect row, ignore *)
          in
          (o, r)

        method alias_recapp
          = fun kinds tyargs ->
          let effect_rows = ListUtils.filter_map2
                              (fun (knd, _) -> is_effect_row_kind knd)
                              (fun (_, (_, typ)) -> typ)
                              kinds tyargs in
          List.fold_left
            (fun acc r ->
              fst (acc#effect_row ~maybe_main_var:true r))
            o effect_rows

        method! typ : typ -> 'self_type * typ
          = fun tp ->
          match tp with
          | Function (d,e,r) | Lolli (d,e,r) ->
             let maybe_main_var = match implicit_shared_var with
               | Some _ -> false
               | None ->
                  begin match r with
                  | Function _ | Lolli _
                    | Alias _ | RecursiveApplication _ -> false
                  | _ -> true
                  end
             in
             let (o, _) = o#effect_row ~maybe_main_var e in
             let (o, _) = o#typ r in
             let (o, _) = o#typ d in
             (o, tp)
          | Alias (_, (_,kinds,tyargs,_), inner_tp) ->
             let o = o#alias_recapp kinds tyargs in
             let (o,_) = o#typ inner_tp in
             (o, tp)
          | RecursiveApplication { r_unique_name ; r_quantifiers = kinds
                                   ; r_args = tyargs ; r_unwind; r_dual ; _ } ->
             let o = o#alias_recapp kinds tyargs in
             let o = if StringSet.mem r_unique_name seen_recapps
                     then o (* skip, decycling *)
                     else begin
                         let inner_type = r_unwind tyargs r_dual in
                         let o = o#add_seen_recapp r_unique_name in
                         fst (o#typ inner_type)
                       end
             in
             (o, tp)
          | _ -> super#typ tp

      end
    in
    let (o,_) = (o#typ tp)in
    let operations = o#operations in
    let implicit_shared_var = o#implicit_shared_var in
    (* if implicit shared var exists, we replace its id with $eff *)
    let operations = match implicit_shared_var with
      | None -> operations
      | Some vid -> RowVarMap.add_raw (-1)
                      (RowVarMap.find_raw vid operations) operations
    in
    operations

(** Gather information about which operations are used with which row
    variables.
    Precondition: cleanup_effects ran on this type *)
let gather_operations (tycon_env : simple_tycon_env) allow_fresh dt =
  let o =
    object (self)
      inherit SugarTraversals.fold as super

      val operations = RowVarMap.empty
      method operations = operations
      method with_operations operations = {< operations >}

      val hidden_operations : stringset stringmap = StringMap.empty
      method hidden_operations = hidden_operations
      method add_hidden_op alias_name label =
        let hidden_operations = StringMap.update alias_name
                                  (function
                                   | None -> Some (StringSet.singleton label)
                                   | Some lset -> Some (StringSet.add label lset))
                                  hidden_operations
        in
        {< hidden_operations >}

      method replace quantifier map =
        let ubq = RowVarMap.update_by_quantifier in
        let fobq = RowVarMap.find_opt_by_quantifier in
        {<operations = ubq quantifier (fun _ -> fobq quantifier map) operations>}

      method quantified action (qs : SugarQuantifier.t list) =
        let mask_operations =
          List.fold_left
            (fun o sq -> RowVarMap.remove_by_quantifier sq o)
            operations qs
        in
        let o = action {<operations = mask_operations>} in
        List.fold_left (fun o sq -> o#replace sq operations) o qs

      method add (var : SugarTypeVar.t) op =
        if TypeUtils.is_builtin_effect op || not (RowVarMap.is_relevant var)
        then self
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
        let { pos; node = t } = dt in
        match t with
        | Function (a, e, t)
        | Lolli (a, e, t) ->
            let o = self#list (fun o -> o#datatype) a in
            let o = o#effect_row e in
            let o = o#datatype t in
            o
        | TypeApplication (name, ts) -> (
            let tycon_info = SEnv.find_opt name tycon_env in
            let rec go o =
              (* We don't know if the arities match up yet, so we handle
                    mismatches, assuming spare rows are effects. *)
              function
              | _, [] -> o
              | (PrimaryKind.Row, (_, Restriction.Effect)) :: qs, Row t :: ts ->
                  go (o#effect_row t) (qs, ts)
              | (([] as qs) | _ :: qs), t :: ts -> go (o#type_arg t) (qs, ts)
            in
            match tycon_info with
            | Some (params, _has_implict_eff, internal_type) ->
               let self = go self (params, ts) in
               let ops = match internal_type with
                 | None -> RowVarMap.empty
                 | Some internal_type ->
                    gather_operation_of_type internal_type
               in
               let operations =
                 RowVarMap.fold
                   (fun vid sset acc ->
                     RowVarMap.update vid
                       (function
                        | None -> Some sset
                        | Some opset -> Some (StringSet.union opset sset))
                       acc)
                   ops self#operations
               in
               let self = match RowVarMap.find_raw_opt (-1) ops with
                 | None -> self
                 | Some hide_ops ->
                    StringSet.fold
                      (fun label acc ->
                        acc#add_hidden_op name label)
                      hide_ops self
               in
               self#with_operations operations
            | None -> raise (Errors.unbound_tycon pos name) )
        | Mu (v, t) ->
            let mtv = SugarTypeVar.get_resolved_type_exn v in
            let var, (_, sk) = unpack_var_id (Unionfind.find mtv) in
            let q : Quantifier.t = (var, (pk_type, sk)) in
            let sq = SugarQuantifier.mk_resolved q in
            self#quantified (fun o -> o#datatype t) [ sq ]
        | Forall (qs, t) -> self#quantified (fun o -> o#datatype t) qs
        | _ -> super#datatype dt

      method! row_var =
        let open Datatype in
        function
        | EffectApplication (name, ts) ->
            let tycon_info = SEnv.find_opt name tycon_env in
            let rec go o =
              (* We don't know if the arities match up yet, so we handle
                    mismatches, assuming spare rows are effects. *)
              function
              | _, [] -> o
              | (PrimaryKind.Row, (_, Restriction.Effect)) :: qs, Row t :: ts ->
                  go (o#effect_row t) (qs, ts)
              | (([] as qs) | _ :: qs), t :: ts -> go (o#type_arg t) (qs, ts)
            in
            begin match tycon_info with
            | Some (params, _has_implict_eff, internal_type) ->
               let self = go self (params, ts) in
               let ops = match internal_type with
                 | None -> RowVarMap.empty
                 | Some internal_type ->
                    gather_operation_of_type internal_type
               in
               let operations =
                 RowVarMap.fold
                   (fun vid sset acc ->
                     RowVarMap.update vid
                       (function
                        | None -> Some sset
                        | Some opset -> Some (StringSet.union opset sset))
                       acc)
                   ops self#operations
               in
               let self = match RowVarMap.find_raw_opt (-1) ops with
                 | None -> self
                 | Some hide_ops ->
                    StringSet.fold
                      (fun label acc ->
                        acc#add_hidden_op name label)
                      hide_ops self
               in
               self#with_operations operations
            | None -> raise (Errors.unbound_tycon SourceCode.Position.dummy name)
            end
        | Closed
        | Open _ ->
            self
        | Recursive (v, r) ->
            let mtv = SugarTypeVar.get_resolved_type_exn v in
            let var, (_, sk) = unpack_var_id (Unionfind.find mtv) in
            let q : Quantifier.t = (var, (pk_row, sk)) in
            let sq = SugarQuantifier.mk_resolved q in
            self#quantified (fun o -> o#row r) [ sq ]

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
    let o = o#datatype dt in
    (o#operations
     |> RowVarMap.map (fun v ->
            StringSet.fold
              (fun op m ->
                let point =
                  lazy
                    (let var = Types.fresh_raw_variable () in
                     Unionfind.fresh (Types.Var (var, (PrimaryKind.Presence, default_effect_subkind), `Rigid)))
                in
                StringMap.add op point m)
              v StringMap.empty),
     o#hidden_operations)
  else (RowVarMap.empty, StringMap.empty)

let preprocess_type (dt : Datatype.with_pos) tycon_env allow_fresh shared_effect
  =
  let dt = cleanup_effects tycon_env dt in
  let row_operations = gather_operations tycon_env allow_fresh dt in
  let shared_effect =
    match shared_effect with
    | None when allow_fresh && has_effect_sugar () ->
        let point =
          lazy
            (let var = Types.fresh_raw_variable () in
             Unionfind.fresh
               (Types.Var (var, (PrimaryKind.Row, default_effect_subkind), `Rigid)))
        in
        Some point
    | _ ->
        (* If the shared_effect variable was already created, for instance
           by the typename logic, we don't have to create one *)
        shared_effect
  in
  (dt, row_operations, shared_effect)

class main_traversal simple_tycon_env =
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

    val tycon_env : simple_tycon_env = simple_tycon_env

    (** Map of effect variables to all mentioned operations, and their
        corresponding effect variables. *)
    val row_operations : Types.meta_presence_var Lazy.t StringMap.t RowVarMap.t
        =
        RowVarMap.empty

    val hidden_operations : stringset stringmap = StringMap.empty

    method set_inside_type inside_type = {<inside_type>}

    method set_tycon_env tycon_env = {<tycon_env>}

    method set_allow_implictly_bound_vars allow_implictly_bound_vars =
      {<allow_implictly_bound_vars>}

    method set_shared_effect shared_effect = {<shared_effect>}

    method disallow_shared_effect = {<shared_effect = None>}

    method! phrasenode =
      let open Sugartypes in
      function
      | Block (_bs, _p) as b ->
          (* aliases bound in `bs'
             should not escape the scope of the block *)
          let o, b = super#phrasenode b in
          (o#set_tycon_env tycon_env, b)
      | p -> super#phrasenode p

    method! datatypenode =
      let open Datatype in
      let do_fun a e r =
        let o, a = o#list (fun o -> o#datatype) a in
        let o, e = o#effect_row e in
        let o, r = o#datatype r in
        (o, a, e, r)
      in
      function
      | Function (a, e, r) ->
          let o, a, e, r = do_fun a e r in
          (o, Function (a, e, r))
      | Lolli (a, e, r) ->
          let o, a, e, r = do_fun a e r in
          (o, Lolli (a, e, r))
      | TypeVar stv ->
          if not (SugarTypeVar.is_resolved stv) then
            raise
              (internal_error
                 ( "All type variables (of kind Type) must have been"
                 ^ " resolved at this point" ));
          (o, TypeVar stv)
      | TypeApplication (tycon, ts) -> (
          (* We need to process the arguments for a type constructor.
             For rows as arguments, we need the expected kinding info to tell
             us whether to process as normal row or effect row.
             Finally, for types expecting a new implicit effect var,
             we add it as the final argument.
             We have to report certain cases of arity/kinding mismatch errors
             here, but we are not exhaustively catching all problems with
             type applications. This must be done in later passes. *)
          let pos = SourceCode.Position.dummy in
          match SEnv.find_opt tycon tycon_env with
          | None -> raise (Errors.unbound_tycon pos tycon)
          | Some (params, _has_implicit_eff, _internal_type) ->
              let qn = List.length params in
              let tn = List.length ts in
              let arity_err () =
                Errors.TypeApplicationArityMismatch
                  { pos; name = tycon; expected = qn; provided = tn }
              in
              let module PK = PrimaryKind in
              let process_type_arg i : Kind.t * type_arg -> Datatype.type_arg =
                function
                | (PK.Row, (_, Restriction.Effect)), Row r ->
                    let _o, erow = o#effect_row ~in_alias:(Some tycon) r in
                    Row erow
                | (PK.Row, _), Row r ->
                    let _o, row = o#row r in
                    Row row
                | k, Row _ ->
                    (* Here we don't know how to transform the row.
                       Transforming it the wrong way may lead to type errors
                       distracting the user from the actual error: the kind missmatch.
                       Hence, we must report a proper error here. *)
                    raise
                      (Errors.type_application_kind_mismatch pos tycon i
                        (PrimaryKind.to_string (fst k))
                        (PrimaryKind.to_string pk_row))
                | _, ta -> snd (o#type_arg ta)
              in
              let rec match_args_to_params index = function
                | k :: ks, ta :: tas ->
                    process_type_arg index (k, ta)
                    :: match_args_to_params (index + 1) (ks, tas)
                | _k :: _ks, [] ->
                    (* this *may* be an arity mismatch, but not handling it
                       here doesn't cause trouble. *)
                    []
                | [], _ta :: _tas ->
                    (* As above, must report proper error here to avoid confusion *)
                    raise (arity_err ())
                | [], [] -> []
              in
              let ts = match_args_to_params 1 (params, ts) in

              let may_procide_shared_effect =
                match ListUtils.last_opt params with
                | Some (PrimaryKind.Row, (_, Restriction.Effect)) ->
                    has_effect_sugar ()
                | _ -> false
              in

              (* now insert implict effect as argument if necessary*)
              let ts =
                match (may_procide_shared_effect, shared_effect, qn - tn) with
                | _, _, 0 ->
                    (* already fully applied, do nothing *)
                    ts
                | false, _, 1
                | _, None, 1 ->
                    (* One argument missing, but we can't provide shared effect *)
                    raise (arity_err ())
                | _, _, n when n > 1 || n < 0 ->
                    (* either too many args or more than one missing*)
                    raise (arity_err ())
                | true, Some lazy_eff, 1 ->
                    (* insert shared effect as final argument *)

                    (* Looking for this gives us the operations associcated with
                       the $eff var. The kind and freedom info are ignored for the lookup *)
                    let eff_sugar_var =
                      SugarTypeVar.mk_unresolved shared_effect_var_name ~is_eff:true None
                        `Rigid
                    in

                    let fields =
                      match RowVarMap.find_opt eff_sugar_var row_operations with
                      | None -> []
                      | Some ops ->
                         let ops_to_hide = match StringMap.find_opt tycon hidden_operations with
                           | None -> StringSet.empty
                           | Some hidden -> hidden
                         in
                          StringMap.fold
                            (fun op p fields ->
                              if StringSet.mem op ops_to_hide
                              then fields
                              else begin
                                  let mpv : Types.meta_presence_var =
                                    Lazy.force p
                                  in
                                  let fieldspec =
                                    Datatype.Var
                                      (SugarTypeVar.mk_resolved_presence mpv)
                                  in
                                  if not allow_implictly_bound_vars then
                                    raise (cannot_insert_presence_var2 pos op);
                                  (op, fieldspec) :: fields
                                end)
                            ops []
                    in
                    let row_var =
                      Datatype.Open
                        (Lazy.force lazy_eff |> SugarTypeVar.mk_resolved_row)
                    in
                    let eff : Datatype.row = (fields, row_var) in
                    ts @ [ Row eff ]
                | _, _, _ -> assert false
                (* unreachable, but compiler can't tell *)
              in
              (o, TypeApplication (tycon, ts)) )
      | Forall (qs, t) ->
          let o = o#set_allow_implictly_bound_vars false in
          let o = o#disallow_shared_effect in
          let o, t = o#datatype t in
          let o = o#set_allow_implictly_bound_vars allow_implictly_bound_vars in
          let o = o#set_shared_effect shared_effect in
          (o, Forall (qs, t))
      | t -> super#datatypenode t

    method! row (fields, rv) =
      let dpos = SourceCode.Position.dummy in
      let module D = Datatype in
      let o, rv =
        match rv with
        | D.EffectApplication _ -> super#row_var rv   (* maybe we should do as for TypeApplication and not just visit the node *)
        | D.Closed -> (o, rv)
        | D.Open stv
          when (not (SugarTypeVar.is_resolved stv))
               && SugarTypeVar.get_unresolved_name_exn stv
                  = shared_effect_var_name -> (
            match shared_effect with
            | None -> raise (shared_effect_forbidden_here dpos)
            | Some s ->
                (o, D.Open (Lazy.force s |> SugarTypeVar.mk_resolved_row)) )
        | D.Open stv
          when (not (SugarTypeVar.is_resolved stv))
               && DesugarTypeVariables.is_anonymous stv ->
            if not allow_implictly_bound_vars then
              raise (DesugarTypeVariables.free_type_variable dpos);

            let _name, (is_eff, sk), freedom = SugarTypeVar.get_unresolved_exn stv in
            let mtv = make_anon_point ~is_eff:is_eff (PrimaryKind.Row) sk freedom in
            let rtv = SugarTypeVar.mk_resolved_row mtv in
            (o, D.Open rtv)
        | D.Open srv when not (SugarTypeVar.is_resolved srv) ->
            raise
              (internal_error
                 ( "Encountered non-anonymous, unresolved effect "
                 ^ " variable. All such variables must have been resolved by "
                 ^ " earlier transformations." ))
        | D.Open _ -> (o, rv)
        | D.Recursive (stv, r) ->
            let o, r = o#row r in
            (o, D.Recursive (stv, r))
      in
      let o, fields =
        o#list
          (fun o (name, fs) ->
            let o, fs = o#fieldspec fs in
            (o, (name, fs)))
          fields
      in
      (o, (fields, rv))

    method effect_row ?(in_alias=None) ((fields, rv) : Datatype.row) =
      let dpos = SourceCode.Position.dummy in
      let fields =
        match rv with
        | Datatype.Open stv when RowVarMap.is_relevant stv -> (
            match RowVarMap.find_opt stv row_operations with
            | Some ops ->
               let ops_to_hide = match in_alias with
                 | None -> StringSet.empty
                 | Some name ->
                    (match StringMap.find_opt name hidden_operations with
                     | None -> StringSet.empty
                     | Some hidden -> hidden)
               in
                let ops_to_add =
                  List.fold_left
                    (fun ops (op, _) -> StringMap.remove op ops)
                    ops fields in
                let add_op op pres_var fields =
                  if not allow_implictly_bound_vars then
                    (* Alternatively, we could just decide not to touch the row and let the type checker
                       complain about the incompatible rows? *)
                    raise (cannot_insert_presence_var dpos op);
                  if StringSet.mem op ops_to_hide
                  then fields
                  else let rpv =
                         SugarTypeVar.mk_resolved_presence (Lazy.force pres_var) in
                       (op, Datatype.Var rpv) :: fields in
                StringMap.fold add_op ops_to_add fields
            | None ->
               fields )
        | _ -> fields in
      (* We need to perform the actions above prior to calling o#row.
         Otherwise, we resolve $eff already, and the lookup in row_operations
         yields no info *)
      let o, (fields, rv) = o#row (fields, rv) in
      (o, (fields, rv))

    method! aliasbody =
    let open Sugartypes.Datatype in
    let module WP = SourceCode.WithPos in
    function
      | Typename _ as t -> super#aliasbody t
      | Effectname (r, _) ->    (* hack to cleanup the row and desugar properly *)
        let wp  = cleanup_effects tycon_env (WP.dummy (Effect r)) in
        match WP.node wp with
          | Effect r -> (o, Effectname(r, None))
          | _ -> assert false

    method! bindingnode =
      function
      | Val (_pat, (_qs, _body), _loc, signature) as b ->
          let implicits_allowed =
            DesugarTypeVariables.sig_allows_implicitly_bound_vars signature
          in
          let o = o#set_allow_implictly_bound_vars implicits_allowed in

          let o, b = o#super_bindingnode b in

          let o = o#set_allow_implictly_bound_vars allow_implictly_bound_vars in
          (o, b)
      | Aliases ts ->
          let open SourceCode.WithPos in
          let tycon_env, tycons =
            List.fold_left
              (fun (alias_env, tycons) { node = t, args, _; _ } ->
                let params =
                  List.map
                    (SugarQuantifier.get_resolved_exn ->- Quantifier.to_kind)
                    args
                in
                (* initially pretend that no type needs an implict parameter *)
                (* TODO                                vvvv ??? *)
                let env' = SEnv.bind t (params, false, None) alias_env in
                let tycons' = StringSet.add t tycons in
                (env', tycons'))
              (tycon_env, StringSet.empty)
              ts
          in

          (* First determine which types require an implicit effect variable. *)
          let implicits, dep_graph =
            List.fold_left
              (fun (implicits, dep_graph) { node = t, _, b; pos ;_ } ->
                match b with
                  | Typename (d,_) ->
                    let d = cleanup_effects tycon_env d in
                    let eff = gather_mutual_info tycon_env d in
                    let has_imp = eff#has_implicit in
                    let implicits = StringMap.add t has_imp implicits in
                    let used_mutuals = StringSet.inter eff#used_types tycons in
                    let dep_graph =
                        StringMap.add t (StringSet.elements used_mutuals) dep_graph
                    in
                    (implicits, dep_graph)
                  | Effectname (r,_) -> (* is this the right thing to do ? *)
                    let d = cleanup_effects tycon_env (SourceCode.WithPos.make ~pos (Datatype.Effect r)) in
                    let eff = gather_mutual_info tycon_env d in
                    let has_imp = eff#has_implicit in
                    let implicits = StringMap.add t has_imp implicits in
                    let used_mutuals = StringSet.inter eff#used_types tycons in
                    let dep_graph =
                        StringMap.add t (StringSet.elements used_mutuals) dep_graph
                    in
                    (implicits, dep_graph))
              (StringMap.empty, StringMap.empty)
              ts
          in
          (* We also gather a dependency graph of our mutually recursive types. Group this into SCCs
             and toposort it. We can then trivially propagate the implicit effect requirement - if
             anyone in our component has an implicit effect, or depends on a type which does, then we
             must too! *)
          let has_implicit implicits name =
            StringMap.find name implicits
            || List.exists
                 (flip StringMap.find implicits)
                 (StringMap.find name dep_graph)
          in
          let sorted_graph =
            Graph.topo_sort_sccs (StringMap.bindings dep_graph)
          in
          let implicits =
            List.fold_left
              (fun implicits scc ->
                let scc_imp = List.exists (has_implicit implicits) scc in
                List.fold_left
                  (fun acc x -> StringMap.add x scc_imp acc)
                  implicits scc)
              implicits sorted_graph
          in
          (* Now patch up the types to include this effect variable. *)
          let patch_type_param_list ((tycon_env : simple_tycon_env), shared_var_env, ts)
              ({ node = t, args, b; pos } as tn) =
            if StringMap.find t implicits then
              let var = Types.fresh_raw_variable () in
              let q = (var, (PrimaryKind.Row, (default_effect_lin, res_effect))) in
              (* Add the new quantifier to the argument list and rebind. *)
              (* let qs = List.map (snd ->- OptionUtils.val_of) args @ [q] in *)
              let args = args @ [ SugarQuantifier.mk_resolved q ] in
              let env_args =
                List.map
                  (SugarQuantifier.get_resolved_exn ->- Quantifier.to_kind)
                  args
              in
              (* TODO maybe this is already bound, take the type from inside
                                                           vvvv *)
              let tycon_env = SEnv.bind t (env_args, true, None) tycon_env in
              let shared_effect_var : Types.meta_row_var Lazy.t =
                lazy
                  (Unionfind.fresh (Types.Var (var, (PrimaryKind.Row, (default_effect_lin, res_effect)), `Rigid)))
              in
              let shared_var_env =
                StringMap.add t (Some shared_effect_var) shared_var_env
              in
              let b' = match b with
                | Typename     (d,_) -> Typename     (d, None)
                | Effectname   (r,_) -> Effectname   (r, None)
              in
              ( tycon_env,
                shared_var_env,
                SourceCode.WithPos.make ~pos (t, args, b') :: ts )
            else
              (* Note that we initially set the has-implict flag to
                 false, so there is nothing to do here *)
              let shared_var_env = StringMap.add t None shared_var_env in
              (tycon_env, shared_var_env, tn :: ts)
          in
          let tycon_env, shared_eff_vars, ts =
            List.fold_left patch_type_param_list
              (tycon_env, StringMap.empty, [])
              ts
          in

          let traverse_body { node = name, args, dt; pos } =
            if inside_type then
              raise
                (internal_error
                   "a type definition should never be a child-node of a type");
            let shared_effect = StringMap.find name shared_eff_vars in
            let o =
              {<tycon_env; shared_effect; allow_implictly_bound_vars = false>}
            in

            (* TODO: no info to flow back out? *)
            let _o, dt' = o#aliasbody dt in

            SourceCode.WithPos.make ~pos (name, args, dt')
          in

          let ts' = List.map traverse_body ts in
          ({<tycon_env>}, Aliases ts')
      | b -> super#bindingnode b

    method super_datatype = super#datatype

    method! datatype dt =
      let pos = SourceCode.WithPos.pos dt in
      let dt, o =
        if not inside_type then begin
            let dt, (row_operations, hidden_operations), shared_effect =
              preprocess_type dt tycon_env allow_implictly_bound_vars
                shared_effect
            in
            (dt, {<row_operations; shared_effect; hidden_operations>})
          end
        else (dt, o)
      in
      let o = o#set_inside_type true in
      let o, dt =
        Errors.rethrow_errors_if_better_position pos o#super_datatype dt
      in
      let o = o#set_inside_type inside_type in
      (o, dt)

    method super_function_definition = super#function_definition

    method super_recursive_functionnode = super#recursive_functionnode

    method super_bindingnode = super#bindingnode

    method! function_definition
        : function_definition -> 'self * function_definition =
      fun fun_def ->
        let implicits_allowed =
          DesugarTypeVariables.sig_allows_implicitly_bound_vars
            fun_def.fun_signature
        in
        let o = o#set_allow_implictly_bound_vars implicits_allowed in

        let o, fun_def = o#super_function_definition fun_def in

        (* restore previous state *)
        let o = o#set_allow_implictly_bound_vars allow_implictly_bound_vars in

        (o, fun_def)

    method! recursive_functionnode
        : recursive_functionnode -> 'self * recursive_functionnode =
      fun rec_def ->
        let implicits_allowed =
          DesugarTypeVariables.sig_allows_implicitly_bound_vars
            rec_def.rec_signature
        in
        let o = o#set_allow_implictly_bound_vars implicits_allowed in

        let o, rec_def = o#super_recursive_functionnode rec_def in

        (* restore previous state *)
        let o = o#set_allow_implictly_bound_vars allow_implictly_bound_vars in

        (o, rec_def)
  end

let program (tycon_env : Types.tycon_environment) p =
  let s_env = simplify_tycon_env tycon_env in
  let v = new main_traversal s_env in
  snd (v#program p)

let sentence (tycon_env : Types.tycon_environment) =
  let s_env = simplify_tycon_env tycon_env in
  function
  | Definitions bs ->
      let v = new main_traversal s_env in
      let _, bs = v#list (fun o b -> o#binding b) bs in
      Definitions bs
  | Expression p ->
      let v = new main_traversal s_env in
      let _o, p = v#phrase p in
      Expression p
  | Directive d -> Directive d

let standalone_signature (tycon_env : Types.tycon_environment) t =
  let s_env = simplify_tycon_env tycon_env in
  let v = new main_traversal s_env in
  snd (v#datatype t)

module Untyped = struct
  open Transform.Untyped

  let name = "effects"

  let program state program' =
    let open Types in
    let tyenv = Context.typing_environment (context state) in
    let program' = program tyenv.tycon_env program' in
    return state program'

  let sentence state sentence' =
    let open Types in
    let tyenv = Context.typing_environment (context state) in
    let sentence'' = sentence tyenv.tycon_env sentence' in
    return state sentence''
end
