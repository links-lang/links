open CommonTypes
open Utility
open Types
open Typevarcheck

(* debug flags *)
let show_unification
  = Settings.(flag "show_unification"
              |> convert parse_bool
              |> sync)

let show_row_unification
  = Settings.(flag "show_row_unification"
              |> convert parse_bool
              |> sync)

let show_recursion = Basicsettings.Types.show_recursion

(*
  what kind of recursive types to allow
  "all"      - allow all recursive types
  "guarded"  - only allow guarded recursive types
  "positive" - only allow positive recursive types
 *)
type recursive_type_kind =
  | All | Guarded | Positive

let infer_recursive_types
  = let parse_rec_type_kind v =
      Some (match String.lowercase_ascii v with
            | "all" -> All
            | "guarded" -> Guarded
            | "positive" -> Positive
            | _ -> raise (Invalid_argument "accepted values: all, guarded, or positive"))
    in
    let string_of_rec_type_kind = function
      | Some All -> "all"
      | Some Guarded -> "guarded"
      | Some Positive -> "positive"
      | None -> "<none>"
    in
    Settings.(option ~default:(Some Guarded) "infer_recursive_types"
              |> synopsis "Selects which kind of recursive types the inference engine is allowed to infer"
              |> hint "<all|guarded|positive>"
              |> to_string string_of_rec_type_kind
              |> convert parse_rec_type_kind
              |> sync)

type error = [
  `Msg of string
(* TODO: is there a need to support errors other than plain
   messages? *)
| `PresentAbsentClash of string * Types.row * Types.row
]

(* We need to be able to track both mu-bound and mutually-recursive
 * (nominal) IDs. *)
type rec_unifier =
  | RecAppl of rec_appl
  | MuBound of (int * typ)

exception Failure of error

let occurs_check var t =
  match Settings.get infer_recursive_types with (* TODO FIXME: validate the setting during set. *)
    | Some All -> true
    | Some Guarded -> is_guarded var t
    | Some Positive -> not (is_negative var t)
    | None -> raise (Errors.settings_error "user setting infer_recursive_types must be set to 'all', 'guarded' or 'positive'")

let occurs_check_row var row =
  match Settings.get infer_recursive_types with
    | Some All -> true
    | Some Guarded -> is_guarded_row var row
    | Some Positive -> not (is_negative_row var row)
    | None -> raise (Errors.settings_error "user setting infer_recursive_types must be set to 'all', 'guarded' or 'positive'")

let var_is_free_in_type var datatype = TypeVarSet.mem var (free_type_vars datatype)

(* a special kind of structural equality on types that doesn't look
inside points *)
let rec eq_types : (datatype * datatype) -> bool =
  fun (t1, t2) ->
    let rec unalias = function
      | Alias (_, _, x) -> unalias x
      | x            -> x in
    match unalias t1 with
      | Not_typed ->
          begin match unalias t2 with
          | Not_typed -> true
          | _         -> false
          end
      | (Var _ | Recursive _ | Closed) ->
         failwith ("freestanding Var / Recursive / Closed not implemented yet (must be inside Meta)")
      | Alias  _ -> assert false
      | Application (s, ts) ->
          begin match unalias t2 with
          | Application (s', ts') ->
             s = s' && List.for_all2 (Utility.curry eq_type_args) ts ts'
          | _ -> false
          end
      | RecursiveApplication a1 ->
          begin match unalias t2 with
          | RecursiveApplication a2 ->
             a1.r_unique_name = a2.r_unique_name &&
               List.for_all2 (Utility.curry eq_type_args) a1.r_args a2.r_args
          | _ -> false
          end
      | Meta lpoint ->
         begin match unalias t2 with
         | Meta rpoint -> Unionfind.equivalent lpoint rpoint
         | _           -> false
         end
      (* Type *)
      | Primitive x ->
          begin match unalias t2 with
          | Primitive y -> x = y
          | _            -> false
          end
      | Function (lfrom, lm, lto) ->
          begin match unalias t2 with
          | Function (rfrom, rm, rto) -> eq_types (lfrom, rfrom)
                                         && eq_types (lto,   rto)
                                         && eq_rows  (lm,    rm)
          | _                          -> false
          end
      | Lolli (lfrom, lm, lto) ->
          begin match unalias t2 with
          | Lolli (rfrom, rm, rto) -> eq_types (lfrom, rfrom)
                                      && eq_types (lto,   rto)
                                      && eq_rows  (lm,    rm)
          | _                       -> false
          end
      | Record l ->
         begin match unalias t2 with
         | Record r -> eq_rows (l, r)
         | _        -> false
         end
      | Variant l ->
         begin match unalias t2 with
         | Variant r -> eq_rows (l, r)
         | _         -> false
         end
      | Table _  -> assert false
      | Lens l ->
         begin match unalias t2 with
         | Lens l2 -> Lens.Type.equal l l2
         | _ -> false
         end
      | ForAll (qs, t) ->
          begin match unalias t2 with
          | ForAll (qs', t') ->
             List.for_all2 Quantifier.eq qs qs' &&
               eq_types (t, t')
          | _ -> false
          end
      (* Effect *)
      | Effect l ->
         begin match unalias t2 with
         | Effect r -> eq_rows (l, r)
         | _        -> false
         end
      | Operation (lfrom, lto, llin) ->
          begin match unalias t2 with
          | Operation (rfrom, rto, rlin) -> eq_types (lfrom, rfrom)
                                            && eq_types (lto,   rto)
                                            && llin = rlin
          | _                            -> false
          end
      (* Row *)
      | Row (lfield_env, lrow_var, ldual) ->
         begin match unalias t2 with
         | Row (rfield_env, rrow_var, rdual) ->
            eq_field_envs (lfield_env, rfield_env) && eq_row_vars (lrow_var, rrow_var) && ldual=rdual
         | _ -> assert false
         end
      (* Presence *)
      | Present l ->
         begin match unalias t2 with
         | Present r -> eq_types (l, r)
         | _ -> false
         end
      | Absent ->
         begin match unalias t2 with
         | Absent -> true
         | _ -> false
         end
      (* Session *)
      | Input (lt, _) ->
         begin match unalias t2 with
         | Input (rt, _) -> eq_types (lt, rt)
         | _             -> false
         end
      | Output (lt, _) ->
         begin match unalias t2 with
         | Output (rt, _) -> eq_types (lt, rt)
         | _              -> false
         end
      | Select l ->
         begin match unalias t2 with
         | Select r -> eq_rows (l, r)
         | _ -> false
         end
      | Choice l ->
         begin match unalias t2 with
         | Choice r -> eq_rows (l, r)
         | _ -> false
         end
      (* TODO: do we need to actually perform any dualisation here? *)
      | Dual l ->
        begin match unalias t2 with
        | Dual r -> eq_types (l, r)
        | _ -> false
        end
      | End ->
         begin match unalias t2 with
         | End -> true
         | _ -> false
         end

and eq_rows : (row * row) -> bool = fun (l, r) -> eq_types (l, r)

and eq_presence = fun (l, r) -> eq_types (l, r)

and eq_field_envs (lfield_env, rfield_env) =
  let eq_specs lf rf = eq_presence (lf, rf) in
    StringMap.equal eq_specs lfield_env rfield_env
and eq_row_vars (lpoint, rpoint) =
  (* QUESTION:
     Do we need to deal with closed rows specially?
  *)
  match Unionfind.find lpoint, Unionfind.find rpoint with
    | Closed, Closed -> true
    | Var (var, _, _), Var (var', _, _)
    | Recursive (var, _, _), Recursive (var', _, _) -> var=var'
    | _, _ -> Unionfind.equivalent lpoint rpoint
and eq_type_args =
  let open PrimaryKind in
  fun ((lpk, lt), (rpk, rt)) ->
  match lpk, rpk with
  | Type, Type         -> eq_types (lt, rt)
  | Row, Row           -> eq_types (lt, rt)
  | Presence, Presence -> eq_presence (lt, rt)
  | _, _               -> false

(*
  unification environment:
    for stopping cycles during unification
*)
type unify_type_env = (datatype list) RecIdMap.t
type unify_row_env = (row list) IntMap.t

(* map left-hand quantifiers to their renamings
and keep track of right-hand quantifiers
 *)
type quantifier_env = int IntMap.t * IntSet.t

let compatible_quantifiers (lvar, rvar) (lenv, _) =
  match IntMap.lookup lvar lenv with
  | Some rvar' -> rvar' = rvar
  | None -> false

let left_quantifier var (lenv, _) =
  IntMap.mem var lenv

let right_quantifier var (_, rs) =
  IntSet.mem var rs

type unify_env =
  { tenv: unify_type_env
  ; renv: unify_row_env
  ; qenv: quantifier_env
  }

let check_subkind var (lin, res) typ =
  if Linearity.is_nonlinear lin then
    if Types.Unl.can_type_be typ then
      Types.Unl.make_type typ
    else
      raise (Failure (`Msg ("Cannot unify the unlimited type variable " ^ string_of_int var ^
                              " with the linear type " ^ string_of_datatype typ)));

  match Types.get_restriction_constraint res with
  | None -> ()
  | Some const ->
     let module M = (val const) in
     if M.can_type_be typ then
       M.make_type typ
     else
       let message = Printf.sprintf "Cannot unify the %s type variable %d with the non-%s type %s."
                       (Restriction.to_string res) var (Restriction.to_string res) (string_of_datatype typ)
       in raise (Failure (`Msg message))

let rec unify' : unify_env -> (datatype * datatype) -> unit =
  let counter = ref 0 in
  fun rec_env ->
  let rec_types = rec_env.tenv in
  let qenv = rec_env.qenv in

  let key_and_body unifier =
    match unifier with
    | MuBound (i, typ) -> (MuBoundId i, typ)
    | RecAppl { r_unique_name; r_dual; r_args; r_unwind ; _ } ->
       (NominalId r_unique_name, r_unwind r_args r_dual) in

  let is_unguarded_recursive t =
    let rec is_unguarded rec_types t =
      match t with
      | Meta point ->
         begin
           match (Unionfind.find point) with
           | Recursive (var, _kind, _) when IntSet.mem var rec_types -> true
           | Recursive (var, _kind, body) -> is_unguarded (IntSet.add var rec_types) body
           | t when is_type_body t -> is_unguarded rec_types t
           | _ -> false
         end
      |  _ -> false
    in
    is_unguarded IntSet.empty t in

  let unify_rec unifier t =
    let (key, body) = key_and_body unifier in
    let ts =
      if RecIdMap.mem key rec_types then
        RecIdMap.find key rec_types
      else
        [body]
    in
    (* break cycles *)
    if List.exists (fun t' -> eq_types (t, t')) ts then
      ()
    else
      unify' {rec_env with tenv=RecIdMap.add key (t::ts) rec_types} (body, t) in

  (* Unification of two recursive types. *)
  let unify_rec2 unifier1 unifier2 =
    let (lkey, lbody) = key_and_body unifier1 in
    let (rkey, rbody) = key_and_body unifier2 in
    let lts =
      if RecIdMap.mem lkey rec_types then
        RecIdMap.find lkey rec_types
      else
        [lbody] in

    let rts =
      if RecIdMap.mem rkey rec_types then
        RecIdMap.find rkey rec_types
      else
        [rbody]
    in
    (* break cycles *)
    if (List.exists (fun t -> eq_types (t, rbody)) lts
        || List.exists (fun t -> eq_types (t, lbody)) rts) then
      ()
    else
      unify' {rec_env with
          tenv=(RecIdMap.add lkey (rbody::lts)
                ->- RecIdMap.add rkey (lbody::rts)) rec_types} (lbody, rbody) in

  (* introduce a recursive type
     give an error if it is non-well-founded and
     non-well-founded type inference is switched off

     preconditions:
     - Unionfind.find point = t
     - var is free in t
   *)
  let rec_intro kind point (var, t) =
    if occurs_check var t then
      Unionfind.change point (Recursive (var, kind, t))
    else
      raise (Failure (`Msg ("Cannot unify type variable "^string_of_int var^" with datatype "^string_of_datatype t^
                              " because "^
                                match Settings.get infer_recursive_types with
                                | Some Guarded -> "the type variable occurs unguarded inside the datatype"
                                | Some Positive -> "the type variable occurs in a negative position inside the datatype"
                                | _ -> assert false))) in

  let ignore_empty_quantifiers =
    function
    | ForAll ([], t) -> t
    | t -> t in

  (* make sure the contents of a point is concrete *)
  let concrete_point point =
    match Unionfind.find point with
    | Meta point' ->
       Unionfind.union point point'
    | _ -> () in

  fun (t1, t2) ->

  let t1 = ignore_empty_quantifiers t1 in
  let t2 = ignore_empty_quantifiers t2 in

  let ut = unify' rec_env in
  let ur = unify_rows' rec_env in
  let utmp (t1, t2) =
      if t1 <> t2 then
        let msg =
            Printf.sprintf "Temporal table specifications %s and %s do not match."
              (Temporality.show t1)
              (Temporality.show t2)
        in
        raise (Failure (`Msg msg))
  in
  counter := !counter+1;
  let counter' = "(" ^ string_of_int !counter ^ ")" in
  Debug.if_set (show_unification) (fun () -> "Unifying "^string_of_datatype t1^" with "^string_of_datatype t2 ^ counter');
  begin
    match (t1, t2) with
    (* Unspecified kind *)
    | Not_typed, _ | _, Not_typed ->
       raise (Errors.internal_error ~filename:"unify.ml" ~message:"`Not_typed' passed to `unify'")
    | (Var _ | Recursive _), _ | _, (Var _ | Recursive _) ->
       failwith ("freestanding Var / Recursive not implemented yet (must be inside Meta)")
    (* it's important that Meta is dealt with before Alias *)
    | Meta lpoint, Meta rpoint ->
       if Unionfind.equivalent lpoint rpoint then
         ()
       else
         begin
           concrete_point lpoint;
           concrete_point rpoint;

           match (Unionfind.find lpoint, Unionfind.find rpoint) with
           | Var (lvar, lkind, `Rigid), Var (rvar, rkind, `Rigid) when
                  lkind=rkind && compatible_quantifiers (lvar, rvar) qenv ->
              ()
           | Var (lvar, _, `Rigid), Var (_, _, `Flexible) when left_quantifier lvar qenv ->
              raise (Failure (`Msg ("Escaping quantifier " ^ string_of_int lvar)))
           | Var (_, _, `Flexible), Var (rvar, _, `Rigid) when right_quantifier rvar qenv ->
              raise (Failure (`Msg ("Escaping quantifier " ^ string_of_int rvar)))
           | Var (l, _, `Rigid), Var (r, _, `Rigid) ->
              if l <> r then
                raise (Failure (`Msg ("Rigid type variables "^ string_of_int l ^" and "^ string_of_int r ^" do not match")))
              else
                (* presumably this should always be a no-op *)
                Unionfind.union lpoint rpoint
           | Var (lvar, (lprimary_kind, (llin,lrest)), `Flexible), Var (rvar, (rprimary_kind, (rlin,rrest)), `Flexible) ->
              assert (lprimary_kind = PrimaryKind.Type && rprimary_kind = PrimaryKind.Type);
              Unionfind.union lpoint rpoint;
              begin
                let lin =
                  match llin, rlin with
                  | Linearity.Unl, _ | _, Linearity.Unl -> Linearity.Unl
                  | _       -> llin in
                let rest =
                  match Restriction.min lrest rrest with
                  | Some rest -> rest
                  | None ->
                     let message =
                       Printf.sprintf "Cannot unify %s type variable %d with %s type variable %d"
                         (Restriction.to_string lrest) lvar (Restriction.to_string rrest) rvar
                     in raise (Failure (`Msg message))
                in
                Unionfind.change lpoint (Var (lvar, (lprimary_kind, (lin, rest)), `Flexible))
              end
           | Var (var, ((primary_kind, (lin, rest)) as kind), `Flexible), _ ->
              assert (primary_kind = PrimaryKind.Type);
              let tidy =
                if var_is_free_in_type var t2 then
                  begin
                    Debug.if_set (show_recursion) (fun () -> "rec intro1 (" ^ (string_of_int var) ^ ")");
                    if Restriction.is_base rest then
                      raise (Failure (`Msg ("Cannot infer a recursive type for the base type variable "^ string_of_int var ^
                                              " with the body "^ string_of_datatype t2)));
                    rec_intro kind rpoint (var, Types.concrete_type t2);
                    true
                  end
                else
                  true in
              (* FIXME: does this really still need to happen if we've just introduced a recursive type? *)
              if tidy then
                begin
                  check_subkind var (lin, rest) t2;
                  Unionfind.union lpoint rpoint
                end
           | _, Var (var, ((_primary_kind, (lin, rest)) as kind), `Flexible) ->
              let tidy =
                if var_is_free_in_type var t1 then
                  begin
                    Debug.if_set (show_recursion) (fun () -> "rec intro2 (" ^ (string_of_int var) ^ ")");
                    if Restriction.is_base rest then
                      raise (Failure (`Msg ("Cannot infer a recursive type for the base type variable "^ string_of_int var ^
                                              " with the body "^ string_of_datatype t1)));
                    rec_intro kind lpoint (var, Types.concrete_type t1);
                    true
                  end
                else
                  true in
              (* FIXME: does this really still need to happen if we've just introduced a recursive type? *)
              if tidy then
                begin
                  check_subkind var (lin, rest) t1;
                  Unionfind.union rpoint lpoint
                end
           | Var (l, _, `Rigid), _ ->
              raise (Failure (`Msg ("Couldn't unify the rigid type variable "^
                                      string_of_int l ^" with the type "^ string_of_datatype (Meta rpoint))))
           | _, Var (r, _, `Rigid) ->
              raise (Failure (`Msg ("Couldn't unify the rigid type variable "^
                                      string_of_int r ^" with the type "^ string_of_datatype (Meta lpoint))))
           | Recursive (lvar, _lkind, t), Recursive (rvar, _rkind, t') ->
              assert (lvar <> rvar);
              Debug.if_set (show_recursion)
                (fun () -> "rec pair (" ^ (string_of_int lvar) ^ "," ^ (string_of_int rvar) ^")");
              begin
                if is_unguarded_recursive (Meta lpoint) then
                  begin
                    if not (is_unguarded_recursive (Meta rpoint)) then
                      raise (Failure (`Msg ("Couldn't unify the unguarded recursive type "^
                                              string_of_datatype (Meta lpoint) ^
                                                " with the guarded recursive type "^ string_of_datatype (Meta rpoint))))
                  end
                else if is_unguarded_recursive (Meta lpoint) then
                  raise (Failure (`Msg ("Couldn't unify the unguarded recursive type "^
                                          string_of_datatype (Meta rpoint) ^
                                            " with the guarded recursive type "^ string_of_datatype (Meta lpoint))))
                else
                  unify_rec2 (MuBound (lvar, t)) (MuBound (rvar, t'))
              end;
              Unionfind.union lpoint rpoint
           | Recursive (var, _lkind, t'), t ->
              Debug.if_set (show_recursion) (fun () -> "rec left (" ^ (string_of_int var) ^ ")");
              begin
                if is_unguarded_recursive (Meta lpoint) then
                  raise (Failure (`Msg ("Couldn't unify the unguarded recursive type "^
                                          string_of_datatype (Meta lpoint) ^
                                            " with the non-recursive type "^ string_of_datatype (Meta rpoint))))
                else
                  unify_rec (MuBound (var, t')) t
              end;
              (* it is critical that the arguments to Unionfind.union are in
                 this order to ensure that we keep the recursive type rather
                 than its unwinding *)
              (* union keeps the data associated with its second argument *)
              Unionfind.union rpoint lpoint
           | t, Recursive (var, _rkind, t') ->
              Debug.if_set (show_recursion) (fun () -> "rec right (" ^ (string_of_int var) ^ ")");
              begin
                if is_unguarded_recursive (Meta rpoint) then
                  raise (Failure (`Msg ("Couldn't unify the unguarded recursive type "^
                                          string_of_datatype (Meta rpoint) ^
                                            " with the non-recursive type "^ string_of_datatype (Meta lpoint))))
                else
                  unify_rec (MuBound (var, t')) t
              end;
              Unionfind.union lpoint rpoint
           | t, t' ->
              ut (t, t')
         (* Apparently this isn't sound as it can lead to unguarded recursion:

            Unionfind.union lpoint rpoint
          *)
         end
    | Meta point, t | t, Meta point ->
       concrete_point point;
       begin
         match Unionfind.find point with
         | Var (l, _, `Rigid) ->
            begin
              raise (Failure (`Msg ("Couldn't unify the rigid type variable "^ string_of_int l ^
                                      " with the type "^ string_of_datatype t)))
            end
         | Var (var, ((primary_kind, (lin, rest)) as kind), `Flexible) ->
            assert (primary_kind = PrimaryKind.Type);
            if var_is_free_in_type var t then
              begin
                Debug.if_set
                  (show_recursion)
                  (fun () -> "rec intro3 ("^string_of_int var^","^string_of_datatype t^")");
                if Restriction.is_base rest then
                  raise (Failure (`Msg ("Cannot infer a recursive type for the type variable "^ string_of_int var ^
                                          " with the body "^ string_of_datatype t)));
                let point' = Unionfind.fresh t in
                rec_intro kind point' (var, t);
                Unionfind.union point point'
              end
            else
              (Debug.if_set (show_recursion) (fun () -> "non-rec intro (" ^ string_of_int var ^ ")");
               check_subkind var (lin, rest) t;
               Unionfind.change point t)
         | Recursive (var, (primary_kind, _subkind), t') ->
            assert (primary_kind = PrimaryKind.Type);
            Debug.if_set (show_recursion) (fun () -> "rec single (" ^ (string_of_int var) ^ ")");
            begin
              if is_unguarded_recursive (Meta point) then
                raise (Failure (`Msg ("Couldn't unify the unguarded recursive type "^
                                        string_of_datatype (Meta point) ^
                                          " with the non-recursive type "^ string_of_datatype t)))
              else
                unify_rec (MuBound (var, t')) t
            end
         (* It's tempting to try to do this, but it isn't sound as point may
            appear inside t

            Unionfind.change point t; *)
         | t' -> ut (t, t')
       end
    | Alias (_, _, t1), t2 | t1, Alias (_, _, t2) -> ut (t1, t2)
    | Application (l, _), Application (r, _) when l <> r ->
       raise (Failure
                (`Msg ("Cannot unify abstract type '"^string_of_datatype t1^
                         "' with abstract type '"^string_of_datatype t2^"'")))
    | Application (_, ls), Application (_, rs) ->
       List.iter2 (fun lt rt -> unify_type_args' rec_env (lt, rt)) ls rs
    | Primitive t, RecursiveApplication a
    | RecursiveApplication a, Primitive t ->
       raise (Failure
                (`Msg ("Cannot unify primitive type '"^string_of_datatype (Primitive t) ^
                         "' with recursive type '"^ a.r_name ^"'")))
    | RecursiveApplication a1, RecursiveApplication a2 ->
       let (n1, args1) = (a1.r_unique_name, a1.r_args) in
       let (n2, args2) = (a2.r_unique_name, a2.r_args) in
       if n1 = n2 && a1.r_dual = a2.r_dual then
         (* Note that cannot eagerly reject incompatible duality flags
          * due to the possibility of self-dual types such as `End`. *)
         List.iter2 (fun lt rt -> unify_type_args' rec_env (lt, rt)) args1 args2
       else
         unify_rec2 (RecAppl a1) (RecAppl a2)
    | RecursiveApplication appl, t2 ->
       unify_rec (RecAppl appl) t2
    |  t1, RecursiveApplication appl->
        unify_rec (RecAppl appl) t1
    (* Type *)
    | Primitive x, Primitive y when x = y -> ()
    | Function (lfrom, Row lm, lto), Function (rfrom, Row rm, rto) ->
       (ut (lfrom, rfrom);
        ur (lm, rm);
        ut (lto, rto))
    | Lolli (lfrom, Row lm, lto), Lolli (rfrom, Row rm, rto) ->
       (ut (lfrom, rfrom);
        ur (lm, rm);
        ut (lto, rto))
    | Record (Row l), Record (Row r) -> ur (l, r)
    | Variant (Row l), Variant (Row r) -> ur (l, r)
    | Table (lt, lf, ld, lr), Table (rt, rf, rd, rr) ->
       (utmp (lt, rt);
        ut (lf, rf);
        ut (ld, rd);
        ut (lr, rr))
    (* FIXME: what about Lens? *)
    | ForAll l, ForAll r ->
       let check_quantifier_kinds l r =
         if Quantifier.to_kind l <> Quantifier.to_kind r then
           raise (Failure (`Msg ("incompatible quantifier kinds")))
         else
           () in
       let rec flatten_to_matching (ls, lbody) (rs, rbody) =
         let ln = List.length ls and rn = List.length rs in
         if ln = rn then (ls, lbody), (rs, rbody)
         else if ln > rn then
           (* We have more left hand quantifiers than right - try to drop off
              another forall from the right. *)
           match TypeUtils.split_quantified_type rbody with
           | ([], _) -> raise (Failure (`Msg ("quantifier length mismatch")))
           | (qs, t) ->
              let (qs, rest) = ListUtils.split (ln - rn) qs in
              flatten_to_matching (ls, lbody) (rs @ qs, Types.for_all (rest, t))
         else
           (* We have more right hand quantifiers than right - try to drop off
              another forall from the left. *)
           match TypeUtils.split_quantified_type lbody with
           | ([], _) -> raise (Failure (`Msg ("quantifier length mismatch")))
           | (qs, t) ->
              let (qs, rest) = ListUtils.split (rn - ln) qs in
              flatten_to_matching (ls @ qs, Types.for_all (rest, t)) (rs, rbody)
       in

       let (ls, lbody), (rs, rbody) = flatten_to_matching l r in

       let qenv =
         List.fold_left2
           (fun (lenv, rs) l r ->
             check_quantifier_kinds l r;
             (IntMap.add (Quantifier.to_var l) (Quantifier.to_var r) lenv,
              IntSet.add (Quantifier.to_var r) rs))
           qenv
           ls
           rs in

       unify' {rec_env with qenv=qenv} (lbody, rbody)
    (* Row *)
    | Row l, Row r -> ur (l, r)
    | Closed, Closed -> ()
    (* Presence *)
    | (Present _ | Absent), (Present _ | Absent) -> unify_presence' rec_env (t1, t2)
    (* Effect *)
    | Effect (Row l), Effect (Row r) -> ur (l, r)
    | Operation (lfrom, lto, llin), Operation (rfrom, rto, rlin) ->
       (ut (lfrom, rfrom);
        ut (lto, rto);
        if llin = rlin then () else
          raise (Failure
          (`Msg ("Cannot unify two operations with different linearity '" ^
          string_of_datatype (Operation (lfrom, lto, llin)) ^ "' and '"^
          string_of_datatype (Operation (rfrom, rto, rlin)) ^"'"))))
    (* Session *)
    | Input (t, s), Input (t', s')
    | Output (t, s), Output (t', s') -> unify' rec_env (t, t'); ut (s, s')
    | Select (Row row), Select (Row row')
    | Choice (Row row), Choice (Row row') ->
       unify_rows' ~var_sk:(lin_any, res_session) rec_env (row, row')
    | Dual s, Dual s' -> ut (s, s')
    (* DODGEYNESS: dual_type doesn't necessarily make the type smaller -
       the following could potentially lead to non-termination *)
    | Dual s, s' ->
       begin
         (* if dual_type yields `Dual s then s must be a type variable *)
         match dual_type s with
         | Dual s -> ut (s, dual_type s')
         | s -> ut (s, s')
       end
    | s, Dual s' ->
       begin
         match dual_type s' with
         | Dual s' -> ut (dual_type s, s')
         | s' -> ut (s, s')
       end
    | End, End -> ()
    | _, _ ->
       raise (Failure (`Msg ("Couldn't match "^ string_of_datatype t1 ^" against "^ string_of_datatype t2)))
  end;
  counter := !counter-1;
  Debug.if_set (show_unification) (fun () -> "Unified types: " ^ string_of_datatype t1 ^ counter')
and unify_presence' : unify_env -> (field_spec * field_spec -> unit) =
  fun rec_env (l, r) ->
  match l, r with
  | Present lt, Present rt -> unify' rec_env (lt, rt)
  | Absent, Absent -> ()
  | Present _, Absent
  | Absent, Present _ ->
     raise (Failure (`Msg ("Present absent clash")))
  (*`PresentAbsentClash (label, lrow, rrow) *)
  | Meta lpoint, Meta rpoint ->
     (* TODO: take into account subkinds! *)
     begin
       match Unionfind.find lpoint, Unionfind.find rpoint with
       | l', _ when is_field_spec_body l' -> unify_presence' rec_env (l', r)
       | _, r' when is_field_spec_body r' -> unify_presence' rec_env (l, r')
       | Var (lvar, _, `Rigid), Var (rvar, _, `Rigid) when compatible_quantifiers (lvar, rvar) rec_env.qenv ->
          ()
       | Var (flexible_var, _, `Flexible), Var (rigid_var, _, `Rigid)
            when compatible_quantifiers (rigid_var, flexible_var) rec_env.qenv ->
          Unionfind.union lpoint rpoint
       | Var (rigid_var, _, `Rigid), Var (flexible_var, _, `Flexible)
            when compatible_quantifiers (rigid_var, flexible_var) rec_env.qenv ->
          Unionfind.union rpoint lpoint
       | Var (l, _, `Rigid), Var (r, _, `Rigid) ->
          if l <> r then
            raise (Failure (`Msg ("Rigid presence variables "^
                                    string_of_int l ^" and "^
                                      string_of_int r ^" do not match")))
          else
            Unionfind.union lpoint rpoint
       | Var (_, _, `Flexible), _ ->
          Unionfind.union lpoint rpoint
       | _, Var (_, _, `Flexible) ->
          Unionfind.union rpoint lpoint
       | _, _ -> assert false
       (* | l, r -> unify_presence' rec_env (l, r) *)
     end
  | Meta point, f | f, Meta point ->
     begin
       match (Unionfind.find point) with
       | Var (l, _, `Rigid) ->
          raise (Failure (`Msg ("Couldn't unify the rigid presence variable "^
                                  string_of_int l ^" with the presence flag "^
                                    string_of_presence f)))
       | Var (_, (_primary_kind, subkind), `Flexible) ->
          begin
            match f with
            | Absent ->
               Unionfind.change point Absent
            | Present t ->
               (* HACK: this ensures that any recursion is confined to
                  ordinary types inside presence types; hence we never
                  need recursive presence types *)
               let tv = Types.fresh_type_variable subkind in
               Unionfind.change point (Present tv);
               unify' rec_env (tv, t)
            (* let [q] = Types.quantifiers_of_type_args [`Presence (`Var point)] in *)
            (* let t' = Instantiate.apply_type (Types.for_all ([q], t)) [`Presence (`Present tv)] in *)
            (* unify' rec_env (tv, t'); *)
            (* Unionfind.change point (`Body (`Present tv)) *)
            | Var _ -> assert false
            | _ -> assert false
          end
       | f' -> unify_presence' rec_env (f, f')
     end
  | _, _ -> assert false

and unify_rows' : ?var_sk:Subkind.t -> unify_env -> ((row' * row') -> unit) =
  let unwrap_row r =
    let r', rvar = unwrap_row r in
    (* Debug.print (Printf.sprintf "Unwrapped row %s giving %s\n" (string_of_row r) (string_of_row r')); *)
    r', rvar in

  fun ?(var_sk=(lin_any, res_any)) rec_env (lrow, rrow) ->
  Debug.if_set (show_row_unification) (fun () -> "Unifying row: " ^ (string_of_row (Row lrow)) ^
                                                   " with row: " ^ (string_of_row (Row rrow)));

  let is_unguarded_recursive row =
    let rec is_unguarded rec_rows (field_env, row_var, _) =
      StringMap.is_empty field_env &&
        (match Unionfind.find row_var with
         | Closed
         | Var _ -> false
         | Recursive (var, _, _) when IntSet.mem var rec_rows -> true
         | Recursive (var, _, row) -> is_unguarded (IntSet.add var rec_rows) (TypeUtils.extract_row_parts row)
         | Row row -> is_unguarded rec_rows row
         | _ -> assert false) in
    is_unguarded IntSet.empty row in

  let domain_of_env : field_spec_map -> StringSet.t =
    fun env -> StringMap.fold (fun label _ labels -> StringSet.add label labels) env StringSet.empty in

  (* unify_field_envs closed rec_env (lenv, renv)

       unify lenv with renv

     The rigid argument should be set to true when both rows are
     rigid. In this case fields appearing in one row but not the other
     can give rise to an error.

     The closed argument should be set to true when both environments
     arise from closed rows. In this case missing labels may be
     treated as absent.
   *)
  let unify_field_envs : closed:bool -> rigid:bool -> unify_env -> (field_spec_map * field_spec_map) -> unit =
    fun ~closed:closed ~rigid:rigid rec_env (lenv, renv) ->
    let ldom = domain_of_env lenv in
    let rdom = domain_of_env renv in

    let shared_dom = StringSet.inter ldom rdom in

    (* rigid row unifcation cannot tolerate extra fields unless both
       rows are closed and all fields can be made absent *)
    if rigid then
      let lextras = StringSet.diff ldom rdom in
      let rextras = StringSet.diff rdom ldom in

      if not (StringSet.is_empty lextras) || not (StringSet.is_empty rextras) then
        (* some fields don't match *)
        if closed then
          (* closed rows don't need to explicitly mention absent *)
          let kill_extras extras env =
            StringSet.iter
              (fun label ->
                match StringMap.find label env with
                | (Absent | Meta _) as f ->
                   unify_presence' rec_env (f, Absent)
                | _ ->
                   raise (Failure (`Msg ("Field environments\n "^ string_of_row (Row (lenv, closed_row_var, false))
                                         ^"\nand\n "^ string_of_row (Row (renv, closed_row_var, false))
                                         ^"\n could not be unified because they have different fields"))))
              extras in
          kill_extras lextras lenv;
          kill_extras rextras renv
        else
          raise (Failure (`Msg ("Field environments\n "^ string_of_row (Row (lenv, closed_row_var, false))
                                ^"\nand\n "^ string_of_row (Row (renv, closed_row_var, false))
                                ^"\n could not be unified because they have different fields")))
      else
        ()
    else
      ();
    (* the apparently redundant else clauses above are present because
       otherwise it becomes hard to parse the code and easy to get the
       semantics wrong *)

    (* unify fields in shared domain *)
    StringSet.iter
      (fun label ->
        let lf = StringMap.find label lenv in
        let rf = StringMap.find label renv in
        unify_presence' rec_env (lf, rf))
      shared_dom in

  (* introduce a recursive row
       give an error if it is non-well-founded and
       non-well-founded type inference is switched off
   *)
  let rec_row_intro point (var, kind, row) =
    if occurs_check_row var row then
      Unionfind.change point (Recursive (var, kind, row))
    else
      raise (Failure (`Msg ("Cannot unify row variable "^string_of_int var^" with row "^string_of_row row^
                              " because "^
                                match Settings.get infer_recursive_types with
                                | Some Guarded -> "the row variable occurs unguarded inside the row"
                                | Some Positive -> "the row variable occurs in a negative position inside the row"
                                | _ -> assert false))) in

  (*
      unify_row_var_with_row rec_env (row_var, row)
      attempts to unify row_var with row

      However, row_var may already have been instantiated, in which case
      it is unified with row.
   *)
  let unify_row_var_with_row : unify_env -> row_var * bool * row' -> unit =
    fun rec_env (row_var, dual, ((extension_field_env, extension_row_var, _extension_dual) as extension_row)) ->
    (* unify row_var with `RowVar None *)
    let close_empty_row_var : row_var -> unit = fun point ->
      match Unionfind.find point with
      | Closed -> ()
      | Var (_, _, `Flexible) ->
         Unionfind.change point Closed
      | Var (_, _, `Rigid) ->
         raise (Failure (`Msg ("Closed row var cannot be unified with rigid row var\n")))
      | _ -> assert false in

    (* TODO: do we need to do something special here for the session subkind? *)

    (* unify row_var with `RigidRowVar var *)
    let rigidify_empty_row_var (point, (var, (lin, rest))) : row_var -> unit = fun point' ->
      match Unionfind.find point' with
      | Closed ->
         raise (Failure (`Msg ("Rigid row var cannot be unified with empty closed row\n")))
      | Var (_, (_, (_, rest')), `Flexible) ->
         if Restriction.is_any rest && Restriction.is_base rest' then
           raise (Failure (`Msg ("Rigid non-base row var cannot be unified with empty base row\n")));
         Unionfind.change point' (Var (var, (PrimaryKind.Row, (lin, rest)), `Rigid))
      | Var (var', _, `Rigid) when var=var' -> ()
      | Var (var', (_, (_, rest')), `Rigid) when rest=rest' && compatible_quantifiers (var, var') rec_env.qenv ->
         Unionfind.union point point'
      | Var (_, _, `Rigid) ->
         raise (Failure (`Msg ("Incompatible rigid row variables cannot be unified\n")))
      | Recursive _ | _ -> assert false in

    let extend = fun point ->
      (* point should be a row variable *)
      match Unionfind.find point with
      | Closed ->
         if is_empty_row (Row extension_row) then
           close_empty_row_var extension_row_var
         else
           raise (Failure (`Msg ("Closed row cannot be extended with non-empty row\n"
                                 ^string_of_row (Row extension_row))))
      | Var (var, (_, subkind), `Rigid) ->
         if is_empty_row (Row extension_row) then
           rigidify_empty_row_var (point, (var, subkind)) extension_row_var
         else
           raise (Failure (`Msg ("Rigid row variable cannot be unified with non-empty row\n"
                                 ^string_of_row (Row extension_row))))
      | Var (var, ((_, (lin, rest)) as kind), `Flexible) ->
         if not (StringMap.is_empty extension_field_env) &&
              TypeVarSet.mem var (free_row_type_vars (Row extension_row)) then
           begin
             if Restriction.is_base rest then
               raise (Failure (`Msg ("Cannot infer a recursive type for the base row variable "^ string_of_int var ^
                                       " with the body "^ string_of_row (Row extension_row))));
             rec_row_intro point (var, kind, Row extension_row)
           end
         else
           begin
             if Linearity.is_nonlinear lin then
               if Types.Unl.can_row_be (Row extension_row) then
                 Types.Unl.make_row (Row extension_row)
               else
                 raise (Failure (`Msg ("Cannot force row " ^ string_of_row (Row extension_row) ^ " to be unlimited")));

             begin
               match Types.get_restriction_constraint rest with
               | None -> ()
               | Some const ->
                  let module M = (val const) in
                  if M.can_row_be (Row extension_row) then
                      M.make_row (Row extension_row)
                  else
                    let message = Printf.sprintf "Cannot unify the %s row variable %d with the non-%s row %s."
                                    (Restriction.to_string rest) var (Restriction.to_string rest) (string_of_row (Row extension_row))
                    in raise (Failure (`Msg message))
             end;

             if StringMap.is_empty extension_field_env then
               if dual then
                 Unionfind.change point (Row (StringMap.empty, extension_row_var, true))
               else
                 Unionfind.union point extension_row_var
             else
               if dual then
                 Unionfind.change point (dual_row (Row extension_row))
               else
                 Unionfind.change point (Row extension_row)
           end
      | Recursive _ ->
         unify_rows' rec_env ((StringMap.empty, point, dual), extension_row)
      | row ->
         unify_rows' rec_env (TypeUtils.extract_row_parts (if dual then dual_row row else row), extension_row) in
    extend row_var in

  (*
       matching_labels (big_field_env, small_field_env)
       return the set of labels that appear in both big_field_env and small_field_env

       precondition: big_field_env contains small_field_env
   *)
  let matching_labels : field_spec_map * field_spec_map -> StringSet.t =
    fun (big_field_env, small_field_env) ->
    StringMap.fold (fun label _ labels ->
        if StringMap.mem label small_field_env then
          StringSet.add label labels
        else
          labels) big_field_env StringSet.empty in

  let row_without_labels : StringSet.t -> row' -> row' =
    fun labels (field_env, row_var, dual) ->
    let restricted_field_env =
      StringSet.fold (fun label field_env ->
          StringMap.remove label field_env) labels field_env in
    (restricted_field_env, row_var, dual) in

  (*
      register a recursive row in the rec_env environment

      return:
      None if the recursive row already appears in the environment
      Some rec_env, otherwise, where rec_env is the updated environment
   *)
  let register_rec_row (wrapped_field_env, unwrapped_field_env, rec_row, unwrapped_row') : unify_env -> unify_env option =
    fun rec_env ->
    let rec_rows = rec_env.renv in
    match rec_row with
    | Some row_var ->
       begin
         match Unionfind.find row_var with
         | Recursive (var, _, _) ->
            let restricted_row = row_without_labels (matching_labels (unwrapped_field_env, wrapped_field_env)) unwrapped_row' in
            let rs =
              if IntMap.mem var rec_rows then
                IntMap.find var rec_rows
              else
                [Row (StringMap.empty, row_var, false)] in
            if List.exists (fun r -> eq_rows (r, Row restricted_row)) rs then
              None
            else
              Some {rec_env with renv=IntMap.add var (Row restricted_row::rs) rec_rows}
         | _ -> assert false
       end
    | None ->
       Some rec_env in

  (*
      register two recursive rows and return None if one of them is already in the environment
   *)
  let register_rec_rows p1 p2 : unify_env -> unify_env option =
    fun rec_env ->
    let rec_env' = register_rec_row p1 rec_env in
    match rec_env' with
    | None -> None
    | Some rec_env -> register_rec_row p2 rec_env in

  let unify_both_rigid_with_rec_env rec_env ((lfield_env, _, _ as lrow), (rfield_env, _, _ as rrow)) =
    (* FIXME: this is why we really shouldn't have row = typ! *)
    let lrow', lrec_row = unwrap_row (Row lrow) in
    let rrow', rrec_row = unwrap_row (Row rrow) in
    let (lfield_env', lrow_var', _) as lrow' = TypeUtils.extract_row_parts lrow' in
    let (rfield_env', rrow_var', _) as rrow' = TypeUtils.extract_row_parts rrow' in
    (* let (lfield_env', lrow_var', _) as lrow', lrec_row = unwrap_row (Row lrow) in
     * let (rfield_env', rrow_var', _) as rrow', rrec_row = unwrap_row (Row rrow) in *)

    let closed =
      begin
        match Unionfind.find lrow_var', Unionfind.find rrow_var' with
        | Var (_, _, `Flexible), _ | _, Var (_, _, `Flexible) ->
           assert false (* both row variables must be rigid! *)
        | Recursive _, _ | _, Recursive _ ->
           assert false (* the rows must be unwrapped *)
        | Closed, Closed -> true
        | Var (_, lkind, `Rigid), Var (_, rkind, `Rigid) when lkind <> rkind ->
           raise (Failure (`Msg ("Rigid rows\n "^ string_of_row (Row lrow)
                                 ^"\nand\n "^ string_of_row (Row rrow)
                                 ^"\n could not be unified because they have different kinds")))
        | Var (lvar, _, `Rigid), Var (rvar, _, `Rigid)
             when (lvar=rvar || compatible_quantifiers (lvar, rvar) rec_env.qenv) ->
           false
        | Var (_, _, `Rigid), Var (_, _, `Rigid) ->
           raise (Failure (`Msg ("Rigid rows\n "^ string_of_row (Row lrow)
                                 ^"\nand\n "^ string_of_row (Row rrow)
                                 ^"\n could not be unified because they have distinct rigid row variables")))
        | Var (_, _, `Rigid), Closed | Closed, Var (_, _, `Rigid) ->
           raise (Failure (`Msg ("Rows\n "^ string_of_row (Row lrow)
                                 ^"\nand\n "^ string_of_row (Row rrow)
                                 ^"\n could not be unified because one is closed and the other has a rigid row variable")))
        | _, _ -> assert false
      end in
    let rec_env' =
      (register_rec_rows
         (lfield_env, lfield_env', lrec_row, rrow')
         (rfield_env, rfield_env', rrec_row, lrow')
         rec_env) in
    match rec_env' with
    | None -> ()
    | Some rec_env ->
       unify_field_envs ~closed ~rigid:true rec_env (lfield_env', rfield_env') in

  let unify_both_rigid (lrow, rrow) =
    unify_both_rigid_with_rec_env rec_env (lrow, rrow) in

  let unify_one_rigid ((rigid_field_env, _, _ as rigid_row), (flexible_field_env, _, _ as flexible_row)) =
    (* FIXME: row = typ was an idiotic mistake! *)
    let rigid_row', rigid_rec_row = unwrap_row (Row rigid_row) in
    let (rigid_field_env', rigid_row_var', rigid_dual') as rigid_row' = TypeUtils.extract_row_parts rigid_row' in
    (* let (rigid_field_env', rigid_row_var', rigid_dual') as rigid_row', rigid_rec_row = unwrap_row (Row rigid_row) in *)
    let closed = is_closed_row (Row rigid_row') in

    let flexible_row', flexible_rec_row = unwrap_row (Row flexible_row) in
    let (flexible_field_env', flexible_row_var', flexible_dual) as flexible_row' = TypeUtils.extract_row_parts flexible_row' in
    (* let (flexible_field_env', flexible_row_var', flexible_dual) as flexible_row', flexible_rec_row = unwrap_row flexible_row in *)
    (* check that the flexible row contains no extra fields *)
    StringMap.iter
      (fun label f ->
        if (StringMap.mem label rigid_field_env') then
          ()
        else
          match f with
          | (Absent | Meta _) when closed ->
             (* closed rows don't need to explicitly mention absent *)
             unify_presence' rec_env (f, Absent)
          | _ ->
             raise (Failure
                      (`Msg
                         ("Rows\n "^ string_of_row (Row rigid_row)
                          ^"\nand\n "^ string_of_row (Row flexible_row)
                          ^"\n could not be unified because the former is rigid"
                          ^" and the latter contains fields not present in the former, namely `"
                          ^ label ^"'.")))
      ) flexible_field_env';

    let rec_env' =
      (register_rec_rows
         (rigid_field_env, rigid_field_env', rigid_rec_row, flexible_row')
         (flexible_field_env, flexible_field_env', flexible_rec_row, rigid_row')
         rec_env) in
    match rec_env' with
    | None -> ()
    | Some rec_env ->
       unify_field_envs ~closed:false ~rigid:false rec_env (rigid_field_env', flexible_field_env');
       let flexible_extension = StringMap.filter (fun label _ -> not (StringMap.mem label flexible_field_env')) rigid_field_env' in
       unify_row_var_with_row rec_env (flexible_row_var', flexible_dual, (flexible_extension, rigid_row_var', rigid_dual')) in

  let unify_both_flexible ((lfield_env, _, ldual as lrow), (rfield_env, _, rdual as rrow)) =
    (* FIXME: more idiocy *)
    let lrow', lrec_row = unwrap_row (Row lrow) in
    let (lfield_env', lrow_var', ldual') as lrow' = TypeUtils.extract_row_parts lrow' in
    let rrow', rrec_row = unwrap_row (Row rrow) in
    let (rfield_env', rrow_var', rdual') as rrow' = TypeUtils.extract_row_parts rrow' in
    (* let (lfield_env', lrow_var', ldual') as lrow', lrec_row = unwrap_row lrow in
     * let (rfield_env', rrow_var', rdual') as rrow', rrec_row = unwrap_row rrow in *)
    (* Debug.print ("lrow': " ^ string_of_row (Row lrow'));
     * Debug.print ("rrow': " ^ string_of_row (Row rrow')); *)
    let rec_env' =
      (register_rec_rows
         (lfield_env, lfield_env', lrec_row, lrow')
         (rfield_env, rfield_env', rrec_row, rrow')
         rec_env) in
    match rec_env' with
    | None -> ()
    | Some rec_env ->
       if get_row_var (Row lrow) = get_row_var (Row rrow) && ldual = rdual then
         unify_both_rigid_with_rec_env rec_env ((lfield_env', Unionfind.fresh Closed, false),
                                                (rfield_env', Unionfind.fresh Closed, false))
       else
         begin
           unify_field_envs ~closed:false ~rigid:false rec_env (lfield_env', rfield_env');

           (* a fresh row variable common to the left and the right *)
           let fresh_row_var = fresh_row_variable var_sk in

           (* each row can contain fields missing from the other *)
           let rextension = StringMap.filter (fun label _ -> not (StringMap.mem label rfield_env')) lfield_env' in
(* Debug.print ("rext: "^string_of_row (Row (rextension, fresh_row_var, false))); *)
           unify_row_var_with_row rec_env (rrow_var', rdual', (rextension, fresh_row_var, false));

           let lextension = StringMap.filter (fun label _ -> not (StringMap.mem label lfield_env')) rfield_env' in
           unify_row_var_with_row rec_env (lrow_var', ldual', (lextension, fresh_row_var, false))
         end in

  (* report an error if an attempt is made to unify an unguarded
     recursive row with a row that is not unguarded recursive *)
  let check_unguarded_recursion lrow rrow =
    if is_unguarded_recursive lrow then
      if not (is_unguarded_recursive rrow) then
        raise (Failure
                 (`Msg ("Could not unify unguarded recursive row"^ string_of_row (Row lrow)
                        ^"\nwith row "^ string_of_row (Row rrow))))
      else if is_unguarded_recursive rrow then
        raise (Failure
                 (`Msg ("Could not unify unguarded recursive row"^ string_of_row (Row rrow)
                        ^"\nwith row "^ string_of_row (Row lrow)))) in

  check_unguarded_recursion lrow rrow;

  if is_rigid_row (Row lrow) then
    if is_rigid_row (Row rrow) then
      unify_both_rigid (lrow, rrow)
    else
      unify_one_rigid (lrow, rrow)
  else if is_rigid_row (Row rrow) then
    unify_one_rigid (rrow, lrow)
  else
    unify_both_flexible (rrow, lrow);

  Debug.if_set (show_row_unification)
    (fun () -> "Unified rows: " ^ (string_of_row (Row lrow)) ^ " and: " ^ (string_of_row (Row rrow)))

and unify_type_args' : unify_env -> (type_arg * type_arg) -> unit =
  fun rec_env ((lpk, lt), (rpk, rt)) ->
  let open PrimaryKind in
  match lpk, rpk with
  | Type, Type         -> unify' rec_env (lt, rt)
  | Row, Row           -> unify' rec_env (lt, rt)
  | Presence, Presence -> unify_presence' rec_env (lt, rt)
  | _, _ ->
     raise (Failure (`Msg ("Couldn't match "^ string_of_type_arg (lpk, lt) ^" against "^ string_of_type_arg (rpk, rt))))

let unify (t1, t2) =
  unify'
    { tenv = RecIdMap.empty
    ; renv = IntMap.empty
    ; qenv = (IntMap.empty, IntSet.empty)
    } (t1, t2)

(* Debug.if_set (show_unification) (fun () -> "Unified types: " ^ string_of_datatype t1) *)
and unify_rows (row1, row2) =
  unify_rows'
    { tenv = RecIdMap.empty
    ; renv = IntMap.empty
    ; qenv = (IntMap.empty, IntSet.empty)
    } (row1, row2)

(* external interface *)
let datatypes = unify
let rows = unify_rows
