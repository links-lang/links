open Utility
open Types
open Typevarcheck

(* debug flags *)
let show_unification = Settings.add_bool("show_unification", false, `User)
let show_row_unification = Settings.add_bool("show_row_unification", false, `User)
let show_recursion = Instantiate.show_recursion

let rigid_type_variables = Settings.add_bool("rigid_type_variables", true, `User)
(*
  what kind of recursive types to allow
  "all"      - allow all recursive types
  "guarded"  - only allow guarded recursive types
  "positive" - only allow positive recursive types
 *)
let infer_recursive_types = Settings.add_string("infer_recursive_types", "guarded", `User)

type error = [
  `Msg of string
(* TODO: is there a need to support errors other than plain
   messages? *)
| `PresentAbsentClash of string * Types.row * Types.row
]

exception Failure of error

let occurs_check var t =
  match Settings.get_value infer_recursive_types with
    | "all" -> true
    | "guarded" -> is_guarded var t
    | "positive" -> not (is_negative var t)
    | s -> failwith ("user setting infer_recursive_types ("^ s ^") must be set to 'all', 'guarded' or 'positive'")

let occurs_check_row var row =
  match Settings.get_value infer_recursive_types with
    | "all" -> true
    | "guarded" -> is_guarded_row var row
    | "positive" -> not (is_negative_row var row)
    | s -> failwith ("user setting infer_recursive_types ("^ s ^") must be set to 'all', 'guarded' or 'positive'")

let var_is_free_in_type var datatype = TypeVarSet.mem var (free_type_vars datatype)

(* a special kind of structural equality on types that doesn't look
inside points *)
let rec eq_types : (datatype * datatype) -> bool =
  fun (t1, t2) ->
    let rec unalias = function
      | `Alias (_, x) -> unalias x
      | x             -> x in
    match unalias t1 with
      | `Not_typed ->
          begin match unalias t2 with
              `Not_typed -> true
            | _          -> false
          end
      | `Primitive x ->
          begin match unalias t2 with
              `Primitive y -> x = y
            | _            -> false
          end
      | `MetaTypeVar lpoint ->
          begin match unalias t2 with
              `MetaTypeVar rpoint -> Unionfind.equivalent lpoint rpoint
            | _                   -> false
          end
      | `Function (lfrom, lm, lto) ->
          begin match unalias t2 with
              `Function (rfrom, rm, rto) -> eq_types (lfrom, rfrom)
                                         && eq_types (lto,   rto)
                                         && eq_rows  (lm,    rm)
            | _                          -> false
          end
      | `Lolli (lfrom, lm, lto) ->
          begin match unalias t2 with
              `Lolli (rfrom, rm, rto) -> eq_types (lfrom, rfrom)
                                      && eq_types (lto,   rto)
                                      && eq_rows  (lm,    rm)
            | _                       -> false
          end
      | `Record l ->
          begin match unalias t2 with
              `Record r -> eq_rows (l, r)
            | _         -> false
          end
      | `Variant l ->
          begin match unalias t2 with
              `Variant r -> eq_rows (l, r)
            | _          -> false
          end
      | `Application (s, ts) ->
          begin match unalias t2 with
              `Application (s', ts') -> s = s' && List.for_all2 (Utility.curry eq_type_args) ts ts'
            | _ -> false
          end
      | `ForAll (qs, t) ->
          begin match unalias t2 with
            | `ForAll (qs', t') ->
                List.for_all2 (fun q q' -> eq_quantifier (q, q'))
                  (Types.unbox_quantifiers qs)
                  (Types.unbox_quantifiers qs') &&
                  eq_types (t, t')
            | _ -> false
          end
      | #session_type as l ->
        begin match unalias t2 with
        | #session_type as r -> eq_sessions (l, r)
        | _          -> false
        end

      | `Alias  _ -> assert false
      | `Table _  -> assert false
and eq_sessions : (datatype * datatype) -> bool =
  function
  | `Input (lt, ls), `Input (rt, rs)
  | `Output (lt, ls), `Output (rt, rs) ->
    eq_types (lt, rt)
  | `Select l, `Select r
  | `Choice l, `Choice r ->
    eq_rows (l, r)
  (* TODO: do we need to actually perform any dualisation here? *)
  | `Dual l, `Dual r ->
    eq_types (l, r)
  | `End, `End -> true
  | _, _ -> false
and eq_quantifier : (quantifier * quantifier) -> bool =
  function
    | (lvar, _, _), (rvar, _, _) -> lvar = rvar
and eq_rows : (row * row) -> bool =
  fun ((lfield_env, lrow_var, ldual), (rfield_env, rrow_var, rdual)) ->
    eq_field_envs (lfield_env, rfield_env) && eq_row_vars (lrow_var, rrow_var) && ldual=rdual
and eq_presence =
  function
    | `Absent, `Absent -> true
    | `Present lt, `Present rt -> eq_types (lt, rt)
    | `Var lpoint, `Var rpoint -> Unionfind.equivalent lpoint rpoint
and eq_field_envs (lfield_env, rfield_env) =
  let eq_specs lf rf = eq_presence (lf, rf) in
    StringMap.equal eq_specs lfield_env rfield_env
and eq_row_vars (lpoint, rpoint) =
  (* QUESTION:
     Do we need to deal with closed rows specially?
  *)
  match Unionfind.find lpoint, Unionfind.find rpoint with
    | `Closed, `Closed -> true
    | `Var (var, _, _), `Var (var', _, _)
    | `Recursive (var, _), `Recursive (var', _) -> var=var'
    | _, _ -> Unionfind.equivalent lpoint rpoint
and eq_type_args =
  function
    | `Type lt, `Type rt -> eq_types (lt, rt)
    | `Row lr, `Row rr -> eq_rows (lr, rr)
    | `Presence lf, `Presence rf -> eq_presence (lf, rf)
    | _, _ -> false

(*
  unification environment:
    for stopping cycles during unification
*)
type unify_type_env = (datatype list) IntMap.t
type unify_row_env = (row list) IntMap.t

type quantifier_stack = int * int IntMap.t * int IntMap.t

let compatible_quantifiers (lvar, rvar) (_, lenv, renv) =
  match IntMap.lookup lvar lenv, IntMap.lookup rvar renv with
    | Some ldepth, Some rdepth when ldepth=rdepth -> true
    | _ -> false

type unify_env = {tenv: unify_type_env; renv: unify_row_env; qstack: quantifier_stack}

let rec unify' : unify_env -> (datatype * datatype) -> unit =
let counter = ref 0 in
fun rec_env ->
  let rec_types = rec_env.tenv in
  let rec_rows = rec_env.renv in
  let qstack = rec_env.qstack in

  let is_unguarded_recursive t =
    let rec is_unguarded rec_types t =
      match t with
        | `MetaTypeVar point ->
            begin
              match (Unionfind.find point) with
                | `Recursive (var, body) when IntSet.mem var rec_types -> true
                | `Recursive (var, body) -> is_unguarded (IntSet.add var rec_types) body
                | `Body t -> is_unguarded rec_types t
                | _ -> false
            end
        |  _ -> false
    in
      is_unguarded IntSet.empty t in

  let unify_rec ((var, body), t) =
    let ts =
      if IntMap.mem var rec_types then
        IntMap.find var rec_types
      else
        [body]
    in
      (* break cycles *)
      if List.exists (fun t' -> eq_types (t, t')) ts then
        ()
      else
        unify' {rec_env with tenv=IntMap.add var (t::ts) rec_types} (body, t) in

  let unify_rec2 ((lvar, lbody), (rvar, rbody)) =
    let lts =
      if IntMap.mem lvar rec_types then
        IntMap.find lvar rec_types
      else
        [lbody] in

    let rts =
      if IntMap.mem rvar rec_types then
        IntMap.find rvar rec_types
      else
        [rbody]
    in
      (* break cycles *)
      if (List.exists (fun t -> eq_types (t, rbody)) lts
       || List.exists (fun t -> eq_types (t, lbody)) rts) then
        ()
      else
        unify' {rec_env with tenv=(IntMap.add lvar (rbody::lts) ->- IntMap.add rvar (lbody::rts)) rec_types} (lbody, rbody) in

  (* introduce a recursive type
     give an error if it is non-well-founded and
     non-well-founded type inference is switched off

     preconditions:
     - Unionfind.find point = t
     - var is free in t
  *)
  let rec_intro point (var, t) =
    if occurs_check var t then
      Unionfind.change point (`Recursive (var, t))
    else
      raise (Failure (`Msg ("Cannot unify type variable "^string_of_int var^" with datatype "^string_of_datatype t^
                              " because "^
                              match Settings.get_value infer_recursive_types with
                                | "guarded" -> "the type variable occurs unguarded inside the datatype"
                                | "positive" -> "the type variable occurs in a negative position inside the datatype"
                                | _ -> assert false))) in

  let ignore_empty_quantifiers =
    function
      | `ForAll (qs, t) when Types.unbox_quantifiers qs = [] -> t
      | t -> t in

  (* make sure the contents of a point is concrete *)
  let concrete_point point =
    match Unionfind.find point with
      | `Body t ->
          begin
            match Types.concrete_type t with
              | `MetaTypeVar point' ->
                  Unionfind.union point point'
              | _ ->
                  () (*Unionfind.change point (`Body t)*)
          end
      | _ -> () in

    fun (t1, t2) ->
      let () = hoist_quantifiers t1 in
      let () = hoist_quantifiers t2 in

      let t1 = ignore_empty_quantifiers t1 in
      let t2 = ignore_empty_quantifiers t2 in

      let ut = unify' rec_env in
      let ur = unify_rows' rec_env in
      counter := !counter+1;
        let counter' = "(" ^ string_of_int !counter ^ ")" in
      Debug.if_set (show_unification) (fun () -> "Unifying "^string_of_datatype t1^" with "^string_of_datatype t2 ^ counter');
      begin
        match (t1, t2) with
          | `Not_typed, _ | _, `Not_typed -> failwith "Internal error: `Not_typed' passed to `unify'"
          | `Primitive x, `Primitive y when x = y -> ()
          | `MetaTypeVar lpoint, `MetaTypeVar rpoint ->
              if Unionfind.equivalent lpoint rpoint then
                ()
              else
                begin
                  concrete_point lpoint;
                  concrete_point rpoint;

                  match (Unionfind.find lpoint, Unionfind.find rpoint) with
                   | `Var (lvar, lkind, `Rigid), `Var (rvar, rkind, `Rigid) when lkind=rkind && compatible_quantifiers (lvar, rvar) qstack ->
                       Unionfind.union lpoint rpoint
                   | `Var (l, _, `Rigid), `Var (r, _, `Rigid) ->
                       if l <> r then
                         raise (Failure (`Msg ("Rigid type variables "^ string_of_int l ^" and "^ string_of_int r ^" do not match")))
                       else
                         (* presumably this should always be a no-op *)
                         Unionfind.union lpoint rpoint
                   | `Var (lvar, (llin,lrest), `Flexible), `Var (rvar, (rlin,rrest), `Flexible) ->
                       Unionfind.union lpoint rpoint;
                       begin
                         let lin =
                           match llin, rlin with
                             | `Unl, _
                             | _, `Unl -> `Unl
                             | _       -> llin in
                         let rest =
                           match lrest, rrest with
                             | `Base, `Any
                             | `Any, `Base -> `Base
                             | `Any, `Session
                             | `Session, `Any -> `Session
                             | `Base, `Session ->
                                 raise (Failure (`Msg ("Cannot unify base type variable " ^ string_of_int lvar ^
                                                         " with session type variable " ^ string_of_int rvar)))
                             | `Session, `Base ->
                                 raise (Failure (`Msg ("Cannot unify session type variable " ^ string_of_int lvar ^
                                                         " with base type variable " ^ string_of_int rvar)))
                             (* in the default case lrest and rrest must be identical *)
                             | _ -> lrest in
                         Unionfind.change lpoint (`Var (lvar, (lin, rest), `Flexible))
                       end
                   | `Var (var, (lin, rest), `Flexible), _ ->
                       (if var_is_free_in_type var t2 then
                          (Debug.if_set (show_recursion) (fun () -> "rec intro1 (" ^ (string_of_int var) ^ ")");
                           if rest = `Base then
                             raise (Failure (`Msg ("Cannot infer a recursive type for the type variable "^ string_of_int var ^
                                                     " with the body "^ string_of_datatype t2)));
                           rec_intro rpoint (var, Types.concrete_type t2))
                        else
                          ());
                       if rest = `Base then
                         if Types.is_baseable_type t2 then
                           Types.basify_type t2
                         else
                           raise (Failure (`Msg ("Cannot unify the base type variable "^ string_of_int var ^
                                                   " with the non-base type "^ string_of_datatype t2)));
                       if lin = `Unl then
                         if Types.type_can_be_unl t2 then
                           Types.make_type_unl t2
                         else
                           raise (Failure (`Msg ("Cannot unify the unlimited type variable " ^ string_of_int var ^
                                                   " with the linear type " ^ string_of_datatype t2)));
                       if rest = `Session then
                         if Types.is_sessionable_type t2 then
                           Types.sessionify_type t2
                         else
                           raise (Failure (`Msg ("Cannot unify the session type variable "^ string_of_int var ^
                                                   " with the non-session type "^ string_of_datatype t2)));
                       Unionfind.union lpoint rpoint
                   | _, `Var (var, (lin, rest), `Flexible) ->
                       (if var_is_free_in_type var t1 then
                          (Debug.if_set (show_recursion) (fun () -> "rec intro2 (" ^ (string_of_int var) ^ ")");
                           if rest = `Base then
                             raise (Failure (`Msg ("Cannot infer a recursive type for the type variable "^ string_of_int var ^
                                                     " with the body "^ string_of_datatype t1)));
                           rec_intro lpoint (var, Types.concrete_type t1))
                        else
                          ());
                       if rest = `Base then
                         if Types.is_baseable_type t1 then
                           Types.basify_type t1
                         else
                           raise (Failure (`Msg ("Cannot unify the base type variable "^ string_of_int var ^
                                                   " with the non-base type "^ string_of_datatype t1)));
                       if lin = `Unl then
                         if Types.type_can_be_unl t1 then
                           Types.make_type_unl t1
                         else
                           raise (Failure (`Msg ("Cannot unify the unlimited type variable " ^ string_of_int var ^
                                                   " with the linear type " ^ string_of_datatype t1)));
                       if rest = `Session then
                         if Types.is_sessionable_type t1 then
                           Types.sessionify_type t1
                         else
                           raise (Failure (`Msg ("Cannot unify the session type variable "^ string_of_int var ^
                                                   " with the non-session type "^ string_of_datatype t1)));
                       Unionfind.union rpoint lpoint
                   | `Var (l, _, `Rigid), _ ->
                       begin
                         match Types.flexible_of_type t2 with
                           | Some t2 -> unify' rec_env (t1, t2)
                           | None ->
                               raise (Failure (`Msg ("Couldn't unify the rigid type variable "^
                                                       string_of_int l ^" with the type "^ string_of_datatype (`MetaTypeVar rpoint))))
                       end
                   | _, `Var (r, _, `Rigid) ->
                       begin
                         match Types.flexible_of_type t1 with
                           | Some t1 -> unify' rec_env (t1, t2)
                           | None ->
                               raise (Failure (`Msg ("Couldn't unify the rigid type variable "^
                                                       string_of_int r ^" with the type "^ string_of_datatype (`MetaTypeVar lpoint))))
                       end
                   | `Recursive (lvar, t), `Recursive (rvar, t') ->
                       assert (lvar <> rvar);
                       Debug.if_set (show_recursion)
                         (fun () -> "rec pair (" ^ (string_of_int lvar) ^ "," ^ (string_of_int rvar) ^")");
                       begin
                         if is_unguarded_recursive (`MetaTypeVar lpoint) then
                           begin
                             if not (is_unguarded_recursive (`MetaTypeVar rpoint)) then
                               raise (Failure (`Msg ("Couldn't unify the unguarded recursive type "^
                                                       string_of_datatype (`MetaTypeVar lpoint) ^
                                                       " with the guarded recursive type "^ string_of_datatype (`MetaTypeVar rpoint))))
                           end
                         else if is_unguarded_recursive (`MetaTypeVar lpoint) then
                           raise (Failure (`Msg ("Couldn't unify the unguarded recursive type "^
                                                   string_of_datatype (`MetaTypeVar rpoint) ^
                                                   " with the guarded recursive type "^ string_of_datatype (`MetaTypeVar lpoint))))
                         else
                           unify_rec2 ((lvar, t), (rvar, t'))
                       end;
                       Unionfind.union lpoint rpoint
                   | `Recursive (var, t'), `Body t ->
                       Debug.if_set (show_recursion) (fun () -> "rec left (" ^ (string_of_int var) ^ ")");
                       begin
                         if is_unguarded_recursive (`MetaTypeVar lpoint) then
                           raise (Failure (`Msg ("Couldn't unify the unguarded recursive type "^
                                                   string_of_datatype (`MetaTypeVar lpoint) ^
                                                   " with the non-recursive type "^ string_of_datatype (`MetaTypeVar rpoint))))
                         else
                           unify_rec ((var, t'), t)
                       end;
                       (*
                          it is critical that the arguments to Unionfind.union are in this order
                          to ensure that we keep the recursive type rather than its unwinding
                       *)
                       (* union keeps the data associated with its second argument *)
                       Unionfind.union rpoint lpoint
                   | `Body t, `Recursive (var, t') ->
                       Debug.if_set (show_recursion) (fun () -> "rec right (" ^ (string_of_int var) ^ ")");
                       begin
                         if is_unguarded_recursive (`MetaTypeVar rpoint) then
                           raise (Failure (`Msg ("Couldn't unify the unguarded recursive type "^
                                                   string_of_datatype (`MetaTypeVar rpoint) ^
                                                   " with the non-recursive type "^ string_of_datatype (`MetaTypeVar lpoint))))
                         else
                           unify_rec ((var, t'), t)
                       end;
                       Unionfind.union lpoint rpoint
                   | `Body t, `Body t' ->
                       ut (t, t')
                       (*
                          Apparently this isn't sound as it can lead to unguarded recursion:

                            Unionfind.union lpoint rpoint
                       *)
                end
          | `MetaTypeVar point, t | t, `MetaTypeVar point ->
              concrete_point point;
              begin
                match Unionfind.find point with
                 | `Var (l, _, `Rigid) ->
                     begin
                       match Types.flexible_of_type t with
                         | Some t -> unify' rec_env (t, `MetaTypeVar point)
                         | None ->
                             raise (Failure (`Msg ("Couldn't unify the rigid type variable "^ string_of_int l ^
                                                     " with the type "^ string_of_datatype t)))
                     end
                 | `Var (var, (lin, rest), `Flexible) ->
                     if var_is_free_in_type var t then
                       begin
                         Debug.if_set (show_recursion)
                           (fun () -> "rec intro3 ("^string_of_int var^","^string_of_datatype t^")");
                         if rest = `Base then
                           raise (Failure (`Msg ("Cannot infer a recursive type for the type variable "^ string_of_int var ^
                                                   " with the body "^ string_of_datatype t)));
                         let point' = Unionfind.fresh (`Body t)
                         in
                           rec_intro point' (var, t);
                           Unionfind.union point point'
                       end
                     else
                       (Debug.if_set (show_recursion) (fun () -> "non-rec intro (" ^ string_of_int var ^ ")");
                        if rest = `Base then
                          if Types.is_baseable_type t then
                            Types.basify_type t
                          else
                            raise (Failure (`Msg ("Cannot unify the base type variable "^ string_of_int var ^
                                                    " with the non-base type "^ string_of_datatype t)));
                        if lin = `Unl then
                          if Types.type_can_be_unl t then
                            Types.make_type_unl t
                          else
                            raise (Failure (`Msg ("Cannot unify the unlimited type variable " ^ string_of_int var ^
                                                    " with the linear type "^ string_of_datatype t)));
                        if rest = `Session then
                          if Types.is_sessionable_type t then
                            Types.sessionify_type t
                          else
                            raise (Failure (`Msg ("Cannot unify the session type variable "^ string_of_int var ^
                                                    " with the non-session type "^ string_of_datatype t)));
                        Unionfind.change point (`Body t))
                 | `Recursive (var, t') ->
                     Debug.if_set (show_recursion) (fun () -> "rec single (" ^ (string_of_int var) ^ ")");
                     begin
                       if is_unguarded_recursive (`MetaTypeVar point) then
                         raise (Failure (`Msg ("Couldn't unify the unguarded recursive type "^
                                                 string_of_datatype (`MetaTypeVar point) ^
                                                 " with the non-recursive type "^ string_of_datatype t)))
                       else
                         unify_rec ((var, t'), t)
                     end
                       (* It's tempting to try to do this, but it isn't sound
                          as point may appear inside t

                          Unionfind.change point t;
                       *)
                 | `Body t' -> ut (t, t')
              end
          | `Alias (_, t1), t2
          | t1, `Alias (_, t2) -> ut (t1, t2)
          | `Function (lfrom, lm, lto), `Function (rfrom, rm, rto) ->
              (ut (lfrom, rfrom);
               ur (lm, rm);
               ut (lto, rto))
          | `Lolli (lfrom, lm, lto), `Lolli (rfrom, rm, rto) ->
              (ut (lfrom, rfrom);
               ur (lm, rm);
               ut (lto, rto))
          | `Record l, `Record r -> ur (l, r)
          | `Variant l, `Variant r -> ur (l, r)
          | `Table (lf, ld, lr), `Table (rf, rd, rr) ->
              (ut (lf, rf);
               ut (ld, rd);
               ut (lr, rr))
          | `Application (l, _), `Application (r, _) when l <> r ->
              raise (Failure
                       (`Msg ("Cannot unify abstract type '"^string_of_datatype t1^
                                "' with abstract type '"^string_of_datatype t2^"'")))
          | `Application (l, ls), `Application (r, rs) ->
              List.iter2 (fun lt rt -> unify_type_args' rec_env (lt, rt)) ls rs
          | `ForAll (lsref, lbody), `ForAll (rsref, rbody) ->
              (* Check that all quantifiers that were originally rigid
                 are still distinct *)
              let rec distinct_rigid_check (qs, ss) =
                let rec drc rigids (qs, ss) =
                  match qs, ss with
                    | [], [] -> ()
                    | q::qs, s::ss ->
                        begin
                          match s with
                            | `Flexible ->
                                drc rigids (qs, ss)
                            | `Rigid ->
                                let x = Types.var_of_quantifier q in
                                  if IntSet.mem x rigids then
                                    raise (Failure (`Msg ("incompatible quantifiers (duplicate rigid quantifiers)")))
                                  else
                                    drc (IntSet.add x rigids) (qs, ss)
                        end
                in
                  drc (IntSet.empty) in

              let ls = !lsref in
              let rs = !rsref in

              (* collect the variables from a quantifier list into a set *)
              let collect =
                List.fold_right
                  (fun q bound_vars ->
                     IntSet.add (Types.var_of_quantifier q) bound_vars) in

              (* the original variables before unification *)
              let original_vars = collect ls (collect rs IntSet.empty) in

              (* check that two quantifiers have the same kind *)
              let are_compatible =
                function
                  | (l, _, _), (r, _, _) -> l=r
                  | _ -> false in

              (* identify which quantifiers start off rigid *)
              let status q =
                if Types.is_rigid_quantifier q then `Rigid
                else `Flexible in

              (* We're assuming that all of the quantifiers start off atomic
                 (either rigid or flexible type variables, rather than instantiated
                 as some other type). Does this assumption always hold?

                 Perhaps we should extract the quantifiers initially just in case.
                 This might allow us to think about other simplifications as well,
                 such as getting rid of the annoying reference type constructor.

                 We could add quantifier extraction to the FixTypeAbstractions
                 sugar transformer pass.

                 Doing this kind of thing may be too difficult,
                 because we might not have enough information
                 available (e.g. how do we know which quantifiers need
                 to be thrown away). Storing the information might
                 take more effort than the current implementation
                 which just requires the quantifier list to be
                 mutable. *)
              let lstatus = List.map status ls in
              let rstatus = List.map status rs in

              let depth, lenv, renv = qstack in
              let depth = depth+1 in
              let lenv = List.fold_right (fun q lenv -> IntMap.add (var_of_quantifier q) depth lenv) ls lenv in
              let renv = List.fold_right (fun q renv -> IntMap.add (var_of_quantifier q) depth renv) rs renv in

              let () = unify' {rec_env with qstack=(depth, lenv, renv)} (lbody, rbody) in

              (* Here we need to extract instantiated quantifiers
                 e.g.:

                 if we unify

                   forall %a.(a) -> a

                 with

                   forall a,b.((a) -> b) -> (a) -> b

                 Then we get:

                   forall ((a) -> b).((a) -> b) -> ((a) -> b)

                 which we can then convert to:

                   forall a,b.((a) -> b) -> ((a) -> b)

                 by pulling out a and b from the %a quantifier (which
                 was instantiated to (a) -> b).

                 Generalise.extract_quantifiers does this.

                 We then propagate any changes due to unification of
                 the bodies back into the quantifiers.
              *)

                distinct_rigid_check (ls, lstatus);
                distinct_rigid_check (rs, rstatus);

              let ls = Generalise.extract_quantifiers ls in
              let rs = Generalise.extract_quantifiers rs in

              let lvars = collect ls IntSet.empty in
              let rvars = collect rs IntSet.empty in

              (* throw away any rigid quantifiers that weren't in the
                 original set of quantifiers, as they must be unbound
                 or bound at an outer scope *)
              let ls, rs =
                let filter =
                  List.filter
                    (fun q -> not (is_rigid_quantifier q)
                       || IntSet.mem (Types.var_of_quantifier q) original_vars)
                in
                  filter ls, filter rs in

              (* throw away any unpartnered flexible quantifiers
                 raise an error for unpartnered rigid quantifiers *)
              let ls, rs =
                let cull vars qs =
                  List.fold_right
                    (fun q qs ->
                       if IntSet.mem (Types.var_of_quantifier q) vars then
                         q::qs
                       else if is_rigid_quantifier q then
                         raise (Failure (`Msg "Incompatible quantifiers"))
                       else
                         qs)
                    qs
                    []
                in
                  cull rvars ls, cull lvars rs in

              (* Now we know that ls and rs contain the same quantifiers *)

              (* unify_quantifiers just checks that the kinds of the
                 quantifiers match up

                 This seems unnecessary as we already know that the
                 names of the quantifiers match up and it should not
                 be possible to have distinct type variables with the
                 same name.

              *)
              (* let rec unify_quantifiers (ls, rs) = *)
              (*   let compare q q' = *)
              (*     Int.compare (Types.var_of_quantifier q) (Types.var_of_quantifier q') *)
              (*   in *)
              (*     match List.sort compare ls, List.sort compare rs with *)
              (*       | [], [] -> () *)
              (*       | l::ls, r::rs when are_compatible (l, r) -> unify_quantifiers (ls, rs) *)
              (*       | _ -> raise (Failure (`Msg ("Incompatible quantifiers"))) *)
              (* in *)
              (*   unify_quantifiers (ls, rs); *)
                lsref := ls;
                rsref := rs
          | `ForAll (qs, body), t ->
              if List.for_all (fun q -> not (Types.is_rigid_quantifier q)) (Types.unbox_quantifiers qs) then
                begin
                  qs := [];
                  ut (body, t)
                end
              else
                raise (Failure (`Msg ("Can't unify quantified type " ^ string_of_datatype t1 ^
                                        " with unquantified type " ^ string_of_datatype t2)))
          | t, `ForAll (qs, body) ->
              if List.for_all (fun q -> not (Types.is_rigid_quantifier q)) (Types.unbox_quantifiers qs) then
                begin
                  qs := [];
                  ut (t, body)
                end
              else
                raise (Failure (`Msg ("Can't unify unquantified type " ^ string_of_datatype t1 ^
                                        " with quantified type " ^ string_of_datatype t2)))
          | `Input (t, s), `Input (t', s')
          | `Output (t, s), `Output (t', s')
            -> unify' rec_env (t, t'); ut (s, s')
          | `Select row, `Select row'
          | `Choice row, `Choice row' ->
             unify_rows' rec_env (row, row')
          | `Dual s, `Dual s' -> ut (s, s')
          (* DODGEYNESS: dual_type doesn't doesn't necessarily make
           the type smaller - the following could potentially lead to
           non-termination *)
          | `Dual s, s' ->
            begin
              (* if dual_type yields `Dual s then s must be a type variable *)
              match dual_type s with
              | `Dual s -> ut (s, dual_type s')
              | s -> ut (s, s')
            end
          | s, `Dual s' ->
            begin
              match dual_type s' with
              | `Dual s' -> ut (dual_type s, s')
              | s' -> ut (s, s')
            end
          | `End, `End -> ()
          | _, _ ->
              raise (Failure (`Msg ("Couldn't match "^ string_of_datatype t1 ^" against "^ string_of_datatype t2)))
       end;
      counter := !counter-1;
      Debug.if_set (show_unification) (fun () -> "Unified types: " ^ string_of_datatype t1 ^ counter')
and unify_presence' : unify_env -> (field_spec * field_spec -> unit) =
  fun rec_env (l, r) ->
    match l, r with
      | `Present lt, `Present rt -> unify' rec_env (lt, rt)
      | `Absent, `Absent -> ()
      | `Present _, `Absent
      | `Absent, `Present _ ->
          raise (Failure (`Msg ("Present absent clash")))
(*`PresentAbsentClash (label, lrow, rrow) *)
      | `Var lpoint, `Var rpoint ->
        (* TODO: take into account subkinds! *)
          begin
            match Unionfind.find lpoint, Unionfind.find rpoint with
              | `Body l, _ -> unify_presence' rec_env (l, `Var rpoint)
              | _, `Body r -> unify_presence' rec_env (`Var lpoint, r)
              | `Var (lvar, _, `Rigid), `Var (rvar, _, `Rigid) when compatible_quantifiers (lvar, rvar) rec_env.qstack ->
                  Unionfind.union lpoint rpoint
              | `Var (flexible_var, _, `Flexible), `Var (rigid_var, _, `Rigid)
                  when compatible_quantifiers (rigid_var, flexible_var) rec_env.qstack ->
                  Unionfind.union lpoint rpoint
              | `Var (rigid_var, _, `Rigid), `Var (flexible_var, _, `Flexible)
                  when compatible_quantifiers (rigid_var, flexible_var) rec_env.qstack ->
                  Unionfind.union rpoint lpoint
              | `Var (l, _, `Rigid), `Var (r, _, `Rigid) ->
                  if l <> r then
                    raise (Failure (`Msg ("Rigid presence variables "^
                                            string_of_int l ^" and "^
                                            string_of_int r ^" do not match")))
                  else
                    Unionfind.union lpoint rpoint
              | `Var (var, _, `Flexible), _ ->
                  Unionfind.union lpoint rpoint
              | _, `Var (var, _, `Flexible) ->
                  Unionfind.union rpoint lpoint
              | `Var (l, _, `Rigid), _ ->
                  raise (Failure (`Msg ("Couldn't unify the rigid presence variable "^
                                          string_of_int l ^" with the presence variable "^
                                          string_of_presence (`Var rpoint))))
              | _, `Var (r, _, `Rigid) ->
                  raise (Failure (`Msg ("Couldn't unify the rigid presence variable "^
                                          string_of_int r ^" with the type "^
                                          string_of_presence (`Var lpoint))))
          end
      | `Var point, f | f, `Var point ->
          begin
            match (Unionfind.find point) with
              | `Var (l, _, `Rigid) ->
                  raise (Failure (`Msg ("Couldn't unify the rigid presence variable "^
                                          string_of_int l ^" with the presence flag "^
                                          string_of_presence f)))
              | `Var (var, subkind, `Flexible) ->
                begin
                  match f with
                  | `Absent ->
                    Unionfind.change point (`Body `Absent)
                  | `Present t ->
                    (* HACK: this ensures that any recursion is
                       confined to ordinary types inside presence
                       types; hence we never need recursive presence
                       types *)
                    let tv = Types.fresh_type_variable subkind in
                    Unionfind.change point (`Body (`Present tv));
                    unify' rec_env (tv, t)
                    (* let [q] = Types.quantifiers_of_type_args [`Presence (`Var point)] in *)
                    (* let t' = Instantiate.apply_type (Types.for_all ([q], t)) [`Presence (`Present tv)] in *)
                    (* unify' rec_env (tv, t'); *)
                    (* Unionfind.change point (`Body (`Present tv)) *)
                end
              | `Body f' -> unify_presence' rec_env (f, f')
          end
      | _, _ ->
          raise (Failure (`Msg ("Couldn't match "^ string_of_presence l ^" against "^ string_of_presence r)))

and unify_rows' : unify_env -> ((row * row) -> unit) =
  let unwrap_row r =
    let r', rvar = unwrap_row r in
    (* Debug.print (Printf.sprintf "Unwrapped row %s giving %s\n" (string_of_row r) (string_of_row r')); *)
    r', rvar in

  fun rec_env (lrow, rrow) ->
    Debug.if_set (show_row_unification) (fun () -> "Unifying row: " ^ (string_of_row lrow) ^ " with row: " ^ (string_of_row rrow));

    (*
       NOTE:

       - All calls to fail_on_absent_fields are currently disabled,
       as under the current model absent fields have
       to be allowed in closed rows (although they're ignored).

       - There's no way of getting rid of absent variables as they're stored in the field
       environment rather than the row variable (good argument for moving them into the
       row variable).
    *)
    (*
      let fail_on_absent_fields field_env =
      StringMap.iter
      (fun _ -> function
      | `Present _ -> ()
      | `Absent ->
      failwith "Internal error: closed row with absent variable"
      ) field_env in
    *)

    let is_unguarded_recursive row =
      let rec is_unguarded rec_rows (field_env, row_var, _) =
        StringMap.is_empty field_env &&
          (match Unionfind.find row_var with
             | `Closed
             | `Var _ -> false
             | `Recursive (var, row) when IntSet.mem var rec_rows -> true
             | `Recursive (var, row) -> is_unguarded (IntSet.add var rec_rows) row
             | `Body row -> is_unguarded rec_rows row)
      in
        is_unguarded IntSet.empty row in

    (* optimisation? *)
    let strip_absent : field_spec_map -> field_spec_map =
      StringMap.filter
        (fun label f ->
          match Types.concrete_field_spec f with
            | `Absent -> false
            | _ -> true) in

    (* extend_field_env env0 env1

       For every label l in env0:
       if l is in env1 then unify env0(l) with env1(l)
       else add l |-> env0(l) to the extension environment

       Return the extension environment
    *)
    let extend_field_env : bool -> unify_env -> field_spec_map -> field_spec_map -> field_spec_map =
      fun closed rec_env env0 env1 ->
        StringMap.fold
          (fun label field_spec extension ->
            if StringMap.mem label env1 then
              let f = field_spec in
              let f' = StringMap.find label env1 in
                (* Because unification is imperative the following
                   might lead to somewhat surprising
                   behaviour. Suppose f = f' = `Absent, t = (a, Bool)
                   and t' = (Int, Int). Then

                   (a,Bool) `unify` (Int,Int)

                   succeeds in unifying a with Int. But Bool can't be
                   unified with Int, so we're left with (`Absent,
                   (Int, Bool)) on the left.

                   Arguably, this doesn't really matter though, as
                   absent labels in closed rows are always redundant
                   anyway. *)
                try
                  unify_presence' rec_env (f, f');
                  extension
                with Failure e ->
                  if closed then
                    begin
                      (* if both rows are closed then absence is all we need *)
                      try
                        unify_presence' rec_env (f, `Absent);
                        unify_presence' rec_env (f', `Absent);
                        extension
                      with Failure _ ->
                        raise (Failure e)
                    end
                  else
                    raise (Failure e)
            else
              StringMap.add label field_spec extension
          ) env0 (StringMap.empty) in

    (* unify_field_envs closed rec_env (lenv, renv)

       unify lenv with renv

       The closed argument should be set to true when both
       environments arise from closed rows. In this case missing
       labels may be treated as absent *)
    let unify_field_envs : bool -> unify_env -> (field_spec_map * field_spec_map) -> unit =
      fun closed rec_env (lenv, renv) ->
        let aux env1 env2 =
          StringMap.iter
            (fun label f ->
              if StringMap.mem label env2 then
                let f' = StringMap.find label env2 in
                  try
                    unify_presence' rec_env (f, f')
                  with
                      Failure e ->
                        begin
                          if closed then
                            begin
                              (* if both rows are closed then absence is all we need *)
                              try
                                unify_presence' rec_env (f, `Absent);
                                unify_presence' rec_env (f', `Absent)
                              with Failure _ ->
                                raise (Failure e)
                            end
                          else
                            raise (Failure e)
                        end
              else
                match f with
                  | (`Absent | `Var _) when closed ->
                    (* closed rows don't need to explicitly mention absent *)
                    unify_presence' rec_env (f, `Absent)
                  | _ ->
                    raise (Failure (`Msg ("Field environments\n "^ string_of_row (lenv, closed_row_var, false)
                                          ^"\nand\n "^ string_of_row (renv, closed_row_var, false)
                                          ^"\n could not be unified because they have different fields")))
            ) env1 in
        let lenv, renv =
          if closed then
            strip_absent lenv, strip_absent renv
          else
            lenv, renv
        in
          aux lenv renv;
          aux renv lenv in

    (* introduce a recursive row
       give an error if it is non-well-founded and
       non-well-founded type inference is switched off
    *)
    let rec_row_intro point (var, row) =
      if occurs_check_row var row then
        Unionfind.change point (`Recursive (var, row))
      else
        raise (Failure (`Msg ("Cannot unify row variable "^string_of_int var^" with row "^string_of_row row^
                                " because "^
                                match Settings.get_value infer_recursive_types with
                                  | "guarded" -> "the row variable occurs unguarded inside the row"
                                  | "positive" -> "the row variable occurs in a negative position inside the row"
                                  | _ -> assert false))) in

    (*
      unify_row_var_with_row rec_env (row_var, row)
      attempts to unify row_var with row

      However, row_var may already have been instantiated, in which case
      it is unified with row.
    *)
    let unify_row_var_with_row : unify_env -> row_var * bool * row -> unit =
      fun rec_env (row_var, dual, ((extension_field_env, extension_row_var, extension_dual) as extension_row)) ->
        (* unify row_var with `RowVar None *)
        let close_empty_row_var : row_var -> unit = fun point ->
          match Unionfind.find point with
            | `Closed -> ()
            | `Var (_, _, `Flexible) ->
                Unionfind.change point `Closed
            | `Var (_, _, `Rigid) ->
                raise (Failure (`Msg ("Closed row var cannot be unified with rigid row var\n")))
            | _ -> assert false in

        (* TODO: do we need to do something special here for the session subkind? *)

        (* unify row_var with `RigidRowVar var *)
        let rigidify_empty_row_var (point, (var, (lin, rest))) : row_var -> unit = fun point' ->
          match Unionfind.find point' with
            | `Closed ->
                raise (Failure (`Msg ("Rigid row var cannot be unified with empty closed row\n")))
            | `Var (_, (lin', rest'), `Flexible) ->
                if rest = `Any && rest' = `Base then
                  raise (Failure (`Msg ("Rigid non-base row var cannot be unified with empty base row\n")));
                Unionfind.change point' (`Var (var, (lin, rest), `Rigid))
            | `Var (var', _, `Rigid) when var=var' -> ()
            | `Var (var', (_, rest'), `Rigid) when rest=rest' && compatible_quantifiers (var, var') rec_env.qstack ->
              Unionfind.union point point'
            | `Var (var', _, `Rigid) ->
                raise (Failure (`Msg ("Incompatible rigid row variables cannot be unified\n")))
            | _ -> assert false in

        let extend = fun point ->
          (* point should be a row variable *)
          match Unionfind.find point with
            | `Closed ->
              if is_empty_row extension_row then
                close_empty_row_var extension_row_var
              else
                raise (Failure (`Msg ("Closed row cannot be extended with non-empty row\n"
                                      ^string_of_row extension_row)))
            | `Var (var, subkind, `Rigid) ->
              if is_empty_row extension_row then
                rigidify_empty_row_var (point, (var, subkind)) extension_row_var
              else
                raise (Failure (`Msg ("Rigid row variable cannot be unified with non-empty row\n"
                                              ^string_of_row extension_row)))
            | `Var (var, (lin, rest), `Flexible) ->
              if TypeVarSet.mem var (free_row_type_vars extension_row) then
                begin
                  if rest = `Base then
                    raise (Failure (`Msg ("Cannot infer a recursive type for the base row variable "^ string_of_int var ^
                                             " with the body "^ string_of_row extension_row)));
                  rec_row_intro point (var, extension_row)
                end
              else
                begin
                  if rest = `Base then
                    if Types.is_baseable_row extension_row then
                      Types.basify_row extension_row
                    else
                      raise (Failure (`Msg ("Cannot unify the base row variable "^ string_of_int var ^
                                               " with the non-base row "^ string_of_row extension_row)));
                  if rest = `Session then
                    if Types.is_sessionable_row extension_row then
                      Types.sessionify_row extension_row
                    else
                      raise (Failure (`Msg ("Cannot unify the session row variable "^ string_of_int var ^
                                               " with the non-session row "^ string_of_row extension_row)));

                  if lin = `Unl then
                    if Types.row_can_be_unl extension_row then
                      Types.make_row_unl extension_row
                    else
                      raise (Failure (`Msg ("Cannot force row " ^ string_of_row extension_row ^ " to be unlimited")));

                  if StringMap.is_empty extension_field_env then
                    if dual then
                      Unionfind.change point (`Body (StringMap.empty, extension_row_var, true))
                    else
                      Unionfind.union point extension_row_var
                  else
                    if dual then
                      Unionfind.change point (`Body (dual_row extension_row))
                    else
                      Unionfind.change point (`Body extension_row)
                end
            | `Recursive _ ->
              unify_rows' rec_env ((StringMap.empty, point, dual), extension_row)
            | `Body row ->
              unify_rows' rec_env ((if dual then dual_row row else row), extension_row)
        in
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

    let row_without_labels : StringSet.t -> row -> row =
      fun labels (field_env, row_var, dual) ->
        let restricted_field_env =
          StringSet.fold (fun label field_env ->
                            StringMap.remove label field_env) labels field_env
        in
          (restricted_field_env, row_var, dual) in

    (*
      register a recursive row in the rec_env environment

      return:
      None if the recursive row already appears in the environment
      Some rec_env, otherwise, where rec_env is the updated environment
    *)
    let register_rec_row (wrapped_field_env, unwrapped_field_env, rec_row, unwrapped_row') : unify_env -> unify_env option =
      fun rec_env ->
        let rec_types = rec_env.tenv in
        let rec_rows = rec_env.renv in
          match rec_row with
            | Some row_var ->
                begin
                  match Unionfind.find row_var with
                    | `Recursive (var, _) ->
                        let restricted_row = row_without_labels (matching_labels (unwrapped_field_env, wrapped_field_env)) unwrapped_row' in
                        let rs =
                          if IntMap.mem var rec_rows then
                            IntMap.find var rec_rows
                          else
                            [(StringMap.empty, row_var, false)]
                        in
                          if List.exists (fun r ->
                                            eq_rows (r, restricted_row)) rs then
                            None
                          else
                            Some {rec_env with renv=IntMap.add var (restricted_row::rs) rec_rows}
                    | _ -> assert false
                end
            | None ->
                Some rec_env in

    (*
      register two recursive rows and return None if one of them is already in the environment
    *)
    let register_rec_rows p1 p2 : unify_env -> unify_env option = fun rec_env ->
      let rec_env' = register_rec_row p1 rec_env in
        match rec_env' with
          | None -> None
          | Some rec_env -> register_rec_row p2 rec_env in

    let unify_both_rigid_with_rec_env rec_env ((lfield_env, _, _ as lrow), (rfield_env, _, _ as rrow)) =
      let (lfield_env', lrow_var', ldual') as lrow', lrec_row = unwrap_row lrow in
      let (rfield_env', rrow_var', rdual') as rrow', rrec_row = unwrap_row rrow in
        (*
          fail_on_absent_fields lfield_env;
          fail_on_absent_fields rfield_env;
        *)
      let closed =
        begin
          match Unionfind.find lrow_var', Unionfind.find rrow_var' with
            | `Var (_, _, `Flexible), _ | _, `Var (_, _, `Flexible) ->
              assert false (* both row variables must be rigid! *)
            | `Recursive _, _ | _, `Recursive _ ->
              assert false (* the rows must be unwrapped *)
            | `Closed, `Closed -> true
            | `Var (lvar, lkind, `Rigid), `Var (rvar, rkind, `Rigid) when lkind <> rkind ->
              raise (Failure (`Msg ("Rigid rows\n "^ string_of_row lrow
                                    ^"\nand\n "^ string_of_row rrow
                                    ^"\n could not be unified because they have different kinds")))
            | `Var (lvar, lkind, `Rigid), `Var (rvar, rkind, `Rigid) when
                                                           (lvar=rvar ||
                                                            compatible_quantifiers (lvar, rvar) rec_env.qstack) ->
              Unionfind.union lrow_var' rrow_var'; false
            | `Var (lvar, _, `Rigid), `Var (rvar, _, `Rigid) ->
              raise (Failure (`Msg ("Rigid rows\n "^ string_of_row lrow
                                    ^"\nand\n "^ string_of_row rrow
                                    ^"\n could not be unified because they have distinct rigid row variables")))
            | `Var (_, _, `Rigid), `Closed
            | `Closed, `Var (_, _, `Rigid) ->
              raise (Failure (`Msg ("Rows\n "^ string_of_row lrow
                                    ^"\nand\n "^ string_of_row rrow
                                    ^"\n could not be unified because one is closed and the other has a rigid row variable")))
        end in
(*        if fields_are_compatible (lrow', rrow') then *)
      let rec_env' =
        (register_rec_rows
           (lfield_env, lfield_env', lrec_row, rrow')
           (rfield_env, rfield_env', rrec_row, lrow')
           rec_env)
      in
        match rec_env' with
          | None -> ()
          | Some rec_env ->
            unify_field_envs closed rec_env (lfield_env', rfield_env') in
(*                unify_compatible_field_environments rec_env (lfield_env', rfield_env') *)
(*
        else
          raise (Failure (`Msg ("Rigid rows\n "^ string_of_row lrow
                                ^"\nand\n "^ string_of_row rrow
                                ^"\n could not be unified because they have different fields"))) in *)

    let unify_both_rigid (lrow, rrow) =
      (* Debug.if_set (show_row_unification) *)
      (*   (fun () -> "(both rigid rows)"); *)
      unify_both_rigid_with_rec_env rec_env (lrow, rrow) in

    let unify_one_rigid ((rigid_field_env, _, _ as rigid_row), (open_field_env, _, _ as open_row)) =
      let (rigid_field_env', rigid_row_var', rigid_dual') as rigid_row', rigid_rec_row = unwrap_row rigid_row in
      let closed = is_closed_row rigid_row' in
      let (open_field_env', open_row_var', open_dual) as open_row', open_rec_row = unwrap_row open_row in
        (* check that the open row contains no extra fields *)
        StringMap.iter
          (fun label f ->
             if (StringMap.mem label rigid_field_env') then
               ()
             else
               match f with
                 | (`Absent | `Var _) when closed ->
                   (* closed rows don't need to explicitly mention absent *)
                   unify_presence' rec_env (f, `Absent)
                 | _ ->
                     raise (Failure
                              (`Msg
                                 ("Rows\n "^ string_of_row rigid_row
                                  ^"\nand\n "^ string_of_row open_row
                                  ^"\n could not be unified because the former is rigid"
                                  ^" and the latter contains fields not present in the former, namely `"
                                  ^ label ^"'.")))
          ) open_field_env';

        let rec_env' =
          (register_rec_rows
             (rigid_field_env, rigid_field_env', rigid_rec_row, open_row')
             (open_field_env, open_field_env', open_rec_row, rigid_row')
             rec_env)
        in
          match rec_env' with
            | None -> ()
            | Some rec_env ->
                let open_extension = extend_field_env closed rec_env rigid_field_env' open_field_env' in
                  unify_row_var_with_row rec_env (open_row_var', open_dual, (open_extension, rigid_row_var', rigid_dual')) in

    let unify_both_open ((lfield_env, _, ldual as lrow), (rfield_env, _, rdual as rrow)) =
      let (lfield_env', lrow_var', ldual') as lrow', lrec_row = unwrap_row lrow in
      let (rfield_env', rrow_var', rdual') as rrow', rrec_row = unwrap_row rrow in
      let rec_env' =
        (register_rec_rows
           (lfield_env, lfield_env', lrec_row, rrow')
           (rfield_env, rfield_env', rrec_row, lrow')
           rec_env)
      in
        match rec_env' with
          | None -> ()
          | Some rec_env ->
              if get_row_var lrow = get_row_var rrow && ldual = rdual then
                unify_both_rigid_with_rec_env rec_env ((lfield_env', Unionfind.fresh `Closed, false),
                                                       (rfield_env', Unionfind.fresh `Closed, false))
              else
                begin
                  let fresh_row_var = fresh_row_variable (`Any, `Any) in
                    (* each row can contain fields missing from the other;
                       thus we call extend_field_env once in each direction *)
                  let rextension =
                    extend_field_env false rec_env lfield_env' rfield_env' in
                    (* NOTE:
                       extend_field_env may change rrow_var' or lrow_var', as either
                       could occur inside the body of lfield_env' or rfield_env'
                    *)
                    unify_row_var_with_row rec_env (rrow_var', rdual', (rextension, fresh_row_var ,false));
                    let lextension = extend_field_env false rec_env rfield_env' lfield_env' in
                      unify_row_var_with_row rec_env (lrow_var', ldual', (lextension, fresh_row_var, false))
                end in

    (* report an error if an attempt is made to unify
       an unguarded recursive row with a row that is not
       unguarded recursive
    *)
    let check_unguarded_recursion lrow rrow =
      if is_unguarded_recursive lrow then
        if not (is_unguarded_recursive rrow) then
          raise (Failure
                   (`Msg ("Could not unify unguarded recursive row"^ string_of_row lrow
                          ^"\nwith row "^ string_of_row rrow)))
        else if is_unguarded_recursive rrow then
          raise (Failure
                   (`Msg ("Could not unify unguarded recursive row"^ string_of_row rrow
                          ^"\nwith row "^ string_of_row lrow))) in

    let _ =
      check_unguarded_recursion lrow rrow;

      if is_rigid_row lrow then
        if is_rigid_row rrow then
          unify_both_rigid (lrow, rrow)
        else
          unify_one_rigid (lrow, rrow)
      else if is_rigid_row rrow then
        unify_one_rigid (rrow, lrow)
      else
        unify_both_open (rrow, lrow)
    in
      Debug.if_set (show_row_unification)
        (fun () -> "Unified rows: " ^ (string_of_row lrow) ^ " and: " ^ (string_of_row rrow))

and unify_type_args' : unify_env -> (type_arg * type_arg) -> unit =
  fun rec_env ->
    function
      | `Type lt, `Type rt -> unify' rec_env (lt, rt)
      | `Row lr, `Row rr -> unify_rows' rec_env (lr, rr)
      | `Presence lf, `Presence rf -> unify_presence' rec_env (lf, rf)
      | l, r ->
          raise (Failure (`Msg ("Couldn't match "^ string_of_type_arg l ^" against "^ string_of_type_arg r)))

let unify (t1, t2) =
  unify' {tenv=IntMap.empty; renv=IntMap.empty; qstack=(0, IntMap.empty, IntMap.empty)} (t1, t2)
(* Debug.if_set (show_unification) (fun () -> "Unified types: " ^ string_of_datatype t1) *)
and unify_rows (row1, row2) =
  unify_rows' {tenv=IntMap.empty; renv=IntMap.empty; qstack=(0, IntMap.empty, IntMap.empty)} (row1, row2)

(* external interface *)
let datatypes = unify
let rows = unify_rows
