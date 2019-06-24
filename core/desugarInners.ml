open Utility
open Operators
open SourceCode.WithPos
open Sugartypes

(* Recursive functions must be used monomorphically inside their
   function bodies (unless annotated with a polymorphic type), but
   they may still be used polymorphically outside (after having been
   generalised). This pass unifies the inner and outer types of
   functions, which is necessary for converting to the IR, which
   stores a single type for each recursive function.
*)

(* FIXME: replace failwith *)

let rec add_extras qs (extras, tyargs) =
  match qs, extras with
  | [], [] -> []
  | q::qs, None::extras -> Types.type_arg_of_quantifier q :: add_extras qs (extras, tyargs)
  | _::qs, Some i::extras -> List.nth tyargs i :: add_extras qs (extras, tyargs)
  | _, _ -> failwith "Mismatch in number of quantifiers and type arguments"

(* let add_extras pos extras =
 *   let rec go = function
 *     | [], [] -> []
 *     | None::extras, tyarg::tyargs -> tyarg :: go (extras, tyargs)
 *     | Some q::extras, tyargs ->
 *       (Types.type_arg_of_quantifier q) :: go (extras, tyargs)
 *     | _, _ ->
 *       raise (Errors.desugaring_error
 *         ~pos
 *         ~stage:Errors.DesugarInners
 *         ~message:"Mismatch in number of quantifiers and type arguments") in
 *   go extras *)

class desugar_inners env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  val extra_env = StringMap.empty

  method with_extra_env env =
    {< extra_env = env >}

  method bind f tyvars extras =
    {< extra_env = StringMap.add f (tyvars, extras) extra_env >}

  method unbind f =
    {< extra_env = StringMap.remove f extra_env >}

  method! phrasenode = function
    | (Var name as e)
    | (Section (Section.Name name) as e) when StringMap.mem name extra_env ->
       let tyvars, extras = StringMap.find name extra_env in
       let tyargs = add_extras tyvars (extras, []) in
       let o = o#unbind name in
       let (o, e, t) = o#phrasenode (TAppl (SourceCode.WithPos.make e, tyargs)) in
       (o#with_extra_env extra_env, e, t)
    | TAppl ({node=Var name;_} as p, tyargs)
    | TAppl ({node=Section (Section.Name name);_} as p, tyargs) when StringMap.mem name extra_env ->
        let tyvars, extras = StringMap.find name extra_env in
        let tyargs = add_extras tyvars (extras, tyargs) in
        let o = o#unbind name in
        let (o, e, t) = o#phrasenode (TAppl (p, tyargs)) in
        (o#with_extra_env extra_env, e, t)
    | InfixAppl ((tyargs, BinaryOp.Name name), e1, e2) when StringMap.mem name extra_env ->
        let tyvars, extras = StringMap.find name extra_env in
        let tyargs = add_extras tyvars (extras, tyargs) in
          super#phrasenode (InfixAppl ((tyargs, BinaryOp.Name name), e1, e2))
    | UnaryAppl ((tyargs, UnaryOp.Name name), e) when StringMap.mem name extra_env ->
        let tyvars, extras = StringMap.find name extra_env in
        let tyargs = add_extras tyvars (extras, tyargs) in
          super#phrasenode (UnaryAppl ((tyargs, UnaryOp.Name name), e))
    (* HACK: manage the lexical scope of extras *)
    | Spawn _ as e ->
        let (o, e, t) = super#phrasenode e in
          (o#with_extra_env extra_env, e, t)
    | Escape _ as e ->
        let (o, e, t) = super#phrasenode e in
          (o#with_extra_env extra_env, e, t)
    | Block _ as e ->
        let (o, e, t) = super#phrasenode e in
          (o#with_extra_env extra_env, e, t)
    | e -> super#phrasenode e

(* FIXME: consider refactoring the above code along the lines of the (out-dated) below *)

  (* method! phrase { node; pos } =
   *   let add_extras = add_extras pos in
   *   let (o, e, t) =
   *     match node with
   *       | TAppl ({node=Var name;_} as phn, tyargs) when StringMap.mem name extra_env ->
   *           let extras = StringMap.find name extra_env in
   *           let tyargs = add_extras (extras, tyargs) in
   *             super#phrasenode (TAppl (phn, tyargs))
   *       | InfixAppl ((tyargs, BinaryOp.Name name), e1, e2) when StringMap.mem name extra_env ->
   *           let extras = StringMap.find name extra_env in
   *           let tyargs = add_extras (extras, tyargs) in
   *             super#phrasenode (InfixAppl ((tyargs, BinaryOp.Name name), e1, e2))
   *       | UnaryAppl ((tyargs, UnaryOp.Name name), e) when StringMap.mem name extra_env ->
   *           let extras = StringMap.find name extra_env in
   *           let tyargs = add_extras (extras, tyargs) in
   *             super#phrasenode (UnaryAppl ((tyargs, UnaryOp.Name name), e))
   *               (\* HACK: manage the lexical scope of extras *\)
   *       | Spawn _ as e ->
   *           let (o, e, t) = super#phrasenode e in
   *             (o#with_extra_env extra_env, e, t)
   *       | Escape _ as e ->
   *           let (o, e, t) = super#phrasenode e in
   *             (o#with_extra_env extra_env, e, t)
   *       | Block _ as e ->
   *           let (o, e, t) = super#phrasenode e in
   *             (o#with_extra_env extra_env, e, t)
   *       | e -> super#phrasenode e in
   *   (o, SourceCode.WithPos.make ~pos e, t) *)

  method! funlit =
    (* HACK: manage the lexical scope of extras *)
    fun inner_mb lam ->
    let (o, lam, t) = super#funlit inner_mb lam in
    (o#with_extra_env extra_env, lam, t)

  method! bindingnode = function
    | Funs defs ->
        (* put the outer bindings in the environment *)
        let o, defs = o#rec_activate_outer_bindings defs in

        (* put the extras in the environment *)
        let o =
          List.fold_left
            (fun o { rec_binder = bndr; rec_definition = ((tyvars, dt_opt), _); _ } ->
               match dt_opt with
                 | Some (_, extras) -> o#bind (Binder.to_name bndr) tyvars extras
                 | None -> assert false
            )
            o defs in

        (* unify inner and outer types for each def *)
        let (o, defs) =
          let rec list o =
            function
              | [] -> (o, [])
              | ({ rec_binder = bndr;
                   rec_definition = ((tyvars, Some (_inner, extras)), lam);
                   _ } as fn) :: defs ->
                 let outer = Binder.to_type bndr in
                 let (o, defs) = list o defs in
                 let extras = List.map (fun _ -> None) extras in
                 (o, { fn with rec_definition = ((tyvars, Some (outer, extras)), lam) } :: defs)
              | _ -> assert false
          in
            list o defs in

        (* transform the function bodies *)
        let (o, defs) = o#rec_bodies defs in

        (*
           It is important to explicitly remove the extras from the
           environment as any existing functions with the same name
           will now be shadowed by the functions defined in this
           binding - and hence will not need any extra type variables
           adding.
        *)
        (* remove the extras from the environment *)
        let o =
          List.fold_left
            (fun o fn -> o#unbind (Binder.to_name fn.rec_binder))
            o defs
        in
          (o, (Funs defs))
    | b -> super#bindingnode b

  method! binder =
    fun bndr ->
    let (o, bndr) = super#binder bndr in
    (* avoid accidentally capturing type applications through shadowing *)
    let o = o#unbind (Binder.to_name bndr) in
    (o, bndr)

  (* method! binder : Binder.with_pos -> ('self_type * Binder.with_pos) = function
   *     | {node=_, None; _} -> assert false
   *     | bndr ->
   *        let var_env = Env.String.bind var_env (Binder.to_name bndr, Binder.to_type_exn bndr) in
   *        ({< var_env=var_env; extra_env=extra_env >}, bndr) *)
end

let desugar_inners env = ((new desugar_inners env) : desugar_inners :> TransformSugar.transform)

let desugar_program : TransformSugar.program_transformer =
  fun env program -> snd3 ((desugar_inners env)#program program)

let desugar_sentence : TransformSugar.sentence_transformer =
  fun env sentence -> snd ((desugar_inners env)#sentence sentence)

let has_no_inners =
object
  inherit SugarTraversals.predicate as super

  val has_no_inners = true
  method satisfied = has_no_inners

  method! bindingnode = function
    | Funs defs ->
        {< has_no_inners =
            List.for_all
              (fun { rec_definition = ((_, dt_opt), _); _ } ->
                 match dt_opt with
                    | None -> assert false
                    | Some (_inner, extras) ->
                         List.for_all (function
                                         | None -> true
                                         | Some _ -> false) extras)
              defs >}
    | b -> super#bindingnode b
end
