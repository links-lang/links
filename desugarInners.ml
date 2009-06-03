open Utility
open Sugartypes

(*
  
*)

let dp = Sugartypes.dummy_position

let rec add_extras =
  function
    | [], [] -> []
    | None::extras, tyarg::tyargs -> tyarg :: add_extras (extras, tyargs)
    | Some q::extras, tyargs ->
        begin
          match q with
            | `TypeVar (_, point) -> `Type (`MetaTypeVar point) :: add_extras (extras, tyargs)
            | `RowVar (_, row_var) -> `Row (StringMap.empty, row_var) :: add_extras (extras, tyargs)
            | `PresenceVar (_, point) -> `Presence (`Var point) :: add_extras (extras, tyargs)
        end

class desugar_inners env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  val extra_env = StringMap.empty

  method with_extra_env env =
    {< extra_env = env >}

  method bind f extras =
    {< extra_env = StringMap.add f extras extra_env >}

  method unbind f =
    {< extra_env = StringMap.remove f extra_env >}

  method phrasenode = function
    | `TAppl (((`Var name), pos), tyargs) when StringMap.mem name extra_env ->
        let extras = StringMap.find name extra_env in
        let tyargs = add_extras (extras, tyargs) in
          super#phrasenode (`TAppl (((`Var name), pos), tyargs))
    | `InfixAppl ((tyargs, `Name name), e1, e2) when StringMap.mem name extra_env ->
        let extras = StringMap.find name extra_env in
        let tyargs = add_extras (extras, tyargs) in
          super#phrasenode (`InfixAppl ((tyargs, `Name name), e1, e2))
    | `UnaryAppl ((tyargs, `Name name), e) when StringMap.mem name extra_env ->
        let extras = StringMap.find name extra_env in
        let tyargs = add_extras (extras, tyargs) in
          super#phrasenode (`UnaryAppl ((tyargs, `Name name), e))         
    (* HACK: manage the lexical scope of extras *)
    | `Spawn _ as e ->
        let extra_env = extra_env in
        let (o, e, t) = super#phrasenode e in
          (o#with_extra_env extra_env, e, t)
    | `SpawnWait _ as e ->
        let extra_env = extra_env in
        let (o, e, t) = super#phrasenode e in
          (o#with_extra_env extra_env, e, t)
    | `Escape _ as e ->
        let extra_env = extra_env in
        let (o, e, t) = super#phrasenode e in
          (o#with_extra_env extra_env, e, t)
    | `Block _ as e ->
        let extra_env = extra_env in
        let (o, e, t) = super#phrasenode e in
          (o#with_extra_env extra_env, e, t)
    | e -> super#phrasenode e
        
  method funlit =
    (* HACK: manage the lexical scope of extras *)
    fun inner_mb lam ->
      let extra_env = extra_env in
      let (o, lam, t) = super#funlit inner_mb lam in
        (o#with_extra_env extra_env, lam, t)

  method bindingnode = function
    | `Funs defs as b ->
        let bt (name, _, pos) t = (name, Some t, pos) in

        (* put the outer bindings in the environment *)
        let o, defs =
          let rec list o =
            function
              | [] -> o, []
              | (f, body, location, t, pos)::defs ->
                  let (o, f) = o#binder f in
                  let (o, defs) = list o defs in
                    o, (f, body, location, t, pos)::defs
          in
            list o defs in

        (* put the extras in the environment *)
        let o =
          List.fold_left
            (fun o ((f, _, _), ((_tyvars, Some (_, extras)), _), _, _, _) ->
               o#bind f extras)
            o defs in
          
        (* transform the function bodies *)
        let (o, defs) =
          let rec list o =
            function
              | [] -> (o, [])
              | ((f, Some outer, fpos), ((tyvars, Some (inner, extras)), lam), location, t, pos)::defs ->
                  let inner_eff = TransformSugar.fun_effects inner (fst lam) in
                  let (o, lam, _) = o#funlit inner_eff lam in
                  let (o, defs) = list o defs in
                  let extras = List.map (fun _ -> None) extras in
                    (o, ((f, Some outer, fpos), ((tyvars, Some (outer, extras)), lam), location, t, pos)::defs)
          in
            list o defs in

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
            (fun o ((f, _, _), ((_tyvars, _), _), _, _, _) ->
               o#unbind f)
            o defs
        in
          (o, (`Funs defs))
    | b -> super#bindingnode b

  method binder : binder -> ('self_type * binder) =
    fun (name, Some t, pos) ->
      let var_env = Env.String.bind var_env (name, t) in
        ({< var_env=var_env; extra_env=extra_env >}, (name, Some t, pos))
end

let desugar_inners env = ((new desugar_inners env) : desugar_inners :> TransformSugar.transform)

let has_no_inners =
object
  inherit SugarTraversals.predicate as super

  val has_no_inners = true
  method satisfied = has_no_inners

  method bindingnode = function
    | `Funs defs ->
        {< has_no_inners =
            List.for_all
              (fun (_f, ((_tyvars, Some (_inner, extras)), _), _, _, _) ->
                 List.for_all (function
                                 | None -> true
                                 | Some _ -> false) extras)
              defs >}
    | b -> super#bindingnode b
end
