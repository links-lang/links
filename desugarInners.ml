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
            | `TypeVar (_, point)
            | `RigidTypeVar (_, point) -> `Type (`MetaTypeVar point) :: add_extras (extras, tyargs)
            | `RowVar (_, row_var)
            | `RigidRowVar (_, row_var) -> `Row (StringMap.empty, row_var) :: add_extras (extras, tyargs)
        end


class desugar_inners {Types.var_env=var_env; Types.tycon_env=tycon_env} =
object (o : 'self_type)
  inherit (TransformSugar.transform (var_env, tycon_env)) as super

  val extra_env = Env.String.empty

  method with_extra_env env =
    {< extra_env = env >}

  method bind f extras =
    {< extra_env = Env.String.bind extra_env (f, extras) >}

  method phrasenode = function
    | `TAppl (((`Var name), pos), tyargs) when Env.String.has extra_env name ->
        let extras = Env.String.lookup extra_env name in
        let tyargs = add_extras (extras, tyargs) in
          super#phrasenode (`TAppl (((`Var name), pos), tyargs))
    | `InfixAppl ((tyargs, `Name name), e1, e2) when Env.String.has extra_env name ->
        let extras = Env.String.lookup extra_env name in
        let tyargs = add_extras (extras, tyargs) in
          super#phrasenode (`InfixAppl ((tyargs, `Name name), e1, e2))
    | `UnaryAppl ((tyargs, `Name name), e) when Env.String.has extra_env name ->
        let extras = Env.String.lookup extra_env name in
        let tyargs = add_extras (extras, tyargs) in
          super#phrasenode (`UnaryAppl ((tyargs, `Name name), e))
    | e -> super#phrasenode e
        
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

        (* backup the extras environment *)
        let extra_env = extra_env in

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
                  let inner_mb = TransformSugar.fun_mailbox inner (fst lam) in
                  let (o, lam, _) = o#funlit inner_mb lam in
                  let (o, defs) = list o defs in
                  let extras = List.map (fun _ -> None) extras in
                    (o, ((f, Some outer, fpos), ((tyvars, Some (outer, extras)), lam), location, t, pos)::defs)
          in
            list o defs in

        (* restore the extras environment *)
        let o = o#with_extra_env extra_env in
          (o, (`Funs defs))
    | b -> super#bindingnode b
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
