open Utility
open Sugartypes

let rec is_raw (phrase, pos) =
  match phrase with
    | `TextNode _ -> true
    | `Block _    -> true
    | `FormBinding _ -> false
    | `Xml (_, _, _, children) ->
        List.for_all is_raw children
    | _ ->
        raise (Errors.SugarError (pos, "Invalid element in formlet literal"))

let tt =
  function
    | [t] -> t
    | ts -> Types.make_tuple_type ts

class desugar_formlets env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  (*
    extract a list of (pattern, constructor, type) triples
    from a formlet body

    (this roughly corresponds to the dagger transformation)
  *)
  method formlet_patterns : Sugartypes.phrase -> (Sugartypes.pattern list * Sugartypes.phrase list * Types.datatype list) =
    fun (e, pos) ->
      let dp = Sugartypes.dummy_position in
      match e with
        | _ when is_raw (e, pos) ->
            [(`Tuple []), dp], [(`TupleLit []), dp], [Types.unit_type]
        | `FormBinding (f, p) ->
            let (_o, _f, ft) = o#phrase f in
            let t = Types.fresh_type_variable (`Any, `Any) in
            let () =
              Unify.datatypes
                (ft, Instantiate.alias "Formlet" [`Type t] tycon_env) in
            let var = Utility.gensym ~prefix:"_formlet_" () in
            let (xb, x) = (var, Some t, dp), ((`Var var), dp) in
              [(`As (xb, p)), dp], [x], [t]
        | `Xml (_, _, _, [node]) ->
            o#formlet_patterns node
        | `Xml (_, _, _, contents) ->
            let ps, vs, ts =
              List.fold_left
                (fun (ps, vs, ts) e ->
                   let ps', vs', ts' = o#formlet_patterns e in
                     match ps', vs', ts' with
                       | [p], [v], [t] -> p::ps, v::vs, t::ts
                       | _ ->
                           ((`Tuple ps'), dp)::ps, ((`TupleLit vs'), dp)::vs, (Types.make_tuple_type ts')::ts)
                ([], [], []) contents
            in
              List.rev ps, List.rev vs, List.rev ts
        | _ ->
            assert false

  (* desugar a formlet body (the ^o transformation) *)
  method private formlet_body_node : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) =
    fun e ->
      let dp = Sugartypes.dummy_position in
        match e with
          | `TextNode s ->
              let e =
                `FnAppl
                  ((`TAppl ((`Var "xml", dp), [`Row (o#lookup_effects)]), dp),
                   [`FnAppl
                      ((`TAppl ((`Var "stringToXml", dp), [`Row (o#lookup_effects)]), dp),
                       [`Constant (`String s), dp]), dp])
              in
                (o, e, Types.xml_type)
          | `Block (bs, e) ->
              let (o, e, _) =
                o#phrasenode
                  (`Block
                     (bs,
                      (`FnAppl
                         ((`TAppl ((`Var "xml", dp), [`Row (o#lookup_effects)]), dp),
                          [e]), dp)))
              in
                (o, e, Types.xml_type)
          | `FormBinding (f, _) ->
              let (o, (f, _), ft) = o#phrase f in
                (o, f, ft)
          | `Xml ("#", [], None, contents) ->
              (* pure (fun ps -> vs) <*> e1 <*> ... <*> ek *)
              let pss, vs, ts =
                let pss, vs, ts =
                  List.fold_left
                    (fun (pss, vs, ts) node ->
                       let ps', vs', ts' = o#formlet_patterns node in
                         match ps', vs', ts' with
                           | [p], [v], [t] ->
                               (* grrr... n-ary arguments are messy!
                                  this type has to be a 1-tuple!
                               *)
                               [p]::pss, v::vs, t::ts
                           | _ ->
                               [`Tuple ps', dp]::pss, (`TupleLit vs', dp)::vs, (Types.make_tuple_type ts')::ts)
                    ([], [], []) contents
                in
                  List.rev pss, List.rev vs, List.rev ts in
              let empty_eff = Types.make_empty_closed_row () in
              let ft =
                List.fold_right
                  (fun t ft ->
                     `Function (Types.make_tuple_type [t], empty_eff, ft))
                  ts (tt ts) in
              let args = List.map (fun t -> (Types.make_tuple_type [t], empty_eff)) ts in
                begin
                  match args with
                    | [] ->
                        let (o, e, _) =
                          super#phrasenode (`Xml ("#", [], None, contents))
                        in
                          (o,
                           (`FnAppl
                              ((`TAppl ((`Var "xml", dp), [`Row (o#lookup_effects)]), dp), [e, dp])),
                           Types.xml_type)
                    | _ ->
                        let (o, es, _) = TransformSugar.list o (fun o -> o#formlet_body) contents in
                        let mb = `Row (o#lookup_effects) in
                        let base : phrase =
                          (`FnAppl
                             ((`TAppl ((`Var "pure", dp), [`Type ft; mb]), dp),
                              [`FunLit (Some (List.rev args), `Unl, (List.rev pss, (`TupleLit vs, dp)), `Unknown), dp]), dp) in
                        let (e, _), et =
                          List.fold_right
                            (fun arg (base, ft) ->
                               let arg_type = List.hd (TypeUtils.arg_types ft) in
                               let ft = TypeUtils.return_type ft in
                               let base : phrase =
                                 (`FnAppl
                                    (((`TAppl (((`Var "@@@"), dp), [`Type arg_type; `Type ft; mb]), dp) : phrase),
                                     [arg; base]), dp)
                               in
                                 base, ft)
                            es (base, ft)
                        in
                          (o, e, et)
                end
          | `Xml(tag, attrs, attrexp, contents) ->
              (* plug (fun x -> (<tag attrs>{x}</tag>)) (<#>contents</#>)^o*)
              let (o, attrexp, _) = TransformSugar.option o (fun o -> o#phrase) attrexp in
              let eff = o#lookup_effects in
              let context : phrase =
                let var = Utility.gensym ~prefix:"_formlet_" () in
                let (xb, x) = (var, Some (Types.xml_type), dp), ((`Var var), dp) in
                  (`FunLit (Some [Types.make_tuple_type [Types.xml_type], eff],
                            `Unl,
                            ([[`Variable xb, dp]],
                             (`Xml (tag, attrs, attrexp, [`Block ([], x), dp]), dp)), `Unknown), dp) in
              let (o, e, t) = o#formlet_body (`Xml ("#", [], None, contents), dp) in
                (o,
                 `FnAppl
                   ((`TAppl ((`Var "plug", dp), [`Type t; `Row eff]), dp),
                    [context; e]),
                 t)
          | _ -> assert false

  method formlet_body : Sugartypes.phrase -> ('self_type * Sugartypes.phrase * Types.datatype) =
    fun (e, pos) ->
      let (o, e, t) = o#formlet_body_node e in (o, (e, pos), t)

  method! phrasenode  : phrasenode -> ('self_type * phrasenode * Types.datatype) = function
    | `Formlet (body, yields) ->
        (* pure (fun q^ -> [[e]]* ) <*> q^o *)
        (* let e_in = `Formlet (body, yields) in *)
        let dp = Sugartypes.dummy_position in
        let empty_eff = Types.make_empty_closed_row () in
        let (ps, _, ts) = o#formlet_patterns body in
        let (o, body, _body_type) = o#formlet_body body in
        let (o, ps) = TransformSugar.listu o (fun o -> o#pattern) ps in
        let (o, yields, yields_type) = o#phrase yields in

        let pss =
          match ps with
            | [p] -> [[p]]
            | _ -> [[`Tuple ps, dp]] in

        let arg_type = Types.make_tuple_type ts in
        let mb = `Row (o#lookup_effects) in

        let e =
          `FnAppl
            ((`TAppl ((`Var "@@@", dp), [`Type arg_type; `Type yields_type; mb]), dp),
             [body;
              `FnAppl
                ((`TAppl ((`Var "pure", dp), [`Type (`Function (Types.make_tuple_type [arg_type], empty_eff, yields_type)); mb]), dp),
                 [`FunLit (Some [Types.make_tuple_type [arg_type], empty_eff], `Unl, (pss, yields), `Unknown), dp]), dp])
        in
          (o, e, Instantiate.alias "Formlet" [`Type yields_type] tycon_env)
    | e -> super#phrasenode e
end

let desugar_formlets env = ((new desugar_formlets env) : desugar_formlets :> TransformSugar.transform)

let has_no_formlets =
object
  inherit SugarTraversals.predicate as super

  val has_no_formlets = true
  method satisfied = has_no_formlets

  method! phrasenode = function
    | `Formlet _ -> {< has_no_formlets = false >}
    | e -> super#phrasenode e
end
