open CommonTypes
open Utility
open SourceCode
open SourceCode.WithPos
open Sugartypes
open SugarConstructors.DummyPositions

let rec is_raw phrase =
  match WithPos.node phrase with
  | TextNode _ -> true
  | Block _    -> true
  | FormBinding _ -> false
  | Xml (_, _, _, children) ->
      List.for_all is_raw children
  | _ ->
      let open Errors in
      raise (desugaring_error
        ~pos:phrase.pos
        ~stage:DesugarFormlets
        ~message:"Invalid element in formlet literal")

let tt =
  function
    | [t] -> t
    | ts -> Types.make_tuple_type ts

let xml_str           = "xml"
let string_to_xml_str = "stringToXml"
let pure_str          = "pure"
let plug_str          = "plug"
let atatat_str        = "@@@"

class desugar_formlets env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  (*
    extract a list of (pattern, constructor, type) triples
    from a formlet body

    (this roughly corresponds to the dagger transformation)
  *)
  method formlet_patterns : Sugartypes.phrase -> (Sugartypes.Pattern.with_pos list * Sugartypes.phrase list * Types.datatype list) =
    fun ph ->
      match WithPos.node ph with
        | _ when is_raw ph ->
            [tuple_pat []], [tuple []], [Types.unit_type]
        | FormBinding (f, p) ->
            let (_o, _f, ft) = o#phrase f in
            let t = Types.fresh_type_variable (lin_any, res_any) in
            let () =
              Unify.datatypes
                (ft, Instantiate.alias (QualifiedName.of_name "Formlet") [`Type t] (o#get_env ()) ) in
            let name = Utility.gensym ~prefix:"_formlet_" () in
            let (xb, x) = (binder name ~ty:t, var (QualifiedName.of_name name)) in
              [with_dummy_pos (Pattern.As (xb, p))], [x], [t]
        | Xml (_, _, _, [node]) ->
            o#formlet_patterns node
        | Xml (_, _, _, contents) ->
            let ps, vs, ts =
              List.fold_left
                (fun (ps, vs, ts) e ->
                     match o#formlet_patterns e with
                       | [p], [v], [t] -> p::ps, v::vs, t::ts
                       | ps', vs', ts' ->
                           (tuple_pat ps')::ps, (tuple vs')::vs, (Types.make_tuple_type ts')::ts)
                ([], [], []) contents
            in
              List.rev ps, List.rev vs, List.rev ts
        | _ ->
            assert false

  (* desugar a formlet body (the ^o transformation) *)
  method private formlet_body_node : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) =
    fun e ->
        match e with
        | TextNode s ->
           let xml = QualifiedName.of_name xml_str in
           let string_to_xml = QualifiedName.of_name string_to_xml_str in
              let e =
                fn_appl_node xml [`Row (o#lookup_effects)]
                  [fn_appl string_to_xml [`Row (o#lookup_effects)]
                     [constant_str s]]
              in (o, e, Types.xml_type)
          | Block (bs, e) ->
              let xml = QualifiedName.of_name xml_str in
              let (o, e, _) =
                o#phrasenode
                  (block_node
                     (bs, (fn_appl xml [`Row (o#lookup_effects)] [e])))
              in (o, e, Types.xml_type)
          | FormBinding (f, _) ->
              let (o, {node=f; _}, ft) = o#phrase f
              in (o, f, ft)
          | Xml ("#", [], None, contents) ->
              (* pure (fun ps -> vs) <*> e1 <*> ... <*> ek *)
              let pss, vs, ts =
                let pss, vs, ts =
                  List.fold_left
                    (fun (pss, vs, ts) node ->
                      match o#formlet_patterns node with
                      | [p], [v], [t] ->
                         (* grrr... n-ary arguments are messy!
                            this type has to be a 1-tuple!
                          *)
                         [p]::pss, v::vs, t::ts
                           | ps', vs', ts' ->
                              [tuple_pat ps']::pss, tuple vs'::vs, (Types.make_tuple_type ts')::ts)
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
                          super#phrasenode (Xml ("#", [], None, contents))
                        in
                        let xml = QualifiedName.of_name xml_str in
                        (o, fn_appl_node xml [`Row (o#lookup_effects)]
                              [with_dummy_pos e],
                         Types.xml_type)
                    | _ ->
                        let (o, es, _) = TransformSugar.list o (fun o -> o#formlet_body) contents in
                        let mb = `Row (o#lookup_effects) in
                        let pure = QualifiedName.of_name pure_str in
                        let base : phrase =
                          fn_appl pure [`Type ft; mb]
                            [fun_lit ~args:(List.rev args) dl_unl (List.rev pss)
                                     (tuple vs)] in
                        let p, et =
                          List.fold_right
                            (fun arg (base, ft) ->
                               let arg_type = List.hd (TypeUtils.arg_types ft) in
                               let ft = TypeUtils.return_type ft in
                               let atatat = QualifiedName.of_name atatat_str in
                               let base : phrase =
                                 fn_appl atatat [`Type arg_type; `Type ft; mb]
                                         [arg; base]
                               in base, ft)
                            es (base, ft)
                        in
                        (o, p.node, et)
                end
          | Xml(tag, attrs, attrexp, contents) ->
              (* plug (fun x -> (<tag attrs>{x}</tag>)) (<#>contents</#>)^o*)
              let (o, attrexp, _) = TransformSugar.option o (fun o -> o#phrase) attrexp in
              let eff = o#lookup_effects in
              let context : phrase =
                let name = Utility.gensym ~prefix:"_formlet_" () in
                fun_lit ~args:[Types.make_tuple_type [Types.xml_type], eff]
                        dl_unl
                        [[variable_pat ~ty:(Types.xml_type) name]]
                        (xml tag attrs attrexp [block ([], var (QualifiedName.of_name name))])
              in
              let plug = QualifiedName.of_name plug_str in
              let (o, e, t) = o#formlet_body (xml "#" [] None contents) in
              (o, fn_appl_node plug [`Type t; `Row eff]
                    [context; e], t)
          | _ -> assert false

  method formlet_body : Sugartypes.phrase -> ('self_type * Sugartypes.phrase * Types.datatype) =
    fun {node; pos} ->
      let (o, node, t) = o#formlet_body_node node in (o, WithPos.make ~pos node, t)

  method! phrasenode  : phrasenode -> ('self_type * phrasenode * Types.datatype) = function
    | Formlet (body, yields) ->
        (* pure (fun q^ -> [[e]]* ) <*> q^o *)
        (* let e_in = `Formlet (body, yields) in *)
        let empty_eff = Types.make_empty_closed_row () in
        let (ps, _, ts) = o#formlet_patterns body in
        let (o, body, _body_type) = o#formlet_body body in
        let (o, ps) = TransformSugar.listu o (fun o -> o#pattern) ps in
        let (o, yields, yields_type) = o#phrase yields in

        let pss =
          match ps with
            | [p] -> [[p]]
            | _ -> [[tuple_pat ps]] in

        let arg_type = Types.make_tuple_type ts in
        let mb = `Row (o#lookup_effects) in

        let e =
          let atatat = QualifiedName.of_name atatat_str in
          let pure = QualifiedName.of_name pure_str in
          fn_appl_node atatat
             [`Type arg_type; `Type yields_type; mb]
             [body; fn_appl pure
                    [`Type (`Function (Types.make_tuple_type [arg_type], empty_eff, yields_type)); mb]
                    [fun_lit ~args:[Types.make_tuple_type [arg_type], empty_eff] dl_unl pss yields]]
        in
          (o, e, Instantiate.alias
                   (QualifiedName.of_name "Formlet")
                   [`Type yields_type]
                   (o#get_env ()))
    | e -> super#phrasenode e
end

let desugar_formlets env = ((new desugar_formlets env) : desugar_formlets :> TransformSugar.transform)

let has_no_formlets =
object
  inherit SugarTraversals.predicate as super

  val has_no_formlets = true
  method satisfied = has_no_formlets

  method! phrasenode = function
    | Formlet _ -> {< has_no_formlets = false >}
    | e -> super#phrasenode e
end
