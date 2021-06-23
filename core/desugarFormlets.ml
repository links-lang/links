open CommonTypes
open Utility
open SourceCode
open SourceCode.WithPos
open Sugartypes
open SugarConstructors.SugartypesPositions

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

let xml_str           = "xml"
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
        | _ when is_raw ph -> [], [], []
        | FormBinding (f, p) ->
            let (_o, _f, ft) = o#phrase f in
            let t = Types.fresh_type_variable (lin_any, res_any) in
            let () =
              Unify.datatypes
                (ft, Instantiate.alias "Formlet" [(PrimaryKind.Type, t)] tycon_env) in
            let name = Utility.gensym ~prefix:"_formlet_" () in
            let (xb, x) = (binder name ~ty:t, var name) in
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
  method formlet_body : Sugartypes.phrase -> ('self_type * Sugartypes.phrase * Types.datatype) =
    fun e ->
        let ppos = WithPos.pos e in
        match WithPos.node e with
          | _ when is_raw e ->
             let e = fn_appl ~ppos xml_str [(PrimaryKind.Row, o#lookup_effects)] [e]
             in (o, e, Types.xml_type)
          | FormBinding (f, _) ->
              let (o, f, ft) = o#phrase f
              in (o, f, ft)
          | Xml ("#", [], None, contents) ->
              (* pure (fun ps -> vs) <*> e1 <*> ... <*> ek *)
              let pss', vs', ts' =
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
              let vs, ts = List.rev vs', List.rev ts' in

              (* Given (f1 -> v1 : t1) ... (fn -> vn : tn), we generate a term of the form
                 (@@@)(f1, ... (@@@)(fn)(pure (fun(pn : tn)...(p1 : t1) { (p1, ..., pn) }))).

                 Thus we generate a function with the arguments in reverse, but the variables
                 in the original order.
               *)
              let ft =
                List.fold_right
                  (fun t ft -> Types.Function (Types.make_tuple_type [t], Types.closed_wild_row, ft))
                  ts' (TypeUtils.pack_types ts) in
              let open PrimaryKind in
              begin
                  match vs with
                    | [] ->
                        let (o, e, _) = super#phrasenode (Xml ("#", [], None, contents)) in
                        (o,
                         fn_appl ~ppos xml_str [Row, o#lookup_effects] [with_dummy_pos e],
                         Types.unit_type)
                    | _ ->
                        let args = List.map (fun t -> (Types.make_tuple_type [t], Types.closed_wild_row)) ts' in
                        let (o, es, _) = TransformSugar.list o (fun o -> o#formlet_body) contents in
                        let eff = o#lookup_effects in
                        let base : phrase =
                          fn_appl pure_str
                            [(Type, ft); (Row, eff)]
                            [fun_lit ~ppos ~args:args dl_unl pss' (tuple vs)]
                        in
                        let p, et =
                          List.fold_right
                            (fun arg (base, ft) ->
                               let arg_type = List.hd (TypeUtils.arg_types ft) in
                               let ft = TypeUtils.return_type ft in
                               let base : phrase =
                                 fn_appl ~ppos atatat_str
                                   [(Type, arg_type); (Type, ft); (Row, o#lookup_effects)]
                                   [arg; base]
                               in base, ft)
                            es (base, ft)
                        in
                          (o, p, et)
                end
          | Xml(tag, attrs, attrexp, contents) ->
              (* plug (fun x -> (<tag attrs>{x}</tag>)) (<#>contents</#>)^o*)
              let (o, attrexp, _) = TransformSugar.option o (fun o -> o#phrase) attrexp in
              let eff = o#lookup_effects in
              let context : phrase =
                let name = Utility.gensym ~prefix:"_formlet_" () in
                fun_lit ~ppos
                        ~args:[Types.make_tuple_type [Types.xml_type], Types.closed_wild_row]
                        dl_unl
                        [[variable_pat ~ty:(Types.xml_type) name]]
                        (xml tag attrs attrexp [block ([], var name)]) in
              let open PrimaryKind in
              let (o, e, t) = o#formlet_body (xml "#" [] None contents) in
              (o, fn_appl ~ppos plug_str [(Type, t); (Row, eff)] [context; e], t)
          | _ -> assert false

  method! phrasenode  : phrasenode -> ('self_type * phrasenode * Types.datatype) = function
    | Formlet (body, yields) ->
        (* pure (fun q^ -> [[e]]* ) <*> q^o *)
        (* let e_in = `Formlet (body, yields) in *)

        let eff = o#lookup_effects in
        let (ps, _, ts) = o#formlet_patterns body in
        let (o, body, _body_type) = o#formlet_body body in
        let (o, ps) = TransformSugar.listu o (fun o -> o#pattern) ps in
        let o = o#with_effects Types.closed_wild_row in
        let (o, yields, yields_type) = o#phrase yields in
        let o = o#with_effects eff in

        let pss =
          match ps with
            | [p] -> [[p]]
            | _ -> [[tuple_pat ps]] in

        let arg_type = TypeUtils.pack_types ts in
        let open PrimaryKind in
        let e =
          fn_appl_node atatat_str
             [(Type, arg_type); (Type, yields_type); (Row, eff)]
             [body; fn_appl pure_str
                      [(Type, Types.Function (Types.make_tuple_type [arg_type], Types.closed_wild_row, yields_type));
                       (Row, eff)]
                    [fun_lit ~args:[Types.make_tuple_type [arg_type], Types.closed_wild_row] dl_unl pss yields]] in
        (o, e, Instantiate.alias "Formlet" [(Type, yields_type)] tycon_env)
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

module Typeable
  = Transform.Typeable.Make(struct
        let name = "formlets"
        let obj env = (desugar_formlets env : TransformSugar.transform :> Transform.Typeable.sugar_transformer)
      end)
