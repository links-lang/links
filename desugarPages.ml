open Sugartypes

let rec is_raw (phrase, pos) =
  match phrase with
    | `TextNode _ -> true
    | `Block _ -> true
    | `FormletPlacement _
    | `PagePlacement _ -> false
    | `Xml (_, _, _, children) ->
        List.for_all is_raw children
    | _e ->
        raise (Errors.SugarError (pos, "Invalid element in page literal"))

(* DODGEYNESS:

   The first argument to desugar_page is an object which is only used
   to lookup effecs and to construct formlet types.

   This code assumes that:

     - the effecs are the same throughout the page literal
     - the environment is unchanged after calling o#phrase formlet
*)
let rec desugar_page (o, page_type) =
  let desugar_nodes pos : phrase list -> phrase =
    fun children ->
      (`FnAppl ((`TAppl ((`Var "joinManyP", pos), [`Row (o#lookup_effects)]), pos),
                [`ListLit (List.map (desugar_page (o, page_type)) children, Some page_type), pos]), pos)
  in
    fun (e, pos) ->
      match e with
        | _ when is_raw (e, pos) ->
          (* TODO: check that e doesn't contain any formletplacements or page placements *)
            (`FnAppl ((`TAppl ((`Var "bodyP", pos), [`Row (o#lookup_effects)]), pos),
                      [e, pos]), pos)
        | `FormletPlacement (formlet, handler, attributes) ->
            let (_, formlet, formlet_type) = o#phrase formlet in
            let formlet_type = Types.concrete_type formlet_type in
            let a = Types.fresh_type_variable (`Any, `Any) in
            let b = Types.fresh_type_variable (`Any, `Any) in
            let _template = `Alias (("Formlet", [`Type a]), b) in
              Unify.datatypes (`Alias (("Formlet", [`Type a]), b), formlet_type);
              (`FnAppl ((`TAppl ((`Var "formP", pos), [`Type a; `Row (o#lookup_effects)]), pos),
                        [formlet; handler; attributes]), pos)
        | `PagePlacement (page) -> page
        | `Xml ("#", [], _, children) ->
            desugar_nodes pos children
        | `Xml (name, attrs, dynattrs, children) ->
            let x = Utility.gensym ~prefix:"xml" () in
              (`FnAppl ((`TAppl ((`Var "plugP", pos), [`Row (o#lookup_effects)]), pos),
                        [(`FunLit
                            (Some ([Types.make_tuple_type [Types.xml_type], o#lookup_effects]),
                             `Unl,
                             ([[`Variable (x, Some (Types.xml_type), pos), pos]],
                              (`Xml (name, attrs, dynattrs,
                                     [`Block ([], (`Var x, pos)), pos]), pos)), `Unknown), pos);
                         desugar_nodes pos children]), pos)
        | _ ->
          raise (Errors.SugarError (pos, "Invalid element in page literal"))

and desugar_pages env =
object
  inherit (TransformSugar.transform env) as super

  method! phrasenode = function
    | `Page e ->
        let (o, e, _t) = super#phrase e in
        let page_type = Instantiate.alias "Page" [] env.Types.tycon_env in
        let (e, _) = desugar_page (o, page_type) e in
          (o, e, page_type)
    | e -> super#phrasenode e
end

let is_pageless =
object
  inherit SugarTraversals.predicate as super

  val pageless = true
  method satisfied = pageless

  method! phrasenode = function
    | `Page _ -> {< pageless = false >}
    | e -> super#phrasenode e
end
