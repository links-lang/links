open Sugartypes

let rec is_raw (phrase, pos) =
  match phrase with
    | `TextNode _
    | `Block _ -> true
    | `FormletPlacement _
    | `PagePlacement _ -> false
    | `Xml (_, _, _, children) ->
        List.for_all is_raw children
    | e -> 
        raise (ConcreteSyntaxError ("Invalid element in page literal", pos))

(* DODGEYNESS:

   The first argument to desugar_page is an object which is only used
   to lookup the mailbox type and to construct formlet types.

   This code assumes that:

     - the mailbox type is the same throughout the page literal
     - the environment is unchanged after calling o#phrase formlet
*)
let rec desugar_page o =
  let desugar_nodes pos : phrase list -> phrase =
    fun children ->
      (`FnAppl ((`TAppl ((`Var "joinManyP", pos), [`Type (o#lookup_mb ())]), pos),
                [`ListLit (List.map (desugar_page o) children, None), pos]), pos)
  in
    fun (e, pos) ->
      match e with
        | _ when is_raw (e, pos) ->
            (`FnAppl ((`TAppl ((`Var "bodyP", pos), [`Type (o#lookup_mb ())]), pos),
                      [e, pos]), pos)
        | `FormletPlacement (formlet, handler, attributes) ->
            let (_, formlet, formlet_type) = o#phrase formlet in
            let a =
              match formlet_type with
                | `Alias ((_, [a]), _) -> a
                | _ -> assert false
            in
              (`FnAppl ((`TAppl ((`Var "formP", pos), [`Type a; `Type (o#lookup_mb ())]), pos),
                        [formlet; handler; attributes]), pos)
        | `PagePlacement (page) -> page
        | `Xml ("#", [], _, children) ->
            desugar_nodes pos children
        | `Xml (name, attrs, dynattrs, children) ->
            let x = Utility.gensym ~prefix:"xml" () in
              (`FnAppl ((`TAppl ((`Var "plugP", pos), [`Type (o#lookup_mb ())]), pos),
                        [(`FunLit
                            (Some ([Types.xml_type, o#lookup_mb ()]),
                             ([[`Variable (x, Some (Types.xml_type), pos), pos]],
                              (`Xml (name, attrs, dynattrs,
                                     [`Block ([], (`Var x, pos)), pos]), pos))), pos);
                         desugar_nodes pos children]), pos)
        | _ ->
            raise (ConcreteSyntaxError ("Invalid element in page literal", pos))

and desugar_pages {Types.var_env=var_env; Types.tycon_env=tycon_env} =
object
  inherit (TransformSugar.transform (var_env, tycon_env)) as super

  method phrasenode = function
    | `Page e ->
        let (o, e, t) = super#phrase e in
        let (e, _) = desugar_page o e in
          (o, e, Instantiate.alias "Page" [] tycon_env)
    | e -> super#phrasenode e
end

let is_pageless =
object
  inherit SugarTraversals.predicate as super

  val pageless = true
  method satisfied = pageless

  method phrasenode = function
    | `Page _ -> {< pageless = false >}
    | e -> super#phrasenode e
end
