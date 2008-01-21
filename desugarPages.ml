open Sugartypes

let rec desugar_page : phrase -> phrase =
  fun (phrase, pos) ->
    let rec is_raw (phrase, pos) =
      match phrase with
        | `TextNode _
        | `Block _ -> true
        | `FormletPlacement _
        | `PagePlacement _ -> false
        | `Xml (_, _, _, children) ->
            List.for_all is_raw children
        | e -> 
            raise (ConcreteSyntaxError ("Invalid element in page literal", pos)) in

    let desugar_pages : phrase list -> phrase = fun children ->
      (`FnAppl ((`Var "joinManyP", pos),
                [`ListLit (List.map desugar_page children), pos]), pos) in
    let dp : phrasenode -> phrase = function
      | e when is_raw (e, pos) ->
          (`FnAppl ((`Var "bodyP", pos),
                    [e, pos]), pos)
      | `FormletPlacement (formlet, handler, attributes) ->
          (`FnAppl ((`Var "formP", pos),
                    [formlet; handler; attributes]), pos)
      | `PagePlacement (page) ->
          page
      | `Xml ("#", [], _, children) ->
          desugar_pages children
      | `Xml (name, attrs, dynattrs, children) ->
          let x = Utility.gensym ~prefix:"xml" () in
            (`FnAppl ((`Var "plugP", pos),
                      [(`FunLit([[`Variable (x,None,pos), pos]],
                                (`Xml (name, attrs, dynattrs,
                                       [`Block ([], (`Var x, pos)), pos]), pos)), pos);
                       desugar_pages children]), pos)
      | _ ->
          raise (ConcreteSyntaxError ("Invalid element in page literal", pos))
    in
      dp phrase

let desugar_pages = 
object
  inherit SugarTraversals.map as super

  method phrase = function
    | `Page e, _  -> desugar_page (super#phrase e)
    | e -> super#phrase e
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
