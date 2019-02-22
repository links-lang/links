open CommonTypes
open Sugartypes
open SugarConstructors.Make

let rec is_raw phrase =
  match phrase.node with
  | TextNode _ -> true
  | Block _ -> true
  | FormletPlacement _
  | PagePlacement _ -> false
  | Xml (_, _, _, children) ->
     List.for_all is_raw children
  | _e ->
     raise (Errors.SugarError (phrase.pos, "Invalid element in page literal"))

(* DODGEYNESS:

   The first argument to desugar_page is an object which is only used
   to lookup effecs and to construct formlet types.

   This code assumes that:

     - the effecs are the same throughout the page literal
     - the environment is unchanged after calling o#phrase formlet
*)
let rec desugar_page (o, page_type) =
  let desugar_nodes : phrase list -> phrase =
    fun children ->
     fn_appl "joinManyP" [`Row (o#lookup_effects)]
       [list ~ty:page_type (List.map (desugar_page (o, page_type)) children)]
  in
    fun ({node=e; pos} as phrase) ->
      match e with
        | _ when is_raw phrase ->
          (* TODO: check that e doesn't contain any formletplacements or page placements *)
           fn_appl "bodyP" [`Row (o#lookup_effects)] [phrase]
        | FormletPlacement (formlet, handler, attributes) ->
            let (_, formlet, formlet_type) = o#phrase formlet in
            let formlet_type = Types.concrete_type formlet_type in
            let a = Types.fresh_type_variable (lin_any, res_any) in
            let b = Types.fresh_type_variable (lin_any, res_any) in
              Unify.datatypes (`Alias (("Formlet", [`Type a]), b), formlet_type);
              fn_appl "formP" [`Type a; `Row (o#lookup_effects)]
                      [formlet; handler; attributes]
        | PagePlacement (page) -> page
        | Xml ("#", [], _, children) ->
            desugar_nodes children
        | Xml (name, attrs, dynattrs, children) ->
            let x = Utility.gensym ~prefix:"xml" () in
            fn_appl "plugP" [`Row (o#lookup_effects)]
               [fun_lit ~args:[Types.make_tuple_type [Types.xml_type], o#lookup_effects]
                        dl_unl [[variable_pat ~ty:Types.xml_type x]]
                        (xml name attrs dynattrs [block ([], var x)]);
                desugar_nodes children]
        | _ ->
          raise (Errors.SugarError (pos, "Invalid element in page literal"))

and desugar_pages env =
object
  inherit (TransformSugar.transform env) as super

  method! phrasenode = function
    | Page e ->
        let (o, e, _t) = super#phrase e in
        let page_type = Instantiate.alias "Page" [] env.Types.tycon_env in
        let e = desugar_page (o, page_type) e in
          (o, e.node, page_type)
    | e -> super#phrasenode e
end

let is_pageless =
object
  inherit SugarTraversals.predicate as super

  val pageless = true
  method satisfied = pageless

  method! phrasenode = function
    | Page _ -> {< pageless = false >}
    | e -> super#phrasenode e
end
