open Utility
open Sugartypes
open List

let appPrim pos name args = 
  (`FnAppl ((`Var name, pos), args), pos : phrase)

(** Construct a Links list out of a list of Links expressions; all
    will have the source position [pos].
*)
let make_links_list pos elems =
  let concat_expr l r = `InfixAppl (([], `Name "++"), l, r), pos in
    fold_right concat_expr elems (`ListLit ([], None), pos)

(** Returns a function that plugs some given XML in as the contents of
    an XML element having the given tag name and attributes. *)
let make_xml_context tag (attrs:(string * phrase) list) pos : phrase = 
  let hole = gensym () in
    `FunLit (None, ([[`Variable (hole, None, pos), pos]], 
                    (`Xml (tag, 
                           List.map (fun (s,a) -> s, [a]) attrs,
                           None,
                           [`Var hole, pos]), pos))), pos

let rec has_form_binding = function
  | `Xml (_, _, _, subnodes),_ -> List.exists has_form_binding subnodes
  | `FormBinding _,_      -> true
  |  _                    -> false

let rec forest_to_form_expr trees yieldsClause 
    (pos:Sugartypes.position) 
    (trees_ppos:Sugartypes.position)
    : (Sugartypes.phrase * Sugartypes.pattern list list) = 
  (* We pass over the forest finding the bindings and construct a
     term-context representing all of the form/yields expression
     except the `yields' part (the handler). Here bindings
     is a list of lists, each list representing a tuple returned
     from an inner instance of forest_to_form_expr--or, if it's a
     singleton, a single value as bound by a binder.  *)
  let (ctxt : Sugartypes.phrase -> Sugartypes.phrase), bindings =
    List.fold_right
      (fun (l : phrase) (ctxt, bs) -> 
         let l_unsugared, bindings = desugar_form_expr l in
           (fun r -> appPrim pos "@@@"  [l_unsugared; ctxt r]),
         bindings @ bs) 
      trees
      (Utility.identity, []) in
    (* Next we construct the handler body from the yieldsClause,
       if any.  The yieldsClause is the user's handler; if it is
       None then we construct a default handler that just bundles
       up all the bound variables and returns them as a tuple.
       Here we also form a list of the values we're
       returning. returning_bindings is a list of lists,
       representing a list of tuples of values.  *)
  let handlerBody, bindings, returning_bindings = 
    match yieldsClause with
        Some formHandler -> 
          formHandler, bindings, ([]:Sugartypes.pattern list list)
      | None ->
          let fresh_bindings = List.map (List.map (fun (_, ppos) -> `Variable (Utility.gensym (), None, ppos), ppos)) bindings in
          let variables = List.map (fun (`Variable (x,_,_), ppos) -> `Var x, ppos) (List.flatten fresh_bindings) in
            ((`TupleLit variables, (Lexing.dummy_pos, Lexing.dummy_pos, None)),
             fresh_bindings,
             [flatten bindings])
  in
    (* The handlerFunc is simply formed by abstracting the
       handlerBody with all the binding names, appropriately
       destructing tuples. *)
    (* Note: trees_ppos will become the position for each tuple;
       the position of the tuple is what's reported when duplicate
       bindings are present within one form. *)
  let handlerFunc  =  `FunLit (None, (map (function
                                             | [b] -> [b]
                                             | bs -> [`Tuple bs, trees_ppos]) (rev bindings),
                                      handlerBody)), trees_ppos in
    ctxt (`FnAppl ((`Var "pure", pos), [handlerFunc]), pos), returning_bindings
      
and desugar_form_expr (formExpr, pos) : Sugartypes.phrase * pattern list list =
    if not (has_form_binding (formExpr, pos)) then
      (`FnAppl ((`Var "xml", pos), [formExpr, pos]), pos), [[]]
    else
      match formExpr with
        | `FormBinding (phrase, ppattern) -> phrase, [[ppattern]]
        | `Xml ("#", [], attrexp, contents) -> forest_to_form_expr contents None pos pos
        | `Xml ("#", _, _, _) ->
            raise (ConcreteSyntaxError ("XML forest literals cannot have attributes", pos))
        | `Xml(tag, attrs, attrexp, contents) ->
            let form, bindings = forest_to_form_expr contents None pos pos in
            let attrs' = alistmap (make_links_list pos) attrs in
              (appPrim pos "plug" [make_xml_context tag attrs' pos; form],
               bindings)
                
        | `TextNode text -> 
            appPrim pos "xml" [appPrim pos "stringToXml" [`Constant (`String text), pos]], [[]]
        | _ -> assert false

let desugar_formlets =
object(o)
  inherit SugarTraversals.map as super

  method phrase = function
    | `Formlet (formExpr, formHandler), pos ->
        fst (forest_to_form_expr [o#phrase formExpr] (Some (o#phrase formHandler)) pos pos)
    | e -> super#phrase e
end

let has_no_formlets =
object
  inherit SugarTraversals.predicate as super

  val has_no_formlets = true
  method satisfied = has_no_formlets

  method phrasenode = function
    | `Formlet _ -> {< has_no_formlets = false >}
    | e -> super#phrasenode e
end
