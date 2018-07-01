open Utility
open Sugartypes
open List

(* TODO:

   Either disallow l:href and l:action in client-side XML or, more
   usefully, provide proper support for sending client-side closures
   to the server.
*)

let has_lattrs : phrasenode -> bool = function
  | `Xml (_, attrs, _, _) -> exists (fst ->- start_of ~is:"l:") attrs
  | _ -> false

let dummy_pos = Lexing.dummy_pos, Lexing.dummy_pos, None

let apply pos name args : phrase =
  let q = QualifiedName.of_name name in
  `FnAppl ((`Var q,pos), args), pos

let server_use name pos =
  apply pos "assoc" [(`Constant (`String name), pos);
                     apply pos "environment" []]

let client_use id pos =
  apply pos "getInputValue" [(`Constant (`String id), pos)]

let fresh_names () =
  let id = gensym ~prefix:"_lnameid_" () in
  let name = gensym ~prefix:"lname_" () in
  id, name

let desugar_lhref : phrasenode -> phrasenode = function
  | `Xml (("a"|"A") as a, attrs, attrexp, children)
      when mem_assoc "l:href" attrs ->
      let attrs =
        match partition (fst ->- (=)"l:href") attrs with
          | [_,[target]], rest ->
              (("href",
                [`Constant (`String "?_k="), dummy_pos;
                 apply dummy_pos "pickleCont" [`FunLit (None, `Unl, ([[]], target), `Server),
                                               dummy_pos]]))
              :: rest
          | _ -> assert false (* multiple l:hrefs, or an invalid rhs;
                                 NOTE: this is a user error and should
                                 be reported as such --ez.*)
      in
        `Xml (a, attrs, attrexp, children)
  | e -> e

let desugar_laction : phrasenode -> phrasenode = function
  | `Xml (("form"|"FORM") as form, attrs, attrexp, children)
      when mem_assoc "l:action" attrs ->
      begin match partition (fst ->- (=)"l:action") attrs with
        | [_,[action_expr]], rest ->
            let hidden : phrase =
              `Xml ("input",
                    ["type",  [`Constant (`String "hidden"), dummy_pos];
                     "name",  [`Constant (`String "_k"), dummy_pos];
                     "value", [apply dummy_pos "pickleCont"
                                [`FunLit(None,`Unl,([[]],action_expr), `Server), dummy_pos]]],
                    None,
                    []), dummy_pos
            and action = ("action", [`Constant (`String "#"), dummy_pos])
            in
              `Xml (form, action::rest, attrexp, hidden::children)
        | _ -> assert false (* multiple l:actions, or an invalid rhs;
                               NOTE: this is a user error and should
                               be reported as such --ez. *)
      end
  | e -> e

let desugar_lonevent : phrasenode -> phrasenode =
  let event_handler_pair pos = function
    | (name, [rhs]) ->
        let event_name = StringLabels.sub ~pos:4 ~len:(String.length name - 4) name in
          `TupleLit [`Constant (`String event_name), pos;
                     `FunLit (None, `Unl, ([[`Variable ("event", None, pos), pos]], rhs), `Client), pos], pos
    | _ -> assert false
  in function
    | `Xml (tag, attrs, attrexp, children)
        when exists (fst ->- start_of ~is:"l:on") attrs ->
        let lons, others = partition (fst ->- start_of ~is:"l:on") attrs in
        let idattr =
          ("key",
           [apply dummy_pos "registerEventHandlers"
              [`ListLit (List.map (event_handler_pair dummy_pos) lons, None), dummy_pos]]) in
          `Xml (tag, idattr::others, attrexp, children)
    | e -> e

let desugar_lnames (p : phrasenode) : phrasenode * (string * string * position) StringMap.t =
  let lnames = ref StringMap.empty in
  let add lname (id,name,pos) = lnames := StringMap.add lname (id,name,pos) !lnames in
  let attr : string * phrase list -> (string * phrase list) list = function
    | "l:name", [`Constant (`String v), pos] ->
        let id, name = fresh_names () in
          add v (id,name,pos);
          [("name", [`Constant (`String name), pos]); ("id", [`Constant (`String id), pos])]
    | "l:name", _ -> failwith ("Invalid l:name binding")
    | a -> [a] in
  let rec aux : phrasenode -> phrasenode  = function
    | `Xml (tag, attrs, attrexp, children) ->
        let attrs = concat_map attr attrs
        and children = List.map (fun (child,p) -> aux child, p) children in
          `Xml (tag, attrs, attrexp, children)
    | p -> p
  in
  let p' = aux p in
    p', !lnames

let let_in pos name rhs body : phrase =
  `Block ([`Val ([], (`Variable (name,None,pos), pos), rhs, `Unknown, None), pos], body), pos

let bind_lname_vars lnames = function
  | "l:action" as attr, es ->
      attr, (List.map (StringMap.fold
                         (fun var (_,name,pos) -> let_in pos var (server_use name dummy_pos))
                         lnames)
               es)
  | attr, es when start_of attr ~is:"l:on" ->
    attr, (List.map (StringMap.fold
                       (fun var (id,_,pos) -> let_in pos var (client_use id dummy_pos))
                       lnames)
             es)
  | attr -> attr

let desugar_form : phrasenode -> phrasenode = function
  | `Xml (("form"|"FORM") as form, attrs, attrexp, children) ->
      let children, children_positions = List.split children in
      let children, lnames = List.split (List.map desugar_lnames children) in
      let lnames =
        try List.fold_left StringMap.union_disjoint StringMap.empty lnames
        with StringMap.Not_disjoint (item, _) ->
          raise (Errors.SugarError (dummy_pos, "Duplicate l:name binding: " ^ item)) in
      let attrs = List.map (bind_lname_vars lnames) attrs in
        `Xml (form, attrs, attrexp, List.combine children children_positions)
  | e -> e

let replace_lattrs : phrasenode -> phrasenode = desugar_form ->- desugar_laction ->- desugar_lhref ->- desugar_lonevent ->-
  (fun (xml) ->
     if (has_lattrs xml) then
       match xml with
         | `Xml (_tag, _attributes, _, _) ->
             raise (Errors.SugarError (dummy_pos, "Illegal l: attribute in XML node"))
         | _ -> assert false
     else
       xml)

let desugar_lattributes =
object
  inherit SugarTraversals.map as super
  method! phrasenode = function
    | `Xml _ as x when has_lattrs x ->
        super#phrasenode (replace_lattrs x)
    | e -> super#phrasenode e
end

let has_no_lattributes =
object (_self)
  inherit SugarTraversals.predicate as super

  val no_lattributes = true
  method satisfied = no_lattributes

  method! phrasenode = function
    | `Xml _ as x when has_lattrs x -> {< no_lattributes = false >}
    | e -> super#phrasenode e
end
