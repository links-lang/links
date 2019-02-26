open Utility
open CommonTypes
open Sugartypes
open List
open SugarConstructors.SugartypesPositions
open SourceCode
open SourceCode.WithPos.Legacy

(* TODO:

   Either disallow l:href and l:action in client-side XML or, more
   usefully, provide proper support for sending client-side closures
   to the server.
*)

let has_lattrs : phrasenode -> bool = function
  | Xml (_, attrs, _, _) -> exists (fst ->- start_of ~is:"l:") attrs
  | _ -> false

let apply name args : phrase = fn_appl name [] args

let server_use name =
  apply "assoc" [constant_str name; apply "environment" []]

let client_use id =
  apply "getInputValue" [constant_str id]

let fresh_names () =
  let id = gensym ~prefix:"_lnameid_" () in
  let name = gensym ~prefix:"lname_" () in
  id, name

let desugar_lhref : phrasenode -> phrasenode = function
  | Xml (("a"|"A") as a, attrs, attrexp, children)
      when mem_assoc "l:href" attrs ->
      let attrs =
        match partition (fst ->- (=)"l:href") attrs with
          | [_,[target]], rest ->
              ("href",
               [constant_str "?_k=";
                apply "pickleCont" [fun_lit ~location:loc_server dl_unl [[]]
                                            target]])
              :: rest
          | _ -> assert false (* multiple l:hrefs, or an invalid rhs;
                                 NOTE: this is a user error and should
                                 be reported as such --ez.*)
      in Xml (a, attrs, attrexp, children)
  | e -> e

let desugar_laction : phrasenode -> phrasenode = function
  | Xml (("form"|"FORM") as form, attrs, attrexp, children)
      when mem_assoc "l:action" attrs ->
      begin match partition (fst ->- (=)"l:action") attrs with
        | [_,[action_expr]], rest ->
            let hidden : phrase =
              xml "input"
                  ["type",  [constant_str "hidden"];
                   "name",  [constant_str "_k"];
                   "value", [apply "pickleCont"
                                   [fun_lit ~location:loc_server dl_unl [[]]
                                            action_expr]]]
                  None []
            and action = ("action", [constant_str "#"])
            in Xml (form, action::rest, attrexp, hidden::children)
        | _ -> assert false (* multiple l:actions, or an invalid rhs;
                               NOTE: this is a user error and should
                               be reported as such --ez. *)
      end
  | e -> e

let desugar_lonevent : phrasenode -> phrasenode =
  let event_handler_pair = function
    | (name, [rhs]) ->
        let event_name = StringLabels.sub ~pos:4 ~len:(String.length name - 4)
                                          name in
          tuple [constant_str event_name;
                 fun_lit ~location:loc_client dl_unl [[variable_pat "event"]]
                         rhs]
    | _ -> assert false
  in function
    | Xml (tag, attrs, attrexp, children)
        when exists (fst ->- start_of ~is:"l:on") attrs ->
        let lons, others = partition (fst ->- start_of ~is:"l:on") attrs in
        let idattr =
          ("key",
           [apply "registerEventHandlers"
                  [list (List.map (event_handler_pair) lons)]]) in
          Xml (tag, idattr::others, attrexp, children)
    | e -> e

let desugar_lnames (p : phrasenode) : phrasenode * (string * string) StringMap.t =
  let lnames = ref StringMap.empty in
  let add lname (id,name) = lnames := StringMap.add lname (id,name) !lnames in
  let attr : string * phrase list -> (string * phrase list) list = function
    | "l:name", [{node=Constant (`String v); _}] ->
        let id, name = fresh_names () in
          add v (id,name);
          [("name", [constant_str name]);
           ("id"  , [constant_str id  ])]
    | "l:name", _ -> failwith ("Invalid l:name binding")
    | a -> [a] in
  let rec aux : phrasenode -> phrasenode  = function
    | Xml (tag, attrs, attrexp, children) ->
        let attrs = concat_map attr attrs in
        let children =
          List.map (fun {node;pos} -> with_pos pos (aux node)) children in
          Xml (tag, attrs, attrexp, children)
    | p -> p
  in
  let p' = aux p in
    p', !lnames

let let_in name rhs body : phrase =
  block ([val_binding' NoSig (PatName name, rhs, loc_unknown)], body)

let bind_lname_vars lnames = function
  | "l:action" as attr, es ->
      attr, (List.map (StringMap.fold
                         (fun var (_,name) -> let_in var (server_use name))
                         lnames)
               es)
  | attr, es when start_of attr ~is:"l:on" ->
    attr, (List.map (StringMap.fold
                       (fun var (id,_) -> let_in var (client_use id))
                       lnames)
             es)
  | attr -> attr

let desugar_form : phrasenode -> phrasenode = function
  | Xml (("form"|"FORM") as form, attrs, attrexp, children) ->
      let children, poss   = List.split (List.map tuple_of_with_pos children) in
      let children, lnames = List.split (List.map desugar_lnames    children) in
      let lnames =
        try List.fold_left StringMap.union_disjoint StringMap.empty lnames
        with StringMap.Not_disjoint (item, _) ->
          raise (Errors.SugarError (Position.dummy, "Duplicate l:name binding: " ^ item)) in
      let attrs = List.map (bind_lname_vars lnames) attrs in
        Xml (form, attrs, attrexp, Utility.zip_with with_pos poss children)
  | e -> e

let replace_lattrs : phrasenode -> phrasenode =
  desugar_form ->- desugar_laction ->- desugar_lhref ->- desugar_lonevent ->-
  (fun (xml) ->
     if (has_lattrs xml) then
       match xml with
         | Xml (_tag, _attributes, _, _) ->
             raise (Errors.SugarError (Position.dummy, "Illegal l: attribute in XML node"))
         | _ -> assert false
     else
       xml)

let desugar_lattributes =
object
  inherit SugarTraversals.map as super
  method! phrasenode = function
    | Xml _ as x when has_lattrs x ->
        super#phrasenode (replace_lattrs x)
    | e -> super#phrasenode e
end

let has_no_lattributes =
object (_self)
  inherit SugarTraversals.predicate as super

  val no_lattributes = true
  method satisfied = no_lattributes

  method! phrasenode = function
    | Xml _ as x when has_lattrs x -> {< no_lattributes = false >}
    | e -> super#phrasenode e
end
