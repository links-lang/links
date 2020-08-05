open Utility
open CommonTypes
open Sugartypes
open List
open SugarConstructors.SugartypesPositions
open SourceCode
open SourceCode.WithPos

(* TODO:

   Either disallow l:href and l:action in client-side XML or, more
   usefully, provide proper support for sending client-side closures
   to the server.
*)

let desugaring_error pos message =
  let open Errors in
  desugaring_error ~pos ~stage:DesugarLAttributes ~message

let has_lattrs : phrase -> bool = function
  | { node=Xml (_, attrs, _, _); _ } ->
      exists (fst ->- start_of ~is:"l:") attrs
  | _ -> false

let apply name args : phrase = fn_appl name [] args

let server_use name =
  apply "assocDefault" [constant_str name; apply "environment" []; constant_str ""]

let client_use id =
  apply "getInputValue" [constant_str id]

let client_radiogroup_use ids =
  apply "getRadioGroupValue" [list (List.map constant_str ids)]

let fresh_names () =
  let id = gensym ~prefix:"_lnameid_" () in
  let name = gensym ~prefix:"lname_" () in
  id, name

let desugar_lhref : phrase -> phrase = function
  | { node=Xml (("a"|"A") as a, attrs, attrexp, children); pos }
      when mem_assoc "l:href" attrs ->
      let attrs =
        match partition (fst ->- (=)"l:href") attrs with
          | [_,[target]], rest ->
              ("href",
               [constant_str "?_k=";
                apply "pickleCont" [fun_lit ~location:loc_server dl_unl [[]]
                                            target]])
              :: rest
          | _ ->
              raise (desugaring_error pos
                ("Invalid l:href: check that there are no " ^
                 "multiple l:href attributes"))
      in WithPos.make ~pos (Xml (a, attrs, attrexp, children))
  | e -> e

let desugar_laction : phrase -> phrase = function
  | { node=Xml (("form"|"FORM") as form, attrs, attrexp, children); pos }
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
            and action = ("action", [constant_str "#"]) in
            WithPos.make ~pos
              (Xml (form, action::rest, attrexp, hidden::children))
        | _ ->
            raise (desugaring_error pos
              ("Invalid l:action: check that there are no " ^
               "multiple l:action attributes"))
      end
  | e -> e

let desugar_lonevent : phrase -> phrase =
  let event_handler_pair = function
    | (name, [rhs]) ->
        let event_name = StringLabels.sub ~pos:4 ~len:(String.length name - 4)
                                          name in
          tuple [constant_str event_name;
                 fun_lit ~location:loc_client dl_unl [[variable_pat "event"]]
                         rhs]
    | _ -> assert false
  in function
    | { node=Xml (tag, attrs, attrexp, children); pos }
        when exists (fst ->- start_of ~is:"l:on") attrs ->
        let lons, others = partition (fst ->- start_of ~is:"l:on") attrs in
        let idattr =
          ("key",
           [apply "registerEventHandlers"
                  [list (List.map (event_handler_pair) lons)]]) in
          WithPos.make ~pos (Xml (tag, idattr::others, attrexp, children))
    | e -> e

type input_ids =
  | Single of string
  | RadioGroup of string list

let is_radio : (string * phrase list) list -> bool = function
  attrs ->
  let is_type_radio = function
  | "type", [{node=Constant (Constant.String "radio"); _}] -> true
  | _ -> false in
  exists is_type_radio attrs
;;
let desugar_lnames (p : phrase list) : phrase list * (input_ids * string) StringMap.t =
  let lnames = ref StringMap.empty in
  let attr_single : Position.t -> string * phrase list -> (string * phrase list) list =
    fun pos attribute ->
    match attribute with
      | "l:name", [{node=Constant (Constant.String lname); _}] ->
          let id, name = fresh_names () in
          if StringMap.mem lname !lnames
          then raise (desugaring_error pos "l:name attributes can only be reused for radio button groups");
          lnames := StringMap.add lname (Single id, name) !lnames;
          [("name", [constant_str name]);
           ("id"  , [constant_str id  ])]
      | "l:name", _ ->
          raise (desugaring_error pos "The value of an l:name attribute must be a string constant")
      | a -> [a] in
  let attr_radio : Position.t -> string * phrase list -> (string * phrase list) list =
    fun pos attribute ->
    match attribute with
      | "l:name", [{node=Constant (Constant.String lname); _}] ->
          begin
          Debug.print lname;
          match StringMap.find_opt lname !lnames with
            | Some (RadioGroup ids,name) ->
                let id, _ = fresh_names () in
                lnames := StringMap.remove lname !lnames;
                lnames := StringMap.add lname (RadioGroup(id::ids), name) !lnames;
                [("name", [constant_str name]);
                 ("id"  , [constant_str id  ])]
            | Some (Single _, _) ->
                raise (desugaring_error pos "l:name attributes can only be reused for radio button groups")
            | None ->
                let id, name = fresh_names () in
                lnames := StringMap.add lname (RadioGroup([id]), name) !lnames;
                [("name", [constant_str name]);
                 ("id"  , [constant_str id  ])]
          end
      | "l:name", _ ->
          raise (desugaring_error pos "The value of an l:name attribute must be a string constant")
      | a -> [a] in
  let rec aux : phrase -> phrase  = function
    | { node=Xml (tag, attrs, attrexp, children); pos } ->
        let attr = if is_radio attrs then attr_radio else attr_single in
        let attrs = concat_map (attr pos) attrs in
        let children =
          List.map aux children in
          WithPos.make ~pos (Xml (tag, attrs, attrexp, children))
    | p -> p
  in
  let p' = List.map aux p in
    p', !lnames

let let_in name rhs body : phrase =
  block ([val_binding' None (PatName name, rhs, loc_unknown)], body)

let bind_lname_vars lnames = function
  | "l:action" as attr, es ->
      attr, (List.map (StringMap.fold
                         (fun var (_,name) -> let_in var (server_use name))
                         lnames)
               es)
  | attr, es when start_of attr ~is:"l:on" ->
    attr, (List.map (StringMap.fold
                       (fun var binding ->
                         match binding with
                           | (Single id,_) -> let_in var (client_use id)
                           | (RadioGroup ids,_) -> let_in var (client_radiogroup_use ids)
                       )
                       lnames)
             es)
  | attr -> attr

let desugar_form : phrase -> phrase = function
  | { node=Xml (("form"|"FORM") as form, attrs, attrexp, children); pos } ->
      let children, lnames = desugar_lnames children in
      let attrs = List.map (bind_lname_vars lnames) attrs in
        WithPos.make ~pos
          (Xml (form, attrs, attrexp, children))
  | e -> e

let replace_lattrs : phrase -> phrase =
  desugar_form ->- desugar_laction ->- desugar_lhref ->- desugar_lonevent ->-
  (fun (xml) ->
     if (has_lattrs xml) then
       match xml with
         | { node=Xml (_tag, attributes, _, _); pos } ->
           let attr,_ = List.find (fst ->- start_of ~is:"l:") attributes in
            raise (desugaring_error pos (attr ^  " attributes must appear lexically within form tags.  (Consider using formlets or MVU instead for more compositional form behavior.)"))
         | _ -> assert false
     else
       xml)

let desugar_lattributes =
object
  inherit SugarTraversals.map as super
  method! phrase = function
    | {node=Xml _; _} as x when has_lattrs x ->
       let x = replace_lattrs x in
       let () = validate_xml x in
       super#phrase x
    | e -> super#phrase e
end

let has_no_lattributes =
object (_self)
  inherit SugarTraversals.predicate as super

  val no_lattributes = true
  method satisfied = no_lattributes

  method! phrase = function
    | {node = Xml _; _ } as x when has_lattrs x ->
        {< no_lattributes = false >}
    | e -> super#phrase e
end

module Untyped
  = Transform.Untyped.Make.Transformer(struct
        let name = "lattributes"
        let obj = desugar_lattributes end)
