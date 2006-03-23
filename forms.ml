(* Special functionality for HTML forms:  l:action/l:name *)

(* Needs serious attention to decide which bits work on typed and
   untyped data *)

(* What are the changes in a form transformation?

   1. Addition of an environment hidden field.  This must
      be done at runtime, when the environment is known.

      We could hack it, though, by returning a function which takes an
      internal environment object, then invoke the function in the
      interpreter.  NB: javascript, etc.

   2. Replacement of l:action={code} with action='...' and moving the serialised 
      l:action code into a hidden field in the body of the form.

   3. Something with l:name-bound names ?
*)
open List
open Num
open Netencoding

open Utility
open Sugar
open Syntax
open Kind
open Result


(** "Runtime" services: handling continuation objects during evaluation **)
type query_record = (string * result) list

type webcontinuation = ContParams of continuation * query_record
                       | ExprEnv of expression * environment

(* Walk the XML tree, looking for <input l:name> bindings that are
   inside the top <form> element, with no intervening <form>s.
*)
let lname_bound_vars : 'a expression' -> string list = 
  let desyntax = function
    | String (name, _) -> name
    | other -> raise (Parse_failure (_DUMMY_POS, string_of_expression other ^ " cannot be bound with l:name")) in
  let rec lnames = function
    | Xml_node (("input"|"textarea"|"select"), attrs, contents, _) ->
        (try [assoc "l:name" attrs] with Not_found -> concat (map lnames contents))
    | Xml_node ("form", _, _, _) -> (* new scope *) []
    | Xml_node (_, _, contents, _) -> concat (map lnames contents)
    | Collection_union (l, r, _) -> lnames l @ lnames r
    | _ -> [] 
  in function
    | Xml_node ("form", _, contents, _)  -> map desyntax (concat (map lnames contents))
    | _ ->  failwith "lname_bound_vars is only applicable to <form> xml elements"

let rec intersects l = function
    [] -> false
  | (h::t) -> mem h l || intersects l t

let is_special x = String.length x > 2 && String.sub x 0 2 = "l:"

let islform : 'data expression' -> bool = function
  | Xml_node ("form", attrs, _, _) 
      when List.exists (is_special -<- fst) attrs -> true
  | _ -> false

(* should be something like is_transformable_anchor *)

let islhref : 'data expression' -> bool = function
  | Xml_node ("a", attrs, _, _) 
      when exists (fun (k,_) -> Str.string_match (Str.regexp "l:") k 0) attrs -> true
  | _ -> false

(* Is an expression an <input l:name ...> expression? *)
let isinput : 'data expression' -> bool = function
  | Xml_node (("input"|"textarea"|"select"), attrs, _, _)
      when mem_assoc "l:name" attrs -> true
  | _ -> false

let add_attrs new_attrs = function
   | Xml_node (tag, attrs, c, d) ->
       Xml_node (tag, attrs @ new_attrs, c, d)
   | o -> failwith ("Non-XML structure passed to add_attrs : " ^ 
                      string_of_expression o)

let is_special_param (k, _) = 
  mem k ["continuation%25"; "environment%25"; "continuation%"; "environment%"; "expression%25"; "expression%"]

let is_pfunc = function
  | (_, `Primitive(`PFunction _)) -> true
  | _ -> false

let string s = 
  String (s, (_DUMMY_POS, Kind.string, None))

let hidden_input name value = 
  Xml_node ("input", [("type", string "hidden");
                      ("name", string name);
                      ("value", string value)], [],  (_DUMMY_POS, `Collection (`List, `Primitive `XMLitem), None))

let expr_to_thunk expr = `Function ("_", [], [], expr)

let attrname = fst
let attrval = snd

(* Serialise the continuation and environment, and adjust the form accordingly *)
let xml_transform env : expression -> expression = 
  let pickle_environment env = serialise_environment (filter (not @@ is_pfunc) env)

  in function 
    | Xml_node ("form", attrs, contents, data) as form ->
        (try 
           let laction = either_assoc "l:onsubmit" "l:handler" attrs in
           let new_fields = match laction with 
               (* l:action denotes a frozen expression *)
             | Left laction -> [hidden_input "expression%" (Base64.encode (serialise_result (expr_to_thunk laction)));
                                hidden_input "environment%" (Base64.encode (pickle_environment env))]
                 (* l:handler denotes an expression which should eval
                    to a continuation value; so this is broken *)
             | Right lhandler -> [hidden_input "continuation%"
                                    (Base64.encode (Syntax.serialise_expression lhandler))] in
             (* FIXME: replace l:handler too *)
             Xml_node ("form", substitute (((=)"l:onsubmit") @@ attrname) ("action", string "#") attrs, new_fields @ contents, data)
         with Not_found -> form)
    | Xml_node (("input"|"textarea"|"select") as tag, attrs, contents, data) as input ->
        (try match assoc "l:name" attrs with 
           | String (name, _) -> Xml_node (tag, substitute (((=)"l:name") @@ attrname) ("name", string name) attrs, contents, data)
           | _ -> failwith "Internal error transforming xml"
         with Not_found -> input)
    | Xml_node ("a", attrs, contents, data) ->
        let href = assoc "l:href" attrs in
        let ser_expr = Base64.encode (serialise_result (expr_to_thunk href)) 
        and ser_env = Base64.encode (pickle_environment (retain (freevars href) env)) in
        let new_value = "?environment%=" ^ ser_env ^ "&expression%=" ^ ser_expr in
        let new_attributes = substitute (((=)"l:href") @@ attrname) ("href", string new_value) attrs in
          Xml_node ("a", new_attributes, contents, data)

(* Everything from here on down handles remote calls from the client *)

open Pickle

(* Just a temporary measure to allow units through *)
let serialise_unit, deserialise_unit = primitive_serialisers 'z' (fun () -> "") (fun _ -> ())

let rec primitive_picklers (kind : Kind.kind) : result serialiser * result deserialiser = match kind with
  | `Primitive `Bool  -> serialise_bool  @@ unbox_bool,  (fun s -> let v, r = deserialise_bool s in box_bool v, r)
  | `Primitive `Int   -> serialise_int   @@ unbox_int,   (fun s -> let v, r = deserialise_int s in box_int v, r)
  | `Primitive `Float -> serialise_float @@ unbox_float, (fun s -> let v, r = deserialise_float s in box_float v, r)
  | `Primitive `Char  -> serialise_char  @@ unbox_char,  (fun s -> let v, r = deserialise_char s in box_char v, r)
  | `Primitive `XMLitem -> serialise_item   @@ unbox_xml,   (fun s -> let v, r = deserialise_item s in box_xml v, r)
  | `Record (field_env, `RowVar None) when StringMap.is_empty field_env -> (fun _ -> "z0+"), (fun s -> let v, r = deserialise_unit s in `Record [], r)
  | `Collection (`List, `Primitive `Char) ->
      ((fun r -> serialise_string (Result.charlist_as_string r)),
       (fun s -> let v, r = deserialise_string s in (string_as_charlist v), r))
  | `Collection (`List, element_type) -> 
      ((function
          | `Collection (_, items) -> serialise_list (fst (primitive_picklers element_type)) items
          | r -> failwith "Internal error: Unexpected result type (expected a list, got "^ string_of_result r ^")"),
       (fun s -> let v, r = deserialise_list (snd (primitive_picklers element_type)) s in `Collection (`List, v), r))
  | _ -> failwith ("Cannot create pickler for " ^ string_of_kind kind)

let primitive_serialiser v = fst (primitive_picklers v)
and primitive_deserialiser v = snd (primitive_picklers v)

(* string_dict_to_charlist_dict: a utility routine *)

let string_dict_to_charlist_dict =
  dict_map Result.string_as_charlist

let rec decode_parameters : Kind.kind list * string -> Result.result list = function 
  | [], "" -> []
  | [], _ -> failwith "Insufficient number of types for the parameter list in remote call"
  | _, "" -> failwith "Too many types for the parameter list in remote call"
  | kind :: kinds, parameters ->
      let arg, rest = primitive_deserialiser kind parameters in
        arg :: decode_parameters (kinds, rest)

(* Extract continuation or expression from the parameters passed in over CGI.*)
let cont_from_params primitive_lookup params =
  try
    let pickled_continuation = (try assoc "continuation%25" params 
                                with Not_found -> assoc "continuation%" params)
    and params = filter (not @@ is_special_param) params in
    let continuation = 
      (deserialise_continuation primitive_lookup
         (Base64.decode pickled_continuation))
    in
      Some(ContParams(continuation, string_dict_to_charlist_dict params))
        
  with Not_found -> try
    let pickled_expression = try assoc "expression%25" params 
                               with Not_found -> assoc "expression%" params
    and pickled_environment  = try assoc "environment%25"  params 
                               with Not_found -> assoc "environment%"  params 
    and params = filter (not @@ is_special_param) params in
    let environment = 
      if pickled_environment = "" then
        []
      else
        fst (deserialise_environment primitive_lookup (Base64.decode pickled_environment))
    and expression = (match fst (deserialise_result primitive_lookup (Base64.decode pickled_expression)) with
                          | `Function (_, _, _, p) -> p
                          | _ -> failwith "Type error unpickling expression")
    in
      debug $ string_of_environment_ez environment;
      Some (ExprEnv (expression, string_dict_to_charlist_dict params
                       @ environment))
        
  with Not_found -> None

let is_remote_call params = 
  mem_assoc "__name" params && mem_assoc "__args" params (* && mem_assoc "t" params*)

let rec apply_fn
    (interpreter : environment -> environment -> Syntax.expression -> Result.result) = function
      | (`Function (var, locals, globals, body) as f) -> 
          (function
             | [one] -> interpreter globals ((var,one) :: locals)  body
             | first :: rest -> apply_fn interpreter (apply_fn interpreter f [first]) rest)
      | otherwise -> failwith "Internal error: attempt to apply (deserialised) non-function"


(* Return the information corresponding to a from-JavaScript call *)
let remote_call_info interpreter toplevel_env type_env primitive_lookup params print = 
  let rec fn_return_type = function
    | `Function (_, (`Function _ as next)) -> fn_return_type next
    | `Function (_, t) -> t 
    | _ -> failwith "internal error : extract return type of non-function" in
  let function_name = Base64.decode (assoc "f" params)
  and params = Base64.decode (assoc "p" params)
  and types = Base64.decode (assoc "t" params)
  in match assoc function_name toplevel_env with
    | `Function _ as f ->
        debug ("remote call of " ^ function_name);
        let parameters = decode_parameters (fst (deserialise_list deserialise_kind types),  params) in
          (* TODO: Doesn't handle polymorphism: need to pass return type back as well *)
          print (Base64.encode (primitive_serialiser (fn_return_type (snd (assoc function_name type_env))) (apply_fn interpreter f parameters)))
    | _ -> failwith "Remote call of non-function"
