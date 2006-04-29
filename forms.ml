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
    | Xml_node (_, _, _, _)  -> []

let is_special x = String.length x > 2 && String.sub x 0 2 = "l:"

let islform : 'data expression' -> bool = function
  | Xml_node ("form", attrs, _, _) 
      when List.exists (is_special -<- fst) attrs -> true
  | _ -> false

(** islhref
    should be something like is_transformable_anchor *)
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
  String (s, (_DUMMY_POS, Kind.string_type, None))

let hidden_input name value = 
  Xml_node ("input", [("type", string "hidden");
                      ("name", string name);
                      ("value", string value)], [],  (_DUMMY_POS, `Collection (`List, `Primitive `XMLitem), None))

let expr_to_thunk expr = `Function ("_", [], [], expr)

let attrname = fst
let attrval = snd

let pickle_environment env =
  serialise_environment (filter (not @@ is_pfunc) env)

let serialize_exprenv expr env =
  (Utility.base64encode (serialise_result (expr_to_thunk expr)),
   Utility.base64encode (pickle_environment (retain (freevars expr) env)))

(* Serialise the continuation and environment, and adjust the form accordingly *)
let xml_transform env eval : expression -> expression = 
  function 
    | Xml_node ("form", attrs, contents, data) as form ->
        (try 
           let laction = either_assoc "l:onsubmit" "l:handler" attrs in
           let new_fields = match laction with 
               (* l:action denotes a frozen expression *)
             | Left laction -> 
                 (* TBD: consolidate these into one thing *)
                 let expr_str, env_str = serialize_exprenv laction env in
                   [hidden_input "expression%" expr_str;
                    hidden_input "environment%" env_str]

             | Right lhandler -> 
                 (match eval lhandler [] with
                      `Continuation c ->
                        [hidden_input "continuation%"
                           (Utility.base64encode (Result.serialise_continuation c))]
                    | _ -> failwith "Internal error: l:handler was not a continuation")
           in
             Xml_node ("form",
                       substitute (fun attr ->
                                     let name = attrname attr in
                                       (name = "l:onsubmit" || name = "l:handler"))
                         ("action", string "#") attrs, 
                       new_fields @ contents, data)
         with Not_found -> form)
    | Xml_node (("input"|"textarea"|"select") as tag, attrs, contents, data) as input ->
        (try match assoc "l:name" attrs with
           | String (name, _) -> Xml_node (tag, substitute (((=)"l:name") @@ attrname) ("name", string name) attrs, contents, data)
           | _ -> failwith "Internal error transforming xml"
         with Not_found -> input)
    | Xml_node ("a", attrs, contents, data) ->
        let href = assoc "l:href" attrs in
        let ser_expr, ser_env = serialize_exprenv href env in
        let new_value = ("?environment%=" ^ ser_env ^
                           "&expression%=" ^ ser_expr) in
        let new_attributes = substitute (((=)"l:href") @@ attrname) ("href", string new_value) attrs in
          Xml_node ("a", new_attributes, contents, data)

(* string_dict_to_charlist_dict: a utility routine *)

let string_dict_to_charlist_dict =
  dict_map Result.string_as_charlist

(* Extract continuation or expression from the parameters passed in over CGI.*)
let cont_from_params primitive_lookup params =
  try
    let pickled_continuation = (try assoc "continuation%25" params 
                                with Not_found -> assoc "continuation%" params)
    and params = filter (not @@ is_special_param) params in
    let continuation = 
      (deserialise_continuation primitive_lookup
         (Utility.base64decode pickled_continuation))
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
        fst (deserialise_environment primitive_lookup (Utility.base64decode pickled_environment))
    and expression = (match fst (deserialise_result primitive_lookup (Utility.base64decode pickled_expression)) with
                          | `Function (_, _, _, p) -> p
                          | _ -> failwith "Type error unpickling expression")
    in
      debug $ string_of_environment environment;
      Some (ExprEnv (expression, string_dict_to_charlist_dict params
                       @ environment))
        
  with Not_found -> None

let is_remote_call params = 
  mem_assoc "__name" params && mem_assoc "__args" params (* && mem_assoc "t" params*)
