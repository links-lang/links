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
open Syntax
open Result

(* Walk the XML tree, looking for <input l:name> bindings that are
   inside the top <form> element, with no intervening <form>s.
*)
let lname_bound_vars : 'a expression' -> string list = 
  let desyntax = function
    | String (name, _) -> name
    | other -> raise (Parse_failure (Sugar._DUMMY_POS, string_of_expression other ^ " cannot be bound with l:name")) in
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

let is_pfunc = function
  | (_, `Primitive(`PFunction _)) -> true
  | _ -> false

let string s = 
  String (s, (Sugar._DUMMY_POS, Kind.string_type, None))

let hidden_input name value = 
  Xml_node ("input", [("type", string "hidden");
                      ("name", string name);
                      ("value", string value)], [], 
            (Sugar._DUMMY_POS, `Collection (`List, `Primitive `XMLitem), None))

let attrname = fst
let attrval = snd

let serialise_env_nopfuncs env =
  serialise_environment (filter (not -<- is_pfunc) env)

let serialize_exprenv expr env =
  let env = strip_env env in
  let env = retain (freevars expr) env in
  let thunk = delay_expr expr in
  let thunk = strip_result thunk in
  (Utility.base64encode (serialise_result thunk),
   Utility.base64encode (serialise_env_nopfuncs env))


let rec is_trivial_apply_aux = function
    Variable(f, _) -> true
  | Apply(e, Variable(x, _), _) -> is_trivial_apply_aux e
  | _ -> false

let is_trivial_apply = function
  | Variable _ -> false
  | expr -> is_trivial_apply_aux expr


let rec is_simple_apply_aux = function
    Variable(f, _) -> true
  | Apply(e, _, _) -> is_simple_apply_aux e
  | _ -> false

let is_simple_apply = function
  | Variable _ -> false
  | expr -> is_simple_apply_aux expr


let rec ultraclean_serialize_apply = function
    Variable(f, _) -> [f]
  | Apply(e, Variable(x, _), _) -> ultraclean_serialize_apply e @ [x]


let rec list_of_appln = function
    Variable _ as v -> [v]
  | Apply(e, arg, _) -> list_of_appln e @ [arg]


let rec simplify lookup = function
  | Variable (x, _) as expr -> 
      (try
         match expr_of_prim_val(lookup x) with
             Some expr -> expr
           | None -> expr
       with
           Not_found -> failwith("at runtime, " ^ x ^ 
                                   " was not declared."))
  | Apply(f, a, d) -> Apply(simplify lookup f, simplify lookup a, d)
  | expr -> expr

let rec clean_serialize_apply = function
    Variable(f, _) -> [f]
  | Apply(e, arg, _) -> 
      clean_serialize_apply e @ [serialise_expression arg]

let is_constant expr = false

(** val_of_const_expr
    Given an expression with a constant value, reduce it to that value.
*)
let val_of_const_expr expr = `Primitive(`Bool false)

let rec value_of_simple_expr lookup = function
  | expr when is_constant(expr) -> Some(val_of_const_expr(expr))
  | Variable(x, _) -> Some(lookup x)
  | Boolean _ | Integer _ | Char _ | Float _ as expr -> prim_val_of_expr expr
  | expr -> Some(delay_expr expr)

exception UnplainResult

let plain_serialise_result = function  
    `Primitive(`Bool b) -> if b then "t" else "f"
  | `Primitive(`Int i) -> string_of_num i
  | `Primitive(`Char ch) -> String.make 1 ch
  | `Primitive(`Float f) -> string_of_float f
  | _ -> raise UnplainResult

let plain_deserialise_result str = 
  match str with
      "t" -> `Primitive(`Bool true)
    | "f" -> `Primitive(`Bool false)
    | str when (Str.string_match (Str.regexp "^(\+|-)?[0-9]+$") str 0) ->
        `Primitive(`Int (int_of_string str))
    | str when (Str.string_match (Str.regexp "^(\+|-)?[0-9]+.[0-9]*(E(+|-)?[0-9*])?$") str 0) -> `Primitive(`Float (float_of_string str))


(* Serialise the continuation and environment, and adjust the form accordingly *)
let xml_transform env lookup eval : expression -> expression = 
  function 
    | Xml_node ("form", attrs, contents, data) as form ->
        (try 
           let laction = either_assoc "l:onsubmit" "l:handler" attrs in
           let new_fields = match laction with 
               (* l:action holds a frozen expression *)
             | Left laction -> 
                 let expr_str, env_str = serialize_exprenv laction env in
                   [hidden_input "expression%" expr_str;
                    hidden_input "environment%" env_str]

             (* an l:handler attribute holds an expression that
                evaluates to a continuation. This continuation will be
                applied to a record representing the form values, when
                the form is submitted.  *)
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
           | String (name, _) -> Xml_node(tag, substitute (((=)"l:name") -<- attrname) ("name", string name) attrs, contents, data)
           | _ -> failwith "Internal error transforming xml"
         with Not_found -> input)
    | Xml_node ("a", attrs, contents, data) ->
        let href_expr = assoc "l:href" attrs in
        let ser_expr, ser_env = serialize_exprenv href_expr env in
        let href_val = 
(*           if is_simple_apply href_expr then *)
(*             let (Variable (func, _)::args) = list_of_appln href_expr in *)
(*             let arg_vals = map (value_of_simple_expr lookup) args in *)
(*               String.concat "/" (func :: map (plain_serialise_result -<- valOf) arg_vals) *)
(* (\*               ^ "?environment%=" ^ ser_env *\) *)
(*           else  *)
            "?environment%=" ^ ser_env ^
              "&expression%=" ^ ser_expr
        in
        let attrs = substitute (((=)"l:href") -<- attrname) ("href", string href_val) attrs 
        in
          Xml_node ("a", attrs, contents, data)
