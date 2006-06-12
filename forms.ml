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

(* exception InvalidLNameExpr of 'a expression' *)

(* Walk the XML tree, looking for <input l:name> bindings that are
   inside the top <form> element, with no intervening <form>s.
*)
let lname_bound_vars : 'a expression' -> string list = 
  let rec lnames = function
    | Xml_node (("input"|"textarea"|"select"), attrs, contents, _) ->
        (try 
          let lname_attr = assoc "l:name" attrs in 
            (try
               [stringlit_value(lname_attr)]
             with
                 (* TBD: we need a way to extract position information 
                    from a typed or untyped expression in an ad-hoc 
                    polymorphic way. *)
               | Match_failure _ ->failwith("l:name attribute was not a string: "
                                           ^ string_of_expression lname_attr))
        with Not_found -> concat (map lnames contents))
    | Xml_node ("form", _, _, _) -> (* new scope *) []
    | Xml_node (_, _, contents, _) -> concat (map lnames contents)
    | Concat (l, r, _) -> lnames l @ lnames r
    | _ -> [] 
  in function
    | Xml_node ("form", _, contents, _)  ->
        concat (map lnames contents)
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
  | (_, `PFunction _) -> true
  | _ -> false

let string s = 
  String (s, (Sugar._DUMMY_POS, Types.string_type, None))

let hidden_input name value = 
  Xml_node ("input", [("type", string "hidden");
                      ("name", string name);
                      ("value", string value)], [], 
            (Sugar._DUMMY_POS, `List (`Primitive `XMLitem), None))

let attrname = fst
let attrval = snd

let serialize_exprenv expr env =
  let env = retain (freevars expr) env in
  let thunk = delay_expr expr in
  (marshal_result thunk, marshal_environment env)

let rec is_trivial_apply_aux = function
    Variable(_, _) -> true
  | Apply(e, Variable(_, _), _) -> is_trivial_apply_aux e
  | _ -> false

let is_trivial_apply = function
  | Variable _ -> false
  | expr -> is_trivial_apply_aux expr


let rec is_variable = function
    Variable _ -> true
  | _ -> false

let rec is_simple_apply_aux = function
    Variable(_, _) -> true
  | Apply(e, a, _) -> is_simple_apply_aux e && (is_variable a(*  || is_tuple a *))
  | _ -> false

let is_simple_apply = function
  | Variable _ -> false
  | expr -> is_simple_apply_aux expr

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

let is_constant _ = false

(** val_of_const_expr
    Given an expression with a constant value, reduce it to that value.
*)
let val_of_const_expr _ = `Bool false

let rec value_of_simple_expr lookup = function
  | expr when is_constant(expr) -> Some (val_of_const_expr expr)
  | Variable(x, _) -> Some (lookup x)
  | Boolean _ | Integer _ | Char _ | Float _ as expr -> prim_val_of_expr expr
  | expr -> Some(delay_expr expr)

exception UnplainResult

let plain_serialise_result = function  
    (`Bool b) -> if b then "t" else "f"
  | (`Int i) -> string_of_num i
  | (`Char ch) -> String.make 1 ch
  | (`Float f) -> string_of_float f
  | `Function _ (*as f -> Pickle_result.pickleS f*)
  | _ -> raise UnplainResult

let plain_deserialise_result str = 
  match str with
      "t" -> `Bool true
    | "f" -> `Bool false
    | str when (Str.string_match (Str.regexp "^(\+|-)?[0-9]+$") str 0) -> `Int (int_of_string str)
    | str when (Str.string_match (Str.regexp "^(\+|-)?[0-9]+.[0-9]*(E(+|-)?[0-9*])?$") str 0) -> `Float (float_of_string str)


(* Serialise the continuation and environment, and adjust the form accordingly *)
let xml_transform env lookup eval : expression -> expression = 
  function 
    | Xml_node ("form", attrs, contents, data) as form ->
        let new_field = 
          match List.find_all (fst ->- flip List.mem ["l:onsubmit"; "l:handler"]) attrs with 
            | [] -> []
            | ("l:onsubmit", laction)::_ -> (* l:action holds a frozen expression *)
                [hidden_input "expression%" (marshal_result (delay_expr laction));
                 hidden_input "environment%" (marshal_environment env)]
            | ("l:handler", lhandler)::_ -> 
                (* an l:handler attribute holds an expression that
                   evaluates to a continuation. This continuation will
                   be applied to a record representing the form values,
                   when the form is submitted.  *)
                (match eval lhandler [] with
                   | `Continuation c ->
                       [hidden_input "continuation%" (marshal_continuation c)]
                   | _ -> failwith "Internal error: l:handler was not a continuation")
        in
          Xml_node ("form",
                    substitute (attrname ->- flip List.mem ["l:onsubmit"; "l:handler"]) ("action", string "#") attrs, 
                    new_field @ contents, 
                    data)

    | Xml_node (("input"|"textarea"|"select") as tag, attrs, contents, data) as input ->
        (try match assoc "l:name" attrs with
           | String (name, _) -> Xml_node (tag, substitute (attrname ->- (=) "l:name") ("name", string name) attrs, contents, data)
           | _ -> failwith ("Internal error transforming xml (no l:name found on " ^ tag)
         with Not_found -> input)

    | Xml_node ("a", attrs, contents, data) ->
        let href_expr = assoc "l:href" attrs in
        let ser_expr, ser_env = serialize_exprenv href_expr env in
        let href_val = 
          if is_simple_apply href_expr then
            let (Variable (func, _)::args) = list_of_appln href_expr in
            let arg_vals = map (value_of_simple_expr lookup) args in
              String.concat "/" (func :: map (plain_serialise_result -<- valOf) arg_vals)
                (*               ^ "?environment%=" ^ ser_env *)
          else
            "?environment%=" ^ ser_env ^ "&expression%=" ^ ser_expr
        in
        let attrs = substitute (((=)"l:href") -<- attrname) ("href", string href_val) attrs 
        in
          Xml_node ("a", attrs, contents, data)
