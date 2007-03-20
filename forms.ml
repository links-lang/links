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

let is_special x = String.length x > 2 && String.sub x 0 2 = "l:"

let islform : 'data expression' -> bool = function
  | Xml_node ("form", attrs, _, _) 
      when List.exists (is_special -<- fst) attrs -> true
  | _ -> false

(** islhref
    should be something like is_transformable_anchor *)
let islhref : 'data expression' -> bool = function
  | Xml_node ("a", attrs, _, _) 
      when exists (fun (k,_) -> start_of k ~is:"l:") attrs -> true
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
  String (s, (`T (Syntax.dummy_position, Types.string_type, None)))

let hidden_input name value = 
  Xml_node ("input", [("type", string "hidden");
                      ("name", string name);
                      ("value", string value)], [], 
            (`T (Syntax.dummy_position, Types.xml_type, None)))

let attrname = fst
let attrval = snd

let serialise_exprenv expr env : string =
  let env = retain (freevars expr) env in
(*  let thunk = delay_expr expr in*)
    marshal_exprenv (expr, env)
(*  (marshal_result thunk, marshal_environment env)*)

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

(* Serialise the continuation and environment, and adjust the form accordingly *)
let xml_transform env lookup eval : expression -> expression = 
  function 
    | Xml_node ("form", attrs, contents, data)  ->
        let new_field = 
          match List.find_all (fst ->- is_special) attrs with 
            | [] -> []
            | ("l:action", laction)::_ ->
                [hidden_input "_k" (serialise_exprenv laction env)]
            | ("l:onsubmit", laction)::_ -> (* l:onsubmit holds a frozen expression *)
                [hidden_input "_k" (serialise_exprenv laction env)]
            | ("l:handler", lhandler)::_ -> 
                (* an l:handler attribute holds an expression that
                   evaluates to a continuation. This continuation will
                   be applied to a record representing the form values,
                   when the form is submitted.  *)
                (match eval lhandler [] with
                   | `Continuation c ->
                       [hidden_input "_cont" (marshal_continuation c)]
                   | _ -> failwith "Internal error: l:handler was not a continuation")
        in

        let attrs = (("action", string "#") 
                     :: filter (attrname ->- is_special ->- not) attrs) in
          Xml_node ("form",
                    attrs,
                    new_field @ contents, 
                    data)

    | Xml_node (("input"|"textarea"|"select") as tag, attrs, contents, data) as input ->
         let rec extract = function
           | HasType (e, _, _) -> extract e
           | String (name, _) -> 
               let attrs = (("name", string name) :: 
                              filter (attrname ->- (=) "l:name" ->- not) attrs) in
               Xml_node (tag, attrs, contents, data)
           | e -> failwith ("Internal error transforming xml (no l:name found on " ^ tag
                           ^ "(expression: " ^ string_of_expression e ^ ")")
         in
           (try extract (assoc "l:name" attrs)
            with Not_found -> input)

    | Xml_node ("a", attrs, contents, data) ->
        let href_expr = assoc "l:href" attrs in
        let href_val = 
(*           if is_simple_apply href_expr then *)
(*             let (Variable (func, _)::args) = list_of_appln href_expr in *)
(*             let arg_vals = map (value_of_simple_expr lookup) args in *)
(*               String.concat "/" (func :: map (plain_serialise_result -<- valOf) arg_vals) *)
(*                 (\*               ^ "?environment%=" ^ ser_env *\) *)
(*           else *)
          "?_k=" ^ serialise_exprenv href_expr env
        in
        let attrs = (("href", string href_val)
                     :: filter (not -<- is_special -<- attrname) attrs )
        in
          Xml_node ("a", attrs, contents, data)
