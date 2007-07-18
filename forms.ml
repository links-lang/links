(* Special functionality for HTML forms:  l:action etc. *)

(* What are the changes in a form transformation?

   1. Addition of an environment hidden field.  This must
      be done at runtime, when the environment is known.

      We could hack it, though, by returning a function which takes an
      internal environment object, then invoke the function in the
      interpreter.  NB: javascript, etc.

   2. Replacement of l:action={code} with action='...' and moving the serialised 
      l:action code into a hidden field in the body of the form.
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

let add_attrs new_attrs = function
   | Xml_node (tag, attrs, c, d) ->
       Xml_node (tag, attrs @ new_attrs, c, d)
   | o -> failwith ("Non-XML structure passed to add_attrs : " ^
                      string_of_expression o)

let is_pfunc = function
  | (_, `PFunction _) -> true
  | _ -> false

let string s = 
  Constant(String s, (`T (Syntax.dummy_position, Types.string_type, None)))

let hidden_input name value = 
  Xml_node ("input", [("type", string "hidden");
                      ("name", string name);
                      ("value", string value)], [], 
            (`T (Syntax.dummy_position, Types.xml_type, None)))

let attrname = fst
let attrval = snd

let serialise_exprenv expr env : string =
  let env = retain (freevars expr) env in
    marshal_exprenv (expr, env)

let rec is_trivial_apply_aux = function
    Variable(_, _) -> true
  | Apply(e, [Variable(_, _)], _) -> is_trivial_apply_aux e
  | _ -> false

let is_trivial_apply = function
  | Variable _ -> false
  | expr -> is_trivial_apply_aux expr


let rec is_variable = function
    Variable _ -> true
  | _ -> false

let rec is_simple_apply_aux = function
    Variable(_, _) -> true
  | Apply(e, [a], _) -> is_simple_apply_aux e && (is_variable a(*  || is_tuple a *))
  | _ -> false

let is_simple_apply = function
  | Variable _ -> false
  | expr -> is_simple_apply_aux expr

let rec list_of_appln = function
    Variable _ as v -> [v]
  | Apply(e, args, _) -> list_of_appln e @ args

let rec simplify lookup = function
  | Variable (x, _) as expr -> 
      (try
         match expr_of_prim_val(lookup x) with
             Some expr -> expr
           | None -> expr
       with
           Not_found -> failwith("at runtime, " ^ x ^ 
                                   " was not declared."))
  | Apply(f, a, d) -> Apply(simplify lookup f, List.map (simplify lookup) a, d)
  | expr -> expr

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
                failwith("onsubmit gen'd at server not impl.");
            | ("l:handler", lhandler)::_ -> 
                (* an l:handler attribute holds an expression that
                   evaluates to a continuation. This continuation will
                   be applied to a record representing the form values,
                   when the form is submitted.  *)
                (match eval lhandler Result.toplevel_cont with
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

    | Xml_node ("a", attrs, contents, data) ->
        let href_expr = assoc "l:href" attrs in
        let href_val = 
          "?_k=" ^ serialise_exprenv href_expr env
        in
        let attrs = (("href", string href_val)
                     :: filter (not -<- is_special -<- attrname) attrs )
        in
          Xml_node ("a", attrs, contents, data)
