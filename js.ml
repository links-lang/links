(* js.ml
    JavaScript generation.
*)

open Num
open Netencoding
open List

open Pickle
open Forms
open Debug
open Utility
(*open Type *)
open Syntax

(* Intermediate language *)
type code = | Var   of string
            | Lit   of string
            | Defs  of ((string * code) list)
            | Fn    of (string list * code)
            | Call  of (code * code list)
            | Binop of (code * string * code)
            | Cond  of (code * code * code)
            | Dict  of ((string * code) list)
            | Lst   of (code list)
            | Bind  of (string * code * code)
            | Seq   of (code * code)
            | Nothing


(*
  Runtime required (any JavaScript functions used /must/ be documented here!)

  _concat(a, b)
     concatenate two sequences: either strings or lists
  _accum(f, i)
    concatMap: apply f to every element of the sequence `i' and concatenate the results.
  _plus, _minus, etc.
    curried function versions of the standard arithmetic operators
  _XML(tag, attrs, children)
    create a DOM node with name `tag'
                       and attributes `attrs' (a dictionary)
                       and children `children' (a sequence of DOM nodes and strings)    
  _extend(record, tag, value)
    extend a record (dictionary) with a new field (label `tag'; value `value').
    Don't update the old record.
  _project(record, tag, value)
    project a field of a record

  _start(tree)
    Replace the current page with `tree'.
  _registerFormAction(continuation)
    Register a continuation function; return an id.
  _continuations
    Table of continuation functions, indexed by id.

  Also, any `builtin' functions from Library.value_env.
 *)

let jsthunk expr = Fn([], expr)

(* local_names

   Retrieve all let bindings within a function.  Don't descend into
   inner function scopes.
*)
let rec local_names : code -> string list = function
  | Var _
  | Lit _
  | Fn _
  | Nothing -> []
  | Call (c, cs) -> local_names c @ List.concat (map local_names cs)
  | Cond (a, b, c) -> local_names a @ local_names b @ local_names c
  | Dict cs -> List.concat (map (local_names -<- snd) cs)
  | Lst  cs -> List.concat (map local_names cs)
  | Binop (l, _, r)
  | Seq (l, r) -> local_names l @ local_names r
  | Bind (l, c1, c2) -> l :: local_names c1 @ local_names c2
  | Defs (bs) -> map fst bs

(* Generate code from intermediate language *) 
let rec show : code -> string = 
  let show_func name (Fn (vars, body)) = 
    "function "^ name ^"("^ String.concat ", " vars ^")"
    ^"{ "^
      (let names = String.concat ", " (local_names body) in
	if names = "" then "" else "var " ^ names ^ ";\n")
    ^"return "^ show body 
    ^"; }" 
  and arglist args = String.concat ", " (map show args) 
  and paren = function
    | Var _
    | Lit _
    | Call _
    | Dict _
    | Lst _
    | Seq _
    | Bind _
    | Nothing as c -> show c
    | c -> "(" ^ show c ^ ")" in
  let show_def = function
      | name, (Fn _ as f) -> show_func name f
      | name, Bind (v, (Fn _ as f), Var v') when v = v'  -> show_func (*name*) v f ^ "\nvar " ^ name ^ " = " ^ v
      | name, value -> "var " ^ name ^ " = " ^ show value in
    function
      | Var s -> s
      | Lit s -> s
      | Defs (defs) -> String.concat ";\n" (map show_def defs) ^ ";"
      | Fn _ as f -> show_func "" f
      | Call (Var "_project", [label; record]) -> (paren record) ^ "[" ^ show label ^ "]"
      | Call (Var "_call", [f; a; k]) -> "_call(" ^ paren f ^ ", " ^ paren a ^ ",\n" ^ paren k ^ ")"
      | Call (fn, args) -> paren fn ^ "(" ^ arglist args  ^ ")"
      | Binop (l, op, r) -> paren l ^ " " ^ op ^ " " ^ paren r
      | Cond (if_, then_, else_) -> "(" ^ show if_ ^ " ? " ^ show then_ ^ " : " ^ show else_ ^ ")"
      | Dict (elems) -> "{" ^ String.concat ", " (map (fun (name, value) -> "'" ^ name ^ "' : " ^ show value) elems) ^ "}"
      | Lst [] -> "[]"
      | Lst elems -> "[" ^ arglist elems ^ "]"
      | Bind (name, value, body) -> "("^ name ^" = "^ 
          show value ^", "^ show body ^")"
      | Seq (l, r) -> "(" ^ show l ^ ", " ^ show r ^ ")"
      | Nothing -> ""

(* create a string literal, quoting special characters *)
let string_quote s = 
  let sub old repl s = Str.global_replace (Str.regexp old) repl s in
    "'" ^ sub "'" "\\'" (sub "\n" "\\n" s) ^ "'"
      
let strlit s = Lit (string_quote s)
let chrlit s = Lit (string_quote (string_of_char s))

(* Specialness:

   * Top-level boilerplate code to replace the root element and reset the focus

     The special function _start takes an html page as a string and
     replaces the currently displayed page with that one.

     Some of the other functions are equivalents to Links builtins
     (e.g. int_of_string, xml)
 *)

let boiler_1 = "<html>
       <head>
          <script type='text/javascript' src=\"lib/json.js\"></script>
          <script type='text/javascript' src=\"lib/regex.js\"></script>
          <script type='text/javascript' src=\"lib/yahoo/YAHOO.js\"></script>
          <script type='text/javascript' src=\"lib/yahoo/event.js\"></script>
          <script type='text/javascript' src=\"lib/jslib.js\"></script>
          <script type='text/javascript'><!-- \n"
and boiler_2 =    "\n--> </script>
      </head>
      <!-- $Id$ -->
      <body><script type='text/javascript'>" 
and  boiler_3 = "</script></body>
   </html>"

(* Operators are represented as functions in the interpreter, but
   operator names aren't valid JS function names.*)
let builtins = ["+", "_plus";
                "+.", "_plus";
                "-", "_minus";
                "-.", "_minus";
                "*", "_times";
                "*.", "_times";
                "/", "_divide";
                "/.", "_divide"]

let binop_name op = 
  try
    assoc op ["+",  "+";
              "+.", "+";
              "-",  "-";
              "-.", "-";
              "*",  "*";
              "*.", "*";
              "/",  "/";
              "/.", "/";
              "==", "==";
              "<>", "!=";
              "<",  "<";
              ">",  ">";
              "<=", "<=";
              ">=", ">=";
	     ]
   with Not_found as e ->  failwith ("Notfound : " ^ op)
      

  
let rename_builtins name =
  try assoc name builtins
  with Not_found -> name

(* Convert colons in qualified names to triple-underscores *) 
let rename_prefixed name = Str.global_replace (Str.regexp ":") "___" name


(* Convert a function that takes a single tuple argument into a
   multi-argument function *)
let untuple_def =
  let rec arglist argvar = 
    (function
	 (* This is very fragile.  We shouldn't assume that the
	    argument destructuring is in a particular form, as this
	    code does.  Rather, we should define a normal form for our
	    AST and make sure it's in that form by this point.  *)
       | Record_selection (lab, 
			   labvar, 
			   restvar, 
			   Variable (avar, _), 
			   Let (x, Variable (y, _), body,_),
			   _) 
	   when numberp lab
	     && y = labvar
	     && argvar = avar
	     (* and restvar is not free in body *)
	     ->  (let vars, body = arglist restvar body in
		    x :: vars, body)
       | Record_selection (lab, 
			   labvar, 
			   restvar, 
			   Variable (avar, _), 
			   body,_)
	   when numberp lab
	     && argvar = avar
	     (* and restvar is not free in body *)
	     ->  (let vars, body = arglist restvar body in
		    labvar :: vars, body)
       | Record_selection_empty (Variable (avar, _), body, _) 
	   when avar = argvar 
	     -> [], body
       | e -> failwith ("Error unpacking arglist: " ^ Show_expression.show e)) in
    function
      | Abstr (var, (Record_selection _ as body), _) -> arglist var body
      | Abstr (var, (Record_selection_empty _ as body), _) -> [], body
      | Abstr (var, body, _) -> [var], body
      | e -> failwith ("Error unpacking arglist " ^ Show_expression.show e)

(* Convert an application of a function to a tuple into an application
   of a function to several arguments *)
let untuple_call = 
  let rec arglist = function
    | Record_empty _ -> []
    | Record_extension (label, value, record, _) when numberp label -> value :: arglist record
    | arg -> [arg]
  in arglist    (* ?? *)

let strip_lcolon evName = 
  String.sub evName 2 ((String.length evName) - 2)

(* Generate a stub that calls the corresponding server function *)
let generate_stub = function
  | Define (n, Rec ([_, (Abstr _ as f)], Variable _, _), `Server, _) ->
      let arglist, _ = untuple_def f in
        Defs [n, Fn(["__kappa"], Fn (arglist, 
                 Call(Call (Var "_remoteCall", [Var "__kappa"]),
                      [strlit n; Dict (
                         List.map2
                           (fun n v -> string_of_int n, Var v) 
                           (Utility.fromTo 1 (1 + List.length arglist))
                           arglist
                       )])))]
  | e
    -> failwith ("Cannot generate stub for " ^ string_of_expression e)

let trivial_cps expr = 
  Fn(["__kappa"], Call(Var "__kappa", [expr]))

(* let idy_js = Fn(["x"], Var "x")*)
let idy_js = Var("_idy")


(* THIS IS NOT CAPTURE-AVOIDING ALPHA-CONVERSION! *)
let rec rename' renamer = function
  | Var x -> Var (renamer x)
  | Defs defs -> Defs(map (fun (x, body) -> renamer x, rename' renamer body) defs)
  | Fn(args, body) -> Fn(map renamer args, rename' renamer body)
  | Call(func, args) -> Call(rename' renamer func,
                             map (rename' renamer) args)
  | Binop(lhs, op, rhs) -> Binop(rename' renamer lhs, op,
                                 rename' renamer rhs)
  | Cond(test, yes, no) ->  Cond(rename' renamer test,
                                 rename' renamer yes,
                                 rename' renamer no)
  | Dict(terms) -> Dict(alistmap (rename' renamer) terms)
  | Lst(terms) -> Lst(map (rename' renamer) terms)
  | Bind(name, expr, body) -> Bind(renamer name, rename' renamer expr, 
                                   rename' renamer body)
  | Seq(first, second) -> Seq(rename' renamer first,
                              rename' renamer second)
  | simple_expr -> simple_expr
and rename renamer body = rename' renamer body



(* let yield_and_call(func, args, kappa) = *)
(*   Call(Var("_yield"), [jsthunk(Call(Call(func, [kappa]), args))]) *)

let make_xml_cps attrs_cps attrs_noncps children_cps children_noncps tag = 
  let innermost_expr = 
    Call(Call (Var "_XML",
               [Var "__kappa"]),
         [strlit tag;
          Dict (attrs_noncps @ map (fun (k, n, _) -> (k, Var n)) attrs_cps);
          Lst (children_noncps @ map (fun (n, _) -> Var n) children_cps)])
  in
  let tower = fold_right (fun (name, item) expr ->
                            Call(item, [Fn([name], expr)])
                         ) children_cps innermost_expr in
  let tower = fold_right (fun (aname, vname, item) expr ->
                            Call(item, [Fn([vname], expr)])
                         ) attrs_cps tower in
    Fn(["__kappa"], tower)
      

(** generate
    Generate javascript code for a Links expression
    
    With CPS transform, result of generate is always : (a -> w) -> b
*)
let rec generate : 'a expression' -> code = 
  let rec reduce_list : 'a expression' -> code = 
    function
      | Concat (c, Nil _, _) -> 
          reduce_list c
      | Concat (l, r, _) ->
          let l_cps = generate l in
          let r_cps = generate r in
            Fn(["__kappa"],
               Call(l_cps, [Fn(["__l"],
                   Call(r_cps, [Fn(["__r"],
                        Call(Var "__kappa", 
                             [Call (Var "_concat", [Var "__l"; Var "__r"])]))]))])) 
      | Concat (List_of (l,_), r, _) -> 
          failwith "unimpl"; 
	  (match reduce_list r with
	     | Lst items -> Lst (generate_noncps l :: items)
	     | c         -> Call (Var "_concat", [Lst [generate_noncps l]; c]))
      | Concat (l, List_of (r,_), _) -> 
          failwith "unimpl"; 
	  (match reduce_list l with
	     | Lst items -> Lst (items @ [generate_noncps r])
	     | c         -> Call (Var "_concat", [c; Lst [generate_noncps r]]))
      | e ->          
          (* failwith "unimpl"; *)
          generate e
  in
    function
  | Integer (v, _)                     -> trivial_cps (Lit (string_of_num v))
  | Float (v, _)                       -> trivial_cps (Lit (string_of_float v))
  | Boolean (v, _)                     -> trivial_cps (Lit (string_of_bool v))
  | Char (v, _)                        -> trivial_cps (chrlit v)
  | String (v, _)                      -> trivial_cps (strlit v)
  | Condition (i, t, e, _)             -> 
      let i_cps = generate i in
      let t_cps = generate t in
      let e_cps = generate e in
        Fn(["__kappa"], 
           Call(i_cps, [Fn(["__i"], Cond (Var "__i",
                                        Call(t_cps, [Var "__kappa"]),
                                        Call(e_cps, [Var "__kappa"])))]))
                          
  | Let (v, e, b, _)                   -> 
      let e' = generate e in
      let b' = generate b in
      let x = gensym ("__" ^ v) in
        Fn(["__kappa"], 
           Call(e', [Fn([x], 
                        Bind(v, Var x,
                             Call(b', [Var "__kappa"])))]))
  | Variable ("~", _)                  -> trivial_cps (Var "tilde")
  | Variable (v, _)                    -> trivial_cps (Var v)
  | Comparison (l, "==", r, _)         -> 
      let l_cps = generate l in
      let r_cps = generate r in
        Fn(["__kappa"],
           Call(l_cps, [Fn(["__l"],
              Call(r_cps, [Fn(["__r"],
                 Call(Var "__kappa", [Call(Var "_eq", [Var "__l"; Var "__r"])]))]))]))
  | Comparison (l, op, r, _)           -> 
      let l_cps = generate l in
      let r_cps = generate r in
        Fn(["__kappa"],
           Call(l_cps, [Fn(["__l"], 
                Call(r_cps, [Fn(["__r"],
                     Call(Var "__kappa", 
                          [Binop(Var "__l", binop_name op, Var "__r")]))]))]))
      (* Should strings be handled differently at this level? *)
  | Nil _                 -> trivial_cps (Lst [])
  | List_of (e, _)        -> 
      let content_cps = generate e in
        Fn(["__kappa"],
           (Call(content_cps, [Fn(["__x"], 
                                  (Call(Var "__kappa", [Lst [Var "__x"]])))])))
  | (Concat _) as c          -> 
      reduce_list c
(*  | Concat (l, r, _)         -> Call (Var "_concat", [generate l; generate r])*)
  | For (e, v, b, _)  -> 
      let b_cps = generate b in
      let e_cps = generate e in
        Fn(["__kappa"],
           Call(b_cps, [Fn(["__b"],
                           Call(Call(Var "_accum", [Var "__kappa"]),
                                [Fn(["__kappa"], 
                                    Fn([v], Call(e_cps, [Var "__kappa"])));
                                 Var "__b"]))]))
  | Xml_node _ as xml when isinput xml -> lname_transformation xml
  | Xml_node _ as xml -> laction_transformation xml
  | Xml_node (tag, attrs, children, _)   -> 
      let attrs_cps = map (fun (k,e) -> (k, gensym "__", generate e)) attrs in
      let children_cps = map (fun e -> (gensym "__", generate e)) children in
        make_xml_cps attrs_cps [] children_cps [] tag

  (* Functions *)
  | Abstr _ as a -> 
      let arglist, body = untuple_def a
      in Fn(["__kappa"], 
            Call(Var "__kappa", 
		 [Fn(["__kappa"],
		     Fn (arglist, Call(generate body, [Var "__kappa"])))]))

  | Apply (Apply (Variable (op, _), l, _), r, _) when mem_assoc op builtins -> 
      let l_cps = generate l in
      let r_cps = generate r in
        Fn(["__kappa"], 
           Call(l_cps, [Fn(["__l"],
                Call(r_cps, [Fn(["__r"],
                     Call(Var "__kappa",
                          [Binop (Var "__l", binop_name op, Var "__r")]))]))]))

  | Apply (Variable ("domutate", _), p, _) -> 
      let p_cps = generate p in
        Fn(["__kappa"], 
           Call(p_cps, [Fn(["__p"],
                          Call(Var "__kappa",
                               [Call(Var "_applyChanges", [Var "__p"])]))]))

  | Apply (f, p, _  ) -> 
      let kappa = Var("__kappa") in
      let f_cps = generate f in
      let f_name = gensym "__f" in
      let arglist = untuple_call p in
      let cps_args = map generate arglist in
      let arg_names = map (fun e -> gensym "__f") arglist in
      let wrap_cps_terms (arg_cps, arg_name) expr = 
        Call(arg_cps, [Fn ([arg_name], expr)])
      in
      let innermost_call = Call(Var "_call",
                                [Var f_name; 
                                 Lst(map (fun name -> Var name) arg_names); 
                                 kappa]
                               ) in
      let arg_tower = fold_right wrap_cps_terms 
                                 (combine cps_args arg_names)
                                 innermost_call in
        
        Fn (["__kappa"],  Call(f_cps, [Fn ([f_name], arg_tower)]))

  (* Binding *)
  | Define (_, _, `Server, _) as d      -> generate_stub d
  | Define (n, e, (`Client|`Unknown), _)-> 
      Defs ([n, Call(generate e,
		     [Var "_idy"])])   (* definitions are always top 
					   level, right? *)
  | Rec (bindings, body, _) ->
      Fn(["__kappa"],
	 (fold_right 
            (fun (v, e) body ->
	       Call(generate e, [Fn(["__e"],
				    Bind (v, Var "__e", body))]))
            bindings
            (Call (generate body, [Var "__kappa"]))))
        
  (* Records *)
  | Record_empty _                    -> trivial_cps (Dict [])
  | Record_extension (n, v, r, _)     -> 
      let r_cps = generate r in
      let v_cps = generate v in
      let extension_val = Call (Var "_extend", [Var "__r"; strlit n; Var "__v"])
      in
        Fn(["__kappa"], Call(r_cps, [Fn(["__r"], 
                      Call(v_cps, [Fn(["__v"],
                                  Call(Var "__kappa",
                                       [extension_val])
                                     )]))]))
  | Record_selection_empty (Variable _, b, _)  -> 
      generate b
  | Record_selection_empty (v, b, _)  -> 
      let v_cps = generate v in
      let b_cps = generate b in
        Fn(["__kappa"], Call(v_cps, [Fn(["ignored"], 
                                        Call(b_cps, [Fn(["__b"],
                                                        Call(Var "__kappa", [Var "__b"]))]))]))
  | Record_selection (l, lv, etcv, r, b, _) when mem etcv (freevars b) ->
      let r_cps = generate r in
      let b_cps = generate b in
      let name = gensym "__r" in
        Fn(["__kappa"],
           Call(r_cps, [Fn (["__r"], 
                (Bind (name, Var "__r", 
                       Bind (lv, 
                             Call (Var "_project",
                                   [strlit l; Var name]),
                             Bind (etcv, 
                                   Var name,
                                   Call(b_cps, [Var "__kappa"]))))))]))

  | Record_selection (l, lv, _, v, Variable (lv', _), _) when lv = lv ->
      (* Could use dot-notation instead of project call *)
      let v_cps = generate v in
        Fn(["__kappa"],
           Call(v_cps, [Fn(["__v"],
                Call(Var "__kappa", 
                     [Call (Var "_project", [strlit l; Var "__v"])]))]))
  | Record_selection (l, lv, _, v, b, _) -> (* var unused: a simple projection *)
      failwith("record selection not implemented");
      let v_cps = generate v in
      let b_cps = generate b in
        Fn(["__kappa"],
           Call(v_cps, [Fn(["__v"], 
                Bind (lv,
	              Call (Var "_project", [strlit l; Var "__v"]),
                      Call(b_cps, [Var "__kappa"])))]))
  (* Variants *)
  | Variant_injection (l, e, _) -> 
      let content_cps = generate e in
        Fn(["__kappa"], 
           Call(content_cps, [Fn(["__content"],
                                 Call(Var "__kappa", 
                                      [Dict [("label", strlit l);
                                             ("value", Var "__content")]]))]))
  | Variant_selection_empty (e, _) ->
      Fn(["__kappa"], Call(Var "_fail",
                         [strlit "closed switch got value out of range"]))
  | Variant_selection (src, case_label, case_var, case_body, 
                       else_var, else_body, _) ->
      let src_cps = generate src in
      let case_body_cps = generate case_body in
      let else_body_cps = generate else_body in
        Fn(["__kappa"],
          Call(src_cps, [Fn(["__src"],
                            Cond(Binop(Call(Var "_vrntLbl", [Var "__src"]),
                                       "==",
                                       strlit case_label),
                                 Bind(case_var,
                                      Call(Var "_vrntVal", [Var "__src"]),
                                      Call(case_body_cps, [Var "__kappa"])),
                                 Bind(else_var,
                                      Var "__src",
                                      Call(else_body_cps, [Var "__kappa"]))
                                )
                           )]))
  | Escape (v, e, _) -> 
      Fn(["__kappa"], 
	 Call (Fn ([v], Call(generate e, [Var "__kappa"])), 
	       [
		 Fn (["__ignore"],
		     Var "__kappa")
	       ])
	)
  | Alien _ -> Nothing

  (* Unimplemented stuff *)
  | Database _
  | Table _ as e -> failwith ("Cannot (yet?) generate JavaScript code for " ^ string_of_expression e)
  | x -> failwith("unknown ast object " ^ string_of_expression x)

(* Specialness: 
   * Modify the l:action to pass the continuation to the top-level boilerplate
   * lift the continuation out of the form.

        def f(x) {
          <form name="form1" l:action={foo(x)}>...</form>
        }

     is translated as 
        _continuation_form1 = null;
        function f(x) { 
           return (_continuation_form1 = function () { foo(x) },
                   '<form name="form1" action="#" onSubmit="_start(_continuation_form1()); return false">...</form>')
        }

   The continuation can't be left in the action attribute because it
   might refer to lexical bindings and action is just a string, so
   scope is broken.  (This will need more care for less simple cases,
   e.g. where there are let bindings)
*)
and laction_transformation (Xml_node (tag, attrs, children, _) as xml) = 
  (* 1. Remove l:action from the attrs 
     2. name the form if not named (TODO; not needed for simple example)
     3. Add an appropriate onSubmit to the attrs
     4. Add an appropriate action to the attrs
     5. return a pair of comma-separated expressions that set up the
        continuation and return the mangled form
     6. Replace l:name-bindings within the action with val("name")
     7. need to add the continuation function name to the top level
        (actually not, JavaScript's odd scoping, but it would be nice) 
  *)
  let essentialAttrs = 
    match tag with
        "form" -> ["action", strlit "#";
                   "method", strlit "post"]
      | "a" -> ["href", strlit "#"]
      | _ -> []
  in
    
  let beginswithl str = Str.string_match (Str.regexp "l:") str 0 in
  let handlers, attrs = partition (fun (attr, _) -> beginswithl attr) attrs in
  let vars = Forms.lname_bound_vars xml in
    (* handlerInvoker generates onFoo="..." attributes--the first 
       thing invoked when that event occurs. *)
(*
  let handlerInvoker (evName, _) = 
    (evName, strlit ("_eventHandlers['" ^ evName ^ "'][this.id](event); return false")) in
*)
  (* [BUG] the id tag isn't necessarily static! *)
(*   let elem_id =  *)
(*     try  *)
(*       match (assoc "id" attrs) with *)
(* 	  String(idStr, _) -> strlit idStr *)
(*     with Not_found -> Lit "0" in *)

  let make_code_for_handler (evName, code) = 
    strip_lcolon evName, (fold_left
                            (fun expr var -> Bind (var, Call (Var "_val", [strlit var]), expr))
                            (end_thread(generate code))
                            vars) in
  let handlers = map make_code_for_handler handlers in
  let attrs_cps = map (fun (k, e) -> (k, gensym "", generate e)) attrs in
  let children_cps = map (fun e -> (gensym "", generate e)) children in
    make_xml_cps attrs_cps (["key", 
                             Call(Var "_registerFormEventHandlers",
                                  [Lst (map (fun (evName, code) -> 
                                               Dict(["evName", strlit evName;
                                                     "handler", 
                                                        Fn (["event"], code)]))
                                          handlers)]);
                            ]
                            @ essentialAttrs
                            (* @ map handlerInvoker handlers *)
			   )
      children_cps [] tag

and lname_transformation (Xml_node (tag, attrs, children, d)) = 
  (* 1. Remove l:name from the attrs
     2. Add appropriate id and name to the attrs
     3. Add onFocus handlers
   *)
  let name, attrs = (assoc "l:name" attrs, remove_assoc "l:name" attrs) in 
  let attrs = 
    ("onfocus", Syntax.String ("_focused = this.id", Sugar.no_expr_data))
    :: ("id", name)
    :: ("name", name)
    :: attrs in
    generate (Xml_node (tag, attrs, children, Sugar.no_expr_data))


(* generate_noncps: generates CPS code for expr and immediately 
  gives idy as the cont. *)
and generate_noncps expr = Call(generate expr, [idy_js])
and end_thread expr = Call(expr, [idy_js])
(* generate_easy_cps: generates CPS code for expr and wraps it to
   accept a continuation kappa and immediately pass the result to 
   kappa. should only be used on trivial expressions--w/o subexprs *)
and generate_easy_cps expr = 
        Fn(["__kappa"], Call(Var "__kappa", [generate expr]))
      
module StringSet = Set.Make(String)

let set_from_list l =
  fold_left StringSet.union StringSet.empty (map (StringSet.singleton) l)

let rec freevars = function
  | Var x -> StringSet.singleton x
  | Defs _ -> StringSet.empty
  | Fn(args, body) -> StringSet.diff (freevars body) (set_from_list args)
  | Call(func, args) -> 
      (fold_left StringSet.union  (freevars func) (map freevars args))
  | Binop (lhs, op, rhs) -> StringSet.union (freevars lhs) (freevars rhs)
  | Cond(a, b, c) -> StringSet.union (StringSet.union (freevars a) (freevars b)) (freevars c)
  | Dict(terms) -> fold_left StringSet.union StringSet.empty (map snd (alistmap freevars terms))
  | Lst(terms) ->  fold_left StringSet.union StringSet.empty (map freevars terms)
  | Bind(var, src, body) -> StringSet.union (freevars src) (StringSet.remove var (freevars body))
  | Seq(first, rest) -> StringSet.union (freevars first) (freevars rest)
  | _ -> StringSet.empty

(* FIXME: There is some problem with this whereby variables are captured *)
let rec replace' var replcmt fvs = function
  | Var x when x = var -> replcmt
  | Defs defs -> Defs(alistmap (replace' var replcmt (freevars replcmt)) defs)
  | Fn(args, body) when not(mem var args) -> 
      (* this may be unnecessary, if whole expr. is uniquified previously *)
     let args, body =
        if StringSet.is_empty (StringSet.inter (set_from_list args) fvs)
        then (args, body) else
          uniquify_args(args, body)
     in
        Fn(args, replace' var replcmt fvs body)
  | Call(func, args) -> Call(replace' var replcmt fvs func,
                             map (replace' var replcmt fvs) args)
  | Binop(lhs, op, rhs) -> Binop(replace' var replcmt fvs lhs, op,
                                 replace' var replcmt fvs rhs)
  | Cond(test, yes, no) ->  Cond(replace' var replcmt fvs test,
                                 replace' var replcmt fvs yes,
                                 replace' var replcmt fvs no)
  | Dict(terms) -> Dict(alistmap (replace' var replcmt fvs) terms)
  | Lst(terms) -> Lst(map (replace' var replcmt fvs) terms)
  | Bind(name, expr, body) -> Bind(name, replace' var replcmt fvs expr, 
                                   if name <> var then (* NOT CORRECT! *)
                                     replace' var replcmt fvs body
                                   else body)
  | Seq(first, second) -> Seq(replace' var replcmt fvs first,
                              replace' var replcmt fvs second)
  | simple_expr -> simple_expr
and replace var expr body = replace' var expr (freevars expr) body
and uniquify_args = function
    (args, body) ->
      let subst = map (fun x -> (x, gensym x)) args in
        (map snd subst,
         fold_right (fun (old, noo) body ->
                       replace' old (Var noo) (StringSet.singleton noo) body)
           subst body)


let rec simplify = function
  | Call(Fn([formal_arg], body), [actual_arg]) 
      when Str.string_match (Str.regexp "^__") formal_arg 0
    ->
      replace formal_arg actual_arg body

  | Call(Var "_idy", [arg]) -> arg
      
      (* The other cases are just compatible closure *)
  | Call(f, args) -> Call(simplify f, map (simplify) args )
  | Defs defs -> Defs(alistmap (simplify) defs)
  | Fn(args, body) -> Fn(args, simplify body)
  | Call(func, args) -> Call(simplify func, map (simplify) args)
  | Binop(lhs, op, rhs) -> Binop(simplify lhs, op, simplify rhs)
  | Cond(test, yes, no) ->  Cond(simplify test, simplify yes, simplify no)
  | Dict(terms) -> Dict(alistmap (simplify) terms)
  | Lst(terms) -> Lst(map (simplify) terms)
  | Bind(name, expr, body) -> Bind(name, simplify expr, simplify body)
  | Seq(first, second) -> Seq(simplify first, simplify second)
  | simple_expr -> simple_expr

let rec simplify_completely expr = 
  let expr2 = simplify expr in
    if expr = expr2 then
      expr2
    else
      simplify_completely expr2

let rec eliminate_admin_redexes = 
  simplify_completely (* ->-
     rename (Str.global_replace (Str.regexp "\*\(.*\\)\*") "\1") *)

let gen = 
  Utility.perhaps_apply Optimiser.uniquify_expression
  ->- generate 
  ->- eliminate_admin_redexes
  ->- show

let rec but_last = function [x] -> [] | (x::y::xs) -> x :: but_last(y::xs)

 (* TODO: imports *)
let generate_program environment expression =
  (boiler_1
 ^ String.concat "\n" (map gen (but_last environment))
 ^ boiler_2
 ^ ((generate ->- (fun expr -> Call(expr, [Var "_start"])) ->- eliminate_admin_redexes ->- show) expression)
 ^ boiler_3)

(* FIXME: The tests below create an unnecessary dependency on
   Inference (maybe other modules to? I'd like to remove this. Can we
   move the tests into a different module?
*)

(* *************************************** *)
(*        A simple testing framework       *)
(*                                         *)
(*  usage: add_qtest("links code",         *)
(*                 fun(p) -> false/true    *)
(* p is the gen'd javascript (type `code') *)

let links2js = (Parse.parse_string
                ->- Inference.type_program Library.type_env ->- snd
                  ->- map ((Utility.perhaps_apply Optimiser.uniquify_expression)
                           ->- generate 
                             ->- simplify_completely))
  
let test_list = ref []
let add_test test = test_list := test :: !test_list
let add_qtest (program, pred) =
  add_test (program, 
            fun () ->
              let rslt = links2js program in
              try pred rslt
              with Match_failure _ ->
                prerr_endline("test failed: " ^ 
                                program ^ " compiled to\n" ^ 
                                String.concat "\n" (map show rslt));
                false
           )
let run_tests() =
  ignore(map (fun (name, code) -> 
                if code() then print_endline(name ^ ": ok")
                else print_endline(name ^ ": failed")
             ) !test_list)


(* ******************* *)
(*   Hereafter tests   *)

let _ = add_qtest("1+1",
                  fun rslt ->
                   match rslt with
                       [Fn(["__kappa"], 
                           Call(Var "__kappa", [Binop(Lit "1", "+", Lit "1")]))] -> true
                 )

let _ = add_qtest("fun f(x) { x+1 } f(1)",
                  fun rslt ->
                   match rslt with
                       Defs([_, Bind(fname, Fn(["__kappa"], Fn([xname], 
                                  Call(Var "__kappa",
                                       [Binop(Var xname2, "+", Lit "1")]))),
                                     Var fname2)])::etc
                         when fname = fname2
                           && xname = xname2
                           -> true
                 )



open Inference

let lstrip s = List.hd (Str.bounded_split (Str.regexp "[ \t\n]+") s 1)

let rhino_output linkscode = 
  let gen = show
    -<- generate
    -<- (Utility.perhaps_apply Optimiser.uniquify_expression)
    -<- List.hd -<- snd -<- Inference.type_program Library.type_env -<- Parse.parse_string in
  let tempfile = Filename.temp_file "linkstest" ".js" in
  let cleanup () = (try Sys.remove tempfile with _ -> ()) in
    try
      let channel = open_out tempfile in 
      let s = gen linkscode in
        debugf "generated code for %s:\n%s\n" linkscode s;
        output_string channel s ;
        flush channel;
        let output = process_output ("rhino < " ^ tempfile ^ " 2>&1 | sed '1d;s/^ *js>//'") in
          close_out channel;
          cleanup ();
          output
    with e -> cleanup(); raise e

let test () = 
  let jsresult = lstrip -<- rhino_output in
  let equal l r = 
    if l = r then true
    else (prerr_endline ("Not equal : " ^ l ^ " and " ^ r); false) in

    (* Factorial: recursion, arithmetic, conditionals *)
    assert (equal (jsresult "{fun fact(n) { if (n == 0) 1 else n * fact(n-1)} fact (3)}") "6");

    (* Mutually recurisve nested functions *)
    assert (equal (jsresult "{ fun even(n) { n == 0 || odd(n - 1) } fun odd(n) { even(n) == false } even(20) }") "true");
        
    (* Closures using anonymous functions *)
    assert (equal (jsresult "{fun addn(n) { fun(x) { x + n } } addn(3)(4)}") "7");

    (* Closures using named functions *)
    assert (equal (jsresult "{fun addn(n) { fun f(x) { x + n } f } addn(3)(4)}") "7");

    (*Closures where the environment contains a closure from a different scope*)
    assert (equal (jsresult "{fun add(x,y){x+y} fun baz(z, w) {z + w} fun foo(f, x) { fun bar(y) { f(3, y) } bar(x) } foo(add,4)}") "7");

    (*Nested scopes*)
    assert (equal (jsresult "{ x = 3; ({ x = 4; x }, x)}") "(4, 3)")




