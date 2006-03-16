(* 
FIXME:
  1. lhref transformation
*)


(*
  JavaScript generation.
*)


open Num
open Netencoding
open List

open Sl_pickle
open Sl_forms
open Sl_utility
open Sl_kind
open Sl_syntax

let gensym =
  let counter = ref 0 in 
    function () -> 
      incr counter ;
      "__jsv" ^ (string_of_int !counter)

(*
  Runtime required (any JavaScript functions used /must/ be documented here!)

  __concat(a, b)
     concatenate two sequences: either strings or lists
  __accum(f, i)
    concatMap: apply f to every element of the sequence `i' and concatenate the results.
  __plus, __minus, etc.
    curried function versions of the standard arithmetic operators
  __XML(tag, attrs, children)
    create a DOM node with name `tag'
                       and attributes `attrs' (a dictionary)
                       and children `children' (a sequence of DOM nodes and strings)    
  __extend(record, tag, value)
    extend a record (dictionary) with a new field (label `tag'; value `value').
    Don't update the old record.
  __project(record, tag, value)
    project a field of a record

  __start(tree)
    Replace the current page with `tree'.
  __registerFormAction(continuation)
    Register a continuation function; return an id.
  __continuations
    Table of continuation functions, indexed by id.

  Also, any `builtin' functions from Sl_library.value_env.
 *)


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
            | Iter  of (string * code (*input*) * code (*output*))
            | Bind  of (string * code * code)
            | Seq   of (code * code)
            | Nothing

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
  | Iter  (_, c, _) -> local_names c (* the `output' code is in an inner scope  *) 
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
      | name, Bind (v, (Fn _ as f), Var v') when v = v'  -> show_func (*name*) v f ^ "\nvar " ^ name ^ " = " ^ v ^ ";"
      | name, value -> "var " ^ name ^ " = " ^ show value in
    function
      | Var s -> s
      | Lit s -> s
      | Defs (defs) -> String.concat ";\n" (map show_def defs) ^ ";"
      | Fn _ as f -> show_func "" f
      | Call (Var "__project", [label; record]) -> (paren record) ^ "[" ^ show label ^ "]"
      | Call (fn, args) -> paren fn ^ "(" ^ arglist args  ^ ")"
      | Binop (l, op, r) -> paren l ^ " " ^ op ^ " " ^ paren r
      | Cond (if_, then_, else_) -> "(" ^ show if_ ^ " ? " ^ show then_ ^ " : " ^ show else_ ^ ")"
      | Dict (elems) -> "{" ^ String.concat ", " (map (fun (name, value) -> name ^ " : " ^ show value) elems) ^ "}"
      | Lst [] -> "[]"
      | Lst elems -> "[" ^ arglist elems ^ "]"
      | Iter (var, input, output) -> "__accum(" ^
          show_func "" (Fn ([var], output)) ^ ","^ show input ^")"
      | Bind (name, value, body) -> "("^ name ^" = "^ 
          show value ^", "^ show body ^")"
      | Seq (l, r) -> "(" ^ show l ^ ", " ^ show r ^ ")"
      | Nothing -> ""

(* create a string literal, quoting special characters *)
let string_quote s = 
  let sub old repl s = Str.global_replace (Str.regexp old) repl s in
    "'" ^ sub "'" "\\'" (sub "\n" "\\n" s) ^ "'"
      
let strlit s = Lit (string_quote s)

(* Specialness:

   * Top-level boilerplate code to replace the root element and reset the focus

     The special function __start takes an html page as a string and
     replaces the currently displayed page with that one.

     Some of the other functions are equivalents to Links builtins
     (e.g. int_of_string, xml)
 *)


let boiler_1 = "<html>
       <head>
          <script type='text/javascript' src=\"json.js\"></script>
          <script type='text/javascript' src=\"jslib.js\"></script>
          <script type='text/javascript'><!-- \n"
and boiler_2 =    "\n--> </script>
      </head>
      <body><script type='text/javascript'>(" 
and  boiler_3 = ")(__start)</script></body>
   </html>"

(* Operators are represented as functions in the interpreter, but
   operator names aren't valid JS function names.*)
let builtins = ["+", "__plus";
                "+.", "__plus";
                "-", "__minus";
                "-.", "__minus";
                "*", "__times";
                "*.", "__times";
                "/", "__divide";
                "/.", "__divide"]

let binop_name op = 
  try
    assoc op ["+",  "+";
              "+.", "+";
              "-",  "-";
              "-.", "-";
              "*",  "*";
              "*.", "*";
              "/",  "/";
              "//", "/";
              "==", "==";
              "<>", "!=";
              "<<", "<";
              ">>", ">";
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
       | Record_selection_empty (Variable (avar, _), body, _) 
	   when avar = argvar 
	     -> [], body
       | _ -> failwith "Error unpacking arglist") in
    function
      | Abstr (var, (Record_selection _ as body), _) -> arglist var body
      | Abstr (var, (Record_selection_empty _ as body), _) -> [], body
      | Abstr (var, body, _) -> [var], body
      | _ -> failwith "Error unpacking arglist"

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
        Defs [n, Fn(["kappa"], Fn (arglist, 
                 Call(Call (Var "__remoteCall", [Var "kappa"]),
                      [strlit n; Dict (
                         List.map2
                           (fun n v -> string_of_int n, Var v) 
                           (Sl_utility.fromTo 1 (1 + List.length arglist))
                           arglist
                       )])))]
| e                         -> failwith ("Cannot generate stub for " ^ string_of_expression e)

let trivial_cps expr = 
  Fn(["kappa"], Call(Var "kappa", [expr]))

(* let idy_js = Fn(["x"], Var "x")*)
let idy_js = Var("__idy")

let yield_and_call(func, args, kappa) =
  Call(Var("__yield"), [jsthunk(Call(Call(func, [kappa]), args))])

let make_xml_cps attrs_cps attrs_noncps children_cps children_noncps tag = 
  let innermost_expr = 
    Call(Call (Var "__XML",
               [Var "kappa"]),
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
    Fn(["kappa"], tower)
      

(** generate
    Generate javascript code for a Links expression
    
    With CPS transform, result of generate is always (a -> w) -> b
*)
let rec generate : 'a expression' -> code = 
  let rec reduce_collection : 'a expression' -> code = 
    function
      | Collection_union (c, Collection_empty _, _) -> 
          reduce_collection c
      | Collection_union (l, r, _) ->
          let l_cps = generate l in
          let r_cps = generate r in
            Fn(["kappa"],
               Call(l_cps, [Fn(["l"],
                   Call(r_cps, [Fn(["r"],
                        Call(Var "kappa", 
                             [Call (Var "__concat", [Var "l"; Var "r"])]))]))]))
      | Collection_union (Collection_single (l,_,_), r, _) -> 
          failwith "unimpl"; 
	  (match reduce_collection r with
	     | Lst items -> Lst (generate_noncps l :: items)
	     | c         -> Call (Var "__concat", [Lst [generate_noncps l]; c]))
      | Collection_union (l, Collection_single (r,_,_), _) -> 
          failwith "unimpl"; 
	  (match reduce_collection l with
	     | Lst items -> Lst (items @ [generate_noncps r])
	     | c         -> Call (Var "__concat", [c; Lst [generate_noncps r]]))
      | e ->          
          (* failwith "unimpl"; *)
          generate e
  in
    function
  | Integer (v, _)                     -> trivial_cps (Lit (string_of_num v))
  | Float (v, _)                       -> trivial_cps (Lit (string_of_float v))
  | Boolean (v, _)                     -> trivial_cps (Lit (string_of_bool v))
  | String (v, _)                      -> trivial_cps (strlit v)
  | Condition (i, t, e, _)             -> 
      let i_cps = generate i in
      let t_cps = generate t in
      let e_cps = generate e in
        Fn(["kappa"], 
           Call(i_cps, [Fn(["i"], Cond (Var "i",
                                        Call(t_cps, [Var "kappa"]),
                                        Call(e_cps, [Var "kappa"])))]))
                          
  | Let (v, e, b, _)                   -> 
      let e' = generate e in
      let b' = generate b in
        Fn(["kappa"], Call(e', [Fn([v], Call(b', [Var "kappa"]))]))
  | Variable (v, _)                    -> trivial_cps (Var v)
  | Comparison (l, "==", r, _)         -> 
      let l_cps = generate l in
      let r_cps = generate r in
        Fn(["kappa"],
           Call(l_cps, [Fn(["l"],
              Call(r_cps, [Fn(["r"],
                 Call(Var "kappa", [Call(Var "__eq", [Var "l"; Var "r"])]))]))]))

  | Comparison (l, op, r, _)           -> 
      let l_cps = generate l in
      let r_cps = generate r in
        Fn(["kappa"],
           Call(l_cps, [Fn(["l"], 
                Call(r_cps, [Fn(["r"],
                     Call(Var "kappa", 
                          [Binop(Var "l", binop_name op, Var "r")]))]))]))
      (* Should strings be handled differently at this level? *)
  | Collection_empty _                 -> trivial_cps (Lst [])
  | Collection_single (e, _, _)        -> 
      let content_cps = generate e in
        Fn(["kappa"],
           (Call(content_cps, [Fn(["x"], 
                                  (Call(Var "kappa", [Lst [Var "x"]])))])))
  | (Collection_union _) as c          -> 
      reduce_collection c
(*  | Collection_union (l, r, _)         -> Call (Var "__concat", [generate l; generate r])*)
      (* CPS me*)
  | Collection_extension (e, v, b, _)  -> failwith "coll. ext. not impl "; 
      trivial_cps (Iter (v, generate_noncps b, generate_noncps e))
      
  | Xml_node _ as xml when islform xml -> laction_transformation xml
  | Xml_node _ as xml when islhref xml -> lhref_transformation xml
  | Xml_node _ as xml when isinput xml -> lname_transformation xml
  | Xml_node (tag, attrs, children, _)   -> 
      let attrs_cps = map (fun (k,e) -> (k, gensym(), generate e)) attrs in
      let children_cps = map (fun e -> (gensym(), generate e)) children in
        make_xml_cps attrs_cps [] children_cps [] tag

  (* Functions *)
  | Abstr _ as a -> 
      let arglist, body = untuple_def a
      in Fn(["kappa"], 
            Fn (arglist, Call(generate body, [Var "kappa"])))
  | Apply (Apply (Variable (op, _), l, _), r, _) when mem_assoc op builtins -> 
      let l_cps = generate l in
      let r_cps = generate r in
        Fn(["kappa"], 
           Call(l_cps, [Fn(["l"],
                Call(r_cps, [Fn(["r"],
                     Call(Var "kappa",
                          [Binop (Var "l", binop_name op, Var "r")]))]))]))
(*   | Apply (Variable ("enxml", _), p, _) -> Fn(["kappa"],  *)
(*                                           Call(generate p, [Var "kappa"])) *)

  | Apply (Variable ("enxml", _), p, _) -> generate p
      
  | Apply (Variable ("domutate", _), p, _) -> 
      let p_cps = generate p in
        Fn(["kappa"], 
           Call(p_cps, [Fn(["p"],
                          Call(Var "kappa",
                               [Call(Var "__applyChanges", [Var "p"])]))]))

  | Apply (f, p, _  ) -> 
      let kappa = Var("kappa") in
      let f_cps = generate f in
      let f_name = gensym() in
      let arglist = untuple_call p in
      let cps_args = map generate arglist in
      let arg_names = map (gensym -<- ignore) arglist in
      let wrap_cps_terms (arg_cps, arg_name) expr = 
        Call(arg_cps, [Fn ([arg_name], expr)])
      in
      let innermost_call = yield_and_call(Var f_name,
                                          map (fun name -> Var name) arg_names,
                                          kappa
                                         ) in
      let arg_tower = fold_right wrap_cps_terms (combine cps_args arg_names) innermost_call in
        
        Fn (["kappa"],  Call(f_cps, [Fn ([f_name], arg_tower)]))

  (* Binding *)
  | Define (_, _, `Server, _) as d      -> generate_stub d
  | Define (n, e, (`Client|`Unknown), _)-> Defs ([n, generate e])
  | Rec (bindings, body, _) ->
      (fold_right 
         (fun (v, e) body ->
            Bind (v, generate e, body))
         bindings
         (Call(generate body, [Var "__idy"]))) (* extra complexity *)
        (* Records *)
  | Record_empty _                    -> trivial_cps (Dict [])
  | Record_extension (n, v, r, _)     -> 
      let r_cps = generate r in
      let v_cps = generate v in
      let extension_val = Call (Var "__extend", [Var "r"; strlit n; Var "v"])
      in
        Fn(["kappa"], Call(r_cps, [Fn(["r"], 
                      Call(v_cps, [Fn(["v"],
                                  Call(Var "kappa",
                                       [extension_val])
                                     )]))]))
  | Record_selection_empty (Variable _, b, _)  -> 
      generate b
  | Record_selection_empty (v, b, _)  -> 
      let v_cps = generate v in
      let b_cps = generate b in
        Fn(["kappa"], Call(v_cps, [Fn(["ignored"], 
                      Call(b_cps, [Fn(["b"],
                      Call(Var "kappa", [Var "b"]))]))]))
  | Record_selection (l, lv, etcv, r, b, _) when mem etcv (freevars b) ->
      let r_cps = generate r in
      let b_cps = generate b in
      let name = gensym () in
        Fn(["kappa"],
           Call(r_cps, [Fn (["r"], 
                (Bind (name, Var "r", 
                       Bind (lv, 
                             Call (Var "__project",
                                   [strlit l; Var name]),
                             Bind (etcv, 
                                   Var name,
                                   Call(b_cps, [Var "kappa"]))))))]))

  | Record_selection (l, lv, _, v, Variable (lv', _), _) when lv = lv ->
      failwith "non impl: record selection";
      (* Could use dot-notation instead of project call *)
      let v_cps = generate v in
        Fn(["kappa"],
           Call(v_cps, [Fn(["v"],
                Call(Var "kappa", 
                     [Call (Var "__project", [strlit l; Var "v"])]))]))
  | Record_selection (l, lv, _, v, b, _) -> (* var unused: a simple projection *)
      failwith "non impl: record selection";
      trivial_cps (Bind (lv,
	                 Call (Var "__project", [strlit l; generate_noncps v]),
	                 generate_noncps b))
  (* Variants *)
  | Variant_injection (l, e, _) -> 
      let content_cps = generate e in
        Fn(["kappa"], 
           Call(content_cps, [Fn(["content"],
                                 Call(Var "kappa", 
                                      [Dict [("label", strlit l);
                                             ("value", Var "content")]]))]))
  | Variant_selection _
  | Variant_selection_empty _
  (* DB stuff *)
  | Sort _
  | Database _
  | Table _
  | Directive _
  | Escape _  as e -> failwith ("Cannot (yet?) generate JavaScript code for " ^ string_of_expression e)

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
                   '<form name="form1" action="#" onSubmit="__start(_continuation_form1()); return false">...</form>')
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
  let beginswithl str = Str.string_match (Str.regexp "l:") str 0 in
  let handlers, attrs = partition (fun (attr, _) -> beginswithl attr) attrs in
  let vars = Sl_forms.lname_bound_vars xml in
  let make_code_for_handler (evName, code) = 
    strip_lcolon evName, (fold_left
                            (fun expr var -> Bind (var, Call (Var "__val", [strlit var]), expr))
                            (end_thread(generate code))
                            vars) in
  let handlerInvoker (evName, _) = (evName, strlit ("__applyChanges(__evContinuations['" ^ evName ^ "'][this.id]()); return false")) in
  let elem_id = 
    try 
      match (assoc "id" attrs) with
	  String(idStr, _) -> strlit idStr
    with Not_found -> Lit "0" in
  let handlers = map make_code_for_handler handlers in
  let attrs_cps = map (fun (k, e) -> (k, gensym(), generate e)) attrs in
  let children_cps = map (fun e -> (gensym(), generate e)) children in
    make_xml_cps attrs_cps (["id", Call(Var "__registerFormEventHandlers",
                                        [elem_id;
                                         Lst (map (fun (evName, code) -> 
                                                     Dict(["evName", strlit evName;
                                                           "handler", Fn ([], code)]))
                                                handlers)]);
                             "action", strlit "#";
                             "method", strlit "post"]
                            @ map handlerInvoker handlers)
      children_cps [] tag
      
(* Specialness:
   
   Add onFocus handlers that save the current field in a global
   variable so that we can restore it when the page is reconstructed.
   Add name and id attributes.

   <input ...> 

   is translated as 
   
   <input name="..." id="..." onFocus="__focused = this.id" ...>
*)
and lname_transformation (Xml_node (tag, attrs, children, d)) = 
  (* 1. Remove l:name from the attrs
     2. Add appropriate id and name to the attrs
     3. Add onFocus handlers
   *)
  let name, attrs = (assoc "l:name" attrs, remove_assoc "l:name" attrs) in 
  let attrs = ("onfocus", Sl_syntax.String ("__focused = this.id",
                                            (Sl_sugar._DUMMY_POS, `Not_typed, None)))
    :: ("id", name)
    :: ("name", name)
    :: attrs in
    generate (Xml_node (tag, attrs, children, d))

(* href transformation is a simplified version of the form transformation *)
and lhref_transformation (Xml_node (tag, attrs, children, d) as xml) = 
  let handler, attrs = (assoc "l:href" attrs, remove_assoc "l:href" attrs) in
    
  let elem_id = 
    try 
      match (assoc "id" attrs) with
	  String(idStr, _) -> strlit idStr
    with Not_found -> Lit "0" in

  let registration = Call(Var "__registerFormEventHandlers",
			  [elem_id; Lst [Dict ["evName", strlit "onclick";
				      "handler", jsthunk (Call(generate handler,
							       [Var "__applyChanges"]))]]]) in
    
  let regAttr = if mem "id" (map fst attrs) then "dummy" else "id" in
  let core_attrs = [regAttr, registration;
                  "onclick", strlit ("__evContinuations['onclick'][this.id](); return false");
                  "href", strlit "#";
                 ] in
  let attrs_cps = map (fun (k, v) -> (gensym(), k, generate v)) attrs in
  let innermost = 
    Dict(fold_right (fun (var, aname, _) (attrs:(string*code)list) -> (aname, Var var) :: attrs)
	   attrs_cps core_attrs )
  in
  let attributes = 
    fold_right (fun (var, aname, attr_cps) expr ->
                  Call(attr_cps, [Fn([var], expr)])
               ) attrs_cps innermost 
  in
    Fn(["kappa"],
       Call (Call(Var ("__XML"), [Var "kappa"]),
             [strlit tag; 
              attributes;
              Lst (map generate_noncps children)]))

(* generate_noncps: generates CPS code for expr and immediately 
  gives idy as the cont. *)
and generate_noncps expr = Call(generate expr, [idy_js])
and end_thread expr = Call(expr, [idy_js])
(* generate_easy_cps: generates CPS code for expr and wraps it to
   accept a continuation kappa and immediately pass the result to 
   kappa. should only be used on trivial expressions--w/o subexprs *)
and generate_easy_cps expr = 
        Fn(["kappa"], Call(Var "kappa", [generate expr]))
      
module StringSet = Set.Make(String)

let set_from_list lyst =
  fold_left StringSet.union StringSet.empty (map (StringSet.singleton) lyst)

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
  | Iter(var, src, body)
  | Bind(var, src, body) -> StringSet.union (freevars src) (StringSet.remove var (freevars body))
  | Seq(first, rest) -> StringSet.union (freevars first) (freevars rest)
  | _ -> StringSet.empty

(* FIXME: There is some problem with this whereby variables are captured *)
let rec replace' var replcmt fvs = function
  | Var  x when x = var -> replcmt
  | Defs defs -> Defs(alistmap (replace' var replcmt (freevars replcmt)) defs)
  | Fn(args, body) when not(mem var args) -> 
(*      let body = 
        if StringSet.is_empty (StringSet.inter (set_from_list args) fvs)
        then body else
          uniquify_args(Fn(args, body))
      in
*)        Fn(args, replace' var replcmt fvs body)
  | Call(func, args) -> Call(replace' var replcmt fvs func,
                             map (replace' var replcmt fvs) args)
  | Binop(lhs, op, rhs) -> Binop(replace' var replcmt fvs lhs, op,
                                 replace' var replcmt fvs rhs)
  | Cond(test, yes, no) ->  Cond(replace' var replcmt fvs test,
                                 replace' var replcmt fvs yes,
                                 replace' var replcmt fvs no)
  | Dict(terms) -> Dict(alistmap (replace' var replcmt fvs) terms)
  | Lst(terms) -> Lst(map (replace' var replcmt fvs) terms)
  | Iter(iter_var, src, body)  -> Iter(iter_var, replace' var replcmt fvs src, 
                                       if iter_var <> var then
                                         replace' var replcmt fvs body
                                       else body)
  | Bind(name, expr, body) -> Bind(name, replace' var replcmt fvs expr, 
                                   if name <> var then
                                     replace' var replcmt fvs body
                                   else body)
  | Seq(first, second) -> Seq(replace' var replcmt fvs first,
                              replace' var replcmt fvs second)
  | simple_expr -> simple_expr
and replace var expr body = replace' var expr (freevars expr) body
and uniquify_args = function
    (Fn(args, body)) ->
      let subst = map (fun x -> (x, gensym())) args in
        Fn(map snd subst,
           fold_right (fun (old, noo) body ->
                         replace' old (Var noo) (StringSet.singleton noo) body)
             subst body)

let rec simplify = function
  | Call(Fn([formal_arg], body), [actual_arg]) ->
      debug("replacing " ^ formal_arg ^ " with " ^ (show actual_arg)); 
      replace formal_arg actual_arg body
  | Call(Var "__idy", [arg]) -> arg
  | Call(f, args) -> Call(simplify f, map (simplify) args )
  | Defs defs -> Defs(alistmap (simplify) defs)
  | Fn(args, body) -> Fn(args, simplify body)
  | Call(func, args) -> Call(simplify func, map (simplify) args)
  | Binop(lhs, op, rhs) -> Binop(simplify lhs, op, simplify rhs)
  | Cond(test, yes, no) ->  Cond(simplify test, simplify yes, simplify no)
  | Dict(terms) -> Dict(alistmap (simplify) terms)
  | Lst(terms) -> Lst(map (simplify) terms)
  | Iter(iter_var, src, body)  -> Iter(iter_var, simplify src, simplify body)
  | Bind(name, expr, body) -> Bind(name, simplify expr, simplify body)
  | Seq(first, second) -> Seq(simplify first, simplify second)
  | simple_expr -> simple_expr

let rec simplify_completely expr = 
  let expr2 = simplify expr in
    if expr = expr2 then
      expr2
    else
      simplify_completely expr2

let gen = 
  Sl_utility.perhaps_apply Sl_optimiser.uniquify_expression
  ->- generate 
(*   ->- simplify_completely *)
  ->- show

 (* TODO: imports *)
let generate_program filename environment expression = 
  (boiler_1
 ^ String.concat "\n" (map gen environment)
 ^ boiler_2
 ^ gen expression
 ^ boiler_3)

(* ******************* *)
(*   Hereafter tests   *)


(* FIXME: The tests below create an unnecessary dependency on Sl_inference (maybe other modules to?
   I'd like to remove this. Can we move the tests into a different module?
*)

open Sl_inference

let lstrip s = List.hd (Str.bounded_split (Str.regexp "[ \t\n]+") s 1)

let rhino_output linkscode = 
  let gen = show
        -<- generate
        -<- (Sl_utility.perhaps_apply Sl_optimiser.uniquify_expression)
        -<- List.hd -<- snd -<- Sl_inference.type_program Sl_library.type_env -<- Parse.parse_string in
  let tempfile = Filename.temp_file "linkstest" ".js" in
  let cleanup () = (try Sys.remove tempfile with _ -> ()) in
    try
      let channel = open_out tempfile in 
      let s = gen linkscode in
        Sl_utility.debug ("generated code for " ^ linkscode ^ ":\n" ^ s ^ "\n");
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




