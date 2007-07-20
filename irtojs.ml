(*pp deriving *)
(* js.ml
    JavaScript generation.
*)

open Num
open Netencoding
open List

open Pickle
open Forms
open Utility
open Syntax
open Ir

let optimising = Settings.add_bool("optimise_javascript", true, `User)
let elim_dead_defs = Settings.add_bool("elim_dead_defs", true, `User)
let js_rename_builtins = Settings.add_bool("js_rename_builtins", false, `User)
let js_lib_url = Settings.add_string("jsliburl", "lib/", `User)
let get_js_lib_url () = Settings.get_value js_lib_url

(* Intermediate language *)
type code = | Var    of string
            | Lit    of string
            | DeclareVar of string
            | Fn     of (string list * code)

            | LetFun of ((string * string list * code * location) * code)
            | LetRec of ((string * string list * code * location) list * code)
            | Call   of (code * code list)
            | Binop  of (code * string * code)
            | If     of (code * code * code)
            | Case   of (string * (string * code) stringmap * (string * code) option)
            | Dict   of ((string * code) list)
            | Lst    of (code list)

            | Bind   of (string * code * code)
            | Seq    of (code * code)

            | Die    of (string)
            | Ret    of code
            | Nothing
  deriving (Show, Rewriter)
module RewriteCode = Rewrite_code

let freevars_list : code -> string list = 
  let rec aux bound = function
    | Var x when List.mem x bound -> []
    | Var x -> [x]
    | Nothing
    | Die _
    | Lit _ -> []
    | Fn (args, body) -> aux (args @ bound) body
    | Call (f, ps) -> aux bound f @ concat_map (aux bound) ps
    | Seq (l, r)
    | Binop (l,_,r) -> aux bound l @ aux bound r
    | If (i,t,e) ->  aux bound i @ aux bound t @ aux bound e
    | Dict (fs) -> concat_map (aux bound -<- snd) fs
    | Lst elems -> concat_map (aux bound) elems
    | Bind (name, e, body) -> aux bound e @ aux (name::bound) body
    | Ret e -> aux bound e
  in aux []

(* "Blind" (non-capture-avoiding) renaming.  

   This is used in the following optimisation:

       let x1 = x2 in e     -- x2 not free in e
    => e[x2:=x1]

   i.e. to eliminate bindings that simply rename.
*)
let rec rename' renamer = function
  | Var x -> Var (renamer x)
  | Fn(args, body) -> Fn(map renamer args, rename' renamer body)
  | Call(func, args) -> Call(rename' renamer func,
                             map (rename' renamer) args)
  | Binop(lhs, op, rhs) -> Binop(rename' renamer lhs, op,
                                 rename' renamer rhs)
  | If(test, yes, no) ->  If(rename' renamer test,
                                 rename' renamer yes,
                                 rename' renamer no)
  | Dict(terms) -> Dict(alistmap (rename' renamer) terms)
  | Lst(terms) -> Lst(map (rename' renamer) terms)
  | Bind(name, expr, body) -> Bind(renamer name, rename' renamer expr, 
                                   rename' renamer body)
  | Seq(first, second) -> Seq(rename' renamer first,
                              rename' renamer second)
  | Ret e -> Ret (rename' renamer e)
  | simple_expr -> simple_expr
and rename renamer body = rename' renamer body

let freein name expr = List.mem name (freevars_list expr)

let replace var repl : RewriteCode.rewriter =
  function
    | Var x when x = var -> Some (repl)
    | _ -> None
let replace var repl = RewriteCode.bottomup (replace var repl)

let stringp s = Str.string_match (Str.regexp "^[\"']") s 0

let concat_lits : RewriteCode.rewriter = 
  let join_strings l r = 
    (Str.string_before l (String.length l - 1))  ^ (Str.string_after r 1)
      in
  function
    | Call (Var "LINKS.concat", [Lit l; Lit r]) when stringp l && stringp r -> Some (Lit (join_strings l r))

        (* Inline LINKS.concat when one argument is known to be a string *)
    | Call (Var "LINKS.concat", [(Lit lit as l); r])
    | Call (Var "LINKS.concat", [l; (Binop (Lit lit, _, _) as r)])
    | Call (Var "LINKS.concat", [l; (Lit lit as r)]) when stringp lit -> Some (Binop (l, "+", r))

    (* Merge literal lists *)
    | Call (Var "LINKS.concat", [Lst l; Lst r]) -> Some (Lst (l @ r))
    | _ -> None

let concat_lits = RewriteCode.bottomup concat_lits

let optimise e = 
  if Settings.get_value optimising then
    fromOption e 
      (RewriteCode.all [concat_lits] e)
  else e


(*
  Runtime required (any JavaScript functions used /must/ be documented here!)

  LINKS.concat(a, b)
     concatenate two sequences: either strings or lists
  LINKS.accum(f, i)
    concatMap: apply f to every element of the sequence `i' and concatenate the results.
  _plus, _minus, etc.
    curried function versions of the standard arithmetic operators
  LINKS.XML(tag, attrs, children)
    create a DOM node with name `tag'
                       and attributes `attrs' (a dictionary)
                       and children `children' (a sequence of DOM nodes and strings)    
  LINKS.union(r, s)
    return the union of the records r and s
    precondition: r and s have disjoint labels 
  LINKS.project(record, label, value)
    project a field of a record
  LINKS.remove(record, label)
    return a record like "record" but without the field labeled "label"

  _start(tree)
    Replace the current page with `tree'.
  _registerFormAction(continuation)
    Register a continuation function; return an id.
  _continuations
    Table of continuation functions, indexed by id.

  Also, any `builtin' functions from Library.value_env.
 *)

let jsthunk expr = Fn([], expr)

(* Generate code from intermediate language *) 
let rec show : code -> string = fun code ->
  let show_func name (Fn (vars, body)) = 
    "function "^ name ^"("^ String.concat ", " vars ^")"^"{ " ^" "^show body^"; }" 
  and arglist args = String.concat ", " (map show args) 
  and paren = function
    | Var _
    | Lit _
    | Call _
    | Dict _
    | Lst _
    | Seq _
    | Bind _
    | Die _
    | Nothing as c -> show c
    | c -> "(" ^ show c ^ ")" in
  let show_case v (l:string) ((x:string), (e:code)) =
    "case " ^ l ^ ":{var " ^ x ^ "=" ^ v ^"._value;" ^ show e ^ "break;}\n" in
  let show_cases v : (string * code) stringmap -> string =
    fun cases ->
      StringMap.fold (fun l c s ->
                        s ^ show_case v l c)
        cases "" in
  let show_default v = opt_app
    (fun (x, e) ->
       "default:{var " ^ x ^ "=" ^ v ^ ";" ^ show e ^ ";" ^ "break;}") "" in
    match code with
      | Var s -> s
      | Lit s -> s
      | Fn _ as f -> show_func "" f
      | DeclareVar x -> "var "^x

      | LetFun ((name, vars, body, location), rest) ->
          (show_func name (Fn (vars, body))) ^ show rest
      | LetRec (defs, rest) ->
          String.concat ";\n" (List.map (fun (name, vars, body, location) -> show_func name (Fn (vars, body))) defs) ^ show rest
      | Call (Var "LINKS.project", [label; record]) -> (paren record) ^ "[" ^ show label ^ "]"
      | Call (Var "hd", [list;kappa]) -> Printf.sprintf "%s(%s[0])" (paren kappa) (paren list)
      | Call (Var "tl", [list;kappa]) -> Printf.sprintf "%s(%s.slice(1))" (paren kappa) (paren list)
      | Call (fn, args) -> paren fn ^ "(" ^ arglist args  ^ ")"
      | Binop (l, op, r) -> paren l ^ " " ^ op ^ " " ^ paren r
      | If (cond, e1, e2) ->
          "if (" ^ show cond ^ ") {" ^ show e1 ^ "} else {" ^ show e2 ^ "}"
      | Case (v, cases, default) ->
          "switch (" ^ v ^ "._label) {" ^ show_cases v cases ^ show_default v default ^ "}"
      | Dict (elems) -> "{" ^ String.concat ", " (map (fun (name, value) -> "'" ^  name ^ "':" ^ show value) elems) ^ "}"
      | Lst [] -> "[]"
      | Lst elems -> "[" ^ arglist elems ^ "]"
      | Bind (name, value, body) ->  name ^" = "^ show value ^"; "^ show body
      | Seq (l, r) -> show l ^"; "^ show r
      | Nothing -> ""
      | Die msg -> "error('" ^ msg ^ "', __kappa)"


module PP =
struct
  open PP

  (** Pretty-print a Code value as a JavaScript string. *)
  let rec format_code c : PP.doc =
    let rec show_func_pp name (Fn (vars, body)) = 
      PP.group (PP.text "function" ^+^ PP.text name ^^ (formal_list vars)
                ^+^  (braces
                        (break ^^ group(nest 2 (format_code body)) ^^ break))) in
    let show_case v l (x, e) =
      PP.text "case" ^+^ PP.text("'"^l^"'") ^^
        PP.text ":" ^+^ braces (PP.text"var" ^+^ PP.text x ^+^ PP.text "=" ^+^ PP.text (v^"._value;") ^+^
                                  format_code e ^^ PP.text ";" ^+^ PP.text "break;") ^^ break in
    let show_cases v =
      fun cases ->
        StringMap.fold (fun l c s ->
                          s ^+^ show_case v l c)
          cases DocNil in
    let show_default v = opt_app
      (fun (x, e) ->
         PP.text "default:" ^+^ braces (PP.text "var" ^+^ PP.text x ^+^ PP.text "=" ^+^ PP.text (v^";") ^+^
                                          format_code e ^^ PP.text ";" ^+^ PP.text "break;") ^^ break) PP.DocNil in
    let maybe_parenize = function
      | Var _
      | Lit _
      | Call _
      | Dict _
      | Lst _
      | Seq _
      | Bind _
      | Die _
      | Nothing as c -> format_code c
      | c -> parens (format_code c)
    in
      match c with
        | Var x -> PP.text x
        | Nothing -> PP.text ""

        | DeclareVar x -> PP.text "var" ^+^ PP.text x

        | Die msg -> PP.text("error('" ^ msg ^ "', __kappa)")
        | Lit literal -> PP.text literal

        | LetFun ((name, vars, body, location), rest) ->
            (show_func_pp name (Fn (vars, body))) ^^ break ^^ format_code rest
        | LetRec (defs, rest) ->
            PP.vsep (punctuate " " (List.map (fun (name, vars, body, location) -> show_func_pp name (Fn (vars, body))) defs)) ^^
              break ^^ format_code rest

        | Fn _ as f -> show_func_pp "" f
        | Call (Var "LINKS.project", [label; record]) -> 
            maybe_parenize record ^^ (brackets (format_code label))
              (* (paren record) ^ "[" ^ show label ^ "]" *)
        | Call (Var "hd", [list;kappa]) -> 
            (maybe_parenize kappa) ^^ (parens (maybe_parenize list ^^ PP.text "[0]"))
        | Call (Var "tl", [list;kappa]) -> 
            (maybe_parenize kappa) ^^ (parens (maybe_parenize list ^^ PP.text ".slice(1)"))
        | Call (fn, args) -> maybe_parenize fn ^^ 
            (PP.arglist (map format_code args))
        | Binop (l, op, r) -> (maybe_parenize l) ^+^ PP.text op ^+^ (maybe_parenize r)
        | If (cond, c1, c2) ->
            PP.group (PP.text "if (" ^+^ format_code cond ^+^ PP.text ")"
                      ^+^  (braces
                              (break ^^ group(nest 2 (format_code c1)) ^^ break))
                      ^+^ PP.text "else"
                      ^+^  (braces
                              (break ^^ group(nest 2 (format_code c2)) ^^ break)))
        | Case (v, cases, default) ->
            PP.group (PP.text "switch" ^+^ (parens (PP.text (v^"._label"))) ^+^
                        (braces ((show_cases v cases) ^+^ (show_default v default))))
        | Dict (elems) -> 
            PP.braces (hsep (punctuate ","
                               (map (fun (name, value) -> 
                                       group (PP.text "'" ^^ PP.text name ^^ 
                                                PP.text "':" ^^ format_code value)) 
                                  elems)))
        | Lst elems -> brackets(hsep(punctuate "," (map format_code elems)))
        | Bind (name, value, body) ->
            PP.text "var" ^+^ PP.text name ^+^ PP.text "=" ^+^ format_code value ^^ PP.text ";" ^^
              break ^^ format_code body
        | Seq (l, r) -> vsep [(format_code l ^^ PP.text ";"); format_code r]
        | Ret e -> PP.text("return ") ^| parens(format_code e)


  let show_pp = format_code ->- PP.pretty 144
end

let show_pp = PP.show_pp

(* create a string literal, quoting special characters *)
let string_js_quote s =
  let sub old repl s = Str.global_replace (Str.regexp old) repl s in
    "'" ^ sub "'" "\\'" (sub "\n" "\\n" (sub "\\" "\\\\\\\\" s)) ^ "'"

(** [strlit] produces a JS literal string from an OCaml string. *)
let strlit s = Lit (string_js_quote s)
let chrlit ch = Lit(string_js_quote(string_of_char ch))
(** [chrlistlit] produces a JS literal for the representation of a Links string. *)
let chrlistlit s  = Lst(map chrlit (explode s))

(* Specialness:

   * Top-level boilerplate code to replace the root element and reset the focus

     The special function _start takes an html page as a string and
     replaces the currently displayed page with that one.

     Some of the other functions are equivalents to Links builtins
     (e.g. int_of_string, xml)
 *)

let script_tag ?(base=get_js_lib_url()) file =
    "  <script type='text/javascript' src=\""^base^file^"\"></script>"

let inline_script file = (* makes debugging with firebug easier *)
  let file_in = open_in file in
  let file_len = in_channel_length file_in in
  let file_contents = String.make file_len '\000' in
    really_input file_in file_contents 0 file_len;
    "  <script type='text/javascript'>" ^file_contents^ "</script>"

let boiler_1 () = "<html>
  <head>
  "^script_tag "json.js"^"
  "^script_tag "regex.js"^"
  "^script_tag "yahoo/YAHOO.js"^"
  "^script_tag "yahoo/event.js"^"
    <script type='text/javascript'>var DEBUGGING="
and boiler_2 () = ";</script>
  "^script_tag "jslib.js"^"
    <script type='text/javascript'><!-- "^"
    function _getDatabaseConfig() {
      return {driver:'" ^ Settings.get_value Library.database_driver ^
 "', args:'" ^ Settings.get_value Library.database_args ^"'}
    }
    var getDatabaseConfig = LINKS.kify(_getDatabaseConfig, 0);\n"
and boiler_3 onload =    "\n--> </script>
  <!-- $Id: js.ml 1001 2007-05-25 16:01:33Z ezra $ -->
  </head>
  <body onload=\'" ^ onload ^ "\'>
    <script type='text/javascript'>
     _startTimer();" 
and  boiler_4 () = ";
    </script>
  </body>
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
	     ]
   with Not_found ->  failwith ("Not found : " ^ op)

let comparison_name = function
  |`Less   -> "<"
  |`LessEq -> "<="
  |`Equal  -> "=="
  |`NotEq  -> "!="
  

let strip_lcolon evName = 
  String.sub evName 2 ((String.length evName) - 2)


let idy_js = Var("_idy")
let thread_end_k = idy_js
let end_thread expr = Call(expr, [idy_js])


(** [cps_prims]: a list of primitive functions that need to see the
    current continuation. Calls to these are translated in CPS rather than
    direct-style.  A bit hackish, this list. *)
let cps_prims = ["recv"; "sleep"]

(** {0 Code generation} *)


let name_binder (x, info) =
  match info with
    | (_, "", `Local) -> (x, "_" ^ string_of_int x)
    | (_, name, `Local) when (Str.string_match (Str.regexp "^_g[0-9]") name 0) ->
        (x, "_" ^ string_of_int x) (* make the generated names slightly less ridiculous in some cases *)
    | (_, name, `Local) -> (x, name ^ "_" ^ string_of_int x)
    | (_, name, `Global) -> (x, name)

module Env :
sig
  type env = string IntMap.t
    deriving (Show)
    
  val empty : env
  val extend : env -> (var * string) list -> env
  val lookup : env -> var -> string
end =
struct
  type env = string IntMap.t
    deriving (Show)

  let empty = IntMap.empty
  let extend env bs =
    List.fold_left (fun env (x,v) ->
                      IntMap.add x v env)
      env bs
  let lookup env x = IntMap.find x env
end

(*
let is_input : value -> bool = function
  | `XmlNode (("input"|"textarea"|"select"), attrs, _)
      when StringMap.mem "l:name" attrs -> true
  | _ -> false
*)

let bind_continuation kappa body =
  match kappa with
    | Var _ -> body kappa
    | _ -> Bind ("_kappa", kappa, body (Var "_kappa"))


(** generate
    Generate javascript code for a Links expression
    
    With CPS transform, result of generate is always : (a -> w) -> b
*)
let rec generate_value env : value -> code =
  let gv v = generate_value env v in
    function
      | `Constant c ->
          begin
            match c with
              | Integer v  -> Lit (string_of_num v)
              | Float v    -> Lit (string_of_float v)
              | Boolean v  -> Lit (string_of_bool v)
              | Char v     -> chrlit v
              | String v   -> chrlistlit v
          end
      | `Variable var ->
          let name = Env.lookup env var in
            if name = "map" then
              Var ("LINKS.accum")
            else
              Var name
      | `Extend (field_map, rest) ->
          let dict =
            Dict
              (StringMap.fold
                 (fun name v dict ->
                    (name, gv v) :: dict)
                 field_map [])
          in
            begin
              match rest with
                | None -> dict
                | Some v ->
                    Call (Var "LINKS.union", [gv v; dict])
            end
      | `Project (name, v) ->
          Call (Var "LINKS.project", [strlit name; gv v])
      | `Erase (name, v) ->
          gv v
(*          Call (Var "LINKS.remove", [gv v; strlit name])*)
      | `Inject (name, v) ->
          Dict [("_label", strlit name);
                ("_value", gv v)]
      | `Nil -> Lst []
      | `Cons (v, vs) ->
          Call (Var "LINKS.concat", [Lst [gv v]; gv vs])
      | `Concat (v, w) ->
          Call (Var "LINKS.concat", [gv v; gv w])
            (*
              | Xml_node _ as xml when isinput xml -> lname_transformation global_names xml
              | Xml_node _ as xml -> laction_transformation global_names xml
            *)
      | `Comparison (v, `Equal, w) ->
          Call(Var "LINKS.eq", [gv v; gv w])
      | `Comparison (v, op, w) ->
          Binop(gv v, comparison_name op, gv w)
(*      | `XmlNode _ as xml when is_input xml -> lname_transformation' env xml*)
      | `XmlNode (name, attributes, children) ->
          generate_xml env name attributes children

      | `ApplyPrim (`Variable op, [l; r]) when mem_assoc (Env.lookup env op) builtins ->
          Binop (gv l, binop_name (Env.lookup env op), gv r)
      | `ApplyPrim (`Variable f, vs) when Library.is_primitive (Env.lookup env f) && not (mem (Env.lookup env f) cps_prims) ->
          Call (Var ("_" ^ (Env.lookup env f)), List.map gv vs)
      | `ApplyPrim (v, vs) ->
          Call (gv v, List.map gv vs)

      | `Coerce (v, _) ->
          gv v
      | `Abs v ->
          Call (Var "abs", [gv v])

(* and lname_transformation' env (`XmlNode (tag, attrs, children)) =  *)
(*   (\* 1. Remove l:name from the attrs *)
(*      2. Add appropriate id and name to the attrs *)
(*      3. Add onFocus handlers *)
(*   *\) *)
(*   let name, attrs = (StringMap.find "l:name" attrs, StringMap.remove "l:name" attrs) in  *)
(*   let attrs = *)
(*     StringMap.add "onfocus" (`Constant(Syntax.String "_focused = this.id")) *)
(*       (StringMap.add "id" name *)
(*          (StringMap.add "name" name attrs)) *)
(*   in *)
(*     generate_value env (`XmlNode (tag, attrs, children)) *)

and make_xml env tag attrs children =
    Call(Var "LINKS.XML",
         [strlit tag;
          Dict (StringMap.fold (fun name v bs -> (name, generate_value env v) :: bs) attrs []);
          Lst (List.map (generate_value env) children)])


(*
  [SL]

  Decided not to implement laction_transformation for now -
  essentially because it's tricky to implement if we're in ANF (how
  can we compute the scope of l:name-bound variables?).

  Note that this is due to a deficiency in the design of l:name rather
  than a deficiency in ANF. If ANF had been used from the start then
  it would have been immediately obvious that l:name was a broken
  design!
*)



(* Specialness: 
 * Modify the l:action to pass the continuation to the top-level boilerplate
 * lift the continuation out of the form.

 * def f(x) {
 * <form name="form1" l:action={foo(x)}>...</form>
 * }

 * is translated as 
 * _continuation_form1 = null;
 * function f(x) { 
 * return (_continuation_form1 = function () { foo(x) },
 * '<form name="form1" action="#" onSubmit="_start(_continuation_form1()); return false">...</form>')
 * }

 * The continuation can't be left in the action attribute because it
 * might refer to lexical bindings and action is just a string, so
 * scope is broken.  (This will need more care for less simple cases,
 * e.g. where there are let bindings)
 *)
(* and laction_transformation' global_names (Xml_node (tag, attrs, children, _) as xml) = *)

and generate_xml env tag attrs children =
  (*
     laction_transformation needs the global names in order to strip them
     out of any environment that it captures.
  *)
  let essential_attrs =
    match tag with
        "form" -> StringMap.from_alist ["action", chrlistlit "#"; "method", chrlistlit "post"]
      | _ -> StringMap.empty in  
  let gv = generate_value env in
  
  let handlers, plain_attrs =
    StringMap.fold
      (fun name v (handlers, plain_attrs) ->
         if start_of name ~is:"l:" then
           (strip_lcolon name, Call (gv v, [Var "event"; idy_js])) :: handlers, plain_attrs
         else
           handlers, StringMap.add name (gv v) plain_attrs)
      attrs ([], StringMap.empty) in

  let children = List.map gv children in

  let key_attr =
    match handlers with
      | [] -> StringMap.empty
      | handlers ->
          let handlers = remove_assoc "href" handlers in
            StringMap.singleton
              "key"
              (Call(Var "_registerFormEventHandlers",
                    [Lst (map (fun (evName, code) ->
                                 Dict(["evName", strlit evName;
                                       "handler", Fn (["event"], code)]))
                            handlers)])) in

  let all_attrs =
    StringMap.union_disjoint plain_attrs (StringMap.union_disjoint key_attr essential_attrs)

  in
    Call(Var "LINKS.XML",
         [strlit tag;
          Dict (StringMap.to_alist all_attrs);
          Lst children])


    (* make_var_list pairs each variable name (as a string) with the
       variable itself. At runtime this is like pairing the variable
       names with their values, effectively capturing the environment. *)
(*   let make_var_list = map (fun v -> Lst [chrlistlit v ; Var v]) in *)

(*   let href_attr = *)
(*     if (tag <> "a") then [] else *)
(*     try *)
(*       let href_hdlr_ast = (assoc "l:href" handlers_ast) in *)
(*       let code_ptr = Result.marshal_exprenv (href_hdlr_ast, []) in *)
(*       let href_hdlr = (assoc "href" handlers) in *)
(*       (\* [NOTE] *)

(*          The following relies on the invariant that global names are distinct from local names, which *)
(*          is established by Syntax.uniquify_names. *)
(*       *\) *)
(*       let local_vars = StringSet.diff (Syntax.freevars href_hdlr_ast) (StringSet.from_list global_names) in *)
(*         ["href", Call(Var "LINKS.stringToCharlist", *)
(*                       [Call(Var "LINKS.jsStrConcat", *)
(*                             [strlit("?_k=" ^ code_ptr ^ "&_jsonArgs="); *)
(*                              Call(Var "JSON.stringifyB64", *)
(*                                   [Lst(make_var_list (StringSet.elements local_vars))])])])] *)
(*     with Not_found -> ["href", chrlistlit "#"] in *)
    
(*     make_xml_cps *)
(* attrs_cps (key_attr @ href_attr @ essentialAttrs) *)
(*       children_cps [] tag *)


let generate_remote_call f_name xs_names =
  Call(Call (Var "LINKS.remoteCall", [Var "__kappa"]),
       [strlit f_name; Dict (
          List.map2
            (fun n v -> string_of_int n, Var v) 
            (Utility.fromTo 1 (1 + List.length xs_names))
            xs_names
        )])

let apply_yielding (f, args) =
  Call(Var "_yield", f :: args)

let callk_yielding kappa arg =
  Call(Var "_yieldCont", [kappa; arg]) 

let rec generate_tail_computation env : tail_computation -> code -> code = fun tc kappa ->
  let gv v = generate_value env v in
  let gc c kappa = snd (generate_computation env c kappa) in
    match tc with
      | `Return v ->           
          callk_yielding kappa (gv v)
(*       | `Apply (`Variable op, [l; r]) when mem_assoc (Env.lookup env op) builtins -> *)
(*           callk_yielding kappa (Binop (gv l, binop_name (Env.lookup env op), gv r)) *)
(*       | `Apply (`Variable f, vs) when Library.is_primitive (Env.lookup env f) && not (mem (Env.lookup env f) cps_prims) -> *)
(*           Call (kappa, [Call (Var ("_" ^ (Env.lookup env f)), List.map gv vs)]) *)
      | `Apply (v, vs) ->
          apply_yielding (gv v, [Lst (map gv vs); kappa])
      | `Special special ->
          generate_special env special kappa
      | `Case (v, cases, default) ->
          let v = gv v in
          let k, x = 
            match v with
              | Var x -> (fun e -> e), x
              | _ ->
                  let x = gensym ~prefix:"x" () in
                    (fun e -> Bind (x, v, e)), x
          in
            bind_continuation kappa
              (fun kappa ->
                 let gen_cont (xb, c) =
                   let (x, x_name) = name_binder xb in
                     x_name, (snd (generate_computation (Env.extend env [(x, x_name)]) c kappa)) in
                 let cases = StringMap.map gen_cont cases in
                 let default = opt_map gen_cont default in
                   k (Case (x, cases, default)))
      | `If (v, c1, c2) ->
          bind_continuation kappa
            (fun kappa ->
               If (gv v, gc c1 kappa, gc c2 kappa))

and generate_special env : special -> code -> code = fun sp kappa ->
  let gv v = generate_value env v in
    match sp with
      | `App (f, vs) ->
          Call (Var "_yield",
                Call (Var "app", [gv f]) :: [Lst ([gv vs]); kappa])
      | `Wrong -> Die "Internal Error: Pattern matching failed"
      | `Database v ->
          callk_yielding kappa (Dict [("_db", gv v)])
      | `TableQuery _ ->
          failwith ("Cannot (yet?) generate JavaScript code for `TableQuery")
      | `TableHandle (db, table_name, (readtype, writetype)) ->
          callk_yielding kappa
            (Dict [("_table",
                    Dict [("db", gv db);
                          ("name", gv table_name);
                          ("row",
                           strlit (Types.string_of_datatype (readtype)))])])
      | `SortBy _ ->
          failwith ("Cannot (yet?) generate JavaScript code for `SortBy")
      | `CallCC v ->
          bind_continuation kappa
            (fun kappa -> apply_yielding (gv v, [Lst [kappa]; kappa]))

and generate_computation env : computation -> code -> (Env.env * code) = fun (bs, tc) kappa -> 
  let rec gbs env c =
    function
      | [] ->
            (*            let _ = Debug.print ("envy: "^Env.Show_env.show env) in*)
          env, c (generate_tail_computation env tc kappa)
      | b :: bs -> 
          let env, c' = generate_binding env b in
            gbs env (c -<- c') bs
  in
    gbs env (fun code -> code) bs

and generate_binding env : binding -> (Env.env * (code -> code)) = fun binding ->
  match binding with
    | `Let (b, tc) ->
        let (x, x_name) = name_binder b in
        let env' = Env.extend env [(x, x_name)] in
          env', (fun code -> generate_tail_computation env tc (Fn ([x_name], code)))
    | `Fun (fb, xsb, body, location) ->
        let (f, f_name) = name_binder fb in
        let bs = List.map name_binder xsb in
        let xs, xs_names = List.split bs in
        let body_env = Env.extend env bs in
        let env' = Env.extend env [(f, f_name)] in
          (env',
           fun code ->
             let body =
               match location with
                 | `Client | `Unknown -> snd (generate_computation body_env body (Var "__kappa"))
                 | `Server -> generate_remote_call f_name xs_names
                 | `Native -> failwith ("Not implemented native calls yet")
             in
               LetFun
                 ((f_name,
                   xs_names @ ["__kappa"],
                   body,
                   location),
                  code))        
    | `Rec defs ->
        let fs = List.map (fun (fb, _, _, _) -> name_binder fb) defs in
        let env' = Env.extend env fs in
          (env',
           fun code ->
             LetRec
               (List.fold_right
                  (fun (fb, xsb, body, location) (defs, code) ->
                     let (f, f_name) = name_binder fb in
                     let bs = List.map name_binder xsb in
                     let _, xs_names = List.split bs in
                     let body_env = Env.extend env (fs @ bs) in
                     let body =
                       match location with
                         | `Client | `Unknown -> snd (generate_computation body_env body (Var "__kappa"))
                         | `Server -> generate_remote_call f_name xs_names
                         | `Native -> failwith ("Not implemented native calls yet")
                     in
                       (f_name,
                        xs_names @ ["__kappa"],
                        body,
                        location) :: defs, code)
                  defs ([], code)))
    | `For _ -> assert false (* should be compiled away to map! *)

    | `Alien _
    | `Alias _ -> env, (fun code -> code)

and generate_defs env : binding list -> (Env.env * code) =
  fun bs ->
    let rec gbs env c =
      function
        | [] -> env, c Nothing
        | b :: bs -> 
            begin
              match b with
                | `Let (b, tc) ->
                    let (x, x_name) = name_binder b in
                    let env' = Env.extend env [(x, x_name)] in
                    let c' =
                      (fun code ->
                         Seq (DeclareVar (x_name),
                             Seq (generate_tail_computation env tc (Fn (["__x"], Binop(Var x_name, "=", Var "__x"))), code)))
                    in
                      gbs env' (c -<- c') bs
                | _ ->
                    let env, c' = generate_binding env b in
                      gbs env (c -<- c') bs
            end
    in
      gbs env (fun code -> code) bs

and generate_prog env : computation -> (Env.env * code) = fun c ->
  generate_computation env c (Var "_start")
  
let rec freevars = 
  let fv = freevars in
  let module S = StringSet in function
  | Var x               -> S.singleton x
  | Fn (args, body)     -> S.diff (fv body) (S.from_list args)
  | Call (func, args)   -> S.union (fv func) (S.union_all (map fv args))
  | Binop (l, _, r)     -> S.union (fv l) (fv r)
  | If (a, b, c)      -> S.union (S.union (fv a) (fv b)) (fv c)
  | Dict terms          -> S.union_all (map (snd ->- fv) terms)
  | Lst terms           -> S.union_all (map fv terms)
  | Bind (var, e, body) -> S.union (fv e) (S.remove var (fv body))
  | Seq (l, r)          -> S.union (fv l) (fv r)
  | Lit _ 
  | Die _ 
  | Nothing             -> S.empty
  | _ -> assert false

let sum xs = fold_right (+) xs 0

let gen_defs env defs =
  let env, code = generate_defs env defs in
    env, optimise(code)

let gen env e =
  let env, code = generate_prog env e in
    env, optimise(code)

let words = 
  [ '!', "bang";
    '$', "dollar";
    '%', "percent";
    '&', "and";
    '*', "star";
    '+', "plus";
    '/', "slash";
    '<', "lessthan";
    '=', "equals";
    '>', "greaterthan";
    '?', "huh";
    '@', "monkey";
    '\\', "backslash";
    '^', "caret";
    '-', "hyphen";
    '.', "fullstop";
    '|', "pipe";
    '_', "underscore"]

let symbols = List.map fst words

let wordify x = 
  catch_notfound_l "wordify"
    (lazy("_" ^ mapstrcat "_" (flip List.assoc words) (Utility.explode x)))

let symbolp name =
  List.for_all (flip List.mem symbols) (explode name) &&
    (if Settings.get_value js_rename_builtins then true
     else (not (Library.is_primitive name)))

let wordify_rewrite : RewriteSyntax.rewriter = function
  | Variable (name, d) when symbolp name -> Some (Variable (wordify name, d))
  | Let (name, rhs, body, d) when symbolp name -> Some (Let (wordify name, rhs, body, d))
  | Rec (bindings, body, d) when List.exists (fst3 ->- symbolp) bindings -> 
      let rename (x,y,z) =
        ((if symbolp x then wordify x 
          else x), y, z) in
        Some (Rec (List.map rename bindings, body, d))
  | _ -> None

let rename_symbol_operators exp = 
  fromOption exp (RewriteSyntax.bottomup wordify_rewrite exp)

let rename_symbol_operators_def def =
  match def with
    | Define (name, b,l,t) when symbolp name -> Define (wordify name, rename_symbol_operators b, l, t)
    | _ -> def

let make_boiler_page ?(onload="") ?(body="") defs =
  boiler_1 ()
  ^ string_of_bool(Settings.get_value(Debug.debugging_enabled))
  ^ boiler_2 ()
  ^ String.concat "\n" defs
  ^ boiler_3 onload
  ^ body
  ^ boiler_4 ()
    
let get_alien_names defs = 
  let alienDefs = List.filter (function Alien _ -> true | _ -> false) defs in
    List.map (function Alien(_, s, _, _) -> s) alienDefs

let invert_env env =
  StringMap.fold
    (fun name var env ->
       if IntMap.mem var env then
         failwith ("crappy error message: duplicate var")
       else
         IntMap.add var name env)
    env IntMap.empty

let generate_program_defs defs root_names =
  let _ = Debug.print ("defs: "^String.concat "\n" (List.map (Syntax.Show_definition.show) (* string_of_definition *) defs)) in

  let aliens = get_alien_names defs in
  let defs = List.map rename_symbol_operators_def defs in
  (* [NOTE] body is just a placeholder *)
  let body = Syntax.unit_expression Syntax.no_expr_data in
  let (Program (defs, body)) =
    if Settings.get_value optimising then
      Optimiser.inline (Optimiser.inline (Optimiser.inline (Program (defs, body)))) 
    else Program (defs, body)
  in
  let library_names = "map" :: List.map fst (fst Library.typing_env) in
  let elim_free_names = aliens @ library_names in
  let defs =
    (if Settings.get_value elim_dead_defs then
       Callgraph.elim_dead_defs elim_free_names defs root_names
     else defs) in
  let initial_env = Compileir.make_initial_env library_names in
  let ((defs', _) as p, _) = Compileir.compile_program initial_env (Program (defs, body)) in
  let _ = Debug.print (Show_computation.show p) in
  let env = Compileir.add_globals_to_env initial_env defs' in
  let env' = invert_env env in
  let _ = Debug.print ("env': "^Env.Show_env.show env') in
  let env', js_defs = gen_defs env' defs' in
  let js_defs = [show_pp js_defs] in
    (env, env'), js_defs

let generate_program ?(onload = "") (Program (defs, body)) =
  let (Program (defs, body)) = rewrite_program
    (Syntax.RewriteSyntax.all [Rewriterules.Prepare.NormalizeProjections.normalize_projections])
    (Program (defs, body)) in

  let (env, env'), js_defs = generate_program_defs defs (Syntax.freevars body) in
  let body = rename_symbol_operators body in
 
  let (e, _) = Compileir.compile_program env (Program ([], body)) in 
  let _ = Debug.print (Show_computation.show e) in
  let _ = Debug.print ("A") in
  let js_root_expr =
    snd (gen env' e) in
  let _ = Debug.print ("B") in
    (make_boiler_page ~body:(show_pp js_root_expr) js_defs)

let generate_program_defs defs root_names =
  snd (generate_program_defs defs root_names)
