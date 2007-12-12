(*pp deriving *)
(* js.ml
    JavaScript generation.
*)

open Num
open List

open Pickle
open Utility
open Syntax

let optimising = Basicsettings.Js.optimise
let elim_dead_defs = Basicsettings.Js.elim_dead_defs
let js_lib_url = Basicsettings.Js.lib_url
let get_js_lib_url () = Settings.get_value js_lib_url

(* Intermediate language *)
type code = | Var   of string
            | Lit   of string
            | Defs  of ((string * code) list)
            | Fn    of (string list * code)
            | Call  of (code * code list)
            | Unop  of (string * code)
            | Binop of (code * string * code)
            | Cond  of (code * code * code)
            | Dict  of ((string * code) list)
            | Lst   of (code list)
            | Bind  of (string * code * code)
            | Seq   of (code * code)
            | Die   of (string)
            | Ret   of code
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
    | Defs (ds) -> concat_map (aux ((List.map fst ds) @ bound)) (List.map snd ds)
    | Fn (args, body) -> aux (args @ bound) body
    | Call (f, ps) -> aux bound f @ concat_map (aux bound) ps
    | Unop(_, body) -> aux bound body
    | Seq (l, r)
    | Binop (l,_,r) -> aux bound l @ aux bound r
    | Cond (i,t,e) ->  aux bound i @ aux bound t @ aux bound e
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
  | Defs defs -> Defs(map (fun (x, body) -> renamer x, rename' renamer body) defs)
  | Fn(args, body) -> Fn(map renamer args, rename' renamer body)
  | Call(func, args) -> Call(rename' renamer func,
                             map (rename' renamer) args)
  | Unop(op, body) -> Unop (op, rename' renamer body)
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
  | Ret e -> Ret (rename' renamer e)
  | simple_expr -> simple_expr
and rename renamer body = rename' renamer body

let freein name expr = List.mem name (freevars_list expr)

let replace var repl : RewriteCode.rewriter =
  function
    | Var x when x = var -> Some (repl)
    | _ -> None
let replace var repl = RewriteCode.bottomup (replace var repl)

let remove_renaming : RewriteCode.rewriter = 
  function
    | Bind (x, Var y, body) when not (freein x body) -> Some body
    | Bind (x, Var y, body) when not (freein y body) -> Some (rename 
                                                                 (fun name ->
                                                                   if name = x then y
                                                                   else name) body)
        (* not really a renaming, but it goes here well enough.  In general this is a pessimisation, though *)
(*    | Bind (x, (Call (Var "LINKS.project", ([Lit _; Var _])) as l), body)*)
    | Bind (x, (Lit _ as l), body) -> Some (from_option body (replace x l body) )

    | _ -> None

let remove_renamings = RewriteCode.bottomup remove_renaming

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
    from_option e 
      (RewriteCode.all [remove_renamings; concat_lits] e)
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
  LINKS.project(record, label)
    project a field of a record
  LINKS.erase(record, label)
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

(* local_names

   Retrieve all let bindings within a function.  Don't descend into
   inner function scopes.
*)
let rec local_names : code -> string list = function
  | Var _
  | Lit _
  | Fn _
  | Die _
  | Nothing -> []
  | Call (c, cs) -> local_names c @ List.concat (map local_names cs)
  | Cond (a, b, c) -> local_names a @ local_names b @ local_names c
  | Dict cs -> List.concat (map (local_names -<- snd) cs)
  | Lst  cs -> List.concat (map local_names cs)
  | Unop (_, body) -> local_names body
  | Binop (l, _, r)
  | Seq (l, r) -> local_names l @ local_names r
  | Bind (l, c1, c2) -> l :: local_names c1 @ local_names c2
  | Defs (bs) -> map fst bs
  | Ret a -> local_names a

(* Generate code from intermediate language *) 
let rec show : code -> string = 
  let show_func name (Fn (vars, body)) = 
    "function "^ name ^"("^ String.concat ", " vars ^")"
    ^"{ "^
      (let names = String.concat ", " (local_names body) in
	if names = "" then "" else "var " ^ names ^ ";\n")
    ^" "^ show body 
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
    | Die _
    | Nothing as c -> show c
    | c -> "(" ^ show c ^ ")" in
  let show_def = function
      | name, (Fn _ as f) -> show_func name f
      | name, Bind (v, (Fn _ as f), Var v') when v = v'  -> show_func (*name*) v f ^ "\nvar " ^ name ^ " = " ^ v
      | name, value -> "var " ^ name ^ "; " ^ show value ^ ";" in
    function
      | Var s -> s
      | Lit s -> s
      | Defs (defs) -> String.concat ";\n" (map show_def defs) ^ ";"
      | Fn _ as f -> show_func "" f
      | Call (Var "LINKS.project", [record; label]) -> (paren record) ^ "[" ^ show label ^ "]"
      | Call (Var "hd", [list;kappa]) -> Printf.sprintf "%s(%s[0])" (paren kappa) (paren list)
      | Call (Var "tl", [list;kappa]) -> Printf.sprintf "%s(%s.slice(1))" (paren kappa) (paren list)
      | Call (fn, args) -> paren fn ^ "(" ^ arglist args  ^ ")"
      | Unop (op, body) -> op ^ paren body
      | Binop (l, op, r) -> paren l ^ " " ^ op ^ " " ^ paren r
      | Cond (if_, then_, else_) -> "(" ^ show if_ ^ " ? " ^ show then_ ^ " : " ^ show else_ ^ ")"
      | Dict (elems) -> "{" ^ String.concat ", " (map (fun (name, value) -> "'" ^  name ^ "':" ^ show value) elems) ^ "}"
      | Lst [] -> "[]"
      | Lst elems -> "[" ^ arglist elems ^ "]"
      | Bind (name, value, body) -> "("^ name ^" = "^ 
          show value ^", "^ show body ^")"
      | Seq (l, r) -> "(" ^ show l ^", "^ show r ^ ")"
      | Nothing -> ""
      | Die msg -> "error('" ^ msg ^ "', __kappa)"
      | Ret a -> "return "^show a

open PP

(** Pretty-print a Code value as a JavaScript string. *)
let rec format_code c : PP.doc =
  let rec show_func_pp name (Fn (vars, body)) = 
    let names = local_names body in
    let local_decls = 
      if names = [] then DocNil
      else
        PP.text "var" ^+^ hsep (punctuate "," (map PP.text names)) ^^ PP.text ";" ^^ break
    in
    let body = local_decls ^^ format_code body in
      PP.group (PP.text "function" ^+^ PP.text name ^^ (formal_list vars)
                ^+^  (braces
                        (break ^^ group(nest 2 body) ^^ break))
               )
  and show_def_pp : (string * code) -> PP.doc = function
    | name, (Fn _ as f) -> show_func_pp name f
    | name, Bind (v, (Fn _ as f), Var v') when v = v' -> 
        show_func_pp v f ^^ PP.break ^^ PP.text "var" ^+^ 
          PP.binop (PP.text name) "=" (PP.text v)
    | name, value -> PP.text "var" ^+^ PP.text name ^^ PP.text ";" 
        ^+^ format_code value ^^ PP.text ";"
  and maybe_parenize = function
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
    | Die msg -> PP.text("error('" ^ msg ^ "', __kappa)")
    | Lit literal -> PP.text literal
    | Defs (defs) -> PP.vsep (punctuate ";" (map show_def_pp defs)) ^^ PP.text ";"
    | Fn _ as f -> show_func_pp "" f
    | Call (Var "LINKS.project", [record; label]) -> 
        maybe_parenize record ^^ (brackets (format_code label))
    | Call (Var "hd", [list;kappa]) -> 
        (maybe_parenize kappa) ^^ (parens (maybe_parenize list ^^ PP.text "[0]"))
    | Call (Var "tl", [list;kappa]) -> 
        (maybe_parenize kappa) ^^ (parens (maybe_parenize list ^^ PP.text ".slice(1)"))
    | Call (fn, args) -> maybe_parenize fn ^^ 
        (PP.arglist (map format_code args))
    | Unop (op, body) -> parens (PP.text op ^+^ (maybe_parenize body))
    | Binop (l, op, r) -> (maybe_parenize l) ^+^ PP.text op ^+^ (maybe_parenize r)
    | Cond (if_, then_, else_) -> 
        PP.parens(PP.trinop (format_code if_) "?"
                    (format_code then_) ":"
                    (format_code else_))
    | Dict (elems) -> 
        PP.braces (hsep (punctuate ","
                           (map (fun (name, value) -> 
                                   group (PP.text "'" ^^ PP.text name ^^ 
                                            PP.text "':" ^^ format_code value)) 
                              elems)))
    | Lst elems -> brackets(hsep(punctuate "," (map format_code elems)))
    | Bind (name, value, body) ->
        parens ((PP.text name ^+^ PP.text "=" ^+^ 
                   (format_code value)) ^^ PP.text "," ^^ break ^^
                  format_code body)
    | Seq (l, r) -> parens(vsep [(format_code l ^^ PP.text ","); format_code r])
    | Ret e -> PP.text("return ") ^| parens(format_code e)

let show_pp = format_code ->- PP.pretty 144
        
(** create a string literal, quoting special characters *)
let string_js_quote s =
  let sub old repl s = Str.global_replace (Str.regexp old) repl s in
    "'" ^ sub "'" "\\'" (sub "\n" "\\n" (sub "\\" "\\\\\\\\" s)) ^ "'"

(** [strlit] produces a JS literal string from an OCaml string. *)
let strlit s = Lit (string_js_quote s)
let chrlit ch = Lit(string_js_quote(string_of_char ch))
(** [chrlistlit] produces a JS literal for the representation of a Links 
    string. *)
let chrlistlit s  = Lst(map chrlit (explode s))

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
  "^script_tag "yahoo/yahoo.js"^"
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
  <!-- $Id$ -->
  </head>
  <body onload=\'" ^ onload ^ "\'>
    <script type='text/javascript'>
     _startTimer();" 
and  boiler_4 () = ";
    </script>
  </body>
</html>"

module Binop :
sig
  val is : string -> bool
  val js_name : string -> string
end =
struct
  let builtin_ops =
    StringMap.from_alist
      [ "+",  "+" ;
        "+.", "+" ;
        "-",  "-" ;
        "-.", "-" ;
        "*",  "*" ;
        "*.", "*" ;
        "/",  "/" ;
        "/.", "/" ]

  let is x = StringMap.mem x builtin_ops
  let js_name op = StringMap.find op builtin_ops
end


let comparison_name = function
  |`Less   -> "<"
  |`LessEq -> "<="
  |`Equal  -> "=="
  |`NotEq  -> "!="
  

let href_rewrite globals : RewriteSyntax.rewriter = function
  | Apply (Variable ("pickleCont",_), 
           [Abstr ([_], e, _) as abs], data) ->
      let fv = StringSet.diff (freevars e) globals 
      and `T (_,_,Some label) = expression_data e in
      let pickled_label = Result.marshal_result (`RecFunction (["f",abs], [], "f"))
      and json_args = Apply (Variable ("stringifyB64", data),
                             [Record_intro (StringSet.fold 
                                              (fun k r -> StringMap.add k (Variable (k, data)) r)
                                              fv StringMap.empty,
                                            None, data)], data) in
        Some (Concat (Constant (String (pickled_label^ "&_jsonArgs="), data),
                      json_args, data))
  | _ -> None

let fixup_hrefs globals e = from_option e (RewriteSyntax.bottomup (href_rewrite globals) e)

let fixup_hrefs_def globals = function
  | Define (name, b,l,t) -> Define (name, fixup_hrefs globals b, l, t)
  | d -> d

(* Convert colons in qualified names to triple-underscores *) 
let rename_prefixed name = Str.global_replace (Str.regexp ":") "___" name

let strip_lcolon evName = 
  String.sub evName 2 ((String.length evName) - 2)

let js_tuple elems = Dict (
  List.map2
    (fun n e -> string_of_int n, e) 
    (Utility.fromTo 1 (1 + List.length elems))
    elems
)

let server_stub name arglist = 
  Defs [name, Fn (arglist @ ["__kappa"], 
                Call(Call(Var "LINKS.remoteCall", [Var "__kappa"]),
                     [strlit name; 
                      js_tuple (map (fun v -> Var v) arglist)
                     ]))]

(* Generate a server stub that calls the corresponding server function *)
let generate_server_stub = function
  | Define (n, Abstr (arglist,_,_), `Server, _)
  | Define (n, Rec ([_, (Abstr (arglist,_,_)), _], Variable _, _), `Server, _) ->
      server_stub n arglist
  | def
    -> failwith ("Cannot generate server stub for " ^
                   Syntax.Show_definition.show def)

 let apply_no_yield (f, args) =
  Call (Var f, args)

let apply_yielding (f, args) =
  Call(Var "_yield", (Var f) :: args)

let callk_no_yield arg = apply_no_yield ("__kappa", [arg])

let callk_yielding arg =
  Call(Var "_yieldCont", [Var "__kappa"; arg]) 


let trivial_cps expr = 
  Fn(["__kappa"], callk_yielding expr)

let idy_js = Var("_idy")
let thread_end_k = idy_js
let end_thread expr = Call(expr, [idy_js])

(** make a cps term that constructs XML from XML-typed
    sub-expressions, including direct-style and CPS sub-expressions
    (note anomalous arg order.) *)
let make_xml_cps attrs_cps attrs_direct children_cps children_direct tag = 
  let innermost_expr = 
    Call(Var "LINKS.XMLk",
         [strlit tag;
          Dict (attrs_direct @ map (fun ((k, _), n) -> (k, Var n)) attrs_cps);
          Lst (children_direct @ map (fun (_, n) -> Var n) children_cps);
          Var "__kappa"
         ])
  in
  let tower = fold_right (fun (item, vname) expr ->
                            Call(item, [Fn([vname], expr)])
                         ) children_cps innermost_expr in
  let tower = fold_right (fun ((aname, item), vname) expr ->
                            Call(item, [Fn([vname], expr)])
                         ) attrs_cps tower in
    Fn(["__kappa"], tower)       

(** [cps_prims]: a list of primitive functions that need to see the
    current continuation. Calls to these are translated in CPS rather than
    direct-style.  A bit hackish, this list. *)
let cps_prims = ["recv"; "sleep"; "spawnWait"]

(** {0 Code generation} *)

(** generate
    Generate javascript code for a Links expression
    
    With CPS transform, result of generate is always : (a -> w) -> b
*)
let rec generate : 'a expression' -> code = 
  let generate' expr =  generate expr in
    function
  | HasType (e, _, _)                  -> generate' e
  | Constant (c, _)                    ->
      begin
        match c with
          | Integer v  -> trivial_cps (Lit (string_of_num v))
          | Float v    -> trivial_cps (Lit (string_of_float v))
          | Boolean v  -> trivial_cps (Lit (string_of_bool v))
          | Char v     -> trivial_cps (chrlit v)
          | String v   -> trivial_cps (chrlistlit v)
      end
  | Condition (i, t, e, _)             -> 
      let i_cps = generate' i in
      let t_cps = generate' t in
      let e_cps = generate' e in
      let k = gensym ~prefix:("_if") ()
        (*
          [NOTE]

          need to make sure this abstraction isn't treated as
          part of an administrative redex as the continuation is duplicated
        *)
      in
        Fn([k], 
           Call(i_cps, [Fn(["__i"], Cond (Var "__i",
                                        Call(t_cps, [Var k]),
                                        Call(e_cps, [Var k])))]))
                          
  | Let (v, e, b, _)                   -> 
      let e' = generate' e in
      let b' = generate' b in
      let x = gensym ~prefix:("__" ^ v) () in
        Fn(["__kappa"], 
           Call(e', [Fn([x], 
                        Bind(v, Var x,
                             Call(b', [Var "__kappa"])))]))
  | Variable ("~", _)                  -> trivial_cps (Var "tilde")
  | Variable (name, _) when Binop.is name ->
      trivial_cps
        (Fn (["x"; "y"; "__kappa"],
             callk_yielding (Binop(Var "x", Binop.js_name name, Var "y"))))
  | Variable (name, _)                 ->
        trivial_cps (Var name)
  | Comparison (l, `Equal, r, _)         -> 
      let l_cps = generate' l in
      let r_cps = generate' r in
        Fn(["__kappa"],
           Call(l_cps, [Fn(["__l"],
              Call(r_cps, [Fn(["__r"],
                 callk_yielding (Call(Var "LINKS.eq", [Var "__l"; Var "__r"])))]))]))
  | Comparison (l, `NotEq, r, _)         -> 
      let l_cps = generate' l in
      let r_cps = generate' r in
        Fn(["__kappa"],
           Call(l_cps, [Fn(["__l"],
              Call(r_cps, [Fn(["__r"],
                 callk_yielding (
                   Unop("!", Call(Var "LINKS.eq", [Var "__l"; Var "__r"]))
                 ))]))]))
(*   | Comparison (l, `NotEq, r, _)         ->  *)
(*       let l_cps = generate' l in *)
(*       let r_cps = generate' r in *)
(*         Fn(["__kappa"], *)
(*            Call(l_cps, [Fn(["__l"], *)
(*               Call(r_cps, [Fn(["__r"], *)
(*                  callk_yielding ( *)
(*                    Unop("!", Call(Var "LINKS.eq", [Var "__l"; Var "__r"])) *)
(*                  ))]))])) *)
  | Comparison (l, op, r, _)           -> 
      let l_cps = generate' l in
      let r_cps = generate' r in
        Fn(["__kappa"],
           Call(l_cps, [Fn(["__l"], 
                Call(r_cps, [Fn(["__r"],
                     callk_yielding (Binop(Var "__l", comparison_name op, Var "__r")))]))]))
      (* Should strings be handled differently at this level? *)
  | Nil _                 -> trivial_cps (Lst [])
  | List_of (e, _)        -> 
      let content_cps = generate' e in
        Fn(["__kappa"],
           (Call(content_cps, [Fn(["__x"], callk_yielding (Lst [Var "__x"]))])))
  | Concat _ as c         ->  generate_concat c
  | For (e, v, b, _)  -> 
      let b_cps = generate' b in
      let e_cps = generate' e in
        Fn(["__kappa"],
           Call(b_cps, [Fn(["__b"], Call (Var "LINKS.accum",
                                          [Fn([v; "__kappa"],
                                              Call(e_cps, [Var "__kappa"]));
                                           Var "__b";
                                           Var "__kappa"]))]))
  | Xml_node (tag, attrs, children, _) as xml ->
      let attrs_cps = alistmap generate attrs in 
      let attrs_cps = pair_fresh_names attrs_cps in
      let children_cps = pair_fresh_names (map generate children) in
        make_xml_cps attrs_cps [] children_cps [] tag

  (* Functions *)
  | Abstr (arglist, body, _) ->
      Fn(["__kappa"], 
         callk_yielding (Fn (arglist@ ["__kappa"],
                             Call(generate' body, [Var "__kappa"]))))

  | Abs (p, d) ->
      Fn(["__kappa"], 
         Call(generate' p, 
              [Fn(["__content"],
                  callk_yielding
                    (Call (Var "abs",
                           [Var "__content"])))]))
  | App (f, p, _) -> 
      let f_name    = gensym ~prefix:"__f" () 
      and arg_name  = gensym ~prefix:"__f" () in
        Fn (["__kappa"],
            Call(generate' f,
                 [Fn ([f_name], 
                      (Call(generate' p,
                            [Fn ([arg_name], 
                                 (Call (Var "_yield", 
                                        Call (Var "app", 
                                              [Var f_name])
                                        :: [Lst ([Var arg_name]); 
                                            Var ("__kappa")])))])))]))
  | Apply (Variable (op, _), [l; r], _) when Binop.is op ->
      let l_cps = generate' l in
      let r_cps = generate' r in
        Fn(["__kappa"], 
           Call(l_cps, [Fn(["__l"],
                Call(r_cps, [Fn(["__r"],
                     callk_yielding (Binop (Var "__l", Binop.js_name op, Var "__r")))]))]))
   | Apply (f, p, _  ) -> 
      let kappa = Var("__kappa") in
      let f_cps = generate' f in
      let f_name = gensym ~prefix:"__f" () in
      let arglist = p in
      let cps_args = pair_fresh_names ~prefix:"__f" (map generate' arglist) in
      let arg_names = map snd cps_args in
      let wrap_cps_terms (arg_cps, arg_name) expr = 
        Call(arg_cps, [Fn ([arg_name], expr)])
      in
      let innermost_call =
        match f with
          | Variable (l, _) when Library.is_primitive l && not (mem l cps_prims)
              && Library.primitive_location l <> `Server 
              -> 
              (* Don't yield when calling library functions.
                 In the future library functions should be "native". *)
              Call (Var "__kappa",
                    [Call(Var("_" ^ l),
                         (map (fun name -> Var name) arg_names))])
          | _ ->
              apply_yielding (f_name,
                              [Lst (map (fun name -> Var name) arg_names); 
                               kappa])
      in
      let arg_tower = 
        fold_right wrap_cps_terms cps_args innermost_call
      in
        (match f with 
          | Variable (l, _) when Library.is_primitive l && not (mem l cps_prims)
              && Library.primitive_location l <> `Server ->
              Fn (["__kappa"],  arg_tower)
          | _ -> 
              Fn (["__kappa"],  Call(f_cps, [Fn ([f_name], arg_tower)])))


  | Rec (bindings, body, _) ->
      Fn(["__kappa"],
	 (fold_right 
            (fun (v, e,_) body ->
	       Call(generate' e, [Fn(["__e"],
				    Bind (v, Var "__e", body))]))
            bindings
            (Call (generate' body, [Var "__kappa"]))))
        
  (* Records *)
  | Record_intro (bs, r, _) ->
      let c, dict =
        StringMap.fold (fun label e (c, dict) ->
                          let name = gensym ~prefix:"__e" () in
                            ((fun body -> c (Call(generate' e, [Fn([name], body)]))),
                             (label, Var name) :: dict)
                       ) bs ((fun x -> x), [])
      in
        begin
          match r with
            | None ->
                Fn(["__kappa"], c (callk_yielding (Dict dict)))
            | Some r ->
                Fn(["__kappa"], c (
                     let name = gensym ~prefix:"__r" () in
                       Call(generate' r,
                            [Fn([name],
                                callk_yielding (
                                  Call (Var "LINKS.union", [Var name; Dict dict])))])))
        end
  | Project (expr, label, _) -> 
      let expr_cps = generate' expr in
         Fn(["__kappa"],
           (Call(expr_cps, 
                 [Fn(["__v"], callk_yielding (Call (Var "LINKS.project", [Var "__v"; strlit label])))])))
  | Erase (expr, label, _) -> 
      let expr_cps = generate' expr in
         Fn(["__kappa"],
           (Call(expr_cps, 
                 [Fn(["__v"], callk_yielding (Call (Var "LINKS.erase", [Var "__v"; strlit label])))])))

  (* Variants *)
  | Variant_injection (l, e, _) -> 
      let content_cps = generate' e in
        Fn(["__kappa"], 
           Call(content_cps, [Fn(["__content"],
                                 callk_yielding
                                      (Dict [("_label", strlit l);
                                             ("_value", Var "__content")]))]))
  | Variant_selection_empty (_, _) ->
      Fn(["__kappa"], Call(Var "_fail",
                         [strlit "closed switch got value out of range"]))
  | Variant_selection (src, case_label, case_var, case_body, 
                       else_var, else_body, _) ->
      let src_cps = generate' src in
      let case_body_cps = generate' case_body in
      let else_body_cps = generate' else_body in
      let k = gensym ~prefix:("_case") ()
        (* Note: need to make sure this abstraction isn't treated as
           part of an administrative redex as the continuation is
           duplicated.
        *)
      in
        Fn([k],
           Call(src_cps, [Fn(["__src"],
                             Cond(Binop(Call(Var "LINKS.vrntLbl", [Var "__src"]),
                                        "==",
                                        strlit case_label),
                                  Bind(case_var,
                                       Call(Var "LINKS.vrntVal", [Var "__src"]),
                                       Call(case_body_cps, [Var k])),
                                  Bind(else_var,
                                       Var "__src",
                                       Call(else_body_cps, [Var k]))
                                 )
                            )]))
  | Call_cc (e, _) -> 
      let k = gensym ~prefix:("_cc") ()
        (*
          [NOTE]

          need to make sure this abstraction isn't treated as
          part of an administrative redex as the continuation is duplicated
        *)
      in
        Fn([k],
           Call(generate' e, [Fn(["_f"], 
                                 apply_yielding("_f", 
                                                [Lst [Var k]; Var k]))]
               ))
  | Wrong _ -> Fn(["__kappa"], Die "Internal Error: Pattern matching failed")
  | Database (e, _) ->
      let db_cps = generate' e in
        Fn(["__kappa"],
           Call(db_cps, [Fn(["__db"],
                                 callk_yielding
                                      (Dict [("_db", Var "__db")]))]))
  | TableHandle (db, name, (readtype, writetype), _) ->
      let db_cps = generate' db in
      let name_cps = generate' name in
        Fn(["__kappa"],
           Call(db_cps,
                [Fn(["__db"],
                    Call(name_cps,
                         [Fn(["__name"],
                             callk_yielding
                               (Dict [("_table",
                                       Dict [("db", Var "__db");
                                             ("name", Var "__name");
                                             ("row",
                                              strlit (Types.string_of_datatype (readtype)))])]))]))]))
  (* Unimplemented stuff *)
  | TableQuery _ as e -> failwith ("Cannot (yet?) generate JavaScript code for " ^ string_of_expression e)
  | x -> failwith("Internal Error: JavaScript gen failed with unknown AST object " ^ string_of_expression x)

and generate_concat : 'a expression' -> code = 
    function
      | Concat (c, Nil _, _) -> 
          generate_concat c
      | Concat (l, r, _) ->
          let l_cps = generate l in
          let r_cps = generate r in
            Fn(["__kappa"],
               Call(l_cps, [Fn(["__l"],
                   Call(r_cps, [Fn(["__r"],
                        callk_yielding (Call (Var "LINKS.concat", [Var "__l"; Var "__r"])))]))]))
      | e ->          generate e

and generate_def : 'a definition' -> code = 
  function
      (* Binding *)
    | Define (_, _, `Server, _) as d -> generate_server_stub d
    | Define (_, _, `Native, _) as d -> generate_native_stub d
    | Define (n, e, (`Client|`Unknown), _) -> 
(* [NOTE]
     Passing in _idy doesn't work because we are not in traditional CPS.
     Traditional CPS terms return a value, but ours don't (because JavaScript
     requires you to write an explicit return). To get round this problem, we
     capture the return value by imperative assignment. An alternative would be
     to modify our CPS transform to produce CPS terms that explicitly return
     values.
*)
        Defs ([n, Call (generate e, [Fn (["__x"], Binop(Var n, "=", Var "__x"))])])
    | Alias _ 
    | Alien _ -> Nothing

(* generate direct style code *)
and generate_direct_style : 'a expression' -> code =
  let gcps = generate in
  let gd = generate_direct_style
  in
    function
  | Constant (c, _)                    ->
      begin
        match c with
          | Integer v  -> Lit (string_of_num v)
          | Float v    -> Lit (string_of_float v)
          | Boolean v  -> Lit (string_of_bool v)
          | Char v     -> chrlit v
          | String v   -> chrlistlit v
      end
  | Condition (i, t, e, _)             ->
      Cond (gd i, gd t, gd e)
  | Let (v, e, b, _)                   ->
      Bind(v, gd e, gd b)
  | Variable ("~", _)                  -> Var "tilde"
  | Variable (name, _) when Binop.is name ->
      Fn (["x"; "y"], Ret(Binop(Var "x", Binop.js_name name, Var "y")))
  | Variable (name, _)                   -> Var name
  | Comparison (l, `Equal, r, _)         -> 
      Call(Var "LINKS.eq", [gd l; gd r])
  | Comparison (l, `NotEq, r, _)         -> 
      Unop("!", Call (Var "LINKS.eq", [gd l; gd r]))
  | Comparison (l, op, r, _)           -> 
      Binop(gd l, comparison_name op, gd r)
      (* Should strings be handled differently at this level? *)
  | Nil _                 -> Lst []
  | List_of (e, _)        ->
      Lst [gd e]   
  | Concat (l, r, _)      -> Call (Var "LINKS.concat", [gd l; gd r])
  | For _ -> failwith "not implemented: native comprehensions"
  | Xml_node _ -> failwith "not implemented: handling of XML in native functions"
  (* Functions *)
  | Abstr (arglist, body, _) ->
      Fn (arglist, gd body)
        
  | Abs (f,_) ->
      failwith "nyi js gen direct_style abs"
  | App _ -> 
      failwith "nyi js gen direct_style app"

  | Apply (Variable (op, _), [l; r], _) when Binop.is op -> 
      Binop(gd l, Binop.js_name op, gd r)
  | Apply (f, ps, _  ) ->
      Call(gd f, map gd ps)

  (* Binding *)
  | Rec (bindings, body, _) ->
      List.fold_right
	(fun (v, e,_) body ->
	   Bind (v, gd e, body))
	bindings
	(gd body)

  (* Records *)
  | Record_intro (bs, r, _) ->
      let dict =
        Dict (StringMap.to_list (fun label e -> label, 
                                         generate e) bs)
      in
        begin
          match r with
            | None -> dict
            | Some r ->
                Call (Var "LINKS.union", [dict; generate r])
        end
  | Project (expr, label, _) ->
      Call (Var "LINKS.project", [gd expr; strlit label])
  | Erase (expr, label, _) ->
      Call (Var "LINKS.erase", [gd expr; strlit label])
  | Erase (expr, label, _) -> 
      (* should we do anything here? *)
      gd expr
  (* Variants *)
  | Variant_injection (l, e, _) -> 
      Dict [("_label", strlit l); ("_value", gd e)]
  | Variant_selection_empty _ ->
      Call(Var "_fail", [strlit "closed switch got value out of range"])
  | Variant_selection (src, case_label, case_var, case_body, 
                       else_var, else_body, _) ->
      let src_var = gensym ~prefix:"_s" () in
      Bind(src_var, gd src,
	   Cond(Binop(Call(Var "LINKS.vrntLbl", [Var src_var]),
                      "==",
                      strlit case_label),
		Bind(case_var,
                     Call(Var "LINKS.vrntVal", [Var src_var]),
                     gd case_body),
		Bind(else_var,
                     Var "__src",
                gd else_body)))
  | Call_cc _ -> 
      failwith "escape cannot be used in native code"
  | Wrong _ -> Nothing (* FIXME: should be a js `throw' *)
  | HasType (e, _, _) -> gd e

  (* Unimplemented stuff *)
  | Database _
  | TableHandle _
  | SortBy _
  | TableQuery _ as e -> failwith ("Cannot (yet?) generate JavaScript code for " ^ string_of_expression e)

and generate_direct_style_def : 'a definition' -> code = 
  function
      (* Binding *)
    | Define _ as d -> generate_def d
    | Alias _ 
    | Alien _ -> Nothing

(* Generate a native stub that calls the corresponding native function *)
and generate_native_stub = function
  | Define (n, Rec ([_, (Abstr (arg,body,_)), _], Variable _, _), `Native, _) ->
      let arglist = arg in
        Defs [n, Fn (arglist @ ["__kappa"], callk_yielding (generate_direct_style body))]
  | def -> failwith ("Cannot generate native stub for " ^ string_of_definition def)
      
let rec freevars = 
  let fv = freevars in
  let module S = StringSet in function
  | Var x               -> S.singleton x
  | Defs _              -> S.empty
  | Fn (args, body)     -> S.diff (fv body) (S.from_list args)
  | Call (func, args)   -> S.union (fv func) (S.union_all (map fv args))
  | Unop (_, body)      -> fv body
  | Binop (l, _, r)     -> S.union (fv l) (fv r)
  | Cond (a, b, c)      -> S.union (S.union (fv a) (fv b)) (fv c)
  | Dict terms          -> S.union_all (map (snd ->- fv) terms)
  | Lst terms           -> S.union_all (map fv terms)
  | Bind (var, e, body) -> S.union (fv e) (S.remove var (fv body))
  | Seq (l, r)          -> S.union (fv l) (fv r)
  | Ret a               -> fv a
  | Lit _ 
  | Die _ 
  | Nothing             -> S.empty
  | _ -> assert false

(* FIXME: There is some problem with this whereby variables are captured *)
let rec replace' var replcmt fvs = function
  | Var x when x = var -> replcmt
  | Defs defs -> Defs(alistmap (replace' var replcmt (freevars replcmt)) defs)
  | Fn(args, body) when not (mem var args) -> 
      (* this may be unnecessary, if whole expr. is uniquified previously *)
     let args, body =
        if StringSet.is_empty (StringSet.inter (StringSet.from_list args) fvs)
        then (args, body) else
          uniquify_args(args, body)
     in
        Fn(args, replace' var replcmt fvs body)
  | Call(func, args) -> Call(replace' var replcmt fvs func,
                             map (replace' var replcmt fvs) args)
  | Unop(op, body) -> Unop(op,
                           replace' var replcmt fvs body)
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
  | Ret(e) -> Ret(replace' var replcmt fvs e)
  | simple_expr -> simple_expr
and replace var expr body = replace' var expr (freevars expr) body
and uniquify_args = function
    (args, body) ->
      let subst = Utility.refresh_names args in
        (map snd subst,
         fold_right (fun (old, noo) body ->
                       replace' old (Var noo) (StringSet.singleton noo) body)
           subst body)

let sum xs = fold_right (+) xs 0

let rec uses x = function
    Var x' -> if x == x' then 1 else 0
  | Die _ | Lit _ | Nothing -> 0
  | Defs _ -> 9999
  | Fn (args, body) when (mem x args) -> 0
  | Fn (args, body) -> uses x body
  | Call (f, args) -> uses x f + sum (map (uses x) args)
  | Unop(op, body) -> uses x body
  | Binop(l, op, r) -> uses x l + uses x r
  | Cond(test, tbranch, fbranch) -> uses x test + 
      (max (uses x tbranch) (uses x fbranch))
  | Dict(alist) -> sum (map snd (alistmap (uses x) alist))
  | Lst(elems) -> sum (map (uses x) elems)
  | Bind(var, defns, body) when (x <> var) -> uses x defns + uses x body 
  | Bind(var, defns, body) -> uses x defns
  | Seq(first, second) -> uses x first + uses x second
  | Ret code -> uses x code

(* reduce administrative redexes *)
let rec simplify : Rewrite_code.rewriter = function
  (* beta reduction *)
  | Call(Fn([formal_arg], body), [actual_arg])
  | Call(Var "_yieldCont", [Fn([formal_arg], body); actual_arg])
      when Str.string_match (Str.regexp "^__") formal_arg 0 
        || uses formal_arg body <= 1 ->
      Some (replace formal_arg actual_arg body)
  | Call(Var "_idy", [arg]) -> Some arg

  (* eta reduction *)
  | Fn([arg], Call(f, [Var arg']))
      when arg = arg' && Str.string_match (Str.regexp "^__") arg 0
    ->
      Some f
  | _ -> None

let simplify_throughout code =
  from_option code (Rewrite_code.bottomup simplify code)

let rec iterate_to_fixedpoint f x =
  let x' = f x in
    if x = x' then x
    else iterate_to_fixedpoint f x'


let simplify_completely = iterate_to_fixedpoint simplify_throughout

let rec eliminate_admin_redexes = 
  simplify_completely

let gen_def ?(pre_opt=identity) =
  generate_def
  ->- eliminate_admin_redexes
  ->- pre_opt
  ->- optimise

let gen ?(pre_opt=identity) =
  generate
  ->- eliminate_admin_redexes
  ->- pre_opt
  ->- optimise

module Symbols :
sig
  val rename : Syntax.expression -> Syntax.expression
  val rename_def : Syntax.definition -> Syntax.definition
end =
struct
  let words =
    CharMap.from_alist
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

  let js_keywords = ["break"; "else";"new";"var";"case";"finally";"return";"void";
                     "catch";"for";"switch";"while";"continue";"function";"this";
                     "with";"default";"if";"throw";"delete";"in";"try";"do";
                     "instanceof";"typeof";
                     (* "future keywords" *)
                     "abstract";"enum";"int";"short";"boolean";"export";
                     "interface";"static";"byte";"extends";"long";"super";"char";
                     "final";"native";"synchronized";"class";"float";"package";
                     "throws";"const";"goto";"private";"transient";"debugger";
                     "implements";"protected";"volatile";
                    ]

  let has_symbols name =
    not (Library.is_primitive name) &&
      List.exists (not -<- Utility.Char.isWord) (explode name)

  let wordify name = 
    if has_symbols name then 
      ("_" ^ 
         mapstrcat "_" 
         (fun ch ->
            if (Utility.Char.isWord ch) then
              String.make 1 ch
            else if CharMap.mem ch words then
              CharMap.find ch words
            else
              failwith("Internal error: unknown symbol character: "^String.make 1 ch))
         (Utility.explode name))
        (* TBD: it would be better if this split to chunks maximally matching
           (\w+)|(\W)
           then we would not split apart words in partly-symbolic idents. *)
    else if mem name js_keywords then
      "_" ^ name (* FIXME: this could conflict with Links names. *)
    else name
      
  let rename exp = 
    from_option exp
      (RewriteSyntax.bottomup
         (function
            | Variable (name, d) -> Some (Variable (wordify name, d))
            | Let (name, rhs, body, d) -> 
                Some (Let (wordify name, rhs, body, d))
            | Rec (bindings, body, d) -> 
                let rename (nm, body, t) = (wordify nm, body, t) in
                  Some (Rec (List.map rename bindings, body, d))
            | _ -> None)
         exp)

  let rename_def def =
    match def with
      | Define (name, b, l, t) -> 
          Define (wordify name, rename b, l, t)
      | Alias (name, _, _, _)
      | Alien (name, _, _, _) when has_symbols name ->
          assert false (* symbols shouldn't appear in types or foreign functions *)
      | _ -> def
end






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

let rec some_vars = function 
    0 -> []      
  | n -> ("x"^string_of_int n) :: some_vars (n-1)

let generate_program_defs defs root_names =
  let aliens = get_alien_names defs in
  let defs = List.map Symbols.rename_def defs in
  (* NOTE: the body is not really used here. *)
  let body = Syntax.unit_expression Syntax.no_expr_data in
  let (Program (defs, body)) =
    if Settings.get_value optimising then
      Optimiser.inline(Optimiser.inline(Optimiser.inline(Program(defs, body))))
    else Program (defs, body)
  in
  let library_names = aliens @ StringSet.elements (Env.String.domain (fst Library.typing_env)) in
  let prim_server_stubs =
    concat_map (fun (name, _) -> 
           match Library.primitive_arity name with
               None -> []
             | Some arity ->
                 [server_stub name (some_vars arity)])
      (filter (fun (name,_) -> Library.primitive_location name = `Server) 
         (StringMap.to_alist !Library.value_env)) in
  let defs =
    (if Settings.get_value elim_dead_defs then
       Callgraph.elim_dead_defs library_names defs root_names
     else defs) in
  let global_names = Syntax.defined_names defs @ library_names in
    map show_pp prim_server_stubs @
      map (fixup_hrefs_def (StringSet.from_list global_names) ->- gen_def ->- 
             show_pp) defs

let generate_program ?(onload = "") (Program(defs,expr)) =
  let expr = Symbols.rename expr in
  let defs = map Symbols.rename_def defs in
  let js_defs = generate_program_defs defs (Syntax.freevars expr) in
  let library_names = StringSet.elements (Env.String.domain (fst Library.typing_env)) in
  let global_names = Syntax.defined_names defs @ library_names in
  let js_root_expr = 
    (gen
       ~pre_opt:(fun expr -> Call(expr, [Var "_start"]))
       (fixup_hrefs (StringSet.from_list global_names) expr))
  in
    (make_boiler_page ~body:(show_pp js_root_expr) js_defs)
     
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

let links2js s = 
  let Program (defs, body) =
    (Parse.parse_string Parse.program ->- fst
     ->- Inference.type_program Library.typing_env ->- snd) s in
  let defs = map ((rewrite_def (Optimiser.uniquify_names)) ->- generate_def) defs in
  let body = ((Utility.perhaps_apply Optimiser.uniquify_names) ->- generate) body in
    map simplify_completely (defs @ [body])
  
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
                     | _ -> false
                 )

let _ = add_qtest("fun f(x) { x+1 } f(1)",
                  fun rslt ->
                   match rslt with
                       Defs([_, Bind(fname, Fn(["__kappa"], Fn([xname], 
                                  Call(Var "__kappa",
                                       [Binop(Var xname2, "+", Lit "1")]))),
                                     Var fname2)])::_
                         when fname = fname2
                           && xname = xname2
                           -> true
                     | _ -> false
                 )

let lstrip s = List.hd (Str.bounded_split (Str.regexp "[ \t\n]+") s 1)

let rhino_output linkscode =  
  let gen s =
    let Program (defs, body) =
      (Parse.parse_string Parse.program ->- fst
       ->- Inference.type_program Library.typing_env ->- snd) s
    in
      ((Utility.perhaps_apply Optimiser.uniquify_names)
       ->- generate
         ->- show) body in
  
  let tempfile = Filename.temp_file "linkstest" ".js" in
  let cleanup () = (try Sys.remove tempfile with _ -> ()) in
    try
      let channel = open_out tempfile in 
      let s = gen linkscode in
        Debug.f "generated code for %s:\n%s\n" linkscode s;
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




