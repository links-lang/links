(*pp deriving *)

(** JavaScript generation *)
open Utility

let js_lib_url = Basicsettings.Js.lib_url
let js_pretty_print = Basicsettings.Js.pp

let js_hide_database_info = Basicsettings.Js.hide_database_info

let get_js_lib_url () =
  let open Pervasives in
  let base_url = Settings.get_value Basicsettings.Appserver.external_base_url |> strip_slashes in
  let base_url = Utility.strip_slashes base_url in
  let js_url = Settings.get_value js_lib_url |> strip_slashes in
  if base_url = "" then
    "/" ^ js_url ^ "/"
  else
    "/" ^ base_url ^ "/" ^ js_url ^ "/"

(* strip any top level polymorphism from an expression *)
let rec strip_poly =
  function
    | `TAbs (_, e)
    | `TApp (e, _) -> strip_poly e
    | e -> e

(** Intermediate language *)
type code = | Var    of string
            | Lit    of string
            | Fn     of (string list * code)

            | LetFun of ((string * string list * code * Ir.location) * code)
            | LetRec of ((string * string list * code * Ir.location) list * code)
            | Call   of (code * code list)
            | Unop   of (string * code)
            | Binop  of (code * string * code)
            | If     of (code * code * code)
            | Case   of (string * (string * code) stringmap * (string * code) option)
            | Dict   of ((string * code) list)
            | Arr    of (code list)

            | Bind   of (string * code * code)
            | Return of code

            | Die    of (string)
            | Nothing
  deriving (Show)


(** IR variable environment *)
module VEnv = Env.Int

(** Type of environments mapping IR variables to source variables *)
type venv = string VEnv.t


(** Continuation parameter name (convention) *)
let __kappa = "__kappa"
(**
  Required runtime support (documenting any JavaScript functions used):

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

  Also, any `builtin' functions from Lib.value_env.
 *)

(** Ugly printer for JavaScript code *)
module UP :
sig
  val show : code -> string
end =
struct
  let rec show code =
    let show_func name fn =
      match fn with
      | Fn (vars, body) ->
         "function "^ name ^"("^ String.concat ", " vars ^")"^"{ " ^" "^show body^"; }"
      | _ -> assert false
    and arglist args = String.concat ", " (List.map show args)
    and paren = function
      | Var _
      | Lit _
      | Call _
      | Dict _
      | Arr _
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

        | LetFun ((name, vars, body, _location), rest) ->
            (show_func name (Fn (vars, body))) ^ show rest
        | LetRec (defs, rest) ->
            String.concat ";\n" (List.map (fun (name, vars, body, _loc) -> show_func name (Fn (vars, body))) defs) ^ show rest
        | Call (Var "LINKS.project", [record; label]) -> (paren record) ^ "[" ^ show label ^ "]"
        | Call (Var "hd", [list;kappa]) -> Printf.sprintf "%s(%s[0])" (paren kappa) (paren list)
        | Call (Var "tl", [list;kappa]) -> Printf.sprintf "%s(%s.slice(1))" (paren kappa) (paren list)
        | Call (Var "_yield", fn :: args) -> Printf.sprintf "_yield(function() { %s(%s) })" (paren fn) (arglist args)
        | Call (fn, args) -> paren fn ^ "(" ^ arglist args  ^ ")"
        | Unop (op, body) -> op ^ paren body
        | Binop (l, op, r) -> paren l ^ " " ^ op ^ " " ^ paren r
        | If (cond, e1, e2) ->
            "if (" ^ show cond ^ ") {" ^ show e1 ^ "} else {" ^ show e2 ^ "}"
        | Case (v, cases, default) ->
            "switch (" ^ v ^ "._label) {" ^ show_cases v cases ^ show_default v default ^ "}"
        | Dict (elems) -> "{" ^ String.concat ", " (List.map (fun (name, value) -> "'" ^  name ^ "':" ^ show value) elems) ^ "}"
        | Arr elems -> "[" ^ arglist elems ^ "]"
        | Bind (name, value, body) ->  name ^" = "^ show value ^"; "^ show body
        | Return expr -> "return " ^ (show expr) ^ ";"
        | Nothing -> ""
        | Die msg -> "error('" ^ msg ^ "', __kappa)"
end

(** Pretty printer for JavaScript code *)
module PP :
sig
  val show : code -> string
end =
struct
  open PP

  (** Pretty-print a Code value as a JavaScript string. *)
  let rec show c : PP.doc =
    let show_func name fn =
      match fn with
      | (Fn (vars, body)) ->
         PP.group (PP.text "function" ^+^ PP.text name ^^ (formal_list vars)
                   ^+^  (braces
                           (break ^^ group(nest 2 (show body)) ^^ break)))
      | _ -> assert false in
    let show_case v l (x, e) =
      PP.text "case" ^+^ PP.text("'"^l^"'") ^^
        PP.text ":" ^+^ braces (PP.text"var" ^+^ PP.text x ^+^ PP.text "=" ^+^ PP.text (v^"._value;") ^+^
                                  show e ^^ PP.text ";" ^+^ PP.text "break;") ^^ break in
    let show_cases v =
      fun cases ->
        StringMap.fold (fun l c s ->
                          s ^+^ show_case v l c)
          cases DocNil in
    let show_default v = opt_app
      (fun (x, e) ->
         PP.text "default:" ^+^ braces (PP.text "var" ^+^ PP.text x ^+^ PP.text "=" ^+^ PP.text (v^";") ^+^
                                          show e ^^ PP.text ";" ^+^ PP.text "break;") ^^ break) PP.DocNil in
    let maybe_parenise = function
      | Var _
      | Lit _
      | Call _
      | Dict _
      | Arr _
      | Bind _
      | Die _
      | Return _
      | Nothing as c -> show c
      | c -> parens (show c)
    in
      match c with
        | Var x -> PP.text x
        | Nothing -> PP.text ""
        | Die msg -> PP.text("error('" ^ msg ^ "', __kappa)")
        | Lit literal -> PP.text literal
        | LetFun ((name, vars, body, _location), rest) ->
            (show_func name (Fn (vars, body))) ^^ break ^^ show rest
        | LetRec (defs, rest) ->
            PP.vsep (punctuate " " (List.map (fun (name, vars, body, _loc) -> show_func name (Fn (vars, body))) defs)) ^^
              break ^^ show rest
        | Fn _ as f -> show_func "" f
        | Call (Var "LINKS.project", [record; label]) ->
            maybe_parenise record ^^ (brackets (show label))
        | Call (Var "hd", [list;kappa]) ->
            (maybe_parenise kappa) ^^ (parens (maybe_parenise list ^^ PP.text "[0]"))
        | Call (Var "tl", [list;kappa]) ->
            (maybe_parenise kappa) ^^ (parens (maybe_parenise list ^^ PP.text ".slice(1)"))
        | Call (Var "_yield", (fn :: args)) ->
            PP.text "_yield" ^^ (parens (PP.text "function () { " ^^ maybe_parenise fn ^^
                                    parens (hsep(punctuate "," (List.map show args))) ^^ PP.text " }"))
        | Call (fn, args) -> maybe_parenise fn ^^
            (PP.arglist (List.map show args))
        | Unop (op, body) -> PP.text op ^+^ (maybe_parenise body)
        | Binop (l, op, r) -> (maybe_parenise l) ^+^ PP.text op ^+^ (maybe_parenise r)
        | If (cond, c1, c2) ->
            PP.group (PP.text "if (" ^+^ show cond ^+^ PP.text ")"
                      ^+^  (braces
                              (break ^^ group(nest 2 (show c1)) ^^ break))
                      ^+^ PP.text "else"
                      ^+^  (braces
                              (break ^^ group(nest 2 (show c2)) ^^ break)))
        | Case (v, cases, default) ->
            PP.group (PP.text "switch" ^+^ (parens (PP.text (v^"._label"))) ^+^
                        (braces ((show_cases v cases) ^+^ (show_default v default))))
        | Dict (elems) ->
            PP.braces (hsep (punctuate ","
                               (List.map (fun (name, value) ->
                                       group (PP.text "'" ^^ PP.text name ^^
                                                PP.text "':" ^^ show value))
                                  elems)))
        | Arr elems -> brackets(hsep(punctuate "," (List.map show elems)))
        | Bind (name, value, body) ->
            PP.text "var" ^+^ PP.text name ^+^ PP.text "=" ^+^ show value ^^ PP.text ";" ^^
              break ^^ show body
        | Return expr ->
           PP.text "return " ^^ (show expr) ^^ PP.text ";"

  let show = show ->- PP.pretty 144
end

let show x =
  if Settings.get_value(js_pretty_print) then
    PP.show x
  else
    UP.show x

(** Create a JS string literal, quoting special characters *)
let string_js_quote s =
  let sub old repl s = Str.global_replace (Str.regexp old) repl s in
    "'" ^ sub "'" "\\'" (sub "\n" "\\n" (sub "\\" "\\\\\\\\" s)) ^ "'"

(** Return a JS literal string from an OCaml int. *)
let intlit i = Lit (string_of_int i)
(** Return a JS literal string from an OCaml string. *)
let strlit s = Lit (string_js_quote s)
(** Return a JS literal string from an OCaml character. *)
let chrlit ch = Dict ["_c", Lit(string_js_quote(string_of_char ch))]
(** Return a literal for the JS representation of a Links string. *)
let chrlistlit = strlit

(* Specialness:

   * Top-level boilerplate code to replace the root element and reset the focus

     The special function _start takes an html page as a string and
     replaces the currently displayed page with that one.

     Some of the other functions are equivalents to Links builtins
     (e.g. int_of_string, xml)
 *)

let ext_script_tag ?(base=get_js_lib_url()) file =
    "  <script type='text/javascript' src=\""^base^file^"\"></script>"

module Arithmetic :
sig
  val is : string -> bool
  val gen : (code * string * code) -> code
end =
struct
  let builtin_ops =
    StringMap.from_alist
      [ "+",   Some "+"  ;
        "+.",  Some "+"  ;
        "-",   Some "-"  ;
        "-.",  Some "-"  ;
        "*",   Some "*"  ;
        "*.",  Some "*"  ;
        "/",   None      ;
        "^",   None      ;
        "^.",  None      ;
        "/.",  Some "/"  ;
        "mod", Some "%"  ]

  let is x = StringMap.mem x builtin_ops
  let js_name op = val_of (StringMap.find op builtin_ops)
  let gen (l, op, r) =
    match op with
      | "/" -> Call (Var "Math.floor", [Binop (l, "/", r)])
      | "^" -> Call (Var "Math.floor", [Call (Var "Math.pow", [l; r])])
      | "^." -> Call (Var "Math.pow", [l; r])
      | _ -> Binop(l, js_name op, r)
end

module StringOp :
sig
  val is : string -> bool
  val gen : (code * string * code) -> code
end =
struct
  let builtin_ops =
    StringMap.from_alist
      [ "^^",  Some "+"  ]

  let is x = StringMap.mem x builtin_ops
  let js_name op = val_of (StringMap.find op builtin_ops)
  let gen (l, op, r) =
    Binop(l, js_name op, r)
end

module Comparison :
sig
  val is : string -> bool
  val js_name : string -> string
  val gen : (code * string * code) -> code
end =
struct
  (* these names should be used for non-primitive types *)
  let funs =
    StringMap.from_alist
      [ "==", "LINKS.eq"  ;
        "<>", "LINKS.neq" ;
        "<",  "LINKS.lt"  ;
        ">",  "LINKS.gt"  ;
        "<=", "LINKS.lte" ;
        ">=", "LINKS.gte" ]

  let is x = StringMap.mem x funs
  let js_name op = StringMap.find op funs
  let gen (l, op, r) =
    match op with
      | "<>" -> Unop("!", Call (Var "LINKS.eq", [l; r]))
          (* HACK

             This is technically wrong, but as we haven't implemented
             LINKS.lt, etc., it is enough to get the demos working.

             Ideally we want to compile to the builtin operators
             whenever we know that the argument types are primitive
             and the general functions otherwise. This would
             necessitate making the JS compiler type-aware.
          *)
      | "<" | ">" | "<=" | ">=" ->
          Binop(l, op, r)
      | _ ->  Call(Var (js_name op), [l; r])
end

(** [cps_prims]: a list of primitive functions that need to see the
    current continuation. Calls to these are translated in CPS rather than
    direct-style.  A bit hackish, this list. *)
let cps_prims = ["recv"; "sleep"; "spawnWait"; "receive"; "request"; "accept"]

(** Generate a JavaScript name from a binder, wordifying symbolic names *)
let name_binder (x, info) =
  let name = Js.name_binder (x, info) in
  if (name = "") then
    prerr_endline (Ir.Show_binder.show (x, info))
  else
    ();
  assert (name <> "");
  (x, Js.name_binder (x,info))

(** Continuation structures *)
module type CONTINUATION = sig
  (* Invariant: the continuation structure is algebraic. For
     programming purposes it is instructive to think of a continuation
     as an abstract list. *)
  type t

  val toplevel : t
  (* A continuation is a monoid. *)
  val identity : t
  val (<>) : t -> t -> t

  (* Returns a scope in which the head and tail of the continuation
     are accessible. *)
  val pop : t -> (code -> code) * t * t

  (* Turns code into a continuation. *)
  val reflect : code -> t
  (* Turns a continuation into code. *)
  val reify   : t -> code

  (* Continuation name binding. *)
  val bind : t -> (t -> code) -> code

  (* Continuation application generation. The optional strategy
     parameter decides whether the application should be yielding or
     direct. *)
  val apply : ?strategy:[`Yield | `Direct] -> t -> code -> code

  (* Augments a function [Fn] with a continuation parameter and
     reflects the result as a continuation. The continuation parameter
     in the callback provides access to the current continuation. *)
  val contify_with_env : (t -> venv * code) -> venv * t

  (* Generates appropriate bindings for primitives *)
  val primitive_bindings : string
end

(* (\* The standard Links continuation (no extensions) *\) *)
module Default_Continuation : CONTINUATION = struct
  (* We can think of this particular continuation structure as
     singleton list. *)
  type t = Identity
         | Code of code

  let identity = Identity
  let toplevel = Code (Fn (["x"], Nothing))
  (* This continuation is a degenerate monoid. The multiplicative
     operator is "forgetful" in the sense that it ignores its second
     argument b whenever a != Identity. Meaning that a <> b == a when
     a != Identity. In other words, composition never yields an
     interesting new element. *)
  let (<>) a b =
    match a with
    | Identity -> b
    | Code _ -> a

  let reflect x = Code x
  let reify = function
    | Identity -> Fn (["_x"], Var "_x")
    | Code code -> code

  let bind k body =
    match k with
    | Code (Var _) -> body k
    | _ ->
        (* It is important to generate a unique name for continuation
           bindings because in the JavaScript code:

           var f = e;
           var f = function (args) {C[f]};

           the inner f is bound to function (args) {C[f]} and not e as we
           might expect in a saner language. (In other words var f =
           function(args) {body} is just syntactic sugar for function
           f(args) {body}.)
        *)
       let kb = "_kappa" ^ (string_of_int (Var.fresh_raw_var ())) in
       Bind (kb, reify k, body (reflect (Var kb)))

  let apply ?(strategy=`Yield) k arg =
    match strategy with
    | `Direct -> Call (Var "_applyCont", [reify k; arg])
    | _       -> Call (Var "_yieldCont", [reify k; arg])

  let primitive_bindings =
    "function _makeCont(k) { return k; }\n" ^
      "var _idy = function(x) { return; };\n" ^
      "var _applyCont = _applyCont_Default; var _yieldCont = _yieldCont_Default;\n" ^
        "var _cont_kind = \"Default_Continuation\";\n" ^
          "function is_continuation(value) {return value != undefined && (typeof value == 'function' || value instanceof Object && value.constructor == Function); }"

  let contify_with_env fn =
    match fn Identity with
    | env, (Fn _ as k) -> env, reflect k
    | _ -> failwith "error: contify: none function argument."

  (* Pop returns the code in "the singleton list" as the second
     component, and returns a fresh singleton list containing the
     identity element in the third component. *)
  let pop k = (fun code -> code), k, Identity
end

(* The higher-order continuation structure for effect handlers
   support *)
module Higher_Order_Continuation : CONTINUATION = struct
  (* We can think of this particular continuation structure as a
     nonempty stack with an even number of elements. *)
  type t = Cons of code * t
         | Reflect of code
         | Identity

  (* Auxiliary functions for manipulating the continuation stack *)
  let nil = Var "lsNil"
  let cons x xs = Call (Var "_lsCons", [x; xs])
  let head xs = Call (Var "_lsHead", [xs])
  let tail xs = Call (Var "_lsTail", [xs])
  let toplevel = Cons (Var "_idk", Cons (Var "_efferr", Reflect nil))

  let reflect x = Reflect x
  let rec reify = function
  | Cons (v, vs) ->
    cons v (reify vs)
  | Reflect v -> v
  | Identity -> reify toplevel

  let identity = Identity
  let (<>) a b =
    match a,b with
    | Identity, b -> b
    | a, Identity -> a
    | Reflect ks, b -> Cons (ks, b)
    | Cons _ as a,b ->
       let rec append xs ys =
         match xs with
         | Cons (x, xs) -> Cons (x, append xs ys)
         | Reflect ks   -> Cons (ks, ys)
         | Identity     -> ys
       in
       append a b

  let bind kappas body =
    let rec bind bs ks =
      fun kappas ->
        match kappas with
        | Identity ->
           let k = gensym ~prefix:"_kappa" () in
          (fun code -> bs (Bind (k, reify Identity, code))), ks, Var k
        | Reflect ((Var _) as v) ->
           bs, ks, v
        | Reflect v ->
           let k = gensym ~prefix:"_kappa" () in
           (fun code -> bs (Bind (k, v, code))), ks, Var k
        | Cons ((Var _) as v, kappas) ->
           bind bs (fun kappas -> Cons (v, kappas)) kappas
        | Cons (v, kappas) ->
           let k = gensym ~prefix:"_kappa" () in
           bind
             (fun code -> bs (Bind (k, v, code)))
             (fun kappas -> Cons (Var k, kappas)) kappas
  in
  let bs, ks, seed = bind (fun code -> code) (fun kappas -> kappas) kappas in
  bs (body (ks (reflect seed)))

  let apply ?(strategy=`Yield) k arg =
    match strategy with
    | `Direct -> Call (Var "_applyCont", [reify k; arg])
    | _       -> Call (Var "_yieldCont", [reify k; arg])

  let primitive_bindings =
    "function _makeCont(k) {\n" ^
      "  return _lsCons(k, _lsSingleton(_efferr));\n" ^
      "}\n" ^
      "var _idy = _makeCont(function(x, ks) { return; }); var _idk = function(x,ks) { };\n" ^
      "var _applyCont = _applyCont_HO; var _yieldCont = _yieldCont_HO;\n" ^
        "var _cont_kind = \"Higher_Order_Continuation\";\n" ^
          "function is_continuation(kappa) {\n" ^
            "return kappa !== null && typeof kappa === 'object' && _lsHead(kappa) !== undefined && _lsTail(kappa) !== undefined;\n" ^
              "}"

  let contify_with_env fn =
    let name = __kappa in
    match fn (reflect (Var name)) with
    | env, Fn (args, body) -> env, reflect (Fn (args @ [name], body))
    | _ -> failwith "error: contify: none function argument."

  let rec pop = function
    | Cons (kappa, kappas) ->
       (fun code -> code), (reflect kappa), kappas
    | Reflect ks ->
       let __k = gensym ~prefix:"__k" () in
       let __ks = gensym ~prefix:"__ks" () in
       (fun code ->
         Bind (__k, head ks,
               Bind (__ks, tail ks, code))),
       (reflect (Var __k)), reflect (Var __ks)
    | Identity -> pop toplevel
end

(** Compiler interface *)
module type WEB_COMPILER = sig
  val generate_program_page : ?cgi_env:(string * string) list ->
    (Var.var Env.String.t * Types.typing_environment) ->
    Ir.program -> Loader.ext_dep list -> string

  val generate_real_client_page : ?cgi_env:(string * string) list ->
    (Var.var Env.String.t * Types.typing_environment) ->
    Ir.binding list -> (Value.env * Value.t) ->
    Webserver_types.websocket_url option -> Loader.ext_dep list -> string

  val make_boiler_page :
    ?cgi_env:(string * string) list ->
    ?onload:string ->
    ?body:string ->
    ?html:string ->
    ?head:string ->
    ?external_files:string list -> string list -> string
end

(** [generate]
    Generates JavaScript code for a Links expression

    With CPS transform, result of generate is always of type : (a -> w) -> b
*)
module CPS_Compiler: functor (K : CONTINUATION) -> sig
  include WEB_COMPILER
end = functor (K : CONTINUATION) -> struct
  type continuation = K.t

  let apply_yielding f args k =
    Call (Var "_yield", f :: (args @ [K.reify k]))

  let contify fn =
    snd @@ K.contify_with_env (fun k -> VEnv.empty, fn k)

  let rec generate_value env : Ir.value -> code =
    let gv v = generate_value env v in
    function
    | `Constant c ->
       begin
         match c with
         | `Int v  -> Lit (string_of_int v)
         | `Float v    ->
            let s = string_of_float' v in
            let n = String.length s in
                    (* strip any trailing '.' *)
            if n > 1 && (s.[n-1] = '.') then
              Lit (String.sub s 0 (n-1))
            else
              Lit s
         | `Bool v  -> Lit (string_of_bool v)
         | `Char v     -> chrlit v
         | `String v   -> chrlistlit v
       end
    | `Variable var ->
          (* HACK *)
       let name = VEnv.lookup env var in
       if Arithmetic.is name then
         Fn (["x"; "y"; __kappa],
             K.apply (K.reflect (Var __kappa))
               (Arithmetic.gen (Var "x", name, Var "y")))
       else if StringOp.is name then
         Fn (["x"; "y"; __kappa],
             K.apply (K.reflect (Var __kappa))
               (StringOp.gen (Var "x", name, Var "y")))
       else if Comparison.is name then
         Var (Comparison.js_name name)
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
       Call (Var "LINKS.project", [gv v; strlit name])
    | `Erase (names, v) ->
       Call (Var "LINKS.erase",
             [gv v; Arr (List.map strlit (StringSet.elements names))])
    | `Inject (name, v, _t) ->
       Dict [("_label", strlit name);
             ("_value", gv v)]

      (* erase polymorphism *)
    | `TAbs (_, v)
    | `TApp (v, _) -> gv v

    | `XmlNode (name, attributes, children) ->
       generate_xml env name attributes children

    | `ApplyPure (f, vs) ->
       let f = strip_poly f in
       begin
         match f with
         | `Variable f ->
            let f_name = VEnv.lookup env f in
            begin
              match vs with
              | [l; r] when Arithmetic.is f_name ->
                 Arithmetic.gen (gv l, f_name, gv r)
              | [l; r] when StringOp.is f_name ->
                 StringOp.gen (gv l, f_name, gv r)
              | [l; r] when Comparison.is f_name ->
                 Comparison.gen (gv l, f_name, gv r)
              | [v] when f_name = "negate" || f_name = "negatef" ->
                 Unop ("-", gv v)
              | _ ->
                 if Lib.is_primitive f_name
                   && not (List.mem f_name cps_prims)
                   && Lib.primitive_location f_name <> `Server
                 then
                   Call (Var ("_" ^ f_name), List.map gv vs)
                 else
                   Call (gv (`Variable f), List.map gv vs)
            end
         | _ ->
            Call (gv f, List.map gv vs)
       end
    | `Closure (f, v) ->
       Call (Var "partialApply", [gv (`Variable f); gv v])
    | `Coerce (v, _) ->
       gv v

  and generate_xml env tag attrs children =
    Call(Var "LINKS.XML",
         [strlit tag;
          Dict (StringMap.fold (fun name v bs ->
            (name, generate_value env v) :: bs) attrs []);
          Arr (List.map (generate_value env) children)])

  let generate_remote_call f_var xs_names env =
    Call(Call (Var "LINKS.remoteCall", [Var __kappa]),
         [intlit f_var;
          env;
          Dict (
            List.map2
              (fun n v -> string_of_int n, Var v)
              (Utility.fromTo 1 (1 + List.length xs_names))
              xs_names
          )])


(** Generate stubs for processing functions serialised in remote calls *)
  module GenStubs =
  struct
    let rec fun_def : Ir.fun_def -> code -> code =
      fun ((fb, (_, xsb, _), zb, location) : Ir.fun_def) code ->
        let f_var, _ = fb in
        let bs = List.map name_binder xsb in
        let _, xs_names = List.split bs in

        let xs_names', env =
          match zb with
          | None -> xs_names, Dict []
          | Some _ ->  "_env" :: xs_names, Var "_env" in

      (* this code relies on eta-expanding functions in order to take
         advantage of dynamic scoping *)

        match location with
        | `Client | `Native | `Unknown ->
           let xs_names'' = xs_names'@[__kappa] in
           LetFun ((Js.var_name_binder fb,
                    xs_names'',
                    Call (Var (snd (name_binder fb)),
                          List.map (fun x -> Var x) xs_names''),
                    location),
                   code)
        | `Server ->
           LetFun ((Js.var_name_binder fb,
                    xs_names'@[__kappa],
                    generate_remote_call f_var xs_names env,
                    location),
                   code)
    and binding : Ir.binding -> code -> code =
      function
      | `Fun def ->
         fun_def def
      | `Rec defs ->
         List.fold_right (-<-)
           (List.map (fun_def) defs)
           identity
      | _ -> identity
    and bindings : Ir.binding list -> code -> code =
      fun bindings code ->
        (List.fold_right
           (-<-)
           (List.map binding bindings)
           identity)
          code

(* FIXME: this code should really be merged with the other
   stub-generation code and we should generate a numbered version of
   every library function.
*)

(** stubs for server-only primitives *)
    let wrap_with_server_lib_stubs : code -> code = fun code ->
      let server_library_funcs =
        List.rev
          (Env.Int.fold
             (fun var _v funcs ->
               let name = Lib.primitive_name var in
               if Lib.primitive_location name = `Server then
                 (name, var)::funcs
               else
                 funcs)
             (Lib.value_env) [])
      in
      let rec some_vars = function
        | 0 -> []
        | n -> (some_vars (n-1) @ ["x"^string_of_int n])
      in
      let prim_server_calls =
        concat_map (fun (name, var) ->
          match Lib.primitive_arity name with
            None -> []
          | Some arity ->
             let args = some_vars arity in
             [(name, args, generate_remote_call var args (Dict[]))])
          server_library_funcs
      in
      List.fold_right
        (fun (name, args, body) code ->
          LetFun
            ((name,
              args @ [__kappa],
              body,
              `Server),
             code))
        prim_server_calls
        code
  end

  let rec generate_tail_computation env : Ir.tail_computation -> continuation -> code =
    fun tc kappa ->
      let gv v = generate_value env v in
      let gc c kappa = snd (generate_computation env c kappa) in
      match tc with
      | `Return v ->
         K.apply kappa (gv v)
      | `Apply (f, vs) ->
         let f = strip_poly f in
         begin
           match f with
           | `Variable f ->
              let f_name = VEnv.lookup env f in
              begin
                match vs with
                | [l; r] when Arithmetic.is f_name ->
                   K.apply kappa (Arithmetic.gen (gv l, f_name, gv r))
                | [l; r] when StringOp.is f_name ->
                   K.apply kappa (StringOp.gen (gv l, f_name, gv r))
                | [l; r] when Comparison.is f_name ->
                   K.apply kappa (Comparison.gen (gv l, f_name, gv r))
                | [v] when f_name = "negate" || f_name = "negatef" ->
                   K.apply kappa (Unop ("-", gv v))
                | _ ->
                   if Lib.is_primitive f_name
                     && not (List.mem f_name cps_prims)
                     && Lib.primitive_location f_name <> `Server
                   then
                     let arg = Call (Var ("_" ^ f_name), List.map gv vs) in
                     K.apply ~strategy:`Direct kappa arg
                   else
                     apply_yielding (gv (`Variable f)) (List.map gv vs) kappa
              end
           | _ ->
              apply_yielding (gv f) (List.map gv vs) kappa
         end
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
         K.bind kappa
           (fun kappa ->
             let gen_cont (xb, c) =
               let (x, x_name) = name_binder xb in
               x_name, (snd (generate_computation (VEnv.bind env (x, x_name)) c kappa)) in
             let cases = StringMap.map gen_cont cases in
             let default = opt_map gen_cont default in
             k (Case (x, cases, default)))
      | `If (v, c1, c2) ->
         K.bind kappa
           (fun kappa ->
             If (gv v, gc c1 kappa, gc c2 kappa))

  and generate_special env : Ir.special -> continuation -> code
    = fun sp kappa ->
      let gv v = generate_value env v in
      match sp with
      | `Wrong _ -> Die "Internal Error: Pattern matching failed" (* THIS MESSAGE SHOULD BE MORE INFORMATIVE *)
      | `Database _ | `Table _
          when Settings.get_value js_hide_database_info ->
         K.apply kappa (Dict [])
      | `Database v ->
         K.apply kappa (Dict [("_db", gv v)])
      | `Table (db, table_name, keys, (readtype, _writetype, _needtype)) ->
         K.apply kappa
           (Dict [("_table",
                   Dict [("db", gv db);
                         ("name", gv table_name);
                         ("keys", gv keys);
                         ("row",
                          strlit (Types.string_of_datatype (readtype)))])])
      | `Query _ -> Die "Attempt to run a query on the client"
      | `Update _ -> Die "Attempt to run a database update on the client"
      | `Delete _ -> Die "Attempt to run a database delete on the client"
      | `CallCC v ->
         K.bind kappa
           (fun kappa -> apply_yielding (gv v) [K.reify kappa] kappa)
      | `Select (l, c) ->
         let arg = Call (Var "_send", [Dict ["_label", strlit l; "_value", Dict []]; gv c]) in
         K.apply ~strategy:`Direct kappa arg
      | `Choice (c, bs) ->
         let result = gensym () in
         let received = gensym () in
         K.bind kappa
           (fun kappa ->
             let scrutinee = Call (Var "LINKS.project", [Var result; strlit "1"]) in
             let channel = Call (Var "LINKS.project", [Var result; strlit "2"]) in
             let generate_branch (cb, b) =
               let (c, cname) = name_binder cb in
               cname, Bind (cname, channel, snd (generate_computation (VEnv.bind env (c, cname)) b kappa)) in
             let branches = StringMap.map generate_branch bs in
             let recv_cont_name = "__recv_cont" in
             Bind (recv_cont_name,
              Call (Var "_makeCont", [Fn ([result], (Bind (received, scrutinee, (Case (received, branches, None)))))]),
              Call (Var "receive", [gv c; Var recv_cont_name])))
      | `DoOperation (name, args, _) ->
         let box vs =
           Dict (List.mapi (fun i v -> (string_of_int @@ i + 1, gv v)) vs)
         in
         let cons k ks =
           Call (Var "_lsCons", [k;ks])
         in
         let nil = Var "lsNil" in
         K.bind kappa
           (fun kappas ->
             let bind_skappa, skappa, kappas = K.pop kappas in
             let bind_seta, seta, kappas   = K.pop kappas in
             let resumption = K.(cons (reify seta) (cons (reify skappa) nil)) in
             let op    =
               Dict [ ("_label", strlit name)
                    ; ("_value", Dict [("p", box args); ("s", resumption)]) ]
             in
             bind_skappa (bind_seta (apply_yielding (K.reify seta) [op] kappas)))
      | `Handle { Ir.ih_comp = comp; Ir.ih_clauses = clauses; _ } ->
         (** Generate body *)
         let gb env binder body kappas =
           let env' = VEnv.bind env (name_binder binder) in
           snd (generate_computation env' body kappas)
         in
         let (return_clause, operation_clauses) = StringMap.pop "Return" clauses in
         let return =
           let (_, xb, body) = return_clause in
           let x_name = snd @@ name_binder xb in
           contify (fun kappa ->
             Fn ([x_name;],
                 let bind, _, kappa = K.pop kappa in
                 bind @@ gb env xb body kappa))
         in
         let operations =
           (** Generate clause *)
           let gc env (ct, xb, body) kappas =
             let x_name = snd @@ name_binder xb in
             let env', r_name =
               match ct with
               | `ResumptionBinder rb ->
                  let rb' = name_binder rb in
                  VEnv.bind env rb', snd rb'
               | _ ->
                  let dummy_binder = Var.fresh_binder (Var.make_local_info (`Not_typed, "_dummy_resume")) in
                  let dummy = name_binder dummy_binder in
                  VEnv.bind env dummy, snd dummy
             in
             let p = Call (Var "LINKS.project", [Var x_name; strlit "p"]) in
             let s = Call (Var "LINKS.project", [Var x_name; strlit "s"]) in
             let r = Call (Var "_makeFun", [s]) in
             let clause_body =
               Bind (r_name, r,
                     Bind (x_name, p, gb env' xb body kappas))
             in
             x_name, clause_body
           in
           let clauses kappas =
             StringMap.map
               (fun clause ->
                 gc env clause kappas)
               operation_clauses
           in
           let forward ks =
             let z_name = "_z" in
             K.bind ks
               (fun ks ->
                 let bind1, k', ks' = K.pop ks in
                 let bind2, h', ks' = K.pop ks' in
                 let bind code = bind1 (bind2 code) in
                 let resumption =
                   Fn (["s"], Return (Call (Var "_lsCons",
                                            [K.reify h';
                                             Call (Var "_lsCons", [K.reify k'; Var "s"])])))
                 in
                 let vmap = Call (Var "_vmapOp", [resumption; Var z_name]) in
                 bind (apply_yielding (K.reify h') [vmap] ks'))
           in
           contify
             (fun ks ->
               Fn (["_z"],
                   Case ("_z",
                         clauses ks,
                         Some ("_z", forward ks))))
         in
         let kappa = K.(return <> operations <> kappa) in
         let _, comp = generate_computation env comp kappa in
         comp

  and generate_computation env : Ir.computation -> continuation -> (venv * code) =
    fun (bs, tc) kappa ->
      let rec gbs : venv -> continuation -> Ir.binding list -> venv * code =
        fun env kappa ->
          function
          | `Let (b, (_, `Return v)) :: bs ->
             let (x, x_name) = name_binder b in
             let env', rest = gbs (VEnv.bind env (x, x_name)) kappa bs in
             (env', Bind (x_name, generate_value env v, rest))
          | `Let (b, (_, tc)) :: bs ->
             let (x, x_name) = name_binder b in
             let bind, skappa, skappas = K.pop kappa in
             let env',skappa' =
               K.contify_with_env
                 (fun kappas ->
                   let env', body = gbs (VEnv.bind env (x, x_name)) K.(skappa <> kappas) bs in
                   env', Fn ([x_name], body))
             in
             env', bind (generate_tail_computation env tc K.(skappa' <> skappas))
          | `Fun ((fb, _, _zs, _location) as def) :: bs ->
             let (f, f_name) = name_binder fb in
             let def_header = generate_function env [] def in
             let env', rest = gbs (VEnv.bind env (f, f_name)) kappa bs in
             (env', LetFun (def_header, rest))
          | `Rec defs :: bs ->
             let fs = List.map (fun (fb, _, _, _) -> name_binder fb) defs in
             let env', rest = gbs (List.fold_left VEnv.bind env fs) kappa bs in
             (env', LetRec (List.map (generate_function env fs) defs, rest))
          | `Module _ :: bs
          | `Alien _ :: bs -> gbs env kappa bs
          | [] -> (env, generate_tail_computation env tc kappa)
      in
      gbs env kappa bs

  and generate_function env fs :
      Ir.fun_def ->
    (string * string list * code * Ir.location) =
    fun (fb, (_, xsb, body), zb, location) ->
      let (f, f_name) = name_binder fb in
      assert (f_name <> "");
      (* prerr_endline ("f_name: "^f_name); *)
      (* optionally add an additional closure environment argument *)
      let xsb =
        match zb with
        | None -> xsb
        | Some zb -> zb :: xsb
      in
      let bs = List.map name_binder xsb in
      let _xs, xs_names = List.split bs in
      let body_env = List.fold_left VEnv.bind env (fs @ bs) in
      let body =
        match location with
        | `Client | `Unknown ->
           snd (generate_computation body_env body (K.reflect (Var __kappa)))
        | `Server -> generate_remote_call f xs_names (Dict [])
        | `Native -> failwith ("Not implemented native calls yet")
      in
      (f_name,
       xs_names @ [__kappa],
       body,
       location)

  and generate_program env : Ir.program -> (venv * code) = fun ((bs, _) as comp) ->
    let (venv, code) = generate_computation env comp (K.reflect (Var "_start")) in
    (venv, GenStubs.bindings bs code)

  let generate_toplevel_binding : Value.env -> Json.json_state -> venv -> Ir.binding -> Json.json_state * venv * string option * (code -> code) =
    fun valenv state varenv ->
      function
      | `Let (b, _) ->
         let (x, x_name) = name_binder b in
      (* Debug.print ("let_binding: " ^ x_name); *)
         let varenv = VEnv.bind varenv (x, x_name) in
         let value = Value.Env.find x valenv in
         let jsonized_val = Json.jsonize_value value in
         let state = ResolveJsonState.add_val_event_handlers value state in
         (state,
          varenv,
          Some x_name,
          fun code -> Bind (x_name, Lit jsonized_val, code))
      | `Fun ((fb, _, _zs, _location) as def) ->
         let (f, f_name) = name_binder fb in
         let varenv = VEnv.bind varenv (f, f_name) in
         let def_header = generate_function varenv [] def in
         (state,
          varenv,
          None,
          fun code -> LetFun (def_header, code))
      | `Rec defs ->
         let fs = List.map (fun (fb, _, _, _) -> name_binder fb) defs in
         let varenv = List.fold_left VEnv.bind varenv fs in
         (state, varenv, None, fun code -> LetRec (List.map (generate_function varenv fs) defs, code))
      | `Alien (bnd, raw_name, _lang) ->
        let (a, _a_name) = name_binder bnd in
        let varenv = VEnv.bind varenv (a, raw_name) in
        state, varenv, None, (fun code -> code)
      | `Module _ -> state, varenv, None, (fun code -> code)

  let rec generate_toplevel_bindings : Value.env -> Json.json_state -> venv -> Ir.binding list -> Json.json_state * venv * string list * (code -> code) =
    fun valenv state venv ->
      function
      | []      -> state, venv, [], identity
      | b :: bs ->
         let state, venv, x, f = generate_toplevel_binding valenv state venv b in
         let state, venv, xs, g = generate_toplevel_bindings valenv state venv bs in
         let xs =
           match x with
           | None -> xs
           | Some x -> x :: xs in
         state, venv, xs, f -<- g

  let script_tag body =
    "<script type='text/javascript'><!--\n'use strict';\n" ^ body ^ "\n--> </script>\n"

  let make_boiler_page ?(cgi_env=[]) ?(onload="") ?(body="") ?(html="") ?(head="") ?(external_files=[]) defs =
    let in_tag tag str = "<" ^ tag ^ ">\n" ^ str ^ "\n</" ^ tag ^ ">" in
    let custom_ext_script_tag str = "<script type='text/javascript' src='" ^ str ^ "'></script>" in
    let ffiLibs = String.concat "\n" (List.map custom_ext_script_tag external_files) in
    let debug_flag onoff = "\n    <script type='text/javascript'>var DEBUGGING=" ^
      string_of_bool onoff ^ ";</script>"
    in
    let extLibs = ext_script_tag "regex.js" in
    let db_config_script =
      if Settings.get_value js_hide_database_info then
        script_tag("    function _getDatabaseConfig() {
     return {}
    }
    var getDatabaseConfig = LINKS.kify(_getDatabaseConfig);\n")
      else
        script_tag("    function _getDatabaseConfig() {
      return {driver:'" ^ Settings.get_value Basicsettings.database_driver ^
                      "', args:'" ^ Settings.get_value Basicsettings.database_args ^"'}
    }
    var getDatabaseConfig = LINKS.kify(_getDatabaseConfig);\n") in
    let env =
      script_tag("  var cgiEnv = {" ^
                    mapstrcat "," (fun (name, value) -> "'" ^ name ^ "':'" ^ value ^ "'") cgi_env ^
                    "};\n  _makeCgiEnvironment();\n") in
    in_tag "html" (in_tag "head"
                     (  extLibs
                        ^ debug_flag (Settings.get_value Debug.debugging_enabled)
                        ^ ext_script_tag "jslib.js" ^ "\n"
                        ^ ffiLibs ^ "\n"
                        ^ db_config_script
                        ^ env
                        ^ head
                        ^ script_tag (String.concat "\n" defs)
                     )
                   ^ "<body onload=\'" ^ onload ^ "\'>
  <script type='text/javascript'>
  'use strict';
  _debug(\"Continuation: \" + _cont_kind);
  _startTimer();" ^ body ^ ";
  </script>" ^ html ^ "</body>")

  let initialise_envs (nenv, tyenv) =
    let dt = DesugarDatatypes.read ~aliases:tyenv.Types.tycon_env in

  (* TODO:

     - add stringifyB64 to lib.ml as a built-in function?
     - get rid of ConcatMap here?
  *)
    let tyenv =
      {Types.var_env =
          Env.String.bind
            (Env.String.bind tyenv.Types.var_env
               ("ConcatMap", dt "((a) -> [b], [a]) -> [b]"))
            ("stringifyB64", dt "(a) -> String");
       Types.tycon_env = tyenv.Types.tycon_env;
       Types.effect_row = tyenv.Types.effect_row } in
    let nenv =
      Env.String.bind
        (Env.String.bind nenv
           ("ConcatMap", Var.fresh_raw_var ()))
        ("stringifyB64", Var.fresh_raw_var ()) in

    let venv =
      Env.String.fold
        (fun name v venv -> VEnv.bind venv (v, name))
        nenv
        VEnv.empty in
    let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in
    (nenv, venv, tenv)

  let generate_program_page ?(cgi_env=[]) (nenv, tyenv) program external_files =
    let printed_code = Loader.wpcache "irtojs" (fun () ->
      let _, venv, _ = initialise_envs (nenv, tyenv) in
      let _, code = generate_program venv program in
      let code = GenStubs.wrap_with_server_lib_stubs code in
      show code)
    in
    (make_boiler_page
       ~cgi_env:cgi_env
       ~body:printed_code
       ~external_files:external_files
     (*       ~head:(String.concat "\n" (generate_inclusions defs))*)
       [])

(* generate code to resolve JSONized toplevel let-bound values *)
  let resolve_toplevel_values : string list -> string =
    fun names ->
      String.concat "" (List.map (fun name -> "    LINKS.resolveValue(state, " ^ name ^ ");\n") names)

  let generate_real_client_page ?(cgi_env=[]) (nenv, tyenv) defs (valenv, v) ws_conn_url external_files =
    let open Json in
    let req_data = Value.Env.request_data valenv in
    let client_id = RequestData.get_client_id req_data in
    let json_state = JsonState.empty client_id ws_conn_url in

    (* Add the event handlers for the final value to be sent *)
    let json_state = ResolveJsonState.add_val_event_handlers v json_state in
    (* Json.jsonize_state req_data v in *)

    (* divide HTML into head and body secitions (as we need to augment the head) *)
    let hs, bs = Value.split_html (List.map Value.unbox_xml (Value.unbox_list v)) in
    let _nenv, venv, _tenv = initialise_envs (nenv, tyenv) in

    let json_state, venv, let_names, f = generate_toplevel_bindings valenv json_state venv defs in
    let init_vars = "  function _initVars(state) {\n" ^ resolve_toplevel_values let_names ^ "  }" in

    (* Add AP information; mark APs as delivered *)
    let json_state = ResolveJsonState.add_ap_information client_id json_state in

    (* Add process information to the JSON state; mark all processes as active *)
    let json_state = ResolveJsonState.add_process_information client_id json_state in

    let state_string = JsonState.to_string json_state in

    let printed_code =
      let _venv, code = generate_computation venv ([], `Return (`Extend (StringMap.empty, None))) (K.toplevel) in
      let code = f code in
      let code =
        let open Pervasives in
        code |> (GenStubs.bindings defs) |> GenStubs.wrap_with_server_lib_stubs
      in
      show code
    in
    let welcome_msg =
      "_debug(\"Links version " ^ Basicsettings.version ^ "\");"
    in
    make_boiler_page
      ~cgi_env:cgi_env
      ~body:printed_code
      ~html:(Value.string_of_xml ~close_tags:true bs)
      ~head:(script_tag welcome_msg ^ "\n" ^ script_tag (K.primitive_bindings) ^ "\n" ^ script_tag("  var _jsonState = " ^ state_string ^ "\n" ^ init_vars)
             ^ Value.string_of_xml ~close_tags:true hs)
      ~onload:"_startRealPage()"
      ~external_files:external_files
      []
end

module Continuation =
  (val
      (match Settings.get_value Basicsettings.Js.backend with
      | "cps" when Settings.get_value Basicsettings.Handlers.enabled ->
         (module Higher_Order_Continuation : CONTINUATION)
      | "cps" ->
         (module Default_Continuation : CONTINUATION)
      (** TODO: better error handling *)
      | _ -> failwith "Unrecognised JS backend.") : CONTINUATION)

module Compiler = CPS_Compiler(Continuation)

let generate_program_page = Compiler.generate_program_page
let generate_real_client_page = Compiler.generate_real_client_page
let make_boiler_page = Compiler.make_boiler_page
