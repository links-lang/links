(*pp deriving *)
(* js.ml
    JavaScript generation.
*)

open Num
open List

open Pickle
open Utility
open Syntax
open Ir

let optimising = Basicsettings.Js.optimise
let elim_dead_defs = Basicsettings.Js.elim_dead_defs
let js_lib_url = Basicsettings.Js.lib_url
let js_pretty_print = Basicsettings.Js.pp

let get_js_lib_url () = Settings.get_value js_lib_url

(* Intermediate language *)
type code = | Var    of string
            | Lit    of string
            | DeclareVar of string * code option
            | Fn     of (string list * code)

            | LetFun of ((string * string list * code * location) * code)
            | LetRec of ((string * string list * code * location) list * code)
            | Call   of (code * code list)
            | Unop   of (string * code)
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


module MaybeUseful =
struct
  let rec freevars = 
    let fv = freevars in
    let module S = StringSet in function
      | Var x               -> S.singleton x
      | Fn (args, body)     -> S.diff (fv body) (S.from_list args)
      | Call (func, args)   -> S.union (fv func) (S.union_all (map fv args))
      | Unop (_, body)      -> fv body
      | Binop (l, _, r)     -> S.union (fv l) (fv r)
      | If (a, b, c)        -> S.union (S.union (fv a) (fv b)) (fv c)
      | Dict terms          -> S.union_all (map (snd ->- fv) terms)
      | Lst terms           -> S.union_all (map fv terms)
      | Bind (var, e, body) -> S.union (fv e) (S.remove var (fv body))
      | Seq (l, r)          -> S.union (fv l) (fv r)
      | Lit _ 
      | Die _ 
      | Nothing             -> S.empty
      | _ -> assert false

  (* "Blind" (non-capture-avoiding) renaming *)
  let rec rename' renamer = function
    | Var x -> Var (renamer x)
    | Fn(args, body) -> Fn(map renamer args, rename' renamer body)
    | Call(func, args) -> Call(rename' renamer func,
                               map (rename' renamer) args)
    | Unop(op, body) -> Unop (op, rename' renamer body)
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
end

let concat_lits : RewriteCode.rewriter = 
  let stringp s = Str.string_match (Str.regexp "^[\"']") s 0 in

  let join_strings l r = 
    (Str.string_before l (String.length l - 1))  ^ (Str.string_after r 1)
      in
  function
    (* This rewrite looks very suspicious. Surely string literals are
       string literals! In which case this rewrite would appear to be
       unsound.
    *)
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
  LINKS.project(record, label)
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

(* Ugly printer for JavaScript code *)
module UP :
sig
  val show : code -> string
end =
struct
  let rec show code =
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
        | DeclareVar (x, c) -> "var "^x^(opt_app (fun c -> " = " ^ show c) "" c)

        | LetFun ((name, vars, body, location), rest) ->
            (show_func name (Fn (vars, body))) ^ show rest
        | LetRec (defs, rest) ->
            String.concat ";\n" (List.map (fun (name, vars, body, location) -> show_func name (Fn (vars, body))) defs) ^ show rest
        | Call (Var "LINKS.project", [record; label]) -> (paren record) ^ "[" ^ show label ^ "]"
        | Call (Var "hd", [list;kappa]) -> Printf.sprintf "%s(%s[0])" (paren kappa) (paren list)
        | Call (Var "tl", [list;kappa]) -> Printf.sprintf "%s(%s.slice(1))" (paren kappa) (paren list)
        | Call (fn, args) -> paren fn ^ "(" ^ arglist args  ^ ")"
        | Unop (op, body) -> op ^ paren body
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
end

(* Pretty printer for JavaScript code *)
module PP :
sig
  val show : code -> string
end =
struct
  open PP

  (** Pretty-print a Code value as a JavaScript string. *)
  let rec show c : PP.doc =
    let rec show_func name (Fn (vars, body)) = 
      PP.group (PP.text "function" ^+^ PP.text name ^^ (formal_list vars)
                ^+^  (braces
                        (break ^^ group(nest 2 (show body)) ^^ break))) in
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
      | Lst _
      | Seq _
      | Bind _
      | Die _
      | Nothing as c -> show c
      | c -> parens (show c)
    in
      match c with
        | Var x -> PP.text x
        | Nothing -> PP.text ""

        | DeclareVar (x, c) -> PP.text "var" ^+^ PP.text x
            ^+^ (opt_app (fun c -> PP.text "=" ^+^ show c) PP.empty c)

        | Die msg -> PP.text("error('" ^ msg ^ "', __kappa)")
        | Lit literal -> PP.text literal

        | LetFun ((name, vars, body, location), rest) ->
            (show_func name (Fn (vars, body))) ^^ break ^^ show rest
        | LetRec (defs, rest) ->
            PP.vsep (punctuate " " (List.map (fun (name, vars, body, location) -> show_func name (Fn (vars, body))) defs)) ^^
              break ^^ show rest

        | Fn _ as f -> show_func "" f
        | Call (Var "LINKS.project", [record; label]) -> 
            maybe_parenise record ^^ (brackets (show label))
        | Call (Var "hd", [list;kappa]) -> 
            (maybe_parenise kappa) ^^ (parens (maybe_parenise list ^^ PP.text "[0]"))
        | Call (Var "tl", [list;kappa]) -> 
            (maybe_parenise kappa) ^^ (parens (maybe_parenise list ^^ PP.text ".slice(1)"))
        | Call (fn, args) -> maybe_parenise fn ^^ 
            (PP.arglist (map show args))
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
                               (map (fun (name, value) -> 
                                       group (PP.text "'" ^^ PP.text name ^^ 
                                                PP.text "':" ^^ show value)) 
                                  elems)))
        | Lst elems -> brackets(hsep(punctuate "," (map show elems)))
        | Bind (name, value, body) ->
            PP.text "var" ^+^ PP.text name ^+^ PP.text "=" ^+^ show value ^^ PP.text ";" ^^
              break ^^ show body
        | Seq (l, r) -> vsep [(show l ^^ PP.text ";"); show r]
        | Ret e -> PP.text("return ") ^| parens(show e)

  let show = show ->- PP.pretty 144
end

let show =
  if Settings.get_value(js_pretty_print) then
    PP.show
  else
    UP.show

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

let ext_script_tag ?(base=get_js_lib_url()) file =
    "  <script type='text/javascript' src=\""^base^file^"\"></script>"

let inline_script file = (* makes debugging with firebug easier *)
  let file_in = open_in file in
  let file_len = in_channel_length file_in in
  let file_contents = String.make file_len '\000' in
    really_input file_in file_contents 0 file_len;
    "  <script type='text/javascript'>" ^file_contents^ "</script>"

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

let href_rewrite typing_env globals : RewriteSyntax.rewriter = function
  | Apply (Variable ("pickleCont",_), 
           [Abstr ([], e, _) as abs], data) as exp ->
(* WARNING (SL):

   I'm not at all convinced that the correct globals are being passed in here.
*)

      let fv = StringSet.diff (freevars e) globals in
      let `T (_,_,Some label) = expression_data e in
      let pickled_label = Result.marshal_result (`RecFunction (["f",abs], [], "f")) in
      (* correct the type of stringifyB64 *)
      let stringifyB64 = set_node_datatype (Variable ("stringifyB64", data),
                                            DesugarDatatypes.read ~aliases:typing_env.Types.tycon_env "(a) -> String") in
      (* BUG: some of the other expressions still have the wrong type *)
      let json_args = Apply (stringifyB64,
                             [Record_intro (StringSet.fold 
                                              (fun k r -> StringMap.add k (Variable (k, data)) r)
                                              fv StringMap.empty,
                                            None, data)], data) in
        Some (Concat (Constant (`String (pickled_label^ "&_jsonArgs="), data),
                      json_args, data))
  | _ -> None

let fixup_hrefs typing_env globals e = from_option e (RewriteSyntax.bottomup (href_rewrite typing_env globals) e)

let fixup_hrefs_def typing_env globals = function
  | Define (name, b,l,t) -> Define (name, fixup_hrefs typing_env globals b, l, t)
  | d -> d


let strip_lcolon evName = 
  String.sub evName 2 ((String.length evName) - 2)

(** [cps_prims]: a list of primitive functions that need to see the
    current continuation. Calls to these are translated in CPS rather than
    direct-style.  A bit hackish, this list. *)
let cps_prims = ["recv"; "sleep"; "spawnWait"]

(** {0 Code generation} *)

(* generate a JavaScript name from a binder *)
let name_binder (x, info) =
  match info with
    | (_, "", `Local) -> (x, "_" ^ string_of_int x)
    | (_, name, `Local) when (Str.string_match (Str.regexp "^_g[0-9]") name 0) ->
        (x, "_" ^ string_of_int x) (* make the generated names slightly less ridiculous in some cases *)
    | (_, name, `Local) -> (x, name ^ "_" ^ string_of_int x)
    | (_, name, `Global) -> (x, name)

module Env' :
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


let bind_continuation kappa body =
  match kappa with
    | Var _ -> body kappa
    | _ -> Bind ("_kappa", kappa, body (Var "_kappa"))


let apply_yielding (f, args) =
  Call(Var "_yield", f :: args)

let callk_yielding kappa arg =
  Call(Var "_yieldCont", [kappa; arg]) 


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
              | `Int v  -> Lit (string_of_num v)
              | `Float v    ->
                  let s = string_of_float v in
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
          begin
            match (Env'.lookup env var) with
              | "ConcatMap" -> Var ("LINKS.accum")
              | name ->
                  if name = "/" then
                    Fn (["x"; "y"; "__kappa"], callk_yielding (Var "__kappa")
                          (Call (Var "Math.floor", [Binop(Var "x", "/", Var "y")])))
                  else if Binop.is name then
                    Fn (["x"; "y"; "__kappa"], callk_yielding (Var "__kappa") (Binop(Var "x", Binop.js_name name, Var "y")))
                  else
                    Var name
          end
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
      | `Erase (name, v) ->
          Call (Var "LINKS.erase", [gv v; strlit name])
      | `Inject (name, v, _t) ->
          Dict [("_label", strlit name);
                ("_value", gv v)]
      | `TApp (v, _) ->
          gv v
(*
      | `Nil -> Lst []
      | `Cons (v, vs) ->
          Call (Var "LINKS.concat", [Lst [gv v]; gv vs])
      | `Concat (v, w) ->
          Call (Var "LINKS.concat", [gv v; gv w])
*)
      | `Comparison (v, `Equal, w) ->
          Call(Var "LINKS.eq", [gv v; gv w])
      | `Comparison (v, `NotEq, w) ->
          Unop("!", Call (Var "LINKS.eq", [gv v; gv w]))
      | `Comparison (v, op, w) ->
          Binop(gv v, comparison_name op, gv w)
      | `XmlNode (name, attributes, children) ->
          generate_xml env name attributes children

      | `ApplyPure (`Variable op, [l; r]) when Binop.is (Env'.lookup env op) ->
          Binop (gv l, Binop.js_name (Env'.lookup env op), gv r)
      | `ApplyPure (`Variable op, [v]) when Env'.lookup env op = "negate" || Env'.lookup env op = "negatef" ->
          Unop("-", gv v)
      | `ApplyPure (`Variable f, vs) when Library.is_primitive (Env'.lookup env f)
          && not (mem (Env'.lookup env f) cps_prims)
          && Library.primitive_location (Env'.lookup env f) <> `Server 
          ->
          Call (Var ("_" ^ (Env'.lookup env f)), List.map gv vs)
      | `ApplyPure (v, vs) ->
          Call (gv v, List.map gv vs)

      | `Coerce (v, _) ->
          gv v
      | `Abs v ->
          Call (Var "abs", [gv v])

and generate_xml env tag attrs children =
    Call(Var "LINKS.XML",
         [strlit tag;
          Dict (StringMap.fold (fun name v bs -> (name, generate_value env v) :: bs) attrs []);
          Lst (List.map (generate_value env) children)])

let generate_remote_call f_name xs_names =
  Call(Call (Var "LINKS.remoteCall", [Var "__kappa"]),
       [strlit f_name; Dict (
          List.map2
            (fun n v -> string_of_int n, Var v) 
            (Utility.fromTo 1 (1 + List.length xs_names))
            xs_names
        )])

let rec generate_tail_computation env : tail_computation -> code -> code = fun tc kappa ->
  let gv v = generate_value env v in
  let gc c kappa = snd (generate_computation env c kappa) in
    match tc with
      | `Return v ->           
          callk_yielding kappa (gv v)
      | `Apply (`Variable op, [l; r]) when Env'.lookup env op = "/" ->
          callk_yielding kappa (Call (Var "Math.floor", [Binop (gv l, Binop.js_name (Env'.lookup env op), gv r)]))
      | `Apply (`Variable op, [l; r]) when Binop.is (Env'.lookup env op) ->
          callk_yielding kappa (Binop (gv l, Binop.js_name (Env'.lookup env op), gv r))
      | `Apply (`Variable f, vs) when Library.is_primitive (Env'.lookup env f)
          && not (mem (Env'.lookup env f) cps_prims)
          && Library.primitive_location (Env'.lookup env f) <> `Server 
          ->
          Call (kappa, [Call (Var ("_" ^ (Env'.lookup env f)), List.map gv vs)])
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
                     x_name, (snd (generate_computation (Env'.extend env [(x, x_name)]) c kappa)) in
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
      | `Wrong _ -> Die "Internal Error: Pattern matching failed"
      | `Database v ->
          callk_yielding kappa (Dict [("_db", gv v)])
      | `Query _ ->
          failwith ("Cannot (yet?) generate JavaScript code for `Query")
      | `Table (db, table_name, (readtype, writetype)) ->
          callk_yielding kappa
            (Dict [("_table",
                    Dict [("db", gv db);
                          ("name", gv table_name);
                          ("row",
                           strlit (Types.string_of_datatype (readtype)))])])
      | `CallCC v ->
          bind_continuation kappa
            (fun kappa -> apply_yielding (gv v, [Lst [kappa]; kappa]))

and generate_computation env : computation -> code -> (Env'.env * code) = fun (bs, tc) kappa -> 
  let rec gbs env c =
    function
      | [] ->
          env, c (generate_tail_computation env tc kappa)
      | b :: bs -> 
          let env, c' = generate_binding env b in
            gbs env (c -<- c') bs
  in
    gbs env (fun code -> code) bs

and generate_binding env : binding -> (Env'.env * (code -> code)) = fun binding ->
  match binding with
    | `Let (b, (_, `Return v)) ->
        let (x, x_name) = name_binder b in
        let env' = Env'.extend env [(x, x_name)] in
          (env',
           fun code ->
             Seq (DeclareVar (x_name, Some (generate_value env v)), code))
    | `Let (b, (_, tc)) ->
        let (x, x_name) = name_binder b in
        let env' = Env'.extend env [(x, x_name)] in
          env', (fun code -> generate_tail_computation env tc (Fn ([x_name], code)))
    | `Fun (fb, (_, xsb, body), location) ->
        let (f, f_name) = name_binder fb in
        let bs = List.map name_binder xsb in
        let xs, xs_names = List.split bs in
        let body_env = Env'.extend env bs in
        let env' = Env'.extend env [(f, f_name)] in
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
        let fs = List.map (fun (fb, _, _) -> name_binder fb) defs in
        let env' = Env'.extend env fs in
          (env',
           fun code ->
             LetRec
               (List.fold_right
                  (fun (fb, (_, xsb, body), location) (defs, code) ->
                     let (f, f_name) = name_binder fb in
                     let bs = List.map name_binder xsb in
                     let _, xs_names = List.split bs in
                     let body_env = Env'.extend env (fs @ bs) in
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
    | `Module _
    | `Alien _
    | `Alias _ -> env, (fun code -> code)

and generate_declaration env
    : binding -> (Env'.env * (code -> code)) = fun binding ->
  match binding with
    | `Let (b, (_, `Return v)) ->
        let (x, x_name) = name_binder b in
        let env' = Env'.extend env [(x, x_name)] in
          (env',
           fun code ->
             Seq (DeclareVar (x_name, Some (generate_value env v)), code))
    | `Let (b, (_, tc)) ->
        if Settings.get_value (Basicsettings.allow_impure_defs) then
          let (x, x_name) = name_binder b in
          let env' = Env'.extend env [(x, x_name)] in
            (env',
             fun code ->
               Seq (DeclareVar (x_name, None), code))
        else
          failwith "Top-level definitions must be values"
    | `Fun (fb, (_, xsb, body), location) ->
        let (f, f_name) = name_binder fb in
        let bs = List.map name_binder xsb in
        let xs, xs_names = List.split bs in
        let body_env = Env'.extend env bs in
        let env' = Env'.extend env [(f, f_name)] in
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
        let fs = List.map (fun (fb, _, _) -> name_binder fb) defs in
        let env' = Env'.extend env fs in
          (env',
           fun code ->
             LetRec
               (List.fold_right
                  (fun (fb, (_, xsb, body), location) (defs, code) ->
                     let (f, f_name) = name_binder fb in
                     let bs = List.map name_binder xsb in
                     let _, xs_names = List.split bs in
                     let body_env = Env'.extend env (fs @ bs) in
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
    | `Module _
    | `Alien _
    | `Alias _ -> env, (fun code -> code)


and generate_definition env
    : binding -> code -> code =
  function
    | `Let (_, (_, `Return _)) -> (fun code -> code)
    | `Let (b, (_, tc)) ->
        let (x, x_name) = name_binder b in
          (fun code ->
             generate_tail_computation env tc
               (Fn ([x_name ^ "$"], Bind(x_name, Var (x_name ^ "$"), code))))
    | `Fun _
    | `Rec _
    | `Module _
    | `Alien _
    | `Alias _ -> (fun code -> code)

and generate_defs env : binding list -> (Env'.env * (code -> code)) =
  fun bs ->
    let rec declare env c =
      function
        | [] -> env, c
        | b :: bs ->
            let env, c' = generate_declaration env b in
              declare env (c -<- c') bs in
    let env, with_declarations = declare env (fun code -> code) bs in
    let rec define c =
      function
        | [] -> c
        | b :: bs ->
            let c' = generate_definition env b in
              define (c -<- c') bs
    in
      if Settings.get_value Basicsettings.allow_impure_defs then
        env, fun code -> with_declarations (define (fun code -> code) bs code)
      else
        env, with_declarations

and generate_program env : computation -> (Env'.env * code) = fun c ->
  generate_computation env c (Var "_start")

module Symbols :
sig
  val rename : Syntax.program -> Syntax.program
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

  let js_keywords = ["break";"else";"new";"var";"case";"finally";"return";"void";
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
      
  let rename_expression exp = 
    from_option exp
      (RewriteSyntax.bottomup
         (function
            | Variable (name, d) -> Some (Variable (wordify name, d))
            | Let (name, rhs, body, d) -> 
                Some (Let (wordify name, rhs, body, d))
            | Rec (bindings, body, d) -> 
                let rename_expression (nm, body, t) = (wordify nm, body, t) in
                  Some (Rec (List.map rename_expression bindings, body, d))
            | _ -> None)
         exp)

  let rec rename_def def =
    match def with
      | Define (name, b, l, t) -> 
          Define (wordify name, rename_expression b, l, t)
      | Alien (name, _, _, _) when has_symbols name ->
          assert false (* symbols shouldn't appear in types or foreign functions *)
      | Module (name, defs, data) ->
          Module (name, opt_map (fun defs -> List.map rename_def defs) defs, data)
      | _ -> def

  let rename_program (Program(defs, expr)) = 
    Program(List.map rename_def defs, rename_expression expr)

  let rename = rename_program
end

let script_tag body = 
  "<script type='text/javascript'><!--\n" ^ body ^ "\n--> </script>\n"

let make_boiler_page ?(onload="") ?(body="") ?(head="") defs =
  let in_tag tag str = "<" ^ tag ^ ">\n" ^ str ^ "\n</" ^ tag ^ ">" in
  let debug_flag onoff = "\n    <script type='text/javascript'>var DEBUGGING=" ^ 
    string_of_bool onoff ^ ";</script>"
  in
  let extLibs = ext_script_tag "json.js"^"
  "            ^ext_script_tag "regex.js"^"
  "            ^ext_script_tag "yahoo/yahoo.js"^"
  "            ^ext_script_tag "yahoo/event.js" in
  let db_config_script = script_tag("    function _getDatabaseConfig() {
      return {driver:'" ^ Settings.get_value Library.database_driver ^
    "', args:'" ^ Settings.get_value Library.database_args ^"'}
    }
    var getDatabaseConfig = LINKS.kify(_getDatabaseConfig, 0);\n")
  in
  let version_comment = "<!-- $Id: js.ml 1367 2007-12-10 16:24:38Z sam $ -->" in
    in_tag "html" (in_tag "head"
                     (  extLibs
                      ^ debug_flag (Settings.get_value Debug.debugging_enabled)
                      ^ ext_script_tag "jslib.js" ^ "\n"
                      ^ db_config_script
                      ^ head
                      ^ script_tag (String.concat "\n" defs)
                      ^ version_comment
                     )
                   ^ "<body onload=\'" ^ onload ^ "\'>
  <script type='text/javascript'>
  _startTimer();" ^ body ^ ";
  </script>")
    
let wrap_with_server_stubs code = 
  let server_library_funcs = (filter (fun (name,_) -> 
                                        Library.primitive_location name =`Server)
                                (StringMap.to_alist !Library.value_env)) in

  let rec some_vars = function 
      0 -> []      
    | n -> (some_vars (n-1) @ ["x"^string_of_int n]) in
    
  let prim_server_calls =
    concat_map (fun (name, _) -> 
                  match Library.primitive_arity name with
                        None -> []
                    | Some arity ->
                        let args = some_vars arity in
                          [(name, args, generate_remote_call name args)])
      server_library_funcs
  in
    fold_right 
      (fun (name, args, body) code ->
         LetFun
           ((name,
             args @ ["__kappa"],
             body,
             `Server),
            code))
      prim_server_calls
      code


let preprocess_program typing_env global_names program =
  let (Program (defs, body)) =
    if Settings.get_value optimising then
      Optimiser.inline (Optimiser.inline (Optimiser.inline program)) 
    else program in

  (* 
     QUESTION:
     
     How are we supposed to know that the collection of names passed
     to fixup_hrefs_def is correct?
  *)
  let defs = List.map (fixup_hrefs_def typing_env
                         (StringSet.from_list
                            (Syntax.defined_names defs @ 
                               Library.primitive_names @ global_names))) defs in

  (*  
      The following transformation may or may not be helpful. We can defer
      thinking about it until after we get rid of the expression datatype.
  *)
  let program = rewrite_program
    (Syntax.RewriteSyntax.all [Sqlcompile.Prepare.NormalizeProjections.normalize_projections])
    (Program (defs, body))
  in
    Symbols.rename program

let make_initial_env typing_env =
  let dt = DesugarDatatypes.read ~aliases:typing_env.Types.tycon_env in
    Compileir.make_initial_env
      {Types.var_env = 
          (Env.String.bind
             (Env.String.bind typing_env.Types.var_env
                ("ConcatMap", dt "((a) -> [b], [a]) -> [b]"))
             ("stringifyB64", dt "(a) -> String"));
      Types.tycon_env = Env.String.empty}

let compile_ir ?(elim=false) typing_env global_names program =
  let program = preprocess_program typing_env global_names program in

  let env, tenv = make_initial_env typing_env in

  let ((bindings, _) as e, _) = Compileir.compile_program (env, tenv) program in 
  let ((bindings, _) as e) =
    if elim then
      Ir.ElimDeadDefs.program tenv e
    else
      e in

  let env, _ = Compileir.add_globals_to_env (env, tenv) bindings in
  let env' = Compileir.invert_env env in
    e, env'

let generate_program_page ?(onload = "") tyenv global_names (Program (defs, _) as program) = 
  let e, env = compile_ir ~elim:true tyenv global_names program in
  let _, code = generate_program env e in
  let code = optimise(wrap_with_server_stubs code) in
    (make_boiler_page
       ~body:(show code)
(*       ~head:(String.concat "\n" (generate_inclusions defs))*)
       [])

let generate_program_defs typing_env global_names defs root_names =
  let (bindings, _), env = compile_ir typing_env global_names (Program (defs, Syntax.unit_expression Syntax.no_expr_data)) in
  let _, code = generate_defs env bindings in
    [show (code Nothing)]

let generate_program_defs global_names defs root_names =
  generate_program_defs Library.typing_env global_names defs root_names
