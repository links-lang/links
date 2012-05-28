(*pp deriving *)
open Num
open Utility
open Ir
open PP

exception Unsupported of string;;

type code =
  | Bool of bool
  | Int of num
  | Char of char
  | NativeString of string
  | Float of float
  | Var of string
  | Rec of (string * string list * code) list * code
  | Fun of string list * code 
  | Let of string * code * code
  | If of (code * code * code)
  | Call of (code * code list)
  | Pair of code * code
  | Triple of code * code * code
  | Lst of code list
  | Case of code * ((code * code) list) * ((code * code) option)
  | Die of code
  | Empty

module type Boxer =
sig
  val wrap_func : bool -> string * string list * string * bool -> code -> code

  val box_bool : code -> code
  val box_record : code -> code
  val box_int : code -> code
  val box_char : code -> code
  val box_float : code -> code
  val box_variant : code -> code
  val box_string : code -> code
  val box_rec : code -> code
  val box_fun : code -> code
  val box_call : code -> code
  val box_if : code -> code
  val box_case : code -> code
  val box_xmlitem : code -> code
  val box_list : code -> code
end

let arg_names = mapIndex (fun _ i -> "arg_" ^ (string_of_int i))

let wrap_with name v =
  match name with
    | "id" -> v
    | _ -> Call (Var name, [v])

module FakeBoxer : Boxer =
struct
  let wrap_func cps (name, unboxers, _, needs_k) rest =
    let unboxed_name = "u_" ^ name in
    let f = 
      if cps && not needs_k then
        let args = arg_names unboxers in
        let v_args = List.map (fun a -> Var a) args in
          Fun("k"::args, Call (Var "k", [Call (Var unboxed_name,  v_args)]))
      else
        Var unboxed_name
    in
      Let (name, f, rest)

  let box_bool = identity
  let box_int = identity
  let box_char = identity
  let box_string = identity
  let box_float = identity
  let box_variant = identity
  let box_record = identity
  let box_rec = identity
  let box_fun = identity
  let box_call = identity
  let box_if = identity
  let box_case = identity
  let box_xmlitem = identity
  let box_list = identity
end

module CamlBoxer : Boxer =
struct
  let box_bool = wrap_with "box_bool"
  let box_int = wrap_with "box_int"
  let box_char = wrap_with "box_char"
  let box_string = wrap_with "box_string"
  let box_float = wrap_with "box_float"
  let box_variant = wrap_with "box_variant"
  let box_record = wrap_with "box_record"
  let box_xmlitem = wrap_with "box_xmlitem"
  let box_list = wrap_with "box_list"

  let curry_box args body =
    List.fold_right
      (fun arg c -> Call (Var "box_func", [Fun ([arg], c)])) args body

  let wrap_func cps (name, unboxers, boxer, needs_k) rest = 
    let args = arg_names unboxers in
    let v_args = List.map (fun a -> Var a) args in
      
    let body_create v_args unboxers =
      wrap_with boxer (
        Call (
          Var ("u_" ^ name),
          List.map2 (fun ub arg -> wrap_with ub arg) unboxers v_args))
    in
      match (needs_k, cps) with
        | (false, false) ->
            let body = body_create v_args unboxers in
              Let (name, curry_box args body, rest)
        | (true, false) ->
            let body = Die (NativeString (name ^ " cannot be used in direct style."))
            in Let (name, curry_box args body, rest)
        | (true, _) ->
            let body = body_create ((Var "k")::v_args) ("unbox_func"::unboxers) in
              Let (name, curry_box ("k"::args) body, rest)
        | (_, true) ->
            let body = Call (
              wrap_with "unbox_func" (Var "k"), [body_create v_args unboxers]) in
              Let (name, curry_box ("k"::args) body, rest)

  let box_fun = function
    | Fun (a, b) -> curry_box a b
    | _ -> assert false

  let box_rec = function
    | Rec (funcs, rest) ->
        let rec box_funcs fs rest =
          match fs with
              [] -> rest
            | ((n, a, r)::fs) ->
                Let (n, Call (Var "box_func", [Var n]), box_funcs fs rest)
        in
          Rec (
            List.map 
              (fun (n, args, r) -> 
                 match args with 
                   | [] -> (n, [], (box_funcs funcs r))
                   | arg::args ->
                       (n, [arg], curry_box args (box_funcs funcs r))) funcs,
            box_funcs funcs rest)
    | _ -> assert false

  let box_call = function
    | Call (f, args) ->
        List.fold_left 
          (fun c arg -> Call(Call (Var "unbox_func", [c]), [arg])) f args
    | _ -> assert false

  let box_if = function 
    | If (b, t, f) ->
        If (Call (Var "unbox_bool", [b]), t, f)
    | _ -> assert false

  let box_case = function 
    | Case (v, c, d) ->
        Case (Call (Var "unbox_variant", [v]), c, d)
    | _ -> assert false
end

(* This is mostly stolen from irtojs.ml *)
module Symbols =
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
        '|', "pipe"; ]

  let has_symbols name =
    List.exists (not -<- Utility.Char.isWord) (explode name)

  let wordify name = 
    if has_symbols name then 
      ("s_" ^
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
    else
      name
end

let lib_funcs = [
  "l_int_add", ["unbox_int"; "unbox_int"], "box_int", false;
  "l_int_minus", ["unbox_int"; "unbox_int"], "box_int", false;
  "l_int_mult", ["unbox_int"; "unbox_int"], "box_int", false;
  "_mod", ["unbox_int"; "unbox_int"], "box_int", false;
  "_negate", ["unbox_int"], "box_int", false;

  "l_int_gt", ["unbox_int"; "unbox_int"], "box_bool", false;
  "l_int_gte", ["unbox_int"; "unbox_int"], "box_bool", false;
  "l_int_lt", ["unbox_int"; "unbox_int"], "box_bool", false;
  "l_int_lte", ["unbox_int"; "unbox_int"], "box_bool", false;

  "_tilde", ["unbox_string"; "id"], "box_bool", false;

  "l_equals", ["id"; "id"], "box_bool", false;
  "l_not_equals", ["id"; "id"], "box_bool", false;

  "l_cons", ["id"; "unbox_list"], "box_list", false;
  "l_concat", ["unbox_list"; "unbox_list"], "box_list", false;
  "_hd", ["unbox_list"], "id", false;
  "_tl", ["unbox_list"], "box_list", false;
  "_drop", ["unbox_int"; "unbox_list"], "box_list", false;
  "_take", ["unbox_int"; "unbox_list"], "box_list", false;

  "_not", ["unbox_bool"], "box_bool", false;

  "_addAttributes", ["unbox_list"; "unbox_list"], "box_list", false;

  "_intToXml", ["unbox_int"], "box_list", false;
  "_stringToXml", ["unbox_string"], "box_list", false;
  "_intToString", ["unbox_int"], "box_string", false;
  "_stringToInt", ["unbox_string"], "box_int", false;
  "_stringToFloat", ["unbox_string"], "box_float", false;

  "_environment", [], "box_list", false;
  "_redirect", ["unbox_string"], "id", true;
  "_exit", ["id"], "id", true;
  "_error", ["unbox_string"], "id", false;
  "_unsafePickleCont", ["id"], "box_string", false;
  "_reifyK", ["id"], "box_string", false;
]

(* TODO: Most of this can be handled generically *)
let ident_substs = StringMap.from_alist
  [ "+", "l_int_add";
    "-", "l_int_minus";
    "*", "l_int_mult";
    ">", "l_int_gt";
    "<", "l_int_lt";
    ">=", "l_int_gte";
    "<=", "l_int_lte";
    "!=", "l_not_equals";
    "==", "l_equals";
    "Nil", "l_nil";
    "Cons", "l_cons";
    "Concat", "l_concat";
  ]

let subst_ident n = 
  if StringMap.mem n ident_substs then
    StringMap.find n ident_substs
  else
    n

let subst_primitive n =
  if Lib.is_primitive n then
    "_"^n
  else
    n

let make_var_name v n = 
  let name = 
    if n = "" then 
      "v_"^(string_of_int v)
    else 
      "_"^n
  in 
    (Symbols.wordify -<- subst_primitive -<- subst_ident) name

let get_var_name n =
  (Symbols.wordify -<- subst_primitive -<- subst_ident) n

let bind_continuation k body =
  match k with 
    | Var _ -> body k
    | _ -> Let ("kappa", k, body (Var "kappa"))

module Translater (B : Boxer) =
struct
  class virtual codeIR env = 
  object (o : 'self_type)
    val env = env

    method wrap_func = B.wrap_func false

    method wrap_lib rest =
      List.fold_left
        (fun r x -> o#wrap_func x r) rest lib_funcs

    method add_bindings : binder list -> 'self_type = fun bs ->
      let env = List.fold_left 
        (fun e (v, (_, n, _)) -> Env.Int.bind e (v, (make_var_name v n))) env bs in
        {< env=env >}
          
    method constant : constant -> code = fun c ->
      match c with
        | `Bool x -> B.box_bool (Bool x)
        | `Int x -> B.box_int (Int x)
        | `Char x -> B.box_char (Char x)
        | `String x -> B.box_string (NativeString x)
        | `Float x -> B.box_float (Float x)
      
    method value : value -> code = fun v ->
      match v with 
        | `Constant c -> o#constant c

        | `Variable v | `SplicedVariable v -> Var (get_var_name (Env.Int.lookup env v))

        | `Extend (r, v) ->
            let record = B.box_record (
              StringMap.fold 
                (fun n v m ->
                   Call (Var "StringMap.add",
                         [NativeString n; o#value v; m]))
                r (Var "StringMap.empty"))
            in
              begin 
                match v with
                    None -> record
                  | Some v -> 
                      Call (Var "union", [o#value v; record])
              end
                
        | `Project (n, v) ->
            Call (Var "project", [o#value v; o#constant (`String n)])

        | `Erase (ns, v) ->
            Call (Var "erase", [o#value v; Lst (List.map (fun n -> o#constant (`String n)) (StringSet.elements ns))])
              
        | `Inject (n, v, _) ->
            B.box_variant (Pair (NativeString n, o#value v))

        | `TAbs (_, v) -> o#value v

        | `TApp (v, _) -> o#value v

        | `XmlNode (name, attrs, children) ->
            B.box_list(
              Lst [
                B.box_xmlitem (          
                  Call (Var "build_xmlitem", [
                          NativeString name;
                          Lst (
                            StringMap.fold
                              (fun n v a -> Pair(NativeString n, o#value v)::a) attrs []);
                          Lst (List.map o#value children)]))])
                        
        | `ApplyPure (v, vl) -> 
            B.box_call (Call (o#value v, List.map o#value vl))

        | `Coerce (v, _) -> o#value v

    method bindings : binding list -> ('self_type -> code) -> code = fun bs f ->
      match bs with
          [] -> f o
        | (b::bs) -> o#binding b (fun o' -> o'#bindings bs f)

    method binder : binder -> string = fun (v, (_, name, _)) ->
      make_var_name v name

    method virtual binding : binding -> ('self_type -> code) -> code

    method virtual program : program -> code

  end
    
  class direct env = 
  object (o : 'self_type)
    inherit codeIR env
      
    method tail_computation : tail_computation -> code = fun tc ->
      match tc with
          `Return v -> o#value v

        | `Apply (v, vl) | `ApplyPL (v, vl)  | `ApplyDB (v, vl) -> 
            B.box_call (Call (o#value v, List.map o#value vl))

        | `Case (v, cases, default) ->
            let gen_case n (b, c) =
              let o = o#add_bindings [b] in
                Pair (n, Var (o#binder b)),
              o#computation c
            in              
              B.box_case (
                Case (
                  o#value v,
                  StringMap.fold (fun n c l -> (gen_case (NativeString n) c)::l) cases [],
                  match default with 
                      None -> None
                    | Some c ->
                        Some (gen_case (Var "_") c)))

        | `If (v, t, f) ->
            B.box_if (
              If (o#value v, o#computation t, o#computation f))
              
        | `Special s ->
            match s with
                `CallCC v -> 
                  Die (NativeString "CallCC not supported in direct style.")

              | `Database _
              | `Table _
              | `Query _ 
				  | `Delete _ 
				  | `Update _ -> Die (NativeString "Database operations not supported.")
              | `Wrong _ -> Die (NativeString "Internal Error: Pattern matching failed")

    method program : program -> code = fun prog ->
      o#wrap_lib (o#computation prog)

    method computation : computation -> code = fun (bs, tc) ->
      o#bindings bs (fun o' -> o'#tail_computation tc)
        
    method binding : binding -> ('self_type -> code) -> code = fun b rest_f ->
      match b with
          `Let (x, (_, tc)) -> 
            let o' = o#add_bindings [x] in
              Let (o#binder x, o#tail_computation tc, rest_f o')
                
        | `Fun  f | `FunQ f ->
            o#binding (`Rec [f]) rest_f
              
        | `Rec funs -> B.box_rec (
            let names = List.map fst3 funs in
            let o' = o#add_bindings (List.map fst3 funs) in
              Rec (
                List.map (
                  fun (binder, (_, f_binders, comp), _) ->
                    let args = List.map o'#binder f_binders in
                    let o'' = o'#add_bindings f_binders in
                      (o''#binder binder, 
                       args, 
                       o''#computation comp)) funs,
                rest_f o'))
            
        | `Alien _ -> assert false
            
        | `Module _ -> assert false      
  end

  class cps env =
  object (o : 'self_type)
    inherit codeIR env as super

    method wrap_func = B.wrap_func true

    (* `ApplyPure is a pain. *)
    method value : value -> code = function
      | `ApplyPure (v, vl) ->
          B.box_call (Call (o#value v, (Var "id")::(List.map o#value vl)))
      | v -> super#value v

    method tail_computation : tail_computation -> code -> code = fun tc k ->
      match tc with
          `Return v -> B.box_call (Call (k, [o#value v]))

        | `Apply (v, vl) | `ApplyDB (v, vl) | `ApplyPL (v, vl)-> 
            B.box_call (Call (o#value v, k::(List.map o#value vl)))

        | `Case (v, cases, default) ->
            bind_continuation k
              (fun k ->
                 let gen_case n (b, c) =
                   let o = o#add_bindings [b] in
                     Pair (n, Var (o#binder b)),
                   o#computation c k 
                 in
                   B.box_case (
                     Case (
                       o#value v,
                       StringMap.fold (fun n c l -> (gen_case (NativeString n) c)::l) cases [],
                       match default with 
                           None -> None
                         | Some c ->
                             Some (gen_case (Var "_") c))))


        | `If (v, t, f) ->
            bind_continuation k
              (fun k -> B.box_if (If (o#value v, o#computation t k, o#computation f k)))
              
        | `Special s ->
            match s with
               `CallCC v ->
                bind_continuation k
                  (fun k ->
                     (* This wrapper dumps the unnecessary continuation argument
                      * that the continuation will be passed when called *)
                     Let ("call_k", 
                          B.box_fun (Fun (["_"; "arg"], B.box_call (Call (k, [Var "arg"])))),
                          B.box_call (Call (o#value v, [k; Var "call_k"]))))
              | `Database _
              | `Table _
              | `Query _ 
				  | `Delete _ 
				  | `Update _ -> Die (NativeString "Database operations not supported.")
              | `Wrong _ -> Die (NativeString "Internal Error: Pattern matching failed")

    method computation : computation -> code -> code = fun (bs, tc) k ->
      o#bindings bs (fun o' -> o'#tail_computation tc k)

    method program : program -> code = fun prog -> 
      o#wrap_lib (o#computation prog (Var "start"))

    method binding : binding -> ('self_type -> code) -> code = fun b rest_f ->
      match b with
          `Let (x, (_, tc)) ->
            let o' = o#add_bindings [x] in
              o#tail_computation tc (B.box_fun (Fun ([o#binder x], rest_f o')))

        | `Fun  f | `FunQ f ->
            o#binding (`Rec [f]) rest_f
              
        | `Rec funs ->
            B.box_rec (
              let names = List.map fst3 funs in
              let o' = o#add_bindings (List.map fst3 funs) in
                Rec (
                  List.map (
                    fun (binder, (_, f_binders, comp), _) ->
                      let o'' = o'#add_bindings f_binders in
                        (o''#binder binder, 
                         "kappa"::(List.map o''#binder f_binders), 
                         o''#computation comp (Var "kappa"))) funs,
                  rest_f o'))
              
        | `Alien _ -> Empty
            
        | `Module _ -> Empty            
  end
end

(* 
   Found the bottleneck :P 
   TODO: Find a tractable indentation scheme for CPS!
*)
let nest : int -> doc -> doc = fun i x -> x

let args_doc args =
  if args = [] then
    text "()"
  else
    doc_join text args

let rec ml_of_code c = 
  match c with
    | Bool x -> text (string_of_bool x)
    (* Represent integer literals as strings so we don't hit range problems. *)
    | Int x -> parens (text "num_of_string" ^| ml_of_code (NativeString (Num.string_of_num x)))
    | Char x -> text ("'" ^ Char.escaped x ^ "'")
    | NativeString x -> text ("\"" ^ String.escaped x ^ "\"")
    | Float x -> text (string_of_float x)
          
    | Var name -> text name

    | Rec (fs, rest) ->
        group (
          group (
            text "let rec" ^|
                doc_concat (break^^text "and"^^break)
              (List.map 
                 (fun (name, args, body) ->
                    let args = if args = [] then text "_" else args_doc args in
                      nest 2 (
                        group (text name ^| args ^| text "=") 
                        ^| ml_of_code body)) fs)) ^|
              if rest = Empty then
                text ";;"
              else
                text "in"
                ^| ml_of_code rest)

    | Fun (args, body) ->
        parens (
          group (          
            nest 2 (
              group (text "fun" ^| args_doc args ^| text "->") 
              ^|  ml_of_code body)))
              
    | Let (name, body, rest) ->
        group (
          group (
            text "let" ^|
                nest 2 (
                  group (text name ^| text "=") 
                  ^| ml_of_code body)) ^|
            text "in"
          ^| ml_of_code rest)

    | If (b, t, f) ->
        group (
          nest 2 (text "if" ^| ml_of_code b) ^|
              nest 2 (text "then" ^| ml_of_code t) ^|
                  nest 2 (text "else" ^| ml_of_code f))

    | Case (v, cases, default) ->
        let pp_case (b, c) =
          group (text "|" ^| ml_of_code b ^| text "->" ^| ml_of_code c)
        in        
        group (
          text "begin" ^|
              nest 2 (
                group (text "match" ^| ml_of_code v ^| text "with") ^|
                  doc_join pp_case cases ^|
                      begin 
                        match default with
                          | None -> empty
                          | Some c -> pp_case c
                      end ^|
                          text "| _ -> assert false") ^|
                  text "end")

    | Pair (v1, v2) ->
        group (
          parens (ml_of_code v1 ^^ text "," ^| ml_of_code v2))

    | Triple (v1, v2, v3) ->
        group (
          nest 2 (
            parens (
              group (ml_of_code v1 ^^ text "," ^| ml_of_code v2 ^^ text ",") ^| 
                  ml_of_code v3)))

    | Lst vs ->
        group (
          text "[" ^^ 
            doc_concat (text "; ") 
            (List.map (group -<- ml_of_code) vs) ^^ 
            text "]")

    | Call (f, args) -> 
          let args = if args = [] then text "l_unit" else doc_join ml_of_code args in
            parens (group (
                      nest 2 ((ml_of_code f) ^| args)))

    | Die s ->
        group (text "raise (InternalError" ^| ml_of_code s ^| text ")")

    | Empty -> empty

let postamble = "\n\nlet _ = run entry"

module BoxingCamlTranslater = Translater (CamlBoxer)
module NonBoxingCamlTranslater = Translater (FakeBoxer)

let ml_of_ir cps box no_prelude env prelude (bs, tc) =
  let env = Env.invert_env env in

  let preamble = "open Num\n" ^
    if box then "open Mllib;;\n\n" else "open Unboxed_mllib;;\n\n"
  in

  let comp = 
    if box && not no_prelude then 
      prelude @ bs, tc
    else 
      bs, tc in

  let c =
    if cps then 
      let t =
        if box then
          new BoxingCamlTranslater.cps env
        else
          new BoxingCamlTranslater.cps env
      in 
        t#program comp
    else
      let t = 
        if box then 
          new BoxingCamlTranslater.direct env
        else
          new BoxingCamlTranslater.direct env
      in 
        t#program comp
  in
    (* Hack: this needs to be fixed so top-level bindings are
       properly exposed. *)
    preamble ^
      "let entry () = begin\n" ^ (pretty 110 (ml_of_code c)) ^ "\nend" ^ 
      postamble
