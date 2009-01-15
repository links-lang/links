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
  | Empty
deriving (Show)

module type Boxer =
sig
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
  val box_xml : code -> code
end

let wrap_with name v =
  Call (Var name, [v])

module FakeBoxer : Boxer =
struct
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
  let box_xml = identity
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
  let box_xml = wrap_with "box_xml"

  let curry_box args body =
    List.fold_right
      (fun arg c -> Call (Var "box_func", [Fun ([arg], c)])) args body

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
              (fun (n, arg::args, r) -> 
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

let ident_substs = StringMap.from_alist
  [ "+", "int_add";
    "-", "int_minus";
    "*", "int_mult";
    "==", "equals";
    "Nil", "nil";
    "Cons", "cons" ]

let subst_ident n = 
  if StringMap.mem n ident_substs then
    StringMap.find n ident_substs
  else
    n

let make_var_name v n = 
  let name = 
    if n = "" then 
      "v_"^(string_of_int v)
    else 
      "_"^n
  in 
    (Symbols.wordify -<- subst_ident) name

let get_var_name n =
  (Symbols.wordify -<- subst_ident) n

let bind_continuation k body =
  match k with 
    | Var _ -> body k
    | _ -> Let ("kappa", k, body (Var "kappa"))

module Translater (B : Boxer) =
struct
  class virtual codeIR env = 
  object (o : 'self_type)
    val env = env

    method add_bindings : binder list -> 'self_type = fun bs ->
      let env = List.fold_left 
        (fun m (v, (_, n, _)) -> IntMap.add v (make_var_name v n) m) env bs in
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

        | `Variable v -> Var (get_var_name (IntMap.find v env))

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

        | `Erase (n, v) ->
            Call (Var "erase", [o#value v; o#constant (`String n)])
              
        | `Inject (n, v, _) ->
            B.box_variant (Pair (NativeString n, o#value v))

        | `TAbs (_, v) -> o#value v

        | `TApp (v, _) -> o#value v

        | `XmlNode (name, attrs, children) ->
            B.box_xml (
              Call (Var "build_xml", 
                    [Triple (
                       NativeString name,
                       Lst (StringMap.fold
                              (fun n v a -> Pair(NativeString n, o#value v)::a) attrs []),                   
                       Lst (List.map o#value children))]))
                      
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
  end
    
  class direct env = 
  object (o : 'self_type)
    inherit codeIR env
      
    method tail_computation : tail_computation -> code = fun tc ->
      match tc with
          `Return v -> o#value v

        | `Apply (v, vl) -> 
            B.box_call (Call (o#value v, List.map o#value vl))

        | `Case (v, cases, default) ->
            let gen_case n (b, c) =
              let o = o#add_bindings [b] in
                Pair (NativeString n, Var (o#binder b)),
              o#computation c
            in              
              B.box_case (
                Case (
                  o#value v,
                  StringMap.fold (fun n c l -> (gen_case n c)::l) cases [],
                  match default with 
                      None -> None
                    | Some c ->
                        Some (gen_case "_" c)))

        | `If (v, t, f) ->
            B.box_if (
              If (o#value v, o#computation t, o#computation f))
              
        | `Special s ->
            match s with
                `CallCC v -> 
                  raise (Unsupported "CallCC not supported in direct style.")
              | `Database _
              | `Table _
              | `Query _ -> raise (Unsupported "Database operations not supported.")
              | _ -> assert false


    method computation : computation -> code = fun (bs, tc) ->
      o#bindings bs (fun o' -> o'#tail_computation tc)
        
    method binding : binding -> ('self_type -> code) -> code = fun b rest_f ->
      match b with
          `Let (x, (_, tc)) -> 
            let o' = o#add_bindings [x] in
              Let (o#binder x, o#tail_computation tc, rest_f o')
                
        | `Fun  f ->
            o#binding (`Rec [f]) rest_f
              
        | `Rec funs -> B.box_rec (
            let names = List.map fst3 funs in
            let o' = o#add_bindings (List.map fst3 funs) in
              Rec (
                List.map (
                  fun (binder, (_, f_binders, comp), _) ->
                    let o'' = o'#add_bindings f_binders in
                      (o''#binder binder, 
                       List.map o''#binder f_binders, 
                       o''#computation comp)) funs,
                rest_f o'))
            
        | `Alien _ -> assert false
            
        | `Module _ -> assert false      
  end

  class cps env =
  object (o : 'self_type)
    inherit codeIR env
    method tail_computation : tail_computation -> code -> code = fun tc k ->
      match tc with
          `Return v -> B.box_call (Call (k, [o#value v]))

        | `Apply (v, vl) -> 
            B.box_call (Call (o#value v, k::(List.map o#value vl)))

        | `Case (v, cases, default) ->
            bind_continuation k
              (fun k ->
                 let gen_case n (b, c) =
                   let o = o#add_bindings [b] in
                     Pair (NativeString n, Var (o#binder b)),
                   o#computation c k 
                 in
                   B.box_case (
                     Case (
                       o#value v,
                       StringMap.fold (fun n c l -> (gen_case n c)::l) cases [],
                       match default with 
                           None -> None
                         | Some c ->
                             Some (gen_case "_" c))))


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
              | `Query _ -> raise (Unsupported "Database operations not supported.")
              | _ -> assert false

    method computation : computation -> code -> code = fun (bs, tc) k ->
      o#bindings bs (fun o' -> o'#tail_computation tc k)
        
    method binding : binding -> ('self_type -> code) -> code = fun b rest_f ->
      match b with
          `Let (x, (_, tc)) ->
            let o' = o#add_bindings [x] in
              o#tail_computation tc (B.box_fun (Fun ([o#binder x], rest_f o')))

        | `Fun  f ->
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
                    nest 2 (
                      group (text name ^| args_doc args ^| text "=") 
                      ^| ml_of_code body)) fs) ^|
                  text "in") 
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
          let args = if args = [] then text "()" else doc_join ml_of_code args in
            parens (group (
                      nest 2 ((ml_of_code f) ^| args)))

    | Empty -> assert false

let preamble = "open Num\nopen Mllib;;\n\n"
let postamble = "\n\nlet _ = run entry"

module BoxingCamlTranslater = Translater CamlBoxer
module NonBoxingCamlTranslater = Translater FakeBoxer

let ml_of_ir cps box env comp =
  let c = 
    if cps then
      if box then 
        (new BoxingCamlTranslater.cps env)#computation comp (Var "start")
      else
        (new NonBoxingCamlTranslater.cps env)#computation comp (Var "start")
    else
      if box then 
        (new BoxingCamlTranslater.direct env)#computation comp
      else
        (new NonBoxingCamlTranslater.direct env)#computation comp
  in
    (* Hack: this needs to be fixed so top-level bindings are
       properly exposed. *)
    preamble ^
      "let entry () = begin\n" ^ (pretty 70 (ml_of_code c)) ^ "\nend" ^ 
      postamble
