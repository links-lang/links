1
(** JavaScript generation *)
open Utility
open CommonTypes

let js_hide_database_info = Js.hide_database_info
let session_exceptions_enabled = Settings.get Basicsettings.Sessions.exceptions_enabled

let internal_error message = Errors.internal_error ~filename:"irtojs.ml" ~message

(* strip any top level polymorphism from an expression *)
let rec strip_poly =
  function
    | Ir.TAbs (_, e)
    | Ir.TApp (e, _) -> strip_poly e
    | e -> e

(** Intermediate language *)
module Code = struct

  module Var = struct
    type t = string
    [@@deriving show]
  end

  module Label = struct
    type t = string
    [@@deriving show]
  end

  type t = Var     of Var.t
         | Lit     of string
         | Fn      of Var.t list * t

         | LetFun  of (Var.t * Var.t list * t * Ir.location) * t
         | LetRec  of (Var.t * Var.t list * t * Ir.location) list * t
         | Call    of t * t list
         | Unop    of Var.t * t
         | Binop   of t * Var.t * t
         | If      of t * t * t
         | Switch  of t * t stringmap * t option
         | Dict    of (Label.t * t) list
         | Arr     of t list
         | Project of t * Label.t

         | Bind    of Var.t * t * t
         | Return  of t

         | InlineJS of string

         | Nothing
         [@@deriving show]

  module MetaContinuation = struct
    type nonrec t = (t -> t)
    let identity code = code
  end

  module ObjectContinuation = struct
    let __kappa = "__kappa"
  end

  module Constructors = struct
    (** Create a JS string literal, quoting special characters *)
    let string_js_quote s =
      String.escaped s
      |> Str.global_replace (Str.regexp "'") "\\'"
      |> Printf.sprintf "'%s'"

    (** Return a JS literal string from an OCaml int. *)
    let intlit i = Lit (string_of_int i)
    (** Return a JS literal string from an OCaml string. *)
    let strlit s = Lit (string_js_quote s)
    (** Return a JS literal string from an OCaml character. *)
    let chrlit ch = Dict ["_c", Lit(string_js_quote(string_of_char ch))]
    (** Return a literal for the JS representation of a Links string. *)
    let chrlistlit = strlit
  end

  module Runtime = struct

    module Links = struct
      let erase   = Var "_$Links.erase"
      let union   = Var "_$Links.union"
      let remoteCall = Var "_$Links.remoteCall"
      let xml = Var "_$Links.XML"
      let eq = Var "_$Links.eq"
      let neq = Var "_$Links.neq"
      let gt = Var "_$Links.gt"
      let lt = Var "_$Links.lt"
      let gte = Var "_$Links.gte"
      let lte = Var "_$Links.lte"
    end

    module Proc = struct
      let yield = Var "_$Proc.yield"
    end

    module Constants = struct
      let unit = Var "_$Constants.UNIT"
    end

    module List = struct
      let nil = Var "_$List.nil"
      let cons = Var "_$List.cons"

      let hd = Var "_$List.head"
      let tl = Var "_$List.tail"

      let append = Var "_$List.append"

      let of_array = Var "_$List.lsFromArray"
    end

    module JSON = struct
      let parse = Var "JSON.parse"
    end

    module Math = struct
      let floor = Var "Math.floor"
      let pow = Var "Math.pow"
    end
  end

  module Aux = struct
    let call f args = Call (f, args)

    let project record label =
      Project (record, label)

    let return exp = Return exp

    let die msg =
      Call (Var "error", [Constructors.strlit msg; Var ObjectContinuation.__kappa])

    let set_of_array arr =
      call (InlineJS "new Set") [arr]

    module List = struct
      let cons x xs =
        call Runtime.List.cons [x; xs]
      let head xs =
        call Runtime.List.hd [xs]
      let tail xs =
        call Runtime.List.tl [xs]
      let of_array arr =
        call Runtime.List.of_array [arr]
    end
  end
end

(** IR variable environment *)
module VEnv = Env.Int

(** Type of environments mapping IR variables to source variables *)
type venv = string VEnv.t

module VariableInspection = struct
  let inspect_code_variables code =
    let vars = ref (StringSet.empty) in
    let add_var s = vars := (StringSet.add s (!vars)) in

    let binders = ref (StringSet.empty) in
    let add_binder s = binders := (StringSet.add s (!binders)) in
    let add_binders = List.iter (add_binder) in

    let get_vars () =
      let res = StringSet.diff (!vars) (!binders) |> StringSet.elements in
      res in

    let rec go cmd =
      let open Code in
      match cmd with
        | Var s -> add_var s
        | Fn (bnds, cmd) ->
            add_binders bnds;
            go cmd
        | LetFun ((bnd1, bnds, cmd1, _), cmd2) ->
            add_binder bnd1; add_binders bnds;
            go cmd1; go cmd2
        | LetRec (xs, cmd) ->
            List.iter (fun (bnd1, bnds, cmd, _) ->
              add_binder bnd1; add_binders bnds; go cmd) xs;
            go cmd
        | Call (c, cs) -> go c; List.iter (go) cs
        | Unop (_, c) -> go c
        | Binop (c1, _, c2) -> go c1; go c2
        | If (i, t, e) -> List.iter (go) [i;t;e]
        | Dict xs -> List.iter (go -<- snd) xs
        | Arr xs -> List.iter (go) xs
        | Project (e, _l) -> go e
        | Bind (bnd, c1, c2) -> add_binder bnd; go c1; go c2
        | Return c -> go c
        | Switch (scrutinee, sm, sc_opt) ->
            go scrutinee;
            StringMap.iter (fun _ code -> go code) sm;
            OptionUtils.opt_iter (fun c -> go c) sc_opt
        | Lit _ | InlineJS _ | Nothing -> () in
    go code;
    get_vars ()

  let get_affected_variables code =
    inspect_code_variables code
    |> List.map (fun v -> Code.Var(v))
end


(** Continuation parameter name (convention) *)
let __kappa = Code.ObjectContinuation.__kappa
(**
  Required runtime support (documenting any JavaScript functions used):

  _$Links.concat(a, b)
     concatenate two sequences: either strings or lists
  _$Links.accum(f, i)
    concatMap: apply f to every element of the sequence `i' and concatenate the results.
  _plus, _minus, etc.
    curried function versions of the standard arithmetic operators
  _$Links.XML(tag, attrs, children)
    create a DOM node with name `tag'
                       and attributes `attrs' (a dictionary)
                       and children `children' (a sequence of DOM nodes and strings)
  _$Links.union(r, s)
    return the union of the records r and s
    precondition: r and s have disjoint labels
  _$Links.project(record, label)
    project a field of a record
  _$Links.erase(record, label)
    return a record like "record" but without the field labeled "label"

  _start(tree)
    Replace the current page with `tree'.

  Also, any `builtin' functions from Lib.value_env.
 *)



module type JS_CODEGEN = sig
  val string_of_js : Code.t -> string
  val output : out_channel -> Code.t -> unit
end

module Js_CodeGen : JS_CODEGEN = struct
  (** Pretty printer for JavaScript code *)
  module PP :
  sig
    val show : Code.t -> string
    val output : out_channel -> Code.t -> unit
  end =
    struct
      open PP
      open Code

      (** Pretty-print a Code value as a JavaScript string. *)
      let rec show c : PP.doc =
        let show_func : ?name:[`Anonymous | `Name of string] -> Code.t -> PP.doc
          = fun ?(name=`Anonymous) fn ->
          match fn, name with
          | Fn (vars, body), `Name name ->
            PP.group
              (PP.text "function" ^+^
               PP.text name ^^
               (formal_list vars) ^+^
               (braces ((nest 2 (break ^^ show body)) ^^ break)))
          | Fn (vars, body), `Anonymous ->
            PP.group
              (PP.text "function" ^^
               (formal_list vars) ^+^
               (braces ((nest 2 (break ^^ show body)) ^^ break)))
          | _ -> assert false
        in
        let show_case l e =
          PP.text "case" ^+^
          PP.text (Printf.sprintf "'%s'" l) ^^
          PP.text ":" ^+^
          braces
            ((nest 2
               (break ^^
                show e ^|
                PP.text "break;")) ^^
             break)
        in
        let show_cases cases =
          StringMap.fold (fun l c s -> s ^| show_case l c) cases empty
        in
        let show_default case =
          opt_app
            (fun e ->
               PP.text "default:" ^+^ braces
                    ((nest 2
                        (break ^^
                         show e ^^
                         PP.text ";" ^|
                         PP.text "break;")) ^^
                  break))
            PP.DocNil case
        in
        let maybe_parenthesise = function
          | Var _
            | Lit _
            | Call _
            | Dict _
            | Arr _
            | Bind _
            | Return _
            | Project _
            | InlineJS _
            | Nothing as c -> show c
          | c -> parens (show c)
        in
        match c with
        | Var x -> PP.text x
        | Nothing -> PP.text ""
        | Lit literal -> PP.text literal
        | LetFun ((name, vars, body, _location), rest) ->
           (show_func ~name:(`Name name) (Fn (vars, body))) ^^ break ^^ show rest
        | LetRec (defs, rest) ->
           PP.vsep (punctuate " " (List.map (fun (name, vars, body, _loc) -> show_func ~name:(`Name name) (Fn (vars, body))) defs)) ^^
             break ^^ show rest
        | Fn _ -> show_func c
        | Call (fn, args) ->
          maybe_parenthesise fn ^^ (PP.arglist (List.map show args))
        | Unop (op, body) -> PP.text op ^+^ (maybe_parenthesise body)
        | Binop (l, op, r) -> (maybe_parenthesise l) ^+^ PP.text op ^+^ (maybe_parenthesise r)
        | If (cond, c1, c2) ->
          PP.group
            (PP.text "if" ^+^
             (parens (show cond)) ^+^
             (braces
                ((nest 2
                    (break ^^
                     group (nest 2 (show c1)))) ^^
                 break)) ^+^
              PP.text "else" ^+^
              (braces
                 ((nest 2
                     (break ^^
                      group (nest 2 (show c2)))) ^^
                  break)))
        | Switch (scrutinee, cases, default) ->
          PP.group
            (PP.text "switch" ^+^
             (parens (show scrutinee) ^+^
             (braces
                ((nest 2
                    (break ^^
                     group (show_cases cases) ^|
                            show_default default))) ^^
                 break)))
        | Dict (elems) ->
           PP.braces (hsep (punctuate ","
                              (List.map (fun (name, value) ->
                                   group (PP.text "'" ^^ PP.text name ^^
                                            PP.text "':" ^^ show value))
                                 elems)))
        | Arr xs -> PP.brackets (hsep (punctuate "," (List.map show xs)))
        | Project (e, l) ->
          let is_identifier s =
            let ident_pattern = Str.regexp "^\\([$_A-Za-z][$_A-Za-z0-9]*\\)$" in
            Str.string_match ident_pattern s 0
          in
          let is_integer s =
            let int_pattern = Str.regexp "^-?\\(\\(0\\)\\|\\([1-9][0-9]*\\)\\)$" in
            Str.string_match int_pattern s 0
          in
          maybe_parenthesise e ^^
          (if is_identifier l then PP.text "." ^^ PP.text l
           else brackets
               (if is_integer l then PP.text l
                else PP.text "'" ^^ PP.text l ^^ PP.text "'"))
        | Bind (name, value, body) ->
          PP.text "let" ^+^
          PP.text name ^+^
          PP.text "=" ^+^
          show value ^^
          PP.text ";" ^^
          break ^^
          show body
        | Return expr ->
           PP.text "return " ^^ (show expr) ^^ PP.text ";"
        | InlineJS raw_code ->
           PP.text raw_code

      let output oc = show ->- PP.out_pretty oc 144
      let show = show ->- PP.pretty 144
    end

  let string_of_js x = PP.show x
  let output oc code = PP.output oc code
end

(* Specialness:

   * Top-level boilerplate code to replace the root element and reset the focus

     The special function _start takes an html page as a string and
     replaces the currently displayed page with that one.

     Some of the other functions are equivalents to Links builtins
     (e.g. int_of_string, xml)
 *)

module Arithmetic : sig
  val is : string -> bool
  val gen : string -> Code.t list -> Code.t
end = struct
  let builtin_binops =
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


  let builtin_unops =
    [ "negate", "-" ;
      "negatef", "-" ]

  let is x = StringMap.mem x builtin_binops || List.mem_assoc x builtin_unops
  let js_name op = val_of (StringMap.find op builtin_binops)
  let gen op args =
    let open Code in
    match op, args with
      | "/", [l; r] -> Aux.call Runtime.Math.floor [Binop (l, "/", r)]
      | "^", [_; _] -> Aux.call Runtime.Math.floor [Call (Runtime.Math.pow, args)]
      | "^.", [_; _] -> Aux.call Runtime.Math.pow args
      | ("negate" | "negatef"), [_] -> Aux.call (Var "-") args
      | _, [l; r] -> Binop(l, js_name op, r)
      | _, _ -> raise (internal_error (Printf.sprintf "Unrecognised primitive arithmetic operation '%s' with arity %d\n" op (List.length args)))
end

module StringOp :
sig
  val is : string -> bool
  val gen : string -> Code.t list -> Code.t
end =
struct
  let builtin_ops =
    StringMap.from_alist
      [ "^^",  Some "+"  ]

  let is x = StringMap.mem x builtin_ops
  let js_name op = val_of (StringMap.find op builtin_ops)
  let gen op args =
    let open Code in
    match args with
    | [l; r] -> Binop(l, js_name op, r)
    | _ -> raise (internal_error (Printf.sprintf "Unrecognised string operation '%s' with arity %d\n" op (List.length args)))
end

module Comparison :
sig
  val is : string -> bool
  val gen : string -> Code.t list -> Code.t
end =
struct
  (* these names should be used for non-primitive types *)
  let funs =
    let open Code in
    StringMap.from_alist
      [ "==", Runtime.Links.eq ;
        "<>", Runtime.Links.neq;
        "<",  Runtime.Links.lt ;
        ">",  Runtime.Links.gt ;
        "<=", Runtime.Links.lte;
        ">=", Runtime.Links.gte ]

  let is x = StringMap.mem x funs
  let js_name op = StringMap.find op funs
  let gen op args =
    let open Code in
    match op, args with
      | "<>", [l; r] -> Unop("!", Aux.call Runtime.Links.eq [l; r])
      | ( "==" | "<" | ">" | "<=" | ">="), [l; r] -> Aux.call (js_name op) [l; r]
      | _, _ -> raise (internal_error (Printf.sprintf "Unrecognised relational operator '%s' with arity %d\n" op (List.length args)))
end

module ListPrim : sig
  val is : string -> bool
  val gen : string -> Code.t list -> Code.t
end = struct
  let names =
    let open Code.Runtime.List in
    [ "Cons", cons
    ; "$$hd", hd
    ; "$$tl", tl
    ; "hd", hd
    ; "tl", tl
    ; "Concat", append ]

  let is fname = List.mem_assoc fname names

  let gen fname vs =
    let open Code in
    match fname with
    | "Nil" -> Code.Runtime.List.nil
    | _ ->
      let name = List.assoc fname names in
      Aux.call name vs
end

(** [cps_prims]: a list of primitive functions that need to see the
    current continuation. Calls to these are translated in CPS rather than
    direct-style.  A bit hackish, this list. *)
let cps_prims = ["recv"; "sleep"; "spawnWait"; "receive"; "request"; "accept"]

(** Generate a JavaScript name from a binder, wordifying symbolic names *)
let name_binder b =
  let name = Js.name_binder b in
  (* TODO(dhil): The below check is unnecessary as `Js.name_binder`
     never returns the empty string. *)
  (if (name = "") then
     prerr_endline (Ir.show_binder b));
  assert (name <> "");
  (Var.var_of_binder b, name)

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
  val pop : t -> Code.MetaContinuation.t * t * t

  (* Turns code into a continuation. *)
  val reflect : Code.t -> t
  (* Turns a continuation into code. *)
  val reify   : t -> Code.t

  (* Continuation name binding. *)
  val bind : t -> (t -> Code.t) -> Code.t

  (* Continuation application generation. The optional strategy
     parameter decides whether the application should be yielding or
     direct. *)
  val apply : ?strategy:[`Yield | `Direct] -> t -> Code.MetaContinuation.t

  (* Augments a function [Fn] with a continuation parameter and
     reflects the result as a continuation. The continuation parameter
     in the callback provides access to the current continuation. *)
  val contify_with_env : (t -> venv * Code.t) -> venv * t

  (* Generates appropriate bindings for primitives *)
  val primitive_bindings : Code.t

  (* Generates a string dump of the continuation, for debugging purposes. *)
  val to_string : t -> string
end

(* (\* The standard Links continuation (no extensions) *\) *)
module Default_Continuation : CONTINUATION = struct
  (* We can think of this particular continuation structure as
     singleton list. *)
  type t = Identity
         | Code of Code.t

  let identity = Identity
  let toplevel = Code Code.(Fn (["x"], Nothing))
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
    | Identity -> Code.(Fn (["_x"], Var "_x"))
    | Code code -> code

  let bind k body =
    match k with
    | Code (Code.Var _) -> body k
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
       Code.Bind (kb, reify k, body (reflect (Code.Var kb)))

  let apply ?(strategy=`Yield) k arg =
    let open Code in
    match strategy with
    | `Direct -> Call (Var "_$K.apply", [reify k; arg])
    | _       -> Call (Var "_$K.yield", [reify k; arg])

  let primitive_bindings =
    Code.InlineJS {|
/* First-order continuation module */
const _$K = (function() {
  return Object.freeze({
     'kind': 'Default_Continuation',
     'apply': function(k, arg) {
        return k(arg);
     },
     'yield': function(k, arg) {
       return _$Proc.yield(function() { return _$K.apply(k, arg); });
     },
     'idy': function(x) { return; },
     'make': function(k) { return k; }
  });
})();
let receive = _default_receive;
|}

  let contify_with_env fn =
    match fn Identity with
    | env, (Code.Fn _ as k) -> env, reflect k
    | _ -> raise (internal_error "error: contify: non-function argument.")

  (* Pop returns the code in "the singleton list" as the second
     component, and returns a fresh singleton list containing the
     identity element in the third component. *)
  let pop k = Code.MetaContinuation.identity, k, Identity

  let to_string = function
    | Identity -> "IDENTITY"
    | Code code -> "CODE: " ^ (Code.show code)
end

(* The higher-order continuation structure for effect handlers
   support *)
module Higher_Order_Continuation : CONTINUATION = struct
  (* We can think of this particular continuation structure as a
     nonempty stack with an even number of elements. *)
  type t = Cons of Code.t * t
         | Reflect of Code.t
         | Identity

  (* Auxiliary functions for manipulating the continuation stack *)
  include Code.Aux.List
  let nil = Code.Runtime.List.nil
  let toplevel =
    let open Code in
    let idk =
      Fn (["x"; "ks"], Nothing)
    in
    let efferr =
      Fn (["z"; "ks"],
          Bind ("tag", Code.Aux.project (Var "z") "_label",
                InlineJS {| _error("Unhandled operation `" + z._label + "'.") |}))
    in
    Cons (idk, Cons (efferr, Reflect nil))

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
    (* Binds a continuation *)
    let rec bind bs ks =
      let open Code in
      fun kappas ->
        match kappas with
        | Identity ->
           (* Generate a new continuation name *)
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
  let bs, ks, seed = bind Code.MetaContinuation.identity (fun kappas -> kappas) kappas in
  bs (body (ks (reflect seed)))

  let apply ?(strategy=`Yield) k arg =
    let open Code in
    match strategy with
    | `Direct -> Aux.call (Var "_$K.apply") [reify k; arg]
    | _       -> Aux.call (Var "_$K.yield") [reify k; arg]

  let primitive_bindings =
    Code.InlineJS ({|
/* Higher-order continuation module. */
const _$K = (function() {
  function make(k) {
    return _$List.cons(k, _$List.singleton(function(z, ks) {
      return _error("Unhandled operation `" + z._label + "'.");
    }));
  }
  return Object.freeze({
    'kind': 'Higher_Order_Continuation',
    'apply': function(ks, arg) {
       const k = _$List.head(ks);
       ks = _$List.tail(ks);
       return k(arg, ks);
    },
    'yield': function(ks, arg) {
       return _$Proc.yield(function() { return _$K.apply(ks, arg) });
    },
    'idy': make(function(x, ks) { return; }),
    'make': make
  });
})();
|} ^ (if session_exceptions_enabled
      then "let receive = _exn_receive;"
      else "let receive = _default_receive;"))

  let contify_with_env fn =
    let open Code in
    let name = __kappa in
    match fn (reflect (Var name)) with
    | env, Fn (args, body) -> env, reflect (Fn (args @ [name], body))
    | _ -> raise (internal_error "contify: non-function argument.")

  let rec pop = function
    | Cons (kappa, kappas) ->
       Code.MetaContinuation.identity, (reflect kappa), kappas
    | Reflect ks ->
       let __k = gensym ~prefix:"__k" () in
       let __ks = gensym ~prefix:"__ks" () in
       (fun code ->
         Code.Bind (__k, head ks,
               Code.Bind (__ks, tail ks, code))),
       (reflect (Code.Var __k)), reflect (Code.Var __ks)
    | Identity -> pop toplevel

  let rec to_string = function
    | Identity -> "IDENTITY"
    | Reflect code -> "REFLECT: " ^ (Code.show code)
    | Cons (code, k) ->
        "CONS: " ^ (Code.show code) ^ ", \n" ^ (to_string k)

end

(** Compiler interface *)
(* module type JS_COMPILER = sig
 *   type source = [
 *     | `Bindings of Ir.binding list
 *     | `Program of Ir.program ]
 *
 *   val compile : source -> Value.env -> Js.program
 * end *)

module type JS_PAGE_COMPILER = sig
  (* include JS_COMPILER *)
  val generate_program : venv -> Ir.computation -> venv * Code.t
  val generate_stubs : Value.env -> Ir.binding list -> Code.MetaContinuation.t
  val generate_toplevel_bindings : Value.env -> Json.json_state -> venv -> Ir.binding list -> Json.json_state * venv * string list * Code.MetaContinuation.t
  val wrap_with_server_lib_stubs : Code.MetaContinuation.t
  val primitive_bindings : Code.t
end

(** [generate]
    Generates JavaScript code for a Links expression

    With CPS transform, result of generate is always of type : (a -> w) -> b
*)
module CPS_Compiler: functor (K : CONTINUATION) -> sig
  include JS_PAGE_COMPILER
end = functor (K : CONTINUATION) -> struct
  type continuation = K.t

  open Code.Aux.List

  let call = Code.Aux.call
  let project = Code.Aux.project
  let return x = Code.Return x

  let apply_yielding f args k =
    let open Code in
    call Runtime.Proc.yield
      [Fn ([], return (call f (args @ [K.reify k])))]

  let contify fn =
    snd @@ K.contify_with_env (fun k -> VEnv.empty, fn k)

  let rec generate_value env : Ir.value -> Code.t =
    let open Code in
    let open Code.Constructors in
    let gv v = generate_value env v in
    function
    | Ir.Constant c ->
      begin
        let open Constant in
        match c with
        | Int v   -> Lit (string_of_int v)
        | Float v -> Lit (string_of_float' v)
        | Bool v   -> Lit (string_of_bool v)
        | Char v   -> chrlit v
        | String v -> chrlistlit v
        | DateTime (Timestamp.Timestamp ts) ->
          Dict [("_type", strlit "timestamp");
                ("_value", Lit (UnixTimestamp.of_calendar ts |> string_of_float))]
        | DateTime Timestamp.Infinity ->
          Dict [("_type", strlit "infinity")]
        | DateTime Timestamp.MinusInfinity ->
             Dict [("_type", strlit "-infinity")]
       end
    | Ir.Variable var ->
       (* HACK *)
       let name = VEnv.find var env in
       if Arithmetic.is name then
         Fn (["x"; "y"; __kappa],
             return (K.apply (K.reflect (Var __kappa))
                       (Arithmetic.gen name [Var "x"; Var "y"])))
       else if StringOp.is name then
         Fn (["x"; "y"; __kappa],
             return (K.apply (K.reflect (Var __kappa))
                       (StringOp.gen name [Var "x"; Var "y"])))
       else if Comparison.is name then
         Fn (["x"; "y"; __kappa],
             return (K.apply (K.reflect (Var __kappa))
                       (Comparison.gen name [Var "x"; Var "y"])))
       else
         Var name
    | Ir.Extend (field_map, rest) ->
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
            call Runtime.Links.union [gv v; dict]
       end
    | Ir.Project (name, v) ->
       project (gv v) name
    | Ir.Erase (names, v) ->
       call Runtime.Links.erase
         [gv v; Aux.set_of_array (Arr (List.map strlit (StringSet.elements names)))]
    | Ir.Inject (name, v, _t) ->
       Dict [("_label", strlit name);
             ("_value", gv v)]

      (* erase polymorphism *)
    | Ir.TAbs (_, v)
    | Ir.TApp (v, _) -> gv v

    | Ir.XmlNode (name, attributes, children) ->
       generate_xml env name attributes children

    | Ir.ApplyPure (f, vs) ->
       let f = strip_poly f in
       begin
         match f with
         | Ir.Variable f ->
            let f_name = VEnv.find f env in
            begin
              match vs with
              | [l; r] when StringOp.is f_name ->
                StringOp.gen f_name [gv l; gv r]
              | [l; r] when Comparison.is f_name ->
                Comparison.gen f_name [gv l; gv r]
              | vs when Arithmetic.is f_name ->
                Arithmetic.gen f_name (List.map gv vs)
              | vs when ListPrim.is f_name ->
                ListPrim.gen f_name (List.map gv vs)
              | _ ->
                 if Lib.is_primitive f_name
                   && not (List.mem f_name cps_prims)
                   && not (Location.is_server (Lib.primitive_location f_name))
                 then call (Var ("_" ^ f_name)) (List.map gv vs)
                 else call (gv (Ir.Variable f)) (List.map gv vs)
            end
         | _ -> call (gv f) (List.map gv vs)
       end
    | Ir.Closure (f, _, v) ->
      let f' = gv (Ir.Variable f) in
      let env = gv v in
      let closure = call (project f' "bind") [Var "null"; env] in
      if session_exceptions_enabled
      then call (project (Var "Object") "defineProperty") [closure; strlit "__closureEnv"; Dict [("value", env)]]
      else closure
    | Ir.Coerce (v, _) -> gv v

  and generate_xml env tag attrs children =
    let open Code in
    call Runtime.Links.xml
      [Constructors.strlit tag;
       Dict (StringMap.fold (fun name v bs ->
           (name, generate_value env v) :: bs) attrs []);
       Aux.List.of_array (Arr (List.map (generate_value env) children))]

  let generate_remote_call f_var xs_names env =
    let open Code in
    call (call Runtime.Links.remoteCall [Var __kappa])
      [Constructors.intlit f_var;
       env;
       Dict (
         List.map2
           (fun n v -> string_of_int n, Var v)
           (Utility.fromTo 1 (1 + List.length xs_names))
           xs_names
       )]


(** Generate stubs for processing functions serialised in remote calls *)
  module GenStubs =
  struct
    let rec fun_def : Ir.fun_def -> Code.MetaContinuation.t =
      fun fundef code ->
        let module Var' = Var in
        let open Code in
        let Ir.{fn_binder = fb; fn_tyvars = _; fn_params = xsb; fn_body = _; fn_closure = zb;
                        fn_location; fn_unsafe = _} = fundef
        in
        let f_var = Var'.var_of_binder fb in
        let bs = List.map name_binder xsb in
        let _, xs_names = List.split bs in

        let xs_names', env =
          match zb with
          | None -> xs_names, Dict []
          | Some _ ->  "_env" :: xs_names, Var "_env" in

      (* this code relies on eta-expanding functions in order to take
         advantage of dynamic scoping *)

        match fn_location with
        | Location.Client | Location.Unknown ->
           let xs_names'' = xs_names'@[__kappa] in
           LetFun ((Js.var_name_binder fb,
                    xs_names'',
                    return (call (Var (snd (name_binder fb)))
                              (List.map (fun x -> Var x) xs_names'')),
                    fn_location),
                   code)
        | Location.Server ->
           LetFun ((Js.var_name_binder fb,
                    xs_names'@[__kappa],
                    return (generate_remote_call f_var xs_names env),
                    fn_location),
                   code)
    and binding : Ir.binding -> Code.MetaContinuation.t =
      function
      | Ir.Fun def ->
         fun_def def
      | Ir.Rec defs ->
         List.fold_right (-<-)
           (List.map (fun_def) defs)
           Code.MetaContinuation.identity
      | _ -> Code.MetaContinuation.identity
    and bindings : Ir.binding list -> Code.MetaContinuation.t =
      fun bindings code ->
        (List.fold_right
           (-<-)
           (List.map binding bindings)
           Code.MetaContinuation.identity)
          code

(* FIXME: this code should really be merged with the other
   stub-generation code and we should generate a numbered version of
   every library function.
*)

(** stubs for server-only primitives *)
    let wrap_with_server_lib_stubs : Code.t -> Code.t = fun code ->
      let open Code in
      let server_library_funcs =
        List.rev
          (Env.Int.fold
             (fun var _v funcs ->
               let name = Lib.primitive_name var in
               if Location.is_server (Lib.primitive_location name) then
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
             [(name, args, return (generate_remote_call var args (Dict [])))])
          server_library_funcs
      in
      List.fold_right
        (fun (name, args, body) code ->
          LetFun
            ((name,
              args @ [__kappa],
              body,
              loc_server),
             code))
        prim_server_calls
        code
  end

  let rec generate_tail_computation : venv -> Ir.tail_computation -> continuation -> Code.t =
    fun env tc kappa ->
    let open Code in
    let gv v = generate_value env v in
    let gc c kappa = snd (generate_computation env c kappa) in
    match (tc : Ir.tail_computation) with
    | Ir.Return v ->
      return (K.apply kappa (gv v))
    | Ir.Apply (f, vs) ->
      let f = strip_poly f in
      begin
        match f with
        | Ir.Variable f ->
          let f_name = VEnv.find f env in
          begin
            match vs with
            | [l; r] when StringOp.is f_name ->
               let l = gv l in
               let r = gv r in
               return (K.apply kappa (StringOp.gen f_name [l; r]))
            | [l; r] when Comparison.is f_name ->
               let l = gv l in
               let r = gv r in
               return (K.apply kappa (Comparison.gen f_name [l; r]))
            | vs when Arithmetic.is f_name ->
              return (K.apply kappa (Arithmetic.gen f_name (List.map gv vs)))
            | vs when ListPrim.is f_name ->
              return (K.apply kappa (ListPrim.gen f_name (List.map gv vs)))
            | _ ->
              if Lib.is_primitive f_name
              && not (List.mem f_name cps_prims)
              && not (Location.is_server (Lib.primitive_location f_name))
              then
                let arg = call (Var ("_" ^ f_name)) (List.map gv vs) in
                return (K.apply ~strategy:`Direct kappa arg)
              else if f_name = "receive" && session_exceptions_enabled then
                let code_vs = List.map gv vs in
                let action cancel_thunk =
                  return (apply_yielding (Var f_name) (code_vs @ [cancel_thunk]) kappa)
                in
                generate_cancel_stub env action kappa
              else
                return (apply_yielding (gv (Ir.Variable f)) (List.map gv vs) kappa)
          end
        | _ ->
          return (apply_yielding (gv f) (List.map gv vs) kappa)
      end
    | Ir.Special special ->
      generate_special env special kappa
    | Ir.Case (v, cases, default) ->
      let v = gv v in
      let k, scrutinee =
        match v with
        | Var _ -> (fun e -> e), v
        | _ ->
          let x = gensym ~prefix:"x" () in
          (fun e -> Bind (x, v, e)), Var x
      in
      K.bind kappa
        (fun kappa ->
           let gen_cont (xb, comp) =
             let (x, x_name) = name_binder xb in
             let comp = snd (generate_computation (VEnv.bind x x_name env) comp kappa) in
             Bind (x_name, project scrutinee "_value", comp)
           in
           let cases = StringMap.map gen_cont cases in
           let default = opt_map gen_cont default in
           k (Switch (project scrutinee "_label", cases, default)))
    | Ir.If (v, c1, c2) ->
      K.bind kappa
        (fun kappa ->
           If (gv v, gc c1 kappa, gc c2 kappa))

  and generate_special env : Ir.special -> continuation -> Code.t
    = fun sp kappa ->
      let module Var' = Var in
      let open Code in
      let open Code.Constructors in
      let gv v = generate_value env v in
      match sp with
      | Ir.Wrong _ -> return (Aux.die "Pattern matching failure") (* THIS MESSAGE SHOULD BE MORE INFORMATIVE *)
      | Ir.Database _ | Ir.Table _
          when Settings.get js_hide_database_info ->
         return (K.apply kappa Runtime.Constants.unit)
      | Ir.Database v ->
         return (K.apply kappa (Dict [("_db", gv v)]))
      | Ir.Table Ir.{ database; table; keys; temporal_fields; table_type = (_, readtype, _, _) } ->
         let fields =
             match temporal_fields with
                | None -> []
                | Some (field_from, field_to) ->
                    [("temporal_from", strlit field_from);
                     ("temporal_to", strlit field_to)]
         in
         let dict_fields =
            [("db", gv database);
                         ("name", gv table);
                         ("keys", gv keys);
                         ("row",
                          strlit (Types.string_of_datatype (readtype)))]
         in
         return (K.apply kappa
                   (Dict [("_table", Dict (fields @ dict_fields))]))
      | Ir.LensSerial _ | Ir.LensSelect _ | Ir.LensJoin _ | Ir.LensDrop _ | Ir.Lens _ | Ir.LensCheck _ ->
        return (K.apply kappa Runtime.Constants.unit)
      | Ir.LensGet _ | Ir.LensPut _ -> return (Aux.die "Attempt to run a relational lens operation on client")
      | Ir.Query _                  -> return (Aux.die "Attempt to run a query on the client")
      | Ir.TemporalJoin _           -> return (Aux.die "Attempt to run a temporal join on the client")
      | Ir.InsertRows _             -> return (Aux.die "Attempt to run a database insert on the client")
      | Ir.InsertReturning _        -> return (Aux.die "Attempt to run a database insert on the client")
      | Ir.Update _                 -> return (Aux.die "Attempt to run a database update on the client")
      | Ir.Delete _                 -> return (Aux.die "Attempt to run a database delete on the client")
      | Ir.CallCC v ->
         K.bind kappa
           (fun kappa -> return (apply_yielding (gv v) [K.reify kappa] kappa))
      | Ir.Select (l, c) ->
         let arg = call (Var "_send") [Dict ["_label", strlit l; "_value", Dict []]; gv c] in
         return (K.apply ~strategy:`Direct kappa arg)
      | Ir.Choice (c, bs) ->
         let result = gensym ~prefix:"result" () in
         let received = gensym ~prefix:"received" () in
         let bind, skappa, skappas = K.pop kappa in
         let skappa' =
           contify (fun kappa ->
             let message = project (Var result) "1" in
             let channel = project (Var result) "2" in
             let generate_branch (cb, comp) =
               let (ch, chname) = name_binder cb in
               Bind (chname, channel, snd (generate_computation (VEnv.bind ch chname env) comp K.(skappa <> kappa)))
             in
             let branches = StringMap.map generate_branch bs in
             Fn ([result], (Bind (received, message, (Switch (project (Var received) "_label", branches, None))))))
         in
         let cont = K.(skappa' <> skappas) in
         if (session_exceptions_enabled) then
           let action cancel_stub =
             call (Var "receive") [gv c; cancel_stub; K.reify cont]
           in
           bind (generate_cancel_stub env action cont)
         else
           bind (call (Var "receive") [gv c; K.reify cont])
      | Ir.DoOperation (name, args, _) ->
         let maybe_box = function
           | [v] -> gv v
           | vs -> Dict (List.mapi (fun i v -> (string_of_int @@ i + 1, gv v)) vs)
         in
         let nil = Runtime.List.nil in
         K.bind kappa
           (fun kappas ->
             (* kappa -- pure continuation *)
             let bind_skappa, skappa, kappas = K.pop kappas in
             (* eta -- effect continuation *)
             let bind_seta, seta, kappas   = K.pop kappas in
             let resumption = K.(cons (reify seta) (cons (reify skappa) nil)) in

             (* Session exceptions arising as a result of "raise" (i.e. those without an
              * environment passed as an argument already) need to be compiled specially *)
             let op =
               if (session_exceptions_enabled &&
                     name = Value.session_exception_operation &&
                   List.length args = 0) then
                 let affected_variables =
                   VariableInspection.get_affected_variables (K.reify kappa) in
                 let affected_arr = Dict ([("1", Aux.List.of_array (Arr affected_variables))]) in
                 Dict [ ("_label", strlit name)
                      ; ("_value", Dict [("p", affected_arr); ("s", resumption)]) ]
               else
                 Dict [ ("_label", strlit name)
                      ; ("_value", Dict [("p", maybe_box args); ("s", resumption)]) ]
             in
             bind_skappa (bind_seta (return (apply_yielding (K.reify seta) [op] kappas))))
      | Ir.Handle { Ir.ih_comp = comp; Ir.ih_cases = eff_cases; Ir.ih_return = return; Ir.ih_depth = depth } ->
         let comp_env = env in
         let vmap r y =
           call (Var "_vmapOp") [r; y]
         in
         let generate_body env (x, n) body kappas =
           let env' = VEnv.bind x n env in
           snd (generate_computation env' body kappas)
         in
         begin match depth with
         | Ir.Shallow -> raise (Errors.runtime_error "CPS compilation of shallow handlers is currently not supported")
         | Ir.Deep params ->
            let translate_parameters params =
              let is_parameterised = List.length params > 0 in
              let param_ptr_binder =
                Var'.fresh_binder
                  (Var'.make_local_info (Types.Not_typed, "_param_ptr"))
              in
              let env =
                let (x, n) = name_binder param_ptr_binder in
                VEnv.bind x n env
              in
              let params =
                List.mapi (fun i (binder,initial_value) -> (i, binder, initial_value)) params
              in
              let ptr = Ir.Variable (Var'.var_of_binder param_ptr_binder) in
              let initial_parameterise (bs, tc) =
                let name_map =
                  List.fold_left
                    (fun box (i, _, initial_value) ->
                      StringMap.add (string_of_int i) initial_value box)
                    StringMap.empty params
                in
                (Ir.Let (param_ptr_binder, ([], Ir.Return (Ir.Extend (name_map, None)))) :: bs, tc)
              in
              let parameterise body =
              (* The pointer points to the box containing the parameters *)
                List.fold_right
                  (fun (i, binder, _) (bs,tc) ->
                    let b =
                      Ir.Let (binder, ([], Ir.Return (Ir.Project (string_of_int i, ptr))))
                    in
                    (b :: bs, tc))
                  params body
              in
              let make_resumption s =
                if is_parameterised
                then call (Var "_make_parameterised_resumption") [Var (snd @@ name_binder param_ptr_binder); s]
                else call (Var "_make_resumption") [s]
              in
              env, comp, make_resumption, initial_parameterise, parameterise
            in
            let env, comp, make_resumption, initial_parameterise, parameterise = translate_parameters params in
            let value_case =
              let (xb, body) = return in
              let xb, x_name = name_binder xb, snd @@ name_binder xb in
              K.reflect
                (Fn ([x_name; "ks"],
                     let bind, _, kappa = K.(reflect ->- pop) (Var "ks") in
                     let body = parameterise body in
                     bind @@ generate_body env xb body kappa))
            in
            let eff_cases =
              let translate_eff_case env scrutinee (xb, resume, body) kappas =
                let (_x, x_name) as xb = name_binder xb in
                let (r, r_name) = name_binder resume in
                let p = project (project scrutinee "_value") "p" in
                let resume =
                  let s = project (project scrutinee "_value") "s" in
                  make_resumption s
                in
                let env' =
                  VEnv.bind r r_name env
                in
                let body = generate_body env' xb (parameterise body) kappas in
                Bind (r_name, resume,
                      Bind (x_name, p, body))
              in
              let translate_exn_case env scrutinee (xb, body) kappas =
                let xb = name_binder xb in
                let dummy_var_name = gensym ~prefix:"dummy" () in
                let x_name = snd xb in
                let p = project scrutinee "p" in
                Bind (x_name, project p "1",
                      (* FIXME: the following call should probably be
                         apply_yielding rather than a raw Call because
                         _handleSessionException makes use of the call
                         stack. *)
                      Bind (dummy_var_name, call (Var "_handleSessionException") [Var x_name],
                            generate_body env xb (parameterise body) kappas))
              in
              let eff_cases scrutinee kappas =
                StringMap.fold
                  (fun operation_name clause cases ->
                    StringMap.add operation_name
                      (if session_exceptions_enabled && operation_name = Value.session_exception_operation
                       then let (xb,_,body) = clause in
                            translate_exn_case env scrutinee (xb, body) kappas
                       else translate_eff_case env scrutinee clause kappas)
                      cases)
                  eff_cases StringMap.empty
              in
              let forward y ks =
                K.bind ks
                  (fun ks ->
                    let bind_k'_ks', k', ks' = K.pop ks in
                    let bind_h'_ks'', h', ks'' = K.pop ks' in
                    let bind code = bind_k'_ks' (bind_h'_ks'' code) in
                    let resumption =
                      Fn (["s"], Return (cons (K.reify h')
                                           (cons (K.reify k') (Var "s"))))
                    in
                    bind (Aux.return (apply_yielding (K.reify h') [vmap resumption y] ks'')))
              in
              K.reflect
                (Fn (["_z"; "ks"],
                     let ks = K.reflect (Var "ks") in
                     let scrutinee = Var "_z" in
                     Switch (project scrutinee "_label",
                             eff_cases scrutinee ks,
                             Some (forward (Var "_z") ks))))
            in
            let kappa = K.(value_case <> eff_cases <> kappa) in
            snd (generate_computation comp_env (initial_parameterise comp) kappa)
         end

  and generate_computation : venv -> Ir.computation -> continuation -> (venv * Code.t) =
    fun env (bs, tc) kappa ->
      let rec gbs : venv -> continuation -> Ir.binding list -> venv * Code.t =
        fun env kappa ->
          let open Code in
          function
          | Ir.Let (b, (_, Ir.Return v)) :: bs ->
             let (x, x_name) = name_binder b in
             let env', rest = gbs (VEnv.bind x x_name env) kappa bs in
             (env', Bind (x_name, generate_value env v, rest))
          | Ir.Let (b, (_, tc)) :: bs ->
             let (x, x_name) = name_binder b in
             let bind, skappa, skappas = K.pop kappa in
             let env',skappa' =
               K.contify_with_env
                 (fun kappas ->
                   let env', body = gbs (VEnv.bind x x_name env) K.(skappa <> kappas) bs in
                   env', Fn ([x_name], body))
             in
             env', bind (generate_tail_computation env tc K.(skappa' <> skappas))
          | Ir.Fun ({Ir.fn_binder = fb; _} as def) :: bs ->
             let (f, f_name) = name_binder fb in
             let def_header = generate_function env [] def in
             let env', rest = gbs (VEnv.bind f f_name env) kappa bs in
             (env', LetFun (def_header, rest))
          | Ir.Rec defs :: bs ->
             let fs = List.map (fun {Ir.fn_binder = fb; _} -> name_binder fb) defs in
             let env', rest = gbs (List.fold_left (fun env (x, n) -> VEnv.bind x n env) env fs) kappa bs in
             (env', LetRec (List.map (generate_function env fs) defs, rest))
          | Ir.Module _ :: bs
          | Ir.Alien _ :: bs -> gbs env kappa bs
          | [] -> (env, generate_tail_computation env tc kappa)
      in
      gbs env kappa bs

  and generate_function env fs :
      Ir.fun_def ->
    (string * string list * Code.t * Ir.location) =
    fun fundef ->
      let Ir.{fn_binder = fb; fn_tyvars = _; fn_params = xsb; fn_body; fn_closure = zb;
                        fn_location; fn_unsafe = _} = fundef
      in
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
      let body_env =
        List.fold_left
          (fun env (n, x) -> VEnv.bind n x env)
          env
          (fs @ bs)
      in
      let body =
        match fn_location with
        | Location.Client | Location.Unknown ->
           snd (generate_computation body_env fn_body (K.reflect (Code.Var __kappa)))
        | Location.Server -> return (generate_remote_call f xs_names (Code.Dict []))
      in
      (f_name,
       xs_names @ [__kappa],
       body,
       fn_location)
  and generate_cancel_stub env (action: Code.MetaContinuation.t) (kappa: K.t)  =
    let module Var' = Var in
    let open Code in
    (* Compile a thunk to be invoked if the operation fails *)
    let cancellation_thunk_name =
      gensym ~prefix:"cancellation_thunk" () in
    (* Grab affected variables from the continuation *)
    let affected_vars_name = gensym ~prefix:"affected_vars" () in
    let fresh_var = Var'.fresh_raw_var () in
    let affected_variables =
       VariableInspection.get_affected_variables (K.reify kappa) in
    (* Bind affected variables array to a fresh variable *)
    let env = VEnv.bind fresh_var affected_vars_name env in
    (* Compile raise operation WRT reflected, bound continuation *)
    let raiseOp =
      generate_special env (Ir.DoOperation (
        Value.session_exception_operation,
        [Ir.Variable fresh_var], Types.Not_typed)) in
    let fresh_kappa = gensym ~prefix:"kappa" () in
    let cancellation_thunk = Fn ([fresh_kappa], raiseOp (K.reflect (Var fresh_kappa))) in

    (* Generate binding code *)
    Bind (affected_vars_name, Aux.List.of_array (Arr affected_variables),
      Bind (cancellation_thunk_name, cancellation_thunk,
        action (Var cancellation_thunk_name)))


  let generate_toplevel_binding :
    Value.env ->
    Json.json_state ->
    venv ->
    Ir.binding ->
    Json.json_state * venv * string option * Code.MetaContinuation.t =

    fun valenv state varenv ->
    let open Code in
    function
    | Ir.Let (b, _) ->
      let (x, x_name) = name_binder b in
      (* Debug.print ("let_binding: " ^ x_name); *)
      let varenv = VEnv.bind x x_name varenv in
      let value = Value.Env.find x valenv in
      let jsonized_val = Json.jsonize_value value |> Json.json_to_string in
      let state = ResolveJsonState.add_value_information value state in
      (state,
       varenv,
       Some x_name,
       fun code -> Bind (x_name, call Runtime.JSON.parse [Constructors.strlit jsonized_val], code))
    | Ir.Fun def ->
      let {Ir.fn_binder = fb; _} = def
      in
      let (f, f_name) = name_binder fb in
      let varenv = VEnv.bind f f_name varenv in
      let def_header = generate_function varenv [] def in
      (state,
       varenv,
       None,
       fun code -> LetFun (def_header, code))
    | Ir.Rec defs ->
      let fs = List.map (fun {Ir.fn_binder = fb; _} -> name_binder fb) defs in
      let varenv =
        List.fold_left
          (fun env (n, x) -> VEnv.bind n x env)
          varenv fs
      in
      (state, varenv, None, fun code -> LetRec (List.map (generate_function varenv fs) defs, code))
    | Ir.Alien { binder; object_name; language } ->
      begin
        let open ForeignLanguage in
        (* TODO(dhil): If the foreign language isn't JavaScript,
           then I think a server-call should be generated. *)
        match language with
        | JavaScript ->
          let (a, _a_name) = name_binder binder in
          let varenv = VEnv.bind a object_name varenv in
          state, varenv, None, Code.MetaContinuation.identity
      end
    | Ir.Module _ -> state, varenv, None, Code.MetaContinuation.identity

  let rec generate_toplevel_bindings : Value.env -> Json.json_state -> venv -> Ir.binding list -> Json.json_state * venv * string list * Code.MetaContinuation.t =
    fun valenv state venv ->
      function
      | []      -> state, venv, [], Code.MetaContinuation.identity
      | b :: bs ->
         let state, venv, x, f = generate_toplevel_binding valenv state venv b in
         let state, venv, xs, g = generate_toplevel_bindings valenv state venv bs in
         let xs =
           match x with
           | None -> xs
           | Some x -> x :: xs in
         state, venv, xs, f -<- g

  let generate_stubs _venv bs = GenStubs.bindings bs
  let wrap_with_server_lib_stubs code = GenStubs.wrap_with_server_lib_stubs code

  let generate_program venv comp =
    let (env, code) = generate_computation venv comp K.toplevel in
    (env, Code.Aux.call (Code.Fn ([], code)) [])

  let primitive_bindings = K.primitive_bindings
end

let backend =
  Settings.(option ~default:(Some "cps") "js_compiler"
            |> privilege `System
            |> hint "<cps>"
            |> synopsis "Selects the JavaScript compiler"
            |> to_string from_string_option
            |> convert Utility.some
            |> sync)

module Continuation =
  (val
      (match Settings.get backend with
      | Some "cps" when Settings.get Basicsettings.Handlers.enabled ->
         (module Higher_Order_Continuation : CONTINUATION)
      | Some "cps" ->
         (module Default_Continuation : CONTINUATION)
      (** TODO: better error handling *)
      | _ -> raise (Errors.runtime_error "Unrecognised JS backend.")) : CONTINUATION)

module Compiler = CPS_Compiler(Continuation)
