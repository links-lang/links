(** JavaScript generation *)
open Utility
open Js
open LispJs

let _ = ParseSettings.config_file

let js_hide_database_info = Basicsettings.Js.hide_database_info
let session_exceptions_enabled = Settings.get_value (Basicsettings.Sessions.exceptions_enabled)

(* strip any top level polymorphism from an expression *)
let rec strip_poly =
  function
    | `TAbs (_, e)
    | `TApp (e, _) -> strip_poly e
    | e -> e

(** IR variable environment *)
module VEnv = Env.Int
type venv = string VEnv.t

module VariableInspection = struct
  (* let inspect_code_variables code =
   *   let open Pervasives in
   *   let vars = ref (StringSet.empty) in
   *   let add_var s = vars := (StringSet.add s (!vars)) in
   *   
   *   let binders = ref (StringSet.empty) in
   *   let add_binder s = binders := (StringSet.add s (!binders)) in
   *   let add_binders = List.iter (add_binder) in
   *   
   *   let get_vars () =
   *     let res = StringSet.diff (!vars) (!binders) |> StringSet.elements in
   *     Debug.print "affected client vars: \n";
   *     List.iter (Debug.print) res;
   *     res in
   *   
   *   let rec go cmd =
   *     match cmd with
   *       | Var s -> add_var s
   *       | Fn (bnds, cmd) ->
   *           add_binders bnds;
   *           go cmd
   *       | LetFun ((bnd1, bnds, cmd1, _), cmd2) ->
   *           add_binder bnd1; add_binders bnds;
   *           go cmd1; go cmd2
   *       | LetRec (xs, cmd) ->
   *           List.iter (fun (bnd1, bnds, cmd, _) ->
   *             add_binder bnd1; add_binders bnds; go cmd) xs;
   *           go cmd
   *       | Call (c, cs) -> go c; List.iter (go) cs
   *       | Unop (_, c) -> go c
   *       | Binop (c1, _, c2) -> go c1; go c2
   *       | If (i, t, e) -> List.iter (go) [i;t;e]
   *       | Dict xs -> List.iter (go -<- snd) xs
   *       | Arr xs -> List.iter (go) xs
   *       | Bind (bnd, c1, c2) -> add_binder bnd; go c1; go c2
   *       | Return c -> go c
   *       | Case (bnd, sm, sc_opt) ->
   *           add_var bnd;
   *           StringMap.iter (fun _ (s, code) -> add_binder s; go code) sm;
   *           OptionUtils.opt_iter (fun (bnd, c) -> add_binder bnd; go c) sc_opt
   *       | Lit _ | Die _ | Nothing -> () in
   *   go code;
   *   get_vars () *)

  let get_affected_variables _code = assert false
    (* let open Pervasives in
     * inspect_code_variables code
     * |> List.map (fun v -> Var(v)) *)
end


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

module type JS_COMPILER = sig
  type source = [
    | `Bindings of Ir.binding list
    | `Program of Ir.program ]

  val compile : source -> Value.env -> Js.program
end

module type JS_PAGE_COMPILER = sig
  include JS_COMPILER

  val generate_program : venv -> Ir.computation -> venv * js
  val generate_stubs : Value.env -> Ir.bindings -> js
  val generate_toplevel_bindings : Value.env -> Json.json_state -> venv -> Ir.binding list -> Json.json_state * venv * string list * js
  val wrap_with_server_lib_stubs : js -> js (* TODO remove *)
  val primitive_bindings : string (* TODO remove *)
end

(** Create a JS string literal, quoting special characters *)
let string_js_quote s =
  let sub old repl s = Str.global_replace (Str.regexp old) repl s in
    "'" ^ sub "'" "\\'" (sub "\n" "\\n" (sub "\\" "\\\\\\\\" s)) ^ "'"

(** Return a JS literal string from an OCaml int. *)
let intlit i = lit (String (string_of_int i))
(** Return a JS literal string from an OCaml string. *)
let strlit s = lit (String (string_js_quote s))
(** Return a JS literal string from an OCaml character. *)
let chrlit ch = obj ["_c", lit (String (string_js_quote(string_of_char ch)))] (* TODO unbox characters *)
(** Return a literal for the JS representation of a Links string. *)
let chrlistlit = strlit

let generate_formal_params n =
  let rec loop acc n =
    if n = 0 then acc
    else loop (Ident.fresh_binder ~prefix:"_x" () :: acc) (n-1)
  in
  loop [] n

(* Primitives *)
module Primitive_Compiler = struct

  module type PRIMITIVE_COMPILER = sig
    val recognise : string -> bool
    val arity : string -> int
    val compile : string -> expr list -> expr
    val maybe_eta : string -> expr
  end

  module type PRIMITIVE_AUX = sig
    type u
    val builtins : (u * int) StringMap.t
    val compile : string -> expr list -> expr
    val standalone : string -> expr
    val js_primitive : bool
  end

  module Make (P: PRIMITIVE_AUX): sig
    include PRIMITIVE_COMPILER
  end = struct
    open LispJs

    let recognise op = StringMap.mem op P.builtins
    let arity op = snd (StringMap.find op P.builtins)
    let compile op args = P.compile op args

    let maybe_eta op =
      if not P.js_primitive then P.standalone op
      else
        let (_, arity) = StringMap.find op P.builtins in
        let params = generate_formal_params arity in
        let vars = List.map (fun b -> variable (Ident.to_var b)) params in
        let body = lift_stmt (return @@ P.compile op vars) in
        arrow params body
  end

  module Arithmetic
    = Make(struct
          type u = binary_op
          let js_primitive = true
          let standalone _ = failwith "Unsupported"
          let builtins =
            StringMap.from_alist
              [ "+"  , (`Plus, 2) ;
                "+." , (`Plus, 2) ;
                "-"  , (`Minus, 2) ;
                "-." , (`Minus, 2) ;
                "*"  , (`Times, 2) ;
                "*." , (`Times, 2) ;
                "/"  , (`Division, 2) ;
                "/." , (`Division, 2) ;
                "^"  , (`Pow, 2) ;
                "^." , (`Pow, 2) ;
                "mod", (`Mod, 2) ;
                "negate" , (`Minus, 1) ;
                "negatef", (`Minus, 1) ;
                "^^", (`Plus, 2) (* it's bit of a stretch to label
                                                    string concatenation as "arithmetic" *)
              ]

          let compile op args =
            let (symbol, arity) = StringMap.find op builtins in
            if arity > 1 then
              match symbol with
              | `Division -> apply (prim "_Arith.divide") args
              | _ ->
                 let (x, y) = List.(hd args, hd (tl args)) in
                 binary symbol x y
            else
              match symbol with (* TODO rather than having arithmetic / logical distinction between operators, split them into binary and unary *)
              | `Minus ->
                 unary `Minus (List.hd args)
              | _ -> assert false
        end)

  module Logical
    = Make(struct
          type u = binary_op
          let js_primitive = true
          let standalone _ = failwith "Unsupported"
          let builtins =
            StringMap.from_alist
              [ "==" , (`StrictEq , 2) ;
                "<>" , (`StrictNeq, 2) ;
                "<"  , (`Lt       , 2) ;
                ">"  , (`Gt       , 2) ;
                "<=" , (`Le       , 2) ;
                ">=" , (`Ge       , 2) ;
                "intEq", (`StrictEq, 2) ;
                "stringEq", (`StrictEq, 2) ;
                "floatEq", (`StrictEq, 2) ;
                "floatNotEq", (`StrictNeq, 2) ;
                "objectEq", (`StrictEq, 2) ;
              ]

          let compile op args =
            let (symbol, _) = StringMap.find op builtins in
            match symbol with
            | `StrictEq when op = "==" || op = "objectEq" ->
               apply (prim "LINKS.eq") args
            | `StrictNeq when op = "<>" ->
               unary `Not (apply (prim "LINKS.eq") args)
            | _ ->
               let (x, y) = List.(hd args, hd (tl args)) in
               binary symbol x y
        end)

  let compilers : (module PRIMITIVE_COMPILER) list
    = [ (module Arithmetic : PRIMITIVE_COMPILER);
        (module Logical    : PRIMITIVE_COMPILER) ]

  let compile : string -> expr list -> expr option
    = fun name args ->
    let exception Impl of expr in
    try
      List.fold_left
        (fun () (module Compiler : PRIMITIVE_COMPILER) ->
          if Compiler.recognise name then
            raise (Impl (Compiler.compile name args))
          else ())
        () compilers;
      None
    with
    | Impl impl -> Some impl

  let standalone : string -> expr option
    = fun name ->
    let exception Impl of expr in
    try
      List.fold_left
        (fun () (module Compiler : PRIMITIVE_COMPILER) ->
          if Compiler.recognise name then
            raise (Impl (Compiler.maybe_eta name))
          else ())
        () compilers;
      None
    with
    | Impl impl -> Some impl
end

(* module Arithmetic :
 * sig
 *   val is : string -> bool
 *   val gen : (code * string * code) -> code
 * end =
 * struct
 *   let builtin_ops =
 *     StringMap.from_alist
 *       [ "+",   Some "+"  ;
 *         "+.",  Some "+"  ;
 *         "-",   Some "-"  ;
 *         "-.",  Some "-"  ;
 *         "*",   Some "*"  ;
 *         "*.",  Some "*"  ;
 *         "/",   None      ;
 *         "^",   None      ;
 *         "^.",  None      ;
 *         "/.",  Some "/"  ;
 *         "mod", Some "%"  ]
 * 
 *   let is x = StringMap.mem x builtin_ops
 *   let js_name op = val_of (StringMap.find op builtin_ops)
 *   let gen (l, op, r) =
 *     match op with
 *       | "/" -> Call (Var "Math.floor", [Binop (l, "/", r)])
 *       | "^" -> Call (Var "Math.floor", [Call (Var "Math.pow", [l; r])])
 *       | "^." -> Call (Var "Math.pow", [l; r])
 *       | _ -> Binop(l, js_name op, r)
 * end
 * 
 * module StringOp :
 * sig
 *   val is : string -> bool
 *   val gen : (code * string * code) -> code
 * end =
 * struct
 *   let builtin_ops =
 *     StringMap.from_alist
 *       [ "^^",  Some "+"  ]
 * 
 *   let is x = StringMap.mem x builtin_ops
 *   let js_name op = val_of (StringMap.find op builtin_ops)
 *   let gen (l, op, r) =
 *     Binop(l, js_name op, r)
 * end
 * 
 * module Comparison :
 * sig
 *   val is : string -> bool
 *   val js_name : string -> string
 *   val gen : (code * string * code) -> code
 * end =
 * struct
 *   (\* these names should be used for non-primitive types *\)
 *   let funs =
 *     StringMap.from_alist
 *       [ "==", "LINKS.eq"  ;
 *         "<>", "LINKS.neq" ;
 *         "<",  "LINKS.lt"  ;
 *         ">",  "LINKS.gt"  ;
 *         "<=", "LINKS.lte" ;
 *         ">=", "LINKS.gte" ]
 * 
 *   let is x = StringMap.mem x funs
 *   let js_name op = StringMap.find op funs
 *   let gen (l, op, r) =
 *     match op with
 *       | "<>" -> Unop("!", Call (Var "LINKS.eq", [l; r]))
 *           (\* HACK
 * 
 *              This is technically wrong, but as we haven't implemented
 *              LINKS.lt, etc., it is enough to get the demos working.
 * 
 *              Ideally we want to compile to the builtin operators
 *              whenever we know that the argument types are primitive
 *              and the general functions otherwise. This would
 *              necessitate making the JS compiler type-aware.
 *           *\)
 *       | "<" | ">" | "<=" | ">=" ->
 *           Binop(l, op, r)
 *       | _ ->  Call(Var (js_name op), [l; r])
 * end *)

(** [cps_prims]: a list of primitive functions that need to see the
    current continuation. Calls to these are translated in CPS rather than
    direct-style.  A bit hackish, this list. *)
let cps_prims = ["recv"; "sleep"; "spawnWait"; "receive"; "request"; "accept"]

(* (\** Generate a JavaScript name from a binder, wordifying symbolic names *\)
 * let name_binder (x, info) =
 *   let name = Js.name_binder (x, info) in
 *   if (name = "") then
 *     prerr_endline (Ir.show_binder (x, info))
 *   else
 *     ();
 *   assert (name <> "");
 *   (x, Js.name_binder (x,info)) *)

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
  (* Pure continuation augmentation *)
  val (&>) : Js.expr -> t -> t * js (* maybe don't return js, instead use bind at the calling site *)

  (* Installs a trap *)
  val install_trap : Js.expr * Js.expr -> t -> t

  (* Turns code into a continuation. *)
  val reflect : Js.expr -> t
  (* Turns a continuation into code. *)
  val reify   : t -> Js.expr

  (* Continuation name binding. *)
  val bind : t -> (t -> js) -> js

  (* Continuation application generation. *)
  val apply : t -> Js.expr -> Js.expr

  val trap : t -> Js.expr -> Js.expr

  (* Bypasses the current trap *)
  val forward : t -> Js.expr -> Js.expr

  (* Augments a function [Fn] with a continuation parameter and
     reflects the result as a continuation. The continuation parameter
     in the callback provides access to the current continuation. *)
  val contify_with_env : (t -> venv * Js.expr) -> venv * t

  val primitive_bindings : string
end

(* The standard Links continuation (no extensions) *)
module Default_Continuation : CONTINUATION = struct
  (* We can think of this particular continuation structure as
     singleton list. *)
  type t = Identity
         | Code of expr

  let identity = Identity
  let toplevel =
    let x, kappa = Ident.fresh_binder ~prefix:"_x" (), Ident.fresh_binder ~prefix:"_kappa" () in
    Code (fun_expr
            (fun_def
               [x; kappa]
               (lift_stmt (return (variable (Ident.to_var x))))))

  (* This continuation is a degenerate monoid. The multiplicative
     operator is "forgetful" in the sense that it ignores its second
     argument b whenever a != Identity. Meaning that a <> b == a when
     a != Identity. In other words, composition never yields an
     interesting new element. *)
  let (<>) a b =
    match a with
    | Identity -> b
    | Code _ -> a

  let (&>) f = function
    | Identity -> Code f, skip
    | Code expr -> assert false (* TODO *)

  let reflect x = Code x
  let rec reify = function
    | Identity -> reify toplevel
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
       let kappa = Ident.fresh_binder ~prefix:"_kappa" () in
       let stmts = body (reflect (variable (Ident.to_var kappa))) in
       const kappa (reify k) :: stmts

  let apply k arg = apply (reify k) [arg]

  let forward _ _ = assert false
  let trap _ _ = assert false
  let install_trap _ _ = assert false

  let contify_with_env fn = assert false
  (* match fn Identity with
   * | env, (Fn _ as k) -> env, reflect k
   * | _ -> failwith "error: contify: non-function argument." *)

  let primitive_bindings =
    "function _makeCont(k) { return k; }\n" ^
      "var _idy = function(x) { return; };\n" ^
        "var _applyCont = _applyCont_Default; var _yieldCont = _yieldCont_Default;\n" ^
          "var _cont_kind = \"Default_Continuation\";\n" ^
            "function is_continuation(value) {return value != undefined && (typeof value == 'function' || value instanceof Object && value.constructor == Function); }\n" ^
              "var receive = _default_receive;"
end

(* The higher-order continuation structure for effect handlers
   support *)
module Higher_Order_Continuation : CONTINUATION = struct
  (* We can think of this particular continuation structure as a
     nonempty stack with an even number of elements. *)
  type f = Dynamic of Js.expr
         | Static of Js.expr * Js.expr * Js.expr
  type t = Cons of f * t
         | Reflect of Js.expr
         | Identity

  open LispJs
  (* Auxiliary functions for manipulating the continuation stack *)
  let nil = prim "LINKEDLIST.Nil"
  let cons x xs = apply (prim "LINKEDLIST.Cons") [x; xs]
  let head xs = apply (prim "LINKEDLIST.head") [xs]
  let tail xs = apply (prim "LINKEDLIST.tail") [xs]
  let augment f kappa = apply (prim "_K.augment") [f; kappa]

  let toplevel =
    let x, ks = Ident.fresh_binder ~prefix:"x" (), Ident.fresh_binder ~prefix:"ks" () in
    Cons (Static
            (nil,
             fun_expr
               (fun_def
                  [x; ks]
                  (lift_stmt
                     (return (variable (Ident.to_var x))))),
             prim "_K.absurd"),
          Reflect nil)

  let make_frame pureFrames ret eff =
    apply (prim "_K.makeFrame") [pureFrames; ret; eff]

  let reflect x = Reflect x
  let rec reify kappa =
    match kappa with
    | Cons (Static (ks, ret, eff), vs) ->
       cons (make_frame ks ret eff) (reify vs)
    | Cons (Dynamic ks, vs) ->
       cons ks (reify vs)
    | Reflect v -> v
    | Identity -> reify toplevel

  let identity = Identity
  let (<>) a b =
    match a,b with
    | Identity, b -> b
    | a, Identity -> a
    | Reflect ks, b -> Cons (Dynamic ks, b)
    | Cons _ as a,b ->
       let rec append xs ys =
         match xs with
         | Cons (x, xs) -> Cons (x, append xs ys)
         | Reflect ks   -> Cons (Dynamic ks, ys)
         | Identity     -> ys
       in
       append a b

  let rec (&>) f = function
    | Cons (Static (ks, ret, eff), tail) ->
       let ks' = Ident.fresh_binder ~prefix:"_kappa" () in
       let decl = const ks' (cons f ks) in
       Cons (Static (variable (Ident.to_var ks'), ret, eff), tail), lift_stmt decl
    | Cons (Dynamic ks, tail) as v -> assert false (* TODO *)
    | Reflect v ->
       let kappa = Ident.fresh_binder ~prefix:"_kappa" () in
       let decl = const kappa (augment f v) in
       Reflect (variable (Ident.to_var kappa)), lift_stmt decl
    | Identity -> f &> toplevel

  let bind kappas (body : t -> js) =
    let rec bind : js -> (t -> js) -> t -> js
      = fun stmts body ->
      function
      | Identity ->
         let k = Ident.fresh_binder ~prefix:"_kappa" () in
         let stmts' = body (reflect (variable (Ident.to_var k))) in
         sequence stmts (const k (reify Identity) :: stmts')
      | Reflect (Var _) as k ->
         let stmts' = body k in
         sequence stmts stmts'
      | Reflect v ->
         let k = Ident.fresh_binder ~prefix:"_kappa" () in
         let stmts' = (const k v) :: body (reflect @@ (variable (Ident.to_var k))) in
         sequence stmts stmts'
      | Cons (Dynamic (Var _ as v), kappas) ->
         bind stmts (fun kappas -> body (Cons (Dynamic v, kappas))) kappas
      | Cons (Dynamic v, kappas) ->
         let k = Ident.fresh_binder ~prefix:"_kappa" () in
         let stmts' = (const k v) :: stmts in
         bind stmts' (fun kappas -> body (Cons (Dynamic (variable (Ident.to_var k)), kappas))) kappas
      | Cons (Static (ks, ret, eff), kappas) ->
         let k = Ident.fresh_binder ~prefix:"_kappa" () in
         let stmts' = const k (make_frame ks ret eff) :: stmts in
         bind stmts' (fun kappas -> body (Cons (Dynamic (variable (Ident.to_var k)), kappas))) kappas
    in
    bind [] body kappas

  let rec install_trap (ret, eff) = function
    | (Reflect _ as vs)
      | (Cons (_, _) as vs) ->
       Cons (Static (nil, ret, eff), vs)
    | Identity -> install_trap (ret, eff) toplevel

  let forward k z =
    apply (prim "_K.forward") [reify k; z]

  let trap k arg =
    apply (prim "_K.trap") [reify k; arg]

  let apply k arg =
    apply (prim "_K.apply") [reify k; arg]

  let contify_with_env fn =
    let kappa = Ident.fresh_binder ~prefix:"_kappa" () in
    match fn (reflect (variable (Ident.to_var kappa))) with
    | env, Func def -> env, reflect (Func { def with params = def.params @ [kappa] })
    | _ -> failwith "error: contify: none function argument."

  let primitive_bindings =
    "function _makeCont(k) {\n" ^
      "  return _Cons(k, _singleton(_efferr));\n" ^
        "}\n" ^
          "var _idy = _makeCont(function(x, ks) { return; }); var _idk = function(x,ks) { };\n" ^
            "var _applyCont = _applyCont_HO; var _yieldCont = _yieldCont_HO;\n" ^
              "var _cont_kind = \"Higher_Order_Continuation\";\n" ^
                "function is_continuation(kappa) {\n" ^
                  "return kappa !== null && typeof kappa === 'object' && _hd(kappa) !== undefined && _tl(kappa) !== undefined;\n" ^
                    "}\n"
end

(** Compiler interface *)
(* module type JS_PAGE_COMPILER = sig
 *   (\* include JS_COMPILER *\)
 *   val generate_program : venv -> Ir.computation -> venv * js
 *   val generate_stubs : Value.env -> Ir.binding list -> js -> js
 *   val generate_toplevel_bindings : Value.env -> Json.json_state -> venv -> Ir.binding list -> Json.json_state * venv * string list * js
 *   val wrap_with_server_lib_stubs : js -> js
 *   val primitive_bindings : string
 * end *)

(** [generate]
    Generates JavaScript code for a Links expression

    With CPS transform, result of generate is always of type : (a -> w) -> b
*)
module CPS_Compiler: functor (K : CONTINUATION) -> sig
  include JS_PAGE_COMPILER
end = functor (K : CONTINUATION) -> struct
  type continuation = K.t

  let __kappa = Ident.of_string "__kappa"

  let rec apply : ?strategy:[`Direct | `Yield] -> ?kappa:continuation option -> expr -> expr list -> expr
    = fun ?(strategy=`Yield) ?(kappa = None) f args ->
    match strategy, kappa with
    | `Direct, Some kappa ->
       LispJs.apply f (args @ [K.reify kappa])
    | `Direct, None       ->
       LispJs.apply f args
    | `Yield, kappa ->
       let g =
         thunk
           (lift_stmt
              (return (apply ~strategy:`Direct ~kappa f args)))
       in
       LispJs.apply (prim "_yield") [g]

  let apply_prim : string -> expr list -> expr
    = fun prim_name args ->
    apply ~strategy:`Direct (prim prim_name) args

  let contify fn =
    snd @@ K.contify_with_env (fun k -> VEnv.empty, fn k)

  let rec generate_value env : Ir.value -> expr =
    let gv v = generate_value env v in
    function
    | `Constant c ->
       begin
         match c with
         | `Int v  -> lit (Int v)
         | `Float v    -> lit (Float v)
         | `Bool v  -> lit (Bool v)
         | `Char v     -> chrlit v
         | `String v   -> chrlistlit v
       end
    | `Variable var ->
       let name = VEnv.lookup env var in
       begin match Primitive_Compiler.standalone name with
       | Some impl -> impl (* TODO: kify *)
       | None -> variable (Ident.of_var var)
       end
    | `Extend (field_map, rest) ->
       let dict =
         obj
           (StringMap.fold
              (fun name v dict ->
                (name, gv v) :: dict)
              field_map [])
       in
       begin
         match rest with
         | None -> dict
         | Some v ->
            apply_prim "LINKS.union" [gv v; dict]
       end
    | `Project (name, v) ->
       apply_prim "LINKS.project" [gv v; strlit name]
    | `Erase (names, v) ->
       apply_prim "LINKS.erase" [gv v; array (List.map strlit (StringSet.elements names))]
    | `Inject (name, v, _t) ->
       obj [("_label", strlit name); ("_value", gv v)]
      (* erase polymorphism *)
    | `TAbs (_, v)
    | `TApp (v, _) -> gv v

    | `XmlNode (name, attributes, children) ->
       generate_xml env name attributes children

    | `ApplyPure (f, vs) ->
       let f = strip_poly f in
       let args = List.map gv vs in
       begin
         match f with
         | `Variable f ->
            let f_name = VEnv.lookup env f in
            begin match Primitive_Compiler.compile f_name args with
            | Some impl -> impl
            | None ->
               if Lib.is_primitive f_name
                  && not (List.mem f_name cps_prims)
                  && Lib.primitive_location f_name <> `Server
               then
                 apply ~strategy:`Direct (prim ("_" ^ f_name)) args
               else
                 apply ~strategy:`Direct (gv (`Variable f)) args
            end
         | _ ->
            apply (gv f) args
       end
    | `Closure (f, v) ->
       let prim_name =
         if session_exceptions_enabled
         then "partialApplySE"
         else "partialApply"
       in
       apply ~strategy:`Direct (prim prim_name) [gv (`Variable f); gv v]
    | `Coerce (v, _) -> gv v

  and generate_xml env tag attrs children =
    apply_prim
      "LINKS.XML"
      [ strlit tag;
        obj (StringMap.fold (fun name v bs ->
                 (name, generate_value env v) :: bs) attrs []);
        array (List.map (generate_value env) children) ]

  let generate_remote_call f_var xs env =
    apply ~strategy:`Direct
      (apply_prim
         "LINKS.remoteCall"
         [variable (Ident.to_var __kappa)])
      [intlit (Ident.to_raw_var f_var);
       env;
       obj (
           List.map2
             (fun n v -> string_of_int n, variable (Ident.to_var v))
             (Utility.fromTo 1 (1 + List.length xs))
             xs
         )]

(** Generate stubs for processing functions serialised in remote calls *)
  module GenStubs =
  struct
    let rec fun_def : Ir.fun_def -> js =
      fun ((fb, (_, bs, _), zb, location) : Ir.fun_def) ->
      let fb = Ident.of_binder fb in
      let bs = List.map Ident.of_binder bs in
      let bs', env =
        match zb with
        | None -> bs, obj []
        | Some _ ->
           let envb = Ident.fresh_binder ~prefix:"_env" () in
           bs @ [envb], variable (Ident.to_var envb)
      in
      (* this code relies on eta-expanding functions in order to take
         advantage of dynamic scoping *)
        match location with
        | `Client | `Native | `Unknown ->
           lift_stmt
             (fun_decl
                (LispJs.fun_def
                   ~name:fb
                   (bs' @ [__kappa])
                   (lift_stmt
                      (return
                         (apply
                            ~strategy:`Direct
                            (variable (Ident.to_var fb))
                            (List.map (fun b -> variable (Ident.to_var b)) bs)))))) (* TODO: there's something dubious about this code (~name:fb and apply fb) *)
        | `Server ->
           lift_stmt
             (fun_decl
                (LispJs.fun_def
                   ~name:fb
                   (bs' @ [__kappa])
                   (lift_stmt @@ return (generate_remote_call (Ident.to_var fb) bs env))))
    and binding : Ir.binding -> js = function
      | `Fun def ->
         fun_def def
      | `Rec defs ->
         List.fold_right
           sequence
           (List.map (fun_def) defs)
           skip
      | _ -> skip
    and bindings : Ir.binding list -> js =
      fun bindings ->
      List.fold_right
        sequence
        (List.map binding bindings)
        skip

(* FIXME: this code should really be merged with the other
   stub-generation code and we should generate a numbered version of
   every library function.
*)

(** stubs for server-only primitives *)
    let wrap_with_server_lib_stubs : js -> js = fun code ->
      let server_library_funcs =
        List.rev
          (Env.Int.fold
             (fun var _v funcs ->
               let prim_name = Lib.primitive_name var in
               if Lib.primitive_location prim_name = `Server then
                 (prim_name, var)::funcs
               else
                 funcs)
             (Lib.value_env) [])
      in
      let prim_server_calls =
        concat_map (fun (prim_name, var) ->
          match Lib.primitive_arity prim_name with
          | None -> []
          | Some arity ->
             let params = generate_formal_params arity in
             [(Ident.of_string prim_name, params, generate_remote_call (Ident.of_var var) params (obj []))])
          server_library_funcs
      in
      List.map
        (fun (name, args, body) ->
          fun_decl
            (LispJs.fun_def ~name args (lift_stmt @@ return body)))
        prim_server_calls
  end

  let rec generate_tail_computation env : Ir.tail_computation -> continuation -> js =
    fun tc kappa ->
      let gv v = generate_value env v in
      let gc c kappa = snd (generate_computation env c kappa) in
      match tc with
      | `Return v ->
         lift_stmt (return (K.apply kappa (gv v)))
      | `Apply (f, vs) ->
         let f = strip_poly f in
         let args = List.map gv vs in
         begin
           match f with
           | `Variable f ->
              let f_name = VEnv.lookup env f in
              begin match Primitive_Compiler.compile f_name args with
              | Some impl ->
                 lift_stmt
                   (return (K.apply kappa impl))
              | None ->
                 if Lib.is_primitive f_name
                    && not (List.mem f_name cps_prims)
                    && Lib.primitive_location f_name <> `Server
                 then
                   let arg =
                     let f' = prim (Printf.sprintf "_%s" f_name) in
                     apply
                       ~strategy:`Direct
                       f'
                       args
                   in
                   lift_stmt
                     (return (K.apply kappa arg))
                 (* else
                  *   if (f_name = "receive" && session_exceptions_enabled) then
                  *     let code_vs = List.map gv vs in
                  *     let action cancel_thunk =
                  *       apply_yielding (Var f_name) (code_vs @ [cancel_thunk]) kappa in
                  *     generate_cancel_stub env action kappa *)
                   else
                     lift_stmt
                       (return (apply (gv (`Variable f)) (List.map gv vs @ [K.reify kappa])))
              end
           | _ ->
              lift_stmt
                (return (apply (gv f) (List.map gv vs @ [K.reify kappa])))
         end
      | `Special special ->
         generate_special env special kappa
      | `Case (scrutinee, cases, default) ->
         let scrutinee = gv scrutinee in
         let k, scrutinee' =
           match scrutinee with
           | Var x -> (fun s -> lift_stmt s), variable x
           | _ ->
              let xb = Ident.fresh_binder ~prefix:"scrutinee" () in
              (fun (s : stmt) -> sequence (lift_stmt @@ const xb scrutinee) (lift_stmt s)), variable (Ident.to_var xb)
         in
         K.bind kappa
           (fun kappa ->
             let gen_cont scrutinee (xb, comp) =
               let xb'     = Ident.of_binder xb in
               let x_name  = Ident.name_binder xb' in
               let env'    = VEnv.bind env (Var.var_of_binder xb, x_name) in
               let _, body = generate_computation env' comp kappa in
               let binding = const xb' scrutinee in
               sequence (lift_stmt binding) body
             in
             let cases =
               List.rev (StringMap.fold
                 (fun name body acc ->
                   let body' = gen_cont scrutinee' body in
                   (name, body') :: acc
                 ) cases [])
             in
             let default = opt_map (gen_cont scrutinee') default in
             let expr = (project scrutinee' "_label") in
             k (switch ?default expr cases))
      | `If (v, c1, c2) ->
         K.bind kappa
           (fun kappa ->
             lift_stmt (ifthenelse (gv v) (gc c1 kappa) (gc c2 kappa)))

  and generate_special env : Ir.special -> continuation -> js
    = fun sp kappa ->
    let gv v = generate_value env v in
    let die msg =
      apply ~strategy:`Direct (prim "_error") [lit (String msg)]
    in
    let return e = lift_stmt (return e) in
      match sp with
      | `Wrong _ ->
         return (die "Internal Error: Pattern matching failed") (* THIS MESSAGE SHOULD BE MORE INFORMATIVE *)
      | `Database _ | `Table _
          when Settings.get_value js_hide_database_info ->
         return (K.apply kappa (obj []))
      | `Database v ->
         return (K.apply kappa (obj [("_db", gv v)]))
      | `Table (db, table_name, keys, (readtype, _writetype, _needtype)) ->
         return
           (K.apply kappa
              (obj [("_table",
                     obj [("db", gv db);
                          ("name", gv table_name);
                          ("keys", gv keys);
                          ("row",
                           strlit (Types.string_of_datatype (readtype)))])]))
      | `Query _ ->
         return (die "Attempt to run a query on the client")
      | `Update _ ->
         return (die "Attempt to run a database update on the client")
      | `Delete _ ->
         return (die "Attempt to run a database delete on the client")
              | `LensSelect _ | `LensJoin _ | `LensDrop _ | `Lens _ ->
              (* Is there a reason to not use js_hide_database_info ? *)
              K.apply kappa (Dict [])
      | `LensGet _ | `LensPut _ -> Die "Attempt to run a relational lens operation on client"
      | `Query _ -> Die "Attempt to run a query on the client"
      | `Update _ -> Die "Attempt to run a database update on the client"
      | `Delete _ -> Die "Attempt to run a database delete on the client"
      | `CallCC v ->
         K.bind kappa
           (fun kappa ->
             return
               (apply (gv v) [K.reify kappa; K.reify kappa]))
      | `Select (l, c) ->
         let arg =
           (apply
              ~strategy:`Direct
              (prim "_send")
              [obj ["_label", strlit l; "_value", obj []]; gv c])
         in
         return (K.apply kappa arg)
      | `Choice (c, bs) -> assert false (* TODO *)
         (* let result = Ident.fresh_binder ~prefix:"_result" () in
          * let received = Ident.fresh_binder ~prefix:"_received" () in
          * let bind, skappa, skappas = K.pop kappa in
          * let skappa' =
          *   contify (fun kappa ->
          *     let scrutinee = apply_prim "LINKS.project" [Var result; strlit "1"] in
          *     let channel = apply_prim "LINKS.project" [Var result; strlit "2"] in
          *     let generate_branch (cb, b) =
          *       let (c, cname) = name_binder cb in
          *       cname, Bind (cname, channel, snd (generate_computation (VEnv.bind env (c, cname)) b K.(skappa <> kappa))) in
          *     let branches = StringMap.map generate_branch bs in
          *     Fn ([result], (Bind (received, scrutinee, (Case (received, branches, None)))))) in
          * 
          * let cont = K.(skappa' <> skappas) in
          * if (session_exceptions_enabled) then
          *   let action cancel_stub =
          *     Call (Var "receive", [gv c; cancel_stub; K.reify cont]) in
          *   bind (generate_cancel_stub env action cont)
          * else
          *   bind (Call (Var "receive", [gv c; K.reify cont])) *)
      | `DoOperation (name, args, _) -> (* TODO session exceptions *)
         let box = function
           | [v] -> gv v
           | vs -> obj (List.mapi (fun i v -> (string_of_int @@ i + 1, gv v)) vs)
         in
         let op =
           obj [ ("_label", strlit name)
               ; ("_value", obj [("p", box args); ("s", prim "LINKEDLIST.Nil")]) ]
         in
         return (K.trap kappa op)
         (* let maybe_box = function
          *   | [v] -> gv v
          *   | vs -> Dict (List.mapi (fun i v -> (string_of_int @@ i + 1, gv v)) vs)
          * in
          * let cons k ks =
          *   Call (Var "_Cons", [k;ks])
          * in
          * let nil = Var "Nil" in
          * K.bind kappa
          *   (fun kappas ->
          *     (\* kappa -- pure continuation *\)
          *     let bind_skappa, skappa, kappas = K.pop kappas in
          *     (\* eta -- effect continuation *\)
          *     let bind_seta, seta, kappas   = K.pop kappas in
          *     let resumption = K.(cons (reify seta) (cons (reify skappa) nil)) in
          * 
          *     (\* Session exceptions arising as a result of "raise" (i.e. those without an
          *      * environment passed as an argument already) need to be compiled specially *\)
          *     let op =
          *       if (session_exceptions_enabled &&
          *             name = Value.session_exception_operation &&
          *           List.length args = 0) then
          *         let affected_variables =
          *           VariableInspection.get_affected_variables (K.reify kappa) in
          *         let affected_arr = Dict ([("1", Arr affected_variables)]) in
          *         Dict [ ("_label", strlit name)
          *              ; ("_value", Dict [("p", affected_arr); ("s", resumption)]) ]
          *       else
          *         Dict [ ("_label", strlit name)
          *              ; ("_value", Dict [("p", maybe_box args); ("s", resumption)]) ]
          *     in
          *     bind_skappa (bind_seta (apply_yielding (K.reify seta) [op] kappas))) *)
      | `Handle { Ir.ih_comp = comp; Ir.ih_return = val_case; Ir.ih_cases = eff_cases; Ir.ih_depth = depth } ->
         (* Generate body *)
         let gb env binder body kappas =
           let var = match Ident.to_var binder with
             | `Var v -> v
             | `Primitive _ -> assert false
           in
           let env' = VEnv.bind env (var, Ident.name_binder binder) in
           snd (generate_computation env' body kappas)
         in
         let val_case_decl, val_case =
           let (xb, body) = val_case in
           let xb = Ident.of_binder xb in
           let _fs = Ident.of_string "_fs" in
           let val_case_b = Ident.fresh_binder ~prefix:"_return" () in
           let val_case_decl =
             const
               val_case_b
               (fun_expr
                  (fun_def
                     [xb; _fs]
                     (gb env xb body K.(reflect (variable (Ident.to_var _fs))))))
           in
           val_case_decl, variable (Ident.to_var val_case_b)
         in
         let eff_decl, eff =
           (* Generate clause *)
           let gc env (xb, rb, body) kappas _z =
             let xb' = Ident.of_binder xb in
             let rb' = Ident.of_binder rb in
             let env' =
               let env = VEnv.bind env (Var.var_of_binder xb, Ident.name_binder xb') in
               VEnv.bind env (Var.var_of_binder rb, Ident.name_binder rb')
             in
             let vb, v = Ident.fresh_binder ~prefix:"_v" (), project _z "_value" in
             let p = project (variable (Ident.to_var vb)) "p" in
             let s = project (variable (Ident.to_var vb)) "s" in
             (* TODO shallow / deep distinction *)
             let r =
               let e = match depth with
                 | `Shallow -> Ident.of_string "_K.shallowResume" (* TODO make continuation operation? *)
                 | `Deep [] -> Ident.of_string "_K.deepResume"
                 | `Deep _ -> failwith "Parameterised handlers not yet supported."
               in
               apply (variable (Ident.to_var e)) [s]
             in
             let stmts = gb env' xb' body kappas in
             (const vb v) :: (const rb' r) :: (const xb' p) :: stmts
           in
           let eff_b = Ident.fresh_binder ~prefix:"_eff" () in
           let eff_decl =
             const
               eff_b
               (let _z = Ident.of_string "_z" in
                let _fs = Ident.of_string "_fs" in
                fun_expr
                  (fun_def
                     [_z; _fs]
                     (let _z = variable (Ident.to_var _z) in
                      let _fs = K.reflect (variable (Ident.to_var _fs)) in
                      let forward = Some (lift_stmt (LispJs.return (K.forward _fs _z))) in
                      let cases =
                        List.rev
                          (StringMap.fold
                             (fun label case acc ->
                               let case' = gc env case _fs _z in
                               (label, case') :: acc)
                             eff_cases [])
                      in
                      lift_stmt (switch ?default:forward (project _z "_label") cases))))
           in
           eff_decl, variable (Ident.to_var eff_b)
         in
         let kappa = K.install_trap (val_case, eff) kappa in
         let _, stmts = generate_computation env comp kappa in
         val_case_decl :: eff_decl :: stmts

         (* let comp_env = env in
          * let cons v vs =
          *   Call (Var "_Cons", [v; vs])
          * in
          * let project record label =
          *   Call (Var "LINKS.project", [record; label])
          * in
          * let vmap r y =
          *   Call (Var "_vmapOp", [r; y])
          * in
          * let generate_body env binder body kappas =
          *   let env' = VEnv.bind env binder in
          *   snd (generate_computation env' body kappas)
          * in
          * begin match depth with
          * | `Shallow -> failwith "CPS compilation of shallow handlers is currently not supported"
          * | `Deep params ->
          *    let translate_parameters params =
          *      let is_parameterised = List.length params > 0 in
          *      let param_ptr_binder =
          *        Var.fresh_binder (Var.make_local_info (`Not_typed, "_param_ptr"))
          *      in
          *      let env = VEnv.bind env (name_binder param_ptr_binder) in
          *      let params =
          *        List.mapi (fun i (binder,initial_value) -> (i, binder, initial_value)) params
          *      in
          *      let ptr = `Variable (Var.var_of_binder param_ptr_binder) in
          *      let initial_parameterise (bs, tc) =
          *        let name_map =
          *          List.fold_left
          *            (fun box (i, _, initial_value) ->
          *              StringMap.add (string_of_int i) initial_value box)
          *            StringMap.empty params
          *        in
          *        (`Let (param_ptr_binder, ([], `Return (`Extend (name_map, None)))) :: bs, tc)
          *      in
          *      let parameterise body =
          *      (\* The pointer points to the box containing the parameters *\)
          *        List.fold_right
          *          (fun (i, binder, _) (bs,tc) ->
          *            let b =
          *              `Let (binder, ([], `Return (`Project (string_of_int i, ptr))))
          *            in
          *            (b :: bs, tc))
          *          params body
          *      in
          *      let make_resumption s =
          *        if is_parameterised
          *        then Call (Var "_make_parameterised_resumption", [Var (snd @@ name_binder param_ptr_binder); s])
          *        else Call (Var "_make_resumption", [s])
          *      in
          *      env, comp, make_resumption, initial_parameterise, parameterise
          *    in
          *    let env, comp, make_resumption, initial_parameterise, parameterise = translate_parameters params in
          *    let value_case =
          *      let (xb, body) = return in
          *      let xb, x_name = name_binder xb, snd @@ name_binder xb in
          *      K.reflect
          *        (Fn ([x_name; "ks"],
          *             let bind, _, kappa = K.(reflect ->- pop) (Var "ks") in
          *             let body = parameterise body in
          *             bind @@ generate_body env xb body kappa))
          *    in
          *    let eff_cases =
          *      let translate_eff_case env (xb, resume, body) kappas =
          *        let xb = name_binder xb in
          *        let resume = name_binder resume in
          *        let p = project (Var (snd xb)) (strlit "p") in
          *        let r =
          *          let s = project (Var (snd xb)) (strlit "s") in
          *          make_resumption s
          *        in
          *        let env' = VEnv.bind env resume in
          *        let body = generate_body env' xb (parameterise body) kappas in
          *        snd xb, Bind (snd resume, r,
          *                      Bind (snd xb, p, body))
          *      in
          *      let translate_exn_case env (xb, body) kappas =
          *        let xb = name_binder xb in
          *        let dummy_var_name = gensym ~prefix:"dummy" () in
          *        let x_name = snd xb in
          *        let p = project (Var x_name) (strlit "p") in
          *        snd xb, Bind (x_name, project p (strlit "1"),
          *                (\* FIXME: the following call should
          *                   probably be apply_yielding rather than a
          *                   raw Call because _handleSessionException
          *                   makes use of the call stack. *\)
          *                      Bind (dummy_var_name, Call (Var "_handleSessionException", [Var x_name]),
          *                            generate_body env xb (parameterise body) kappas))
          *      in
          *      let eff_cases kappas =
          *        StringMap.fold
          *          (fun operation_name clause cases ->
          *            StringMap.add operation_name
          *              (if session_exceptions_enabled && operation_name = Value.session_exception_operation
          *               then let (xb,_,body) = clause in translate_exn_case env (xb, body) kappas
          *               else translate_eff_case env clause kappas)
          *              cases)
          *          eff_cases StringMap.empty
          *      in
          *      let forward y ks =
          *        K.bind ks
          *          (fun ks ->
          *            let bind_k'_ks', k', ks' = K.pop ks in
          *            let bind_h'_ks'', h', ks'' = K.pop ks' in
          *            let bind code = bind_k'_ks' (bind_h'_ks'' code) in
          *            let resumption =
          *              Fn (["s"], Return (cons (K.reify h')
          *                                   (cons (K.reify k') (Var "s"))))
          *            in
          *            bind (apply_yielding (K.reify h') [vmap resumption y] ks''))
          *      in
          *      K.reflect
          *        (Fn (["_z"; "ks"],
          *             let ks = K.reflect (Var "ks") in
          *             Case ("_z",
          *                   eff_cases ks,
          *                   Some ("_z", forward (Var "_z") ks))))
          *    in
          *    let kappa = K.(value_case <> eff_cases <> kappa) in
          *    snd (generate_computation comp_env (initial_parameterise comp) kappa)
          * end *)

  and generate_computation env : Ir.computation -> continuation -> (venv * js) =
    fun (bs, tc) kappa ->
      let rec gbs : venv -> continuation -> Ir.binding list -> venv * js =
        fun env kappa ->
          function
          | `Let (b, (_, `Return v)) :: bs ->
             let xb' = Ident.of_binder b in
             let env', rest = gbs (VEnv.bind env (Var.var_of_binder b, Ident.name_binder xb')) kappa bs in
             (env', const xb' (generate_value env v) :: rest)
          | `Let (b, (_, tc)) :: bs ->
             let xb' = Ident.of_binder b in
             let _fs = Ident.fresh_binder ~prefix:"_fs" () in
             let env', body =
               gbs
                 (VEnv.bind env (Var.var_of_binder b, Ident.name_binder xb'))
                 K.(reflect (variable (Ident.to_var _fs)))
                 bs
             in
             let f =
               fun_expr (fun_def [xb'; _fs] body)
             in
             let kappa', stmts = K.(f &> kappa) in
             let stmts' = generate_tail_computation env tc kappa' in
             env', stmts @ stmts'
          | `Fun ((fb, _, _zs, _location) as def) :: bs ->
             let fb' = Ident.of_binder fb in
             let fun_def = generate_function env [] def in
             let env', rest = gbs (VEnv.bind env (Var.var_of_binder fb, Ident.name_binder fb')) kappa bs in
             (env', (fun_decl fun_def) :: rest)
          | `Rec defs :: bs ->
             let fbs = List.map (fun (fb, _, _, _) -> Ident.of_binder fb) defs in
             let env', rest =
               gbs
                 (List.fold_left
                    (fun env fb ->
                      let var = match Ident.to_var fb with
                        | `Primitive _ -> assert false
                        | `Var v -> v
                      in
                      VEnv.bind env (var, Ident.name_binder fb))
                    env fbs)
                 kappa bs
             in
             (env', (List.map (fun def -> fun_decl (generate_function env fbs def)) defs) @ rest)
          | `Module _ :: bs
          | `Alien _ :: bs -> gbs env kappa bs
          | [] -> (env, generate_tail_computation env tc kappa)
      in
      gbs env kappa bs

  and generate_function env fs : Ir.fun_def -> fun_def =
    fun (fb, (_, xsb, body), zb, location) ->
    let fb = Ident.of_binder fb in
    (* optionally add an additional closure environment argument *)
    let xsb =
      match zb with
      | None -> xsb
      | Some zb -> zb :: xsb
    in
    let bs = List.map Ident.of_binder xsb in
    let body_env =
      List.fold_left
        (fun env b ->
          let var = match Ident.to_var b with
            | `Var v -> v
            | `Primitive _ -> assert false
          in
          VEnv.bind env (var, Ident.name_binder b))
        env (fs @ bs)
    in
    let body =
      match location with
      | `Client | `Unknown ->
         snd (generate_computation body_env body (K.reflect (variable (Ident.to_var __kappa))))
      | `Server -> lift_expr (generate_remote_call (Ident.to_var fb) bs (obj []))
      | `Native -> failwith ("Not implemented native calls yet")
    in
    fun_def
    ~name:fb
     (bs @ [__kappa])
     body
  (* and generate_cancel_stub env (action: code -> code) (kappa: K.t)  =
   *   let open Pervasives in
   *   (\* Compile a thunk to be invoked if the operation fails *\)
   *   let cancellation_thunk_name =
   *     gensym ~prefix:"cancellation_thunk" () in
   *   let () = Debug.print ("Kappa in GCS: " ^ K.to_string kappa) in
   *   (\* Grab affected variables from the continuation *\)
   *   let affected_vars_name = gensym ~prefix:"affected_vars" () in
   *   let fresh_var = Var.fresh_raw_var () in
   *   let affected_variables =
   *      VariableInspection.get_affected_variables (K.reify kappa) in
   *   (\* Bind affected variables array to a fresh variable *\)
   *   let env = VEnv.bind env (fresh_var, affected_vars_name) in
   *   (\* Compile raise operation WRT reflected, bound continuation *\)
   *   let raiseOp =
   *     generate_special env (`DoOperation (
   *       Value.session_exception_operation,
   *       [`Variable fresh_var], `Not_typed)) in
   *   let fresh_kappa = gensym ~prefix:"kappa" () in
   *   let cancellation_thunk = Fn ([fresh_kappa], raiseOp (K.reflect (Var fresh_kappa))) in
   * 
   *   (\* Generate binding code *\)
   *   Bind (affected_vars_name, Arr affected_variables,
   *     Bind (cancellation_thunk_name, cancellation_thunk,
   *       action (Var cancellation_thunk_name))) *)


  let generate_toplevel_binding :
    Value.env ->
    Json.json_state ->
    venv ->
    Ir.binding ->
    Json.json_state * venv * Ident.binder option * js =

    fun valenv state varenv ->
    let json_parse e =
      apply (prim "JSON.parse") [e]
    in
      function
      | `Let (b, _) ->
         let b' = Ident.of_binder b in
      (* Debug.print ("let_binding: " ^ x_name); *)
         let varenv = VEnv.bind varenv (Var.var_of_binder b, Ident.name_binder b') in
         let value = Value.Env.find (Var.var_of_binder b) valenv in
         let jsonized_val = Json.jsonize_value value in
         let state = ResolveJsonState.add_value_information value state in
         (state,
          varenv,
          Some b',
          lift_stmt
            (const b' (json_parse (lit (String jsonized_val))))) (* TODO represent "json state" in the JS IR *)
      | `Fun ((fb, _, _zs, _location) as def) ->
         let fb' = Ident.of_binder fb in
         let varenv = VEnv.bind varenv (Var.var_of_binder fb, Ident.name_binder fb') in
         let def_header = generate_function varenv [] def in
         (state,
          varenv,
          None,
          lift_stmt (fun_decl def_header))
      | `Rec defs ->
         let fbs = List.map (fun (fb, _, _, _) -> Ident.of_binder fb) defs in
         let varenv =
           List.fold_left
             (fun env fb ->
               let var = match Ident.to_var fb with
                 | `Primitive _ -> assert false
                 | `Var v -> v
               in
               VEnv.bind env (var, Ident.name_binder fb))
            varenv fbs
         in
         let fun_defs =
           List.map (generate_function varenv fbs) defs
         in
         (state, varenv, None, List.map fun_decl fun_defs) (* TODO possibly inline `fun_decl` into the map above. *)
      | `Alien (bnd, raw_name, _lang) ->
         let b = Ident.of_binder bnd in
         let varenv = VEnv.bind varenv (Var.var_of_binder bnd, raw_name) in
         state, varenv, None, skip
      | `Module _ -> state, varenv, None, skip

  let rec generate_toplevel_bindings : Value.env -> Json.json_state -> venv -> Ir.binding list -> Json.json_state * venv * string list * js =
    fun valenv state venv ->
      function
      | []      -> state, venv, [], skip
      | b :: bs ->
         let state, venv, x, p = generate_toplevel_binding valenv state venv b in
         let state, venv, xs, q = generate_toplevel_bindings valenv state venv bs in
         let xs =
           match x with
           | None -> xs
           | Some x -> (Ident.name_binder x) :: xs in
         state, venv, xs, sequence p q

  let generate_stubs _venv bs = GenStubs.bindings bs
  let wrap_with_server_lib_stubs code = GenStubs.wrap_with_server_lib_stubs code

  let generate_program venv comp =
    generate_computation venv comp K.toplevel

  let primitive_bindings = K.primitive_bindings

  type source = [
    | `Bindings of Ir.binding list
    | `Program of Ir.program ]

  let compile : source -> Value.env -> Js.program
    = fun _ _ -> assert false
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
