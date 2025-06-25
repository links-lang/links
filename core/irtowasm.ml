open Utility
open CommonTypes
let internal_error message = Errors.internal_error ~filename:"irtowasm.ml" ~message

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
         | LitInt  of int
         | LitBool of bool
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

         | Nothing
         | Absurd
         [@@deriving show]

  module MetaContinuation = struct
    type nonrec t = (t -> t)
    let identity code = code
  end

  module ObjectContinuation = struct
    let __kappa = "__kappa"
  end

  (* TODO L1 *)
  module Constructors = struct
    (** Create a JS string literal, quoting special characters *)
    let string_js_quote s =
      String.escaped s
      |> Str.global_replace (Str.regexp "'") "\\'"
      |> Printf.sprintf "'%s'"

    (** Return a JS literal string from an OCaml int. *)
    let intlit i = LitInt i
    (** Return a JS literal string from an OCaml string. *)
    let strlit _ = Absurd
    (** Return a JS literal string from an OCaml character. *)
    let boollit b = LitBool b
    (** Return a JS literal string from an OCaml character. *)
    let floatlit _ = Absurd
    (** Return a JS literal string from an OCaml character. *)
    let chrlit _ = Absurd
    (** Return a literal for the JS representation of a Links string. *)
    let chrlistlit = strlit
  end
  (* TODO L1 *)
  module Runtime = struct

    module Wasm = struct
      let erase   = Var "_$Links.erase"
      let union   = Var "_$Links.union"
      let eq = Var "_$Links.eq"
      let neq = Var "_$Links.neq"
      let gt = Var "_$Links.gt"
      let lt = Var "_$Links.lt"
      let gte = Var "_$Links.gte"
      let lte = Var "_$Links.lte"
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

    (* TODO L1: bizarre que ce soit pas utilisé *)
    let set_of_array _ =
      Absurd

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



module type WASM_CODEGEN = sig
  val string_of_wasm : Code.t -> string
  val output : out_channel -> Code.t -> unit
end

(* TODO L1 *)
module Wasm_CodeGen : WASM_CODEGEN = struct
  (** Pretty printer for WASM code *)
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
            | LitInt _
            | LitBool _
            | Call _
            | Dict _
            | Arr _
            | Bind _
            | Return _
            | Project _
            | Nothing
            | Absurd as c -> show c
          | c -> parens (show c)
        in
        match c with
        | Var x -> PP.text x
        | Nothing -> PP.text ""
        | Absurd -> PP.text "" (* TODO L1: mettre un unreachable WASM *)
        | LitInt i -> PP.text ("(i64.const " ^ (string_of_int i) ^ ")\n")
        | LitBool b -> PP.text ("(i64.const " ^ (if b then "1" else "0") ^ ")\n")
        | LetFun ((name, vars, body, _location), rest) ->
           (show_func ~name:(`Name name) (Fn (vars, body))) ^^ break ^^ show rest
        | LetRec (defs, rest) ->
           PP.vsep (punctuate " " (List.map (fun (name, vars, body, _loc) -> show_func ~name:(`Name name) (Fn (vars, body))) defs)) ^^
             break ^^ show rest
        | Fn _ -> show_func c
        | Call (fn, args) ->
          maybe_parenthesise fn ^^ (PP.arglist (List.map show args))
        | Unop (op, body) -> PP.text op ^+^ (maybe_parenthesise body)
        | Binop (l, op, r) -> (maybe_parenthesise l) ^+^ (maybe_parenthesise r) ^+^ PP.text op
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

      let output oc = show ->- PP.out_pretty oc 144
      let show = show ->- PP.pretty 144
    end

  let string_of_wasm x = PP.show x
  let output oc code = PP.output oc code
end

(* Specialness:

   * Top-level boilerplate code to replace the root element and reset the focus

     The special function _start takes an html page as a string and
     replaces the currently displayed page with that one.

     Some of the other functions are equivalents to Links builtins
     (e.g. int_of_string)
 *)

module Arithmetic : sig
  val is : string -> bool
  val gen : string -> Code.t list -> Code.t
end = struct
  (* TODO L1 *)
  let builtin_binops =
    StringMap.from_alist
      [ "+",   Some "i64.add"  ;
        "+.",  Some "+"  ;
        "-",   Some "i64.sub"  ;
        "-.",  Some "-"  ;
        "*",   Some "i64.mul"  ;
        "*.",  Some "*"  ;
        "/",   Some "i64.div_s" ;
        "^",   None      ;
        "^.",  None      ;
        "/.",  Some "/"  ;
        "mod", Some "i64.mod_s"]

    (* TODO L1 *)
  let builtin_unops =
    [ "negate", "-" ;
      "negatef", "-" ]

  let is x = StringMap.mem x builtin_binops || List.mem_assoc x builtin_unops
  let wasm_name op = val_of (StringMap.find op builtin_binops)
  (* TODO L1 *)
  let gen op args =
    let open Code in
    match op, args with
      | "/", [l; r] -> Aux.call Runtime.Math.floor [Binop (l, "/", r)]
      | "^", [_; _] -> Aux.call Runtime.Math.floor [Call (Runtime.Math.pow, args)]
      | "^.", [_; _] -> Aux.call Runtime.Math.pow args
      | ("negate" | "negatef"), [_] -> Aux.call (Var "-") args
      | _, [l; r] -> Binop(l, wasm_name op, r)
      | _, _ -> raise (internal_error (Printf.sprintf "Unrecognised primitive arithmetic operation '%s' with arity %d\n" op (List.length args)))
end

(* TODO L1 *)
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
  let wasm_name op = val_of (StringMap.find op builtin_ops)
  let gen op args =
    let open Code in
    match args with
    | [l; r] -> Binop(l, wasm_name op, r)
    | _ -> raise (internal_error (Printf.sprintf "Unrecognised string operation '%s' with arity %d\n" op (List.length args)))
end

(* TODO L1 *)
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
      [ "==", Runtime.Wasm.eq ;
        "<>", Runtime.Wasm.neq;
        "<",  Runtime.Wasm.lt ;
        ">",  Runtime.Wasm.gt ;
        "<=", Runtime.Wasm.lte;
        ">=", Runtime.Wasm.gte ]

  let is x = StringMap.mem x funs
  let wasm_name op = StringMap.find op funs
  let gen op args =
    let open Code in
    match op, args with
      | "<>", [l; r] -> Unop("!", Aux.call Runtime.Wasm.eq [l; r])
      | ( "==" | "<" | ">" | "<=" | ">="), [l; r] -> Aux.call (wasm_name op) [l; r]
      | _, _ -> raise (internal_error (Printf.sprintf "Unrecognised relational operator '%s' with arity %d\n" op (List.length args)))
end

(* TODO L1 *)
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
let cps_prims = []

(** Generate a JavaScript name from a binder, wordifying symbolic names *)
(* TODO L1 *)
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

  val apply : t -> Code.MetaContinuation.t

  (* Augments a function [Fn] with a continuation parameter and
     reflects the result as a continuation. The continuation parameter
     in the callback provides access to the current continuation. *)
  val contify_with_env : (t -> venv * Code.t) -> venv * t

  (* Generates appropriate bindings for primitives *)
  val primitive_bindings : Code.t

  (* Generates a string dump of the continuation, for debugging purposes. *)
  val to_string : t -> string
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
                (* InlineJS {| _error("Unhandled operation `" + z._label + "'.") |})) *)
                Nothing))
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

  let apply k arg =
    let open Code in
    Aux.call (Var "_$K.apply") [reify k; arg]

  let primitive_bindings =
    (* Code.InlineJS ({|
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
    'idy': make(function(x, ks) { return; }),
    'make': make
  });
})();
|}) *)
  Code.Nothing

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
(* module type WASM_COMPILER = sig
 *   type source = [
 *     | `Bindings of Ir.binding list
 *     | `Program of Ir.program ]
 *
 *   val compile : source -> Value.env -> Js.program
 * end *)

module type WASM_PROG_COMPILER = sig
  (* include WASM_COMPILER *)
  val generate_program : venv -> Ir.computation -> venv * Code.t
  val primitive_bindings : Code.t
end

(** [generate]
    Generates JavaScript code for a Links expression

    With CPS transform, result of generate is always of type : (a -> w) -> b
*)
module CPS_Compiler: functor (K : CONTINUATION) -> sig
  include WASM_PROG_COMPILER
end = functor (K : CONTINUATION) -> struct
  type continuation = K.t

  open Code.Aux.List

  let call = Code.Aux.call
  let project = Code.Aux.project
  let return x = Code.Return x

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
        | Int v   -> intlit v
        | Float v -> floatlit v
        | Bool v   -> boollit v
        | Char v   -> chrlit v
        | String v -> chrlistlit v
        | DateTime _ -> Absurd
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
            call Runtime.Wasm.union [gv v; dict]
       end
    | Ir.Project (name, v) ->
       project (gv v) name
    | Ir.Erase (names, v) ->
       call Runtime.Wasm.erase
         [gv v; Aux.set_of_array (Arr (List.map strlit (StringSet.elements names)))]
    | Ir.Inject (name, v, _t) ->
       Dict [("_label", strlit name);
             ("_value", gv v)]

      (* erase polymorphism *)
    | Ir.TAbs (_, v)
    | Ir.TApp (v, _) -> gv v

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
      closure
    | Ir.Coerce (v, _) -> gv v
    | _ -> failwith "Not supported stuff"


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
              (* if Lib.is_primitive f_name
              && not (List.mem f_name cps_prims)
              && not (Location.is_server (Lib.primitive_location f_name))
              then *)
                let arg = call (Var ("_" ^ f_name)) (List.map gv vs) in
                return (K.apply kappa arg)
          end
        | _ ->
          return (call (gv f) ((List.map gv vs) @ [K.reify kappa]))
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
      | Ir.CallCC v ->
         K.bind kappa
           (fun kappa -> return (call (gv v) [K.reify kappa]))
      | Ir.Select (l, c) ->
         let arg = call (Var "_send") [Dict ["_label", strlit l; "_value", Dict []]; gv c] in
         return (K.apply kappa arg)
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

             let op =
                 Dict [ ("_label", strlit name)
                      ; ("_value", Dict [("p", maybe_box args); ("s", resumption)]) ]
             in
             bind_skappa (bind_seta (return (call (K.reify seta) [op; K.reify kappas]))))
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
              
              let eff_cases scrutinee kappas =
                StringMap.fold
                  (fun operation_name clause cases ->
                    StringMap.add operation_name
                      (translate_eff_case env scrutinee clause kappas)
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
                    bind (Aux.return (call (K.reify h') ([vmap resumption y] @ [K.reify ks'']))))
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
      | _ -> failwith "Not supported stuff"

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
      let (_, f_name) = name_binder fb in (* L1: Bizarre de pas utiliser le premier argument f *)
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
           snd (generate_computation body_env fn_body (K.reflect (Code.Var __kappa)))
      in
      (f_name,
       xs_names @ [__kappa],
       body,
       fn_location)

  let generate_program venv comp =
    let (env, code) = generate_computation venv comp K.toplevel in
    (env, Code.Aux.call (Code.Fn ([], code)) [])

  let primitive_bindings = K.primitive_bindings
end

module Continuation =
  (val (module Higher_Order_Continuation : CONTINUATION))

  module Compiler = CPS_Compiler(Continuation)
