let name = "Irtocps"

open Utility
open CommonTypes
let internal_error message = Errors.internal_error ~filename:"irtocps.ml" ~message

(* strip any top level polymorphism from an expression *)
let rec strip_poly =
  function
    | Ir.TAbs (_, e)
    | Ir.TApp (e, _) -> strip_poly e
    | e -> e

module Code = struct
  module MetaContinuation = struct
    type nonrec t = (Ir.value -> Ir.tail_computation)
    let identity code = Ir.Return code
  end

  module ObjectContinuation = struct
    let __kappa_bind, __kappa = Var.fresh_var (Var.make_global_info (Types.Not_typed, "__kappa"))
  end

  module RuntimeList = struct
    let nil = Ir.Variable (Env.String.find "Nil" Lib.nenv)
  end

  module Aux = struct
    let apply f args = Ir.Apply (f, args)
    let apply_pure f args = Ir.ApplyPure (f, args)

    let project record label =
      Ir.Project (record, label)

    let return exp = Ir.Return exp

    let value_placeholder = Ir.Constant (CommonTypes.Constant.Int 666)
    let tail_comp_placeholder = Ir.Special (Ir.Wrong Types.Not_typed)

    (* TODO L1: change this to an error, this is just a placeholder *)
    let die _ =
      value_placeholder



    module List = struct
      let cons x xs =
        Ir.ApplyPure (Ir.Variable (Env.String.find "Cons" Lib.nenv), [x; xs])
      let head xs =
        apply (Ir.Variable (Env.String.find "hd" Lib.nenv)) [xs]
      let tail xs =
        apply (Ir.Variable (Env.String.find "tl" Lib.nenv)) [xs]
    end
  end
end

(** IR variable environment *)
module VEnv = Env.Int

(** Type of environments mapping IR variables to source variables and their types *)
type venv = (string * Types.typ) VEnv.t

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


(* Specialness:

   * Top-level boilerplate code to replace the root element and reset the focus

     The special function _start takes an html page as a string and
     replaces the currently displayed page with that one.

     Some of the other functions are equivalents to Links builtins
     (e.g. int_of_string)
 *)

module Arithmetic : sig
  val is : string -> bool
  val gen : string -> Ir.value list -> Ir.value
end = struct
  let builtin_binops =
      [ "+" ;
        "+.";
        "-" ;
        "-.";
        "*" ;
        "*.";
        "/" ;
        "^" ;
        "^.";
        "/.";
        "mod"]

  let builtin_unops =
    [ "negate";
      "negatef"]

  let is x = List.mem x builtin_binops || List.mem x builtin_unops

  let gen op args =
    let open Code in
    match op, args with
      | _, [_; _] when List.mem op builtin_binops -> Aux.apply_pure (Ir.Variable (Env.String.find op Lib.nenv)) args
      | _, [_] when List.mem op builtin_unops -> Aux.apply_pure (Ir.Variable (Env.String.find op Lib.nenv)) args
      | _, _ -> raise (internal_error (Printf.sprintf "Unrecognised primitive arithmetic operation '%s' with arity %d\n" op (List.length args)))
end


module StringOp :
sig
  val is : string -> bool
  val gen : string -> Ir.value list -> Ir.value
end =
struct
  let builtin_ops =
      [ "^^"]

  let is x = List.mem x builtin_ops

  let gen op args =
    let open Code in
    match args with
    | [_; _] -> Aux.apply_pure (Ir.Variable (Env.String.find op Lib.nenv)) args
    | _ -> raise (internal_error (Printf.sprintf "Unrecognised string operation '%s' with arity %d\n" op (List.length args)))
end

module Comparison :
sig
  val is : string -> bool
  val gen : string -> Ir.value list -> Ir.value
end =
struct
  (* these names should be used for non-primitive types *)
  let funs =
      [ "==";
        "<>";
        "<" ;
        ">" ;
        "<=";
        ">=" ]

  let is x = List.mem x funs
  let gen op args =
    let open Code in
    match op, args with
      | _, [_; _] when List.mem op funs -> Aux.apply_pure (Ir.Variable (Env.String.find op Lib.nenv)) args
      | _, _ -> raise (internal_error (Printf.sprintf "Unrecognised relational operator '%s' with arity %d\n" op (List.length args)))
end

module ListPrim : sig
  val is : string -> bool
  val gen : string -> Ir.value list -> Ir.value
end = struct
  let builtins =
    [ "Cons"
    ; "$$hd"
    ; "$$tl"
    ; "hd"
    ; "tl"
    ; "Concat"
    ; "Nil" ]

  let is op = List.mem op builtins

  let gen op args =
    let open Code in
    match op, args with
    | "Nil", [] -> Aux.apply_pure (Ir.Variable (Env.String.find op Lib.nenv)) args
    | ("Cons" | "Concat"), [_;_] -> Aux.apply_pure (Ir.Variable (Env.String.find op Lib.nenv)) args
    | _, [_] when is op -> Aux.apply_pure (Ir.Variable (Env.String.find op Lib.nenv)) args
    | _, _ -> raise (internal_error (Printf.sprintf "Unrecognised list operator '%s' with arity %d\n" op (List.length args)))
end

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
  val pop : t -> Code.MetaContinuation.t * Ir.binding list * t * t

  (* Turns code into a continuation. *)
  val reflect : Ir.value -> t
  (* Turns a continuation into code. *)
  val reify   : t -> Ir.value

  (* Continuation name binding. *)
  val bind : t -> (t -> Ir.tail_computation) -> Ir.tail_computation

  val apply : t -> Ir.value -> Types.typ -> Types.typ -> Ir.tail_computation

  (* Augments a function [Fn] with a continuation parameter and
     reflects the result as a continuation. The continuation parameter
     in the callback provides access to the current continuation. *)
  (* val contify_with_env : (t -> venv * Ir.tail_computation) -> venv * t *)

  (* Generates a string dump of the continuation, for debugging purposes. *)
  val to_string : t -> string

  val builtins : Ir.binding list
end

(* The higher-order continuation structure for effect handlers
   support *)
module Higher_Order_Continuation : CONTINUATION = struct
  (* We can think of this particular continuation structure as a
     nonempty stack with an even number of elements. *)
  type t = Cons of Ir.value * t
         | Reflect of Ir.value
         | Identity

  (* Auxiliary functions for manipulating the continuation stack *)
  include Code.Aux.List
  let nil = Code.RuntimeList.nil
  let toplevel, funs =
    let open Code in
    let idk_fun, idk_var = 
      let _idk_bind, _idk = Var.fresh_var (Var.make_global_info (Types.Not_typed, "_idk")) in
      let _x_bind, _x = Var.fresh_var (Var.make_local_info (Types.Not_typed, "_x")) in
      let _ks_bind, _ks = Var.fresh_var (Var.make_local_info (Types.Not_typed, "_ks")) in
      let open Ir in
      Ir.Fun {
        fn_binder = _idk_bind;
        fn_tyvars = [];
        fn_params = [_x_bind; _ks_bind];
        fn_body = [], Ir.Return (Ir.Extend (Utility.StringMap.empty, None));
        fn_closure = None;
        fn_location = CommonTypes.Location.Unknown; (* TODO L1: check which CommonTypes.Location.t is preferred *)
        fn_unsafe = false
      }, _idk
    in
    let efferr_fun, efferr_var = 
      let _efferr_bind, _efferr = Var.fresh_var (Var.make_global_info (Types.Not_typed, "_efferr")) in
      let _z_bind, _z = Var.fresh_var (Var.make_local_info (Types.Not_typed, "_z")) in
      let _ks_bind, _ks = Var.fresh_var (Var.make_local_info (Types.Not_typed, "_ks")) in
      let _tag_bind, _tag = Var.fresh_var (Var.make_local_info (Types.Not_typed, "_tag")) in
      let open Ir in
      Ir.Fun {
        fn_binder = _efferr_bind;
        fn_tyvars = [];
        fn_params = [_z_bind; _ks_bind];
        fn_body = [Ir.Let (_tag_bind, ([],Ir.Return (Code.Aux.project "_label" (Ir.Variable _z))))], Aux.tail_comp_placeholder;
        fn_closure = None;
        fn_location = CommonTypes.Location.Unknown; (* TODO L1: check which CommonTypes.Location.t is preferred *)
        fn_unsafe = false
      }, _efferr
    in
    Cons (Ir.Variable idk_var, Cons (Ir.Variable efferr_var, Reflect nil)), [idk_fun; efferr_fun]

  let reflect x = Reflect x
  let rec reify = function
  | Cons (v, vs) ->
    cons v (reify vs)
  | Reflect v ->
    v
  | Identity ->
    reify toplevel

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

  (* let bind kappas body =
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
  bs (body (ks (reflect seed))) *)

  (* placeholder bind *)
  let bind x f = f x

  let builtins, _K_apply =
    let open Types in
    let open Var in
    let _tcontinuation = Meta (Unionfind.fresh (Application (continuation,[]))) in
    let kind = (PrimaryKind.Type, (Linearity.Any, Restriction.Any)) in
    let _tretid, _targid = fresh_raw_variable (), fresh_raw_variable () in
    let _tret = make_rigid_type_variable _tretid (Linearity.Any, Restriction.Any) in (* Not sure about all that *)
    let _targ = make_rigid_type_variable _targid (Linearity.Any, Restriction.Any) in
    let _K_apply_bind, _K_apply = fresh_var (make_local_info (
      ForAll ([(_targid, kind); (_tretid, kind)], Function (Record (Row (Utility.StringMap.of_list ["1", Present _tcontinuation; "2", Present _targ], closed_row_var, false)), make_empty_closed_row (), _tret)),
      "_K_apply"
    )) in
    let _ks_bind, _ks = fresh_var (make_local_info (_tcontinuation, "_ks")) in
    let _arg_bind, _arg = fresh_var (make_local_info (_targ, "_arg")) in
    let _k_bind, _k = fresh_var (make_local_info (Function(Record (Row (Utility.StringMap.of_list ["1", Present _targ], closed_row_var, false)), make_empty_closed_row (), _tret), "_k")) in
    let _ks2_bind, _ks2 = fresh_var (make_local_info (_tcontinuation, "_ks2")) in
    let open Ir in
    [Ir.Rec [{
      fn_binder = _K_apply_bind;
      fn_tyvars = [];
      fn_params = [_ks_bind; _arg_bind];
      fn_body = [Ir.Let (_k_bind, ([], head (Ir.Variable _ks))); Ir.Let (_ks2_bind, ([], tail (Ir.Variable _ks)))],
      (Ir.Apply ((Ir.Variable _k), [Ir.Variable _arg; Ir.Variable _ks2]));
      fn_closure = None;
      fn_location = CommonTypes.Location.Unknown; (* TODO L1: check which CommonTypes.Location.t is preferred *)
      fn_unsafe = false
    }]], _K_apply
  
  let apply (ks: t) arg targ tret =
    Code.Aux.apply (Ir.TApp ((Ir.Variable _K_apply), [(PrimaryKind.Type, targ); (PrimaryKind.Type, tret)])) [reify ks; arg]
    (*
    function(ks, arg) {
       const k = _$List.head(ks);
       ks2 = _$List.tail(ks);
       return k(arg, ks2);
    }
    *)


  (* Contify with env will probably need to thread the current __kappa variable,
  as it cannot be a shared name and rely on scope *)
  
  (* let contify_with_env fn =
    let open Code in
    let var = __kappa in
    match fn (reflect (Ir.Variable var)) with
    | env, Fn (args, body) -> env, reflect (Fn (args @ [name], body))
    | _ -> raise (internal_error "contify: non-function argument.") *)


  let rec pop = function
    | Cons (kappa, kappas) ->
       Code.MetaContinuation.identity, [], (reflect kappa), kappas
    | Reflect ks ->
       let __k_bind, __k = Var.fresh_var (Var.make_local_info (Types.Not_typed, "__k")) in
       let __ks_bind, __ks = Var.fresh_var (Var.make_local_info (Types.Not_typed, "__ks")) in
       (fun value -> Ir.Return value),

       [
        Ir.Let (__k_bind, ([], head ks));
        Ir.Let (__ks_bind, ([], tail ks))
       ],

       (reflect (Ir.Variable __k)), reflect (Ir.Variable __ks)
    | Identity -> pop toplevel

  let rec to_string = function
    | Identity -> "IDENTITY"
    | Reflect code -> "REFLECT: " ^ (Ir.show_value code)
    | Cons (code, k) ->
        "CONS: " ^ (Ir.show_value code) ^ ", \n" ^ (to_string k)

end

module type CPS_Compiler_sig = sig
  val generate_program : venv -> Ir.computation -> Types.typ -> venv * Ir.computation
end

(** [generate]
    Generates CPS IR code from IR code

    With CPS transform, result of generate is always of type : (a -> w) -> b
*)
module CPS_Compiler: functor (K : CONTINUATION) -> sig
  include CPS_Compiler_sig
end = functor (K : CONTINUATION) -> struct
  type continuation = K.t
  type new_funs = Ir.binding list
  type new_lets = Ir.binding list

  open Code.Aux.List

  let apply = Code.Aux.apply
  (* let project = Code.Aux.project *)
  let return x = Ir.Return x

  let rec generate_value _env : Ir.value -> Ir.value * Types.typ =
    (* let open Code in
    let open Code.Constructors in
    let gv v = generate_value env v in
    function
    | Ir.Constant _ as c -> c
    | Ir.Variable var -> Ir.Variable var
       (* HACK
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
         Var name *)
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
    | _ -> failwith "Not supported stuff" *)
    function v -> v, Types.Not_typed (* TODO L1: find the type of a value here *)


  let rec generate_tail_computation : venv -> Ir.tail_computation -> continuation -> Types.typ -> Ir.binding list * Ir.tail_computation =
    fun env tc kappa tret ->
    let open Code in
    let gv v = generate_value env v in
    let gc c kappa typ = snd (generate_computation env c kappa typ) in
    match (tc : Ir.tail_computation) with
    | Ir.Return v ->
      let v', targ = gv v in
      [], K.apply kappa v' targ tret
    | Ir.Apply (f, vs) ->
      let f = strip_poly f in
      begin
        match f with
        | Ir.Variable f ->
          let ftype = snd (VEnv.find f env) in 
          begin
            match vs with
            (**| [l; r] when StringOp.is f_name ->
               let l = gv l in
               let r = gv r in
               [], K.apply kappa (StringOp.gen f_name [l; r])
            | [l; r] when Comparison.is f_name ->
               let l = gv l in
               let r = gv r in
               [], K.apply kappa (Comparison.gen f_name [l; r])
            | vs when Arithmetic.is f_name ->
              [], K.apply kappa (Arithmetic.gen f_name (List.map gv vs))
            | vs when ListPrim.is f_name ->
              [], K.apply kappa Aux.value_placeholder *) (* (ListPrim.gen f_name (List.map gv vs)) *)
            | _ ->
              (* if Lib.is_primitive f_name
              && not (Location.is_server (Lib.primitive_location f_name))
              then *)
                let values = List.map (fun x -> fst (gv x)) vs in
                let arg_tc = apply (Ir.Variable f) values in
                let arg_typ = match ftype with
                  | Types.Function (_,_,ftret) -> ftret
                  | _ -> failwith "Trying to apply a non-function type"
                in
                let _arg_bind, _arg = Var.fresh_var (Var.make_local_info (arg_typ, "_arg")) in
                [Ir.Let (_arg_bind, ([], arg_tc))], K.apply kappa (Ir.Variable _arg) arg_typ tret
          end
        | _ ->
          let f', tf = gv f in
          [], apply f ((List.map (fun x -> fst (gv x)) vs) @ [K.reify kappa])
          (* (apply (gv f) ((List.map gv vs) @ [K.reify kappa])) *)
      end
    | Ir.Special special ->
      generate_special env special kappa
    | Ir.Case (v, cases, default) ->
      [], Aux.tail_comp_placeholder
      (* let v = gv v in
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
           k (Switch (project scrutinee "_label", cases, default))) *)
    | Ir.If (v, c1, c2) ->
      [], Aux.tail_comp_placeholder
      (* K.bind kappa
        (fun kappa ->
           If (gv v, gc c1 kappa, gc c2 kappa)) *)

  and generate_special env : Ir.special -> continuation -> Ir.binding list * Ir.tail_computation
    = fun sp kappa ->
      let module Var' = Var in
      let open Code in
      let gv v = generate_value env v in
      match sp with
      | Ir.Wrong _ -> [], return (Aux.die "Pattern matching failure") (* THIS MESSAGE SHOULD BE MORE INFORMATIVE *)
      | Ir.DoOperation (name, args, _) ->
        [], Aux.tail_comp_placeholder
         (* let maybe_box = function
           | [v] -> gv v
           | vs -> Dict (List.mapi (fun i v -> (string_of_int @@ i + 1, gv v)) vs)
         in
         let nil = RuntimeList.nil in
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
             bind_skappa (bind_seta (return (apply (K.reify seta) [op; K.reify kappas])))) *)
      | Ir.Handle { Ir.ih_comp = comp; Ir.ih_cases = eff_cases; Ir.ih_return = return; Ir.ih_depth = depth } ->
        [], Aux.tail_comp_placeholder
         (* let comp_env = env in
         let vmap r y =
           apply (Var "_vmapOp") [r; y]
         in
         let generate_body env (x, n) body kappas =
           let env' = VEnv.bind x n env in
           snd (generate_computation env' body kappas)
         in
         begin match depth with
         | Ir.Shallow -> raise (Errors.runtime_error "CPS compilation of shallow handlers is not supported")
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
                then apply (Var "_make_parameterised_resumption") [Var (snd @@ name_binder param_ptr_binder); s]
                else apply (Var "_make_resumption") [s]
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
                    bind (Aux.return (apply (K.reify h') ([vmap resumption y] @ [K.reify ks'']))))
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
            (generate_computation comp_env (initial_parameterise comp) kappa) |> snd |> snd
         end *)
      | _ -> failwith "Not supported stuff"

  and generate_computation : venv -> Ir.computation -> continuation -> Types.typ -> (venv * Ir.computation) =
    fun env (bs, tc) kappa typ ->
      let rec gbs : venv -> continuation -> Ir.binding list -> venv * Ir.computation =
        fun env kappa ->
          let open Code in
          function
          (* | Ir.Let (b, (_, Ir.Return v)) :: bs ->
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
          | Ir.Alien _ :: bs -> gbs env kappa bs *)
(* | [] *)| _ ->  let bindings, tc = generate_tail_computation env tc kappa typ in
                  (env, (K.builtins @ bindings, tc))
      in
      gbs env kappa bs

  (* and generate_function env fs :
      Ir.fun_def ->
    (string * string list * Ir.value * Ir.location) =
    fun fundef ->
      let Ir.{fn_binder = fb; fn_tyvars = _; fn_params = xsb; fn_body; fn_closure = zb;
                        fn_location; fn_unsafe = _} = fundef
      in
      let (_, f_name) = name_binder fb in
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
       fn_location) *)

  let generate_program venv comp typ =
    let (env, code) = generate_computation venv comp K.toplevel typ in
    (env, code)
end

module Continuation =
  (val (module Higher_Order_Continuation : CONTINUATION))

module Compiler = CPS_Compiler(Continuation)

open IrTransform
let program state program =
  let nenv  = Context.name_environment state.context in
  let venv =
        Env.String.fold
        (fun name v venv -> begin
          match Env.String.find_opt name Lib.type_env with
          | Some t -> Env.Int.bind v (name, t) venv
          | None -> failwith "A default variable is not a builtin"
        end) (* TODO L1: Find the type of base variables here... *)
        nenv
        Env.Int.empty
  in
  print_endline "\n\nStarting CPS transform";
  let venv', program' = Compiler.generate_program venv program state.datatype in
  print_endline "Finished CPS transform";
  IrTransform.Result {state; program=program'}




module IrPrint = struct
  let name = "IrPrint"
  let program state program =
    Format.printf "%a@." Ir.pp_program program; IrTransform.Result {state; program}
end