(*** pattern matching compiler ***)
(*
  This pattern matching compiler is tree-based (like the one used in
  SML/NJ). Thus it can result in the duplication of continuations.

  In order to give an automaton-based implementation (which would
  guarantee no duplication of continuations), we would probably need
  to adjust our intermediate language.
*)

open Utility
open Ir

type pattern = [
| `Any
| `Nil
| `Cons     of pattern * pattern
| `Variant  of name * pattern
| `Negative of StringSet.t
| `Record   of (pattern StringMap.t) * pattern option
| `Constant of constant
| `Variable of tybinder
| `As       of tybinder * pattern
| `HasType  of pattern * Types.datatype
]

module Const = struct
  type t = Syntax.constant
  let compare = Pervasives.compare
  module Show_t = Syntax.Show_constant
end

module type CONSTSET = Set with type elt = Syntax.constant
module ConstSet = Set.Make(Const)
module ConstMap = Map.Make(Const)

type context =
    [ `Nil | `Cons
    | `Variant of string | `NVariant of StringSet.t
    | `Constant of constant | `NConstant of ConstSet.t ]

module NEnv = Env.String
module TEnv = Env.Int
module PEnv = Env.Int

let rec desugar_pattern : var NEnv.t -> Sugartypes.pattern -> pattern * var NEnv.t =
  fun env (p, pos) ->
    let pp = desugar_pattern in
    let fresh_binder env =
      function
        | (name, Some t, _) ->
            let xb, x = Var.fresh_var (t, name, `Local) in
              xb, NEnv.bind env (name, x)
        | _ -> assert false
    in
      match p with
        | `Any -> `Any, env
        | `Nil -> `Nil, env
        | `Cons (p, ps) ->
            let p, env = pp env p in
            let ps, env = pp env ps in
              `Cons (p, ps), env
        | `List [] -> pp env (`Nil, pos)
        | `List (p::ps) ->
            let p, env = pp env p in
            let ps, env = pp env (`List ps, pos) in
              `Cons (p, ps), env
        | `Variant (name, None) -> `Variant (name, `Any), env
        | `Variant (name, Some p) ->
            let p, env = pp env p in
              `Variant (name, p), env
        | `Negative names -> `Negative (StringSet.from_list names), env
        | `Record (bs, p) ->
            let bs, env =
              List.fold_right
                (fun (name, p) (bs, env) ->
                   let p, env = pp env p in
                     StringMap.add name p bs, env)
                bs
                (StringMap.empty, env) in
            let p, env =
              match p with
                | None -> None, env
                | Some p ->
                    let p, env = pp env p in
                      Some p, env
            in
              `Record (bs, p), env
        | `Tuple ps ->
            let bs = mapIndex (fun (p, pos) i -> (string_of_int (i+1), (p, pos))) ps in
              pp env (`Record (bs, None), pos)
        | `Constant constant ->
            `Constant constant, env
        | `Variable (tyvars, b) ->
            let xb, env = fresh_binder env b in
              `Variable (tyvars, xb), env
        | `As ((tyvars, b), p) ->
            let xb, env = fresh_binder env b in
            let p, env = pp env p in
              `As ((tyvars, xb), p), env
        | `HasType (p, (_, Some t)) ->
            let p, env = pp env p in
              `HasType (p, t), env
        | `HasType (_, (_, None)) -> assert false    

type nenv = var NEnv.t
type tenv = Types.datatype TEnv.t
type penv = (context * value) PEnv.t

type env = nenv * tenv * penv
type raw_env = nenv * tenv

let bind_context var context (nenv, tenv, penv) =
  (nenv, tenv, PEnv.bind penv (var, context))
  
let bind_type var t (nenv, tenv, penv) =
  (nenv, TEnv.bind tenv (var, t), penv)

let mem_context var (_nenv, _tenv, penv) =
  PEnv.has penv var

let mem_type var (_nenv, tenv, _penv) =
  TEnv.has tenv var

let lookup_context var (_nenv, _tenv, penv) =
  PEnv.lookup penv var

let lookup_type var (_nenv, tenv, _penv) =
  TEnv.lookup tenv var

let lookup_name name (nenv, _tenv, _penv) =
  NEnv.lookup nenv name

type raw_bound_computation = raw_env -> computation
type bound_computation = env -> computation

let list_head env : value -> tail_computation = fun v ->
  `Apply(`Variable (lookup_name "hd" env), [v])

let list_tail env : value -> tail_computation = fun v ->
  `Apply(`Variable (lookup_name "tl" env), [v])

let show_pattern_compilation = Settings.add_bool("show_pattern_compilation2", false, `User)
  
type annotation = [`Binder of tybinder | `Type of Types.datatype] list
type annotated_pattern = annotation * pattern

type raw_clause = pattern list * raw_bound_computation
type clause = annotated_pattern list * bound_computation
type annotated_clause = annotation * clause

type pattern_type = [ `List | `Variant | `Negative | `Record | `Constant | `Variable ]

(* pattern-matching let *)
let let_pattern : (env * Types.datatype) -> Types.datatype -> pattern -> value -> computation -> computation =
  fun (env, body_type) pattern_type pat value body ->
    let rec lp t pat value body =
      match pat with
        | `Nil ->
            [], `If(`Comparison(value, `Equal, `Variable (lookup_name "Nil" env)),
                    body,
                    ([], `Special (`Wrong body_type)))
        | `Cons (head, tail) ->
            let xt = TypeUtils.element_type t in
            let xst = t in
            let xb, x = Var.fresh_var_of_type xt in
            let xsb, xs = Var.fresh_var_of_type xst in
              with_bindings
                [letm (xb, list_tail env value); letm (xsb, list_head env value)]
                (lp xt head (`Variable x) (lp xst tail (`Variable xs) body))
        | `Variant (name, patt) ->
            let case_type = TypeUtils.variant_at name t in
            let case_binder, case_variable = Var.fresh_var_of_type case_type in
            let comp = lp case_type patt (`Variable case_variable) body in
            let cases = StringMap.singleton name (case_binder, comp) in
              [], `Case (value, cases, None)
        | `Negative _ ->
            body
        | `Record (fields, rest) ->
            let body =
              match rest with
                | None -> body
                | Some p ->
                    let rt =
                      StringMap.fold
                        (fun name _ t ->
                           TypeUtils.erase_type name t)
                        fields
                        t
                    in
                      lp rt p (`Coerce (value, rt)) body
            in
              StringMap.fold
                (fun name p body ->
                   lp (TypeUtils.project_type name t) p (`Project (name, value)) body)
                fields
                body
        | `Constant c ->
            [], `If(`Comparison(value, `Equal, `Constant c),
                    body,
                    ([], `Special (`Wrong body_type)))
        | `Any -> body
        | `Variable xb ->
            with_bindings
              [letv (xb, value)]
              body
        | `As (xb, pattern) ->
            let body = lp t pattern value body in
              with_bindings
                [letv (xb, value)]
                body
        | `HasType (pat, t) ->           
            lp t pat (`Coerce (value, t)) body
    in
      lp pattern_type pat value body

let rec get_pattern_type : pattern -> pattern_type = 
  function
    | `Nil | `Cons _ -> `List
    | `Variant _ -> `Variant
    | `Negative _ -> `Negative
    | `Record _ -> `Record
    | `Constant _ -> `Constant 
    | `Any | `Variable _ -> `Variable
    | `As (_, pattern) -> get_pattern_type pattern
    | `HasType (pattern, _) -> get_pattern_type pattern

let get_clause_pattern_type : clause -> pattern_type =
  fun ((_, pattern)::_, _) -> get_pattern_type pattern

let get_clauses_pattern_type : clause list -> pattern_type =
  fun (((_, pattern)::_, _)::_) -> get_pattern_type pattern

(* compile away top-level As and HasType patterns *)
let rec reduce_pattern : pattern -> annotated_pattern = function
  | `As (binder, pattern) ->
      let annotations, pattern = reduce_pattern pattern in
        `Binder binder :: annotations, pattern
  | `HasType (pattern, t) ->
      let annotations, pattern = reduce_pattern pattern in
        `Type t :: annotations, pattern
  | pattern -> [], pattern

(* reduce a raw clause to a clause  *)
let reduce_clause : raw_clause -> clause =
  fun (ps, body) ->
    (List.map reduce_pattern ps, fun (nenv, tenv, penv) -> body (nenv, tenv))

(* partition clauses sequentially by pattern type *)
let partition_clauses : clause list -> (clause list) list =
  function
    | [] -> []
    | clauses ->
        let (_, es, ess) =
          List.fold_right
            (fun clause (t, es, ess) ->
               let t' = get_clause_pattern_type clause in
               let es', ess' =
                 (* group non-variable patterns of the same type *)
                 if es = [] || (t' = t && t' <> `Variable && t' <> `Negative) then
                   clause::es, ess
                 else
                   [clause], es::ess
               in
                 (t', es', ess')) clauses (`Variable, [], [])
        in
          es::ess

(* arrange list clauses by constructor *)
let arrange_list_clauses : clause list -> (annotated_clause list * annotated_clause list) =
  fun clauses ->
    List.fold_right (fun (ps, body) (nil_clauses, cons_clauses) ->
                       match ps with
                         | (annotation, `Nil)::ps ->
                             (annotation, (ps, body))::nil_clauses, cons_clauses
                         | (annotation, `Cons (px, pxs))::ps ->
                             let px = reduce_pattern px in 
                             let pxs = reduce_pattern pxs in
                               nil_clauses, (annotation, (px::pxs::ps, body))::cons_clauses
                         | _ -> assert false) clauses ([], [])

(* arrange variant clauses by constructor *)
let arrange_variant_clauses
    : clause list -> (annotated_clause list) StringMap.t =
  fun clauses ->
    (List.fold_right
       (fun (ps, body) env ->
          match ps with
            | (annotation, `Variant (name, pattern))::ps ->
                let annotated_clauses = 
                  if StringMap.mem name env then
                    StringMap.find name env
                  else
                    [] in
                let pattern = reduce_pattern pattern in
                  StringMap.add name ((annotation, (pattern::ps, body))::annotated_clauses) env
            | _ -> assert false
       ) clauses StringMap.empty)

(* arrange constant clauses by constant value *)
let arrange_constant_clauses
    : clause list -> (annotated_clause list) ConstMap.t =
  fun clauses ->
    List.fold_right
      (fun (ps, body) env ->
         match ps with
           | (annotation, `Constant constant)::ps ->
               let annotated_clauses =
                 if ConstMap.mem constant env then
                   ConstMap.find constant env
                 else
                   []
               in
                 ConstMap.add constant ((annotation, (ps, body))::annotated_clauses) env
           | _ -> assert false
      ) clauses ConstMap.empty
      
(* arrange record clauses *)
(*
  Note that record patterns always match.
  This function flattens all the record clauses.
*)
let arrange_record_clauses
    : clause list -> (annotated_pattern StringMap.t * annotated_pattern option * annotated_clause) list =
  fun clauses ->
    let rec flatten =
      function
        | `Record (bs, None) ->
            bs, None
        | `Record (bs, Some p) ->
            let bs', p' = flatten p in
              StringMap.union_disjoint bs bs', p'
        | p ->
            StringMap.empty, Some p
    in
      List.fold_right
        (fun (ps, body) xs ->
           match ps with
             | (annotation, p)::ps ->
                 let bs, p = flatten p in
                 let bs = StringMap.map reduce_pattern bs in
                 let p = opt_map reduce_pattern p in
                   (bs, p, (annotation, (ps, body)))::xs
             | _ -> assert false
        ) clauses []

(* 
   apply an annotation to an expression
*)
let apply_annotation : value -> annotation * bound_computation -> bound_computation =
  fun v (annotation, body) env ->
    let dummy t = Var.fresh_binder_of_type t in
    let env, bs =
      List.fold_right
        (fun a (env, bs) ->
           match a with
             | `Binder b ->
                 let var = Var.var_of_tybinder b in
                 let t = Var.type_of_tybinder b in
                   bind_type var t env, letv (b, v)::bs
             | `Type t ->
                 env, (letmv (dummy t, `Coerce (v, t)))::bs
             | `Type _ -> assert false)
        annotation
        (env, [])
    in
      with_bindings bs (body env)

(* apply annotations in an annotated clause list *)
let apply_annotations : value -> annotated_clause list -> clause list =
  fun v annotated_clauses ->
    List.map (fun (annotation, (ps, body)) ->
           (ps, apply_annotation v (annotation, body))) annotated_clauses

(* the entry point to the pattern-matching compiler *)
let rec match_cases : var list -> clause list -> bound_computation -> bound_computation =
  fun vars clauses def env ->
    match vars, clauses with 
      | [], [] -> def env
      | [], ([], body)::_ -> body env
      | var::vars, _ ->
          let clausess = partition_clauses clauses in
            List.fold_right
              (fun clauses comp ->
                 match get_clauses_pattern_type clauses with
                   | `List ->
                       match_list vars (arrange_list_clauses clauses) comp var
                   | `Variant ->
                       match_variant vars (arrange_variant_clauses clauses) comp var
                   | `Negative ->
                       assert (List.length clauses == 1);
                       match_negative vars (List.hd clauses) comp var
                   | `Variable ->
                       match_var vars clauses comp var
                   | `Record ->
                       match_record vars (arrange_record_clauses clauses) comp var
                   | `Constant ->
                       match_constant vars (arrange_constant_clauses clauses) comp var
              ) clausess def env

and match_var : var list -> clause list -> bound_computation -> var -> bound_computation =
  fun vars clauses def var env ->
    match_cases vars
      (List.map (fun ((annotation, pattern)::ps, body) ->
                   let body = apply_annotation (`Variable var) (annotation, body) in
                     match pattern with
                       | `Variable b ->
                           (ps,
                            fun env ->
                              with_bindings
                                [letv (b, `Variable var)]
                                (body env))
                       | `Any ->
                           (ps, body)
                       | _ -> assert false) clauses) def env

and match_list
    : var list -> (annotated_clause list * annotated_clause list) -> bound_computation -> var -> bound_computation =
  fun vars (nil_clauses, cons_clauses) def var env ->
    let t = lookup_type var env in
    let var_val = `Variable var in

    let nil = `Variable (lookup_name "Nil" env) in

    let nil_branch () =
      let env = bind_context var (`Nil, nil) env in
      let nil_clauses = apply_annotations var_val nil_clauses in
        match nil_clauses with
          | [] -> def env
          | _ ->
              match_cases vars nil_clauses def env in

    let cons_branch () =
      let env = bind_context var (`Cons, var_val) env in
      let cons_clauses = apply_annotations var_val cons_clauses in
        match cons_clauses with
          | [] -> def env
          | _ ->
              let t' = TypeUtils.element_type t in
              let xb, x = Var.fresh_var_of_type t' in
              let xsb, xs = Var.fresh_var_of_type t in
              let env = bind_type x t' (bind_type xs t env) in
                with_bindings
                  [letm (xb, list_head env var_val); letm (xsb, list_tail env var_val)]
                  (match_cases (x::xs::vars) cons_clauses def env) in

      if mem_context var env then
        match lookup_context var env with
          | `Nil, _ -> nil_branch ()
          | `Cons, _ -> cons_branch ()
          | _ -> assert false
      else
        ([], `If (`Comparison (var_val, `Equal, nil),
                  nil_branch (),
                  cons_branch()))

and match_variant
    : var list -> (annotated_clause list) StringMap.t -> bound_computation -> var -> bound_computation =
  fun vars bs def var env ->
    let t = lookup_type var env in
    let inject var name = `Inject (name, `Variable var) in

    let context, cexp =
      if mem_context var env then             
        lookup_context var env
      else
        `NVariant StringSet.empty, `Variable var
    in
      match context with
        | `Variant name ->
            if StringMap.mem name bs then
              match cexp with
                | `Inject (_, `Variable (case_variable)) ->
                    let annotated_clauses = StringMap.find name bs in
                    let case_type = lookup_type case_variable env in
                    let inject_type = TypeUtils.inject_type name case_type in
                    let clauses = apply_annotations cexp annotated_clauses in                           
                      match_cases (case_variable::vars) clauses def env
                | _ -> assert false
            else
              def env
        | `NVariant names ->
            let cases, cs =
              StringMap.fold
                (fun name annotated_clauses (cases, cs) ->
                   if StringSet.mem name names then
                     (cases, cs)
                   else
                     let case_type = TypeUtils.variant_at name t in
                     let inject_type = TypeUtils.inject_type name case_type in
                     let (case_binder, case_variable) = Var.fresh_var_of_type case_type in
                     let match_env = bind_type case_variable case_type env in
                     let match_env = bind_context var (`Variant name, inject case_variable name) match_env in
                     let clauses = apply_annotations (inject case_variable name) annotated_clauses in                           
                       (StringMap.add name
                          (case_binder,
                           match_cases (case_variable::vars) clauses def match_env) cases,
                        StringSet.add name cs))
                bs
                (StringMap.empty, names) in

            let default_type =
              StringSet.fold
                (fun name t ->
                   let _, t = TypeUtils.split_variant_type name t in t) cs t in
              begin
                match default_type with
                  | `Variant row ->
                      if Types.is_empty_row row && Types.is_closed_row row then
                        ([], `Case (`Variable var, cases, None))
                      else
                        let default_binder, default_variable = Var.fresh_var_of_type default_type in
                        let default_env = bind_type default_variable default_type env in
                        let default_env =
                          bind_context
                            var
                            (`NVariant cs, `Variable default_variable)
                            default_env
                        in
                          ([], `Case (`Variable var, cases, Some (default_binder, def default_env)))
                  | _ -> assert false
              end
        | _ -> assert false

and match_negative
    : var list -> clause -> bound_computation -> var -> bound_computation =
  fun vars clause def var env ->
    let t = lookup_type var env in
    let inject var name = `Inject (name, `Variable var) in
    let ((annotation, pattern)::ps, body) = clause in
      match pattern with
        | `Negative names ->
            let context, cexp =
              if mem_context var env then
                lookup_context var env
              else
                `NVariant StringSet.empty, `Variable var
            in
              begin
                match context with
                  | `Variant name when StringSet.mem name names ->
                      def env
                  | `Variant name ->
                      let body = apply_annotation (`Variable var) (annotation, body) in
                        match_cases vars [(ps, body)] def env
                  | `NVariant names' ->
                      let diff = StringSet.diff names names' in
                      let cs = StringSet.union names names' in

                      let cases =
                        StringSet.fold
                          (fun name cases ->
                             let case_type = TypeUtils.variant_at name t in
                             let inject_type = TypeUtils.inject_type name case_type in
                             let (case_binder, case_variable) = Var.fresh_var_of_type case_type in
                             let match_env = bind_type case_variable case_type env in
                             let match_env = bind_context var (`Variant name, inject case_variable name) match_env in
                               StringMap.add name (case_binder, def match_env) cases)
                          diff
                          StringMap.empty in
                      let default_type =
                        StringSet.fold
                          (fun name t ->
                             let _, t = TypeUtils.split_variant_type name t in t) cs t in
                      let (default_binder, default_variable) = Var.fresh_var_of_type default_type in
                      let default_env = bind_type default_variable default_type env in
                      let default_env =
                        bind_context
                          var
                          (`NVariant cs, `Variable default_variable)
                          default_env in
                      let body = apply_annotation (`Variable var) (annotation, body) in
                        ([], `Case (`Variable var,
                                    cases,
                                    Some (default_binder,
                                          match_cases vars [(ps, body)] def default_env)))
                  | _ -> assert false
              end
        | _ -> assert false

and match_constant
    : var list -> (annotated_clause list) ConstMap.t -> bound_computation -> var -> bound_computation =
  fun vars bs def var env ->
    let context, cexp =
      if mem_context var env then
        lookup_context var env
      else
        `NConstant ConstSet.empty, `Variable var
    in
      match context with
        | `Constant constant ->
            if ConstMap.mem constant bs then
              let clauses =
                apply_annotations
                  (`Variable var)
                  (ConstMap.find constant bs)
              in
                match_cases vars clauses def env
            else
              def env
        | `NConstant constants ->
            let bs = ConstMap.filteri (fun c _ -> not (ConstSet.mem c constants)) bs in
            let comp, constants =
              ConstMap.fold
                (fun constant annotated_clauses (comp, constants) ->
                   let constants = ConstSet.add constant constants in
                   let env = bind_context var (`NConstant constants, `Variable var) env in
                   let clauses = apply_annotations (`Variable var) annotated_clauses in
                   let comp =
                     ([],
                      `If 
                        (`Comparison (`Variable var, `Equal, `Constant constant),
                         match_cases vars clauses def env,
                         comp))
                   in
                     (comp, constants))              
                bs
                (def env, constants)
            in
              comp
        | _ -> assert false

and match_record
    : var list -> (annotated_pattern StringMap.t * annotated_pattern option * annotated_clause) list ->
    bound_computation -> var -> bound_computation =
  fun vars xs def var env ->
    let t = lookup_type var env in

    let names =
      List.fold_right
        (fun (bs, _, _) names ->
           StringMap.fold (fun name _ names -> StringSet.add name names) bs names) xs StringSet.empty in
    let all_closed = List.for_all (function
                                 | (_, None, _) -> true
                                 | (_, Some _, _) -> false) xs in
    let annotated_clauses =
      List.fold_right
        (fun (bs, p, (annotation, (ps, body))) annotated_clauses ->
           let p, closed =
             match p with
               | None -> ([], `Any), true
               | Some p -> p, false in

           let rps, fields =
             StringSet.fold
               (fun name (ps, fields) ->
                  if StringMap.mem name bs then
                    StringMap.find name bs :: ps, fields
                  else
                    if closed then
                      ([], `Any)::ps, fields
                    else
                      let xt = TypeUtils.project_type name t in
                      let xb, x = Var.fresh_var_of_type xt in
                        ([], `Variable ([], xb))::ps, StringMap.add name (`Variable x) fields)
               names
               ([], StringMap.empty) in
           let rps, body =
             if all_closed then
               rps, body
             else if closed then
               ([], `Any)::List.rev rps, body
             else
               (* type of the original record continuation *)
               let pt =
                 StringMap.fold
                   (fun name _ t ->
                      TypeUtils.erase_type name t)
                   bs
                   t in

               (* type of the flattened record continuation *)
               let xt =
                 StringSet.fold
                   (fun name t ->
                      TypeUtils.erase_type name t)
                   names
                   t in
               let xb, x = Var.fresh_var_of_type xt in

               let body =
                 fun env ->
                   match p with
                     | ([], `Any) ->
                         body env
                     | (annotation, `Any) ->
                         let yb, y = Var.fresh_var_of_type pt in
                           with_bindings
                             [letmv (yb, `Extend (fields, Some (`Variable x)))]
                             ((apply_annotation (`Variable y) (annotation, body)) env)
                     | (annotation, `Variable (_, yb)) ->
                         let y = Var.var_of_binder yb in
                           with_bindings
                             [letmv (yb, `Extend (fields, Some (`Variable x)))]
                             ((apply_annotation (`Variable y) (annotation, body)) env)
                     | _ -> assert false
               in
                 ([], `Variable ([], xb))::rps, body in
           let ps = List.rev rps @ ps in
             (annotation, (ps, body))::annotated_clauses
        ) xs [] in

    let bindings, xs, env =
      StringSet.fold
        (fun name (bindings, xs, env) ->
           let xt = TypeUtils.project_type name t in
           let xb, x = Var.fresh_var_of_type xt in
           let binding = letmv (xb, `Project (name, `Variable var)) in
             binding::bindings, x::xs, bind_type x xt env)
        names
        ([], [], env) in 

    let bindings = List.rev bindings in
    let xs = List.rev xs in
    let clauses = apply_annotations (`Variable var) annotated_clauses in
      with_bindings
        bindings
        (match_cases (xs @ vars) clauses def env)
                   
(* the interface to the pattern-matching compiler *)
let compile_cases
    : raw_env -> (Types.datatype * var * raw_clause list) -> Ir.computation =
  fun (nenv, tenv) (output_type, var, raw_clauses) ->
    let clauses = List.map reduce_clause raw_clauses in
    let initial_env = (nenv, tenv, PEnv.empty) in
    let result =
      match_cases [var] clauses (fun _ -> ([], `Special (`Wrong output_type))) initial_env
    in
      Debug.if_set (show_pattern_compilation)
        (fun () -> "Compiled pattern: "^(string_of_computation result));
      result
