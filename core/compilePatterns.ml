(*** pattern matching compiler ***)
(*
  This pattern matching compiler is tree-based (like the one used in
  SML/NJ). Thus it can result in the duplication of continuations.

  In order to give an automaton-based implementation (which would
  guarantee no duplication of continuations), we would probably need
  to adjust our intermediate language.
*)

open CommonTypes
open SourceCode
open Utility
open Ir
open Var

let show_pattern_compilation
  = Settings.(flag "show_pattern_compilation"
              |> convert parse_bool
              |> sync)

module type CONSTSET = Set with type elt = Constant.t
module ConstSet = Set.Make(Constant)
module ConstMap = Map.Make(Constant)

module Pattern =
struct

  type t =
    | Any
    | Nil
    | Cons     of t * t
    | Variant  of Name.t * t
    | Operation of Name.t * t list * t
    | Negative of StringSet.t
    | Record   of t StringMap.t * t option
    | Constant of Constant.t
    | Variable of binder
    | As       of binder * t
    | HasType  of t * Types.datatype
    [@@deriving show]

  type context =
    | CNil
    | CCons
    | CVariant   of string
    | CNVariant  of StringSet.t
    | CConstant  of Constant.t
    | CNConstant of ConstSet.t

  type sort =
    | SList
    | SVariant
    | SNegative
    | SRecord
    | SConstant
    | SVariable
    | SOperation

  type annotation_element =
    | Binder of binder
    | Type of Types.datatype

end



module NEnv = Env.String
module TEnv = Env.Int
module PEnv = Env.Int

type nenv = var NEnv.t
type tenv = Types.datatype TEnv.t
type penv = (Pattern.context * value) PEnv.t

type env = nenv * tenv * Types.row * penv
type raw_env = nenv * tenv * Types.row

let bind_context var context (nenv, tenv, eff, penv) =
  (nenv, tenv, eff, PEnv.bind var context penv)

let bind_type var t (nenv, tenv, eff, penv) =
  (nenv, TEnv.bind var t tenv, eff, penv)

let mem_context var (_nenv, _tenv, _eff, penv) =
  PEnv.has var penv

let mem_type var (_nenv, tenv, _eff, _penv) =
  TEnv.has var tenv

let lookup_context var (_nenv, _tenv, _eff, penv) =
  PEnv.find var penv

let lookup_type var (_nenv, tenv, _eff, _penv) =
  TEnv.find var tenv

let lookup_name name (nenv, _tenv, _eff, _penv) =
  NEnv.find name nenv

let lookup_effects (_nenv, _tenv, eff, _penv) = eff

let rec desugar_pattern : Types.row -> Sugartypes.Pattern.with_pos -> Pattern.t * raw_env =
  fun eff {WithPos.node=p; pos} ->
    let desugar_pattern = desugar_pattern eff in
    let empty = (NEnv.empty, TEnv.empty, eff) in
    let (++) (nenv, tenv, _) (nenv', tenv', eff') = (NEnv.extend nenv nenv', TEnv.extend tenv tenv', eff') in
    let fresh_binder (nenv, tenv, eff) bndr =
      assert (Sugartypes.Binder.has_type bndr);
      let name = Sugartypes.Binder.to_name bndr in
      let t = Sugartypes.Binder.to_type bndr in
      let xb, x = Var.(fresh_var (make_local_info (t, name))) in
      xb, (NEnv.bind name x nenv, TEnv.bind x t tenv, eff)
    in
      let open Sugartypes.Pattern in
      match p with
        | Any -> Pattern.Any, empty
        | Nil -> Pattern.Nil, empty
        | Cons (p, ps) ->
            let p, env = desugar_pattern p in
            let ps, env' = desugar_pattern ps in
              Pattern.Cons (p, ps), env ++ env'
        | List [] -> desugar_pattern (WithPos.make ~pos Nil)
        | List (p::ps) ->
            let p, env = desugar_pattern p in
            let ps, env' = desugar_pattern (WithPos.make ~pos (List ps)) in
              Pattern.Cons (p, ps), env ++ env'
        | Variant (name, None) -> Pattern.Variant (name, Pattern.Any), empty
        | Variant (name, Some p) ->
            let p, env = desugar_pattern p in
            Pattern.Variant (name, p), env
        | Operation (name, ps, k, _) ->
           let ps, env =
             List.fold_right
               (fun p (ps, env) ->
                 let p', env' = desugar_pattern p in
                 (p' :: ps, env ++ env'))
               ps ([], empty)
           in
           let k, env' = desugar_pattern k in
           Pattern.Operation (name, ps, k), env ++ env'
        | Negative names -> Pattern.Negative (StringSet.from_list names), empty
        | Record (bs, p) ->
            let bs, env =
              List.fold_right
                (fun (name, p) (bs, env) ->
                   let p, env' = desugar_pattern p in
                     StringMap.add name p bs, env ++ env')
                bs
                (StringMap.empty, empty) in
            let p, env =
              match p with
                | None -> None, env
                | Some p ->
                    let p, env' = desugar_pattern p in
                      Some p, env ++ env'
            in
              Pattern.Record (bs, p), env
        | Tuple ps ->
            let bs = mapIndex (fun p i -> (string_of_int (i+1), p)) ps in
              desugar_pattern (WithPos.make ~pos (Record (bs, None)))
        | Constant constant ->
            Pattern.Constant constant, empty
        | Variable b ->
            let xb, env = fresh_binder empty b in
              Pattern.Variable xb, env
        | As (b, p) ->
            let xb, env = fresh_binder empty b in
            let p, env' = desugar_pattern p in
              Pattern.As (xb, p), env ++ env'
        | HasType (p, (_, Some t)) ->
            let p, env = desugar_pattern p in
              Pattern.HasType (p, t), env
        | HasType (_, (_, None)) -> assert false

type raw_bound_computation = raw_env -> computation
type bound_computation = env -> computation

module CompileLists :
sig
  val nil : raw_env -> Types.datatype -> value
  val list_head : raw_env -> Types.datatype -> value -> tail_computation
  val list_tail : raw_env -> Types.datatype -> value -> tail_computation
end
  =
struct
  open PrimaryKind

  let lookup_effects (_nenv, _tenv, eff) = eff

  let nil _env t : value =
    TApp (Variable (NEnv.find "Nil" Lib.nenv),
           [(Type, t)])

  let list_head env t : value -> tail_computation = fun v ->
    let eff = lookup_effects env in
      Apply
        (TApp
           (Variable (NEnv.find "$$hd" Lib.nenv),
            [(Type, t); (Row, eff)]),
         [v])

  let list_tail env t : value -> tail_computation = fun v ->
    let eff = lookup_effects env in
      Apply
        (TApp
           (Variable (NEnv.find "$$tl" Lib.nenv),
            [(Type, t); (Row, eff)]),
         [v])
end
open CompileLists

module CompileEq :
sig
  val eq : raw_env -> Types.datatype -> value -> value -> value
end
  =
struct
  open PrimaryKind

  let lookup_effects (_nenv, _tenv, eff) = eff

  let eq env t v1 v2 =
    let eff = lookup_effects env in
    ApplyPure
      (TApp
         (Variable (NEnv.find "==" Lib.nenv),
          [(Type, t); (Row, eff)]),
       [v1; v2])
end
open CompileEq

type annotation = Pattern.annotation_element list
type annotated_pattern = annotation * Pattern.t

type raw_clause = Pattern.t list * raw_bound_computation
type clause = annotated_pattern list * bound_computation
type annotated_clause = annotation * clause



let let_pattern : raw_env -> Pattern.t -> value * Types.datatype -> computation * Types.datatype -> computation =
  fun env pat (value, value_type) (body, body_type) ->
    let rec lp t pat value body =
      match pat with
        | Pattern.Nil ->
            [], If (eq env t value (nil env (TypeUtils.element_type t)),
                     body,
                     ([], Special (Wrong body_type)))
        | Pattern.Cons (head, tail) ->
            let xt = TypeUtils.element_type t in
            let xst = t in
            let xb, x = Var.fresh_var_of_type xt in
            let xsb, xs = Var.fresh_var_of_type xst in
              with_bindings
                [letm (xb, list_head env xt value); letm (xsb, list_tail env xt value)]
                (lp xt head (Variable x) (lp xst tail (Variable xs) body))
        | Pattern.Variant (name, patt) ->
            let case_type = TypeUtils.variant_at name t in
            let case_binder, case_variable = Var.fresh_var_of_type case_type in
            let body = lp case_type patt (Variable case_variable) body in
            let cases = StringMap.singleton name (case_binder, body) in
              [], Case (value, cases, None)
        | Pattern.Negative names ->
           (* The following expands the negative pattern into
              a switch-case expression:

              [| var -(l1,...,lN) = value; body |]
            = switch (value) {
                case l1 -> Wrong
                     ...
                case lN -> Wrong
                case _  -> body
              }
            *)
            let negative_cases, t' =
              StringSet.fold
                (fun label (cases, t) ->
                  let case_type = TypeUtils.variant_at label t in
                  let case_binder = Var.fresh_binder_of_type case_type in
                  let body = ([], Special (Wrong body_type)) in
                  let cases' = StringMap.add label (case_binder, body) cases in
                  let t' =
                    let row = TypeUtils.extract_row t in
                    Types.Variant (Types.row_with (label, Types.Absent) row)
                  in
                  (cases', t'))
                names (StringMap.empty, t)
            in
            let success_case =
              let case_binder = Var.fresh_binder_of_type t' in
              (case_binder, body)
            in
            [], Case (value, negative_cases, Some success_case)
        | Pattern.Record (fields, rest) ->
            let body =
              match rest with
                | None -> body
                | Some p ->
                    let names =
                      StringMap.fold
                        (fun name _ names ->
                           StringSet.add name names)
                        fields
                        StringSet.empty in
                    let rt = TypeUtils.erase_type names t in
                      lp rt p (Erase (names, value)) body
(*                      lp rt p (`Coerce (value, rt)) body *)
            in
              StringMap.fold
                (fun name p body ->
                   let t' = (TypeUtils.project_type name t) in
                     (lp t' p (Project (name, value)) body))
                fields
                body
        | Pattern.Constant c ->
            [], If (eq env t value (Constant c),
                     body,
                     ([], Special (Wrong body_type)))
        | Pattern.Any -> body
        | Pattern.Variable xb ->
            with_bindings
              [letmv (xb, value)]
              body
        | Pattern.As (xb, pattern) ->
            with_bindings
              [letmv (xb, value)]
              (lp t pattern value body)
        | Pattern.HasType (pat, t) ->
           lp t pat (Coerce (value, t)) body
        | Pattern.Operation _ -> assert false (* This pattern cannot appear in a let expression *)
    in
      lp value_type pat value body

let rec get_pattern_sort : Pattern.t -> Pattern.sort =
  let open Pattern in
  function
    | Nil | Cons _ -> SList
    | Variant _ -> SVariant
    | Negative _ -> SNegative
    | Record _ -> SRecord
    | Constant _ -> SConstant
    | Any | Variable _ -> SVariable
    | As (_, pattern) -> get_pattern_sort pattern
    | HasType (pattern, _) -> get_pattern_sort pattern
    | Operation _ -> SOperation

let get_clause_pattern_sort : clause -> Pattern.sort =
  function
  | ((_, pattern)::_, _) -> get_pattern_sort pattern
  | _ -> assert false

let get_clauses_pattern_sort : clause list -> Pattern.sort =
  function
  | (((_, pattern)::_, _)::_) -> get_pattern_sort pattern
  | _ -> assert false

(* compile away top-level As and HasType patterns *)
let rec reduce_pattern : Pattern.t -> annotated_pattern = function
  | Pattern.As (binder, pattern) ->
      let annotations, pattern = reduce_pattern pattern in
        Pattern.Binder binder :: annotations, pattern
  | Pattern.HasType (pattern, t) ->
      let annotations, pattern = reduce_pattern pattern in
      Pattern.Type t :: annotations, pattern
  | pattern -> [], pattern

(* reduce a raw clause to a clause  *)
let reduce_clause : raw_clause -> clause =
  fun (ps, body) ->
    (List.map reduce_pattern ps, fun (nenv, tenv, eff, _penv) -> body (nenv, tenv, eff))

(* partition clauses sequentially by pattern sort *)
let partition_clauses : clause list -> (clause list) list =
  function
    | [] -> []
    | clauses ->
        let (_, es, ess) =
          List.fold_right
            (fun clause (t, es, ess) ->
               let t' = get_clause_pattern_sort clause in
               let es', ess' =
                 (* group non-variable patterns of the same sort *)
                 if es = [] || (t' = t && t' <> Pattern.SVariable && t' <> Pattern.SNegative) then
                   clause::es, ess
                 else
                   [clause], es::ess
               in
                 (t', es', ess')) clauses (Pattern.SVariable, [], [])
        in
          es::ess

(* arrange list clauses by constructor *)
let arrange_list_clauses : clause list -> (annotated_clause list * annotated_clause list) =
  fun clauses ->
    List.fold_right (fun (ps, body) (nil_clauses, cons_clauses) ->
                       match ps with
                         | (annotation, Pattern.Nil)::ps ->
                             (annotation, (ps, body))::nil_clauses, cons_clauses
                         | (annotation, Pattern.Cons (px, pxs))::ps ->
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
            | (annotation, Pattern.Variant (name, pattern))::ps ->
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
           | (annotation, Pattern.Constant constant)::ps ->
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
        | Pattern.Record (bs, None) ->
            bs, None
        | Pattern.Record (bs, Some p) ->
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
    let massage t =
      function
        | Inject (name, v, _) -> Inject (name, v, t)
        | v -> v in
    let env, bs =
      List.fold_right
        (fun a (env, bs) ->
           match a with
             | Pattern.Binder b ->
                 let var = Var.var_of_binder b in
                 let t = Var.type_of_binder b in
                 let v = massage t v in
                   bind_type var t env, letmv (b, v)::bs
             | Pattern.Type t ->
                 let v = massage t v in
                   env, (letmv (dummy t, Coerce (v, t)))::bs)
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
                 let open Pattern in
                 match get_clauses_pattern_sort clauses with
                   | SList ->
                       match_list vars (arrange_list_clauses clauses) comp var
                   | SVariant ->
                       match_variant vars (arrange_variant_clauses clauses) comp var
                   | SNegative ->
                       assert (List.length clauses == 1);
                       match_negative vars (List.hd clauses) comp var
                   | SVariable ->
                       match_var vars clauses comp var
                   | SRecord ->
                       match_record vars (arrange_record_clauses clauses) comp var
                   | SConstant ->
                      match_constant vars (arrange_constant_clauses clauses) comp var
                   | SOperation -> assert false (* TODO FIXME have proper pattern matching compilation of effect patterns *)
              ) clausess def env
      | _, _ -> assert false

and match_var : var list -> clause list -> bound_computation -> var -> bound_computation =
  fun vars clauses def var env ->
    match_cases vars
      (List.map (
          function
          | ((annotation, pattern)::ps, body) ->
                   let body = apply_annotation (Variable var) (annotation, body) in
                   begin
                     match pattern with
                       | Pattern.Variable b ->
                           (ps,
                            fun env ->
                              with_bindings
                                [letmv (b, Variable var)]
                                (body env))
                       | Pattern.Any ->
                           (ps, body)
                       | _ -> assert false
                   end
          | _ -> assert false) clauses) def env

and match_list
    : var list -> (annotated_clause list * annotated_clause list) -> bound_computation -> var -> bound_computation =
  fun vars (nil_clauses, cons_clauses) def var env ->
    let t = lookup_type var env in
    let var_val = Variable var in

    let nil, list_head, list_tail =
      let raw (nenv, tenv, eff, _) = (nenv, tenv, eff) in

      let nil = nil (raw env) (TypeUtils.element_type t) in
      let list_head env = list_head (raw env) in
      let list_tail env = list_tail (raw env) in
        nil, list_head, list_tail in

    let nil_branch () =
      let env = bind_context var (Pattern.CNil, nil) env in
      let nil_clauses = apply_annotations var_val nil_clauses in
        match nil_clauses with
          | [] -> def env
          | _ ->
              match_cases vars nil_clauses def env in

    let cons_branch () =
      let env = bind_context var (Pattern.CCons, var_val) env in
      let cons_clauses = apply_annotations var_val cons_clauses in
        match cons_clauses with
          | [] -> def env
          | _ ->
              let t' = TypeUtils.element_type t in
              let xb, x = Var.fresh_var_of_type t' in
              let xsb, xs = Var.fresh_var_of_type t in
              let env = bind_type x t' (bind_type xs t env) in
                with_bindings
                  [letm (xb, list_head env t' var_val); letm (xsb, list_tail env t' var_val)]
                  (match_cases (x::xs::vars) cons_clauses def env) in

      if mem_context var env then
        match lookup_context var env with
          | Pattern.CNil, _ -> nil_branch ()
          | Pattern.CCons, _ -> cons_branch ()
          | _ -> assert false
      else
        let (nenv, tenv, eff, _) = env in
          ([], If (eq (nenv, tenv, eff) t var_val nil,
                    nil_branch (),
                    cons_branch()))


(*
  DODGEYNESS:

  I'm not sure if injections are being given the correct type
  argument in match_variant and match_negative.

  RESOLUTION:

  Aha... the types in the injections are never actually used when the
  injections are used for context optimisations, so it doesn't
  matter in these cases.

  Hmm... but there remains another case where the injection is used in
  apply_annotations. We deal with this by massaging the value to have
  the correct type in apply_annotations.
*)

and match_variant
    : var list -> (annotated_clause list) StringMap.t -> bound_computation -> var -> bound_computation =
  fun vars bs def var env ->
    let t = lookup_type var env in

    let context, cexp =
      if mem_context var env then
        lookup_context var env
      else
        Pattern.CNVariant StringSet.empty, Variable var
    in
      match context with
        | Pattern.CVariant name ->
            if StringMap.mem name bs then
              match cexp with
                | Inject (_, (Variable case_variable), _) ->
                    let annotated_clauses = StringMap.find name bs in
                    (* let case_type = lookup_type case_variable env in *)
                      (*                    let inject_type = TypeUtils.inject_type name case_type in *)
                    let clauses = apply_annotations cexp annotated_clauses in
                      match_cases (case_variable::vars) clauses def env
                | _ -> assert false
            else
              def env
        | Pattern.CNVariant names ->
            let cases, cs =
              StringMap.fold
                (fun name annotated_clauses (cases, cs) ->
                   if StringSet.mem name names then
                     (cases, cs)
                   else
                     let case_type =
                       if Settings.get Basicsettings.Sessions.exceptions_enabled &&
                          not (Settings.get Basicsettings.Sessions.expose_session_fail) &&
                          String.equal name Value.session_exception_operation
                       then
                         Types.empty_type
                       else
                         TypeUtils.variant_at name t
                     in
(*                     let inject_type = TypeUtils.inject_type name case_type in *)
                     let (case_binder, case_variable) = Var.fresh_var_of_type case_type in
                     let match_env = bind_type case_variable case_type env in
                     let match_env =
                       bind_context var
                         (Pattern.CVariant name,
                          Inject (name, Variable case_variable, t)) match_env in
                     let clauses =
                       apply_annotations
                         (Inject (name, Variable case_variable, t)) annotated_clauses
                     in
                       (StringMap.add name
                          (case_binder,
                           match_cases (case_variable::vars) clauses def match_env) cases,
                        StringSet.add name cs))
                bs
                (StringMap.empty, names) in

            let default_type =
              StringSet.fold
                (fun name t ->
                   if Settings.get Basicsettings.Sessions.exceptions_enabled &&
                      not (Settings.get Basicsettings.Sessions.expose_session_fail) &&
                      String.equal name Value.session_exception_operation
                   then
                     t
                   else
                     let _, t = TypeUtils.split_variant_type name t in t) cs t in
              begin
                match default_type with
                  | Types.Variant row
                  | Types.Choice row ->
                      if Types.is_empty_row row && Types.is_closed_row row then
                        ([], Case (Variable var, cases, None))
                      else
                        let default_binder, default_variable = Var.fresh_var_of_type default_type in
                        let default_env = bind_type default_variable default_type env in
                        let default_env =
                          bind_context
                            default_variable
                            (Pattern.CNVariant cs, Variable default_variable)
                            default_env
                        in
                          ([], Case (Variable var, cases, Some (default_binder, def default_env)))
                  | _ -> assert false
              end
        | _ -> assert false

and match_negative
    : var list -> clause -> bound_computation -> var -> bound_computation =
  fun vars clause def var env ->
    let t = lookup_type var env in
    let annotation, pattern, ps, body =
      match clause with
      | ((annotation, pattern)::ps, body) -> annotation, pattern, ps, body
      | _ -> assert false in
    match pattern with
    | Pattern.Negative names ->
      let context, _cexp =
        if mem_context var env then
          lookup_context var env
        else
          Pattern.CNVariant StringSet.empty, Variable var
      in
      begin
        match context with
        | Pattern.CVariant name when StringSet.mem name names ->
          def env
        | Pattern.CVariant _name ->
          let body = apply_annotation (Variable var) (annotation, body) in
          match_cases vars [(ps, body)] def env
        | Pattern.CNVariant names' ->
          let diff = StringSet.diff names names' in
          let cs = StringSet.union names names' in

          let cases =
            StringSet.fold
              (fun name cases ->
                  let case_type = TypeUtils.variant_at name t in
(*                             let inject_type = TypeUtils.inject_type name case_type in *)
                  let (case_binder, case_variable) = Var.fresh_var_of_type case_type in
                  let match_env = bind_type case_variable case_type env in
                  let match_env =
                    bind_context var
                      (Pattern.CVariant name,
                       Inject (name, Variable case_variable, t)) match_env
                  in
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
              (Pattern.CNVariant cs, Variable default_variable)
              default_env in
          let body = apply_annotation (Variable var) (annotation, body) in
          ([], Case (Variable var,
                      cases,
                      Some (default_binder,
                            match_cases vars [(ps, body)] def default_env)))
        | _ -> assert false
      end
    | _ -> assert false

and match_constant
    : var list -> (annotated_clause list) ConstMap.t -> bound_computation -> var -> bound_computation =
  fun vars bs def var env ->
    let t = lookup_type var env in
    let context, _cexp =
      if mem_context var env then
        lookup_context var env
      else
        Pattern.CNConstant ConstSet.empty, Variable var
    in
      match context with
        | Pattern.CConstant constant ->
            if ConstMap.mem constant bs then
              let clauses =
                apply_annotations
                  (Variable var)
                  (ConstMap.find constant bs)
              in
                match_cases vars clauses def env
            else
              def env
        | Pattern.CNConstant constants ->
            let bs = ConstMap.filter (fun c _ -> not (ConstSet.mem c constants)) bs in
            let comp, _constants =
              ConstMap.fold
                (fun constant annotated_clauses (comp, constants) ->
                   let constants = ConstSet.add constant constants in
                   let env = bind_context var (Pattern.CNConstant constants, Variable var) env in
                   let clauses = apply_annotations (Variable var) annotated_clauses in
                   let comp =
                     let (nenv, tenv, eff, _) = env in
                       ([],
                        If
                          (eq (nenv, tenv, eff) t (Variable var) (Constant constant),
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

    (* type of the flattened record continuation *)
    let restt = TypeUtils.erase_type names t in
    let restb, rest = Var.fresh_var_of_type restt in

    let annotated_clauses =
      List.fold_right
        (fun (bs, p, (annotation, (ps, body))) annotated_clauses ->
           let p, closed =
             match p with
               | None -> ([], Pattern.Any), true
               | Some p -> p, false in

           let rps, fields =
             StringSet.fold
               (fun name (ps, fields) ->
                  if StringMap.mem name bs then
                    StringMap.find name bs :: ps, fields
                  else
                    if closed then
                      ([], Pattern.Any)::ps, fields
                    else
                      let xt = TypeUtils.project_type name t in
                      let xb, x = Var.fresh_var_of_type xt in
                        ([], Pattern.Variable xb)::ps, StringMap.add name (Variable x) fields)
               names
               ([], StringMap.empty) in
           let rps, body =
             if all_closed then
               rps, body
             else if closed then
               ([], Pattern.Any)::List.rev rps, body
             else
               let original_names =
                 StringMap.fold
                   (fun name _ names ->
                      StringSet.add name names)
                   bs
                   StringSet.empty in

               (* type of the original record continuation *)
               let pt = TypeUtils.erase_type original_names t in

               let body =
                 fun env ->
                   match p with
                     | ([], Pattern.Any) ->
                         body env
                     | (annotation, Pattern.Any) ->
                         let yb, y = Var.fresh_var_of_type pt in
                           with_bindings
                             [Let (yb, ([], Return (Extend (fields, Some (Variable rest)))))]
                             ((apply_annotation (Variable y) (annotation, body)) env)
                     | (annotation, Pattern.Variable yb) ->
                         let y = Var.var_of_binder yb in
                           with_bindings
                             [Let (yb, ([], Return (Extend (fields, Some (Variable rest)))))]
                             ((apply_annotation (Variable y) (annotation, body)) env)
                     | _ -> assert false
               in
                 ([], Pattern.Variable restb)::rps, body in
           let ps = List.rev rps @ ps in
             (annotation, (ps, body))::annotated_clauses
        ) xs [] in

    let bindings, xs, env =
      StringSet.fold
        (fun name (bindings, xs, env) ->
           let xt = TypeUtils.project_type name t in
           let xb, x = Var.fresh_var_of_type xt in
           let binding = letmv (xb, Project (name, Variable var)) in
             binding::bindings, x::xs, bind_type x xt env)
        names
        ([], [], env) in

    let bindings, xs, env =
      if all_closed then
        bindings, xs, env
      else
        let bindings =
          let qs =
            match restt with
              | Types.ForAll (qs, _) -> qs
              | _ -> [] in
          let tyargs = List.map Types.type_arg_of_quantifier qs in
            Let (restb, (qs, Return (tapp (Erase (names, Variable var), tyargs)))) :: bindings in
        let xs = rest :: xs in
        let env = bind_type rest restt env in
          bindings, xs, env in

    let bindings = List.rev bindings in
    let xs = List.rev xs in
    let clauses = apply_annotations (Variable var) annotated_clauses in
      with_bindings
        bindings
        (match_cases (xs @ vars) clauses def env)

(* the interface to the pattern-matching compiler *)
let compile_cases
    : raw_env -> (Types.datatype * var * raw_clause list) -> Ir.computation =
  fun (nenv, tenv, eff) (output_type, var, raw_clauses) ->
    let clauses = List.map reduce_clause raw_clauses in
    let initial_env = (nenv, tenv, eff, PEnv.empty) in
    let result =
      match_cases [var] clauses (fun _ -> ([], Special (Wrong output_type))) initial_env
    in
      Debug.if_set (show_pattern_compilation)
        (fun () -> "Compiled pattern: "^(string_of_computation result));
      result

(* Handler cases compilation *)
let handle_parameter_pattern : raw_env -> (Pattern.t * Types.datatype) -> Ir.computation -> (Ir.binder * Ir.value) * ((Ir.computation -> Ir.computation) * Ir.binding list)
  = fun env (pat, t) body ->
    let pb, p = Var.fresh_var_of_type t in
    let pb', p' = Var.fresh_var_of_type t in
    let outer_bindings =
      let (bs, tc) = body in
      bs @ [letm (pb', tc)]
    in
    let inner_bindings =
         fun cont -> let_pattern env pat (Variable p, t) (cont, Types.Not_typed)
    in
    (pb, Variable p'), (inner_bindings, outer_bindings)

let compile_handle_parameters : raw_env -> (Pattern.t * Ir.computation * Types.datatype) list -> (Ir.binder * Ir.value) list * ((Ir.computation -> Ir.computation) * Ir.binding list)
  = fun env parameters ->
    List.fold_left
      (fun (bvs, (inner, outer)) (pat, body, t) ->
        let (bv, (inner', outer')) =
          handle_parameter_pattern env (pat, t) body
        in
        (bv :: bvs, ((fun comp -> inner' (inner comp)), outer' @ outer)))
      ([], ((fun x -> x), [])) parameters

let compile_handle_cases
    : raw_env -> (raw_clause list * raw_clause list * (Pattern.t * Ir.computation * Types.datatype) list * Sugartypes.handler_descriptor) -> Ir.computation -> Ir.computation =
  fun (nenv, tenv, eff) (raw_value_clauses, raw_effect_clauses, params, desc) m ->
  (* Observation: reduced continuation patterns are always trivial,
     i.e. a reduced continuation pattern is either a variable or a
     wildcard. Thus continuation patterns _always_ match and therefore
     have no impact on whether a given effect clause match. In this
     sense effect pattern compilation reduces to compilation of
     variants, almost. There is one catch: continuation patterns still
     need to be compiled. However, we can handle this in a
     post-processing step.

     The idea is to transform effect patterns into variant patterns by
     dropping the continuation pattern, and the compile them into a
     case expression. This requires constructing a (correct) type for
     the variant pattern. We can construct this type from the
     computation signature of the handler. Afterwards, we create a
     fresh continuation binder for each compiled clause. We gather the
     continuation binders for each raw clause, and bind them in their
     respective compiled clause bodies such that each raw continuation
     binder is an alias of the fresh continuation binder. *)
  let (params, (with_parameters, outer_param_bindings)) =
    compile_handle_parameters (nenv, tenv, eff) params
  in
  let compiled_effect_cases =  (* The compiled cases *)
    if List.length raw_effect_clauses = 0 then
      StringMap.empty
    else begin
        let (comp_eff, comp_ty, _, _) = Sugartypes.(desc.shd_types) in
        let variant_type =
          let (fields,_,_) = comp_eff |> TypeUtils.extract_row_parts in
          let fields' =
            StringMap.filter
              (fun _ ->
                function
                | Types.Present _ -> true
                | _ -> false)
              fields
          in
          let rec extract t = match TypeUtils.concrete_type t with
            | Types.Operation (domain, _, _) ->
              let (fields, _, _) = TypeUtils.extract_row domain |> TypeUtils.extract_row_parts in
              let arity = StringMap.size fields in
              if arity = 1 then
                match StringMap.find "1" fields with
                | Types.Present t -> t
                | _ -> assert false
              else
                domain (* n-ary operation *)
            | Types.ForAll (_, t) -> extract t
            | _ -> Types.unit_type (* nullary operation *)
          in
          let fields'' =
            StringMap.map
              (function
                | Types.Present t ->
                  extract t
                | _ -> assert false)
              fields'
          in
          Types.make_variant_type fields''
        in
        let transformed_effect_clauses =
          let raw_cases =
            List.map
              (fun (ps, body) ->
                let variant_pat =
                  match ps with
                  | [Pattern.Operation (name, [], _)] ->
                     Pattern.Variant (name, Pattern.Any)
                  | [Pattern.Operation (name, [p], _)] ->
                     Pattern.Variant (name, p)
                  | [Pattern.Operation (name, ps, _)] ->
                     let packaged_args =
                       let fields =
                         List.mapi (fun i p -> (string_of_int (i+1), p)) ps
                       in
                       Pattern.Record (StringMap.from_alist fields, None)
                     in
                     Pattern.Variant (name, packaged_args)
                  | _ -> assert false
                in
              [variant_pat], body)
              raw_effect_clauses
          in
          List.map reduce_clause raw_cases
        in
        let compiled_transformed_effect_cases =
          let dummy_var = Var.(make_local_info ->- fresh_binder ->- var_of_binder) (variant_type, "_m") in
          let tenv = TEnv.bind dummy_var variant_type tenv in
          let initial_env = (nenv, tenv, eff, PEnv.empty) in (* Need to bind raw continuation binders in tenv and nenv? *)
          match snd @@ match_cases [dummy_var] transformed_effect_clauses (fun _ -> ([], Special (Wrong comp_ty))) initial_env with
          | Case (_, clauses, _) -> clauses (* No default effect pattern *)
          | _ -> assert false
        in
        let continuation_binders =
          let upd effname ks map =
            match StringMap.lookup effname map with
            | None -> StringMap.add effname ks map
            | Some ks' -> StringMap.add effname (ks @ ks') map
          in
          let rec gather_binders = function
            | Pattern.Any -> []
            | Pattern.Variable b -> [b]
            | Pattern.As (b, p) -> b :: gather_binders p
            | _ -> assert false
          in
          List.fold_left
            (fun acc -> function
              | [Pattern.Operation (name, _, k)] ->
                 upd name (gather_binders k) acc
              | _ -> assert false)
            StringMap.empty (List.map fst raw_effect_clauses)
        in
        StringMap.mapi
          (fun effname (x, body) ->
            let body =
              with_parameters body
            in
            match StringMap.find effname continuation_binders with
            | [] ->
               let resume =
                 Var.(make_local_info ->- fresh_binder) (Types.Not_typed, "_resume")
               in
               (x, resume, body)
            | [resume] -> (* micro-optimisation: if there is only one
                             resumption binder then just use it. *)
               (x, resume, body)
            | ks ->
               let resume_ty =
                 Var.type_of_binder (List.hd ks)
               in
               let resume_b =
                 Var.(make_local_info ->- fresh_binder) (resume_ty, "_resume")
               in
               let resume_v = Var.var_of_binder resume_b in
               let body =
                 List.fold_left
                   (fun (bs, tc) kb ->
                     letmv (kb, Variable resume_v) :: bs, tc)
                   body ks
               in
               (x, resume_b, body))
          compiled_transformed_effect_cases
      end
  in
  let return : binder * computation =
    let (_, comp_ty, _, _) = Sugartypes.(desc.shd_types) in
    let scrutinee = Var.(make_local_info ->- fresh_binder) (comp_ty, "_return_value") in
    let tenv = TEnv.bind (Var.var_of_binder scrutinee) comp_ty tenv in
    let initial_env = (nenv, tenv, eff, PEnv.empty) in
    let clauses = List.map reduce_clause raw_value_clauses in
    let body = match_cases [Var.var_of_binder scrutinee] clauses (fun _ -> ([], Special (Wrong comp_ty))) initial_env in
    scrutinee, with_parameters body
  in
  let handle =
    Handle {
        ih_comp   = m;
        ih_return = return;
        ih_cases  = compiled_effect_cases;
        ih_depth  =
          let open Sugartypes in
          match desc.shd_depth  with
          | Shallow -> Ir.Shallow
          | Deep    -> Ir.Deep params
      }
  in
  (outer_param_bindings, Special handle)

(* Session typing choice compilation *)
let match_choices : var -> clause list -> bound_computation =
  fun var clauses env ->
    let t = lookup_type var env in
      ([], Special (Choice (Variable var,
                              List.fold_left
                                (fun cases -> function
                                  | ([(annotation, pattern)], body) ->
                                    let (name, b) =
                                      match pattern with
                                      | Pattern.Variant (name, Pattern.Variable b) -> (name, b)
                                      | Pattern.Variant (name, Pattern.Any)        ->
                                         let bt = TypeUtils.choice_at name t in
                                         let info = Var.make_local_info (bt, "_") in
                                         (name, Var.fresh_binder info)
                                      | _ ->
                                        (* TODO: give a more useful error message - including the position
                                           (it may be necessary to detect the error earlier on) *)
                                         failwith ("Only choice patterns are supported in choice compilation") in
                                    let x = Var.var_of_binder b in
                                    let body = apply_annotation (Variable x) (annotation, body) in
                                    StringMap.add name (b, body env) cases
                                  | _ -> assert false)
                                StringMap.empty
                                clauses)))

let compile_choices
    : raw_env -> (Types.datatype * var * raw_clause list) -> Ir.computation =
  fun (nenv, tenv, eff) (_output_type, var, raw_clauses) ->
    let clauses = List.map reduce_clause raw_clauses in
    let initial_env = (nenv, tenv, eff, PEnv.empty) in
    let result =
      match_choices var clauses initial_env
    in
      Debug.if_set (show_pattern_compilation)
        (fun () -> "Compiled choices: "^(string_of_computation result));
      result
