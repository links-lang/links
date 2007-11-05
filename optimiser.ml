(** Optimisation passes: rewrites of the AST. *)

open List

open Utility
open Rewrite
open Syntax

let optimising = Settings.add_bool("optimising", true, `User)
let show_opt_verbose = Settings.add_bool("show_opt_verbose", false, `User)
let show_optimisation = Settings.add_bool("show_optimisation", false, `User)
let reduce_recs = Settings.add_bool("reduce_recursion", false, `User)

(**
   Check that "Project" and "Erase" do not occur in the syntax tree.
*)
let no_project_erase : RewriteSyntax.rewriter = function
  | Project _ 
  | Erase _ -> assert false
  | _ -> None

(** Inlining **)

(* Number of nodes in a syntax tree *)
let countNodes e = 
  let count = ref 0 in 
    Syntax.reduce_expression
      (fun recurse e -> incr count; recurse e)
      (fun _ -> ()) e; 
    !count;;

module Env = Env.String

(* Inline small, non-recursive functions *)
let contains_no_extrefs : Syntax.expression -> bool =
  (=) [] -<- List.filter (not -<- Env.has Library.type_env) -<- StringSet.elements -<- freevars

let recursivep : Syntax.expression -> bool = function
  | Rec ([(name, fn, _)], Variable (v, _), _) when v = name 
      -> StringSet.mem name (freevars fn)
  | _ -> false

let size_limit = 150

let is_inline_candidate' = function
  | Define (_, (Rec _ as e), _, _) -> 
      not (recursivep e) && contains_no_extrefs e && countNodes e < size_limit
  | Define (_, e, _, _) when Syntax.is_value e -> contains_no_extrefs e && Syntax.pure e
  | _ -> false

let find_inline_candidates es : (string * expression * location) list = 
  let is_inline_candidate = function
    | Define (name, rhs, location, _) as e when is_inline_candidate' e -> 
        [name, rhs, location]
    | _ -> []
  in Utility.concat_map is_inline_candidate es

let location_matches location = function
  | Define (_, _, location', _) -> location=location'
  | _ -> false

let perform_value_inlining location name rhs (Program (defs, body)) =
  Program (
    List.map (fun def ->
                if location_matches location def then
                  Syntax.subst_fast_def name rhs def
                else
                  def) defs,
    Syntax.subst_fast name rhs body)

let perform_function_inlining location name var rhs (Program (defs, body)) =
  let replace_application : RewriteSyntax.rewriter = function
    | Apply (Variable (n, _), [p], d) when n = name -> 
        (* FIXME: inlining only implemented for single-argument functions! *)
        Some (Let (var, p, rhs, d))
    | _ -> None
  in
    Program (
      List.map
        (fun def ->
           if location_matches location def then
             Syntax.rewrite_def (RewriteSyntax.bottomup replace_application) def
           else
             def) defs,
      from_option body (RewriteSyntax.bottomup replace_application body))

let inline (Program (defs, body) as program) = 
  let valuedefp = function
    | _, Rec _, _ -> false
    | _           -> true
  in
  let candidates = find_inline_candidates defs in
  let value_candidates, fn_candidates = List.partition valuedefp candidates in
  let program' = 
    List.fold_right 
      (fun (name, rhs, location) program ->
	 perform_value_inlining location name rhs program)
      value_candidates
      program
  in 
  let program'' = 
    List.fold_left 
      (fun program (_, rhs, location)  ->
         match rhs with
           | Rec ([(name, Abstr ([v], body, _), _)], _, _) ->
	       perform_function_inlining location name v body program
           | Rec _ ->
                program)
      program'
      fn_candidates
  in program''

(** Minimize the amount of recursion expressed in the syntax tree *)
let reduce_recursion : RewriteSyntax.rewriter = function
  | Rec (bindings, cont, data) ->
      let untyped_bindings = List.map (fun (name, expr, _) -> name, expr) bindings in
      let find_definition v = List.find (fun (name, expr, annotation) -> name = v) bindings in
      let recursive_p (name, expr, annot) =  StringSet.mem name (freevars expr) in
      let cliques = Callgraph.group_and_order_bindings_by_callgraph untyped_bindings in
        begin match map (map find_definition) cliques with
          | [_::_::_] -> None (* one multi-element group.  Everything must be mutually-recursive *)
          | [[x]] when recursive_p x -> None (* One single-element group; element is recursive. *)
          | groups -> (* either a group consisting of a single-element non-recursive element 
                         or multiple groups *)
              Some (List.fold_right
                      (fun group body -> 
                         match group with 
                           | [(name, expr, annot)] when not (StringSet.mem name (freevars expr)) ->
                               let rhs = match annot with
                                 | Some annot -> HasType (expr, annot, data)
                                 | None -> expr in
                                 Let (name, rhs, body, data)
                           | xs -> Rec (xs, body, data)) groups cont) end
  | _ -> None



(** [uniquify_names]

    Give unique names to all local bindings.  Local names should be
    distinct from each other and from toplevel names

    After this, we can be much less careful about scope.
*)
let uniquify_names : RewriteSyntax.rewriter = 
  let rewrite_node = function
    | Abstr (vs, b, data) -> 
        let names = List.map (fun v -> (v, gensym ~prefix:v ())) vs in
          Some (Abstr (List.map snd names, 
                       List.fold_right 
                         (uncurry Syntax.rename_fast)
                         names
                         b, data))
    | Let (v, e, b, data) -> 
        let name = gensym ~prefix:v () in
          Some (Let (name, e, Syntax.rename_fast v name b, data))
    | Rec (vs, b, data) -> 
        let bindings = List.map (fun (name, _, _) -> 
                                   (name, gensym ~prefix:name ())) vs in
        let rename = List.fold_right (fun (x, r) expr ->
                                        Syntax.rename_fast x r expr) bindings in
          Some (Rec (List.map (fun (n, v, t) ->
                                 (List.assoc n bindings, rename v, t)) vs,
                     rename b, data))
    | For (b, v, src, data) -> 
        let name = gensym ~prefix:v () in
          Some (For (Syntax.rename_fast v name b, name, src, data))
    | Variant_selection (value, clab, cvar, cbody, var, body, data) ->
        let cvar' = gensym ~prefix:cvar ()
        and var'  = gensym ~prefix:var () in
          Some (Variant_selection (value, clab, 
                                   cvar', Syntax.rename_fast cvar cvar' cbody, 
                                   var',  Syntax.rename_fast var var' body,
                                   data))
    | _ -> None
  (* Note that this will only work bottomup, not topdown, since
     we need to replace bindings from the inside out. (So that subtrees
     are already uniquified and thus we can use rename_fast.) *)
  in RewriteSyntax.bottomup rewrite_node

(** {0 Renaming} *)

(** renaming
    I think this is meant to "remove renamings", that is, remove instances of
    { var x = y; ... }
    It is incomplete, because the guard is very conservative. If we used
    capture-avoiding substitution, or ensured binder uniqueness in advance,
    we would be more complete.
*)
let renaming : RewriteSyntax.rewriter = 
  (* [bound_in x expr == true] iff [x] is bound anywhere inside [expr] *)
  let bound_in var = 
    let binds default = function
      | Let (v, _, _, _)
          when v = var -> true
      | Abstr (vs, _, _) when mem var vs -> true
      | Variant_selection (_, _, v1, _, v2, _, _) when var = v1 || var = v2 -> true
      | Rec (bindings, _, _) when List.exists (fun (v,_,_) -> v = var) bindings -> true
      | other -> default other
    and combiner l = fold_left (||) false l 
    in reduce_expression binds (combiner -<- snd) 
  in function 
        (* 1. Don't replace where there's an inner binding of the name `x'.
           2. Don't replace where there's an inner binding of the name `y'.
        *)
  | Let (y, Variable (x,  _), body,  _) when bound_in x body || bound_in y body
      -> None (* Could do better: does the binding shadow? *)
  | Let (y, Variable (x, _), body, _)
      -> Some (Syntax.rename_fast y x body)
  | _ -> None

(** [unused_variables]: Remove all let bindings where the name is not used 
    in the body.

    FIXME: also letrec ("transitively") and record selection
    operators.

    FIXME: needs to take account of variables used within Query
    expressions.
*)

let unused_variables : RewriteSyntax.rewriter = function
    (* This'd be quite a bit more efficient bottom-up, passing the
       list of free variables outwards rather than searching the body
       every time we find a let. *)
  | Let (var, expr, body, _) 
      when pure expr && not (StringSet.mem var (freevars body)) -> Some body
  | _ -> None
      (* FIXME: this ignores variables that are hidden inside queries *)



(** [simplify_regex] rewrites an expression as follows:
{
   s ~ let x in b
       let x in s ~ b
}

    (FIXME; Note there are lots of constructors that commute with let,
    and it might be quite wise to do so).

    In a regex match such as the following, the order of eval is unspecified:
    f() ~ /{g()}/
*)

let simplify_regex : RewriteSyntax.rewriter = function
  | Apply (Variable ("~", _) as tilde, [lhs; Let (v, e, rhs, d1)], d2)  ->
      Some (Let (v, e, Apply (tilde, [lhs;rhs], d1), d2))
  | _ -> None

(** {3 SQL optimisers} All following values are optimiser functions
    that can be applied to an expression. Generally, they try to push
    as much calculation to the DBMS as possible. {e Beware:} some
    optimisers have dependencies with other optimisers. *)

let lift_lets : RewriteSyntax.rewriter = function
  | For(loopbody, loopvar, Let(letvar, letval, letbody, letdata), data) 
    -> Some(Let(letvar, letval,
                For(loopbody, loopvar, letbody, data), letdata))
  | For(Let(letvar, letval, letbody, letdata), loopvar, src, data)
      when not (StringSet.mem loopvar (freevars letval))
        && pure letval
        -> Some(Let(letvar, letval, For(letbody, loopvar, src, data), letdata))
  | Condition(cond, Let(letvar, letval, letbody, letdata), e, data)
      when pure letval
        -> Some(Let(letvar, letval, Condition(cond, letbody, e, data), letdata))
  | Condition(cond, t, Let(letvar, letval, letbody, letdata), data)
      when pure letval
        -> Some(Let(letvar, letval, Condition(cond, t, letbody, data), letdata))
  | Apply(Let(letvar, letval, letbody, letdata), ap_arg, data)
        -> Some(Let(letvar, letval, Apply(letbody, ap_arg, data), letdata))
  | Apply(ap_func, [Let(letvar, letval, letbody, letdata)], data)
      when pure ap_func
        -> Some(Let(letvar, letval, Apply(ap_func, [letbody], data), letdata))
  | SortBy(Let(letvar, letval, letbody, letdata), byExpr, data) ->
      Some (Let(letvar, letval, SortBy(letbody, byExpr, data), letdata))
  | _ -> None


(* return true if the argument is an atom *)
let is_atom = function
  | Variable _ | Constant(Integer _, _) -> true
  | _ -> false

(** if [e] is not an atom then bind it to a variable by extending the
    continuation [k] return the new continuation and an atomic expression
    representing [e].
 *)
let lift_let data e k =
  if is_atom e then
    k, e
  else
    let x = gensym () in
      (fun body -> k (Let (x, e, body, data))), Variable (x, expression_data e)

let remove_trivial_extensions : RewriteSyntax.rewriter = function
  | For (List_of (Variable (v1, _), _), v2, e, _)
      when v1 = v2 -> Some e
  | _ -> None
     
(* Evaluate expressions involving only constants and pure functions at
 * compile time. *)
let fold_constant : RewriteSyntax.rewriter = 
  (* TODO: Also arithmetic, etc. *)
  let constantp = function
    | Constant _ | Nil _ -> true
    | Record_intro (fields, None, _) when StringMap.is_empty fields -> true
    | _ -> false 
  in function 
	(* Is this safe without unboxing? *)
(*    | Comparison (l, op, r, data) when constantp l && constantp r -> Some (Boolean ((assoc op ops) l r, data)) *)
    | Condition (Constant (Boolean true, _), t, _, _)  -> Some t
    | Condition (Constant (Boolean false, _), _, e, _) -> Some e
    | Concat (Nil _, c, _) 
    | Concat (c, Nil _, _) -> Some c
    | Concat (Constant(String l, _), Constant(String r, _), data) -> Some (Constant(String (l ^ r), data))
    | _ -> None 

(** Useful for printing the program at specific points of the
    optimisation pipeline.*)
let print_expression msg expr = 
  Debug.if_set show_optimisation
    (fun () -> msg ^ string_of_expression expr);
  None

(** Useful for checking a specific definition at specific points of the
    optimisation pipeline.*)
let print_definition of_name ?msg:msg def = 
 (match def with
    | Define (name, value, locn, _) when name = of_name 
        -> Debug.if_set show_optimisation
        (fun () -> from_option "" msg ^ string_of_definition def)
    | _ -> ());
  None


let rewriters env = [
  uniquify_names;
(*  RewriteSyntax.bottomup no_project_erase; *)
  RewriteSyntax.bottomup renaming;
  RewriteSyntax.bottomup unused_variables;
  if Settings.get_value reduce_recs then
    RewriteSyntax.topdown reduce_recursion
  else RewriteSyntax.never;
  RewriteSyntax.topdown simplify_regex;
  RewriteSyntax.loop (RewriteSyntax.bottomup lift_lets);
  RewriteSyntax.bottomup fold_constant;
  RewriteSyntax.topdown remove_trivial_extensions;
  Sqlcompile.sql_compile;
]

let run_optimisers
    : (Types.environment * Types.alias_environment) -> RewriteSyntax.rewriter
  = RewriteSyntax.all -<- rewriters

let optimise' env expr =
  if not (Settings.get_value optimising) then 
    None
  else
    let expr' = run_optimisers env expr in
    let _ =
        match expr' with
            None -> Debug.if_set_l show_optimisation
              (lazy "Optimization had no effect")
          | Some expr' -> 
              Debug.if_set_l show_optimisation
                (lazy(
                  (if (Settings.get_value show_opt_verbose) then 
                     "Before optimization : " ^ 
                       Show_stripped_expression.show (strip_data expr)
                   else "")
		  ^ "\nAfter optimization  : " ^
                    Show_stripped_expression.show (strip_data expr')))
    in
      expr'
    
let optimise env expr = from_option expr (optimise' env expr)

let optimise_program (env, (Program (defs, body))) =
  Program (
    List.map (rewrite_def (optimise' env)) defs,
    optimise env body)
