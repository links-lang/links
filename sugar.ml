open Num
open List

open Utility
open Syntax
open Sugartypes

exception ConcreteSyntaxError of (string * (Lexing.position * Lexing.position))
exception PatternDuplicateNameError of (position * string * string)

(* Internal representation for patterns. 

   This is slightly easier for the pattern compiler to work with than
   what comes out of the parser, which contains lots of n-ary
   patterns (e.g.  {x=a,y=b|z}).
*)
type 'r a_pattern = [
  | `Nil
  | `Cons of ('r * 'r)
  | `Variant of (string * 'r)
  | `Record of (string * 'r * 'r)
  | `Constant of untyped_expression
  | `Variable of string
  | `As of (string * 'r)
  | `HasType of ('r * Inferencetypes.datatype)
]

type simple_pattern = simple_pattern a_pattern * Syntax.untyped_data

(** {Expression Utilities} 
    Some utilities for constructing expressions.

    I'm absolutely un-crazy about these names; let's have a conference.
*)
let patternlist_to_tuplepattern ppos : ppattern list -> ppattern list = function
  | [p] -> [p]
  | ps -> [`Tuple ps, ppos]


(** [abstract_expr_curried_for_tuple_list ppos expr tuples] 

    Given an expression [expr] (possibly not closed) and a list of
    lists of variables [tuples], this abstracts the expression to form
    a curried function of several tuples (each tuple in [tuples]).
    The expression is in Sugar format, not Syntax.
*)
let abstract_expr_curried_for_tuple_list ppos expr tuples = 
  fold_left (fun result ps -> 
               (FunLit(None, patternlist_to_tuplepattern ppos ps, result), ppos)
            ) expr tuples

(** Construct a Links list out of a list of Links expressions; all
    will have the source position [pos].
*)
let make_links_list pos elems =
  let concat_expr l r = Concat(l, r, pos) in
    fold_right concat_expr elems (Nil pos)

(** Returns a (Syntax-format) function that plugs some given XML in as
    the contents of an XML element having the given tag name and attributes. *)
let make_xml_context tag (attrs:(string * untyped_expression) list) pos = 
  let hole = gensym() in
    Abstr(hole, Xml_node(tag, attrs, [Variable(hole, pos)], pos), pos)

let apply2_curried pos f x y =
  Apply(Apply(f, x, pos), y, pos)

(* Various flavours of a sort of `gensym'*)
let unique_name () = Utility.gensym ()
let db_unique_name = Utility.gensym ~prefix:"Table"

let list_head expr pos = 
  Apply(Variable ("hd", pos), expr, pos)

let list_tail expr pos = 
  Apply(Variable ("tl", pos), expr, pos)

let show_desugared = Settings.add_bool("show_desugared", false, `User)
let show_sugared = Settings.add_bool("show_sugared", false, `User)

let unit_hack = Settings.add_bool("pattern_unit_hack", false, `User)
let cons_unit_hack = Settings.add_bool("pattern_cons_unit_hack", true, `User)

exception RedundantPatternMatch of Syntax.position
module PatternCompiler =
  (*** pattern matching compiler ***)
  (*
    This is similar to the pattern matching compiler described by
    Phil Wadler in Chapter 5 of 'The Implementation of Functional
    Programming Languages, Simon Peyton Jones, 1987'.
    
    It is not yet optimised and can result in duplication of terms. In
    order to improve the pattern matching implementation, we may need
    to adjust our intermediate language.
  *)
  (struct
     let show_pattern_compilation = Settings.add_bool("show_pattern_compilation", false, `User)

     type annotation = string list * Inferencetypes.datatype list
     type annotated_pattern = annotation * simple_pattern

     type ('a, 'b) equation_basis = 'a list * 'b

     type raw_equation = (simple_pattern, untyped_expression) equation_basis
     type equation = (annotated_pattern, (untyped_expression * bool ref)) equation_basis
     type annotated_equation = annotation * equation

     let string_of_equation = string_of_expression -<- fst -<- snd

     type pattern_env = untyped_expression StringMap.t

     type bound_expression = pattern_env -> untyped_expression

     type pattern_type = [ | `List | `Variant | `Record | `Constant | `Variable ]

     let rec get_pattern_type : simple_pattern -> pattern_type = 
       fun (p,_) -> match p with
         | `Nil | `Cons _ -> `List
         | `Variant _ -> `Variant
         | `Record _ -> `Record
         | `Constant _ -> `Constant
         | `Variable _ -> `Variable
         | `As (_, pattern) -> get_pattern_type pattern
         | `HasType (pattern, _) -> get_pattern_type pattern

     let get_equation_pattern_type : equation -> pattern_type =
       fun ((_, pattern)::_, _) -> get_pattern_type pattern

     let get_equations_pattern_type : equation list -> pattern_type =
       fun (((_, pattern)::_, _)::_) -> get_pattern_type pattern

     let string_of_constant = function
       | Syntax.Boolean (v, _) -> string_of_bool v
       | Syntax.Integer (v, _) -> string_of_num v
       | Syntax.Char (v, _) -> string_of_char v
       | Syntax.String (v, _) -> v
       | Syntax.Float (v, _) -> string_of_float v
       | Syntax.Record_intro (fields, None, _) when StringMap.is_empty fields -> "()"

     (* compile away top-level As and HasType patterns *)
     let rec reduce_pattern : simple_pattern -> annotated_pattern = function
       | `As (name, pattern), _ ->
           let (names, datatypes), pattern = reduce_pattern pattern in
             (name::names, datatypes), pattern
       | `HasType (pattern, datatype), _ ->
           let (names, datatypes), pattern = reduce_pattern pattern in
             (names, datatype::datatypes), pattern
       | pattern -> ([], []), pattern

     let reduce_equation : raw_equation -> equation =
       fun (ps, body) ->
         (map reduce_pattern ps, (body, ref false))

     (* partition equations sequentially by pattern type *)
     let partition_equations : equation list -> (equation list) list =
       function
         | [] -> []
         | equations ->
             let (_, es, ess) =
               List.fold_right
                 (fun equation (t, es, ess) ->
                    let t' = get_equation_pattern_type equation in
                    let es', ess' =
                      (* group non-variable patterns of the same type *)
                      if es = [] || ((t' = t) && (t' <> `Variable)) then
                        equation::es, ess
                      else
                        [equation], es::ess
                    in
                      (t', es', ess')) equations (`Variable, [], [])
             in
               es::ess

     (* partition list equations by constructor *)
     let partition_list_equations : equation list -> (annotated_equation list * annotated_equation list) =
       fun equations ->
        List.fold_right (fun (ps, body) (nil_equations, cons_equations) ->
                           match ps with
                             | (annotation, (`Nil,_))::ps ->
                                 (annotation, (ps, body))::nil_equations, cons_equations
                             | (annotation, (`Cons (px, pxs),_))::ps ->
                                 let px = reduce_pattern px in 
                                 let pxs = reduce_pattern pxs in
                                   nil_equations, (annotation, (px::pxs::ps, body))::cons_equations
                             | _ -> assert false) equations ([], [])

     (* partition variant equations by constructor *)
     let partition_variant_equations
         : equation list -> (string * ((string * string) * annotated_equation list)) list =
       fun equations ->
         assoc_list_of_string_map
           (List.fold_right
              (fun (ps, body) env ->
                 match ps with
                   | (annotation, (`Variant (name, pattern),_))::ps ->
                       let vars, annotated_equations = 
                         if StringMap.mem name env then
                           StringMap.find name env
                         else
                           (unique_name (), unique_name ()), [] in
                       let pattern = reduce_pattern pattern
                       in
                         StringMap.add name (vars, (annotation, (pattern::ps, body))::annotated_equations) env
                   | _ -> assert false
              ) equations StringMap.empty)

     (* partition record equations by label *)
     let partition_record_equations
         : equation list -> (string * ((string * string) * annotated_equation list)) list =
       fun equations ->
         assoc_list_of_string_map
           (List.fold_right
              (fun (ps, body) env ->
                 match ps with
                   | (annotation, (`Record (name, pattern, ext_pattern),_))::ps ->
                       let vars, annotated_equations =
                         if StringMap.mem name env then
                           StringMap.find name env
                         else
                           (unique_name (), unique_name ()), [] in
                       let pattern = reduce_pattern pattern in
                       let ext_pattern = reduce_pattern ext_pattern
                       in
                         StringMap.add name (vars, (annotation, (pattern::ext_pattern::ps, body))::annotated_equations) env
                   | _ -> assert false
              ) equations StringMap.empty)

     (* partition constant equations by constant value *)
     let partition_constant_equations
         : equation list -> (string * (untyped_expression * annotated_equation list)) list =
       fun equations ->
         assoc_list_of_string_map
          (List.fold_right
              (fun (ps, body) env ->
                 match ps with
                   | (annotation, (`Constant exp,_))::ps ->
                       let name = string_of_constant exp in
                       let exp, annotated_equations = 
                         if StringMap.mem name env then
                           StringMap.find name env
                         else
                           exp, []
                       in
                         StringMap.add name (exp, (annotation, (ps, body))::annotated_equations) env
                   | _ -> assert false
             ) equations StringMap.empty)

     (* 
        create a let binding,
        inlining it if the bound expression is a variable
     *)
     let bind_or_subst (var, exp, body, pos) =
       match exp with
         | Variable (var', _) ->
             Syntax.rename_free var var' body
         | _ -> Let (var, exp, body, pos)

     (* 
        apply an annotation to an expression
         - rename variables
         - move type annotations into the expression
     *)
     let apply_annotation : Syntax.untyped_data -> untyped_expression -> annotation * untyped_expression -> untyped_expression =
       fun pos exp ((names, datatypes), body) ->
         let body = List.fold_right (fun name body ->
                                      bind_or_subst (name, exp, body, pos)) names body in
         let body = List.fold_right (fun datatype body ->
                                      Let ("_", HasType (exp, datatype, pos), body, pos)) datatypes body
         in
           body

     (* apply annotations in an annotated equation list *)
     let apply_annotations : Syntax.untyped_data -> untyped_expression -> annotated_equation list -> equation list =
       fun pos exp annotated_equations ->
         map (fun (annotation, (ps, (body, used))) ->
                (ps, (apply_annotation pos exp (annotation, body), used))) annotated_equations

     (*
       [TODO]
         - better error messages
     *)
     (*
       Redundant patterns
       ------------------
 
       We say that an equation or pattern matching clause is redundant
       if its continuation can never be invoked. We only care about
       redundancy of the raw equations passed as input to the
       pattern matching compiler (i.e. the clauses of a switch
       statement in the source code). We say that a switch statement is
       redundant if at least one of its clauses is redundant.

       Two kinds of redundancy can arise in pattern matching:
        - control-flow:
          when it can be proven that a continuation can never be
          invoked, by a static analysis of the control flow
        - type-oriented:
          when it can be proven that a continuation can never be
          invoked, using type information

       The pattern-matching compiler is deliberately designed *not* to
       take into account type information, in order that it can be used
       effectively with static typing disabled. Therefore we do not
       attempt to detect type-oriented redundancy here.

       We do detect (and report as an error) control-flow redundancy.
       Control-flow redundancy checking is implemented by attaching a
       'used' flag to the body of each continuation. The used flag is
       initialised to false, and set to true whenever a continuation
       is used in the compiled program. After compiling the equations
       a check is made to ensure that all continuations have been used.

       Control-flow redundancy arises when two consecutive patterns
       are equivalent under the assumption that variables in the first
       pattern can be instantiated to any pattern.

       Examples:
        - switch (x) { case(x,1) -> 1; case (0,1) -> 2;}
       is redundant because 0 in the second pattern has already been
       matched by x in a context where all the other sub-patterns are
       identical
    
        -  switch (x) { case(A(B(C(1,2,3)))) -> 0; case(A(B(C(1,2,3)))) -> 1;}
       is redundant because the two patterns are identical

       An example of type-oriented redundancy:
        - switch (x) { case(true) -> 0; case(false) -> 1; case(x) -> 2 }
       The third clause is redundant as true and false cover the whole
       of the type Bool.
     *)

     (*
       Nonexhaustive matches
       ---------------------
       
       We do not attempt to detect non-exhaustive matches as this
       requires a type-directed analysis. However, the way we handle
       variant matching ensures that non-exhaustive matches of
       variants will be ill-typed.

       Non-exhaustive matches can occur when matching constant or
       list patterns.
     *)

     (*
       [NOTE]
         this abstraction is deliberate as we may want to change it - e.g.
         if we want to share default continuations in the resulting term
         rather than simply inlining them as we do now.
     *)
     let apply_default def env =
       def env

     let lookup var (env : pattern_env) =
       try
         StringMap.find var env
       with
           Not_found -> failwith ("Variable: "^var^" not in environment (lookup)")

     let is_trivial var (env : pattern_env) =
       match lookup var env with
         | Variable (var', _) when var=var' -> true
         | _ -> false

     (* add a binding to the environment
        first applying it to all the other expressions in the environment
     *)
     let extend var exp (env : pattern_env) =
       let env = StringMap.fold
         (fun var' exp' env ->
            StringMap.add var' (Syntax.subst_free var exp exp') env) env StringMap.empty 
       in
         StringMap.add var exp env

     (* extend the environment with a list of trivial bindings
        i.e binding each var_i to Variable(var_i, pos)

        (we assume the bindings do not occur in the existing environment,
        so do not have to traverse the existing environment)
     *)
     let extend_trivial pos vars : pattern_env -> pattern_env =
       List.fold_right (fun var env ->
                          extend var (Variable (var, pos)) env) vars
         
     (* the entry point to the pattern-matching compiler *)
     let rec match_cases
         : Syntax.untyped_data -> string list -> equation list ->
       bound_expression -> bound_expression          
         =
       fun pos vars equations def (env : pattern_env) ->
         match vars, equations with 
           | [], [] -> apply_default def (env : pattern_env)
           | [], ([], (body, used))::_ -> used := true; body
(* identical patterns could be detected here if we wanted *)
(*           | [], ([], _)::_ -> failwith "Redundant pattern"*)
           | var::vars, _ ->
               let equationss = partition_equations equations in
                 List.fold_right
                   (fun equations exp ->
                      (match get_equations_pattern_type equations with
                         | `List ->
                             match_list pos vars (partition_list_equations equations) exp var
                         | `Variant ->
                             match_variant pos vars (partition_variant_equations equations) exp var
                         | `Variable ->
                             match_var pos vars equations exp var
                         | `Record ->
                             match_record pos vars (partition_record_equations equations) exp var
                         | `Constant ->
                             match_constant pos vars (partition_constant_equations equations) exp var)
                   ) equationss def env

     and match_var
         : Syntax.untyped_data -> string list -> equation list ->
       bound_expression -> string -> bound_expression
         =
       fun pos vars equations def var (env : pattern_env) ->
         match_cases pos vars
           (List.map (fun ((annotation, pattern)::ps, (body, used)) ->
                        let var_exp = lookup var env in
                        let body = apply_annotation pos var_exp (annotation, body)
                        in
                          match pattern with
                            | (`Variable var',_) ->
                                (ps,
                                 (bind_or_subst (var', var_exp, body, pos), used))
                            | _ -> assert false) equations) def env

     and match_list
         : Syntax.untyped_data -> string list -> (annotated_equation list * annotated_equation list)
           -> bound_expression -> string -> bound_expression =
       fun pos vars (nil_equations, cons_equations) def var env ->
         let var_exp = lookup var env in

         let nil_equations = apply_annotations pos var_exp nil_equations in
         let cons_equations = apply_annotations pos var_exp cons_equations in
         let nil_branch =
           match nil_equations with
             | [] -> apply_default def env
             | _ ->
                 match_cases pos vars nil_equations def env in
         let cons_branch =
           match cons_equations with
             | [] -> apply_default def env
             | _ ->
                 let x = unique_name () in
                 let xs = unique_name () in
                 let env = extend_trivial pos [x; xs] env
                 in
                   Let(x, list_head var_exp pos,
                       Let(xs, list_tail var_exp pos,
                           match_cases pos (x::xs::vars) cons_equations def env,
                           pos), pos)
         in
           (Condition(Comparison(var_exp, `Equal, Syntax.Nil pos, pos),
                      nil_branch,
                      cons_branch, pos))

     and match_variant
         : Syntax.untyped_data -> string list -> ((string * ((string * string) * annotated_equation list)) list) ->
           bound_expression -> string -> bound_expression =
       fun pos vars bs def var env ->
         match bs with
           | [] ->
               apply_default def env
           | (name, ((case_variable, default_variable), annotated_equations))::bs ->
               let var_exp = lookup var env in

               let var_as_exp var = Variable (var, pos) in
               let inject var = Variant_injection(name, var_as_exp var, pos) in
               let empty var = Variant_selection_empty(var_as_exp var, pos) in
                   
               let equations = apply_annotations pos (inject case_variable) annotated_equations in
                 (*
                     close variant types when possible
                     
                   [NOTE]
                   this has the side-effect of manifesting non-exhaustive matches
                   on polymorphic variants as type errors.
                 *)
               let massage_wrong var = function
                 | Wrong _ ->
                     empty var
                 | e -> e in


               let match_branch case_variable =
                 let match_env =
                   extend_trivial pos [case_variable]
                     (extend var (inject case_variable) env)
                 in
                   match_cases pos (case_variable::vars) equations def match_env in
               let default_branch default_variable =
                 let default_env = 
                   extend_trivial pos [default_variable]
                     (extend var (var_as_exp default_variable) env)
                 in
                   massage_wrong default_variable
                     (match_variant pos vars bs def default_variable default_env) (*in*)

               in
(* is this worth doing here / does it ever happen? *)
(*
                 match lookup var env with
                   | Variant_injection(name', Variable(case_variable, _), _) ->
                       if name=name' then
                          match_branch case_variable
                       else
                         default_branch default_variable
                   | _ ->
*)
                         Variant_selection(var_exp, name,
                                           case_variable,
                                           match_branch case_variable,
                                           default_variable,
                                           default_branch default_variable,
                                           pos)       
     and match_record
         : Syntax.untyped_data -> string list ->
           ((string * ((string * string) * annotated_equation list)) list) ->
           bound_expression -> string -> bound_expression =
       fun pos vars bs def var env ->
         match bs with
           | [] -> apply_default def env
           | (name, ((label_variable, extension_variable), annotated_equations))::bs ->
               let var_exp = lookup var env in
               let equations = apply_annotations pos var_exp annotated_equations in
(* is this worth doing here / does it ever happen? *)
(*
                 match var_exp with
                   | Record_intro(StringMap.add name' (Variable (label_variable, _)) StringMap.empty,
                                      Some (Variable (extension_variable, _)),
                                      _) when name=name' ->
                       match_cases
                         pos
                         (label_variable::extension_variable::vars)
                         equations
                         (match_record pos vars bs def var)
                         env
                   | _ ->
*)
               let env =
                 extend_trivial pos [label_variable; extension_variable]
                   (extend var (Record_intro(
                                  StringMap.add name (Variable (label_variable, pos)) StringMap.empty,
                                  Some (Variable (extension_variable, pos)),
                                  pos))
                      env)
               in                        
                 Record_selection
                   (name,
                    label_variable,
                    extension_variable,
                    var_exp,
                    match_cases
                      pos
                      (label_variable::extension_variable::vars)
                      equations
                      (match_record pos vars bs def var)
                      env,
                    pos)

     and match_constant
         : Syntax.untyped_data -> string list -> (string * (untyped_expression * annotated_equation list)) list
           -> bound_expression -> string -> bound_expression =
       fun pos vars bs def var env ->
         match bs with
           | [] -> apply_default def env
           | (name, (exp, annotated_equations))::bs ->
               let var_exp = lookup var env in
               let equations = apply_annotations pos var_exp annotated_equations in
                 (match exp with
                    | Record_intro (fields, None, _)
                        when Settings.get_value unit_hack 
                          && StringMap.is_empty fields ->
                        (* 
                           This is the only place in the pattern
                           matching compiler that we do type-directed
                           optimisation. By default unit_hack is
                           disabled and cons_unit_hack is enabled (see
                           the use of cons_unit_hack below for further
                           details).

                           Enabling unit_hack allows the compiler to
                           make the assumption that a comparison with
                           unit will always succeed. This assumption
                           is sound providing we're using static
                           typing.
                        *)
                        Let ("_", HasType (var_exp, Inferencetypes.unit_type, pos),
                             match_cases pos vars equations def env, pos)
                    | _ ->
                        (Condition(Comparison(var_exp, `Equal, exp, pos),
                                   match_cases pos vars equations def env,
                                   match_constant pos vars bs def var env,
                                   pos)))
                   
     (* the interface to the pattern-matching compiler *)
     let match_cases
         : (Syntax.untyped_data * untyped_expression * raw_equation list) -> untyped_expression =
       fun (`U (p1, _, _) as pos, exp, raw_equations) ->
         Debug.if_set (show_pattern_compilation)
           (fun () -> "Compiling pattern match: "^ p1.Lexing.pos_fname);
         let var, wrap =
           match exp with
             | Variable (var, _) ->
                 var, Utility.identity
             | _ ->
                 let var = unique_name()
                 in
                   var, fun body -> Let (var, exp, body, pos)
         and equations = map reduce_equation raw_equations in
         let initial_env = StringMap.add var (Variable (var, pos)) StringMap.empty in
         let result = wrap (match_cases pos [var] equations (fun _ -> Wrong pos) (initial_env : pattern_env))
         in
           Debug.if_set (show_pattern_compilation)
             (fun () -> "Compiled pattern: "^(string_of_expression result));
           if (List.for_all (fun (_, (_, used)) -> !used) equations) then
             result
           else
             raise (RedundantPatternMatch (Syntax.data_position pos))
   end 
     : 
    sig
      type raw_equation = simple_pattern list * untyped_expression
      val match_cases : (Syntax.untyped_data * untyped_expression * raw_equation list) -> untyped_expression
    end)

module Desugarer =
  (* Convert a syntax tree as returned by the parser into core syntax *)
(struct
   type quantifier = [`TypeVar of string | `RigidTypeVar of string | `RowVar of string]

   (* Generation of fresh type variables *)
   let type_variable_counter = ref 0
   let fresh_type_variable : unit -> datatype =
     function () -> 
       incr type_variable_counter; TypeVar ("_" ^ string_of_int (!type_variable_counter))


   let rec typevars : datatype -> quantifier list = 
     let rvars (fields, rv) =
       let rowvars = match rv with
         | None   -> []
         | Some s -> [`RowVar s] in
         (Utility.concat_map 
            (function (_, `Present k) -> typevars k
               | _ -> [])
            fields) @ rowvars
     in function
       | TypeVar s -> [`TypeVar s]
       | RigidTypeVar s -> [`RigidTypeVar s]
       | FunctionType (s, m, t) -> Utility.unduplicate (=) (typevars s @ typevars m @ typevars t)
       | MuType (v, k) -> snd (partition ((=)(`RigidTypeVar v)) (typevars k))
       | TupleType ks -> Utility.concat_map typevars ks
       | RecordType r
       | VariantType r
       | TableType r -> rvars r
       | ListType k -> typevars k
       | TypeApplication (_,ks) -> Utility.concat_map typevars ks
       | UnitType
       | PrimitiveType _
       | DBType -> []

   type assumption = quantifier list * datatype

   let generalize (k : datatype) : assumption =
     typevars k, k

   type var_env =
       (Inferencetypes.datatype Unionfind.point) StringMap.t *
         (Inferencetypes.row Unionfind.point) StringMap.t 

   let generate_var_mapping : quantifier list -> (Inferencetypes.quantifier list * var_env) =
     fun vars ->
       List.fold_right
         (fun v (vars, (tenv, renv)) ->
            let var = Inferencetypes.fresh_raw_variable () in
              match v with
                | `TypeVar name ->
                    (`TypeVar var::vars,
                     (StringMap.add name
                        (Unionfind.fresh (`TypeVar var)) tenv, renv))
                | `RigidTypeVar name ->
                    (`RigidTypeVar var::vars,
                     (StringMap.add name
                        (Unionfind.fresh (`RigidTypeVar var)) tenv, renv))
                | `RowVar name ->
                    (`RowVar var::vars,
                     (tenv, StringMap.add name
                        (Unionfind.fresh (StringMap.empty, `RowVar (Some var))) renv))) vars ([], (StringMap.empty, StringMap.empty))

   let desugar_datatype, desugar_row =
     let rec desugar ((tenv, renv) as var_env) =
       let lookup_type = flip StringMap.find tenv in
(*       let extend = fun (name, value) -> StringMap.add name value varmap in*)
         function
           | TypeVar s -> (try `MetaTypeVar (lookup_type s)
                           with Not_found -> failwith ("Not found `"^ s ^ "' while desugaring assumption"))
           | RigidTypeVar s -> (try `MetaTypeVar (lookup_type s)
                                with Not_found -> failwith ("Not found `"^ s ^ "' while desugaring assumption"))
           | FunctionType (f, m, t) ->
               `Function (desugar var_env f, desugar var_env m, desugar var_env t)
           | MuType (name, t) ->
               let var = Inferencetypes.fresh_raw_variable () in
               let point = Unionfind.fresh (`TypeVar var) in
               let tenv = StringMap.add name point tenv in
               let _ = Unionfind.change point (`Recursive (var, desugar (tenv, renv) t)) in
                 `MetaTypeVar point
           | UnitType -> Inferencetypes.unit_type
           | TupleType ks -> 
               let labels = map string_of_int (Utility.fromTo 1 (1 + length ks)) 
               and unit = Inferencetypes.InferenceTypeOps.make_empty_closed_row ()
               and present (s, x) = (s, `Present x)
               in `Record (fold_right2 (curry (Inferencetypes.InferenceTypeOps.set_field -<- present)) labels (map (desugar var_env) ks) unit)
           | RecordType row -> `Record (desugar_row var_env row)
           | VariantType row -> `Variant (desugar_row var_env row)
           | TableType row -> `Table (desugar_row var_env row)
           | ListType k -> `Application ("List", [desugar var_env k])
           | TypeApplication (t, k) -> `Application (t, List.map (desugar var_env) k)
           | PrimitiveType k -> `Primitive k
           | DBType -> `Primitive `DB
     and desugar_row ((_, renv) as var_env) (fields, rv) =
       let lookup_row = flip StringMap.find renv in
       let seed = match rv with
         | None    -> Inferencetypes.InferenceTypeOps.make_empty_closed_row ()
         | Some rv ->
             (StringMap.empty, `MetaRowVar (lookup_row rv))
(*             Inferencetypes.InferenceTypeOps.make_empty_open_row_with_var (lookup rv)*)
       and fields = map (fun (k, v) -> match v with
                           | `Absent -> (k, `Absent)
                           | `Present v -> (k, `Present (desugar var_env v))) fields 
       in fold_right Inferencetypes.InferenceTypeOps.set_field fields seed
     in desugar, desugar_row

   let desugar_assumption ((vars, k)  : assumption) : Inferencetypes.assumption = 
     let vars, var_env = generate_var_mapping vars in
       vars, desugar_datatype var_env k

   let rec get_type_vars : phrase -> quantifier list =
     let empty = [] in
     let union = (unduplicate (=)) -<- List.concat in
     let flatten = List.concat in
     let rec get_type_vars =
       fun (s, _) ->
         let tv datatype = [typevars datatype] in
         let etv = get_type_vars in
         let etvs = flatten -<- (List.map get_type_vars) in
         let opt_etv = function
           | None -> empty
           | Some e -> etv e in
         let opt_etv2 = function
           | None -> empty
           | Some (_, e) -> etv e in
         let ptv = get_pattern_type_vars in
         let btv (p, e) = flatten [ptv p; etv e] in
         let btvs = flatten -<- (List.map btv) in
         let gtv = function
           | `List b
           | `Table b -> btv b in
         let ftv (_, e) = etv e in
         let ftvs = flatten -<- (List.map ftv)
         in
           match s with
             | FloatLit _
             | IntLit _
             | StringLit _
             | BoolLit _
             | CharLit _
             | InfixDecl
             | Var _ -> empty
             | FunLit (_, patterns, body) -> flatten ((List.map ptv patterns) @ [etv body])
             | Spawn e -> etv e
             | ListLit es -> etvs es
             | Definition (_, e, _) -> etv e
             | Iteration (generator, body, filter, sort) ->
                 flatten [gtv generator; etv body; opt_etv filter; opt_etv sort]
             | Escape (_, e) ->  etv e
             | HandleWith (e1, _, e2) -> flatten [etv e1; etv e2]
             | Section _ -> empty
             | Conditional (e1, e2, e3) -> flatten [etv e1; etv e2; etv e3]
             | Binding b -> btv b
             | Block (es, exp) -> flatten [etvs es; etv exp]
             | Foreign (_, _, datatype) -> tv datatype
             | InfixAppl (_, e1, e2) -> flatten [etv e1; etv e2]
             | Regex _ -> empty
             | UnaryAppl (_, e) -> etv e
             | FnAppl (fn, (ps, _)) -> flatten [etv fn; etvs ps]
             | TupleLit fields -> etvs fields
             | RecordLit (fields, e) ->
                 flatten ((List.map (fun (_, field) -> etv field) fields) @ [opt_etv e])
             | With (e, fields) -> 
                 flatten ((List.map (fun (_, field) -> etv field) fields) @ [etv e])
             | Projection (e, _) -> etv e
             | SortBy_Conc(pattern, expr, sort_expr) -> flatten [ptv pattern; etv expr; etv sort_expr]

             | TypeAnnotation(e, k) -> flatten [etv e; tv k]
             | TypeDeclaration (_, args, datatype) -> [List.map (fun k -> `RigidTypeVar k) args] @ tv datatype

             | ConstructorLit (_, e) -> opt_etv e
             | Switch (exp, binders) -> flatten [etv exp; btvs binders]
             | Receive (binders) -> btvs binders

             | DatabaseLit (name, (opt_driver, opt_args)) -> flatten [etv name; opt_etv opt_driver; opt_etv opt_args]
             | TableLit (_, datatype, db) -> flatten [tv datatype; etv db]
             | DBInsert (e1, e2) -> flatten [etv e1; etv e2]
             | DBDelete ((p, e1), e2) -> flatten [ptv p; etv e1; opt_etv e2]
             | DBUpdate ((p, e1), e2, fs) -> flatten [ptv p; etv e1; opt_etv e2; ftvs fs]

             | Xml (_, attrs, subnodes) ->
                 flatten ((List.map (fun (_, es) -> etvs es) attrs) @ [etvs subnodes])
             | XmlForest es -> etvs es
             | TextNode _ -> empty
             | Form (e1, e2) -> flatten [etv e1; etv e2]
             | FormBinding (e, p) -> flatten [etv e; ptv p]
     and get_pattern_type_vars (p, _) = (* fold *)
       match p with 
         | `Any
         | `Nil
         | `Variable _
         | `Variant (_, None)   -> []
         | `Variant (_, Some p)
         | `As (_, p)           -> get_pattern_type_vars p
         | `Cons (l, r)         -> get_pattern_type_vars l @ get_pattern_type_vars r
         | `Record (ps, Some p) -> concat_map (snd ->- get_pattern_type_vars) ps @ get_pattern_type_vars p
         | `Record (ps, None)   -> concat_map (snd ->- get_pattern_type_vars) ps
         | `List ps
         | `Tuple ps            -> concat_map get_pattern_type_vars ps 
         | `Constant pn         -> get_type_vars pn
         | `HasType (p, t)      -> get_pattern_type_vars p @ [typevars t]
     in
       union -<- get_type_vars

   (** With respect to scope of variables bound at the same level the
       rules are these:
       
       * Adjacent function definitions are mutually recursive with
       respect to the scope of their names.
       
       * Other function definitions are simply recursive
       
       * All other RHS can refer to previously established bindings
       (i.e.  bindings occuring textually previously.
   *)
   (* pattern-matching let *)
   let polylet : simple_pattern -> untyped_data -> untyped_expression -> untyped_expression -> untyped_expression =
     fun pat pos value body ->
       let rec pl pat pos value body =
         match fst pat with
           | `Nil ->
               (Condition(Comparison(value, `Equal, Syntax.Nil pos, pos),
                          body,
                          Syntax.Wrong pos, pos))
           | `Cons (head, tail) ->
               let name = unique_name () in
               let var = Variable (name, pos) in
                 Let(name, value,
                     pl head pos (list_head var pos)
                       (pl tail pos (list_tail var pos) body), pos)
           | `Variant (name, patt) ->
               let case_variable = unique_name () in
               let variable = unique_name () in
                 Variant_selection(value, name,
                                   case_variable,
                                   (pl patt pos (Variable (case_variable, pos)) body),
                                   variable,
                                   Variant_selection_empty(Variable(variable, pos), pos),
                                   pos)
           | `Record (label, patt, rem_patt) ->
               let temp_var_field = unique_name () in
               let temp_var_ext = unique_name () in
                 Record_selection (label,
                                   temp_var_field,
                                   temp_var_ext,
                                   value,
                                   pl patt pos
                                     (Variable (temp_var_field, pos))
                                     (pl rem_patt pos
                                        (Variable (temp_var_ext, pos))
                                        body),
                                   pos)
           | `Constant c ->
               (Condition(Comparison(value, `Equal, c, pos),
                          body,
                          Syntax.Wrong pos, pos))
           | `Variable name -> Let (name, value, body, pos)
           | `As (name, pattern) ->
               Let (name, value, pl pattern pos (Variable (name, pos)) body, pos)
           | `HasType (pat, t) ->
               pl pat pos (HasType (value, t, pos)) body
       in
         pl pat pos value body

   let rec polylets (bindings : (simple_pattern * untyped_expression * untyped_data * bool) list) expression =  
     let folder (patt, value, pos, recp) expr = 
       match patt, value, expr, recp with 
         | (`Variable s, _), Abstr _, Rec (bindings, e, p), _ ->  
             Rec ((s, value, None)  :: bindings, e, p) 
         | (`Variable s, _), Abstr _, _, true ->  
             Rec ([s, value, None], expr, pos) 
         | _ ->  
             polylet patt pos value expr in 
       fold_right folder bindings expression 


   let func (pos : untyped_data) (body : untyped_expression) : simple_pattern -> untyped_expression = function
     | `Variable name, _ -> Abstr (name, body, pos)
     | pat -> let temp_var = unique_name () in Abstr (temp_var, polylet pat pos (Variable (temp_var, pos)) body, pos)
                                              
   let rec polyfunc (patterns : simple_pattern list) (pos : untyped_data) (expr : untyped_expression) : untyped_expression =
     match patterns with 
       | [] -> raise (ASTSyntaxError (data_position pos, "At least one parameter must be defined for a function")) 
       | [patt] -> func pos expr patt 
       | patt :: patts -> func pos (polyfunc patts pos expr) patt

   let string_of_pattern_pos ((pos : Lexing.position), _, expr) = 
     Printf.sprintf "%s:%d:%s" pos.Lexing.pos_fname pos.Lexing.pos_lnum expr

   (* give an error if the pattern has duplicate names *)
   let check_for_duplicate_names ((pattern, pos) : simple_pattern) =
     let rec check_and_add name env =
       if StringSet.mem name env then
         let (pos, dpos, expr) = data_position pos in 
         raise(PatternDuplicateNameError((pos, dpos, expr), name, expr))
(*         raise (ASTSyntaxError(data_position pos, "Duplicate name '"^ name  ^"' in pattern "^string_of_pattern_pos (data_position pos))) *)
       else
         StringSet.add name env in
     let rec check env =
       function
         | `Nil -> env
         | `Cons ((pattern,_), (pattern',_)) ->
             let env' = check env pattern in
               check env' pattern'
         | `Variant (_, (pattern,_)) ->
             check env pattern
         | `Record (_, (pattern,_), (pattern',_)) ->
             let env' = check env pattern in
               check env' pattern'
         | `Constant _ -> env
         | `Variable name -> check_and_add name env
         | `As (name, (pattern,_)) ->
             let env = check_and_add name env in
               check env pattern
         | `HasType ((pattern,_), _) ->
             check env pattern
     in
       ignore (check StringSet.empty pattern)

   let as_list pos = function
     | `List (p, e) -> p, e
     | `Table (p, e) -> p, (FnAppl ((Var ("asList"), pos), ([e], pos)), pos)

   let desugar lookup_pos (e : phrase) : untyped_expression =
     let _, ((tenv, renv) as var_env) = (generate_var_mapping -<- get_type_vars) e in
     let rec desugar' lookup_pos ((s, pos') : phrase) : untyped_expression =
       let pos = `U (lookup_pos pos') in
       let desugar = desugar' lookup_pos
       and patternize = simple_pattern_of_pattern var_env lookup_pos in
         match s with
           | TypeAnnotation ((Definition (name, (FunLit (Some _, patterns, body),_), loc), _), t)  -> 
               Define (name,
                       Rec ([name, desugar (FunLit (None, patterns, body), pos'), Some (desugar_datatype var_env t)],
                            Variable (name, pos),
                            pos),
                       loc,pos)
           | TypeAnnotation ((Definition (name, rhs, loc), _), t)  -> 
               Define (name, HasType(desugar rhs, desugar_datatype var_env t, pos),loc, pos)
           | TypeAnnotation(e, k) -> HasType(desugar e, desugar_datatype var_env k, pos)
           | FloatLit f  -> Float (f, pos)
           | IntLit i    -> Integer (i, pos)
           | StringLit s -> HasType(String (s, pos), Inferencetypes.string_type, pos)
           | BoolLit b   -> Boolean (b, pos)
           | CharLit c   -> Char (c, pos)
           | Var v       -> Variable (v, pos)
           | InfixAppl (`Name ">", e1, e2)  -> Comparison (desugar e2, `Less, desugar e1, pos)
           | InfixAppl (`Name ">=", e1, e2)  -> Comparison (desugar e2, `LessEq, desugar e1, pos)
           | InfixAppl (`Name "==", e1, e2)  -> Comparison (desugar e1, `Equal, desugar e2, pos)
           | InfixAppl (`Name "<", e1, e2)  -> Comparison (desugar e1, `Less, desugar e2, pos)
           | InfixAppl (`Name "<=", e1, e2)  -> Comparison (desugar e1, `LessEq, desugar e2, pos)
           | InfixAppl (`Name "<>", e1, e2)  -> Comparison (desugar e1, `NotEq, desugar e2, pos)
           | InfixAppl (`Name "++", e1, e2)  -> Concat (desugar e1, desugar e2, pos)
           | InfixAppl (`Name "!", e1, e2)  -> Apply (Apply (Variable ("send", pos), desugar e1, pos), desugar e2, pos) 
           | InfixAppl (`Name n, e1, e2)  -> Apply (Apply (Variable (n, pos), desugar e1, pos), desugar e2, pos) 
           | InfixAppl (`Cons, e1, e2) -> Concat (List_of (desugar e1, pos), desugar e2, pos)
           | InfixAppl (`RegexMatch, e1, (Regex r, _)) -> Apply (Apply (Variable ("~", pos), desugar e1, pos), 
                                                                 desugar (desugar_regex desugar pos' r, pos'), pos)
           | InfixAppl (`RegexMatch, _, _) -> raise (ASTSyntaxError(Syntax.data_position pos, "Internal error: unexpected rhs of regex operator"))
           | InfixAppl (`FloatMinus, e1, e2)  -> Apply (Apply (Variable ("-.", pos), desugar e1, pos), desugar e2, pos) 
           | InfixAppl (`Minus, e1, e2)  -> Apply (Apply (Variable ("-", pos), desugar e1, pos), desugar e2, pos) 
           | InfixAppl (`And, e1, e2) -> Condition (desugar e1, desugar e2, Boolean (false, pos), pos)
           | InfixAppl (`Or, e1, e2)  -> Condition (desugar e1, Boolean (true, pos), desugar e2, pos)
           | ConstructorLit (name, None) -> Variant_injection (name, unit_expression pos, pos)
           | ConstructorLit (name, Some s) -> Variant_injection (name, desugar s, pos)
           | Escape (name, e) -> 
               Syntax.Call_cc(Abstr(name, desugar e, pos), pos)
           | Spawn e -> desugar (FnAppl ((FnAppl ((Var "spawn", pos'), 
                                                  ([FunLit (None, [`Record ([],None), pos'], e), 
                                                    pos'], pos')),
                                          pos'), ([], pos')), pos')
           | Section (`FloatMinus) -> Variable ("-.", pos)
           | Section (`Minus) -> Variable ("-", pos)
           | Section (`Project name) -> (let var = unique_name () in
                                           desugar (FunLit (None, [`Variable var, pos'], 
                                                            ((Projection ((Var var, pos'), name), pos'):Sugartypes.phrase)), pos'))
           | Section (`Name name) -> Variable (name, pos)
           | Conditional (e1, e2, e3) -> Condition (desugar e1, desugar e2, desugar e3, pos)
           | Projection (e, name) -> (let s = unique_name ()
                                      in Record_selection (name, s, unique_name (), desugar e, Variable (s, pos), pos))
           | With (e, fields) -> 
               ListLabels.fold_right ~init:(desugar e) fields 
                 ~f:(fun (label, value) record ->
                       let rvar = gensym () in
                         Record_intro (StringMap.add label (desugar value) StringMap.empty,
                                       Some (Record_selection (label, gensym(), rvar, record,
                                                               Variable (rvar,pos), pos)),
                                       pos))
           | TableLit (name, datatype, db) -> 
               let row = match datatype with
                 | RecordType row ->
                     desugar_row var_env row
                 | UnitType ->
                     raise (ASTSyntaxError(data_position pos, "Tables must have at least one field"))
                 | _ ->
                     raise (ASTSyntaxError(data_position pos, "Tables must take a non-empty record type"))
               in
                 TableHandle (desugar db, desugar name, row, pos)
           | UnaryAppl (`Minus, e)      -> Apply (Variable ("negate",   pos), desugar e, pos)
           | UnaryAppl (`FloatMinus, e) -> Apply (Variable ("negatef",  pos), desugar e, pos)
           | UnaryAppl (`Name n, e) -> Apply (Variable (n,  pos), desugar e, pos)
           | ListLit  [] -> Nil (pos)
           | ListLit  (e::es) -> Concat (List_of (desugar e, pos), desugar (ListLit (es), pos'), pos)
           | DBDelete ((pattern, table), condition) ->
               let t = unique_name () in
               let r = unique_name () in
               let tv = ((Var t), pos') in
               let rv = ((Var r), pos') in
               let generator =
                 `Table ((`As (r, pattern), pos'), tv) in
               let rows = Iteration (generator, ((ListLit [rv]), pos'), condition, None), pos' in
                 desugar (
                   Block ([(Binding (((`Variable t), pos'), table)), pos'],
                          (FnAppl ((Var "deleterows", pos'),
                                   ([tv;
                                     rows
                                    ], pos')), pos')), pos')
(*            | DBDelete (table, rows) -> *)
(*                desugar (FnAppl ((Var "deleterows", pos'), *)
(*                                 ([table; *)
(*                                   rows *)
(*                                  ], pos')), pos') *)
           | DBInsert (table, rows) -> 
               desugar (FnAppl ((Var "insertrows", pos'),
                                ([table;
                                  rows
                                 ], pos')), pos')
           | DBUpdate ((pattern, table), condition, row) ->
               let t = unique_name () in
               let r = unique_name () in
               let s = unique_name () in
               let tv = ((Var t), pos') in
               let rv = ((Var r), pos') in
               let sv = ((Var s), pos') in
               let generator =
                 `Table ((`As (r, pattern), pos'), tv) in
               let ignorefields = 
                 List.map (fun (name, value) -> name, ((`Variable (unique_name ())), pos')) row in
               let row_pair = 
                 (TupleLit
                    [rv;
                     (RecordLit (row, Some sv), pos')]), pos' in
               let body =
                 Block([Binding (
                          ((`Record (ignorefields,
                                     Some ((`Variable s), pos'))), pos'), rv), pos'],
                       ((ListLit [row_pair]), pos')), pos' in
               let row_pairs = Iteration (generator, body, condition, None), pos'
               in      
                 desugar (
                   Block ([(Binding (((`Variable t), pos'), table)), pos'],
                          (FnAppl ((Var "updaterows", pos'),
                                   ([tv;
                                     row_pairs
                                    ], pos')), pos')), pos')
(*            | DBUpdate (table, row_pairs) ->  *)
(*                desugar (FnAppl ((Var "updaterows", pos'), *)
(*                                 ([table; *)
(*                                   row_pairs *)
(*                                  ], pos')), pos') *)
           | DatabaseLit (name, (opt_driver, opt_args)) ->
               let e =
                 match opt_driver with
                   | None ->
                       RecordLit ([("name", name)],
                                  Some (FnAppl((Var "getDatabaseConfig", pos'),
                                               ([RecordLit ([], None), pos'], 
                                                pos')), pos')), pos'
                   | Some driver ->
                       let args =
                         match opt_args with
                           | None -> StringLit (""), pos'
                           | Some args -> args
                       in
                         RecordLit ([("name", name); ("driver", driver); ("args", args)], None), pos'
               in
                 Database (desugar e, pos)
           | Definition (name, e, loc) -> Define (name, desugar e, loc, pos)
           | TypeDeclaration (name, args, rhs) ->
               let get_var arg =
                 match (Unionfind.find (StringMap.find arg tenv)) with
                   | `TypeVar var | `RigidTypeVar var -> var
                   | _ -> assert false
               in
                 TypeDecl (name,
                           List.map get_var args,
                           desugar_datatype var_env rhs, pos)
           | RecordLit (fields, r) ->
               Record_intro (string_map_of_assoc_list (alistmap desugar fields),
                             opt_map desugar r,
                             pos) 
           | TupleLit [field] -> desugar field
           | TupleLit fields  ->
               desugar (RecordLit (List.map2 (fun exp n ->
                                                string_of_int n, exp)
                                     fields (fromTo 1 (1 + length fields)), None), pos')
           | HandleWith (e1, name, e2) -> 
               Syntax.Call_cc(Abstr("return", 
                                    Let (name, Syntax.Call_cc(Abstr("handler",
                                                                    Apply (Variable ("return", pos), 
                                                                           desugar e1, pos), pos), pos), desugar e2, pos), pos), pos)
           | FnAppl (fn, ([],ppos))  -> Apply (desugar fn, unit_expression (`U(lookup_pos ppos)), pos)
           | FnAppl (fn, ([p], _)) -> Apply (desugar fn, desugar p, pos)
           | FnAppl (fn, (ps, ppos))  -> Apply (desugar fn, desugar (TupleLit ps, ppos), pos)
           | FunLit (None, patterns, body) -> polyfunc (List.map patternize patterns) pos (desugar body)
           | FunLit (Some name, patterns, body) -> Rec ([name, desugar (FunLit (None, patterns, body), pos'), None],
                                                        Variable (name, pos),
                                                        pos)
           | Block (es, exp) -> let es = 
               List.map (function (* pattern * untyped_expression * position * recursivep *)
                           | Binding (p, e), pos -> 
                               (patternize p, desugar e, `U (lookup_pos pos), false)
                           | FunLit (Some n, patts, body), fpos -> 
                               ((`Variable n, pos), 
                                desugar (FunLit (None, patts, body), fpos), 
                                `U (lookup_pos fpos), 
                                true)
                           | expr, epos -> 
                               (`Variable "__", pos), desugar (expr, epos), `U (lookup_pos epos), false) es in
               polylets es (desugar exp)
           | Foreign (language, name, datatype) -> 
               Alien (language, name, desugar_assumption (generalize datatype), pos)
           | InfixDecl -> unit_expression pos
           | SortBy_Conc(patt, expr, sort_expr) ->
               (match patternize patt with
                  | `Variable var, _ -> 
                      SortBy(desugar expr, (Abstr(var, desugar sort_expr, pos)), pos)
                  | pattern -> raise (ASTSyntaxError(data_position pos, "orderby clause on non-simple pattern-matching for is not yet implemented.")))
           | Iteration (generator, body, None, None) ->
               let pattern, from = as_list pos' generator
               in
                 (match patternize pattern with
                    | `Variable var, _ -> For (desugar body, var, desugar from, pos)
                    | pattern -> (let var = unique_name () in
                                    For (polylet pattern pos (Variable (var, pos)) (desugar body),
                                         var, desugar from, pos)))
           | Iteration (generator, body, filter_cond, Some sort_expr) -> 
               let pattern, from = as_list pos' generator
               in
                 desugar (Iteration (`List (pattern, (SortBy_Conc(pattern, from, sort_expr), pos')),
                                     body, filter_cond, None),
                          pos')
           | Iteration (generator, body, Some exp, sort_expr) ->
               desugar (Iteration (generator, 
                                   (Conditional (exp,
                                                 body,
                                                 (ListLit [], pos')), pos'), 
                                   None, sort_expr),
                        pos')
           | Binding _ -> raise (ASTSyntaxError(data_position pos, "Unexpected binding outside a block"))
           | Switch (exp, patterns) ->
               PatternCompiler.match_cases
                 (pos, desugar exp, 
                  (List.map (fun (patt, body) -> ([patternize patt], desugar body)) patterns))
           | Receive patterns -> 
               desugar (Switch ((FnAppl ((Var "recv", pos'), ([TupleLit [], pos'], pos')), pos'),
                                patterns), pos')

           (*  TBD: We should die if the XML text literal has bare ampersands or
               is otherwise ill-formed. It should also be made to properly handle
               CDATA.
               Where's a good place to do so? 
           *)
           | TextNode s -> Apply (Variable ("stringToXml", pos), String (s, pos), pos)
           | Xml (tag, attrs, subnodes) -> 
               let concat a b = 
                 Concat (desugar a, b, pos) in
               let desugar_attr = function
                 | [] -> String ("", pos)
                 | [x] -> desugar x
                 | xs  -> (fold_right concat xs (Nil (pos))) in
                 if (tag = "#") then
                   begin
                     if List.length attrs != 0 then
                       raise (ASTSyntaxError (Syntax.data_position pos, "XML forest literals cannot have attributes"))
                     else
                       List.fold_right
                         (fun node nodes ->
                            Concat (desugar node, nodes, pos)) subnodes (HasType (Nil pos, Inferencetypes.xml_type, pos))
                   end
                 else
                   Xml_node (tag, alistmap desugar_attr attrs, map desugar subnodes, pos)
           | XmlForest []  -> HasType(Nil pos, Inferencetypes.xml_type, pos)
           | XmlForest [x] -> HasType(desugar x, Inferencetypes.xml_type, pos)
           | XmlForest (x::xs) -> Concat (desugar x, desugar (XmlForest xs, pos'), pos)

           | Form (formExpr, formHandler) ->
               let formHandlerSyntax = desugar formHandler in
               let XmlForest trees, trees_ppos = formExpr in
               let result, _ = forest_to_form_expr trees (Some formHandler) pos trees_ppos in
                 result

     (* TBD: Need to move the Links functions xml, pure, plug, (@@@)
        into a Prelude of some kind. *)
     and forest_to_form_expr trees yieldsClause (pos:Syntax.untyped_data) 
         (trees_ppos) = 
       (* (TBD: could have a pass-through case for when there are no
          bindings in the forest.) *)

       (* We pass over the forest finding the bindings and construct a
          term-context representing all of the form/yields expression
          except the `yields' part (the handler). Here bindings
          is a list of lists, each list representing a tuple returned
          from an inner instance of forest_to_form_expr--or, if it's a
          singleton, a single value as bound by a binder.  *)
       let ctxt, bindings =
         fold_right
           (fun (_, lpos as l) (ctxt, bs) -> 
              let l_unsugared, bindings = desugar_form_expr l in
                ((fun r -> (apply2_curried pos
                              (Variable("@@@", pos)) 
                              l_unsugared
                              (ctxt(r)))),
                 bindings @ bs)
           ) trees ((fun r -> r), []) in
         (* Next we construct the handler body from the yieldsClause,
            if any.  The yieldsClause is the user's handler; if it is
            None then we construct a default handler that just bundles
            up all the bound variables and returns them as a tuple.
            Here we also form a list of the values we're
            returning. returning_bindings is a list of lists,
            representing a list of tuples of values.  *)
       let handlerBody, bindings, returning_bindings = 
         match yieldsClause with
             Some formHandler -> formHandler, bindings, []
           | None ->
               let fresh_bindings = map (fun bs ->
                                           map (fun (_, ppos) ->
                                                  `Variable (unique_name ()), ppos) bs) bindings
               in
                 ((TupleLit (map (fun (`Variable x, ppos) -> Var x, ppos) (flatten fresh_bindings)),
                   (Lexing.dummy_pos, Lexing.dummy_pos)),
                  fresh_bindings,
                  [flatten bindings])
       in
       let _, ppos = handlerBody in
         (* The handlerFunc is simply formed by abstracting the
            handlerBody with all the binding names, appropriately
            destructing tuples. *)
         (* Note: trees_ppos will become the position for each tuple;
            the position of the tuple is what's reported when duplicate
            bindings are present within one form. *)
       let handlerFunc = abstract_expr_curried_for_tuple_list (trees_ppos) handlerBody bindings in
         ctxt(Apply(Variable("pure", pos), desugar' lookup_pos handlerFunc, pos)), returning_bindings

     and desugar_form_expr formExpr : untyped_expression * ppattern list list =
       if (xml_tree_has_form_binding formExpr) then
         match formExpr with
           | XmlForest trees, trees_ppos -> forest_to_form_expr trees None (`U(lookup_pos trees_ppos)) trees_ppos
               (* re: FormBinding; we ought to pass this pos' to such
                  a place where a duplicate binding could be pinned down
                  to this binding; but the nature of the duplicate-name
                  checking for patterns is that it finds the error in the
                  whole tuple (the tuple argument that's used by the
                  handler function we later construct.) *)
           | FormBinding ((expr, pos), ppattern), pos' ->
               desugar' lookup_pos (expr, pos), [[ppattern]]
           | Xml("#", [], contents), pos' -> forest_to_form_expr contents None (`U(lookup_pos pos')) pos'
           | Xml("#", _, contents), pos' -> raise(ASTSyntaxError(Syntax.data_position (`U(lookup_pos pos')),
                                                                 "XML forest literals cannot have attributes"))
           | Xml(tag, attrs, contents), pos' ->
               let form_expr, bindings = forest_to_form_expr contents None (`U(lookup_pos pos')) pos' in
               let attrs' = (alistmap 
                               (fun attr_phrases -> 
                                  make_links_list (`U(lookup_pos pos')) (map (desugar' lookup_pos) attr_phrases))
                               attrs) in
               let pos = (`U(lookup_pos pos')) in 
                 (apply2_curried pos
                    (Variable("plug", pos))
                    (make_xml_context tag attrs' pos)
                    form_expr,
                  bindings)
           | TextNode text, pos' -> 
               let pos = `U(lookup_pos pos') in 
                 Apply(Variable("xml", pos),
                       Apply(Variable("stringToXml", pos),
                             String(text, pos), pos), pos), [[]]
       else
         let _, pos' = formExpr in 
         let pos = (`U(lookup_pos pos')) in 
         Apply(Variable("xml", pos), desugar' lookup_pos formExpr, pos), [[]]

     and xml_tree_has_form_binding = function
       | Xml(tag, attrs, contents), _ ->
           List.exists xml_tree_has_form_binding contents
       | XmlForest(trees), _ ->  List.exists xml_tree_has_form_binding trees
       | FormBinding(expr, var), _ -> true
       |  _, _ -> false

     and desugar_repeat _ : Regex.repeat -> phrasenode = function
       | Regex.Star      -> ConstructorLit ("Star", None)
       | Regex.Plus      -> ConstructorLit ("Plus", None)
       | Regex.Question  -> ConstructorLit ("Question", None)
     and desugar_regex _ pos : regex -> phrasenode = 
       (* Desugar a regex, making sure that only variables are embedded
          within.  Any expressions that are spliced into the regex must be
          let-bound beforehand.  *)
       let exprs = ref [] in
       let expr e = 
         let v = gensym ~prefix:"_regex_" () in
           begin
             exprs := (v, e) :: !exprs;
             Var v, pos
           end in
       let rec aux = 
         function
           | Range (f, t)    -> ConstructorLit ("Range", Some (TupleLit [CharLit f, pos; CharLit t, pos], pos))
           | Simply s        -> ConstructorLit ("Simply", Some (StringLit s, pos))
           | Any             -> ConstructorLit ("Any", None)
           | Seq rs          -> ConstructorLit ("Seq", Some (ListLit (List.map (fun s -> aux s, pos) 
                                                                        rs), pos))
           | Repeat (rep, r) -> ConstructorLit ("Repeat", Some (TupleLit [desugar_repeat pos rep, pos; 
                                                                          aux r, pos], pos))
           | Splice e        -> ConstructorLit ("Simply", Some (expr e))
       in fun e ->
         let e = aux e in
           Block 
             (List.map
                (fun (v, e1) -> Binding ((`Variable v, pos), e1), pos)
                !exprs,
              (e, pos))
     and simple_pattern_of_pattern var_env lookup_pos ((pat,pos') : ppattern) : simple_pattern = 
       let desugar = simple_pattern_of_pattern var_env lookup_pos
       and pos = `U (lookup_pos pos') in
       let rec aux = function
         | `Variable _
         | `Nil as p -> p, pos
         | `Any -> `Variable (unique_name ()), pos
         | `Constant (p,_) ->
             `Constant (match p with
                          | IntLit v    -> Integer (v, pos)
                          | FloatLit v  -> Float (v, pos)
                          | StringLit v -> String (v, pos)
                          | BoolLit v   -> Boolean (v, pos)
                          | CharLit v   -> Char (v,  pos)
                          | _ -> assert false),
             pos
         | `Cons (l,r) -> `Cons (desugar l, desugar r), pos
         | `List ps ->
             List.fold_right
               (fun pattern list ->
                  `Cons (desugar pattern, list), pos)
               ps
               (`Nil, pos)
         | `As (name, p) -> `As (name, desugar p), pos
         | `HasType (p, datatype) -> `HasType (desugar p, desugar_datatype var_env datatype), pos
         | `Variant (l, Some v) ->
             `Variant (l, desugar v), pos
         | `Variant (l, None) ->
             if Settings.get_value cons_unit_hack then
               `Variant (l, (`HasType (((`Variable (unique_name ())), pos), Inferencetypes.unit_type), pos)), pos
             else
               `Variant (l, (`Constant (unit_expression pos), pos)), pos
               (* 
                  When cons_unit_hack is enabled (the default), elimination of A is identified with
                  elimination of A(x:()) which allows us to have programs such as:

                    (-)  switch (A) {case A -> B; case x -> x;}

                  compile, even when unit_hack is disabled.

                  When cons_unit_hack is disabled (which makes sense
                  when unit_hack is enabled) we instead identify
                  elimination of A with elimination of A().
                  
                  If cons_unit_hack is disabled and unit_hack is enabled
                  then (-) compiles to:

                    let z = A in
                      case z of
                        A(y) -> if y = () then B
                                else let x = A(y) in x
                        x -> let _ = x:[|-A|rho|] in x

                  which does not type check as [|A:()|rho'|] cannot be unifed with [|-A|rho|].

                  If cons_unit_hack is disabled and unit_hack is enabled
                  then (-) compiles to:

                    let z = A in
                      case z of
                        A(y) -> B
                        x -> let _ = x:[|-A|rho|] in x

                  which does type check.

                  If cons_unit_hack is enabled and unit_hack is disabled (the default)
                  then (-) compiles to:

                    let z = A in
                      case z of
                        A(y) -> let _ = (y:()); B
                         x -> let _ = x:[|-A|rho|] in x
                  
                  which again type checks.

                  The latter case is chosen as the default, as it will remain sound even if we
                  disable static typing.
               *)
         | `Record (labs, base) ->
             List.fold_right
               (fun (label, patt) base ->
                  `Record (label, desugar patt, base), pos)
               labs
               ((fromOption (`Constant (unit_expression pos), pos) (Utility.opt_map desugar base)))
         | `Tuple ps ->
             List.fold_right2
               (fun patt n base ->
                  `Record (string_of_int n, desugar patt, base), pos)
               ps
               (Utility.fromTo 1 (1 + List.length ps))
               ((`Constant (unit_expression pos)), pos)
       in let p = aux pat in
         begin
           check_for_duplicate_names p;
           p
         end
     in
(*        (Debug.if_set show_sugared (Show_phrase.show e); *)
     let result = desugar' lookup_pos e in
       (Debug.if_set show_desugared (fun()-> string_of_expression result);
       result)

   let desugar_datatype = generalize ->- desugar_assumption

 end : 
  sig 
    val desugar : (pposition -> Syntax.position) -> phrase -> Syntax.untyped_expression
    val desugar_datatype : Sugartypes.datatype -> Inferencetypes.assumption
    val fresh_type_variable : unit -> Sugartypes.datatype
  end)

include Desugarer

type directive = string * string list
type sentence = (phrase list, directive) either
type sentence' = (untyped_expression list, directive) either
