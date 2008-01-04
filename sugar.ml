open Num

open Utility
open List
open Syntax
open Sugartypes

let lookup_pos =
  function
    | (start, finish, Some source_code) -> source_code#lookup(start, finish)
    | _ -> Syntax.dummy_position

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
  | `HasType of ('r * Types.datatype)
]

let appPrim pos name args = 
 Apply (Variable (name, pos), args, pos)

type simple_pattern = simple_pattern a_pattern * Syntax.untyped_data

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
    Abstr([hole], Xml_node(tag, attrs, [Variable(hole, pos)], pos), pos)

(* Various specializations of [gensym] *)
let unique_name () = Utility.gensym ()
let db_unique_name = Utility.gensym ~prefix:"Table"

let list_head expr pos = 
  Apply(Variable ("hd", pos), [expr], pos)

let list_tail expr pos = 
  Apply(Variable ("tl", pos), [expr], pos)

let show_desugared = Settings.add_bool("show_desugared", false, `User)
let show_sugared = Settings.add_bool("show_sugared", false, `User)

let unit_hack = Settings.add_bool("pattern_unit_hack", false, `User)
let cons_unit_hack = Settings.add_bool("pattern_cons_unit_hack", true, `User)

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

     type annotation = string list * Types.datatype list
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

     let string_of_constant =
       let soc = function    
         | Syntax.Boolean v -> string_of_bool v
         | Syntax.Integer v -> string_of_num v
         | Syntax.Char v -> string_of_char v
         | Syntax.String v -> v
         | Syntax.Float v -> string_of_float v
       in
         function
           | Syntax.Constant (c, _) -> soc c
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
         StringMap.to_alist
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
         StringMap.to_alist
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
         StringMap.to_alist
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
           NotFound _ -> failwith ("Variable: "^var^" not in environment (lookup)")

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
                 record_selection
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
           | (_name, (exp, annotated_equations))::bs ->
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
                        Let ("_", HasType (var_exp, Types.unit_type, pos),
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
   let rec alias_is_closed vars t =
     let aic = alias_is_closed vars in
       match t with
         | TypeVar var
         | RigidTypeVar var -> StringSet.mem var vars
         | FunctionType (s, m, t) ->
             (List.for_all aic s) &&
               (match m with
                  | TypeVar var -> true
                  | _ -> aic m) &&
               (aic t)
         | MuType (v, k) ->
             alias_is_closed (StringSet.add v vars) k
         | TupleType ks ->
             List.for_all aic ks
         | RecordType r
         | VariantType r -> row_alias_is_closed vars r
         | TableType (r, w) -> aic r && aic w
         | ListType k -> aic k
         | TypeApplication (_,ks) -> List.for_all aic ks
         | UnitType
         | PrimitiveType _
         | DBType -> true
   and row_alias_is_closed vars (fields, rv) =
     (List.for_all (function
                  | (_, `Present k) -> alias_is_closed vars k
                  | (_, `Absent) -> true) fields)
     && row_var_alias_is_closed vars rv
   and row_var_alias_is_closed vars =
     function
       | `Closed -> true
       | `Open var -> StringSet.mem var vars
       | `OpenRigid var -> StringSet.mem var vars
       | `Recursive (v, r) ->
           row_alias_is_closed (StringSet.add v vars) r






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
                 record_selection (label,
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

   let string_of_pattern_pos ((pos : Lexing.position), _, expr) = 
     Printf.sprintf "%s:%d:%s" pos.Lexing.pos_fname pos.Lexing.pos_lnum expr

   let check_for_duplicate_names' (env:StringSet.t) (pattern,pos) : StringSet.t =
     let (pos, dpos, expr) = data_position pos in      
     let module S = StringSet in
     let add elt set = 
       if S.mem elt set then
         raise (PatternDuplicateNameError ((pos, dpos, expr), elt, expr))
       else S.add elt set in
     let union = S.fold add in
     let rec names : simple_pattern a_pattern -> StringSet.t = function
       | (`Nil:simple_pattern a_pattern)
       | `Constant _ -> S.empty
       | (`HasType ((p,_), _):simple_pattern a_pattern)
       | `Variant (_, (p,_)) -> names p
       | `Cons ((l,_),(r,_))
       | `Record (_,(l,_),(r,_)) -> union (names l) (names r)
       | `Variable name -> S.singleton name 
       | `As (name, (p,_)) -> add name (names p)
     in union (names pattern) env

   (* give an error if the pattern has duplicate names *)
   let check_for_duplicate_names =
     check_for_duplicate_names' StringSet.empty

   let as_list pos = function
     | `List (p, e) -> p, e
     | `Table (p, e) -> p, (`FnAppl ((`Var ("asList"), pos), [e]), pos)

   let desugar_constant pos = function
     | `Int v    -> Constant(Integer v, pos)
     | `Float v  -> Constant(Float v, pos)
     | `String v -> Constant(String v, pos)
     | `Bool v   -> Constant(Boolean v, pos)
     | `Char v   -> Constant(Char v,  pos)
  
   let desugar_expression' env (e : phrase) : untyped_expression =
     let _, var_env = env in
     let rec desugar' ((s, pos') : phrase) : untyped_expression =
       let pos = `U (lookup_pos pos')
       and patternize = simple_pattern_of_pattern var_env in
       let appPrim = appPrim pos in
       let desugar = desugar' in
         match (s : phrasenode) with
           | `TypeAnnotation(e, k) -> HasType(desugar e, DesugarDatatype.desugar_datatype' var_env k, pos)
           | `Upcast(e, t1, t2) ->
               (* HACK:

                  use unsafe_cast here to avoid having to add extra syntax to the old IR
               *)
               desugar (`TypeAnnotation ((`FnAppl ((`Var "unsafe_cast", pos'), [`TypeAnnotation(e, t2), pos']), pos'), t1), pos')
           | `Constant p -> desugar_constant pos p
           | `Var v       -> Variable (v, pos)
           | `InfixAppl (`Name ">", e1, e2)  -> Comparison (desugar e2, `Less, desugar e1, pos)
           | `InfixAppl (`Name ">=", e1, e2)  -> Comparison (desugar e2, `LessEq, desugar e1, pos)
           | `InfixAppl (`Name "==", e1, e2)  -> Comparison (desugar e1, `Equal, desugar e2, pos)
           | `InfixAppl (`Name "<", e1, e2)  -> Comparison (desugar e1, `Less, desugar e2, pos)
           | `InfixAppl (`Name "<=", e1, e2)  -> Comparison (desugar e1, `LessEq, desugar e2, pos)
           | `InfixAppl (`Name "<>", e1, e2)  -> Comparison (desugar e1, `NotEq, desugar e2, pos)
           | `InfixAppl (`Name "++", e1, e2)  -> Concat (desugar e1, desugar e2, pos)
           | `InfixAppl (`Name "!", e1, e2)  -> appPrim "send" [desugar e1; desugar e2]
           | `InfixAppl (`Name n, e1, e2)  -> 
               let `U (a,_,_) = pos (* somewhat unpleasant attempt to improve error messages *) in 
                 Apply (Variable (n,  `U (a,n,n)), [desugar e1; desugar e2], pos)
           | `InfixAppl (`Cons, e1, e2) -> Concat (List_of (desugar e1, pos), desugar e2, pos)
           | `InfixAppl (`FloatMinus, e1, e2)  -> appPrim "-." [desugar e1; desugar e2]
           | `InfixAppl (`Minus, e1, e2)  -> appPrim "-" [desugar e1; desugar e2]
           | `InfixAppl (`And, e1, e2) -> Condition (desugar e1, desugar e2, Constant(Boolean false, pos), pos)
           | `InfixAppl (`Or, e1, e2)  -> Condition (desugar e1, Constant(Boolean true, pos), desugar e2, pos)
           | `InfixAppl (`App, e1, e2) -> App (desugar e1, desugar e2, pos)
           | `InfixAppl (`RegexMatch _, _, _) -> failwith "regex found after regex desugaring"
           | `ConstructorLit (name, None) -> Variant_injection (name, unit_expression pos, pos)
           | `ConstructorLit (name, Some s) -> Variant_injection (name, desugar s, pos)
           | `Escape ((name,_,_), e) -> 
               Syntax.Call_cc(Abstr([name], desugar e, pos), pos)
           | `Spawn e -> desugar 
               (`FnAppl ((`Var "spawn", pos'), [`FunLit ([[]], e),  pos']), pos')
           | `SpawnWait e -> desugar 
               (`FnAppl ((`Var "spawnWait", pos'), [`FunLit ([[]], e),  pos']), pos')
           | `Section (`FloatMinus) -> Variable ("-.", pos)
           | `Section (`Minus) -> Variable ("-", pos)
           | `Section (`Project name) -> (let var = unique_name () in
                                            desugar (`FunLit ([[`Variable (var, None, pos'), pos']], 
                                                              (`Projection ((`Var var, pos'), name), pos')), pos'))
           | `Section (`Name name) -> Variable (name, pos)
           | `Conditional (e1, e2, e3) -> Condition (desugar e1, desugar e2, desugar e3, pos)
           | `Projection (e, name) ->
               Project (desugar e, name, pos)
           | `With (e, fields) -> 
               ListLabels.fold_right ~init:(desugar e) fields 
                 ~f:(fun (label, value) record ->
                       let rvar = gensym () in
                         Record_intro (StringMap.add label (desugar value) StringMap.empty,
                                       Some (record_selection (label, gensym(), rvar, record,
                                                               Variable (rvar,pos), pos)),
                                       pos))
           | `TableLit (name, datatype, constraints, db) -> 
               (* HACK:

                  This isn't as flexible as it should be - it's just a
                  quick hack to get things going. We should probably
                  maintain the constraints in the IR and generate the
                  actual table types during type inference.
                  
               *)
               let row = match datatype with
                 | RecordType row ->
                     row
                 | UnitType ->
                     raise (ASTSyntaxError(data_position pos, "Tables must have at least one field"))
                 | _ ->
                     raise (ASTSyntaxError(data_position pos, "Tables must take a non-empty record type")) in
               let write_row = DesugarDatatype.make_write_row row constraints in

               let readtype = `Record (DesugarDatatype.desugar_row var_env row) in
               let writetype = `Record (DesugarDatatype.desugar_row var_env write_row) in
                 TableHandle (desugar db, desugar name, (readtype, writetype), pos)
           | `UnaryAppl (`Minus, e)      -> appPrim "negate" [desugar e]
           | `UnaryAppl (`FloatMinus, e) -> appPrim "negatef" [desugar e]
           | `UnaryAppl (`Name n, e) -> appPrim n [desugar e]
           | `UnaryAppl (`Abs, e) -> Abs (desugar e, pos)
           | `ListLit  [] -> Nil (pos)
           | `ListLit  (e::es) -> Concat (List_of (desugar e, pos), desugar (`ListLit (es), pos'), pos)
           | `DBDelete (pattern, table, condition) ->
               let t = unique_name () in
               let r = unique_name () in
               let tv = `Var t, pos' in
               let rv = `Var r, pos' in
               let generator =
                 `Table ((`As ((r,None,pos'), pattern), pos'), tv) in
               let rows = `Iteration ([generator], (`ListLit [rv], pos'), condition, None), pos' in
                 desugar (
                   `Block ((([`Val ((`Variable (t,None,pos'), pos'), table, `Unknown, None), pos'])),
                           (`FnAppl ((`Var "deleterows", pos'), [tv; rows]), pos')), pos')
           | `DBInsert (table, rows) -> 
               desugar (`FnAppl ((`Var "insertrows", pos'), [table; rows]), pos')
           | `DBUpdate (pattern, table, condition, row) ->
               let t = unique_name () in
               let r = unique_name () in

               let tv = ((`Var t), pos') in
               let rv = ((`Var r), pos') in

               let generator =
                 `Table ((`As ((r,None,pos'), pattern), pos'), tv) in
               let body = 
                 (`ListLit
                    [(`TupleLit
                        [rv;
                         (`RecordLit (row, None), pos')]), pos']), pos' in
               let row_pairs = `Iteration ([generator], body, condition, None), pos'
               in      
                 desugar (
                   `Block ([`Val ((`Variable (t,None,pos'), pos'), table, `Unknown, None), pos'],
                           (`FnAppl ((`Var "updaterows", pos'), [tv; row_pairs]), pos')), pos')
           | `DatabaseLit (name, (opt_driver, opt_args)) ->
               let e =
                 match opt_driver with
                   | None ->
                       `RecordLit ([("name", name)],
                                  Some (`FnAppl((`Var "getDatabaseConfig", pos'), []), pos')), pos'
                   | Some driver ->
                       let args =
                         match opt_args with
                           | None -> `Constant (`String ""), pos'
                           | Some args -> args
                       in
                         `RecordLit ([("name", name); ("driver", driver); ("args", args)], None), pos'
               in
                 Database (desugar e, pos)
           | `RecordLit (fields, r) ->
               Record_intro (StringMap.from_alist (alistmap desugar fields),
                             opt_map desugar r,
                             pos) 
           | `TupleLit [field] -> desugar field
           | `TupleLit fields  ->
               desugar (`RecordLit (List.map2 (fun exp n ->
                                                string_of_int n, exp)
                                     fields (fromTo 1 (1 + length fields)), None), pos')

           | `FnAppl (fn, ps)  -> Apply (desugar fn, List.map desugar ps, pos)

           | `FunLit (patterns_lists, body) -> 
               let patternized = (List.map (List.map patternize) patterns_lists) in
               ignore (List.fold_left
                         (List.fold_left check_for_duplicate_names')
                         StringSet.empty
                         patternized);
               List.fold_right 
                 (fun patterns expr -> 
                    let names = List.map (fun _ -> unique_name ()) patterns in
                      Abstr (names,
                             List.fold_right 
                               (fun (pattern,name) expr ->
                                  polylet pattern pos (Variable (name, pos)) expr )
                               (combine patterns names)
                               expr,
                             pos))
                 patternized
                 (desugar body)
(*           | `FunLit (Some name, patterns, body) -> Rec ([name, desugar (`FunLit (None, patterns, body), pos'), None],
                                                        Variable (name, pos),
                                                        pos)*)
           | `Block (es, exp) ->
               let es = 
                 concat_map (fst ->- (function (* pattern * untyped_expression * position * recursivep *)
                             | `Val (p, e, _, _) (* TODO: use datatype, if any *) -> 
                                 [(patternize p, desugar e, pos, false)]
                             | `Fun ((n,_,_), funlit, _, _) (* TODO: use datatype, if any *) -> 
                                 [((`Variable n, pos), 
                                   desugar (`FunLit funlit, pos'),
                                   pos, 
                                   true)]
                             | `Exp expr -> 
                                 [(`HasType ((`Variable "__", pos), Types.unit_type), pos), 
                                 desugar expr,
                                  pos, false]
                             | `Infix -> assert false
                             | `Funs defs ->
                                 List.map 
                                   (fun ((n,_,_), funlit, _, _) ->
                                      ((`Variable n, pos), 
                                       desugar (`FunLit funlit, pos'),
                                       pos, 
                                       true)) defs
                             | `Type _ -> assert false
                             | `Include _ -> assert false
                             | `Foreign _ -> assert false (* TODO *)) : binding -> _) es in
                 polylets es (desugar exp)

           | `Iteration ([], body, filter, None) ->
               let body =
                 match filter with
                   | None -> body
                   | Some condition ->
                       `Conditional (condition, body, (`ListLit [], pos')), pos'
               in
                 desugar body
           | `Iteration (generators, body, filter, None) ->
               let body =
                 match filter with
                   | None -> body
                   | Some condition ->
                       `Conditional (condition, body, (`ListLit [], pos')), pos'
               in
                 List.fold_right
                   (fun generator body ->
                      let pattern, from = as_list pos' generator in
                      let var, body =
                        match patternize pattern with
                          | `Variable var, _ -> var, body
                          | pattern ->
                              let var = unique_name () in
                                var, polylet pattern pos (Variable (var, pos)) body
                      in
                        For (body, var, desugar from, pos))
                   generators
                   (desugar body)
           | `Iteration ([generator], body, filter, Some sort) -> 
               let body =
                 match filter with
                   | None -> body
                   | Some condition ->
                       `Conditional (condition, body, (`ListLit [], pos')), pos'
               and pattern, from = as_list pos' generator
               in
                 begin
                   match patternize pattern with
                     | `Variable var, _ ->
                         For (desugar body, var, SortBy(desugar from, (Abstr([var], desugar sort, pos)), pos), pos)
                     | _ ->
                         raise (ASTSyntaxError(data_position pos,
                                               "orderby clause on non-simple pattern-matching for is not yet implemented."))
                 end
           | `Iteration (_, _, _, Some _) ->
               raise (ASTSyntaxError(data_position pos,
                                     "orderby clause with multiple generators is not yet implemented."))
               
           | `Switch (exp, patterns) ->
               PatternCompiler.match_cases
                 (pos, desugar exp, 
                  (List.map (fun (patt, body) -> ([patternize patt], desugar body)) patterns))
           | `Receive patterns -> 
               desugar (`Switch ((`FnAppl ((`Var "recv", pos'), []), pos'),
                                patterns), pos')

           (*  TBD: We should die if the XML text literal has bare ampersands or
               is otherwise ill-formed. It should also be made to properly handle
               CDATA. Where's a good place to do so? 
           *)
           | `TextNode s -> appPrim "stringToXml" [Constant(String s, pos)]
           | `Xml (tag, attrs, attrexp, subnodes) -> 
               let () =
                 let rec dup_check names =
                   function
                     | [] -> ()
                     | (name, _) :: attrs ->
                         if StringSet.mem name names then
                           raise (ASTSyntaxError (Syntax.data_position pos,
                                                  "XML attribute '"^name^"' is defined more than once"))
                         else
                           dup_check (StringSet.add name names) attrs
                 in
                   dup_check StringSet.empty attrs in

               let rec coalesce : (Sugartypes.phrase list -> Sugartypes.phrase list)
                   = function 
                   [] -> []
                 | [x] -> [x]

                 | ((`TextNode s1, d1)::(`TextNode s2, _d2)::xs) ->
                     coalesce((`TextNode (s1^s2), d1) :: xs)

                 | x::xs -> x :: coalesce xs in

               let concat a b = 
                 Concat (desugar a, b, pos) in
               let desugar_attr = function
                 | [] -> Constant(String "", pos)
                 | [x] -> desugar x
                 | xs  -> (fold_right concat xs (Nil (pos)))
               in
                 if (tag = "#") then
                   begin
                     if List.length attrs != 0 || attrexp <> None then
                       raise (ASTSyntaxError (Syntax.data_position pos, "XML forest literals cannot have attributes"))
                     else
                       HasType (
                         List.fold_right
                           (fun node nodes ->
                              Concat (desugar node, nodes, pos)) subnodes (Nil pos),
                         Types.xml_type,
                         pos)
                   end
                 else
                   begin
                     match attrexp with
                       | None ->
                           Xml_node (tag, alistmap desugar_attr attrs,
                                     map desugar (coalesce subnodes), pos)
                       | Some attrexp ->
                           Apply(Variable ("addAttributes", pos),
                                 [Xml_node (tag, alistmap desugar_attr attrs,
                                            map desugar (coalesce subnodes), pos);
                                  desugar attrexp],
                                 pos)
                   end

           | `Formlet (formExpr, formHandler) ->
               fst (forest_to_form_expr [formExpr] (Some formHandler) pos pos')
           | `Page e ->               
               let rec desugar_page : phrase -> phrase =
                 fun (phrase, pos) ->
                   let rec is_raw (phrase, pos) =
                     match phrase with
                       | `TextNode _
                       | `Block _ -> true
                       | `FormletPlacement _
                       | `PagePlacement _ -> false
                       | `Xml (_, _, _, children) ->
                           List.for_all is_raw children
                       | _ ->
                           raise (ASTSyntaxError (lookup_pos pos, "Invalid element in page literal")) in

                   let desugar_pages : phrase list -> phrase = fun children ->
                     (`FnAppl ((`Var "joinManyP", pos), [`ListLit (map desugar_page children), pos]), pos) in
                   let dp : phrasenode -> phrase = function
                     | e when is_raw (e, pos) ->
                         (`FnAppl ((`Var "bodyP", pos), [e, pos]), pos)
                     | `FormletPlacement (formlet, handler, attributes) ->
                         (`FnAppl ((`Var "formP", pos), [formlet; handler; attributes]), pos)
                     | `PagePlacement (page) ->
                         page
                     | `Xml ("#", [], _, children) ->
                         desugar_pages children
                     | `Xml (name, attrs, dynattrs, children) ->
                         let x = gensym ~prefix:"xml" () in
                           (`FnAppl ((`Var "plugP", pos),
                                     [(`FunLit([[`Variable (x,None,pos), pos]],
                                               (`Xml (name, attrs, dynattrs, [`Block ([], (`Var x, pos)), pos]), pos)), pos);
                                      desugar_pages children
                                     ]), pos)
                     | _ ->
                         raise (ASTSyntaxError (lookup_pos pos, "Invalid element in page literal"))
                   in
                     dp phrase
               in
                 desugar (desugar_page e)
           | `FormletPlacement _ ->
               raise (ASTSyntaxError (lookup_pos pos', "A formlet can only be rendered in a page expression"))
           | `PagePlacement _ ->
               raise (ASTSyntaxError (lookup_pos pos', "An embedded page can only appear in a page expression"))
           | `FormBinding _ ->
               raise (ASTSyntaxError (lookup_pos pos', "A formlet binding can only appear in a formlet expression"))
           | `Regex _ -> assert false

     and forest_to_form_expr trees yieldsClause 
         (pos:Syntax.untyped_data) 
         (trees_ppos:Sugartypes.position)
         : (Syntax.untyped_expression * Sugartypes.pattern list list) = 
       (* We pass over the forest finding the bindings and construct a
          term-context representing all of the form/yields expression
          except the `yields' part (the handler). Here bindings
          is a list of lists, each list representing a tuple returned
          from an inner instance of forest_to_form_expr--or, if it's a
          singleton, a single value as bound by a binder.  *)
       let ctxt, bindings =
         fold_right
           (fun l (ctxt, bs) -> 
              let l_unsugared, bindings = desugar_form_expr l in
                (fun r -> appPrim pos "@@@"  [l_unsugared; ctxt r]),
              bindings @ bs) 
           trees
           (identity, []) in
         (* Next we construct the handler body from the yieldsClause,
            if any.  The yieldsClause is the user's handler; if it is
            None then we construct a default handler that just bundles
            up all the bound variables and returns them as a tuple.
            Here we also form a list of the values we're
            returning. returning_bindings is a list of lists,
            representing a list of tuples of values.  *)
       let handlerBody, bindings, returning_bindings = 
         match yieldsClause with
             Some formHandler -> 
               formHandler, bindings, ([]:Sugartypes.pattern list list)
           | None ->
               let fresh_bindings = map (map (fun (_, ppos) -> `Variable (unique_name (), None, ppos), ppos)) bindings in
               let variables = map (fun (`Variable (x,_,_), ppos) -> `Var x, ppos) (flatten fresh_bindings) in
                 ((`TupleLit variables, (Lexing.dummy_pos, Lexing.dummy_pos, None)),
                  fresh_bindings,
                  [flatten bindings])
       in
         (* The handlerFunc is simply formed by abstracting the
            handlerBody with all the binding names, appropriately
            destructing tuples. *)
         (* Note: trees_ppos will become the position for each tuple;
            the position of the tuple is what's reported when duplicate
            bindings are present within one form. *)
       let handlerFunc =  `FunLit (map (function
                                          | [b] -> [b]
                                          | bs -> [`Tuple bs, trees_ppos]) (rev bindings),
                                   handlerBody), trees_ppos in
         ctxt (Apply(Variable("pure", pos), [desugar' handlerFunc], pos)), returning_bindings
           
     and desugar_form_expr (formExpr, ppos) : untyped_expression * pattern list list =
       let pos = `U(lookup_pos ppos) 
       and desugar = desugar' in
       let appPrim = appPrim pos in
       if not (has_form_binding (formExpr,ppos)) then
         Apply (Variable("xml", pos), [desugar (formExpr,ppos)], pos), [[]]
       else
         match formExpr with
           | `FormBinding (phrase, ppattern) -> desugar phrase, [[ppattern]]
           | `Xml ("#", [], attrexp, contents) -> forest_to_form_expr contents None pos ppos
           | `Xml ("#", _, _, _) -> raise (ASTSyntaxError(Syntax.data_position pos,
                                                      "XML forest literals cannot have attributes"))
           | `Xml(tag, attrs, attrexp, contents) ->
               let form, bindings = forest_to_form_expr contents None pos ppos in
               let attrs' = alistmap (map desugar ->- make_links_list pos) attrs in
                 (appPrim "plug" [make_xml_context tag attrs' pos; form],
                  bindings)
                   
           | `TextNode text -> 
               appPrim "xml" [appPrim "stringToXml" [Constant (String text, pos)]], [[]]
           | _ -> assert false

     and has_form_binding = function
       | `Xml (_, _, _, subnodes),_ -> exists has_form_binding subnodes
       | `FormBinding _,_      -> true
       |  _                    -> false
     and simple_pattern_of_pattern var_env ((pat,pos') : pattern) : simple_pattern = 
       let desugar = simple_pattern_of_pattern var_env
       and pos = `U (lookup_pos pos') in
       let rec aux : patternnode -> _ = function
         | `Variable (v,_,_) -> `Variable v, pos
         | `Nil            -> `Nil, pos
         | `Any            -> `Variable (unique_name ()), pos
         | `Constant p     -> `Constant (desugar_constant pos p), pos
         | `Cons (l,r) -> `Cons (desugar l, desugar r), pos
         | `List ps ->
             List.fold_right
               (fun pattern list ->
                  `Cons (desugar pattern, list), pos)
               ps
               (`Nil, pos)
         | `As ((name,_,_), p) -> `As (name, desugar p), pos
         | `HasType (p, datatype) -> `HasType (desugar p, DesugarDatatype.desugar_datatype' var_env datatype), pos
         | `Variant (l, Some v) ->
             `Variant (l, desugar v), pos
         | `Variant (l, None) ->
             if Settings.get_value cons_unit_hack then
               `Variant (l, (`HasType (((`Variable (unique_name ())), pos), Types.unit_type), pos)), pos
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
               ((from_option (`Constant (unit_expression pos), pos) (Utility.opt_map desugar base)))
         | `Tuple ps ->
             List.fold_right2
               (fun patt n base ->
                  `Record (string_of_int n, desugar patt, base), pos)
               ps
               (Utility.fromTo 1 (1 + List.length ps))
               ((`Constant (unit_expression pos)), pos) in
       let p = aux pat in
         begin
           check_for_duplicate_names p;
           p
         end
     in
     let result = desugar' e in
       (Debug.if_set show_desugared (fun()-> string_of_expression result);
        result)

   let desugar_expression (_,pos as e) =
     desugar_expression' (DesugarDatatype.var_mapping_from_binding ((`Exp e), pos)) e

   let desugar_definition ((s, pos') : binding) : untyped_definition list =
     let _, ((tenv, _) as var_env) = DesugarDatatype.var_mapping_from_binding (s, pos') in
     let pos = `U (lookup_pos pos') in
     let desugar_expression = desugar_expression in
     let ds : bindingnode -> _ Syntax.definition' list = function
       | `Val ((`Variable (name,_,_), _), p, location, None) ->
           [Define (name, desugar_expression p, location, pos)]
       | `Val ((`Variable (name,_,_), _), p, location, Some t) ->
           [Define (name, HasType (desugar_expression p, DesugarDatatype.desugar_datatype' var_env t, pos), location, pos)]
       | `Val _ -> assert false (* TODO: handle other patterns *)
       | `Fun ((name,_,_), funlit, location, dtype) ->
           [Define (name, Rec ([name, desugar_expression (`FunLit funlit, pos'), 
                                opt_map (DesugarDatatype.desugar_datatype' var_env) dtype],
                               Variable (name, pos),
                               pos),
                    location, pos)]
       | `Type (name, args, rhs) ->
           let get_var arg =
             match (Unionfind.find (StringMap.find arg tenv)) with
               | `Flexible var | `Rigid var -> var
               | _ -> assert false
           in
             if alias_is_closed (StringSet.from_list args) rhs then
               [Alias (name,
                       List.map get_var args,
                       DesugarDatatype.desugar_datatype' var_env rhs, pos)]
             else
               failwith ("Free variable(s) in alias")
       | `Foreign (language, name, datatype) -> 
           [Alien (language, name, DesugarDatatype.desugar_datatype datatype, pos) ]
       | `Include path ->
           failwith "Includes not supported"
       | `Funs defs ->
           List.map 
             (fun ((n,_,_), funlit, location, ft) ->
                Define (n, Rec ([n,
                                 desugar_expression (`FunLit funlit, pos'),
                                 opt_map (DesugarDatatype.desugar_datatype' var_env) ft],
                                Variable (n, pos), pos), location, pos))
             defs
       | `Exp _ -> assert false
       | `Infix -> assert false in
     let results = ds s in
       Debug.if_set show_desugared
         (fun () ->
            String.concat ", \n" (List.map string_of_definition results));
       results

   let desugar_definitions =
     let rec desugar : binding list -> _ = function
       | [] -> []
       | (`Infix, _) :: phrases -> desugar phrases
       | phrase :: phrases ->
           desugar_definition phrase @ desugar phrases
     in
       desugar


   let desugar_expression phrase =
     Debug.if_set show_sugared (fun () -> Show_phrase.show phrase);
     desugar_expression phrase

   let desugar_definitions bindings =
     Debug.if_set show_sugared
       (fun () ->
          (String.concat "\n"
             (List.map Show_binding.show bindings)));
     desugar_definitions bindings

   let desugar_program (bindings, body) =
     let defs = desugar_definitions bindings in
     let body = opt_map desugar_expression body in
       Syntax.Program (defs, from_option (Syntax.unit_expression (`U Syntax.dummy_position)) body)   
 end : 
  sig 
    val desugar_expression : phrase -> Syntax.untyped_expression
    val desugar_definitions : binding list -> Syntax.untyped_definition list
    val desugar_program : program -> Syntax.untyped_program
  end)

include Desugarer
