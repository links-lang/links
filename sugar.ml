open Num
open List

open Utility
open Syntax
open Sugartypes

exception ConcreteSyntaxError of (string * (Lexing.position * Lexing.position))

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

type simple_pattern = simple_pattern a_pattern * Syntax.position

(* Various flavours of a sort of `gensym'*)
let unique_name () = Utility.gensym ()
let db_unique_name = Utility.gensym ~prefix:"Table"

let list_head expr pos = 
  Apply(Variable ("hd", pos), expr, pos)

let list_tail expr pos = 
  Apply(Variable ("tl", pos), expr, pos)

exception RedundantPatternMatch of Syntax.position
module PatternCompiler =
  (*** pattern matching compiler ***)
  (*
    This is similar to the pattern matching compiler described in by
    Phil Wadler in Chapter 5 of 'The Implementation of Functional
    Programming Languages, Simon Peyton Jones, 1987'.
    
    It is not yet optimised and can result in duplication of terms. In
    order to improve the pattern matching implementation, we may need
    to adjust our intermediate language.
  *)
  (struct
     let unit_hack = Settings.add_bool("pattern_unit_hack", true, true)

     type annotation = string list * Types.datatype list
     type annotated_pattern = annotation * simple_pattern

     type ('a, 'b) equation_basis = 'a list * 'b

     type raw_equation = (simple_pattern, untyped_expression) equation_basis
     type equation = (annotated_pattern, (untyped_expression * bool ref)) equation_basis
     type annotated_equation = annotation * equation

     type bound_expression = string -> untyped_expression

     let eq_patterns : simple_pattern * simple_pattern -> bool =
       let rec eq = function
         | `Nil, `Nil 
         | `Nil, `Cons _
         | `Cons _, `Nil
         | `Cons _, `Cons _
         | `Variant _,  `Variant _
         | `Record _,   `Record _
         | `Constant _, `Constant _
         | `Variable _, `Variable _ -> true
         | `As (_, (pattern,_)), pattern'
         | pattern, `As (_, (pattern',_)) -> eq (pattern, pattern')
         | `HasType (((pattern,_):simple_pattern), _), pattern' 
         |  pattern, `HasType (((pattern',_):simple_pattern), _) -> eq (pattern, pattern')
         | _, _ -> false
       in fun ((p, _), (q, _)) -> eq (p, q)

     let eq_equation_patterns : equation * equation -> bool =
       fun (((_, pattern)::_, _), ((_, pattern')::_, _)) -> eq_patterns (pattern, pattern')

     (*
       subst e u v
       substitutes the variable v for the free variable u in the expression e
     *)
     let subst : untyped_expression -> string -> string -> untyped_expression = fun exp u v ->
       match
         (RewriteUntypedExpression.bottomup
            (function
               | Variable (var, data) ->
                   if var=u then Some (Variable (v, data))
                   else None
               | _ -> None) exp)
       with
           None -> exp
         | Some exp -> exp

     let subst_exp : untyped_expression -> string -> untyped_expression -> untyped_expression = fun exp x v ->
       match
         (RewriteUntypedExpression.bottomup
            (function
               | Variable (var, data) ->
                   if var=x then Some v
                   else None
               | _ -> None) exp)
       with
           None -> exp
         | Some exp -> exp

     let string_of_constant = function
       | Syntax.Boolean (v, _) -> string_of_bool v
       | Syntax.Integer (v, _) -> string_of_num v
       | Syntax.Char (v, _) -> string_of_char v
       | Syntax.String (v, _) -> v
       | Syntax.Float (v, _) -> string_of_float v
       | Syntax.Record_empty _ -> "()"

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

     let get_equations_pattern_type : equation list -> pattern_type =
       fun (((_, pattern)::_, _)::_) -> get_pattern_type pattern


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

     (* partition equations sequentially according to the equality predicate *)
     let partition_equations : (equation * equation -> bool) -> equation list -> (equation list) list =
       fun equality_predicate ->
         function
           | [] -> []
           | equation::equations ->
               let (es, ess) =
                 List.fold_left
                   (fun (es, ess) equation ->
                      let es, ess =
                        if equality_predicate (List.hd es, equation) then
                          equation::es, ess
                        else
                          [equation], (List.rev es)::ess
                      in
                        (es, ess)) ([equation], []) equations
               in
                 List.rev(es :: ess)

     (*
       push any variable patterns to the end of the top-most equation
       and reorder the variables and other equations accordingly
     *)
     let reorder_patterns : (string list * equation list) -> (string list * equation list) =
       fun (vars,  (ps, body) :: equations) ->
         let ps, vs, indices, _ =
           List.fold_right
             (fun p (ps, vs, var_indices, index) ->
                match p with
                  | (_, ((`Variable _), _)) ->
                      ps, p::vs, IntSet.add index var_indices, index+1
                  | _ ->
                      p::ps, vs, var_indices, index+1)
             ps ([], [], IntSet.empty, 0) in

         let reorder_list indices xs =
           let xs, ys, _ =
             List.fold_right
               (fun x (xs, ys, index) ->
                  if IntSet.mem index indices then
                    xs, x::ys, index+1
                  else
                    x::xs, ys, index+1) xs ([], [], 0)
           in
             xs @ ys in
         let reorder_equation indices (ps, body) = (reorder_list indices ps, body)
         in
           (reorder_list indices vars,
            (ps @ vs, body) :: map (reorder_equation indices) equations)

     (* partition list equations by constructor *)
     let partition_list_equations : equation list -> (annotated_equation list * annotated_equation list) =
       fun equations ->
         List.fold_left (fun (nil_equations, cons_equations) (ps, body) ->
                            match ps with
                              | (annotation, (`Nil,_))::ps ->
                                  (annotation, (ps, body))::nil_equations, cons_equations
                              | (annotation, (`Cons (px, pxs),_))::ps ->
                                  let px = reduce_pattern px in 
                                  let pxs = reduce_pattern pxs in
                                    nil_equations, (annotation, (px::pxs::ps, body))::cons_equations
                              | _ -> assert false) ([], []) equations

     (* partition variant equations by constructor *)
     let partition_variant_equations
         : equation list -> (string * ((string * string) * annotated_equation list)) list =
       fun equations ->
         assoc_list_of_string_map
           (List.fold_left
              (fun env (ps, body) ->
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
              ) StringMap.empty equations)

     (* partition record equations by label *)
     let partition_record_equations
         : equation list -> (string * ((string * string) * annotated_equation list)) list =
       fun equations ->
         assoc_list_of_string_map
           (List.fold_left
              (fun env (ps, body) ->
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
              ) StringMap.empty equations)

     (* partition constant equations by constant value *)
     let partition_constant_equations
         : equation list -> (string * (untyped_expression * annotated_equation list)) list =
       fun equations ->
         assoc_list_of_string_map
           (List.fold_left
              (fun env (ps, body) ->
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
              ) StringMap.empty equations)

     (* 
        create a let binding,
        inlining it if the bound expression is a variable
     *)
     let bind_or_subst (var, exp, body, pos) =
       match exp with
         | Variable (var', _) ->
             subst body var var'
         | _ -> Let (var, exp, body, pos)


     (* 
        apply an annotation to an expression
         - rename variables
         - move type annotations into the expression
     *)
     let apply_annotation : Syntax.position -> untyped_expression -> annotation * untyped_expression -> untyped_expression =
       fun pos exp ((names, datatypes), body) ->
         let body = List.fold_right (fun name body ->
                                      bind_or_subst (name, exp, body, pos)) names body in
         let body = List.fold_right (fun datatype body ->
                                      Let ("_", HasType (exp, datatype, pos), body, pos)) datatypes body
         in
           body

     (* apply annotations in an annotated equation list *)
     let apply_annotations : Syntax.position -> untyped_expression -> annotated_equation list -> equation list =
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
        - switch x { case(x,1) -> 1; case (0,1) -> 2;}
       is redundant because 0 in the second pattern has already been
       matched by x in a context where all the other sub-patterns are
       identical
    
        -  switch x { case(A(B(C(1,2,3)))) -> 0; case(A(B(C(1,2,3)))) -> 1;}
       is redundant because the two patterns are identical

       An example of type-oriented redundancy:
        - switch x { case(true) -> 0; case(false) -> 1; case(x) -> 2 }
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
         if we want to share default continuations in the reulting term
         rather than simply inlining them as we do now.
     *)
     let apply_default def var =
       def var

     (* the entry point to the pattern-matching compiler *)
     let rec match_cases
         : Syntax.position -> string list -> equation list -> bound_expression -> untyped_expression =
       fun pos vars equations def ->
         match vars, equations with
           | [], [] -> apply_default def "_"
           | [], ([], (body, used))::_ -> used := true; body
(* identical patterns could be detected here if we wanted *)
(*           | [], ([], _)::_ -> failwith "Redundant pattern"*)
           | vars, _ ->
               let (var::vars), equations = reorder_patterns (vars, equations) in
               let equationss = partition_equations eq_equation_patterns equations in
                 List.fold_right
                   (fun equations exp ->
                      match get_equations_pattern_type equations with
                        | `List ->
                            match_list pos vars (partition_list_equations equations) exp
                        | `Variant ->
                            match_variant pos vars (partition_variant_equations equations) exp
                        | `Variable ->
                            match_var pos vars equations exp
                        | `Record ->
                            match_record pos vars (partition_record_equations equations) exp
                        | `Constant ->
                            match_constant pos vars (partition_constant_equations equations) exp
                   ) equationss def var

     and match_var
         : Syntax.position -> string list -> equation list -> bound_expression -> bound_expression =
       fun pos vars equations def var ->
         match_cases pos vars
           (List.map (fun ((annotation, pattern)::ps, (body, used)) ->
                        let body = apply_annotation pos (Variable (var, pos)) (annotation, body)
                        in
                          match pattern with
                            | (`Variable var',_) ->
                                (ps, (subst body var' var, used))
                            | _ -> assert false) equations) def

           
     and match_list
         : Syntax.position -> string list -> (annotated_equation list * annotated_equation list)
           -> bound_expression -> bound_expression =
       fun pos vars (nil_equations, cons_equations) def var ->
         let nil_equations = apply_annotations pos (Variable (var, pos)) nil_equations in
         let cons_equations = apply_annotations pos (Variable (var, pos)) cons_equations in
         let nil_branch =
           match nil_equations with
             | [] -> apply_default def var
             | _ ->
                 match_cases pos vars nil_equations def in
         let cons_branch =
           match cons_equations with
             | [] -> apply_default def var
             | _ ->
                 let x = unique_name () in
                 let xs = unique_name () in
                   Let(x, list_head (Variable (var, pos)) pos,
                       Let(xs, list_tail (Variable (var, pos)) pos,
                           match_cases pos (x::xs::vars) cons_equations def,
                           pos), pos)
         in
           (Condition(Comparison(Variable (var, pos), "==", Syntax.Nil pos, pos),
                      nil_branch,
                      cons_branch, pos))

     and match_variant
         : Syntax.position -> string list -> ((string * ((string * string) * annotated_equation list)) list) ->
           bound_expression -> bound_expression =
       fun pos vars bs def var ->
         match bs with
           | [] ->
               apply_default def var
           | (name, ((case_variable, default_variable), annotated_equations))::bs ->
               let inject var = Variant_injection(name, Variable(var, pos), pos) in
               let empty var = Variant_selection_empty(Variable (var, pos), pos) in

               let equations = apply_annotations pos (inject var) annotated_equations in
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

               (*
                   Bind an appropriate variable to pass to def in the case of a match.

                   This is needed for the case in which var matches A(z), but a subsequent
                   pattern is not matched. For instance:

                     case e of
                      (A(z), 1) -> m
                      k -> n

                   is compiled to

                     case e of
                      (x, y) ->
                       case x of
                        A(z) ->
                         if y=1 then m
                         else
                          let r = A(z) in n[k := r]
                        s -> n[k := s]
                   where
                     x,y,z,r,s are fresh
                   
                   Compare the two instances of n
                    - when e is of the form (A(z), _) k is rebound to A(z)
                    - when e is of the form (s, _), where s is not of the form A(z),
                   k is rebound to s
               *)
               let bind_match def var =
                 let cons_var = unique_name () in
                 let e = apply_default def cons_var in
                   match e with
                     | Wrong _ -> e 
                     | e ->
                         Let(cons_var,
                             inject var,
                             e,
                             pos)
               in              
                 Variant_selection(Variable (var, pos), name,
                                   case_variable,
                                   match_cases pos (case_variable::vars) equations (bind_match def),
                                   default_variable,
                                   massage_wrong default_variable
                                     (match_variant pos vars bs def default_variable),
                                   pos)
     and match_record
         : Syntax.position -> string list ->
           ((string * ((string * string) * annotated_equation list)) list) ->
           bound_expression -> bound_expression =
       fun pos vars bs def var ->
         match bs with
           | [] -> apply_default def var
           | (name, ((label_variable, extension_variable), annotated_equations))::bs ->
               let equations = apply_annotations pos (Variable (var, pos)) annotated_equations in
                 Record_selection (name,
                                   label_variable,
                                   extension_variable,
                                   Variable (var, pos),
                                   match_cases
                                     pos
                                     (label_variable::extension_variable::vars)
                                     equations
                                     (match_record pos vars bs def),
                                   pos)

     and match_constant
         : Syntax.position -> string list -> (string * (untyped_expression * annotated_equation list)) list
           -> bound_expression -> bound_expression =
       fun pos vars bs def var ->
         match bs with
           | [] -> apply_default def var
           | (name, (exp, annotated_equations))::bs ->
               let equations = apply_annotations pos (Variable (var, pos)) annotated_equations in
		 (match exp with
		    | Record_empty _ when Settings.get_value unit_hack ->
                        (*
                          This is the only place in the pattern matching compiler that
                          we do type-directed optimisation.

                          We make the assumption that a comparison with unit will always
                          succeed. This assumption is sound providing we're using static
                          typing.

                          This is necessary in order for refinement of variant types to
                          be useful when the variant constructor being matched takes no
                          arguments. All variant constructors are unary in Links, so
                          nullary constructors are represented by unary constructors, 
                          whose sole argument is unit.

                          Consider:
                            switch A {case A -> 0; case (x:B) -> 1;}

                          Without the unit hack, this gets translated to
                            let x = A in
                             case x of
                              A(y) -> if y = () then 0
                                      else let _ = (A(y)):B in 1
                              x -> let _ = x:B in 1
                          which clearly doesn't type check as A(y) can't possibly have type B!

                          Some alternative implementation strategies to consider include:
                           - defer the unit optimisation until after pattern matching
                           compilation
                           - perform this and other type-directed optimisations during pattern
                           matching compilation, providing that static typing is enabled
                        *)
			Let ("_", HasType (Variable (var, pos), Types.unit_type, pos),
			     match_cases pos vars equations def, pos)
		    | _ ->
			(Condition(Comparison(Variable (var, pos), "==", exp, pos),
				   match_cases pos vars equations def,
				   match_constant pos vars bs def var,
                                   pos)))

     (* the interface to the pattern-matching compiler *)
     let match_cases
         : (Syntax.position * untyped_expression * raw_equation list) -> untyped_expression =
       fun (pos, exp, raw_equations) ->
	 let var, wrap =
	   match exp with
	     | Variable (var, _) ->
		 var, Utility.identity
	     | _ ->
		 let var = unique_name()
		 in
		   var, fun body -> Let(var, exp, body, pos)
         and equations = map reduce_equation raw_equations in
         let result = wrap (match_cases pos [var] equations (fun _ -> Wrong pos))
         in
           if (List.for_all (fun (_, (_, used)) -> !used) equations) then
             result
           else
             raise (RedundantPatternMatch(pos))
   end 
     : 
    sig
      type raw_equation = simple_pattern list * untyped_expression
      val match_cases : (Syntax.position * untyped_expression * raw_equation list) -> untyped_expression
    end)


module Desugarer =
  (* Convert a syntax tree as returned by the parser into core syntax *)
(struct
   type quantifier = [`TypeVar of string | `RowVar of string]

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
       | FunctionType (s,t) -> Utility.unduplicate (=) (typevars s @ typevars t)
       | MuType (v, k) -> snd (partition ((=)(`TypeVar v)) (typevars k))
       | TupleType ks -> Utility.concat_map typevars ks
       | RecordType r
       | VariantType r
       | TableType r -> rvars r
       | ListType k -> typevars k
       | MailboxType k -> typevars k
       | UnitType
       | PrimitiveType _
       | DBType -> []

   type assumption = quantifier list * datatype

   let generalize (k : datatype) : assumption =
     typevars k, k

   let generate_var_mapping : quantifier list -> (Types.quantifier list * int StringMap.t) =
     fun vars ->
       List.fold_right
         (fun v (vars, varmap) ->
            let var = Type_basis.fresh_raw_variable () in
              match v with
                | `TypeVar name ->
                    (`TypeVar var::vars, StringMap.add name var varmap)
                | `RowVar name ->
                    (`RowVar var::vars, StringMap.add name var varmap)) vars ([], StringMap.empty)

   let desugar_datatype, desugar_row =
     let rec desugar varmap = 
       let lookup = flip StringMap.find varmap in
       let extend = fun (name, value) -> StringMap.add name value varmap in
         function
           | TypeVar s -> (try `TypeVar (lookup s)
                           with Not_found -> failwith ("Not found `"^ s ^ "' while desugaring assumption"))
           | FunctionType (k1, k2) -> `Function (desugar varmap k1, desugar varmap k2)
           | MuType (v, k) -> let n = Type_basis.fresh_raw_variable () in
                                `Recursive (n, desugar (extend (v,n)) k)
           | UnitType -> Types.unit_type
           | TupleType ks -> 
               let labels = map string_of_int (Utility.fromTo 1 (1 + length ks)) 
               and unit = Types.TypeOps.make_empty_closed_row ()
               and present (s, x) = (s, `Present x)
               in `Record (fold_right2 (curry (Types.TypeOps.set_field -<- present)) labels (map (desugar varmap) ks) unit)
           | RecordType row -> `Record (desugar_row varmap row)
           | VariantType row -> `Variant (desugar_row varmap row)
           | TableType row -> `Table (desugar_row varmap row)
           | ListType k -> `List (desugar varmap k)
           | MailboxType k -> `Mailbox (desugar varmap k)
           | PrimitiveType k -> `Primitive k
           | DBType -> `Primitive `DB
     and desugar_row varmap (fields, rv) =
       let lookup = flip StringMap.find varmap in
       let seed = match rv with
         | None    -> Types.TypeOps.make_empty_closed_row ()
         | Some rv -> Types.TypeOps.make_empty_open_row_with_var (lookup rv)
       and fields = map (fun (k, v) -> match v with
                           | `Absent -> (k, `Absent)
                           | `Present v -> (k, `Present (desugar varmap v))) fields 
       in fold_right Types.TypeOps.set_field fields seed
     in desugar, desugar_row

   let desugar_assumption ((vars, k)  : assumption) : Types.assumption = 
     let vars, varmap = generate_var_mapping vars in
       vars, desugar_datatype varmap k

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
           match s with
             | FloatLit _
             | IntLit _
             | StringLit _
             | BoolLit _
             | CharLit _
             | Var _ -> empty
             | FunLit (_, patterns, body) -> flatten ((List.map ptv patterns) @ [etv body])
             | Spawn e -> etv e
             | ListLit es -> etvs es
             | Definition (_, e, _) -> etv e
             | Iteration (pattern, from, body, filter, sort) ->
                 flatten [ptv pattern; etv from; etv body; opt_etv filter; opt_etv sort]
             | Escape (_, e) ->  etv e
             | HandleWith (e1, name, e2) -> flatten [etv e1; etv e2]
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
             | Projection (e, name) -> etv e
             | SortBy_Conc(pattern, expr, sort_expr) -> flatten [ptv pattern; etv expr; etv sort_expr]

             | TypeAnnotation(e, k) -> flatten [etv e; tv k]

             | ConstructorLit (_, e) -> opt_etv e
             | Switch (exp, binders, def) -> flatten [etv exp; btvs binders; opt_etv2 def]
             | Receive (binders, def) -> flatten [btvs binders; opt_etv2 def]

             | DatabaseLit e -> etv e
             | TableLit (name, datatype, unique, db) -> flatten [tv datatype; etv db]
             | DBDelete (table, rows)
             | DBInsert (table, rows) as op -> flatten [etv table; etv rows]

             | Xml (tag, attrs, subnodes) ->
                 flatten ((List.map (fun (_, es) -> etvs es) attrs) @ [etvs subnodes])
             | XmlForest es -> etvs es
             | TextNode _ -> empty
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
   let polylet : simple_pattern -> position -> untyped_expression -> untyped_expression -> untyped_expression =
     fun pat pos value body ->
       let rec pl pat pos value body =
         match fst pat with
           | `Nil ->
               (Condition(Comparison(value, "==", Syntax.Nil pos, pos),
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
               (Condition(Comparison(value, "==", c, pos),
                          body,
                          Syntax.Wrong pos, pos))
           | `Variable name -> Let (name, value, body, pos)
           | `As (name, pattern) ->
               Let (name, value, pl pattern pos (Variable (name, pos)) body, pos)
           | `HasType (pat, t) ->
               pl pat pos (HasType (value, t, pos)) body
       in
	 pl pat pos value body

   let rec polylets (bindings : (simple_pattern * untyped_expression * position * bool) list) expression =  
     let folder (patt, value, pos, recp) expr = 
       match patt, value, expr, recp with 
         | (`Variable s, _), Abstr _, Rec (bindings, e, p), _ ->  
             Rec ((s, value, None)  :: bindings, e, p) 
         | (`Variable s, _), Abstr _, _, true ->  
             Rec ([s, value, None], expr, pos) 
         | _ ->  
             polylet patt pos value expr in 
       fold_right folder bindings expression 


   let func (pos : position) (body : untyped_expression) : simple_pattern -> untyped_expression = function
     | `Variable name, _ -> Abstr (name, body, pos)
     | pat -> let temp_var = unique_name () in Abstr (temp_var, polylet pat pos (Variable (temp_var, pos)) body, pos)
                                              
   let rec polyfunc (patterns : simple_pattern list) (pos : position) (expr : untyped_expression) : untyped_expression =
     match patterns with 
       | [] -> raise (ASTSyntaxError (pos, "At least one parameter must be defined for a function")) 
       | [patt] -> func pos expr patt 
       | patt :: patts -> func pos (polyfunc patts pos expr) patt

   let string_of_pattern_pos ((pos : Lexing.position), _, expr) = 
     Printf.sprintf "%s:%d:%s" pos.Lexing.pos_fname pos.Lexing.pos_lnum expr

   (* give an error if the pattern has duplicate names *)
   let check_for_duplicate_names ((pattern, pos) : simple_pattern) =
     let rec check_and_add name env =
       if StringSet.mem name env then
         failwith ("Duplicate name '"^ name  ^"' in pattern "^string_of_pattern_pos pos)
       else
         StringSet.add name env in
     let rec check env =
       function
         | `Nil -> env
         | `Cons ((pattern,_), (pattern',_)) ->
             let env' = check env pattern in
               check env' pattern'
         | `Variant (name, (pattern,_)) ->
             check env pattern
         | `Record (name, (pattern,_), (pattern',_)) ->
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

   let desugar lookup_pos (e : phrase) : untyped_expression =
     let _, varmap = (generate_var_mapping -<- get_type_vars) e in
     let rec desugar' lookup_pos ((s, pos') : phrase) : untyped_expression =
       let pos = lookup_pos pos' in
       let desugar = desugar' lookup_pos
       and patternize = simple_pattern_of_pattern varmap lookup_pos in
         match s with
           | TypeAnnotation ((Definition ((`Variable name, _), (FunLit (Some _, patterns, body),_), loc), dpos), t)  -> 
               Define (name,
                       Rec ([name, desugar (FunLit (None, patterns, body), pos'), Some (desugar_datatype varmap t)],
                            Variable (name, pos),
                            pos),
                       loc,pos)
           | TypeAnnotation(e, k) -> HasType(desugar e, desugar_datatype varmap k, pos)
           | FloatLit f  -> Float (f, pos)
           | IntLit i    -> Integer (i, pos)
           | StringLit s -> String (s, pos)
           | BoolLit b   -> Boolean (b, pos)
           | CharLit c   -> Char (c, pos)
           | Var v       -> Variable (v, pos)
           | InfixAppl (`Name ">", e1, e2)  -> Comparison (desugar e2, "<", desugar e1, pos)
           | InfixAppl (`Name ">=", e1, e2)  -> Comparison (desugar e2, "<=", desugar e1, pos)
           | InfixAppl (`Name ("=="|"<"|"<="|"<>" as op), e1, e2)  -> Comparison (desugar e1, op, desugar e2, pos)
           | InfixAppl (`Name "++", e1, e2)  -> Concat (desugar e1, desugar e2, pos)
           | InfixAppl (`Name "!", e1, e2)  -> Apply (Apply (Variable ("send", pos), desugar e1, pos), desugar e2, pos) 
           | InfixAppl (`Name n, e1, e2)  -> Apply (Apply (Variable (n, pos), desugar e1, pos), desugar e2, pos) 
           | InfixAppl (`Cons, e1, e2) -> Concat (List_of (desugar e1, pos), desugar e2, pos)
           | InfixAppl (`RegexMatch, e1, (Regex r, _)) -> Apply (Apply (Variable ("~", pos), desugar e1, pos), 
                                                                 desugar (desugar_regex desugar pos' r, pos'), pos)
           | InfixAppl (`RegexMatch, _, _) -> failwith "Internal error: unexpected rhs of regex operator"
           | InfixAppl (`FloatMinus, e1, e2)  -> Apply (Apply (Variable ("-.", pos), desugar e1, pos), desugar e2, pos) 
           | InfixAppl (`Minus, e1, e2)  -> Apply (Apply (Variable ("-", pos), desugar e1, pos), desugar e2, pos) 
           | InfixAppl (`And, e1, e2) -> Condition (desugar e1, desugar e2, Boolean (false, pos), pos)
           | InfixAppl (`Or, e1, e2)  -> Condition (desugar e1, Boolean (true, pos), desugar e2, pos)
           | ConstructorLit (name, None) -> Variant_injection (name, Record_empty pos, pos)
           | ConstructorLit (name, Some s) -> Variant_injection (name, desugar s, pos)
           | Escape (name, e) -> Syntax.Escape (name, desugar e, pos)
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
           | TableLit (name, datatype, unique, db) -> 
               (let db_query (name:string) (pos:position) (datatype:Types.datatype) (unique:bool) : Query.query =
                  (* FIXME: this is not the appropriate place to gensym the
                     table name. The table will move around later. The right place
                     to do it is when joining two queries: at that point,
                     alpha-convert to ensure that the involved tables have
                     different names. *)
                  let table_name = (db_unique_name ()) in
                  let selects = match datatype with
                    | `Record (field_env, `RowVar row_var) ->
                        let present_fields, absent_fields = Types.split_fields field_env in
                          if row_var = None && absent_fields = [] then
                            List.map (fun
                                        (field_name, field_datatype) ->
                                          {Query.table_renamed = table_name;
                                           name=field_name; renamed=field_name; 
                                           col_type = field_datatype})
                              present_fields
                          else raise (ASTSyntaxError (pos, "Table datatypes are records with only field present elements"))
                    | _ -> raise (ASTSyntaxError (pos, "Table datatypes must be records " ^ Types.string_of_datatype datatype)) in
                    {Query.distinct_only = unique;
                     result_cols = selects;
                     tables = [(`TableName name, table_name)];
                     condition = Query.Boolean true;
                     sortings = [];
                     max_rows = None;
                     offset = Query.Integer (Int 0)} in
                  (* the null query against this table: db_query name pos (desugar_datatype varmap datatype) unique *)
                let row = match datatype with
                    RecordType row ->
                      desugar_row varmap row
                in
                  TableHandle (desugar db, desugar name, row, pos))
           | UnaryAppl (`Minus, e)      -> Apply (Variable ("negate",   pos), desugar e, pos)
           | UnaryAppl (`FloatMinus, e) -> Apply (Variable ("negatef",  pos), desugar e, pos)
           | ListLit  [] -> Nil (pos)
           | ListLit  (e::es) -> Concat (List_of (desugar e, pos), desugar (ListLit (es), pos'), pos)
           | DBDelete (table, rows) ->
               desugar (FnAppl ((Var "deleterows", pos'),
                                ([table;
                                  rows
                                 ], pos')), pos')
           | DBInsert (table, row) -> 
               desugar (FnAppl ((Var "insertrow", pos'),
                                ([table;
                                  row
                                 ], pos')), pos')
           | DatabaseLit e -> Database (desugar e, pos)
           | Definition ((`Variable name, _), e, loc) -> Define (name, desugar e, loc, pos)
           | Definition (_, _, _) -> failwith "top-level patterns not yet implemented"
           | RecordLit (fields, None)   -> fold_right (fun (label, value) next -> Syntax.Record_extension (label, value, next, pos)) (alistmap desugar fields) (Record_empty pos)
           | RecordLit (fields, Some e) -> fold_right (fun (label, value) next -> Syntax.Record_extension (label, value, next, pos)) (alistmap desugar fields) (desugar e)
           | TupleLit [field] -> desugar field
           | TupleLit fields  -> desugar (RecordLit (List.map2 (fun exp n -> string_of_int n, exp) fields (fromTo 1 (1 + length fields)), None), pos')
           | HandleWith (e1, name, e2) -> 
               Syntax.Escape("return", 
                             Let (name, Syntax.Escape("handler",  
                                                      Apply (Variable ("return", pos), 
                                                             desugar e1, pos), pos), desugar e2, pos), pos)
           | FnAppl (fn, ([],ppos))  -> Apply (desugar fn, Record_empty (lookup_pos ppos), pos)
           | FnAppl (fn, ([p], _)) -> Apply (desugar fn, desugar p, pos)
           | FnAppl (fn, (ps, ppos))  -> Apply (desugar fn, desugar (TupleLit ps, ppos), pos)
           | FunLit (None, patterns, body) -> polyfunc (List.map patternize patterns) pos (desugar body)
           | FunLit (Some name, patterns, body) -> Rec ([name, desugar (FunLit (None, patterns, body), pos'), None],
                                                        Variable (name, pos),
                                                        pos)
           | Block (es, exp) -> let es = 
               List.map (function (* pattern * untyped_expression * position * recursivep *)
                           | Binding (p, e), pos -> 
                               (patternize p, desugar e, lookup_pos pos, false)
                           | FunLit (Some n, patts, body), fpos -> 
                               (((`Variable n, pos):simple_pattern), desugar (FunLit (None, patts, body), fpos), 
                                lookup_pos fpos, true)
                           | expr, epos -> 
                               (`Variable "__", pos), desugar (expr, epos), lookup_pos epos, false) es in
                                  polylets es (desugar exp)
           | Foreign (language, name, datatype) -> 
               Alien (language, name, desugar_assumption (generalize datatype), pos)
           | SortBy_Conc(patt, expr, sort_expr) ->
               (match patternize patt with
                  | `Variable var, _ -> 
                      SortBy(desugar expr, (Abstr(var, desugar sort_expr, pos)), pos)
                  | pattern -> failwith("orderby clause on non-simple pattern-matching for is not yet implemented."))
           | Iteration (pattern, from, body, None, None) ->
               (match patternize pattern with
                  | `Variable var, _ -> For (desugar body, var, desugar from, pos)
                  | pattern -> (let var = unique_name () in
                                  For (polylet pattern pos (Variable (var, pos)) (desugar body),
                                       var, desugar from, pos)))
           | Iteration (pattern, from, body, filter_cond, Some sort_expr) -> 
               desugar (Iteration (pattern, (SortBy_Conc(pattern, from, sort_expr), pos'),
                                   body, filter_cond, None),
                        pos')
           | Iteration (pattern, from, body, Some exp, sort_expr) -> 
               desugar (Iteration (pattern, from, 
                                   (Conditional (exp,
                                                 body,
                                                 (ListLit [], pos')), pos'), 
                                   None, sort_expr),
                        pos')
           | Binding _ -> failwith "Unexpected binding outside a block"
           | Switch (exp, patterns, _) ->
               PatternCompiler.match_cases
                 (pos, desugar exp, 
                  (List.map (fun (patt, body) -> ([patternize patt], desugar body)) patterns))
           | Receive (patterns, final) -> 
               desugar (Switch ((FnAppl ((Var "recv", pos'), ([TupleLit [], pos'], pos')), pos'),
                                patterns, final), pos')

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
                 Xml_node (tag, alistmap desugar_attr attrs, map desugar subnodes, pos)
           | XmlForest []  -> Nil  (pos)
           | XmlForest [x] -> desugar x
           | XmlForest (x::xs) -> Concat (desugar x, desugar (XmlForest xs, pos'), pos)
     and desugar_repeat pos : Regex.repeat -> phrasenode = function
       | Regex.Star      -> ConstructorLit ("Star", None)
       | Regex.Plus      -> ConstructorLit ("Plus", None)
       | Regex.Question  -> ConstructorLit ("Question", None)
     and desugar_regex desugar pos : regex -> phrasenode = 
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
     and simple_pattern_of_pattern varmap lookup_pos ((pat,pos') : ppattern) : simple_pattern = 
       let desugar = simple_pattern_of_pattern varmap lookup_pos
       and pos = lookup_pos pos' in
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
                          | other -> failwith "Invalid pattern" (* This message could be better *)),
             pos
         | `Cons (l,r) -> `Cons (desugar l, desugar r), pos
         | `List ps ->
             List.fold_right
               (fun pattern list ->
                  `Cons (desugar pattern, list), pos)
	       ps
               (`Nil, pos)
         | `As (name, p) -> `As (name, desugar p), pos
         | `HasType (p, datatype) -> `HasType (desugar p, desugar_datatype varmap datatype), pos
         | `Variant (l, Some v) ->
             `Variant (l, desugar v), pos
         | `Variant (l, None) ->
             `Variant (l, (`Constant (Record_empty pos), pos)), pos
         | `Record (labs, base) ->
             List.fold_right
               (fun (label, patt) base ->
                  `Record (label, desugar patt, base), pos)
               labs
               ((fromOption (`Constant (Record_empty pos), pos) (Utility.opt_map desugar base)))
         | `Tuple ps ->
             List.fold_right2
               (fun patt n base ->
                  `Record (string_of_int n, desugar patt, base), pos)
               ps
               (Utility.fromTo 1 (1 + List.length ps))
               ((`Constant (Record_empty pos)), pos)
       in let p = aux pat in
            begin
              check_for_duplicate_names p;
              p
            end
     in
       desugar' lookup_pos e

   let desugar_datatype = generalize ->- desugar_assumption

 end : 
  sig 
    val desugar : (pposition -> Syntax.position) -> phrase -> Syntax.untyped_expression
    val desugar_datatype : Sugartypes.datatype -> Types.assumption
  end)

include Desugarer

type directive = string * string list
type sentence = (phrase list, directive) either
type sentence' = (untyped_expression list, directive) either
