open Num
open List

open Query
open Utility
open Syntax

module RewriteSyntax = 
  Rewrite.Rewrite
    (Rewrite.SimpleRewrite
       (struct
          type t = Syntax.untyped_expression
          type rewriter = t -> t option
          let process_children = Syntax.perhaps_process_children
        end))

(*
   subst e u v
     substitutes the variable v for the free variable u in the expression e
*)
let subst : untyped_expression -> string -> string -> untyped_expression = fun exp u v ->
  match
    (RewriteSyntax.bottomup
       (function
	  | Variable (var, data) ->
	      if var=u then Some (Variable (v, data))
	      else None
	  | _ -> None) exp)
  with
      None -> exp
    | Some exp -> exp

exception ConcreteSyntaxError of (string * (Lexing.position * Lexing.position))

let _DUMMY_POS = Syntax.dummy_position
let no_expr_data = (_DUMMY_POS, `Not_typed, None)

let base_value = ref 0

let unique_name () =  
  incr base_value; 
  "g" ^ string_of_int !base_value ^ "" 

let db_unique_name () = 
  incr base_value; 
  "Table_" ^ string_of_int !base_value 
    
(* col_unique_name
   Make up a globally-unique name for a db column usage.
   Note: this is sensitive to capitalization. PostgreSQL, at least, is
   case insensitive, and normalizes table names (?), but our syntax for
   records is case sensitive. *)
let col_unique_name () =
  incr base_value;
  "col_" ^ string_of_int !base_value

let list_head expr pos = 
  Apply(Variable ("hd", pos), expr, pos)

let list_tail expr pos = 
  Apply(Variable ("tl", pos), expr, pos)

type pattern = [
  | `Nil
  | `Cons of (pattern * pattern)
  | `Variant of (string * pattern)
  | `Record of (string * pattern * pattern)
  | `Constant of untyped_expression
  | `Variable of string
  | `As of (string * pattern)
  | `HasType of pattern * Types.datatype
]
deriving (Show, Pickle)

(*
   copied with modifications from errors.ml
     - can't leave it there because it would create a cyclic dependency!
     - we need to sort out our modules
*)
let string_of_pattern_pos ((pos : Lexing.position), _, expr) = 
  Printf.sprintf "%s:%d:%s" pos.Lexing.pos_fname pos.Lexing.pos_lnum expr

(* give an error if the pattern has duplicate names *)
let check_for_duplicate_names pos pattern =
  let rec check_and_add name env =
    if StringSet.mem name env then
      failwith ("Duplicate name '"^ name  ^"' in pattern "^string_of_pattern_pos pos)
    else
      StringSet.add name env in

  let rec check env =
    function
      | `Nil -> env
      | `Cons (pattern, pattern') ->
	  let env' = check env pattern in
	    check env' pattern'
      | `Variant (name, pattern) ->
	  check env pattern
      | `Record (name, pattern, pattern') ->
	  let env' = check env pattern in
	    check env' pattern'
      | `Constant _ -> env
      | `Variable name -> check_and_add name env
      | `As (name, pattern) ->
	  let env = check_and_add name env in
	    check env pattern
      | `HasType (pattern, _) ->
	  check env pattern
  in
    ignore (check StringSet.empty pattern)

(* [TODO]
   - re-enable string_of_pattern
*)
(*
let string_of_pattern = function
  | `Constant _ -> "Constant"
  | `Variable _ -> "Variable"
  | `Record _ -> "Record_extension"
  | `Variant _ -> "Variant"
  | `Nil -> "[]"
  | `Cons _ -> "::"
  | `As _ -> "?"
*)

(* let rec string_of_pattern = function *)
(*   | `Constant expr -> string_of_expression expr *)
(*   | Variable name -> name *)
(*   | `Variable name -> "^" ^ name *)
(*   | `As (name, patt) -> "^" ^ name ^ "&" ^ (string_of_pattern patt) *)
(*   | `Record (label, patt, rem_patt) -> "{#"^ label ^"="^ string_of_pattern patt ^"|"^ string_of_pattern rem_patt ^"}" *)
(*   | `Variant (label, patt) -> "< " ^ label ^ "=" ^ (string_of_pattern patt) ^ ">" *)


let string_of_constant = function
  | Syntax.Boolean (v, _) -> string_of_bool v
  | Syntax.Integer (v, _) -> string_of_num v
  | Syntax.Char (v, _) -> string_of_char v
  | Syntax.String (v, _) -> v
  | Syntax.Float (v, _) -> string_of_float v
  | Syntax.Record_empty _ -> "()"

(** Convert an untyped_expression to a pattern.  Patterns and expressions are
    parsed in the same way, so this is a post-parsing phase *)
let rec patternize_expression = fun exp ->
  match exp with
    | Syntax.Boolean _
    | Syntax.Integer _
    | Syntax.Char _
    | Syntax.String _
    | Syntax.Float _  -> `Constant exp
    | Syntax.Nil _ -> `Nil
    | Syntax.Variable ("_", _) ->  `Variable (unique_name ())
    | Syntax.Variable (name, _) -> `Variable (name)
    | Syntax.Record_empty _ -> `Constant exp
    | Syntax.Record_extension (name, value, record, _)
      -> `Record (name, patternize_expression value, patternize_expression record)
    | Syntax.Variant_injection (name, value, _)
      -> `Variant (name, patternize_expression value)
    | Syntax.Concat (List_of (_, _), _, _) as cons_patt ->
	patternize_cons_pattern cons_patt
    | Syntax.HasType (exp, datatype, _) ->
	`HasType (patternize_expression exp, datatype)
    | other -> raise (ASTSyntaxError (untyped_pos other, Syntax.string_of_expression other ^ " cannot appear in a pattern"))
and patternize_cons_pattern = function
  | Concat (List_of (e1, _), e2, _) ->
      `Cons(patternize_expression e1, patternize_expression e2)
	
let is_null expr pos = 
  Comparison(expr, "==", Nil pos, pos)

let is_not_null expr pos = 
  Comparison(expr, "<>", Nil pos, pos)

let and_expr l r pos = 
  Condition(l, Condition(r, Boolean(true, pos), 
                         Boolean(false, pos), pos), 
            Boolean(false, pos), pos)
      
(* pattern-matching let *)
let rec polylet : (pattern -> position -> untyped_expression -> untyped_expression -> untyped_expression) =
  let rec pl topt pat pos value body =
    let value = match topt with
      | None -> value
      | Some t -> HasType (value, t, pos)
    in
      match pat with
	| `Nil ->
            (Condition(Comparison(value, "==", Syntax.Nil pos, pos),
                       body,
                       Syntax.Wrong pos, pos))
	| `Cons (head, tail) ->
            (pl topt head pos (list_head value pos)
               (pl topt tail pos (list_tail value pos)
                  body))
	| `Variant (name, patt) ->
	    let case_variable = unique_name () in
	    let variable = unique_name () in
	      Variant_selection(value, name,
				case_variable,
				(pl topt patt pos (Variable (case_variable, pos)) body),
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
				pl topt patt pos
                                  (Variable (temp_var_field, pos))
                                  (pl topt rem_patt pos
                                     (Variable (temp_var_ext, pos))
                                     body),
				pos)
	| `Constant c ->
            (Condition(Comparison(value, "==", c, pos),
                       body,
                       Syntax.Wrong pos, pos))
	| `Variable name -> Let (name, value, body, pos)
	| `As (name, pattern) ->
	    Let (name, value, pl topt pattern pos (Variable (name, pos)) body, pos)
	| `HasType (pat, t) ->
	    pl (Some t) pat pos value body
  in
    pl None


(*** pattern matching compiler ***)
(*
  This is similar to the pattern matching compiler described in by
  Phil Wadler in Chapter 5 of 'The Implementation of Functional
  Programming Languages, Simon Peyton Jones, 1987'.

  It is not yet optimised and can result in duplication of terms. In
  order to improve the pattern matching implementation, we may need
  to adjust our intermediate language.
*)

type annotation = string list * Types.datatype list
type annotated_pattern = annotation * pattern

type raw_equation = pattern list * Syntax.untyped_expression
type equation = annotated_pattern list * Syntax.untyped_expression
type annotated_equation = annotation * equation

let rec eq_patterns : pattern * pattern -> bool =
  function
    | `Nil, `Nil | `Nil, `Cons _ | `Cons _, `Nil | `Cons _, `Cons _
    | `Variant _, `Variant _
    | `Record _, `Record _
    | `Constant _, `Constant _
    | `Variable _, `Variable _ -> true
    | `As (_, pattern), pattern' | pattern, `As (_, pattern') -> eq_patterns(pattern, pattern')
    | `HasType (pattern, _), pattern' |  pattern, `HasType (pattern', _) -> eq_patterns(pattern, pattern')
    | _, _ -> false

let eq_equation_patterns : equation * equation -> bool =
  fun (((_, pattern)::_, _), ((_, pattern')::_, _)) -> eq_patterns (pattern, pattern')

type pattern_type = [
| `List | `Variant | `Record | `Constant | `Variable
]

let rec get_pattern_type : pattern -> pattern_type = function
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
let rec reduce_pattern : pattern -> annotated_pattern = function
  | `As (name, pattern) ->
      let (names, datatypes), pattern = reduce_pattern pattern in
	(name::names, datatypes), pattern
  | `HasType (pattern, datatype) ->
      let (names, datatypes), pattern = reduce_pattern pattern in
	(names, datatype::datatypes), pattern
  | pattern -> ([], []), pattern

let reduce_equation (ps, body) =
  (map reduce_pattern ps, body)

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
		     [equation], es::ess
		 in
		   (es, ess)) ([equation], []) equations
	  in
	    List.rev(es :: ess)

(* partition list equations by constructor *)
let partition_list_equations : equation list -> (annotated_equation list * annotated_equation list) =
  fun equations ->
    List.fold_right (fun (ps, body) (nil_equations, cons_equations) ->
		       match ps with
			 | (annotation, `Nil)::ps ->
			     (annotation, (ps, body))::nil_equations, cons_equations
			 | (annotation, `Cons (px, pxs))::ps ->
			     let px = reduce_pattern px in 
			     let pxs = reduce_pattern pxs in
			       nil_equations, (annotation, (px::pxs::ps, body))::cons_equations
			 | _ -> assert false) equations ([], [])

(* partition variant equations by constructor *)
let partition_variant_equations
    : equation list -> (string * ((string * string) * (annotation * equation) list)) list =
  fun equations ->
    assoc_list_of_string_map
      (List.fold_right
	 (fun (ps, body) env ->
	    match ps with
	      | (annotation, `Variant (name, pattern))::ps ->
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
	      | (annotation, `Record (name, pattern, ext_pattern))::ps ->
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
    : equation list -> (string * (Syntax.untyped_expression * annotated_equation list)) list =
  fun equations ->
    assoc_list_of_string_map
      (List.fold_right
	 (fun (ps, body) env ->
	    match ps with
	      | (annotation, `Constant exp)::ps ->
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

(* apply an annotation to an expression
    - rename variables
    - move type annotations into the expression
 *)
let apply_annotation : Syntax.position -> string -> annotation * Syntax.untyped_expression -> Syntax.untyped_expression =
  fun pos var ((names, datatypes), exp) ->
    let exp = List.fold_right (fun name exp ->
				  subst exp name var) names exp in
    let exp = List.fold_right (fun datatype exp ->
				  Let ("_", HasType (Variable (var, pos), datatype, pos), exp, pos)) datatypes exp
    in
      exp

(* apply annotations in an annotated equation list *)
let apply_annotations : Syntax.position -> string -> annotated_equation list -> equation list =
  fun pos var annotated_equations ->
    map (fun (annotation, (ps, body)) ->
	   (ps, apply_annotation pos var (annotation, body))) annotated_equations

(* the entry point to the pattern-matching compiler *)
let rec match_cases
    : Syntax.position -> string list -> equation list -> Syntax.untyped_expression -> Syntax.untyped_expression =
  fun pos vars equations def ->
    match vars, equations with
      | [], [] -> def
      | [], ([], body)::_ -> body
      | _, _ ->
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
	      ) equationss def

and match_var
    : Syntax.position -> string list -> equation list -> Syntax.untyped_expression -> Syntax.untyped_expression =
  fun pos (var::vars) equations def ->
    match_cases pos vars
      (List.map (fun ((annotation, pattern)::ps, body) ->
		   let body = apply_annotation pos var (annotation, body)
		   in
		     match pattern with
		       | `Variable var' ->
			   (ps, subst body var' var)
		       | _ -> assert false) equations) def    
      
and match_list
    : Syntax.position -> string list -> ((annotation * equation) list * annotated_equation list)
    -> Syntax.untyped_expression -> Syntax.untyped_expression =
  fun pos (var::vars) (nil_equations, cons_equations) def ->
    let nil_equations = apply_annotations pos var nil_equations in
    let cons_equations = apply_annotations pos var cons_equations in
    let nil_branch =
      match nil_equations with
	| [] -> def
	| equations -> match_cases pos vars nil_equations def in
    let cons_branch =
      match cons_equations with
	| [] -> def
	| equations ->
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
    Syntax.untyped_expression -> Syntax.untyped_expression
    = fun pos (var::vars) bs def ->
      (* [HACK] attempt to close variant types *)
      let massage_wrong def var = match def with 
	| Syntax.Wrong _ -> Variant_selection_empty(Variable(var, pos), pos)
	| _ -> def in
	match bs with
	  | [] ->
	      massage_wrong def var
	  | (name, ((case_variable, default_variable), annotated_equations))::bs ->
              let equations = apply_annotations pos var annotated_equations in
		Variant_selection(Variable (var, pos), name,
				  case_variable,
				  match_cases pos (case_variable::vars) equations def,
				  default_variable,
				  match_variant pos (default_variable::vars) bs def,
				  pos)

and match_record
    : Syntax.position -> string list ->
    ((string * ((string * string) * annotated_equation list)) list) ->
    Syntax.untyped_expression -> Syntax.untyped_expression
    = fun pos (var::vars) bs def ->
      match bs with
	| [] -> def
	| (name, ((label_variable, extension_variable), annotated_equations))::bs ->
            let equations = apply_annotations pos var annotated_equations in
	      Record_selection (name,
				label_variable,
				extension_variable,
				Variable (var, pos),
				match_cases
				  pos
				  (label_variable::extension_variable::vars)
				  equations
				  (match_record pos (var::vars) bs def),
				pos)

and match_constant
    : Syntax.position -> string list -> (string * (Syntax.untyped_expression * annotated_equation list)) list
    -> Syntax.untyped_expression -> Syntax.untyped_expression =
  fun pos (var::vars) bs def ->
    match bs with
      | [] -> def
      | (name, (exp, annotated_equations))::bs ->
          let equations = apply_annotations pos var annotated_equations in
	    (Condition(Comparison(Variable (var, pos), "==", exp, pos),
		       match_cases pos vars equations def,
		       match_constant pos (var::vars) bs def, pos))     

		
(** With respect to scope of variables bound at the same level the
    rules are these:
    
    * Adjacent function definitions are mutually recursive with
    respect to the scope of their names.
    
    * Other function definitions are simply recursive
    
    * All other RHS can refer to previously established bindings (i.e.
    bindings occuring textually previously.
*)
let rec polylets (bindings : (pattern * untyped_expression * position * bool) list) expression =  
  let folder (patt, value, pos, recp) expr = 
    match patt, value, expr, recp with 
      | `Variable s, Abstr _, Rec (bindings, e, p), _ ->  
          Rec ((s, value)  :: bindings, e, p) 
      | `Variable s, Abstr _, _, true ->  
          Rec ([s, value], expr, pos) 
      | _ ->  
          polylet patt pos value expr in 
    fold_right folder bindings expression 


let func (pos : position) (body : untyped_expression) : pattern -> untyped_expression = function
      | `Variable name -> Abstr (name, body, pos)
      | pat -> let temp_var = unique_name () in Abstr (temp_var, polylet pat pos (Variable (temp_var, pos)) body, pos)

let rec polyfunc (patterns : pattern list) (pos : position) (expr : untyped_expression) : untyped_expression =
  match patterns with 
    | [] -> raise (ASTSyntaxError (pos, "At least one parameter must be defined for a function")) 
    | [patt] -> func pos expr patt 
    | patt :: patts -> func pos (polyfunc patts pos expr) patt

type name = string (* for now *)
type url = string (* for now *)

type unary_op = [
| `Minus
| `FloatMinus
| `Not
]
type comparison_binop = [`Eq | `Less | `LessEq | `Greater | `GreaterEq | `NotEq | `RegexMatch ]
type arith_binop = [`Times | `Div | `Exp | `Plus | `Minus | `FloatTimes | `FloatDiv | `FloatExp | `FloatPlus | `FloatMinus]
type logical_binop = [`And | `Or]
type binop = [comparison_binop | logical_binop | arith_binop | `Concat | `Cons]

type operator = [ unary_op | binop | `Project of name ]
type location = Syntax.location


type order = [`Asc of string | `Desc of string]

type pposition = Lexing.position * Lexing.position (* start * end *)

type datatype = 
  | TypeVar of string
  | FunctionType of datatype * datatype
  | MuType of string * datatype
  | UnitType
  | TupleType of (datatype list)
  | RecordType of row
  | VariantType of row
  | ListType of datatype
  | MailboxType of datatype
  | PrimitiveType of Types.primitive
  | DBType
and row = (string * [`Present of datatype | `Absent]) list * string option

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
    | VariantType r -> rvars r
    | ListType k -> typevars k
    | MailboxType k -> typevars k
    | UnitType
    | PrimitiveType _
    | DBType -> []

type assumption = quantifier list * datatype

let generalize (k : datatype) : assumption =
  typevars k, k

let desugar_assumption ((vars, k)  : assumption) : Types.assumption = 
  let max = length vars in
  let vars, varmap = split (map (fun m ->
                                   let n = Type_basis.fresh_raw_variable () in
                                     match m with 
				       | `TypeVar x -> `TypeVar n, (x, n)
				       | `RowVar x  -> `RowVar n, (x, n))
				    vars) in
  let rec desugar varmap = 
    let lookup = flip assoc varmap in
      function
	| TypeVar s -> (try `TypeVar (lookup s)
			with Not_found -> failwith ("Not found `"^ s ^ "' while desugaring assumption"))
	| FunctionType (k1, k2) -> `Function (desugar varmap k1, desugar varmap k2)
	| MuType (v, k) -> let n = Type_basis.fresh_raw_variable () in
                             `Recursive (n, desugar ((v,n):: varmap) k)
	| UnitType -> Types.unit_type
	| TupleType ks -> 
	    let labels = map string_of_int (Utility.fromTo 1 (1 + length ks)) 
	    and unit = Types.TypeOps.make_empty_closed_row ()
	    and present (s, x) = (s, `Present x)
	    in `Record (fold_right2 (curry (Types.TypeOps.set_field -<- present)) labels (map (desugar varmap) ks) unit)
	| RecordType row -> `Record (desugar_row varmap row)
	| VariantType row -> `Variant (desugar_row varmap row)
	| ListType k -> `List (desugar varmap k)
	| MailboxType k -> `Mailbox (desugar varmap k)
	| PrimitiveType k -> `Primitive k
	| DBType -> `DB
  and desugar_row varmap (fields, rv) = 
    let lookup = flip assoc varmap in
    let seed = match rv with
      | None    -> Types.TypeOps.make_empty_closed_row ()
      | Some rv -> Types.TypeOps.make_empty_open_row_with_var (lookup rv)
    and fields = map (fun (k, v) -> match v with
			| `Absent -> (k, `Absent)
			| `Present v -> (k, `Present (desugar varmap v))) fields 
    in fold_right Types.TypeOps.set_field fields seed
  in (vars, desugar varmap k)
       
let desugar_datatype k = snd (desugar_assumption ([], k))

type phrasenode =
(* ... *)
  | FloatLit of (float)
  | IntLit of (num)
  | StringLit of (string)
  | BoolLit of (bool)
  | CharLit of (char)
  | Var of (name)
  | FunLit of (name option * ppattern list * phrase)
  | Spawn of phrase
  | ListLit of (phrase list)
  | Definition of (name * phrase * location)
  | Iteration of (ppattern * phrase * phrase * (*where:*)phrase option * (*orderby:*)phrase option)
  | Escape of (name * phrase)
  | HandleWith of (phrase * name * phrase)
  | Section of ([arith_binop|`Project of name])
  | Conditional of (phrase * phrase * phrase)
  | Binding of binder
  | Block of (phrase list * phrase)
  | Foreign of (name * name * datatype)
(* Applications *)
  | InfixAppl of (binop * phrase * phrase)
  | Regex of (regex)
  | UnaryAppl of (unary_op * phrase)
  | FnAppl of (phrase * (phrase list * pposition))
  | Send of (phrase * phrase)
(* Record operations *)
  | TupleLit of (phrase list)
  | RecordLit of ((name * phrase) list * phrase option)
  | Projection of (phrase * name)
  | SortBy_Conc of (ppattern * phrase * phrase)

  | TypeAnnotation of (phrase * datatype)
  | TypeSignature of (name * datatype)

(* Variant operations *)
  | ConstructorLit of (name * phrase option)
(*  TBD: remove `None' from Switch constructor *)
  | Switch of (phrase * (binder list) * (name * phrase) option)
  | Receive of ((binder list) * (name * phrase) option)

(* Database operations *)
  | DatabaseLit of (string)
  | TableLit of (string * datatype * bool (* unique *) * phrase)
  | DBUpdate of (string * phrase * phrase)
  | DBDelete of (string * phrase * phrase)
  | DBInsert of (string * phrase * phrase)
(* Xml *)
  | Xml of (name * (string * (phrase list)) list * phrase list)
  | XmlForest of (phrase list)
  | TextNode of (string)
and phrase = (phrasenode * pposition)
and ppattern = | Pattern of phrase (* parse patterns as phrases, then convert later: avoids ambiguities in the grammar  *)
	       | AsPattern of string * phrase
and binder = ppattern * phrase
and regex = | Range of (char * char)
            | Simply of string
            | Any
            | Seq of regex list
            | Repeat of (Regex.repeat * regex)
            | Splice of phrase

let asPattern (Pattern phrase, name) = AsPattern (name, phrase)

let _DUMMY_PHRASE = TupleLit [], (Lexing.dummy_pos, Lexing.dummy_pos)
let _DUMMY_PATTERN = Pattern _DUMMY_PHRASE

let rec curried_apply (head : untyped_expression) (pos : position) : untyped_expression list -> untyped_expression = function 
  | [expr] -> Apply (head, expr, pos)
  | expr :: exprs -> curried_apply (Apply (head, expr, pos)) pos exprs 
  | [] -> failwith "Internal error : curried_apply to zero arguments"

let uncompare : comparison_binop -> string = 
  (* FIXME: this is buggy: should eliminate greater, greatereq *)
  flip List.assoc [`Eq, "=="; `Less, "<"; `LessEq, "<="; `Greater, ">"; `GreaterEq, ">="; `NotEq, "<>"; `RegexMatch, "~"]
and unarith : arith_binop -> string = 
  flip List.assoc [`Times, "*"; `Div, "/"; `Exp, "^"; `Plus, "+"; `Minus, "-"; `FloatTimes, "*."; `FloatDiv, "/."; `FloatExp, "^."; `FloatPlus, "+."; `FloatMinus, "-."]


(* [TODO]
     - change typevars to return a set
     - implement typevars for phrases
     - construct a map from typevars as strings to typevars as numbers
     - use this to ensure that free type vars are identified and properly
     quantified
*)

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
	  | TypeAnnotation(e, k) -> flatten [etv e; tv k]
	  | FloatLit _
	  | IntLit _
	  | StringLit _
	  | BoolLit _
	  | CharLit _
	  | Var _ -> empty
	  | InfixAppl (_, e1, e2) -> flatten [etv e1; etv e2]
	  | ConstructorLit (_, e) -> opt_etv e
	  | Escape (_, e)
	  | Spawn e -> etv e
	  | Section _ -> empty
	  | Conditional (e1, e2, e3) -> flatten [etv e1; etv e2; etv e3]
	  | Projection (e, name) -> etv e
	  | TableLit (name, datatype, unique, db) -> flatten [tv datatype; etv db]
	  | UnaryAppl (_, e) -> etv e
	  | ListLit es -> etvs es
	  | DBUpdate (table, db, rows)
	  | DBDelete (table, db, rows)
	  | DBInsert (table, db, rows) as op -> flatten [etv db; etv rows]
	  | DatabaseLit s -> empty
	  | Definition (_, e, _) -> etv e
	  | RecordLit (fields, e) ->
	      flatten ((List.map (fun (_, field) -> etv field) fields) @ [opt_etv e])
	  | TupleLit fields -> etvs fields
	  | HandleWith (e1, name, e2) ->
	      flatten [etv e1; etv e2]
	  | FnAppl (fn, (ps, _)) -> flatten [etv fn; etvs ps]
	  | Send (l, r) -> flatten [etv l; etv r]
	  | FunLit (None, patterns, body) -> flatten ((List.map ptv patterns) @ [etv body])
	  | Block (es, exp) -> flatten [etvs es; etv exp]
	  | Foreign (_, _, datatype) -> tv datatype
	  | SortBy_Conc(pattern, expr, sort_expr) -> flatten [ptv pattern; etv expr; etv sort_expr]
	  | Iteration (pattern, from, body, filter, sort) ->
	      flatten [ptv pattern; etv from; etv body; opt_etv filter; opt_etv sort]
	  | Binding b -> btv b
	  | Switch (exp, binders, def) -> flatten [etv exp; btvs binders; opt_etv2 def]
	  | Receive (binders, def) -> flatten [btvs binders; opt_etv2 def]
	  | TextNode _ -> empty
	  | Xml (tag, attrs, subnodes) ->
	      flatten ((List.map (fun (_, es) -> etvs es) attrs) @ [etvs subnodes])
	  | XmlForest es -> etvs es
  and get_pattern_type_vars = function
    | Pattern e
    | AsPattern (_, e) -> get_type_vars e
  in
    union -<- get_type_vars
    


(* Convert a syntax tree as returned by the parser into core syntax *)
let rec desugar lookup_pos ((s, pos') : phrase) : Syntax.untyped_expression = 
  let pos = lookup_pos pos' in 
  let desugar = desugar lookup_pos
  and patternize = patternize lookup_pos in
    match s with
      | TypeAnnotation(e, k) -> HasType(desugar e, 
					snd (desugar_assumption (generalize k)), pos)
      | FloatLit f  -> Float (f, pos)
      | IntLit i    -> Integer (i, pos)
      | StringLit s -> String (s, pos)
      | BoolLit b   -> Boolean (b, pos)
      | CharLit c   -> Char (c, pos)
      | Var v       -> Variable (v, pos)
      | InfixAppl (`Cons, e1, e2) -> Concat (List_of (desugar e1, pos), desugar e2, pos)
      | InfixAppl (`Concat, e1, e2) -> Concat (desugar e1, desugar e2, pos)
      | InfixAppl (`Greater, e1, e2) -> desugar (InfixAppl (`Less, e2, e1), pos')
      | InfixAppl (`GreaterEq, e1, e2) -> desugar (InfixAppl (`LessEq, e2, e1), pos')
      | InfixAppl (`RegexMatch, e1, (Regex r, _)) -> 
	  Apply (Apply (Variable ("~", pos), desugar e1, pos), 
		 desugar (desugar_regex desugar pos' r, pos'), pos)
      | InfixAppl (`RegexMatch, _, _) -> failwith "Internal error: unexpected rhs of regex operator"
      | InfixAppl (#comparison_binop as p, e1, e2) -> Comparison (desugar e1, uncompare p, desugar e2, pos)
      | InfixAppl (#arith_binop as a, e1, e2)  -> Apply (Apply (Variable (unarith a, pos), desugar e1, pos), desugar e2, pos) 
      | InfixAppl (`And, e1, e2) -> Condition (desugar e1, desugar e2, Boolean (false, pos), pos)
      | InfixAppl (`Or, e1, e2)  -> Condition (desugar e1, Boolean (true, pos), desugar e2, pos)
      | ConstructorLit (name, None) -> Variant_injection (name, Record_empty pos, pos)
      | ConstructorLit (name, Some s) -> Variant_injection (name, desugar s, pos)
      | Escape (name, e) -> Syntax.Escape (name, desugar e, pos)
      | Spawn e -> desugar (FnAppl ((FnAppl ((Var "spawn", pos'), 
                                             ([FunLit (None, [Pattern (RecordLit ([], None), pos')], e), 
                                               pos'], pos')),
                                     pos'), ([], pos')), pos')
      | Section (#arith_binop as a) -> Variable (unarith a, pos)
      | Section (`Project name) -> (let var = unique_name () in
				      desugar (FunLit (None, [Pattern (Var var, pos')], 
						       (Projection ((Var var, pos'), name), pos')), pos'))
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
			             {table_renamed = table_name;
			              name=field_name; renamed=field_name; 
                                      col_type = field_datatype})
			 present_fields
	             else raise (ASTSyntaxError (pos, "Table datatypes are records with only field present elements"))
               | _ -> raise (ASTSyntaxError (pos, "Table datatypes must be records " ^ Types.string_of_datatype datatype)) in
               {distinct_only = unique;
		result_cols = selects;
		tables = [(name, table_name)];
		condition = Query.Boolean true;
		sortings = [];
		max_rows = None;
		offset = Query.Integer (Int 0)} in
             Table (desugar db, "IGNORED", db_query name pos (desugar_datatype datatype) unique, pos))
      | UnaryAppl (`Minus, e)      -> Apply (Variable ("negate",   pos), desugar e, pos)
      | UnaryAppl (`FloatMinus, e) -> Apply (Variable ("negatef",  pos), desugar e, pos)
      | UnaryAppl (`Not, e)        -> Apply (Variable ("not", pos), desugar e, pos)
      | ListLit  [] -> Nil (pos)
      | ListLit  (e::es) -> Concat (List_of (desugar e, pos), desugar (ListLit (es), pos'), pos)
      | DBUpdate (table, db, rows)
      | DBDelete (table, db, rows)
      | DBInsert (table, db, rows) as op -> 
	  let fn =  match op with
            | DBUpdate _ -> "updaterows"
            | DBDelete _ -> "deleterows"
            | DBInsert _ -> "insertrow" 
            | _ -> assert false in
	    desugar (FnAppl ((Var fn, pos'),
			     ([StringLit table, pos'; 
                               db;
                               rows
                              ], pos')), pos')
      | DatabaseLit s -> Database (String (s, pos), pos)
      | Definition (name, e, loc) -> Define (name, desugar e, loc, pos)
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
      | Send (l, r)      -> desugar (FnAppl ((FnAppl ((Var "send", pos'), ([l], pos')), pos'), ([r], pos')), pos')

      | FunLit (None, patterns, body) -> polyfunc (List.map patternize patterns) pos (desugar body)
      | FunLit (Some name, patterns, body) -> Rec ([name, desugar (FunLit (None, patterns, body), pos')],
						   Variable (name, pos),
						   pos)
      | Block (es, exp) -> let es = 
	  List.map (function (* pattern * untyped_expression * position * recursivep *)
                      | Binding (p, e), pos -> 
			  (patternize p, desugar e, lookup_pos pos, false)
                      | FunLit (Some n, patts, body), pos -> 
			  (`Variable n, desugar (FunLit (None, patts, body), pos), 
			   lookup_pos pos, true)
                      | expr, pos -> 
			  `Variable "__", desugar (expr, pos), lookup_pos pos, false) es in
	  polylets es (desugar exp)
      | Foreign (language, name, datatype) -> 
	  Alien (language, name, desugar_assumption (generalize datatype), pos)
      | SortBy_Conc(patt, expr, sort_expr) ->
	  (match patternize patt with
             | `Variable var -> 
		 SortBy(desugar expr, (Abstr(var, desugar sort_expr, pos)), pos)
             | pattern -> failwith("orderby clause on non-simple pattern-matching for is not yet implemented."))
      | Iteration (pattern, from, body, None, None) ->
	  (match patternize pattern with
             | `Variable var -> For (desugar body, var, desugar from, pos)
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
	  let x = unique_name () in
	    Let(x, desugar exp,
		match_cases
		  pos
		  [x]
		  (List.map (fun (patt, body) ->
			       reduce_equation ([patternize patt], desugar body)) patterns)
		  (Syntax.Wrong pos),
	  pos)
| Receive (patterns, final) -> 
    desugar (Switch ((FnAppl ((Var "recv", pos'), ([TupleLit [], pos'], pos')), pos'),
                     patterns, final), pos')

(* (\* TBD: We should die if the XML text literal has bare ampersands or *)
(*    is otherwise ill-formed. It should also be made to properly handle *)
(*    CDATA. *)
(*    Where's a good place to do so? *)
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
and patternize lookup_pos : ppattern -> pattern = fun ppattern ->
  (* For now, simply delegate to the old patternize.  Eventually, we
     should convert directly from phrases to patterns *)
  let pattern, pos = match ppattern with
    | Pattern ((_, pos') as p) -> patternize_expression (desugar lookup_pos p), lookup_pos pos'
    | AsPattern (x, ((_, pos') as p)) -> `As (x, patternize_expression (desugar lookup_pos p)), lookup_pos pos'
  in
    check_for_duplicate_names pos pattern;
    pattern
and list_patt_to_bool value pos = function
    Var _, _ -> Boolean(true, pos)
  | TupleLit [p], _ -> list_patt_to_bool value pos p
  | ListLit (head::tail), ppos ->
      and_expr (is_not_null value pos)
        (and_expr (list_patt_to_bool (list_head value pos) pos head)
           (list_patt_to_bool (list_tail value pos) pos (ListLit tail, ppos)) pos) pos
  | InfixAppl(`Cons, head, tail), _ ->
      and_expr (is_not_null value pos)
        (and_expr (list_patt_to_bool (list_head value pos) pos head)
           (list_patt_to_bool (list_tail value pos) pos tail) pos) pos
  | ListLit [], _ -> is_null value pos
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
      let v = gensym "_regex_" in
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
             (fun (v, e1) -> Binding (Pattern (Var v, pos), e1), pos)
             !exprs,
           (e, pos))
          


(* (\* project_subset *)

(*    creates a new record from an old one, with the new record showing  *)
(*    just a subset of the fields present in the original. *)

(*    Suppose the source is s = (l1=3,l2="four") *)

(*    It works like this: *)
(*      (l1=a|r) = s; *)
(*        (l2=b|r) = s; *)
(*          (l3=c|r) = s; *)
(*            (p1=a | (p2=b | (p3=c | ()))) *)

(*    [If only we could do this: *)
(*       (l1=a.l1, l2=b.l2, l3=c.l3)] *)
(* *\) *)


(* d: internal shorthand for nowhere_pos *)
let d = no_expr_data

let project_subset (fields : (string * string) list) (source : Syntax.expression) : Syntax.expression =
  let dummy = unique_name () in
    (* generate a fresh variable for each (old, noo) pair in the input *)
  let variables = map (fun (old, noo) -> (unique_name (), old, noo)) fields in
  let recext_builder (var, _, noo) record = Syntax.Record_extension(noo, Syntax.Variable(var, d), record, d) in
  let select (label_var, old, _) body = Syntax.Record_selection(old, label_var, dummy, source, body, d) in
    (* build a record using the free variables we named above *)
  let record_extension = fold_right recext_builder variables (Record_empty d) in
    (* project out the source record into the free variables *)
    fold_right select variables record_extension


type directive = string * string list
type sentence = (phrase list, directive) either
type sentence' = (untyped_expression list, directive) either

