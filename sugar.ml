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
  | `Constant of untyped_expression 
  | `Bind of string 
  | `Bind_using of (string * pattern) (* [QUESTION] What's this? *)
  | `Record_extension of (string * pattern * pattern) 
  | `Empty_record
  | `Variant of (string * pattern)
  | `Nil
  | `Cons of (pattern * pattern)
]

(* [TODO]
   - re-enable string_of_pattern
*)
(*
let string_of_pattern = function
  | `Constant _ -> "Constant"
  | `Bind _ -> "Variable"
  | `Record_extension _ -> "Record_extension"
  | `Empty_record -> "()"
  | `Variant _ -> "Variant"
  | `Nil -> "[]"
  | `Cons _ -> "::"
  | `Bind_using _ -> "?"
*)

(* let rec string_of_pattern = function *)
(*   | `Constant expr -> string_of_expression expr *)
(*   | Variable name -> name *)
(*   | `Bind name -> "^" ^ name *)
(*   | `Bind_using (name, patt) -> "^" ^ name ^ "&" ^ (string_of_pattern patt) *)
(*   | `Record_extension (label, patt, rem_patt) -> "{#"^ label ^"="^ string_of_pattern patt ^"|"^ string_of_pattern rem_patt ^"}" *)
(*   | `Empty_record -> "()" *)
(*   | `Variant (label, patt) -> "< " ^ label ^ "=" ^ (string_of_pattern patt) ^ ">" *)


let string_of_constant = function
  | Syntax.Boolean (v, _) -> string_of_bool v
  | Syntax.Integer (v, _) -> string_of_num v
  | Syntax.Char (v, _) -> string_of_char v
  | Syntax.String (v, _) -> v
  | Syntax.Float (v, _) -> string_of_float v

(** Convert an untyped_expression to a pattern.  Patterns and expressions are
    parsed in the same way, so this is a post-parsing phase *)
let rec patternize' = fun exp ->
  match exp with
    | Syntax.Boolean _
    | Syntax.Integer _
    | Syntax.Char _
    | Syntax.String _
    | Syntax.Float _  -> `Constant exp
    | Syntax.Nil _ -> `Nil
    | Syntax.Variable ("_", _) ->  `Bind (unique_name ())
    | Syntax.Variable (name, _) -> `Bind (name)
    | Syntax.Record_empty _ -> `Empty_record
    | Syntax.Record_extension (name, value, record, _)
      -> `Record_extension (name, patternize' value, patternize' record)
    | Syntax.Variant_injection (name, value, _)
      -> `Variant (name, patternize' value)
	(* See note above `amper' *)
    | Syntax.Apply(Syntax.Variable("&", _),
                   Apply(Syntax.Variable (var, _), expr, _), _) -> 
	`Bind_using(var, patternize' expr)
    | Syntax.Concat (List_of (_, _), _, _) as cons_patt ->
	patternize_cons_pattern cons_patt
    | other -> raise (ASTSyntaxError (untyped_pos other, Syntax.string_of_expression other ^ " cannot appear in a pattern"))
and patternize_cons_pattern = function
  | Concat (List_of (e1, _), e2, _) ->
      `Cons(patternize' e1, patternize' e2)
	
let is_null expr pos = 
  Comparison(expr, "==", Nil pos, pos)

let is_not_null expr pos = 
  Comparison(expr, "<>", Nil pos, pos)

let and_expr l r pos = 
  Condition(l, Condition(r, Boolean(true, pos), 
                         Boolean(false, pos), pos), 
            Boolean(false, pos), pos)
      

let rec polylet : (pattern -> position -> untyped_expression -> untyped_expression -> untyped_expression) =
  fun pat pos value body ->
    match pat with
(*       | `Constant _     -> failwith "Constants cannot be used in function parameter or let patterns" *)
      | `Variant (name, patt) ->
	  let case_variable = unique_name () in
	  let variable = unique_name () in
	    Variant_selection(value, name,
			      case_variable,
			      (polylet patt pos (Variable (case_variable, pos)) body),
			      variable,
			      Variant_selection_empty(Variable(variable, pos), pos),
			      pos)
      | `Bind_using _   -> failwith "Bind-using cannot be used in function parameter or let patterns"
      | `Bind name -> Let (name, value, body, pos)
      | `Constant c ->
          (Condition(Comparison(value, "==", c, pos),
                     body,
                     Syntax.Wrong pos, pos))
      | `Nil ->
          (Condition(Comparison(value, "==", Syntax.Nil pos, pos),
                     body,
                     Syntax.Wrong pos, pos))
      | `Cons (head, tail) -> 
          (polylet head pos (list_head value pos)
             (polylet tail pos (list_tail value pos)
                body))
      | `Record_extension (label, patt, rem_patt) ->
	  let temp_var_field = unique_name () in
	  let temp_var_ext = unique_name () in
	    Record_selection (label,
                              temp_var_field,
                              temp_var_ext,
                              value,
                              polylet patt pos
                                (Variable (temp_var_field, pos))
                                (polylet rem_patt pos
                                   (Variable (temp_var_ext, pos))
                                   body),
                              pos)
      | `Empty_record -> Record_selection_empty (value, body, pos)


(*** pattern matching compiler ***)
(*
  This is similar to the pattern matching compiler described in by
  Phil Wadler in Chapter 5 of 'The Implementation of Functional
  Programming Languages, Simon Peyton Jones, 1987'.

  It is not yet optimised and can result in duplication of terms. In
  order to improve the pattern matching implementation, we may need
  to adjust our intermediate language.
*)
type equation = pattern list * Syntax.untyped_expression

let eq_pattern : equation * equation -> bool = fun ((pattern::_, _), (pattern'::_, _)) ->
  match pattern, pattern' with
    | `Nil, `Nil | `Nil, `Cons _ | `Cons _, `Nil | `Cons _, `Cons _
    | `Variant _, `Variant _
    | `Empty_record, `Empty_record
    | `Record_extension _, `Record_extension _
    | `Constant _, `Constant _
    | `Bind _, `Bind _ -> true
    | _, _ -> false

type pattern_type = [
| `List | `Variant | `Empty | `Record | `Constant | `Variable
]

let get_pattern_type : equation -> pattern_type = fun (pattern::_, _) ->
  match pattern with
    | `Nil | `Cons _ -> `List
    | `Variant _ -> `Variant
    | `Empty_record -> `Empty
    | `Record_extension _ -> `Record
    | `Constant _ -> `Constant
    | `Bind _ -> `Variable
    | `Bind_using _ -> assert false

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
	      
let partition_list_equations (equations : equation list) =
  List.fold_right (fun (ps, body) (nil_equations, cons_equations) ->
		     match ps with
		       | `Nil::ps ->
			   (ps, body)::nil_equations, cons_equations
		       | `Cons (px, pxs)::ps ->
			   nil_equations, (px::pxs::ps, body)::cons_equations
		       | _ -> assert false) equations ([], [])

let partition_variant_equations
    : equation list -> (string * ((string * string) * equation list)) list =
  fun equations ->
    assoc_list_of_string_map
      (List.fold_right
	 (fun (ps, body) env ->
	    match ps with
	      | `Variant (name, pattern)::ps ->
		  let vars, equations = 
		    if StringMap.mem name env then
		      StringMap.find name env
		    else
		      (unique_name (), unique_name ()), []
		  in
		    StringMap.add name (vars, (pattern::ps, body)::equations) env
	      | _ -> assert false
	 ) equations StringMap.empty)

let partition_record_equations
    : equation list -> (string * ((string * string) * equation list)) list =
  fun equations ->
    assoc_list_of_string_map
      (List.fold_right
	 (fun (ps, body) env ->
	    match ps with
	      | `Record_extension (name, pattern, ext_pattern)::ps ->
		  let vars, equations =
		    if StringMap.mem name env then
		      StringMap.find name env
		    else
		      (unique_name (), unique_name ()), []
		  in
		    StringMap.add name (vars, (pattern::ext_pattern::ps, body)::equations) env
	      | _ -> assert false
	 ) equations StringMap.empty)

let partition_constant_equations
    : equation list -> (string * (Syntax.untyped_expression * equation list)) list =
  fun equations ->
    assoc_list_of_string_map
      (List.fold_right
	 (fun (ps, body) env ->
	    match ps with
	      | `Constant exp::ps ->
		  let name = string_of_constant exp in
		  let exp, equations = 
		    if StringMap.mem name env then
		      StringMap.find name env
		    else
		      exp, []
		  in
		    StringMap.add name (exp, (ps, body)::equations) env
	      | _ -> assert false
	 ) equations StringMap.empty)

let rec match_cases
    : Syntax.position -> string list -> equation list -> Syntax.untyped_expression -> Syntax.untyped_expression =
  fun pos vars equations def ->
    match vars, equations with
      | [], [] -> def
      | [], ([], body)::_ -> body
      | _, _ ->
	  let equationss = partition_equations eq_pattern equations in
	    List.fold_right
	      (fun equations exp ->
		 match get_pattern_type (List.hd equations) with
		   | `List -> match_list pos vars (partition_list_equations equations) exp
		   | `Variant ->
		       match_variant pos vars (partition_variant_equations equations) exp
		   | `Variable ->
		       match_var pos vars equations exp
		   | `Empty -> 		      
		       match_empty pos vars equations exp
		   | `Record ->
		       match_record pos vars (partition_record_equations equations) exp
		   | `Constant ->
		       match_constant pos vars (partition_constant_equations equations) exp
	      ) equationss def

and match_var
    : Syntax.position -> string list -> equation list -> Syntax.untyped_expression -> Syntax.untyped_expression =
  fun pos (var::vars) equations def ->
    match_cases pos vars
      (List.map (fun (pattern::ps, body) ->
		   match pattern with
		     | `Bind var' ->
			 (ps, subst body var' var)
		     | _ -> assert false) equations) def

and match_empty
    : Syntax.position -> string list -> equation list -> Syntax.untyped_expression -> Syntax.untyped_expression =
  fun pos (var::vars) equations def ->
    match_cases pos vars
      (List.map (fun (pattern::ps, body) ->
		   match pattern with
		     | `Empty_record ->
			 (ps, body)
		     | _ -> assert false) equations) def

and match_list
    : Syntax.position -> string list -> (equation list * equation list)
    -> Syntax.untyped_expression -> Syntax.untyped_expression =
  fun pos (var::vars) (nil_equations, cons_equations) def ->
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
    : Syntax.position -> string list -> ((string * ((string * string) * equation list)) list) ->
    Syntax.untyped_expression -> Syntax.untyped_expression
    = fun pos (var::vars) bs def ->
      (* [HACK] attempt to close variant types *)
      let massage_wrong def var = match def with 
	| Syntax.Wrong _ -> Variant_selection_empty(Variable(var, pos), pos)
	| _ -> def in
      match bs with
	| [] ->
	    massage_wrong def var
	| (name, ((case_variable, default_variable), equations))::bs ->
	    Variant_selection(Variable (var, pos), name,
			      case_variable,
			      match_cases pos (case_variable::vars) equations def,
			      default_variable,
			      match_variant pos (default_variable::vars) bs def,
			      pos)

and match_record
    : Syntax.position -> string list ->
    ((string * ((string * string) * equation list)) list) ->
    Syntax.untyped_expression -> Syntax.untyped_expression
    = fun pos (var::vars) bs def ->
      match bs with
	| [] -> def
	| (name, ((label_variable, extension_variable), equations))::bs ->
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
    : Syntax.position -> string list -> (string * (Syntax.untyped_expression * equation list)) list
    -> Syntax.untyped_expression -> Syntax.untyped_expression =
  fun pos (var::vars) bs def ->
    match bs with
      | [] -> def
      | (name, (exp, equations))::bs ->
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
      | `Bind s, Abstr _, Rec (bindings, e, p), _ ->  
          Rec ((s, value)  :: bindings, e, p) 
      | `Bind s, Abstr _, _, true ->  
          Rec ([s, value], expr, pos) 
      | _ ->  
          polylet patt pos value expr in 
    fold_right folder bindings expression 


let func (pos : position) (body : untyped_expression) : pattern -> untyped_expression = function
      | `Bind name -> Abstr (name, body, pos)
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
  | Binding of (ppattern * phrase)
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

(* Variant operations *)
  | ConstructorLit of (name * phrase option)
(*  TBD: remove `None' from Switch constructor *)
  | Switch of (phrase * (switchcase list) * (name * phrase) option)
  | Receive of ((switchcase list) * (name * phrase) option)

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
and ppattern = Pattern of phrase (* parse patterns as phrases, then convert later: avoids ambiguities in the grammar  *)
and switchcase = ppattern * phrase
and regex = | Range of (char * char)
            | Simply of string
            | Any
            | Seq of regex list
            | Repeat of (Regex.repeat * regex)
            | Splice of phrase

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
  flip List.assoc [`Times, "*"; `Div, "/"; `Exp, "^"; `Plus, "+"; `Minus, "-"; `FloatTimes, "*."; `FloatDiv, "/."; `FloatExp, "^^"; `FloatPlus, "+."; `FloatMinus, "-."]

(* TBD: Will be obviated when we use straightahead bindings with
   failure, as in Wadler/Peyton-Jones. *)
let rec list_type_switch = function
    [] -> false
  | (Pattern(ListLit _, _), _) :: _ 
  | (Pattern(TupleLit [ListLit _, _], _), _) :: _ 
  | (Pattern(InfixAppl (`Cons, _, _), _), _) :: _
  | (Pattern(TupleLit [InfixAppl (`Cons, _, _), _], _), _) :: _ 
    -> true
  | (Pattern(Var _, _), _) :: cases
  | (Pattern(TupleLit [Var _, _], _), _) :: cases
    -> list_type_switch cases
  | _ -> false
      
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
		offset = Query.Integer (Num.Int 0)} in
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
			  (`Bind n, desugar (FunLit (None, patts, body), pos), 
			   lookup_pos pos, true)
                      | expr, pos -> 
			  `Bind "__", desugar (expr, pos), lookup_pos pos, false) es in
	  polylets es (desugar exp)
      | Foreign (language, name, datatype) -> 
	  Alien (language, name, desugar_assumption (generalize datatype), pos)
      | SortBy_Conc(patt, expr, sort_expr) ->
	  (match patternize patt with
             | `Bind var -> 
		 SortBy(desugar expr, (Abstr(var, desugar sort_expr, pos)), pos)
             | pattern -> failwith("orderby clause on non-simple pattern-matching for is not yet implemented."))
      | Iteration (pattern, from, body, None, None) ->
	  (match patternize pattern with
             | `Bind var -> For (desugar body, var, desugar from, pos)
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

      (* FIXME: The following is not correct when all cases are 
         variable patterns *)
      | Switch (exp, (patterns), dflt) when list_type_switch patterns
	  -> open_list_match (desugar exp) patterns dflt lookup_pos pos

      | Switch (exp, patterns, _) ->
	  let x = unique_name () in
	    Let(x, desugar exp,
		match_cases
		  pos
		  [x]
		  (List.map (fun (Pattern patt, body) ->
			       [patternize' (desugar patt)], desugar body) patterns)
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
and patternize lookup_pos : ppattern -> pattern = function
    (* For now, simply delegate to the old patternize.  Eventually, we
       should convert directly from phrases to patterns *)
  | Pattern p -> patternize' (desugar lookup_pos p)
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
and open_list_match value cases default lookup_pos pos =
    let inner_case =
      match default with
          None -> Wrong pos
	| Some(x, body) -> Let(x, value, desugar lookup_pos body, pos)
    in
      fold_right (fun (Pattern patt, body) otherwise ->
                    Condition(list_patt_to_bool value pos patt,
                              polylet (patternize' (desugar lookup_pos patt)) pos value (desugar lookup_pos body),
                              otherwise, pos)
		 ) cases inner_case
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

