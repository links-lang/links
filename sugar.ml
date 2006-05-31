open Num
open List

open Query
open Utility
open Syntax

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

type pattern = 
  | Constant of untyped_expression 
  | Bind of string 
  | Bind_using of (string * pattern) 
  | Record_extension of (string * pattern * pattern) 
  | Empty_record 
  | Variant of (string * pattern) 
  | Cons of (pattern * pattern)

(* let rec string_of_pattern = function *)
(*   | Constant expr -> string_of_expression expr *)
(*   | Variable name -> name *)
(*   | Bind name -> "^" ^ name *)
(*   | Bind_using (name, patt) -> "^" ^ name ^ "&" ^ (string_of_pattern patt) *)
(*   | Record_extension (label, patt, rem_patt) -> "{#"^ label ^"="^ string_of_pattern patt ^"|"^ string_of_pattern rem_patt ^"}" *)
(*   | Empty_record -> "()" *)
(*   | Variant (label, patt) -> "< " ^ label ^ "=" ^ (string_of_pattern patt) ^ ">" *)

(* let defrec : (string -> position -> untyped_expression -> location -> untyped_expression) = fun name pos expr location -> *)
(*   Define (name, Rec ([name, expr], Variable (name, pos), pos), location, pos) *)

(* let record_selection : ((string * string) list -> position -> string -> untyped_expression -> untyped_expression -> untyped_expression) = *)
(*   fun selects pos record value body -> *)
(*     let rec one_more = (fun value -> function *)
(* 			  | (label, label_var) :: [] -> *)
(* 			      Record_selection (label, label_var, record, value, body, pos) *)
(* 			  | (label, label_var) :: selects -> *)
(* 			      let new_var = (unique_name ()) in *)
(* 				Record_selection (label, label_var, new_var, value, (one_more (Variable (new_var, pos)) selects), pos) *)
(* 			  | [] -> raise (Failure "Programming error (SU025)") *)
(* 		       ) in *)
(*       one_more value selects *)

(** Convert an untyped_expression to a pattern.  Patterns and expressions are
    parsed in the same way, so this is a post-parsing phase *)
let rec patternize' = function 
  | Syntax.Nil _
  | Syntax.Variable ("_", _) ->  Bind (unique_name ())
  | Syntax.Variable (name, _) -> Bind (name)
  | Syntax.Record_empty _ -> Empty_record
  | Syntax.Record_extension (name, value, record, _) 
    -> Record_extension (name, patternize' value, patternize' record)
  (* See note above `amper' *)
  | Syntax.Apply(Syntax.Variable("&", _),
                 Apply(Syntax.Variable (var, _), expr, _), _) -> 
      Bind_using(var, patternize' expr)
  | Syntax.Concat (List_of (_, _), _, _) as cons_patt ->
      patternize_cons_pattern cons_patt
  | other -> raise (ASTSyntaxError (untyped_pos other, Syntax.string_of_expression other ^ " cannot appear in a pattern"))
and patternize_cons_pattern = function
  | Concat (List_of (e1, _), e2, _) ->
      Cons(patternize' e1, patternize' e2)

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
      | Constant _     -> failwith "Constants cannot be used in function parameter or let patterns"
      | Variant _      -> failwith "Variant selection cannot be used in function parameter or let patterns"
      | Bind_using _   -> failwith "Bind-using cannot be used in function parameter or let patterns"
      | Bind name -> Let (name, value, body, pos)
      | Cons (head, tail) -> 
          (polylet head pos (list_head value pos)
             (polylet tail pos (list_tail value pos)
                body))
      | Record_extension (label, patt, rem_patt) ->
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
      | Empty_record -> Record_selection_empty (value, body, pos)

let variant_selection
    (source : untyped_expression) 
    (cases : (string * pattern * untyped_expression) list) 
    (pos : position) 
    (default : (string * untyped_expression) option) : untyped_expression =
    let rec one_more source = function
      | [] -> source
      | (case_label, case_patt, case_body) :: [] ->
	  let case_var = (unique_name ()) in
	  let case_var_expr = (Variable (case_var, pos)) in
          let var, body = match default with
              None -> let x = unique_name() in
                x, Variant_selection_empty(Variable(x, pos), pos)
            | Some(var, body) -> var, body
          in
	    Variant_selection (source, case_label, case_var, 
                               polylet case_patt pos case_var_expr case_body, 
                               var, body, pos)
      | (case_label, Bind case_var, case_body) :: selects ->
	  let new_var = (unique_name ()) in
	  let new_var_expr = (Variable (new_var, pos)) in
	    Variant_selection (source, case_label, case_var, case_body, new_var, 
                               (one_more new_var_expr selects), pos)
      | (case_label, case_patt, case_body) :: selects ->
	  let case_var = (unique_name ()) in
	  let new_var = (unique_name ()) in
	  let case_var_expr = (Variable (case_var, pos)) in
	  let new_var_expr = (Variable (new_var, pos)) in
	    Variant_selection (source, case_label, case_var,
                               polylet case_patt pos case_var_expr case_body,
                               new_var, one_more new_var_expr selects,
                               pos)
    in
      one_more source cases

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
      | Bind s, Abstr _, Rec (bindings, e, p), _ ->  
          Rec ((s, value)  :: bindings, e, p) 
      | Bind s, Abstr _, _, true ->  
          Rec ([s, value], expr, pos) 
      | _ ->  
          polylet patt pos value expr in 
    fold_right folder bindings expression 


let func (pos : position) (body : untyped_expression) : pattern -> untyped_expression = function
      | Bind name -> Abstr (name, body, pos)
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
  | DBUpdate (table, db, rows) -> curried_apply (Variable("updaterows", pos)) pos [String (table, pos); desugar db; desugar rows]
  | DBDelete (table, db, rows) -> curried_apply (Variable("deleterows", pos)) pos [String (table, pos); desugar db; desugar rows]
  | DBInsert (table, db, rows) -> curried_apply (Variable("insertrow",  pos)) pos [String (table, pos); desugar db; desugar rows]
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
                      (Bind n, desugar (FunLit (None, patts, body), pos), 
                       lookup_pos pos, true)
                  | expr, pos -> 
                      Bind "__", desugar (expr, pos), lookup_pos pos, false) es in
      polylets es (desugar exp)
  | Foreign (language, name, datatype) -> 
      Alien (language, name, desugar_assumption (generalize datatype), pos)
  | SortBy_Conc(patt, expr, sort_expr) ->
      (match patternize patt with
        | Bind var -> 
            SortBy(desugar expr, (Abstr(var, desugar sort_expr, pos)), pos)
        | pattern -> failwith("orderby clause on non-simple pattern-matching for is not yet implemented."))
  | Iteration (pattern, from, body, None, None) ->
      (match patternize pattern with
         | Bind var -> For (desugar body, var, desugar from, pos)
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
  | Switch (exp, (((Pattern(InfixAppl (`Cons, _, _), _), _)::_) as patterns), dflt)       (* TBD: Remove the _ at the end of the Switch node *)
  | Switch (exp, (((Pattern(ListLit [], _), _)::_) as patterns), dflt)
    -> open_list_match (desugar exp) patterns dflt lookup_pos pos
  | Switch (exp, patterns, _) ->
      let patterns, default_case = match (unsnoc patterns) with
          patterns, (Pattern(Var x, _), body) -> patterns, Some(x, desugar body)
        | _ -> patterns, None
      in
      (variant_selection 
         (desugar exp)
         (List.map
            (fun (Pattern patt, body) ->
               let name, content = match patt with
                   ConstructorLit(name, None), pos ->
                     name, (RecordLit ([], None), pos)
                 | ConstructorLit(name, Some content), _ ->
                     name, content
                 | patt -> 
                     (ignore (patternize' (desugar patt));
                     failwith("Internal error: Unknown expression used as pattern; patternize did not detect the error."))
(*                      raise(RichSyntaxError{ *)
(*                              Syntax.string_of_expression(desugar(patt, pos)) ^ " cannot appear in a pattern.", pos)) *)
               in
                 (name, patternize (Pattern content), desugar body))
            patterns) 
	 pos
         default_case)
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
  | InfixAppl(`Cons, head, tail), _ ->
      and_expr (is_not_null value pos)
        (and_expr (list_patt_to_bool value pos head)
           (list_patt_to_bool value pos tail) pos) pos
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

