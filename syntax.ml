(*pp deriving *)
open Num
open List
open Utility
open Show
open Pickle

type lexpos = Lexing.position
module Typeable_lexpos = Typeable.Primitive_typeable(struct type t = lexpos end)

module LexposType = struct type a = lexpos let tname = "Syntax.lexpos" end
module Show_lexpos = Show_unprintable (LexposType)
(*module Pickle_lexpos = Pickle_unpicklable (LexposType)*)

module Pickle_lexpos : Pickle with type a = lexpos = Pickle.Pickle_defaults(
  struct
    type a = lexpos
    let pickle buffer e = ()
    and unpickle stream = Lexing.dummy_pos 
  end)

type position = lexpos *  (* source line: *) string 
                  * (* expression source: *) string
    deriving (Typeable, Show, Pickle)

let dummy_position = Lexing.dummy_pos, "<dummy>", "<dummy>"
    
exception ASTSyntaxError of position * string

type location = [`Client | `Server | `Native | `Unknown]
    deriving (Typeable, Show, Pickle)

type label = int
    deriving (Typeable, Show, Pickle)

type comparison = [`Less | `LessEq | `Equal | `NotEq]
    deriving (Typeable, Show, Pickle)

let string_of_comparison = function
  | `Less   -> "<"
  | `LessEq -> "<="
  | `Equal  -> "=="
  | `NotEq  -> "<>"

type 'data expression' =
  | Define of (string * 'data expression' * location * 'data)
  | TypeDecl of (string * int list * Types.datatype * 'data)
  | Boolean of (bool * 'data)
  | Integer of (num * 'data)
  | Char of (char * 'data)
  | String of (string * 'data)
  | Float of (float * 'data)
  | Variable of (string * 'data)

  | Apply of ('data expression' * 'data expression' * 'data)
  | Condition of ('data expression' * 'data expression' * 'data expression' * 
                    'data)
  | Comparison of ('data expression' * comparison * 'data expression' * 'data)
  | Abstr of (string * 'data expression' * 'data)
  | Let of (string * 'data expression' * 'data expression' * 'data)
  | Rec of ((string * 'data expression' * Types.datatype option) list * 'data expression' * 'data)
  | Xml_node of (string * ((string * 'data expression') list) * 
                   ('data expression' list) * 'data)
  | Record_empty of 'data
  | Record_extension of (string * 'data expression' * 'data expression' * 'data)
  | Record_selection of (string * string * string * 'data expression' * 
                           'data expression' * 'data)
  | Record_selection_empty of ('data expression' * 'data expression' * 'data)
  | Variant_injection of (string * 'data expression' * 'data)
  | Variant_selection of ('data expression' * 
                            string * string * 'data expression' * 
                            string * 'data expression' * 'data)
  | Variant_selection_empty of ('data expression' * 'data)
  | Nil of ('data)
  | List_of of ('data expression' * 'data)
  | Concat of ('data expression' * 'data expression' * 'data)
  | For of ('data expression' * string * 'data expression' * 'data)
  | Database of ('data expression' * 'data)
  | TableQuery of ((* the tables: *) (string * 'data expression') list
      * (* the query: *) Query.query 
      * 'data)
  | TableHandle of ((* the database: *) 'data expression' 
      * (* the table name: *) 'data expression'
      * (* the type of a table row: *) Types.row
      * 'data)
     
  | SortBy of ('data expression' * 'data expression' * 'data)
  | Escape of (string * 'data expression' * 'data)
  | Wrong of 'data
  | HasType of ('data expression' * Types.datatype * 'data)
  | Alien of (string * string * Types.assumption * 'data)
  | Placeholder of (label * 'data)
      deriving (Typeable, Show, Pickle, Functor, Rewriter) (* Should this be picklable? *)

let is_define = 
  function
    | Define _
    | TypeDecl _
    | Alien _ -> true
    | _ -> false

(* Whether a syntax node is a value for the purposes of generalization.
   This means, approximately "it doesn't contain any applications" *)
let rec is_value : 'a expression' -> bool = function
  | Boolean _
  | Integer _
  | Char _
  | String _
  | Float _
  | Variable _
  | Xml_node _ (* ? *)
  | Record_empty _
  | Nil _
  | Abstr _ -> true
  | HasType (e, _, _)
  | Variant_injection (_, e, _)
  | Variant_selection_empty (e, _)
  | Database (e, _)
  | List_of (e, _) -> is_value e
  | TableHandle (a, b, _, _)
  | Comparison (a,_,b,_)
  | Concat (a, b, _)
  | For (a, _, b, _)
  | Record_extension (_, a, b, _)
  | Record_selection_empty (a, b, _)
  | Record_selection (_, _, _, a, b, _)
  | Let (_, a, b,_)  -> is_value a && is_value b
  | Variant_selection (a, _, _, b, _, c, _)
  | Condition (a,b,c,_) -> is_value a && is_value b && is_value c
  | Rec (bs, e, _) -> List.for_all (is_value -<- (fun (_,x,_) -> x)) bs && is_value e
  | _ -> false

type typed_data = [`T of (position * Types.datatype * label option)] deriving (Typeable, Show, Pickle)
type untyped_data = [`U of position] deriving (Typeable, Show, Pickle)
type data = [untyped_data | typed_data] deriving (Typeable, Show, Pickle)
type expression = typed_data  expression'
and untyped_expression = untyped_data expression'
and stripped_expression = unit expression'
  deriving (Typeable, Show, Pickle)

let data_position = function
  | `T (pos, _, _)
  | `U pos -> pos

let rec show t : 'a expression' -> string = function 
  | HasType(expr, datatype, data) -> show t expr ^ " : " ^ Types.string_of_datatype datatype ^ t data
  | Define (variable, value, location, data) -> variable ^ "=" ^ show t value
      ^ "[" ^ Show_location.show location ^ "]; " ^ t data
  | TypeDecl (typename, quantifiers, datatype, data) ->
      "typename "^typename^"(TODO:update pretty-printer to display quantifiers) = "^ Types.string_of_datatype datatype ^ t data
  | Boolean (value, data) -> string_of_bool value ^ t data
  | Integer (value, data) -> string_of_num value ^ t data
  | Char (c, data) -> "'"^ Char.escaped c ^"'" ^ t data
  | String (s, data) -> "\"" ^ s ^ "\"" ^ t data
  | Float (value, data)   -> string_of_float value ^ t data
  | Variable (name, data) -> name ^ t data
  | Apply (f, p, data)    -> show t f ^ "(" ^ show t p ^ ")" ^ t data
  | Condition (cond, if_true, if_false, data) ->
      "if (" ^ show t cond ^ ") " ^ show t if_true ^ " else " ^ show t if_false ^ t data
  | Comparison (left_value, oper, right_value, data) ->
      show t left_value ^ " " ^ string_of_comparison oper ^ " " ^ show t right_value ^ t data
  | Abstr (variable, body, data) ->
      "fun (" ^ variable ^ ") { " ^ show t body ^ " }" ^ t data
  | Let (variable, value, body, data) ->
      "{ var " ^ variable ^ "=" ^ show t value ^ "; " ^ show t body ^ "}" ^ t data
  | Rec (variables, body, data) ->
      "{" ^ (String.concat " ; " (map (function (label, expr, _) -> " " ^ label ^ "=" ^ show t expr) variables))
      ^ "; " ^ show t body ^ "}" ^ t data
  | Escape (var, body, data) -> "escape " ^ var ^ " in " ^ show t body ^ t data
  | Xml_node (tag, attrs, elems, data) ->  
      let attrs = 
        let attrs = String.concat " " (map (fun (k, v) -> k ^ "=\"" ^ show t v ^ "\"") attrs) in
          match attrs with 
            | "" -> ""
            | _ -> " " ^ attrs in
        (match elems with 
           | []    -> "<" ^ tag ^ attrs ^ "/>" ^ t data
           | elems -> "<" ^ tag ^ attrs ^ ">" ^ String.concat "" (map (show t) elems) ^ "</" ^ tag ^ ">" ^ t data)
  | Record_empty (data) ->  "()" ^ t data
  | Record_extension (label, value, record, data) ->
      "(" ^ label ^ "=" ^ show t value ^ "|" ^ show t record ^ ")" ^ t data
  | Record_selection (label, label_variable, variable, value, body, data) ->
      "{(" ^ label ^ "=" ^ label_variable ^ "|" ^ variable ^ ") = " 
      ^ show t value ^ "; " ^ show t body ^ "}" ^ t data
  | Record_selection_empty (value, body, data) ->
      "{() = " ^ show t value ^ "; " ^ show t body ^ "}" ^ t data
  | Variant_injection (label, value, data) ->
      label ^ "(" ^ show t value ^ ")" ^ t data
  | Variant_selection (value, case_label, case_variable, case_body, variable, body, data) ->
      "case " ^ show t value ^ " of " ^ case_label ^ "=" 
      ^ case_variable ^ " in " ^ (show t case_body) ^ " | " 
      ^ variable ^ " in " ^ show t body ^ t data
  | Variant_selection_empty (value, data) ->
      show t value ^ " is empty " ^ t data
  | Nil (data)              -> "[]" ^ t data
  | List_of (elem, data)       -> "[" ^ show t elem ^ "]" ^ t data
  | Concat (left, right, data) -> 
      "(" ^ show t left ^ t data ^ "++" ^ show t right ^ ")" 
  | For (expr, variable, value, data) ->
      "(for (" ^ variable ^ " <- " ^ show t value ^ ") " ^ show t expr ^ ")" ^ t data
  | Database (params, data) -> "database (" ^ show t params ^ ")" ^ t data
  | TableHandle (db, name, row, data) ->
      "("^ show t name ^" from "^ show t db ^"["^Types.string_of_row row^"])" ^ t data
  | TableQuery (ths, query, data) ->
      "("^ mapstrcat "," (fun (alias, th) -> show t th ^ "("^alias^")") ths ^
        "["^Sql.string_of_query query^"])" ^ t data
  | SortBy (expr, byExpr, data) ->
      "sort (" ^ show t expr ^ ") by (" ^ show t byExpr ^ ")" ^ t data
  | Wrong data -> "wrong" ^ t data
  | Placeholder (s, data) -> "PLACEHOLDER : " ^ Show_label.show s ^ t data
  | Alien (s1, s2, k, data) -> Printf.sprintf "alien %s %s : %s;" s1 s2 (Types.string_of_assumption k) ^ t data

let string_of_expression s = show (fun _ -> "") s

let strip_data : 'a expression' -> stripped_expression =
  fun e -> Functor_expression'.map (fun _ -> ()) e

let erase : expression -> untyped_expression = 
  Functor_expression'.map (fun (`T (pos, _, _)) -> `U pos)

(*let has_label = function
    (_,_,Some_) -> true
  | (_,_,None) -> false

let is_labelized = reduce_expression (fun _ -> expression_data) (fun(_,datas) -> ) 
*)
let labelize =
  let label_seq = ref 0 in
let new_label () = incr label_seq; !label_seq in
let new_label_str () = (*"T" ^ string_of_int*) (new_label ()) in
  fun (expr:expression) ->
    Functor_expression'.map
      (fun (`T(a,b,_)) ->
         let label = new_label_str () in
           (`T(a, b, Some label))) expr

let reduce_expression (visitor : ('a expression' -> 'b) -> 'a expression' -> 'b)
    (combine : (('a expression' * 'b list) -> 'c)) : 'a expression' -> 'c =
  (* The "default" action: do nothing, just process subnodes *)
  let rec visit_children expr = 
    combine (expr, match expr with
               | Boolean _
               | Integer _
               | Char _
               | String _
               | Float _ 
               | Record_empty _
               | Nil _
               | Alien _
               | Placeholder _ 
               | Wrong _
               | TypeDecl _
               | Variable _ -> []

               | Variant_selection_empty (e, _)
               | Define (_, e, _, _)
               | Abstr (_, e, _)
               | Database (e, _)
               | Variant_injection (_, e, _)
               | List_of (e, _)
               | Escape (_, e, _)
               | HasType (e, _, _) -> [visitor visit_children e]

               | TableQuery (es, _, _) -> (map (fun (_,e) -> visitor visit_children e) es)

               | TableHandle (e1, e2, _, _)
               | Apply (e1, e2, _)
               | Comparison (e1, _, e2, _)
               | Let (_, e1, e2, _)
               | Record_extension (_, e1, e2, _)
               | Record_selection_empty (e1, e2, _)
               | Concat (e1, e2, _)
               | Record_selection (_, _, _, e1, e2, _)
               | For (e1, _, e2, _)
               | SortBy (e1, e2, _) ->
                   [visitor visit_children e1; visitor visit_children e2]
                   
               | Condition (e1, e2, e3, _)
               | Variant_selection (e1, _, _, e2, _, e3, _) -> [visitor visit_children e1; visitor visit_children e2; visitor visit_children e3]

               | Rec (b, e, _) -> visitor visit_children e :: map (fun (_, e, _) -> visitor visit_children e) b
               | Xml_node (_, es1, es2, _)          -> map (fun (_,v) -> visitor visit_children v) es1 @ map (visitor visit_children) es2)
  in
    visitor visit_children

let freevars (expression : 'a expression') : string list = 
  let remove x = List.filter ((<>)x)
  and remove_all xs = List.filter (not -<- flip List.mem xs) 
  and combine = unduplicate (=) -<-List.concat in
  let rec aux default = function
    | Variable (name, _) -> [name]
    | Let (var, value, body, _) -> aux default value @ (remove var (aux default body))
    | Escape (var, body, _)
    | Abstr (var, body, _) -> remove var (aux default body)
    | Record_selection (_, labvar, var, value, body, _) ->
        aux default value @ (remove_all [var;labvar] (aux default body))
    | Variant_selection (value, _, cvar, cbody, var, body, _) ->
        aux default value @ (remove cvar (aux default cbody)) @ (remove var (aux default body))
    | Rec (bindings, body, _) ->
        let vars, vals = List.split (map (fun (n,v,_) -> (n,v)) bindings) in
          remove_all vars (concat_map (aux default) (body::vals))
    | TableQuery (_, query, _) -> Query.freevars query
    | other -> default other
  in 
    reduce_expression aux (combine -<- snd) expression
          
let rec list_expr data = function
    [] -> Nil(data)
  | expr::etc -> Concat(List_of(expr, data), list_expr data etc, data)

let expression_data : ('a expression' -> 'a) = function 
        | Define (_, _, _, data) -> data
        | TypeDecl (_, _, _, data) -> data
        | HasType (_, _, data) -> data
        | Boolean (_, data) -> data
        | Integer (_, data) -> data
        | Float (_, data) -> data
        | Char (_, data) -> data
        | String (_, data) -> data
        | Variable (_, data) -> data
        | Apply (_, _, data) -> data
        | Condition (_, _, _, data) -> data
        | Comparison (_, _, _, data) -> data
        | Abstr (_, _, data) -> data
        | Let (_, _, _, data) -> data
        | Rec (_, _, data) -> data
        | Xml_node (_, _, _, data) -> data
        | Record_empty (data) -> data
        | Record_extension (_, _, _, data) -> data
        | Record_selection (_, _, _, _, _, data) -> data
        | Record_selection_empty (_, _, data) -> data
        | Variant_injection (_, _, data) -> data
        | Variant_selection (_, _, _, _, _, _, data) -> data
        | Variant_selection_empty (_, data) -> data
        | Nil (data) -> data
        | List_of (_, data) -> data
        | Concat (_, _, data) -> data
        | For (_, _, _, data) -> data
        | Database (_, data) -> data
        | TableQuery (_, _, data) -> data
        | TableHandle (_, _, _, data) -> data
        | SortBy (_, _, data) -> data
        | Escape (_, _, data) -> data
        | Wrong data -> data
        | Alien (_,_,_,data) -> data
        | Placeholder (_,data) -> data

(*
(** A hypothetical [set_data] would set the data members of all the
    subnodes to a given value; could be useful when you have a big tree to
    insert somewhere during desugaring & you want it be at certain line
    numbers, etc. *)
let rec set_data : ('b -> 'a expression -> 'b expression) =
  fun d -> function
  | Define ... 
*)

let node_datatype : (expression -> Types.datatype) = (fun (`T(_, datatype, _)) -> datatype) -<- expression_data

let position e = data_position (expression_data e)

let rec stringlit_value = function
  | HasType (e, _, _) -> stringlit_value e
  | String (name, _) -> name
  | _ -> assert false

let no_expr_data = `T(dummy_position, `Not_typed, None)

module RewriteSyntax = Rewrite_expression'(struct type a = typed_data end)
module RewriteUntypedExpression = Rewrite_expression'(struct type a = untyped_data end)
  
let rec map_free_occ u f expr =
  let recurse = map_free_occ u f in
  let rec rewrite = function
    | Variable(x, _) as node when x = u -> Some (f node)
    | Abstr(x, body, d) when x <> u -> Some (Abstr(x, recurse body, d))
    | Abstr _ -> None
    | Let(letvar, letval, body, d) ->
        Some(Let(letvar, recurse letval, 
                 (if u <> letvar then recurse body else body), d))
    | Rec(defs, body, d) when (not (mem_assoc3 u defs)) ->
        Some(Rec(map (fun (n, defn, t) -> (n, recurse defn, t)) defs, 
                 recurse body, d))
    | Record_selection(label, label_var, etc_var, src, body, d) ->
        Some(Record_selection(label, label_var, etc_var, recurse src, 
                              (if (u <> label_var && u <> etc_var) then
                                 recurse body else body), d))
    | Variant_selection(value, case_label, case_variable, case_body, 
                        etc_var, etc_body, d) ->
        Some(Variant_selection(recurse value, case_label, case_variable, 
                          (if u <> case_variable then
                             recurse case_body
                           else case_body),
                          etc_var,
                          (if u <> etc_var then 
                             recurse etc_body
                           else etc_body), 
                          d))
    | For(body, loop_var, src, d) ->
        Some(For((if (u <> loop_var) then recurse body else body),
                 loop_var, recurse src, d))
    | Escape(esc_var, body, d) when u <> esc_var ->
        Some(Escape(esc_var, recurse body, d))
    | expr -> RewriteUntypedExpression.process_children rewrite expr
  in fromOption expr (rewrite expr)

let subst_free u r expr =
  map_free_occ u (fun _ -> r) expr

(**
   [rename_free e u v]
   Substitutes the variable [v] for free occurrences of [u] in the expression [e].
   Note: this is *not* presently capture-avoiding (but perhaps it should be).
*)

let rename_free u v e =
  map_free_occ u (fun (Variable(x, d)) -> Variable(v, d)) e


let subst_fast name replacement expr =
  let replacer name replacement : RewriteSyntax.rewriter = function
    | Variable (n, _) when n = name -> Some replacement
    | _ -> None
  in
    fromOption expr (RewriteSyntax.bottomup (replacer name replacement) expr)

let rename_fast name replacement expr = 
  let replacer name replacement : RewriteSyntax.rewriter = function
    | Variable (n, d) when n = name -> Some (Variable(replacement, d))
    | TableQuery(th, q, data) -> 
        let q = Query.query_replace_var name (Query.Variable replacement) q in
          Some(TableQuery(th, q, data)) 
    | _ -> None
  in
    fromOption expr (RewriteSyntax.bottomup (replacer name replacement) expr)




(** [skeleton] has a case for each of the [Syntax] constructors, and
    gives an approrpiate name to each component. Use this to get 
    started on a function that takes Syntax trees by case. *)
let skeleton = function
    (* Zero sub-expressions *)
  | Nil d -> Nil d
  | Wrong d -> Wrong d
  | Record_empty d -> Record_empty d
  | Boolean(value, d) -> Boolean(value, d)
  | Integer(value, d) -> Integer(value, d)
  | Char(value, d) -> Char(value, d)
  | String(value, d) -> String(value, d)
  | Float(value, d) -> Float(value, d)
  | Variable(x, d) -> Variable(x, d)
  | Apply(f, a, d) -> Apply(f, a, d)
  | TypeDecl(typename, quantifiers, datatype, d) ->
      TypeDecl(typename, quantifiers, datatype, d)
  | Placeholder(label, d) -> Placeholder(label, d)
  | Alien(language, name, assumption, d) -> Alien(language, name, assumption, d)
  | TypeDecl(typename, quantifiers, datatype, d) -> TypeDecl(typename, quantifiers, datatype, d)

  (* One sub-expression *)
  | Define(name, expr, loc_annotation, d) ->
      Define(name, expr, loc_annotation, d)
  | Abstr(var, body, d) -> Abstr(var, body, d)
  | Record_extension(label, labelval, record, d) ->
      Record_extension(label, labelval, record, d)
  | Variant_injection(label, value_expr, d) -> 
      Variant_injection(label, value_expr, d)
  | Variant_selection_empty(src_expr, d) -> 
      Variant_selection_empty(src_expr, d)
  | List_of(single_member, d) -> List_of(single_member, d)
  | Database(db_args_expr, d) -> Database(db_args_expr, d)
  | HasType(expr, datatype, d) -> HasType(expr, datatype, d)
      
  (* Two sub-expressions *)
  | Comparison(lhs, op, rhs, d) -> Comparison(lhs, op, rhs, d)
  | Let(letvar, letsrc, letbody, d) -> Let(letvar, letsrc, letbody, d)
  | Record_selection(label, labelvar, etcvar, src, body, d) ->
      Record_selection(label, labelvar, etcvar, src, body, d)
  | Record_selection_empty(record, body, d) -> 
      Record_selection_empty(record, body, d)
  | Concat(lhs, rhs, d) -> Concat(lhs, rhs, d)
  | For(body, loop_var, src, d) -> For(body, loop_var, src, d)
  | SortBy(list_target, sort_func, d) -> SortBy(list_target, sort_func, d)
  | TableHandle(db_expr, tablename_expr, row_type, d) -> 
      TableHandle(db_expr, tablename_expr, row_type, d)
  | Escape(esc_var, body, d) -> Escape(esc_var, body, d)
      
  (* Three sub-expressions *)
  | Condition(condn, ifcase, elsecase, d) -> 
      Condition(condn, ifcase, elsecase, d)
  | Variant_selection(src_expr, case_label, case_variable, case_body, 
                      etc_var, etc_body, d) -> 
      Variant_selection(src_expr, case_label, case_variable, case_body, 
                      etc_var, etc_body, d)

  (* n-ary expressions *)
  | Rec(defs, body, d) -> Rec(defs, body, d)
  | Xml_node(tagname, attrs, contents, d) -> 
      Xml_node(tagname, attrs, contents, d)
  | TableQuery(thandle_alist, query, d) -> TableQuery(thandle_alist, query, d)
      (* note: besides the alist, [query] can also contain
         expressions, in the [query.ml] sublanguage *)
