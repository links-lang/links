open Num
open List
open Utility
open Show
open Pickle

type lexpos = Lexing.position
module LexposType = struct type a = lexpos let tname = "Syntax.lexpos" end
module Show_lexpos = Show_unprintable (LexposType)
module Pickle_lexpos = Pickle_unpicklable (LexposType)

type position = lexpos * (* source line : *) string * (* expression source: *) string
    deriving (Show, Pickle)

let dummy_position = Lexing.dummy_pos, "<dummy>", "<dummy>"
    
exception ASTSyntaxError of position * string

type location = [`Client | `Server | `Native | `Unknown]
    deriving (Show, Pickle)

type label = int
    deriving (Show, Pickle)

type 'data expression' =
  | Define of (string * 'data expression' * location * 'data)
  | Boolean of (bool * 'data)
  | Integer of (num * 'data)
  | Char of (char * 'data)
  | String of (string * 'data)
  | Float of (float * 'data)
  | Variable of (string * 'data)

  | Apply of ('data expression' * 'data expression' * 'data)
  | Condition of ('data expression' * 'data expression' * 'data expression' * 
                    'data)
  | Comparison of ('data expression' * string * 'data expression' * 'data)
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
  | TableQuery of ((* the table: *) 'data expression'
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
      deriving (Show, Pickle, Functor, Rewriter)

let is_define = 
  function
    | Define _
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

type expression = (position * Types.datatype * label option) expression'
and untyped_expression = position expression'
and stripped_expression = unit expression'
  deriving (Show, Pickle)

let rec show t : 'a expression' -> string = function 
  | HasType(expr, datatype, data) -> show t expr ^ " : " ^ Types.string_of_datatype datatype ^ t data
  | Define (variable, value, location, data) -> variable ^ "=" ^ show t value
      ^ "[" ^ Show_location.show location ^ "]; " ^ t data
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
      show t left_value ^ " " ^ oper ^ " " ^ show t right_value ^ t data
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
  | TableQuery (th, query, data) ->
      "("^ show t th ^"["^Sql.string_of_query query^"])" ^ t data
  | SortBy (expr, byExpr, data) ->
      "sort (" ^ show t expr ^ ") by (" ^ show t byExpr ^ ")" ^ t data
  | Wrong data -> "wrong" ^ t data
  | Placeholder (s, data) -> "PLACEHOLDER : " ^ Show_label.show s ^ t data
  | Alien (s1, s2, k, data) -> Printf.sprintf "alien %s %s : %s;" s1 s2 (Types.string_of_assumption k) ^ t data

let string_of_expression s = show (fun _ -> "") s

let strip_data : 'a expression' -> stripped_expression =
  fun e -> Functor_expression'.map (fun _ -> ()) e

let erase : expression -> untyped_expression = 
  Functor_expression'.map (fun (pos, _, _) -> pos)

let labelize =
  let label_seq = ref 0 in
let new_label () = incr label_seq; !label_seq in
let new_label_str () = (*"T" ^ string_of_int*) (new_label ()) in
  fun (expr:expression) ->
    Functor_expression'.map
      (fun (a,b,_) ->
         let label = new_label_str () in
           (a, b, Some label)) expr

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
               | Variable _ -> []

               | Variant_selection_empty (e, _)
               | Define (_, e, _, _)
               | Abstr (_, e, _)
               | Database (e, _)
               | Variant_injection (_, e, _)
               | List_of (e, _)
               | Escape (_, e, _)
               | HasType (e, _, _)
               | TableQuery (e, _, _) -> [visitor visit_children e]

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

let expression_data : ('a expression' -> 'a) = function 
        | Define (_, _, _, data) -> data
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

let node_datatype : (expression -> Types.datatype) = (fun (_, datatype, _) -> datatype) -<- expression_data
and untyped_pos  : (untyped_expression -> position) = expression_data 

let stringlit_value = function
  | String (name, _) -> name
  | _ -> assert false

let no_expr_data = (dummy_position, `Not_typed, None)

module RewriteSyntax = Rewrite_expression'(struct type a = (position * Types.datatype * label option) end)
module RewriteUntypedExpression = Rewrite_expression'(struct type a = position end)
