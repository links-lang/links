open Num
open List

open Utility
open Debug
open Types
open Sql

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

let string_of_location = Show_location.show

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
  | Rec of ((string * 'data expression' * datatype option) list * 'data expression' * 'data)
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
      * (* the type of a table row: *) row
      * 'data)
     
  | SortBy of ('data expression' * 'data expression' * 'data)
  | Escape of (string * 'data expression' * 'data)
  | Wrong of 'data
  | HasType of ('data expression' * datatype * 'data)
  | Alien of (string * string * assumption * 'data)
  | Placeholder of (label * 'data)
      deriving (Show, Pickle, Functor, Rewriter)

let is_define = 
  function
    | Define _
    | Alien _ -> true
    | _ -> false

(* This doesn't seem right:
     e.g. Let _ is never a value
   What is the definition of a value?
 *)
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

type expression = (position * datatype * label option) expression'
    deriving (Show, Pickle)
type untyped_expression = position expression'
    deriving (Show, Pickle)
type stripped_expression = unit expression'
  deriving (Show, Pickle)

let rec show t : 'a expression' -> string = function 
  | HasType(expr, datatype, data) -> show t expr ^ " : " ^ string_of_datatype datatype ^ t data
  | Define (variable, value, location, data) -> variable ^ "=" ^ show t value
      ^ "[" ^ string_of_location location ^ "]; " ^ t data
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
      "("^ show t th ^"["^string_of_query query^"])" ^ t data
  | SortBy (expr, byExpr, data) ->
      "sort (" ^ show t expr ^ ") by (" ^ show t byExpr ^ ")" ^ t data
  | Wrong data -> "wrong" ^ t data
  | Placeholder (s, data) -> "PLACEHOLDER : " ^ Show_label.show s ^ t data
  | Alien (s1, s2, k, data) -> Printf.sprintf "alien %s %s : %s;" s1 s2 (string_of_assumption k) ^ t data

let string_of_typed_expression (s : expression) : string = 
  show (function (_, datatype, _) -> 
	  " : (" ^ (string_of_datatype datatype) ^ ")") s

let string_of_expression s = show (fun _ -> "") s

(*
Functions involved in a visit:

  1. The custom visitor.  This is provided by the user.
  2. The default visitor.  This just calls the custom visitor on each sub-node of the tree.


How do we make it more general?

  Add a further custom function that is called on each node and
  returns a result which is accumulated as in a fold.  The result of
  this function should be available to the visitor functions.

  Hopefully this "more general version" should correspond to a fold,
  as the current version corresponds to a map.  (Should it be an
  "upwards" or "downwards" fold?  I think it shouhld be foldr.)

*)

(** [visit_expressions' combiner unit visitor (expr, data)]
    Recursively visit an expression top-down, passing data in both
    directions (down into the leaves as we recurse and back up towards
    the roots as we return). This is something like a fancy "fold" for
    expression trees.

    [visitor] will be called, on each sub-expression [expr], as
    [visitor f (expr, data)] and should return a new [(expr', data')]
    pair. ([data'] can be a different type from [data].) To
    recursively visit the children, just call the given [f] on the
    whole node (it takes care of finding the children and descending
    into them).

    [unit] tell how to generate the output data for leaf nodes. It
    gets as argument the data passed down from the parent nodes and
    should return the data to attach to the leaf (NOTE: this is
    asymmetrical; why shouldn't [unit] get the data on the node
    itself?)

    As we return back up the tree, we have lots of data items from
    sub-expressions that we need to congeal together. [combiner] does
    this. We assume that the operation to combine data items is
    associative, so [combiner] only takes two arguments: [left_data]
    and [right_data]. There is an implicit left-to-right ordering for
    child nodes, so [combiner] need not be commutative (but this is a
    bit sketchy).

    (Currently, [combiner] doesn't get to see the nodes themselves,
    only the data items. Consider passing the present node as a
    parameter to [combiner].)
*)
let visit_expressions'
    (* This could be made better by combining `visitor' and `combiner'
       and providing a seed for the data. The problem is that there's
       no `unit' for expressions. Still, it seems a shame to have
       `combiner' be symmetric.*)
    (combiner : 'output -> 'output -> 'output) 
    (unit : 'input -> 'output)
    (visitor : (('a expression' * 'input) -> ('a expression' * 'output))
                  -> ('a expression' * 'input) -> ('a expression' * 'output))
    : ('a expression' * 'input) -> ('a expression' * 'output) =
  (* The "default" action: do nothing, just process subnodes *)
  let rec visit_children (expr, data) = match expr with
    | Define (s, e, l, d) -> let e, data = visitor visit_children (e, data) in
                                Define (s, e, l, d), data
    | Boolean (v, d) ->  Boolean (v, d), unit data
    | Integer (v, d) -> Integer (v, d), unit data
    | Char (v, d) -> Char (v, d), unit data
    | String (s, d) -> String (s, d), unit data
    | Float (v, d) -> Float (v, d), unit data
    | Variable (s, d) -> Variable (s, d), unit data
    | Apply (e1, e2, d) -> let e1, data1 = visitor visit_children (e1, data)
                           and e2, data2 = visitor visit_children (e2, data) in
        Apply (e1, e2, d), combiner data1 data2
    | Condition (e1, e2, e3, d) -> let e1, data1 = visitor visit_children (e1, data)
                                   and e2, data2 = visitor visit_children (e2, data)
                                   and e3, data3 = visitor visit_children (e3, data) in
        Condition (e1, e2, e3, d), combiner (combiner data1 data2) data3
    | Comparison (e1, s, e2, d) -> let e1, data1 = visitor visit_children (e1, data)
                                   and e2, data2 = visitor visit_children (e2, data) in
        Comparison (e1, s, e2, d), combiner data1 data2
    | Abstr (s, e, d) -> let e, data = visitor visit_children (e, data) in
        Abstr (s, e, d), data
    | Let (s, e1, e2, d) -> let e1, data1 = visitor visit_children (e1, data) 
                            and e2, data2 = visitor visit_children (e2, data) in
        Let (s, e1, e2, d), combiner data1 data2


    | Rec (b, body, d) -> 
        (* Would a fold across the boundvals be better? *)
        let boundvals = map (fun (name, value, t) -> 
                               let e, d = visitor visit_children (value, data) in
                                 (e, d, name, t)) b in
        let bindings_data = map (fun (_,d,_,_) -> d)  boundvals in 
        let data1 = fold_left combiner (hd bindings_data) (tl bindings_data) in
        let body, data2 = visitor visit_children (body, data) in 
          (Rec (map (fun (e, _, name, t) -> (name, e, t)) boundvals, body, d),
           combiner data1 data2)
    | Xml_node (tag, attrs, elems, d) -> 
        (* Not 100% sure this is right *)
        let attrvals = map (fun (name, value) ->
                              let e, d = visitor visit_children (value, data) in 
                                (e, d, name)) attrs in
        let data1 = (match attrvals with
                       | [] -> unit data
                       | _::_ -> 
                           let attrdata = map (fun (_,d,_)->d) attrvals in
                             fold_left combiner (hd attrdata) (tl attrdata)) in
        let bodyvals = map (fun value ->
                              let e, d = visitor visit_children (value, data) in
                                (e, d)) elems in
        let data2 = (match map snd bodyvals with
                       | [] -> unit data
                       | bodydata  -> fold_left combiner (hd bodydata) (tl bodydata)) in
          (Xml_node (tag, map (fun (a, _, b) -> (b, a)) attrvals, map fst bodyvals, d), 
           combiner data1 data2)

    | Record_empty d -> Record_empty d, unit data
    | Record_extension (s, e1, e2, d) -> let e1, data1 = visitor visit_children (e1, data) 
                                         and e2, data2 = visitor visit_children (e2, data) in
        Record_extension (s, e1, e2, d), combiner data1 data2
    | Record_selection (s1, s2, s3, e1, e2, d) -> let e1, data1 = visitor visit_children (e1, data)
                                                  and e2, data2 = visitor visit_children (e2, data) in
        Record_selection (s1, s2, s3,  e1,  e2, d), combiner data1 data2
    | Record_selection_empty (e1, e2, d) -> let e1, data1 = visitor visit_children (e1, data)
                                            and e2, data2 = visitor visit_children (e2, data) in
        Record_selection_empty ( e1,  e2, d), combiner data1 data2
    | Variant_injection (s, e, d) -> let e, data = visitor visit_children (e, data) in
        Variant_injection (s, e, d), data
    | Variant_selection (e1, s1, s2, e2, s3, e3, d) -> let e1, data1 = visitor visit_children (e1, data)
                                                       and e2, data2 = visitor visit_children (e2, data)
                                                       and e3, data3 = visitor visit_children (e3, data) in
        Variant_selection ( e1, s1, s2, e2, s3, e3, d), combiner (combiner data1 data2) data3
    | Variant_selection_empty (d) -> Variant_selection_empty (d), unit data
    | Nil (d) -> Nil (d), unit data

    | List_of (e, d) -> let e, data = visitor visit_children (e, data) in
        List_of (e, d), data
    | Concat (e1, e2, d) -> let e1, data1 = visitor visit_children (e1, data)
                                      and e2, data2 = visitor visit_children (e2, data) in
        Concat ( e1,  e2, d), combiner data1 data2
    | For (e1, s, e2, d) -> let e1, data1 = visitor visit_children (e1, data)
                            and e2, data2 = visitor visit_children (e2, data) in
        For ( e1, s, e2, d), combiner data1 data2

    | Database (e, d) -> let e, data = visitor visit_children (e, data) in
        Database (e, d), data
    | TableQuery (e, q, d) -> let e, data = visitor visit_children (e, data) in
        TableQuery (e, q, d), data
    | TableHandle (e1, e2, r, d) -> let e1, data1 = visitor visit_children (e1, data)
				    and e2, data2 = visitor visit_children (e2, data) in
        TableHandle (e1, e2, r, d), combiner data1 data2
    | Escape (n, e, d) -> let e, data = visitor visit_children (e, data) in
        Escape (n, e, d), data
    | HasType (e, k, d) -> let e, data = visitor visit_children (e, data) in
	HasType (e, k, d), data
    | SortBy (e1, e2, d) -> let e1, data1 = visitor visit_children (e1, data)
                            and e2, data2 = visitor visit_children (e2, data) in
        SortBy (e1, e2, d), combiner data1 data2
    | Wrong (d) -> Wrong (d), unit data
    | Placeholder (str, d) -> Placeholder (str, d), unit data
    | Alien _ as v -> v, unit data
  in visitor visit_children
       
(* A simplified version which doesn't pass data around *)
let simple_visit visitor expr = 
  let visitor default (e, _) = 
    (visitor (fun e -> fst (default (e, ()))) e, ()) in
  fst (visit_expressions'
         (* combiner: *) (fun () () -> ()) (* unit: *) (fun () -> ()) 
         visitor (expr, ()))

(* Could be made more efficient by not constructing the expression in
   parallel (i.e. discarding it, similar to `simple_visit'  *)
let freevars : 'a expression' -> string list = 
  (* FIXME : doesn't consider variables in the query.
             doesn't consider l:name-bound vars. *)
  fun expression ->
    let rec visit default (expr, vars) = 
      let childvars = snd -<- (visit default) in
        match expr with
          | Variable (name, _) when mem name vars -> expr, []
          | Variable (name, _) -> expr, [name]
          | Let (var, value, body, _) ->
              expr, childvars (value, vars) @ childvars (body, var::vars)
          | Abstr (var, body, _) -> visit default (body, var :: vars)
          | Record_selection (_, labvar, var, value, body, _) ->
              expr, childvars (value, vars) @ childvars (body, labvar :: var :: vars)
          | Variant_selection (value, _, cvar, cbody, var, body, _) ->
              expr, childvars (value, vars) @ childvars (cbody, cvar::vars) @ childvars (body, var::vars)
          | Rec (bindings, body, _) ->
              let vars = map (fun (v,_,_) -> v) bindings @ vars in  
                expr, List.concat (map (fun value -> (childvars (value, vars))) (map (fun (_,x,_) -> x) bindings)) @ childvars (body, vars)
	  | Escape (var, body, _) -> expr, childvars (body, var::vars)
          | TableQuery (_, query, _) -> expr, Query.freevars query
          | other -> default (other, vars)
    in Utility.unduplicate (=) (snd (visit_expressions' (@) (fun _ -> []) visit (expression, [])))

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

let node_datatype : (expression -> datatype) = (fun (_, datatype, _) -> datatype) -<- expression_data
and untyped_pos  : (untyped_expression -> position) = expression_data 

let stringlit_value = function
  | String (name, _) -> name
  | _ -> assert false

let no_expr_data = (dummy_position, `Not_typed, None)

module RewriteSyntax = Rewrite_expression'(struct type a = (position * datatype * label option) end)
module RewriteUntypedExpression = Rewrite_expression'(struct type a = position end)
