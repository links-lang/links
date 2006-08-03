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
      * (* the table name: *) string 
      * (* the type of a table row: *) row
      * 'data)
     
  | SortBy of ('data expression' * 'data expression' * 'data)
  | Escape of (string * 'data expression' * 'data)
  | Wrong of 'data
  | HasType of ('data expression' * datatype * 'data)
  | Alien of (string * string * assumption * 'data)
  | Placeholder of (string * 'data)
      deriving (Show, Pickle)

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
  | TableHandle (e, _, _, _)
  | List_of (e, _) -> is_value e
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

let gensym =
  let counter = ref 0 in
    function str ->
      begin
        incr counter;
        str ^ "_g" ^ string_of_int !counter
      end

type label = string 
    deriving (Show, Pickle)

type expression = (position * datatype * label option) expression'
    deriving (Show, Pickle)
type untyped_expression = position expression'
    deriving (Show, Pickle)
type stripped_expression = unit expression'
  deriving (Show, Pickle)


let string_of_location : location -> string = function
  | `Client -> "client" | `Server -> "server" | `Native -> "native" | `Unknown -> "unknown"

let rec unparse_sequence empty unit append = function 
  | Nil _ -> empty
  | List_of (elem, _) -> unit elem
  | Concat (l, r, _) -> (append 
                                     (unparse_sequence empty unit append l) 
                                     (unparse_sequence empty unit append r))
  | other -> failwith ("Unexpected argument to unparse_sequence : " ^ show (fun _ -> "") other)
and unparse_list x = unparse_sequence [] (fun x -> [x]) (@) x
and show t : 'a expression' -> string = function 
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
  | TableHandle (db, s, row, data) ->
      "("^ s ^" from "^ show t db ^"["^Types.string_of_row row^"])" ^ t data
  | TableQuery (th, query, data) ->
      "("^ show t th ^"["^string_of_query query^"])" ^ t data
  | SortBy (expr, byExpr, data) ->
      "sort (" ^ show t expr ^ ") by (" ^ show t byExpr ^ ")"
  | Wrong data -> "wrong" ^ t data
  | Placeholder (s, data) -> "PLACEHOLDER : " ^ s ^ t data
  | Alien (s1, s2, k, data) -> Printf.sprintf "alien %s %s : %s;" s1 s2 (string_of_assumption k) ^ t data

let string_of_typed_expression (s : expression) : string = 
  show (function (_, datatype, _) -> 
	  " : (" ^ (string_of_datatype datatype) ^ ")") s

let with_label = (fun (_, _, lbl) ->
     " [" ^ fromOption "BOGUS" lbl ^ "] ")

let string_of_expression s = show (fun _ -> "") s

let string_of_labelled_expression (s : expression) = show with_label s

let string_of_order order = match order with
  | `Asc name -> ""^ name ^":asc"
  | `Desc name -> ""^ name ^":asc"

let string_of_orders orders = match orders with 
  | [] -> " " 
  | _ -> "order [" ^ String.concat ", " (map string_of_order orders) ^ "]"

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

(* combiner should have type 
      expression -> data -> data
   (or perhaps 
      data -> expression -> data)

   Currently it doesn't get to see the nodes, which is wrong.  (Is
   this right?  Or is it sufficient that visitor sees the nodes?)
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
    | TableHandle (e, n, r, d) -> let e, data = visitor visit_children (e, data) in
        TableHandle (e, n, r, d), data
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
let simple_visit visitor expr = let visitor default (e, _) = (visitor (fun e -> fst (default (e, ()))) e, ()) in
  fst (visit_expressions' (fun () () -> ()) (fun () -> ()) visitor (expr, ()))

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

let rec redecorate (f : 'a -> 'b) : 'a expression' -> 'b expression' = function
  | Define (a, b, loc, data) -> Define (a, redecorate f b, loc, f data)
  | Boolean (a, data) -> Boolean (a, f data)
  | Integer (a, data) -> Integer (a, f data)
  | Float (a, data) -> Float (a, f data)
  | Char (a, data) -> Char (a, f data)
  | String (a, data) -> String (a, f data)
  | Variable (a, data) -> Variable (a, f data)
  | Apply (a, b, data) -> Apply (redecorate f a, redecorate f b, f data)
  | Condition (a, b, c, data) -> Condition (redecorate f a, redecorate f b, redecorate f c, f data)
  | Comparison (a, b, c, data) -> Comparison (redecorate f a, b, redecorate f c, f data)
  | Abstr (a, b, data) -> Abstr (a, redecorate f b, f data)
  | Let (a, b, c, data) -> Let (a, redecorate f b, redecorate f c, f data)
  | Rec (a, b, data) -> 
      Rec (map (fun (s,e,t) -> s, redecorate f e,t) a, redecorate f b, f data)
  | Xml_node (a, b, c, data) -> 
      Xml_node (a, map (fun (s,e) -> s, redecorate f e) b, 
                map (redecorate f) c, f data)
  | Record_empty (data) -> Record_empty (f data)
  | Record_extension (a, b, c, data) -> Record_extension (a, redecorate f b, redecorate f c, f data)
  | Record_selection (a, b, c, d, e, data) -> Record_selection (a, b, c, redecorate f d, redecorate f e, f data)
  | Record_selection_empty (a, b, data) -> Record_selection_empty (redecorate f a, redecorate f b, f data)
  | Variant_injection (a, b, data) -> Variant_injection (a, redecorate f b, f data)
  | Variant_selection (a, b, c, d, e, g, data) -> Variant_selection (redecorate f a, b, c, redecorate f d, e, redecorate f g, f data)
  | Variant_selection_empty (value, data) -> Variant_selection_empty (redecorate f value, f data)
  | Nil (data) -> Nil (f data)
  | List_of (a, data) -> List_of (redecorate f a, f data)
  | Concat (a, b, data) -> Concat (redecorate f a, redecorate f b, f data)
  | For (a, b, c, data) -> For (redecorate f a, b, redecorate f c, f data)
  | Database (a, data) -> Database (redecorate f a, f data)
  | TableHandle (a, b, c, data) -> TableHandle (redecorate f a, b, c, f data)
  | TableQuery (a, b, data) -> TableQuery (redecorate f a, b, f data)
  | SortBy (a, b, data) -> SortBy (redecorate f a, redecorate f b, f data)
  | Escape (var, body, data) -> Escape (var, redecorate f body, f data)
  | HasType (expr, typ, data) -> HasType (redecorate f expr, typ, f data)
  | Wrong (data) -> Wrong (f data)
  | Placeholder (s, data) -> Placeholder (s, f data) 
  | Alien (s1, s2, k, data) -> Alien (s1, s2, k, f data)

let strip_data : 'a expression' -> stripped_expression =
  fun e -> redecorate (fun _ -> ()) e

let erase : expression -> untyped_expression = 
  redecorate (fun (pos, _, _) -> pos)

let label_seq = ref 0
let new_label () = label_seq := !label_seq +1; !label_seq
let new_label_str () = "T" ^ string_of_int (new_label ())

let labelize (expr:expression) : expression = 
  redecorate 
    (fun (a,b,_) ->
       let label = new_label_str () in
(*          debug("labelizing " ^ label ^ " at " ^ string_of_expression expr); *)
	 (a, b, Some label)) expr

let labelize_ut (expr:untyped_expression) : expression = redecorate 
  (fun a ->
     (a, `Not_typed, Some(new_label_str ()))) expr

let labelize_env env = alistmap labelize env

let labelize_program (env, exprs) = (env, map labelize exprs)

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
               | TableQuery (e, _, _)
               | TableHandle (e, _, _, _) -> [visitor visit_children e]


               | Apply (e1, e2, _)
               | Comparison (e1, _, e2, _)
               | Let (_, e1, e2, _)
               | Record_extension (_, e1, e2, _)
               | Record_selection_empty (e1, e2, _)
               | Concat (e1, e2, _)
               | Record_selection (_, _, _, e1, e2, _)
               | For (e1, _, e2, _) ->
                   [visitor visit_children e1; visitor visit_children e2]
               | SortBy (e1, e2, _) ->
                   [visitor visit_children e1; visitor visit_children e2]
                   
               | Condition (e1, e2, e3, _)
               | Variant_selection (e1, _, _, e2, _, e3, _) -> [visitor visit_children e1; visitor visit_children e2; visitor visit_children e3]

               | Rec (b, e, _) -> visitor visit_children e :: map (fun (_, e, _) -> visitor visit_children e) b
               | Xml_node (_, es1, es2, _)          -> map (fun (_,v) -> visitor visit_children v) es1 @ map (visitor visit_children) es2)

  in
    visitor visit_children

(* Apply a function to each subnode.  Return Some c if any changes
   were made, otherwise None. *)
let perhaps_process_children (f : 'a expression' -> 'a expression' option) :  'a expression' -> 'a expression' option =
  let passto = Rewrite.passto f in
    function
        (* No children *)
      | Boolean _
      | Integer _
      | Char _
      | String _
      | Float _
      | Variable _
      | Nil _
      | Wrong _
      | Alien _
      | Record_empty _ -> None
          
      (* fixed children *)
      | HasType (e, k, b)                          -> passto [e] (fun [e] -> HasType (e, k, b))
      | Abstr (a, e, b)                            -> passto [e] (fun [e] -> Abstr (a, e, b))

      | Variant_injection (a, e, b)                -> passto [e] (fun [e] -> Variant_injection (a, e, b))
      | Define (a, e, b, c)                        -> passto [e] (fun [e] -> Define (a, e, b, c))
      | List_of (e, b)                             -> passto [e] (fun [e] -> List_of (e, b))
      | Database (e, a)                            -> passto [e] (fun [e] -> Database (e, a))
      | TableHandle (e, a, b, c)                   -> passto [e] (fun [e] -> TableHandle (e, a, b, c))
      | TableQuery (e, a, b)                       -> passto [e] (fun [e] -> TableQuery (e, a, b))
      | Escape (a, e, b)                           -> passto [e] (fun [e] -> Escape (a, e, b))
      | Variant_selection_empty (e, d)             -> passto [e] (fun [e] -> Variant_selection_empty (e, d))

      | Apply (e1, e2, a)                          -> passto [e1; e2] (fun [e1; e2] -> Apply (e1, e2, a))
      | Comparison (e1, a, e2, b)                  -> passto [e1; e2] (fun [e1; e2] -> Comparison (e1, a, e2, b))
      | Let (a, e1, e2, b)                         -> passto [e1; e2] (fun [e1; e2] -> Let (a, e1, e2, b))
      | Record_extension (a, e1, e2, b)            -> passto [e1; e2] (fun [e1; e2] -> Record_extension (a, e1, e2, b))
      | Record_selection (a, b, c, e1, e2, d)      -> passto [e1; e2] (fun [e1; e2] -> Record_selection (a, b, c, e1, e2, d))
      | Record_selection_empty (e1, e2, a)         -> passto [e1; e2] (fun [e1; e2] -> Record_selection_empty (e1, e2, a))
      | Concat (e1, e2, a)               -> passto [e1; e2] (fun [e1; e2] -> Concat (e1, e2, a))
      | For (e1, a, e2, b)        -> passto [e1; e2] (fun [e1; e2] -> For (e1, a, e2, b))
      | SortBy (e1, e2, b)        -> passto [e1; e2] (fun [e1; e2] -> SortBy (e1, e2, b))
      | Variant_selection (e1, a, b, e2, c, e3, d) -> passto [e1; e2; e3] (fun [e1; e2; e3] -> Variant_selection (e1, a, b, e2, c, e3, d))
      | Condition (e1, e2, e3, a)                  -> passto [e1; e2; e3] (fun [e1; e2; e3] -> Condition (e1, e2, e3, a))
      (* varying children *)
      | Rec (es, e2, a) -> (let names, vals, types = split3 es in 
                              passto (e2::vals) (fun (e2::vals) -> Rec (combine3 (names, vals, types), e2, a)))
      | Xml_node (a, es1, es2, b) -> 
          (let anames, avals = split es1 in 
             passto
               (avals @ es2)
               (fun children -> let alength = length avals in 
                  Xml_node (a, combine anames (take alength children), 
                            drop alength children, b)))
            
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

let fst3 (a, _, _) = a
and snd3 (_, b, _) = b
and thd3 (_, _, c) = c

let node_datatype : (expression -> datatype) = snd3 -<- expression_data
and node_pos  : (expression -> position) = fst3 -<- expression_data 
and untyped_pos  : (untyped_expression -> position) = expression_data 

let stringlit_value = function
  | String (name, _) -> name
