open Num
open List

open Utility
open Pickle
open Kind
open Sql

type position = Lexing.position * (* source line : *) string * (* expression source: *) string

let dummy_position = Lexing.dummy_pos, "<dummy>", "<dummy>"
    
exception Parse_failure of position * string

type location = [`Client | `Server | `Unknown]

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
  | Rec of ((string * ('data expression')) list * 'data expression' * 'data)
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
  | For of ('data expression' * string * 'data expression' * 
                               'data)
  | Database of ('data expression' * 'data)
  | Table of ('data expression' * string * Query.query * 'data)
  | Escape of (string * 'data expression' * 'data)
  | Wrong of 'data
  | HasType of ('data expression' * kind * 'data)
  | Placeholder of (string * 'data)


let is_define = function Define _ -> true | _ -> false


let s0 () () =
  "()"
let s1 (s1) (e1) =
  "(" ^ s1 e1 ^ ")"
let s2 (s1, s2) (e1, e2) =
  "(" ^ s1 e1 ^ "," ^ s2 e2  ^ ")"
let s3 (s1, s2, s3) (e1, e2, e3) =
  "(" ^ s1 e1 ^ "," ^ s2 e2 ^ "," ^ s3 e3 ^ ")"
let s4 (s1, s2, s3, s4) (e1, e2, e3, e4) =
  "(" ^ s1 e1 ^ "," ^ s2 e2 ^ "," ^ s3 e3 ^ "," ^ s4 e4 ^ ")"
let s5 (s1, s2, s3, s4, s5) (e1, e2, e3, e4, e5) =
  "(" ^ s1 e1 ^ "," ^ s2 e2 ^ "," ^ s3 e3 ^ "," ^ s4 e4 ^ "," ^ s5 e5 ^ ")"
let s6 (s1, s2, s3, s4, s5, s6) (e1, e2, e3, e4, e5, e6) =
  "(" ^ s1 e1 ^ "," ^ s2 e2 ^ "," ^ s3 e3 ^ "," ^ s4 e4 ^ "," ^ s5 e5 ^ "," ^ s6 e6 ^ ")"
let s7 (s1, s2, s3, s4, s5, s6, s7) (e1, e2, e3, e4, e5, e6, e7) =
  "(" ^ s1 e1 ^ "," ^ s2 e2 ^ "," ^ s3 e3 ^ "," ^ s4 e4 ^ "," ^ s5 e5 ^ "," ^ s6 e6 ^  "," ^ s7 e7 ^ ")"

let null = fun _ -> "_"

let slist s l = 
  "[" ^ String.concat "," (map s l) ^ "]"

let gensym =
  let counter = ref 0 in
    function str ->
      begin
        incr counter;
        str ^ "_g" ^ string_of_int !counter
      end

let rec show_expression = 
  let exp = show_expression in
    function
  | Define v -> "Define " ^ s4 (identity, show_expression, null, null) v
  | Boolean v -> "Boolean " ^ s2 (string_of_bool, null) v
  | Integer v -> "Integer " ^ s2 (string_of_num, null) v
  | Char v -> "Char " ^ s2 (String.make 1, null) v
  | String v -> "String " ^ s2 (identity, null) v
  | Float v -> "Float " ^ s2 (string_of_float, null) v
  | Variable v -> "Variable " ^ s2 (identity, null) v
  | Apply v -> "Apply " ^ s3 (exp, exp, null) v
  | Condition v -> "Condition " ^ s4 (exp, exp, exp, null) v
  | Comparison v -> "Comparison " ^ s4 (exp, identity, exp, null) v
  | Abstr v -> "Abstr " ^ s3 (identity, exp, null) v
  | Let v -> "Let " ^ s4 (identity, exp, exp, null) v
  | Rec v -> "Rec " ^ s3 (slist (s2 (identity, exp)), exp, null) v
  | Xml_node v -> "Xml_node " ^ s4 (identity, slist (s2 (identity, exp)), slist exp, null) v
  | Record_empty v -> "Record_empty " ^ s1 (null) v
  | Record_extension v -> "Record_extension " ^ s4 (identity, exp, exp, null) v
  | Record_selection v -> "Record_selection " ^ s6 (identity, identity, identity, exp, exp, null) v
  | Record_selection_empty v -> "Record_selection_empty " ^ s3 (exp, exp, null) v
  | Variant_injection v -> "Variant_injection " ^ s3 (identity, exp, null) v
  | Variant_selection v -> "Variant_selection " ^ s7 (exp, identity, identity, exp, identity, exp, null) v
  | Variant_selection_empty v -> "Variant_selection_empty " ^ s2 (exp, null) v
  | Nil v -> "Nil " ^ s1 (null) v
  | List_of v -> "List_of " ^ s2 (exp, null) v
  | Concat v -> "Concat " ^ s3 (exp, exp, null) v
  | For v -> "For " ^ s4 (exp, identity, exp, null) v
  | Database v -> "Database " ^ s2 (exp, null) v
  | Table v -> "Table " ^ s4 (exp, identity, string_of_query, null) v
  | Escape v -> "Escape " ^ s3 (identity, exp, null) v
  | Wrong v -> "Wrong " ^ s1 null v
  | HasType v -> "HasType " ^ s3 (exp, null, null) v

type expression = (position * kind * string option (* label *)) expression'
type untyped_expression = position expression'

let string_of_location : location -> string = function
  | `Client -> "client" | `Server -> "server" | `Unknown -> "unknown"

let rec unparse_sequence empty unit append = function 
  | Nil _ -> empty
  | List_of (elem, _) -> unit elem
  | Concat (l, r, _) -> (append 
                                     (unparse_sequence empty unit append l) 
                                     (unparse_sequence empty unit append r))
  | other -> failwith ("Unexpected argument to unparse_sequence : " ^ show (fun _ -> "") other)
and unparse_list x = unparse_sequence [] (fun x -> [x]) (@) x
and show t : 'a expression' -> string = function 
  | HasType(expr, kynd, data) -> show t expr
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
      "{" ^ (String.concat " ; " (map (function (label, expr) -> " " ^ label ^ "=" ^ show t expr) variables))
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
  | Table (daba, s, query, data) ->
      "("^ s ^" from "^ show t daba ^"["^string_of_query query^"])" ^ t data
  | Wrong data -> "wrong"

(* this is meant to do the work for pretty-printing an expression,
   making it look like the user would expect it to look; e.g. it should
   fold nested record extensions back into a single record.
   very incomplete at this point.
*)
let rec ppexpr t : 'a expression' -> string = function 
  | Define (variable, value, location, data) -> variable ^ "=" ^ ppexpr t value
      ^ "[" ^ string_of_location location ^ "]; " ^ t data
  | Boolean (value, data) -> string_of_bool value ^ t data
  | Integer (value, data) -> string_of_num value ^ t data
  | Char (c, data) -> "'"^ Char.escaped c ^"'" ^ t data
  | String (s, data) -> "\"" ^ s ^ "\"" ^ t data
  | Float (value, data)   -> string_of_float value ^ t data
  | Variable (name, data) -> name ^ t data

  | Apply (Variable("enxml", _), String (s, _), data)    -> s ^ t data

  | Apply (f, p, data)    -> ppexpr t f ^ "(" ^ ppexpr t p ^ ")" ^ t data
  | Condition (cond, if_true, if_false, data) ->
      "if " ^ ppexpr t cond ^ " then " ^ ppexpr t if_true ^ " else " ^ ppexpr t if_false ^ "" ^ t data
  | Comparison (left_value, oper, right_value, data) ->
      "" ^ ppexpr t left_value ^ " " ^ oper ^ " " ^ ppexpr t right_value ^ "" ^ t data
  | Abstr (variable, body, data) ->
      "fun (" ^ variable ^ ") {" ^ ppexpr t body ^ "}" ^ t data
  | Let (variable, value, body, data) ->
      "{" ^ variable ^ "=" ^ ppexpr t value ^ "; " ^ ppexpr t body ^ "}" ^ t data
  | Rec (variables, body, data) ->
      "{" ^ (String.concat " ; " (map (function (label, expr) -> " " ^ label ^ "=" ^ ppexpr t expr) variables))
      ^ "; " ^ ppexpr t body ^ "}" ^ t data
  | Escape(var, body, data) -> "escape {" ^ var ^ " in " ^ ppexpr t body ^"}"^ t data
  | Xml_node (tag, attrs, elems, data) ->  
      let attrs = 
        let attrs = String.concat " " (map (fun (k, v) -> k ^ "=\"" ^ ppexpr t v ^ "\"") attrs) in
          match attrs with 
            | "" -> ""
            | _ -> " " ^ attrs in
        (match elems with 
           | []    -> "<" ^ tag ^ attrs ^ "/>" ^ t data
           | elems -> "<" ^ tag ^ attrs ^ ">" ^ String.concat "" (map (ppexpr t) elems) ^ "</" ^ tag ^ ">" ^ t data)
  | Record_empty (data) ->  "()" ^ t data
  | Record_extension (_,_,_,data) as record ->
      pp_record t data "(" record
  | Record_selection (label, label_variable, variable, value, body, data) ->
      "{(" ^ label ^ "=" ^ label_variable ^ "|" ^ variable ^ ") = " 
      ^ ppexpr t value ^ "; " ^ ppexpr t body ^ "}" ^ t data
  | Record_selection_empty (value, body, data) ->
      "{() = " ^ ppexpr t value ^ "; " ^ ppexpr t body ^ "}" ^ t data
  | Variant_injection (label, value, data) ->
      label ^ "(" ^ ppexpr t value ^ ")" ^ t data
  | Variant_selection (value, case_label, case_variable, case_body, variable, body, data) ->
      "case " ^ ppexpr t value ^ " of < " ^ case_label ^ "=" 
      ^ case_variable ^ "> in " ^ (ppexpr t case_body) ^ " | " 
      ^ variable ^ " in " ^ ppexpr t body ^ t data
  | Variant_selection_empty (value, data) ->
      show t value ^ " is empty" ^ t data
  | Nil (data)              -> "[]" ^ t data
  | Nil (data)              -> "[]" ^ t data
  | List_of (elem, data)       -> "[" ^ ppexpr t elem ^ "]" ^ t data
  | Concat (left, right, data)  -> "(" ^ ppexpr t left ^ t data ^ "::" ^ ppexpr t right ^ ")" 
  | For (expr, variable, value, data) ->
      "(for " ^ variable ^ " <- " ^ ppexpr t value ^ " in " ^ ppexpr t expr ^ ")" ^ t data
  | Database (params, data) -> "(database " ^ ppexpr t params ^ ")" ^ t data
  | Table (daba, s, query, data) ->
      "("^ s ^" from "^ ppexpr t daba ^"["^string_of_query query^"])" ^ t data
and pp_record t orig_data accum = function 
  | Record_empty _ -> (accum ^ ")" ^ t orig_data)
  | Record_extension (label, value, record, data) ->
      pp_record t orig_data (accum ^ label ^ "=" ^ ppexpr t value ^ ", ") record
  | expr -> (accum ^ ")" ^ t orig_data) ^ ppexpr t expr

let prettyprint expr = ppexpr (fun _ -> " ") expr

let string_of_kinded_expression (s : expression) : string = 
  show (function (_, kind, _) -> 
	  " : (" ^ (string_of_kind kind) ^ ")") s

let with_label = (fun (pos, kind, lbl) ->
     " [" ^ fromOption "BOGUS" lbl ^ "] ")

let string_of_expression s = show (fun _ -> " ") s

let string_of_labelled_expression (s : expression) = show with_label s

let string_of_order order = match order with
  | `Asc name -> ""^ name ^":asc"
  | `Desc name -> ""^ name ^":asc"

let string_of_orders orders = match orders with 
  | [] -> " " 
  | _ -> "order [" ^ String.concat ", " (map string_of_order orders) ^ "]"


let rec serialise_expression : ('data expression' serialiser) 
    (* Ick.  Some introspection would help here. *)
    = let string = serialise_string
      and exp = serialise_expression
      and data = null_serialiser in
      (function 
        | Define v                  -> serialise4 'd' (string, exp, serialise_location, data) v
        | Boolean v                 -> serialise2 'b' (serialise_bool, data) v
        | Integer v                 -> serialise2 'i' (serialise_int, data) v
        | Char v                    -> serialise2 'c' (serialise_char, data) v
        | String v                  -> serialise2 's' (string, data) v
        | Float v                   -> serialise2 'f' (serialise_float, data) v
        | Variable v                -> serialise2 'v' (string, data) v
        | Apply v                   -> serialise3 'A' (exp, exp, data) v
        | Condition v               -> serialise4 'C' (exp, exp, exp, data) v
        | Comparison v              -> serialise4 'S' (exp, string, exp, data) v
        | Abstr v                   -> serialise3 'F' (string, exp, data) v
        | Let v                     -> serialise4 'L' (string, exp, exp, data) v
        | Rec v                     -> serialise3 'N' (serialise_list serialise_recbinding, exp, data) v
        | Xml_node v                -> serialise4 'g' (string, serialise_list serialise_recbinding (* temporary *), serialise_list serialise_expression, data) v
        | Record_empty v            -> serialise1 'R' (data) v
        | Record_extension v        -> serialise4 'E' (string, exp, exp, data) v
        | Record_selection v        -> serialise6 'G' (string, string, string, exp, exp, data) v
        | Record_selection_empty v  -> serialise3 'H' (exp, exp, data) v
        | Variant_injection v       -> serialise3 'I' (string, exp, data) v
        | Variant_selection v       -> serialise7 'J' (exp, string, string, exp, string, exp, data) v
        | Variant_selection_empty v -> serialise1 'K' (data) v
        | Nil v        -> serialise1 'O' (data) v
        | List_of v       -> serialise2 'P' (exp, data) v
        | Concat v        -> serialise3 'Q' (exp, exp, data) v
        | For v    -> serialise4 'T' (exp, string, exp, data) v
        | Database v                -> serialise2 'V' (exp, data) v
        | Table v                   -> serialise4 'Z' (exp, string, serialise_query, data) v
        | Escape v                  -> serialise3 'e' (string, exp, data) v
        | Placeholder v             -> serialise2 'p' (string, data) v
      )
and deserialise_expression : (expression deserialiser)
    = let poskind = null_deserialiser (dummy_position, `Not_typed, None)
      and string = deserialise_string
      and exp = deserialise_expression in
      fun s -> let t, obj, rest = extract_object s in
        let e = 
        (match t with 
           | 'd' -> Define (deserialise4 (string, exp, deserialise_location, poskind) obj)
           | 'b' -> Boolean (deserialise2 (deserialise_bool, poskind) obj)
           | 'i' -> Integer (deserialise2 (deserialise_int, poskind) obj)
           | 'c' -> Char (deserialise2 (deserialise_char, poskind) obj)
           | 's' -> String (deserialise2 (deserialise_string, poskind) obj)
           | 'f' -> Float (deserialise2 (deserialise_float, poskind) obj)
           | 'v' -> Variable (deserialise2 (string, poskind) obj)
           | 'A' -> Apply (deserialise3 (exp, exp, poskind) obj)
           | 'C' -> Condition (deserialise4 (exp, exp, exp, poskind) obj)
           | 'S' -> Comparison (deserialise4 (exp, string, exp, poskind) obj)
           | 'F' -> Abstr (deserialise3 (string, exp, poskind) obj)
           | 'L' -> Let (deserialise4 (string, exp, exp, poskind) obj)
           | 'N' -> Rec (deserialise3 (deserialise_list deserialise_recbinding, exp, poskind) obj)
           | 'g' -> Xml_node (deserialise4 (string, deserialise_list deserialise_recbinding, deserialise_list deserialise_expression, poskind) obj)
           | 'R' -> Record_empty (deserialise1 (poskind) obj)
           | 'E' -> Record_extension (deserialise4 (string, exp, exp, poskind) obj)
           | 'G' -> Record_selection (deserialise6 (string, string, string, exp, exp, poskind) obj)
           | 'H' -> Record_selection_empty (deserialise3 (exp, exp, poskind) obj)
           | 'I' -> Variant_injection (deserialise3 (string, exp, poskind) obj)
           | 'J' -> Variant_selection (deserialise7 (exp, string, string, exp, string, exp, poskind) obj)
           | 'K' -> Variant_selection_empty (deserialise2 (exp, poskind) obj)
           | 'O' -> Nil (deserialise1 (poskind) obj)
           | 'P' -> List_of (deserialise2 (exp, poskind) obj)
           | 'Q' -> Concat (deserialise3 (exp, exp, poskind) obj)
           | 'T' -> For (deserialise4 (exp, string, exp, poskind) obj)
           | 'V' -> Database (deserialise2 (exp, poskind) obj)
           | 'Z' -> Table (deserialise4 (exp, string, deserialise_query, poskind) obj)
           | 'p' -> Placeholder(deserialise2 (string, poskind) obj)
           | x -> failwith ("Unexpected expression type header during deserialisation : "^ (String.make 1  x)))
      in e, rest
and serialise_recbinding : (string * expression) serialiser
    = fun b -> serialise2 'B' (serialise_string, serialise_expression) b
and deserialise_recbinding :  (string * expression) deserialiser
    = fun s ->
      let t, obj, rest = extract_object s in
        match t with
          | 'B' -> deserialise2 (deserialise_string, deserialise_expression) obj, rest
          | x -> failwith ("Error deserialising binding header (expected 'B'; got '" ^ (String.make 1 x) ^ "')")
and serialise_location : location serialiser
    = function | `Client -> "c" | `Server -> "s" | `Unknown -> "u"
and deserialise_location : location deserialiser
    = fun s -> (assoc (String.sub s 0 1) ["c", `Client; "s", `Server; "u", `Unknown],
                String.sub s 1 (String.length s - 1))

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
        let boundvals = map (fun (name, value) -> 
                               let e, d = visitor visit_children (value, data) in
                                 (e, d, name)) b in
        let bindings_data = map (fun (_,d,_) -> d)  boundvals in 
        let data1 = fold_left combiner (hd bindings_data) (tl bindings_data) in
        let body, data2 = visitor visit_children (body, data) in 
          (Rec (map (fun (e, _, name) -> (name, e)) boundvals, body, d),
           combiner data1 data2)
    | Xml_node (tag, attrs, elems, d) -> 
        (* Not 100% sure this is right *)
        let attrvals = map (fun (name, value) ->
                              let e, d = visitor visit_children (value, data) in 
                                (e, d, name)) attrs in
        let data1 = (match map (fun (_,d,_)->d) attrvals with
                       | [] -> data
                       | attrdata -> fold_left combiner (hd attrdata) (tl attrdata)) in
        let bodyvals = map (fun value ->
                              let e, d = visitor visit_children (value, data) in
                                (e, d)) elems in
        let data2 = (match map snd bodyvals with
                       | [] -> data
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
    | Table (e, s, q, d) -> let e, data = visitor visit_children (e, data) in
        Table (e, s, q, d), data
    | Escape (n, e, d) -> let e, data = visitor visit_children (e, data) in
        Escape (n, e, d), data
    | Wrong (d) -> Wrong (d), unit data
    | Placeholder (str, d) -> Placeholder (str, d), unit data
  in visitor visit_children
       
(* A simplified version which doesn't pass data around *)
let simple_visit visitor expr = let visitor default (e, _) = (visitor (fun e -> fst (default (e, ()))) e, ()) in
  fst (visit_expressions' (fun () () -> ()) (fun () -> ()) visitor (expr, ()))

(* Could be made more efficient by not constructing the expression in
   parallel (i.e. discarding it, similar to `simple_visit'  *)
let freevars : 'a expression' -> string list = 
  (* FIXME : doesn't consider variables in the query *)
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
              let vars = map fst bindings @ vars in  
                expr, List.concat (map (fun value -> (childvars (value, vars))) (map snd bindings)) @ childvars (body, vars)
	  | Escape (var, body, _) -> expr, childvars (body, var::vars)
          | Table (_, _, query, _) -> expr, Query.freevars query
          | other -> default (other, vars)
    in snd (visit_expressions' (@) (fun _ -> []) visit (expression, []))

let qnamep name = String.contains name ':'

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
      Rec (map (fun (s,e) -> s, redecorate f e) a, redecorate f b, f data)
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
  | Table (a, b, c, data) -> Table (redecorate f a, b, c, f data)
  | Escape (var, body, data) -> Escape (var, redecorate f body, f data)
  | HasType (expr, typ, data) -> HasType (redecorate f expr, typ, f data)
  | Wrong (data) -> Wrong (f data)

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
               | Variable _ -> []

               | Variant_selection_empty (e, _)
               | Define (_, e, _, _)
               | Abstr (_, e, _)
               | Database (e, _)
               | Variant_injection (_, e, _)
               | List_of (e, _)
               | Escape (_, e, _)
               | Table (e, _, _, _) -> [visitor visit_children e]


               | Apply (e1, e2, _)
               | Comparison (e1, _, e2, _)
               | Let (_, e1, e2, _)
               | Record_extension (_, e1, e2, _)
               | Record_selection_empty (e1, e2, _)
               | Concat (e1, e2, _)
               | Record_selection (_, _, _, e1, e2, _)
               | For (e1, _, e2, _) ->
                   [visitor visit_children e1; visitor visit_children e2]
                   
               | Condition (e1, e2, e3, _)
               | Variant_selection (e1, _, _, e2, _, e3, _) -> [visitor visit_children e1; visitor visit_children e2; visitor visit_children e3]

               | Rec (b, e, _) -> visitor visit_children e :: map (fun (_, e) -> visitor visit_children e) b
               | Xml_node (_, es1, es2, _)          -> map (fun (_,v) -> visitor visit_children v) es1 @ map (visitor visit_children) es2)
  in
    visitor visit_children

(* Apply a function to each subnode.  Return Some c if any changes
   were made, otherwise None. *)
let perhaps_process_children (f : 'a expression' -> 'a expression' option) :  'a expression' -> 'a expression' option =
  let transform = 
    let rec aux passed es = function
      | [] -> passed, es
      | x::xs ->
          (match f x with 
             | Some x -> aux true (x::es) xs
             | None   -> aux passed (x::es) xs) in
      aux false [] in
    (** passto: if applying f to any of the expressions has an effect, pass all
        the transformed or original to `next' and return Some of the
        result.  Otherwise, return None *)
  let passto exprs next = 
    match transform exprs with
      | false, _ -> None
      | true,  es -> Some (next (List.rev es))
  in
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
      | Record_empty _ -> None
          
      (* fixed children *)
      | HasType (e, k, b)                          -> passto [e] (fun [e] -> HasType (e, k, b))
      | Abstr (a, e, b)                            -> passto [e] (fun [e] -> Abstr (a, e, b))
      | Variant_injection (a, e, b)                -> passto [e] (fun [e] -> Variant_injection (a, e, b))
      | Define (a, e, b, c)                        -> passto [e] (fun [e] -> Define (a, e, b, c))
      | List_of (e, b)                             -> passto [e] (fun [e] -> List_of (e, b))
      | Database (e, a)                            -> passto [e] (fun [e] -> Database (e, a))
      | Table (e, a, b, c)                         -> passto [e] (fun [e] -> Table (e, a, b, c))
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
      | Variant_selection (e1, a, b, e2, c, e3, d) -> passto [e1; e2; e3] (fun [e1; e2; e3] -> Variant_selection (e1, a, b, e2, c, e3, d))
      | Condition (e1, e2, e3, a)                  -> passto [e1; e2; e3] (fun [e1; e2; e3] -> Condition (e1, e2, e3, a))
      (* varying children *)
      | Rec (es, e2, a) -> (let names, vals = split es in 
                              passto (e2::vals) (fun (e2::vals) -> Rec (combine names vals, e2, a)))
      | Xml_node (a, es1, es2, b) -> 
          (let anames, avals = split es1 in 
             passto
               (avals @ es2)
               (fun children -> let alength = length avals in 
                  Xml_node (a, combine anames (take alength children), 
                            drop alength children, b)))
            
(* Apply a function to each subnode.  Return Some c if any changes
   were made, otherwise None. *)
let perhaps_process_children_bindings 
    (f : string list -> expression -> expression option) 
    (vars :string list) :  expression -> expression option =
  let transform = 
    let rec aux passed es = function
      | [] -> passed, es
      | (f,x)::rest ->
          (match f x with 
             | Some x -> aux true (x::es) rest
             | None   -> aux passed (x::es) rest) in
      aux false [] in
  let passto exprs next = 
    (* if applying f to any of the expressions has an effect, pass all
       the transformed or original to `next' and return Some of the
       result.  Otherwise, return None *)
    match transform exprs with
      | false, _ -> None
      | true,  es -> Some (next es) in
  let bind names = f (names @ vars) in
    function
        (* No children *)
      | Boolean _
      | Integer _
      | Char _
      | String _
      | Float _
      | Variable _
      | Nil _
      | Variant_selection_empty _
      | Record_empty _ -> None
          
      (* fixed children *)
      | Abstr (var, e, b)                          -> passto [bind [var],e] (fun [e] -> (* var visible in body *)
        Abstr (var, e, b))
      | Variant_injection (a, e, b)                -> passto [bind [],e] (fun [e] -> 
        Variant_injection (a, e, b))
      | Define (var, e, b, c)                      -> passto [bind [],e] (fun [e] ->  (* binding not visible in rhs *)
        Define (var, e, b, c))
      | List_of (e, b)                             -> passto [bind [],e] (fun [e] ->
        List_of (e, b))
      | Database (e, a)                            -> passto [bind [],e] (fun [e] ->
        Database (e, a))
      | Table (e, a, b, c)                         -> passto [bind [],e] (fun [e] ->
        Table (e, a, b, c))
      | Escape (var, e, b)                         -> passto [bind [var],e] (fun [e] -> (* binding visible in body *)
        Escape (var, e, b))
      | Apply (e1, e2, a)                          -> passto [bind [],e1; bind [],e2] (fun [e1;e2] ->
        Apply (e1, e2, a))
      | Comparison (e1, a, e2, b)                  -> passto [bind [],e1; bind [],e2] (fun [e1;e2] ->
        Comparison (e1, a, e2, b))
      | Let (var, e1, e2, b)                       -> passto [bind [],e1; bind [var],e2] (fun [e1;e2] ->
        Let (var, e1, e2, b))
      | Record_extension (a, e1, e2, b)            -> passto [bind [],e1; bind [],e2] (fun [e1;e2] ->
        Record_extension (a, e1,  e2, b))
      | Record_selection (a, var1, var2, e1, e2, d)-> passto [bind [],e1; bind [var1;var2],e2] (fun [e1;e2] ->
        Record_selection (a, var1, var2, e1, e2, d))
      | Record_selection_empty (e1, e2, a)         -> passto [bind [],e1; bind [],e2] (fun [e1;e2] ->
        Record_selection_empty (e1, e2, a))
      | Concat (e1, e2, a)               -> passto [bind [],e1; bind [],e2] (fun [e1;e2] ->
        Concat (e1, e2, a))
      | For (e1, var, e2, b)      -> passto [bind [],e1; bind [var],e2] (fun [e1;e2] ->
        For (e1, var, e2, b))
      | Variant_selection (e1, a, var1, e2, var2, e3, d) -> passto [bind [],e1; bind [var1],e2; bind [var2],e3]
	                                                           (fun [e1;e2;e3] ->
	Variant_selection (e1, a, var1, e2, var2, e3, d))
      | Condition (e1, e2, e3, a)                  -> passto [bind [],e1; bind [],e2; bind [],e2] (fun [e1;e2;e3] ->
        Condition (e1, e2, e3, a))
  
(* is this right... doesn't look like it *)        
(*
open Num
open List

open Utility
open Kind
*)

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
	| Table (_, _, _, data) -> data
	| Escape (_, _, data) -> data
        | Wrong data -> data

let fst3 (a, _, _) = a
and snd3 (_, b, _) = b
and thd3 (_, _, c) = c

let node_kind : (expression -> kind) = snd3 -<- expression_data
and node_pos  : (expression -> position) = fst3 -<- expression_data 
and untyped_pos  : (untyped_expression -> position) = expression_data 

