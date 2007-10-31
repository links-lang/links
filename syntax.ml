(*pp deriving *)
open Num
open List
open Utility
open Show
open Pickle

type lexpos = Lexing.position 
module Typeable_lexpos = Typeable.Primitive_typeable(struct type t = lexpos end)

module Eq_lexpos : Eq.Eq with type a = lexpos = 
struct
  type a = lexpos
  let eq = (==)
end

let print_digest_junk = Settings.add_bool("print_digest_junk", false, `User)

module LexposType = struct type a = lexpos let tname = "Syntax.lexpos" end
module Show_lexpos = Show_unprintable (LexposType)
(*module Pickle_lexpos = Pickle_unpicklable (LexposType)*)

type position = lexpos *  (* source line: *) string 
                  * (* expression source: *) string
    deriving (Typeable, Show,  Eq)

let dummy_position = Lexing.dummy_pos, "<dummy>", "<dummy>"
    
exception ASTSyntaxError of position * string

type location = [`Client | `Server | `Native | `Unknown]
    deriving (Eq, Typeable, Show, Pickle, Shelve)

type label = string
    deriving (Eq, Typeable, Show, Pickle, Shelve)
    (* Q: Can I write my own show for these? I want to base64 it *)

let string_of_comparison = function
  | `Less   -> "<"
  | `LessEq -> "<="
  | `Equal  -> "=="
  | `NotEq  -> "<>"

type constant =
  | Boolean of bool
  | Integer of num
  | Char of char
  | String of string
  | Float of float
      deriving (Eq, Typeable, Show, Pickle, Shelve)

type 'data expression' =
  | Constant of (constant * 'data)
  | Variable of (string * 'data)

  | Abs of ('data expression' * 'data)
  | App of ('data expression' * 'data expression' * 'data)

  | Apply of ('data expression' * 'data expression' list * 'data)
  | Condition of ('data expression' * 'data expression' * 'data expression' * 
                    'data)
  | Comparison of ('data expression' * Syntaxutils.comparison * 
                     'data expression' * 'data)
  | Abstr of (string list * 'data expression' * 'data)
  | Let of (string * 'data expression' * 'data expression' * 'data)
  | Rec of ((string * 'data expression' * Types.datatype option) list 
            * 'data expression' * 'data)
  | Xml_node of (string * ((string * 'data expression') list) * 
                   ('data expression' list) * 'data)
  | Record_intro of (('data expression') stringmap * ('data expression') option 
                     * 'data)
  | Project of ('data expression' * string * 'data)
  | Erase of ('data expression' * string * 'data)
  | Variant_injection of (string * 'data expression' * 'data)
  | Variant_selection of ('data expression' * 
                            string * string * 'data expression' * 
                            string * 'data expression' * 'data)
  | Variant_selection_empty of ('data expression' * 'data)
  | Nil of ('data)
  | List_of of ('data expression' * 'data)
  | Concat of ('data expression' * 'data expression' * 'data)
  | For of ('data expression' * string * 'data expression' * 'data)
      (* For(body, var, src, _data) *)
  | Database of ('data expression' * 'data)
  | TableQuery of (SqlQuery.sqlQuery * 'data)
  | TableHandle of ((* the database: *) 'data expression' 
      * (* the table: *) 'data expression'
      * (* the read / write (record) types of a table row: *) 
        (Types.datatype * Types.datatype)
      * 'data)
     
  | SortBy of ('data expression' * 'data expression' * 'data)
  | Call_cc of ('data expression' * 'data)
  | Wrong of 'data
  | HasType of ('data expression' * Types.datatype * 'data)
      deriving (Functor, Rewriter, Show)
      (* Q: Should syntax exprs be picklable or not? *)

type 'a definition' =
  | Define of (string * 'a expression' * location * 'a)
  | Alias of (string * int list * Types.datatype * 'a)
  | Alien of (string * string * Types.assumption * 'a)
      deriving (Functor, Rewriter, Show)

(* [HACK]
   programs derive Functor and Rewriter
   but they don't work yet for tuple types!

   hence the redundant 'Program' tag.

   [REMARK]

   Perhaps the tag isn't such a bad thing after all... it helps with
   documenting the code.
*)

let visit_def unit visitor def =
  match def with
    | Define(name, expr, loc_annotation, d) -> visitor expr
    | Alias _
    | Alien _ -> unit

type 'a program' = Program of ('a definition' list * 'a expression')
  deriving (Functor, Rewriter, Show)

let program_body :'a program' -> 'a expression' =
  function Program(_defs, body) -> body

let program_defs :'a program' -> 'a definition' list =
  function Program(defs, _body) -> defs

let unit_expression data = Record_intro (StringMap.empty, None, data)

let defined_names exprs = 
  concat_map (function Define(f, _, _, _) -> [f] | _ -> []) exprs

(* Whether a syntax node is a value for the purposes of generalization.
   This means, approximately "it doesn't contain any applications" *)
let rec is_value : 'a expression' -> bool = function
  | Constant _
  | Variable _
  | Xml_node _ (* ? *)
  | Nil _
  | Abstr _ -> true
  | HasType (e, _, _)
  | Project (e, _, _)
  | Erase (e, _,_)
  | Variant_injection (_, e, _)
  | Variant_selection_empty (e, _)
  | Database (e, _)
  | List_of (e, _) -> is_value e
  | TableHandle (a, b, _, _)
  | Comparison (a,_,b,_)
  | Concat (a, b, _)
  | For (a, _, b, _)
  | Let (_, a, b,_)  -> is_value a && is_value b
  | Variant_selection (a, _, _, b, _, c, _)
  | Condition (a,b,c,_) -> is_value a && is_value b && is_value c
  | Record_intro (bs, e, _) ->
      StringMap.for_all (is_value) bs && opt_app is_value true e
  | Rec (bs, e, _) -> List.for_all (is_value -<- (fun (_,x,_) -> x)) bs && is_value e
  | _ -> false


type typed_data = [`T of (position * Types.datatype * label option)] deriving (Eq, Typeable, Show)
type untyped_data = [`U of position] deriving (Eq, Typeable, Show)
type data = [untyped_data | typed_data] deriving (Typeable, Show)

let is_symbolic_ident name = 
  (Str.string_match (Str.regexp "^[!$%&*+/<=>?@\\^-.|_]+$") name 0)

let is_alphanumeric_ident name = 
  (Str.string_match (Str.regexp "^[a-zA-Z_][a-zA-Z_0-9]*$") name 0)

let rec show t : 'a expression' -> string = function 
  | HasType(expr, datatype, data) -> show t expr ^ " : " ^ Types.string_of_datatype datatype ^ t data
  | Constant(c, data) ->
      begin
        match c with
          | Boolean value -> string_of_bool value ^ t data
          | Integer value -> string_of_num value ^ t data
          | Char c -> "'"^ Char.escaped c ^"'" ^ t data
          | String s -> "\"" ^ s ^ "\"" ^ t data
          | Float value   -> string_of_float value ^ t data
      end
  | Variable (name, data) when is_symbolic_ident name -> "(" ^ name ^ ")" ^ t data
  | Variable (name, data) -> name ^ t data
  | Apply (f, ps, data)    -> show t f ^ "(" ^ String.concat "," (List.map (show t) ps) ^ ")" ^ t data
  | Abs (f, data) -> "abs " ^ show t f ^ t data
  | App (e1, e2, data) -> show t e1 ^ " app " ^ show t e2 ^ t data
  | Condition (cond, if_true, if_false, data) ->
      "if (" ^ show t cond ^ ") " ^ show t if_true ^ " else " ^ show t if_false ^ t data
  | Comparison (left_value, oper, right_value, data) ->
      show t left_value ^ " " ^ string_of_comparison oper ^ " " ^ show t right_value ^ t data
  | Abstr (variables, body, data) ->
      "fun (" ^ String.concat ", " variables ^ ") { " ^ show t body ^ " }" ^ t data
  | Let (variable, value, body, data) ->
      "{ var " ^ variable ^ "=" ^ show t value ^ "; " ^ show t body ^ "}" ^ t data
  | Rec (variables, body, data) ->
      "{" ^ (String.concat " ; " (map (function (label, expr, _) -> " " ^ label ^ "=" ^ show t expr) variables))
      ^ "; " ^ show t body ^ "}" ^ t data
  | Call_cc (Abstr([var], body, _), data) -> 
      "escape " ^ var ^ " in " ^ show t body ^ t data
  | Call_cc (f, data) -> "callCC " ^ show t f ^ t data
  | Xml_node (tag, attrs, elems, data) ->  
      let attrs = 
        let attrs = String.concat " " (map (fun (k, v) -> k ^ "=\"" ^ show t v ^ "\"") attrs) in
          match attrs with 
            | "" -> ""
            | _ -> " " ^ attrs in
        (match elems with 
           | []    -> "<" ^ tag ^ attrs ^ "/>" ^ t data
           | elems -> "<" ^ tag ^ attrs ^ ">" ^ String.concat "" (map (show t) elems) ^ "</" ^ tag ^ ">" ^ t data)
  | Record_intro (bs, r, data) ->
      "(" ^
        String.concat ","
        (StringMap.to_list (fun label e -> label ^ "=" ^ (show t e)) bs) ^
        (opt_app (fun e -> " | " ^ show t e) "" r) ^
        ")" ^ t data
  | Project (e, l, data) -> show t e ^ "." ^ l ^ t data
  | Erase (e, l, data) -> show t e ^ "\\" ^ l ^ t data
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
  | TableHandle (db, name, (readtype, writetype), data) ->
      "("^ show t name ^" from "^ show t db ^
      "["^Types.string_of_datatype readtype^";"^Types.string_of_datatype writetype^"])" ^ t data
  | TableQuery (query, data) ->
      "(["^SqlQuery.Show_sqlQuery.show query^"])" ^ t data
  | SortBy (expr, byExpr, data) ->
      "sort (" ^ show t expr ^ ") by (" ^ show t byExpr ^ ")" ^ t data
  | Wrong data -> "wrong" ^ t data

let show_definition t : 'a definition' -> string = function
  | Define (variable, value, location, data) -> 
      (if is_symbolic_ident variable then "(" ^ variable ^ ")" else variable) 
      ^ "=" ^ show t value
      ^ "[" ^ Show_location.show location ^ "]; " ^ t data
  | Alias (typename, quantifiers, datatype, data) ->
      "typename "^typename^"(TODO:update pretty-printer to display quantifiers) = "^ Types.string_of_datatype datatype ^ t data
  | Alien (s1, s2, k, data) -> Printf.sprintf "alien %s %s : %s;" s1 s2 (Types.string_of_assumption k) ^ t data

let show_program t : 'a program' -> string =
  fun (Program (ds, body)) ->
    (String.concat "" (List.map (show_definition t) ds)) ^ show t body

let string_of_expression s = show (fun _ -> "") s
let string_of_definition d = show_definition (fun _ -> "") d
let string_of_program p = show_program (fun _ -> "") p

type expression = typed_data  expression'
and untyped_expression = untyped_data expression'
and stripped_expression = unit expression'
  deriving (Show)

type definition = typed_data definition'
and untyped_definition = untyped_data definition'
and stripped_definition = unit definition'
  deriving (Show)

type program = typed_data program'
and untyped_program = untyped_data program'
and stripped_program = unit program'
  deriving (Show)

let show_label =
  function
    | `T (_,_,Some lbl) -> "(label:" ^ Utility.base64encode(lbl) ^ ")"
    | _ -> "(NO LABEL)"

let labelled_string_of_expression s = show show_label s
let labelled_string_of_definition d = show_definition show_label d
let labelled_string_of_program p = show_program show_label p

let strip_data : 'a expression' -> stripped_expression =
  fun e -> Functor_expression'.map (fun _ -> ()) e

let erase : expression -> untyped_expression = 
  Functor_expression'.map (fun (`T (pos, _, _)) -> `U pos)
     
let reduce_expression (visitor : ('a expression' -> 'b) -> 'a expression' -> 'b)
    (combine : (('a expression' * 'b list) -> 'c)) : 'a expression' -> 'c =
  (* The "default" action: do nothing, just process subnodes *)
  let rec visit_children expr = 
    combine (expr, match expr with
               | Constant _
               | Nil _
               | Wrong _
               | TableQuery _
               | Variable _ -> []

               | Variant_selection_empty (e, _)
               | Abstr (_, e, _)
               | Database (e, _)
               | Variant_injection (_, e, _)
               | Project (e, _, _)
               | Erase (e, _, _)
               | List_of (e, _)
               | Call_cc(e, _)
               | Abs (e, _)
               | HasType (e, _, _) -> [visitor visit_children e]

               | TableHandle (e1, e2, _, _)
               | Comparison (e1, _, e2, _)
               | Let (_, e1, e2, _)
               | Concat (e1, e2, _)
               | For (e1, _, e2, _)
               | App (e1, e2, _)
               | SortBy (e1, e2, _) ->
                   [visitor visit_children e1; visitor visit_children e2]
                   
               | Condition (e1, e2, e3, _)
               | Variant_selection (e1, _, _, e2, _, e3, _) ->
                   [visitor visit_children e1; visitor visit_children e2; visitor visit_children e3]
               | Record_intro (bs, r, _) ->
                   (StringMap.to_list (fun _ e -> visitor visit_children e) bs) @
                     (opt_app (fun e -> [visitor visit_children e]) [] r)
               | Apply (e, es, _) -> visitor visit_children e :: map (visitor visit_children) es
               | Rec (b, e, _) -> map (fun (_, e, _) -> visitor visit_children e) b @ [visitor visit_children e]
               | Xml_node (_, es1, es2, _)          -> map (fun (_,v) -> visitor visit_children v) es1 @ map (visitor visit_children) es2)
  in
    visitor visit_children

let reduce_definition visitor combine combine_def def =
  combine_def(def,
              match def with
                | Define(name, expr, loc_annotation, d) ->
                    [reduce_expression visitor combine expr]
                | Alias _
                | Alien _ -> [])

let reduce_program visitor combine combine_def combine_program (Program (defs, body)) =
  let def_values = List.map (reduce_definition visitor combine combine_def) defs in
  let body_values = reduce_expression visitor combine body in
    combine_program (def_values, body_values)

(* This is a candidate for `deriving', I think, perhaps in conjunction with a fold *)
let set_subnodes (exp : 'a expression') (exps : 'a expression' list) : 'a expression' =
  match exp, exps with
      (* 0 subnodes *)
    | Constant _, []
    | Variable _, []
    | Nil _, [] 
    | Wrong _, [] -> exp
        
    (* 1 subnodes *)
    | Abstr (s, _, d)                , [e] -> Abstr (s, e, d)
    | Project (_, s, d)              , [e] -> Project (e, s, d)
    | Erase (_, s, d)                , [e] -> Erase (e, s, d)
    | Variant_injection (s, _, d)    , [e] -> Variant_injection (s, e, d)
    | Variant_selection_empty (_, d) , [e] -> Variant_selection_empty (e, d)
    | List_of (_, d)                 , [e] -> List_of (e, d)
    | Database (_, d)                , [e] -> Database (e, d)
    | Call_cc (_, d)                 , [e] -> Call_cc (e, d)
    | HasType (_, t, d)              , [e] -> HasType (e, t, d)
    | Abs (_, d)                     , [e] -> Abs (e, d)

    (* 2 subnodes *)
    | Comparison (_, c, _, d)                , [e1;e2] -> Comparison (e1, c, e2, d)
    | Let (s, _, _, d)                       , [e1;e2] -> Let (s, e1, e2, d)
    | Concat (_, _, d)                       , [e1;e2] -> Concat (e1, e2, d)
    | For (_, s, _, d)                       , [e1;e2] -> For (e1, s, e2, d)
    | TableHandle (_, _, t, d)               , [e1;e2] -> TableHandle (e1, e2, t, d)
    | SortBy (_, _, d)                       , [e1;e2] -> SortBy (e1, e2, d)
    | App (_, _, d)                          , [e1;e2] -> App (e1, e2, d)

    (* 3 subnodes *)
    | Condition (_, _, _, d), [e1;e2;e3] -> Condition (e1, e2, e3, d)
    | Variant_selection (_, s1, s2, _, s3, _, d), [e1;e2;e3] -> Variant_selection (e1, s1, s2, e2, s3, e3, d)

    (* n subnodes *)
    | Apply (_, _, d), e::es -> Apply (e, es, d)
    | Rec (bindings, _, d), (_::_ as nodes) ->
        let others, body = unsnoc nodes in 
          Rec (List.map2 (fun (a,_,c) b -> (a,b,c)) bindings others, body, d)
    | Record_intro (fields, None, d), nodes -> 
        let addnode k _ ((node::nodes), map) = nodes, StringMap.add k node map in
          Record_intro (snd (StringMap.fold addnode fields (nodes, StringMap.empty)), None, d)
    | Record_intro (fields, Some _, d), (_::_ as nodes) -> 
        let addnode k _ ((node::nodes), map) = nodes, StringMap.add k node map in
        let others, expr = unsnoc nodes in
          Record_intro (snd (StringMap.fold addnode fields (others, StringMap.empty)), Some expr, d)
    | Xml_node (tag, attrs, _, d), nodes -> (* (string, ((string, EXP) list), (EXP list), 'data)*)
        let nattrs = length attrs in
        let attrnodes, childnodes = take nattrs nodes, drop nattrs nodes in
          Xml_node (tag, 
                    List.map2 (fun (k,_) v -> (k,v)) attrs attrnodes,
                    childnodes,
                    d)
    | TableQuery (q, d), nodes ->
        TableQuery (q, d)
    | e -> raise (Invalid_argument "set_subnodes")
        
let rec stringlit_value = function
  | HasType (e, _, _) -> stringlit_value e
  | Constant(String name, _) -> name
  | _ -> assert false

let freevars (expression : 'a expression') : StringSet.t =
  let module S = StringSet in
  let rec aux' default v = 
    let aux = aux' default in match v with
      | Variable (name, _) -> S.add name S.empty
      | For (body, var, generator, _) -> S.union (aux generator) (S.remove var (aux body))
      | Let (var, value, body, _) -> S.union (aux value) (S.remove var (aux body))
      | Abstr (vars, body, _) -> S.diff (aux body) (S.from_list vars)
      | Variant_selection (value, _, cvar, cbody, var, body, _) ->
          S.union (aux value)
            (S.union (S.remove cvar (aux cbody))
               (S.remove var (aux body)))
      | Rec (bindings, body, _) ->
          let vars, vals = List.split (map (fun (n,v,_) -> (n,v)) bindings) in
            S.diff
              (List.fold_right (fun v set -> S.union (aux v) set) (body::vals) S.empty)
              (S.from_list vars)
      | TableQuery (query, _) ->
          SqlQuery.freevars_sqlQuery query
      | other -> default other
  in 
    reduce_expression aux' (S.union_all -<- snd) expression

let freevars_all expr = StringSet.union_all (map freevars expr)

let freevars_def def = visit_def StringSet.empty freevars def

let freevars_program (Program (defs, body)) =
  StringSet.union_all (freevars body :: List.map freevars_def defs)

let free_bound_type_vars expression =
  let module S = Types.TypeVarSet in
  let fbtv = Types.free_bound_type_vars in
  let rec fb default = function
    | HasType (e, t, _) -> S.union (fb default e) (fbtv t)
    | Rec (defs, e, _) ->
        S.union
          (List.fold_right (fun (_, e, t) xs ->
                              S.union_all
                                [fb default e; opt_app fbtv S.empty t; xs]) defs S.empty)
          (fb default e)
    | TableHandle (e1, e2, (r, w), _) ->
        S.union_all [fb default e1; fb default e2; fbtv r; fbtv w]
    | other -> default other
  in
    reduce_expression fb (S.union_all -<- snd) expression

let free_bound_type_vars_def def =
  let module S = Types.TypeVarSet in
  let fbtv = Types.free_bound_type_vars in
    match def with
      | Define (_, e, _, _) -> free_bound_type_vars e
      | Alias (_, vars, t, _) ->
          S.union (S.from_list vars) (fbtv t)
      | Alien (_, _, (vars, t), _) ->
          S.union
            (S.from_list
               (List.map (function
                            | `TypeVar var | `RigidTypeVar var | `RowVar var -> var) vars))
            (fbtv t)

let free_bound_type_vars_program (Program (defs, body))=
  let module S = Types.TypeVarSet in
  let fbtv = Types.free_bound_type_vars in
    S.union
      (S.union_all (List.map free_bound_type_vars_def defs))
      (free_bound_type_vars body)

let expression_data : ('a expression' -> 'a) = function 
  | HasType (_, _, data) -> data
  | Constant (_, data) -> data
  | Variable (_, data) -> data
  | Abs (_, data) -> data
  | App (_, _, data) -> data
  | Apply (_, _, data) -> data
  | Condition (_, _, _, data) -> data
  | Comparison (_, _, _, data) -> data
  | Abstr (_, _, data) -> data
  | Let (_, _, _, data) -> data
  | Rec (_, _, data) -> data
  | Xml_node (_, _, _, data) -> data
  | Record_intro (_, _, data) -> data
  | Project (_,_,data) -> data
  | Erase (_,_,data) -> data
  | Variant_injection (_, _, data) -> data
  | Variant_selection (_, _, _, _, _, _, data) -> data
  | Variant_selection_empty (_, data) -> data
  | Nil (data) -> data
  | List_of (_, data) -> data
  | Concat (_, _, data) -> data
  | For (_, _, _, data) -> data
  | Database (_, data) -> data
  | TableQuery (_, data) -> data
  | TableHandle (_, _, _, data) -> data
  | SortBy (_, _, data) -> data
  | Call_cc (_, data) -> data
  | Wrong data -> data
let definition_data : ('a definition' -> 'a) = function 
  | Define (_, _, _, data) -> data
  | Alias (_, _, _, data) -> data
  | Alien (_,_,_,data) -> data
let program_data : ('a program' -> 'a) =
  fun (Program (_, body)) -> expression_data body
  
(** [set_data] sets the data member of an expression to a given value;
 *)
let set_data : ('b -> 'a expression' -> 'b expression') =
  fun data -> function
    | HasType (a, b,_) ->  HasType (a, b,data) 
    | Constant (a, _) -> Constant (a, data)
    | Variable (a, _) -> Variable (a, data)
    | Abs (a,_) -> Abs (a, data)
    | App (a, b,_) -> App (a, b,data)
    | Apply (a, b,_) -> Apply (a, b,data)
    | Condition (a, b, c, _) -> Condition (a, b, c, data)
    | Comparison (a, b, c, _) -> Comparison (a, b, c, data)
    | Abstr (a, b,_) -> Abstr (a, b,data)
    | Let (a, b, c, _) -> Let (a, b, c, data) 
    | Rec (a, b,_) -> Rec (a, b,data)
    | Xml_node (a, b, c, data) ->  Xml_node (a, b, c, data)
    | Record_intro (a, b,_) -> Record_intro (a, b,data)
    | Project (a,b,_) -> Project(a,b,data)
    | Erase (a,b,_) -> Erase(a,b,data)
    | Variant_injection (a, b,_) ->  Variant_injection (a, b,data)
    | Variant_selection (a, b, c, d, e, f, _) ->
        Variant_selection (a, b, c, d, e, f, data)
    | Variant_selection_empty (a, _) -> Variant_selection_empty (a, data)
    | Nil (_) -> Nil (data)
    | List_of (a, _) -> List_of (a, data)
    | Concat (a, b,_) -> Concat (a, b,data)
    | For (a, b, c, _) -> For (a, b, c, data)
    | Database (a, _) ->  Database (a, data)
    | TableQuery (a, _) ->  TableQuery (a, data)
    | TableHandle (a, b, c, _) -> TableHandle (a, b, c, data)
    | SortBy (a, b,_) -> SortBy (a, b,data)
    | Call_cc (a, _) -> Call_cc (a, data)
    | Wrong _ -> Wrong data
let set_definition_data : ('b -> 'a definition' -> 'b definition') =
  fun data -> function
    | Define (a, b, c, _) ->  Define (a, b, c, data)
    | Alias (a, b, c, _) -> Alias (a, b, c, data)
    | Alien (a, b, c,_) -> Alien (a, b, c,data)
let set_program_data : ('b -> 'a program' -> 'b program') =
  fun data (Program (ds, body)) -> Program (ds, set_data data body)

let data_position = function
  | `T (pos, _, _)
  | `U pos -> pos

let position e = data_position (expression_data e)

let no_expr_data = `T(dummy_position, `Not_typed, None)      

let node_datatype : (expression -> Types.datatype) = (fun (`T(_, datatype, _)) -> datatype) -<- expression_data
let def_datatype : (definition -> Types.datatype) = (fun (`T(_, datatype, _)) -> datatype) -<- definition_data

let set_node_datatype : (expression * Types.datatype) -> expression =
  fun (e, t) ->
    let `T(pos, _, label) = expression_data e
    in
      set_data (`T(pos, t, label)) e

let show_pos : position -> string = 
  fun ((pos : Lexing.position), _, _) ->
    Printf.sprintf "%s:%d" pos.Lexing.pos_fname pos.Lexing.pos_lnum

module RewriteSyntax = Rewrite_expression'(struct type a = typed_data end)
module RewriteUntypedExpression = Rewrite_expression'(struct type a = untyped_data end)

(** [pure]

    Checkes whether the evaluation of an expression is known to be free
    from side effects
*)
let pure : expression -> bool = 
  (* Everything is pure except the application of certain primitive
     functions and of any functions which call those.  For now, we'll
     just punt when we see a function application.  Eventually the
     type system will help us out.

     NB: continuation invocation is impure in the sense that we can't
     replace `x = f(3); 4' with `4' if `f' is a continuation.
  *)
  let rec pure default = function 
      (* TBD: annotate ALL prim funcs as to pureness *)
    | Apply((Variable("take", _) | Variable("drop", _)), arg, _)
      -> for_all (pure default) arg
    | Apply _    -> false
    | App _       -> false
    | TableQuery _ -> false
        (* Is callCC pure? *)
    | e       -> default e
  and all_true l = fold_right (&&) l true in
    reduce_expression pure (all_true -<- snd)

(* apply a transformer (map) on expressions to a definition *)
let transform_def transformer def =
  match def with
    | Define(name, expr, loc_annotation, d) ->
        Define (name, transformer expr, loc_annotation, d)
    | Alias _
    | Alien _ -> def

let transform_program transformer (Program (defs, body)) =
  Program (List.map (transform_def transformer) defs,
           transformer body)

(* apply a rewriter on expressions to a definition *)
let rewrite_def rewriter =
  transform_def (fun expr -> fromOption expr (rewriter expr))

let rewrite_program rewriter =
  transform_program (fun expr -> fromOption expr (rewriter expr))

let rec map_free_occ u f expr =
  let recurse = map_free_occ u f in
  let rec rewrite = function
    | Variable(x, _) as node when x = u -> Some (f node)
    | Abstr(xs, body, d) when not (List.mem u xs) -> Some (Abstr(xs, recurse body, d))
    | Abstr _ -> None
    | Let(letvar, letval, body, d) ->
        Some(Let(letvar, recurse letval, 
                 (if u <> letvar then recurse body else body), d))
    | Rec(defs, body, d) when (not (mem_assoc3 u defs)) ->
        Some(Rec(map (fun (n, defn, t) -> (n, recurse defn, t)) defs, 
                 recurse body, d))
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

let subst_fast_replacer name replacement : RewriteSyntax.rewriter =
  function
    | Variable (n, _) when n = name -> Some replacement
    | _ -> None

let subst_fast name replacement expr =
  fromOption expr (RewriteSyntax.bottomup (subst_fast_replacer name replacement) expr)

let subst_fast_def name replacement =
  rewrite_def (RewriteSyntax.bottomup (subst_fast_replacer name replacement))

let rename_fast name replacement expr = 
  let replacer name replacement : RewriteSyntax.rewriter = function
    | Variable (n, d) when n = name -> Some (Variable(replacement, d))
    | TableQuery(q, data) -> 
        let q = SqlQuery.subst_sqlQuery name (`V replacement) q in
          Some(TableQuery(q, data)) 
    | _ -> None
  in
    fromOption expr (RewriteSyntax.bottomup (replacer name replacement) expr)

(** {0 Sanity Checks} *)

let is_closed expr = StringSet.is_empty (freevars expr)

let is_closed_wrt expr freebies = freevars expr <|StringSet.subset|> freebies

(** {0 Labelizing} *)

let set_label expr lbl = 
  let (`T(pos, t, _)) = expression_data expr in
    set_data (`T(pos, t, lbl)) expr

let has_label expr =
  match expression_data expr with
      (_,_,None) -> false
    | (_,_,Some _) -> true

let label_for_expr expr =
  (Digest.string -<- string_of_expression) expr

let labelize =
  rewrite_program
    (RewriteSyntax.topdown 
       (fun expr -> 
          Some(set_label expr (Some(label_for_expr expr)))))

(** {0 Utilities to construct various kinds of expressions given 
    sub-expressions} *)

let rec list_expr data = function
    [] -> Nil(data)
  | expr::etc -> Concat(List_of(expr, data), list_expr data etc, data)

let record_expr alist data = 
  let fields = StringMap.from_alist alist in
    Record_intro(fields, None, data)

(** {0 Skeleton} *)

(** [skeleton] has a case for each of the [Syntax] constructors, and
    gives an approrpiate name to each component. Use this to get 
    started on a function that takes Syntax trees by case. *)
let skeleton = function
    (* Zero sub-expressions *)
  | Nil d -> Nil d
  | Wrong d -> Wrong d
  | Constant (value, d) -> Constant (value, d)
  | Variable(x, d) -> Variable(x, d)
  | Abs (f, d) -> Abs (f, d)
  | App (f, p, d) -> App (f, p, d)
  | Apply(f, a, d) -> Apply(f, a, d)

  (* One sub-expression *)
  | Abstr(var, body, d) -> Abstr(var, body, d)
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
  | Project (expr, label, d) -> Project (expr, label, d)
  | Concat(lhs, rhs, d) -> Concat(lhs, rhs, d)
  | For(body, loop_var, src, d) -> For(body, loop_var, src, d)
  | SortBy(list_target, sort_func, d) -> SortBy(list_target, sort_func, d)
  | TableHandle(db_expr, tablename_expr, t, d) -> 
      TableHandle(db_expr, tablename_expr, t, d)
  | Call_cc(body, d) -> Call_cc(body, d)

  (* Three sub-expressions *)
  | Condition(condn, ifcase, elsecase, d) -> 
      Condition(condn, ifcase, elsecase, d)
  | Variant_selection(src_expr, case_label, case_variable, case_body, 
                      etc_var, etc_body, d) -> 
      Variant_selection(src_expr, case_label, case_variable, case_body, 
                      etc_var, etc_body, d)

  (* n-ary expressions *)
  | Record_intro(bs, r, d) -> Record_intro(bs, r, d)
  | Rec(defs, body, d) -> Rec(defs, body, d)
  | Xml_node(tagname, attrs, contents, d) -> 
      Xml_node(tagname, attrs, contents, d)
  | TableQuery(query, d) -> TableQuery(query, d)
      (* note: [query] can also contain
         expressions, in the [query.ml] sublanguage *)

let definition_skeleton = function
  | Define(name, expr, loc_annotation, d) ->
      Define(name, expr, loc_annotation, d)
  | Alien(language, name, assumption, d) -> Alien(language, name, assumption, d)
  | Alias(typename, quantifiers, datatype, d) -> Alias(typename, quantifiers, datatype, d)

let program_skeleton =
  fun (ds, body) ->
    (List.map definition_skeleton ds, skeleton body)

let read_proj = function
  | Project (record, name, _) -> Some (record, name)
  | _ -> None 

let record_selection (name, label_variable, extension_variable, scrutinee, body, data) =
  let v = gensym () in
    Let(v,
        scrutinee,
        Let(label_variable,
            Project(Variable (v, data), name, data),
            Let (extension_variable, Erase(Variable (v, data), name, data), body, data),
            data),
        data)
 
