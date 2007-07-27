(** Contains the definition of an sql statement. *)

open Num
open List
open Query

let sorting_to_sql = function
  | `Asc (table, col)  -> table ^ "." ^ col ^ " ASC" 
  | `Desc (table, col) -> table ^ "." ^ col ^ " DESC"
  
let rec string_of_expression : expression -> string = function
  | Field (table, field)     -> table ^"."^ field
(*   | NamedField field         -> field *)
  | Variable name            -> "VARIABLE:"^ name
  | Null                     -> "NULL"
  | Integer value            -> string_of_num value
  | Float value              -> string_of_float value
  | Boolean value            -> string_of_bool value
  | LikeExpr v               -> Query.like_as_string v
  | Text value               -> "\'"^ value ^"\'"
  | Binary_op (symbol, l, r) -> "("^ string_of_expression l ^" "^ symbol ^" "^ string_of_expression r ^")"
  | Unary_op (symbol, expr)  -> "("^ symbol ^" "^ (string_of_expression expr) ^")"
  | Query query              -> "("^ string_of_query query ^")"
and string_of_query (qry:query) : string =
    "SELECT "
     ^ (match qry.result_cols with
	  | [] -> "NULL as null"
	  | _ -> (Utility.mapstrcat ", " 
                    (function 
                       | `Column col -> col.table_alias ^"."^ col.name ^" AS "^ col.col_alias
                       | `Expr(expr, alias) -> string_of_expression expr ^ " AS " ^ alias)
                    qry.result_cols))
     ^ " FROM " ^ (Utility.mapstrcat ", " 
                     (fun (table, rename) ->
                        string_of_table_spec table ^ " AS " ^ rename) qry.tables) ^
       string_of_condition qry.condition
     ^ (match qry.sortings with
	  | [] -> "" 
	  | orders -> " ORDER BY " ^ Utility.mapstrcat ", " sorting_to_sql orders)
     ^ (match qry.max_rows with
          | None   -> ""
          | Some m -> " limit " ^ string_of_expression m
	      ^ " offset " ^ string_of_expression qry.offset)
and string_of_table_spec = function
    `TableName name -> name
  | `TableVariable vrbl -> "VARIABLE:" ^ vrbl

and string_of_condition cond = match string_of_expression cond with
  | "true" -> ""
  | where  -> " WHERE " ^ where
	
(** conjunction, disjunction
    These routines should form simplified SQL expressions out of a
    list of SQL exprs.
*)

let rec conjunction = function
  | [] -> Boolean true
  | (Boolean true :: ts) -> conjunction ts
  | (Boolean false :: _) -> Boolean false
  | (t :: ts) -> match conjunction ts with
	Boolean true -> t
      | Boolean false -> Boolean false
      | rhs -> Binary_op("AND", t, rhs)

let rec disjunction = function
  | [] -> Boolean false
  | (Boolean true :: _) -> Boolean true
  | (Boolean false :: ts) -> disjunction ts
  | (t :: ts) -> match disjunction ts with
	Boolean true -> Boolean true
      | Boolean false -> t
      | rhs -> Binary_op("OR", t, rhs)

let rec negation = function
    Boolean true -> Boolean false
  | Boolean false -> Boolean true
  | Unary_op("NOT", t) -> t
  | t -> Unary_op("NOT", t)

let rec simplify = function
  | Unary_op("NOT", Unary_op("NOT", expr)) -> simplify expr
  | Binary_op("AND", lhs, Boolean true) -> simplify lhs
  | Binary_op("AND", Boolean true, rhs) -> simplify rhs
  | Binary_op("OR", lhs, Boolean false) -> simplify lhs
  | Binary_op("OR", Boolean false, rhs) -> simplify rhs
  | Binary_op("=", lhs, rhs) when lhs == rhs -> Boolean true
  | expr -> expr (* really, need to descend inside *)
