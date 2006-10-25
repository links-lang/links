(*pp deriving *)
open Num
open Utility

type like_expr = [`percent | `string of string | `variable of string | `seq of like_expr list]
    deriving (Show, Pickle)

(* Convert a like expression to a string. *)
let rec like_as_string : like_expr -> string =
  let quote = Str.global_replace (Str.regexp_string "%") "\\%" in
    function
      | `percent -> "%"
      | `string s -> quote s
      | `variable v -> "VARIABLE : " ^ v
      | `seq rs -> mapstrcat "" like_as_string rs

type table_spec = [ `TableName of string | `TableVariable of string ]
    deriving (Show, Pickle)

type table_instance = table_spec * string (* (real_name, alias) *)
    deriving (Show, Pickle)

(** A SQL expression to be used as the condition for a query. *)
type expression =
  | Field of (string (* table name (as) *) * string (* field name (real) *))
  | Variable of string
  | Null
  | Integer of num
  | Float of float
  | Boolean of bool
  | LikeExpr of like_expr
  | Text of string
  | Binary_op of (string * expression * expression)
  | Unary_op of (string * expression)
  | Query of query
      
(** query:
    A query to a database. The elements in the record type are in order: {ol
      {li True if no duplicates should be returned, false otherwise.}
      {li A set of all columns to be returned formated as (table renaming, column name, renaming, SLinks kind).
        If this list is empty, the SQL 'NULL' value will be returned for every row.}
      {li A list of tables to query formated as (table name, table renaming).}
      {li The condition to be satisfied for a row to be returned.}
      {li A list of colums to be ordered formated as `Asc (table renaming, column name) for ascending ordering,
         `Desc (table renaming, column name) for descending ordering. If the list is empty, no ordering is done.}}
    @version 1.0 *)
and query = {distinct_only : bool;
             result_cols : column list;
             tables : table_instance list;
             condition : expression;
             sortings : sorting list;
             max_rows : expression option; (* The maximum number of rows to be returned *)
             offset : expression; (* The row in the table to start from *)
            }

and sorting = [`Asc of (string * string) | `Desc of (string * string)]
and column = {table_renamed : string;
              name : string;
              renamed : string; (* TBD: call this `alias' *)
              col_type : Types.datatype}
    deriving (Show, Pickle)

(* Simple accessors *)
let get_renaming col = col.renamed

let add_sorting query col = 
  {query with
     sortings = col :: query.sortings}

let owning_table of_col qry =
  let col_rec = (List.find (fun c -> c.name = of_col) qry.result_cols) in
    col_rec.table_renamed

let rec freevars {condition = condition;
                  offset = offset;
                  max_rows = max_rows} =
  qexpr_freevars condition @ qexpr_freevars offset @ fromOption [] (opt_map qexpr_freevars max_rows)
and qexpr_freevars = function
    Variable name -> [name]
  | Binary_op (_, lhs, rhs) -> qexpr_freevars lhs @ qexpr_freevars rhs
  | Unary_op (_, arg) -> qexpr_freevars arg
  | Query q -> freevars q
  | _ -> []

let rec replace_var name expr = function
  | Variable var when var = name -> expr
  | Binary_op(op, lhs, rhs) -> Binary_op(op, replace_var name expr lhs,
                                         replace_var name expr rhs)
  | Unary_op(op, arg) -> Unary_op(op, replace_var name expr arg)
  | Query query -> Query (query_replace_var name expr query)
  | x -> x
and query_replace_var var expr query =
  {query with condition = replace_var var expr query.condition}

let occurs_free var q = List.mem var (freevars q)
