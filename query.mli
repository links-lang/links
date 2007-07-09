(*pp deriving *)
type like_expr =
    [ `caret
    | `dollar
    | `underscore
    | `percent
    | `seq of like_expr list
    | `string of string
    | `variable of string ]
type table_spec = [ `TableName of string | `TableVariable of string ]
type table_instance = table_spec * string
type expression =
    Field of (string * string)
  | Variable of string
  | Null
  | Integer of Num.num
  | Float of float
  | Boolean of bool
  | LikeExpr of like_expr
  | Text of string
  | Funcall of (string * expression list)
  | Binary_op of (string * expression * expression)
  | Unary_op of (string * expression)
  | Query of query
and query = {
  distinct_only : bool;
  result_cols : col_or_expr list;
  tables : table_instance list;
  condition : expression;
  sortings : sorting list;
  max_rows : expression option;
  offset : expression;
}
and sorting = [ `Asc of string * string | `Desc of string * string ]
and column = {
  table_alias : string;
  name : string;
  col_alias : string;
  col_type : Types.datatype;
} 
and col_or_expr = (column, expression) Utility.either
deriving (Eq, Typeable, Show, Pickle, Shelve)

val like_as_string : like_expr -> string
val owning_table : string -> query -> string
val query_replace_var : string -> expression -> query -> query
val add_sorting : query -> sorting -> query
val freevars : query -> string list
val occurs_free : string -> query -> bool

(*
mini-sql that we handle here:

  SELECT [ DISTINCT * | expression [ AS output_name ] ]
    [ FROM from_item [, ...] ]
    [ WHERE condition ]
    [ ORDER BY expression [ ASC | DESC ] ]
    [ LIMIT { count } ]
    [ OFFSET start ]

where from_item is:

    table_name [ [ AS ] alias ]
*)
