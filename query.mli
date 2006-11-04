(*pp deriving *)
type like_expr =
    [ `percent
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
  | Binary_op of (string * expression * expression)
  | Unary_op of (string * expression)
  | Query of query
and query = {
  distinct_only : bool;
  result_cols : column list;
  tables : table_instance list;
  condition : expression;
  sortings : sorting list;
  max_rows : expression option;
  offset : expression;
}
and sorting = [ `Asc of string * string | `Desc of string * string ]
and column = {
  table_renamed : string;
  name : string;
  renamed : string;
  col_type : Types.datatype;
} deriving (Show, Pickle)

val like_as_string : like_expr -> string
val owning_table : string -> query -> string
val query_replace_var : string -> expression -> query -> query
val get_renaming : column -> string
val add_sorting : query -> sorting -> query
val freevars : query -> string list
val occurs_free : string -> query -> bool
