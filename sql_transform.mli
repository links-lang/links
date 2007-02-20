type projection_source = {
  field_name : string;
  field_var : string;
  etc_var : string;
  source_var : string;
}

type binding =
    [ `Calculated of string * Syntax.expression
    | `Selected of projection_source
    | `Table_loop of string * Query.query
    | `Unavailable of string ]
type bindings = binding list
type origin =
    [ `Earlier of string * projection_source list
    | `Table of Query.query
    | `Table_field of string * string
    | `Unavailable ]

exception ColumnNotInQuery of string

val like_as_string : (string * Result.result) list -> Query.like_expr -> string
val condition_to_sql :  Syntax.expression ->  bindings -> (Query.expression * projection_source list) option
val null_query : Query.query -> Query.query
val project : string list -> Query.query -> Query.query
val select : Query.expression list -> Query.query -> Query.query
val select_by_origin :  projection_source list -> Syntax.expression -> Syntax.expression
val trace_variable : string -> bindings -> origin
val sep_assgmts :  bindings -> Syntax.expression -> bindings * Syntax.expression
val join : Query.expression list -> Query.query * Query.query -> (Query.column * string) list * Query.query
val pos_and_neg : Query.expression list * Query.expression list -> Query.expression
