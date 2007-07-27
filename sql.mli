val string_of_expression : Query.expression -> string
val string_of_query : Query.query -> string
val conjunction : Query.expression list -> Query.expression
val disjunction : Query.expression list -> Query.expression
val negation : Query.expression -> Query.expression
val simplify : Query.expression -> Query.expression
