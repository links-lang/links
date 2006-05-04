(** <form> manipulation **)
(* Is an expression a <form l:action ... > expression? *)
val islform : 'data Syntax.expression' -> bool

(* Is an expression an <input l:name ...> expression? *)
val isinput : 'data Syntax.expression' -> bool

(* Is an expression a <a l:href ... > expression? *)
val islhref : 'data Syntax.expression' -> bool

(* (\* Add an l:action attribute into an XML form expression *\) *)
(* val insert_action : 'data Syntax.expression' -> 'data Syntax.expression' -> 'data Syntax.expression' *)

(* Add an l:action attribute into an XML form expression *)
 val add_attrs : (string * 'data Syntax.expression') list -> 'data Syntax.expression' -> 'data Syntax.expression' 

(* Which variables are l:name-bound? *)
val lname_bound_vars : 'data Syntax.expression' -> string list

(* Serialise the continuation and environment, and adjust the form accordingly *)
val xml_transform : Result.environment -> (string -> Result.result) -> (Syntax.expression -> Result.continuation -> Result.result) -> Syntax.expression -> Syntax.expression

val is_special : string -> bool

(* TBD: this ought to go somewhere else *)
val serialize_exprenv : Syntax.expression -> Result.environment -> string * string

