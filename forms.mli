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
val xml_transform : Result.environment -> (Syntax.expression -> Result.continuation -> Result.result) -> Syntax.expression -> Syntax.expression

(** "Runtime" services: handling continuation objects during evaluation **)

type query_record = (string * Result.result) list

type webcontinuation = ContParams of Result.continuation * query_record
		       | ExprEnv of Syntax.expression * Result.environment

(* Extract a continuation or expression from the parameters passed in over CGI. *)
val cont_from_params : (*(string -> string -> string -> 
			  string -> string -> database) -> *)
  (string -> Result.result) -> (string * string) list -> (webcontinuation) option

(** Handling remote calls (from JavaScript) **)
(* Are we being called from JavaScript? *)
val is_remote_call : (string * string) list -> bool

val is_special : string -> bool

(* TBD: this ought to go somewhere else *)
val serialize_exprenv : Syntax.expression -> Result.environment -> string * string

