open Sl_syntax
open Sl_kind
open Sl_result

(** <form> manipulation **)
(* Is an expression a <form l:action ... > expression? *)
val islform : 'data expression' -> bool

(* Is an expression an <input l:name ...> expression? *)
val isinput : 'data expression' -> bool

(* Is an expression a <a l:href ... > expression? *)
val islhref : 'data expression' -> bool

(* (\* Add an l:action attribute into an XML form expression *\) *)
(* val insert_action : 'data expression' -> 'data expression' -> 'data expression' *)

(* Add an l:action attribute into an XML form expression *)
 val add_attrs : (string * 'data expression') list -> 'data expression' -> 'data expression' 

(* Which variables are l:name-bound? *)
val lname_bound_vars : 'data expression' -> string list

(* Serialise the continuation and environment, and adjust the form accordingly *)
val xml_transform : environment -> expression -> expression

(** "Runtime" services: handling continuation objects during evaluation **)

type query_record = (string * result) list

type webcontinuation = ContParams of continuation * query_record
		       | ExprEnv of expression * environment

(* Extract a continuation or expression from the parameters passed in over CGI. *)

(* Like continuation_from_params, but for continuations. *)
val cont_from_params : (*(string -> string -> string -> 
			  string -> string -> database) -> *)
  (string -> result) -> (string * string) list -> (webcontinuation) option

(** Handling remote calls (from JavaScript) **)
(* Are we being called from JavaScript? *)
val is_remote_call : (string * string) list -> bool

(* Return the information corresponding to a from-JavaScript call *)
val remote_call_info : (environment -> environment -> expression -> result) ->
                       (string * result) list ->
                       (string * ('a * kind)) list ->
                       'c ->
                       (string * string) list ->
                       (string -> 'd) ->
                       'd

val is_special : string -> bool

