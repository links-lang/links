(* Scope graph, as per "A Theory of Name Resolution" by Neron, Tolmach, Visser,
 * and Wachmuth *)

type scope_name
type scope_graph
type scope
type declaration
type reference
type import

(* Scope graph construction *)
val create_scope_graph : Sugartypes.program -> scope_graph
val make_and_print_scope_graph : Uniquify.unique_ast -> unit

(* Name resolution *)
type name = Sugartypes.name
type fq_name = Sugartypes.name
type unique_name = Sugartypes.name

(* Given a unique declaration name and a scope graph, returns the
 * fully-qualified name of the declaration (or None if it doesn't exist) *)
(* val get_fq_decl_name : unique_name -> scope_graph -> fq_name option *)

(* Given a unique reference name and a scope graph, returns a list of possible
 * fully-qualified (plain) names *)
(* val resolve_fq_name : unique_name -> scope_graph -> fq_name list *)
