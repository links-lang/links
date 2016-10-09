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
val create_type_scope_graph : Sugartypes.program -> scope_graph
val make_and_print_scope_graph : Uniquify.unique_ast -> unit

(* Name resolution *)
type name = Sugartypes.name
type fq_name = Sugartypes.name
type unique_name = Sugartypes.name


type resolution_result = [
  | `AmbiguousResolution of name list
  | `SuccessfulResolution of name
  | `UnsuccessfulResolution
]

(* Resolves a unique *declaration* name to a fully-qualified plain name *)
val make_resolved_plain_name: unique_name -> scope_graph -> Uniquify.unique_ast -> name

(* Resolves a unique *reference* name to a resolution_result, which will either
 * be a successful resolution containing a single unique *declaration* name,
 * a list of possible ambiguous resolutions (containing declaration names), or
 * an unsuccessful resolution *)
val resolve_reference: unique_name -> scope_graph -> Uniquify.unique_ast -> resolution_result

val show_scope_graph : scope_graph -> string
