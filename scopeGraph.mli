(* Scope graph, as per "A Theory of Name Resolution" by Neron, Tolmach, Visser,
 * and Wachmuth *)

type scope_name
type scope_graph
type scope
type declaration
type reference
type import

(* Functions on scopes and scope graphs *)
val get_scopes : scope_graph -> scope list
val get_declarations : scope -> declaration list
val get_references : scope -> reference list
val get_imports : scope -> import_list
val get_parent : scope -> scope option

(* Scope graph construction *)
val create_scope_graph : Sugartypes.program -> scope_graph
