
(* Wrapper for running simple SPARQL queries against an endpoint.
 * Errors are just thrown as dynamic failures.
 * TODO: make better in almost every way.
 *)

(* select base uri query
 * - base is the base IRI according to the SPARQL protocol.
 * - uri is the URI of the SPARQL endpoint
 * - query is the SPARQL query string (unparsed/unchecked)
 *   it should be a SELECT query returning a list of bindings
 * returns: a Links value consisting of a list of association lists
 *          bound variables in the query result to their bindings
 *)

val select : base:Iri.t -> Uri.t -> string -> (string * string) list list Lwt.t
