
open Lwt
open Rdf.Sparql_protocol
open Rdf_sparql_http_lwt

let internal_error message =
  Errors.internal_error ~filename:"query/sparql.ml" ~message



let select ~base uri query =
  get ~base ~accept:"application/json" uri {in_query = query; in_dataset = empty_dataset} >>= fun result ->
  match result with
  | Result(Rdf.Sparql.Solutions ss) ->
      Lwt.return (List.map (fun s -> Rdf.Sparql.solution_fold (fun x t l -> (x, Rdf.Term.string_of_term t)::l) s []) ss)
  | Result(Rdf.Sparql.Bool _) -> raise (internal_error("expected SELECT query, but result is a boolean"))
  | Result(Rdf.Sparql.Graph _) -> raise (internal_error("expected SELECT query, but result is a graph"))
  | Ok -> raise (internal_error("expected SELECT query, but no result returned"))
  | Error(msg) -> raise (internal_error("SPARQL error: " ^ (Rdf.Sparql_protocol.string_of_error msg)))
