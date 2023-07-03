
open Rdf_sparql_http_lwt
open Rdf.Sparql_protocol

let internal_error message =
  Errors.internal_error ~filename:"query/sparql.ml" ~message

let select ~base uri query =
  let result = Lwt_main.run (get ~base uri {in_query = query; in_dataset = empty_dataset}) in
  match result with
  | Result(Rdf.Sparql.Solutions _) -> 
      Value.box_list []
  | Result(Rdf.Sparql.Bool _) -> raise (internal_error("expected SELECT query, but result is a boolean"))
  | Result(Rdf.Sparql.Graph _) -> raise (internal_error("expected SELECT query, but result is a graph"))
  | Ok -> raise (internal_error("expected SELECT query, but no result returned"))
  | Error(msg) -> raise (internal_error(Rdf.Sparql_protocol.string_of_error msg))
