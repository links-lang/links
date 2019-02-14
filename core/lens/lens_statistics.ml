(** Benchmarking helpers *)
let query_timer = ref 0
let query_count = ref 0

let add_query time =
  query_timer := !query_timer + time;
  query_count := !query_count + 1

let time_query fn =
  Debug.debug_time_out fn add_query

let reset () =
  query_count := 0;
  query_timer := 0

let get_query_time () = !query_timer

let get_query_count () = !query_count
