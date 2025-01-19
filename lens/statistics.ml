(** Benchmarking helpers *)
let query_timer = ref 0.0

let query_count = ref 0

let time_milliseconds () = Unix.gettimeofday () *. 1000.0

let debug_time_out f (withtime : float -> unit) =
  let start_time = time_milliseconds () in
  let raw_result = f () in
  let time = time_milliseconds () -. start_time in
  let _ = withtime time in
  raw_result

let add_query time =
  query_timer := !query_timer +. time;
  query_count := !query_count + 1

let time_query fn = debug_time_out fn add_query

let reset () =
  query_count := 0;
  query_timer := 0.0

let get_query_time () = int_of_float !query_timer

let get_query_count () = !query_count
