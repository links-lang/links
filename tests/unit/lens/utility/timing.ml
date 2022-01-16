let time fn =
  Lens.Statistics.reset ();
  let ttime = ref 0.0 in
  let _ = Lens.Statistics.debug_time_out fn (fun time -> ttime := time) in
  (Lens.Statistics.get_query_time (), int_of_float !ttime)
