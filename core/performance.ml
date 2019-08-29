(* Measure performance *)
let measuring
  = Settings.(flag "measure_performance"
              |> synopsis "Instruments the server-side runtime to measure various performance characteristics"
              |> convert parse_bool
              |> CLI.(add (long "measure_performance"))
              |> sync)

let noisy_gc
  = Settings.(flag "noisy_garbage_collection"
              |> synopsis "Prints information about garbage collection cycles"
              |> convert parse_bool
              |> sync)


let notify_gc () =
  Debug.if_set noisy_gc (fun _ -> "Completing GC cycle")

let measure_diff_l obtain diff e =
  let start = obtain () in
  let result = Lazy.force e in
  let finish = obtain () in
    result, diff finish start

let statdiff (major, minor, promoted) (major', minor', promoted') =
  (* Return the total amount of memory allocated *)
  (major +. minor -. promoted) -. (major' +. minor' -. promoted')

let time_l e          = measure_diff_l Sys.time (-.) e
let measure_memory_l e = measure_diff_l Gc.counters statdiff e

let write_begin s =
  Printf.fprintf stderr "%20s : started\n" s;
  flush stderr

let write_time s t =
  Printf.fprintf stderr "%20s : %3f seconds taken\n" s t;
  flush stderr

let write_memory s t =
  Printf.fprintf stderr "%20s : %d words allocated\n" s t;
  flush stderr

let measure_l name (e) : 'b =
  if Settings.get measuring then
    let _ = write_begin name in
    let (result, time_taken), memory_allocated =
      measure_memory_l (lazy (time_l e))
    in
      write_time name time_taken;
      write_memory name (int_of_float memory_allocated);
      flush stderr;
      result
  else
    Lazy.force e

let measure name (f : 'a -> 'b) (a : 'a) : 'b =
  measure_l name (lazy (f a))

let measure_as name thunk = measure_l thunk name

let _ =
  Gc.create_alarm notify_gc
