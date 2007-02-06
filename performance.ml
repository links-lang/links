(* Measure performance *)
let measuring = Settings.add_bool("measure_performance", false, `User)

let notify_gc () = 
  Debug.print ("Completing GC cycle")

let measure_diff obtain diff f a = 
  let start = obtain () in
  let result = f a in 
  let finish = obtain () in 
    result, diff finish start

let statdiff (major, minor, promoted) (major', minor', promoted') =
  (* Return the total amount of memory allocated *)
  (major +. minor -. promoted) -. (major' +. minor' -. promoted')

let time f           = measure_diff Sys.time (-.) f
let measure_memory f = measure_diff Gc.counters statdiff f

let write_time s t = 
  Printf.fprintf stderr "%.20s : %3f seconds taken\n" s t;
  flush stderr

let write_memory s t = 
  Printf.fprintf stderr "%.20s : %d words allocated\n" s t;
  flush stderr

let measure name (f : 'a -> 'b) (a : 'a) : 'b = 
  if Settings.get_value measuring then 
    let (result, time_taken), memory_allocated = measure_memory (time f) a in
      write_time name time_taken;
      write_memory name (int_of_float memory_allocated);
      flush stderr;
      result
  else
    f a
      
let _ = 
  Gc.create_alarm notify_gc
