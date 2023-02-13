(*** Debugging ***)
(** Whether to turn on debug printing *)
let enabled =
  Settings.(flag "debug"
            |> synopsis "Prints internal debugging information (development)"
            |> convert parse_bool
            |> CLI.(add (short 'd' <&> long "debug"))
            |> sync)


(** print a debug message if debugging is enabled *)
let print message =
  (if Settings.get enabled then prerr_endline message; flush stderr)

(** print a debug message if debugging is enabled *)
let print_no_lf message =
  (if Settings.get enabled then prerr_string message)

(** print a debug message if debugging is enabled; [message] is a lazy expr. *)
let print_l message =
  (if Settings.get enabled then
     prerr_endline (Lazy.force message); flush stderr)

(** Print a formatted debugging message if debugging is enabled *)
let f fmt = Printf.ksprintf print fmt

(** Print a debugging message if debugging is enabled and setting is on.
    [message] is a thunk returning the string to print.
*)
let if_set setting message =
  (if Settings.get setting then print (message ()))

(* Print [message] if debugging is enabled and setting is on;
   [message] is a lazy expression *)
let if_set_l setting message =
  (if Settings.get setting then print (Lazy.force message))


(* Print [message] with time taken by evaluating f *)
let debug_time msg f =
  if Settings.get enabled
  then
    let start_time = Utility.time_microseconds() in
    let raw_result = f () in
    print (msg ^" time: " ^ string_of_int (Utility.time_microseconds() - start_time));
    raw_result
  else f ();;

let debug_time_out f (withtime : int -> unit) =
  let start_time = Utility.time_milliseconds() in
  let raw_result = f () in
  let time = Utility.time_milliseconds() - start_time in
  let _ = withtime time in
  raw_result

let eval_l e =
  if Settings.get enabled
  then ignore (Lazy.force e)

