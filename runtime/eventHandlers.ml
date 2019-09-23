let event_handlers = (Hashtbl.create 10000 : (int, Value.t) Hashtbl.t)

(* generate a unique key for each block of HTML containing event
   handlers *)
let fresh_key =
  let current_key = (ref 0 : int ref) in
  fun () ->
    begin
      incr current_key;
      !current_key
    end

let register hs =
  let key = fresh_key() in
  Hashtbl.add event_handlers key hs;
  key

let find key = Hashtbl.find event_handlers key
