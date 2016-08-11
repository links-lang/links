open Printf

module type STATE = sig
  type t
  val put : t -> unit
  val get : unit -> t
  val run : (unit -> 'a) -> init:t -> t
end

module State (S : sig type t end) : STATE with type t = S.t = struct

  type t = S.t

  effect Put : t -> unit
  let put v = perform (Put v)

  effect Get : t
  let get () = perform Get

  let run f ~init =
    let comp =
      match f () with
      | v -> (fun s -> s)
      | effect (Put s') k -> (fun s -> continue k () s')
      | effect Get k -> (fun s -> continue k s s)
    in comp init
end

module IS = State (struct type t = int end)

let rec count () =
  let i = IS.get () in
  if i == 0 then i
  else let _ = IS.put (i - 1) in count ()

let b n =
  let x = IS.run count n in
  x
                             
let _ = print_endline (string_of_int (b 10000000))
