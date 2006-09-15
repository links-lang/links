module type S = sig
  type key
  type 'a t

  val create: unit -> 'a t
  val fold: 'a t -> (key -> 'a -> 'b -> 'b) -> 'b -> 'b
  val clear: 'a t -> unit
  val add: 'a t -> key -> 'a -> unit
  val find: 'a t -> key -> 'a
  val mem: 'a t -> key -> bool
  val remove: 'a t -> key -> unit
end

type key = int
type 'a t = 'a option array ref

let create () = ref (Array.create 16 None)

let clear t = t := Array.create 16 None

let fold t f x =
  let rec aux i x =
    if i < 0 then x 
    else
      let x = 
	match !t.(i) with
	  | Some y -> f i y x
	  | None -> x 
      in
      aux (pred i) x
  in
  aux (pred (Array.length !t)) x

let add t i x =
  let l = Array.length !t in
  if i >= l then (
    let n = max (i + 1) (l * 2) in
    let a = Array.create n None in
    Array.blit !t 0 a 0 l;
    t := a;
  );
  (!t).(i) <- Some x

let remove t i =
  if (i <= Array.length !t) then (!t).(i) <- None

let find t i =
  if i >= Array.length !t then raise Not_found
  else match (!t).(i) with
    | None -> raise Not_found
    | Some x -> x
    
let mem t i =
  if i >= Array.length !t then false
  else match (!t).(i) with
    | None -> false
    | Some _ -> true
