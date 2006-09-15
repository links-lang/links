(*
module type T = 
sig
  include Custom.T
  type value
    
  val clear: unit -> unit  
  val mk: value -> t
  val dummy_min: t
  val dummy_max: t

  val value: t -> value
end

module Make(H : Custom.T) =
struct
  include Custom.Int
  type value = H.t

  module Tbl = Hashtbl.Make(H)

  let cache    = State.ref "Pool.cache" (Tbl.create 63)
  let values   = State.ref "Pool.values" (Array.create 63 None)
  let counter  = State.ref "Pool.counter" 0

  let clear () =
    Tbl.clear !cache;
    values := Array.create 63 None;
    counter := 0

  let check i = 
    assert((i >= 0) && (i < !counter) && (!values.(i) <> None))

  let mk x =
    try Tbl.find !cache x
    with Not_found ->
      let n = !counter in
      incr counter;
      Tbl.add !cache x n;
      if (n = Array.length !values) then
	( 
	  let new_values = Array.create (2 * Array.length !values) None in
	  Array.blit !values 0 new_values 0 n;
	  values := new_values
	);
      !values.(n) <- Some x;
      n

  let dummy_min = -1
  let dummy_max = max_int

  let value n = match !values.(n) with Some x -> x | None -> assert false

  let memo = 
    Serialize.Put.mk_property (fun t -> Array.create !counter false)

  let serialize t i =
    let memo = Serialize.Put.get_property memo t in
    Serialize.Put.int t i;
    if not memo.(i) then (
      H.serialize t (value i);
      memo.(i) <- true
    )

  (* Use an array here ? *)
  module DMemo = Map.Make(Custom.Int)
  let memo = Serialize.Get.mk_property (fun t -> ref DMemo.empty)

  let deserialize t =
    let memo = Serialize.Get.get_property memo t in
    let i = Serialize.Get.int t in
    try DMemo.find i !memo
    with Not_found ->
      let j = mk (H.deserialize t) in
      memo := DMemo.add i j !memo;
      j
      
    
end

module NoHash(H : Custom.T) =
struct
  include Custom.Int
  type value = H.t


  let values   = State.ref "Pool.values" (Array.create 63 None)
  let counter  = State.ref "Pool.counter" 0

  let clear () =
    values := Array.create 63 None;
    counter := 0

  let check i = 
    assert((i >= 0) && (i < !counter) && (!values.(i) <> None))

  let mk x =
    let n = !counter in
    incr counter;
    if (n = Array.length !values) then
      ( 
	let new_values = Array.create (2 * Array.length !values) None in
	Array.blit !values 0 new_values 0 n;
	values := new_values
      );
    !values.(n) <- Some x;
    n

  let dummy_min = -1
  let dummy_max = max_int

  let value n = match !values.(n) with Some x -> x | None -> assert false

  let memo = 
    Serialize.Put.mk_property (fun t -> Array.create !counter false)

  let serialize t i =
    let memo = Serialize.Put.get_property memo t in
    Serialize.Put.int t i;
    if not memo.(i) then (
      H.serialize t (value i);
      memo.(i) <- true
    )

  (* Use an array here ? *)
  module DMemo = Map.Make(Custom.Int)
  let memo = Serialize.Get.mk_property (fun t -> ref DMemo.empty)

  let deserialize t =
    let memo = Serialize.Get.get_property memo t in
    let i = Serialize.Get.int t in
    try DMemo.find i !memo
    with Not_found ->
      let j = mk (H.deserialize t) in
      memo := DMemo.add i j !memo;
      j
      
    
end

module Weak(H : Custom.T) = struct
  type value = H.t
  module P = Weak.Make(H)
  let pool = P.create 17

  include H
  let mk = P.merge pool
  let value x = x
end
*)
