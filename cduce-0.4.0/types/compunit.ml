(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

type t = { mutable rank: int; mutable hash: int }

let hash c = c.rank + 16 * c.hash

let equal c1 c2 = c1 == c2 || (c1.rank == c2.rank && c1.hash == c2.hash)

let compare c1 c2 =
  if (c1 == c2) then 0
  else let c = c1.rank - c2.rank in
  if c !=0 then c else
    if c1.hash < c2.hash then (-1) 
    else if c1.hash > c2.hash then (1)
    else 0
    
let pervasives = { rank = 0; hash = 0 }
let current_ref = ref pervasives
let current () = !current_ref

let enter () =
  assert(!current_ref == pervasives);
  current_ref := { rank = max_int; hash = 0 }

let leave () =
  assert(!current_ref != pervasives);
  current_ref := pervasives;
  ()

let set_hash t i1 i2 = t.rank <- i1; t.hash <- i2
let get_hash t = (t.rank,t.hash)

let wrap f x =
  enter ();
  let r = try f x with exn -> leave (); raise exn in
  leave ();
  r


let ctbl = Hashtbl.create 64

let register c s =
  if Hashtbl.mem ctbl c && Hashtbl.find ctbl c <> s then
    failwith (Printf.sprintf 
		"Collision on descriptors for CDuce units: %s and %s"
		(Hashtbl.find ctbl c) s
	     );
(*  Printf.eprintf "Register %s\n" s; *)
  Hashtbl.add ctbl c s

