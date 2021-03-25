type 'a point = 'a UnionFind.elem

let fresh a = UnionFind.make a 
let find p = UnionFind.get p
let change a b = UnionFind.set a b
let equivalent p1 p2 = UnionFind.eq p1 p2
let union x y =
  let _ = UnionFind.union x y in ()

(* Prints the address of the representative point in order to debug sharing
   effects *)
let pp_point pf formatter p =
  let address_of (x:'a) : nativeint =
  if Obj.is_block (Obj.repr x) then
    Nativeint.shift_left (Nativeint.of_int (Obj.magic x)) 1
  else
    invalid_arg "Can only find address of boxed values." in
  let repr_elt = UnionFind.find p in
  let descriptor = UnionFind.get p in
  let addr_of_repr  = address_of repr_elt in
  Format.pp_print_string formatter  (Printf.sprintf "Point @ 0x%nx " addr_of_repr);
  pf formatter descriptor

let show_point f v = Format.asprintf "%a" (pp_point f) v
