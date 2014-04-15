open Eq

exception Done of int

type state = {
  mutable code : int ;
  mutable size : int ;
}

let default_depth = 5

let initial_state depth : state = {
  code = 0;
  size = depth
}

type 'a hash = {
  hash : 'a -> state -> state ;
  _Eq  : 'a eq
}

let alpha = 65599

let combine n hash_accu = hash_accu * alpha + n

let merge value state = 
  if state.size > 0 then {
    code = combine value state.code;
    size = state.size - 1
  }
  else raise (Done state.code)

let primitive_hash : 'a -> state -> state
  = fun v -> merge (Hashtbl.hash v)

let hash hash ?(depth=default_depth) v = 
  let s = initial_state depth in
    try hash.hash v s; s.code
    with Done code -> code

let eq hash = hash._Eq.Eq.eq

let hash_int = { hash = primitive_hash ; _Eq = eq_int }
let hash_bool = { hash = primitive_hash ; _Eq = eq_bool }
let hash_float = { hash = primitive_hash ; _Eq = eq_float }
let hash_unit = { hash = primitive_hash ; _Eq = eq_unit }
let hash_char = { hash = primitive_hash ; _Eq = eq_char }
let hash_string = { hash = primitive_hash ; _Eq = eq_string }

let hash_ref h = { hash = (fun v state -> h.hash (!v) state) ; _Eq = eq_ref h._Eq }
let hash_array h = {
  hash = Array.fold_right h.hash;
  _Eq = eq_array h._Eq }


let hash_option e =
  { hash = (fun v state -> match v with
              | None -> merge 1 state
              | Some v -> e.hash v (merge 2 state));
    _Eq = eq_option e._Eq }

module Hash_map_s_t (M : Map.S) =
struct
  let hash v = assert false (* TODO *)
end  

module Hash_set_s_t (S : Set.S) =
struct
  let hash = assert false (* TODO *)
end  

let rec hash_list h = 
  { hash = (fun l state -> match l with
              | [] -> merge 1 state
              | x::xs -> (hash_list h).hash xs (h.hash x (merge 2 state)));
    _Eq = eq_list h._Eq }

let hash_num = { hash = primitive_hash ; _Eq = eq_num }

let hash_6 a1 a2 a3 a4 a5 a6 =
  {hash = (fun (l1, l2, l3, l4, l5, l6) state -> 
             a1.hash l1
               (a2.hash l2
                  (a3.hash l3
                     (a4.hash l4
                        (a5.hash l5
                           (a6.hash l6 state))))));
   _Eq = eq_6 a1._Eq a2._Eq a3._Eq a4._Eq a5._Eq a6._Eq }

let hash_5 a1 a2 a3 a4 a5 =
  {hash = (fun (l1, l2, l3, l4, l5) state -> 
             a1.hash l1
               (a2.hash l2
                  (a3.hash l3
                     (a4.hash l4
                        (a5.hash l5
                           state)))));
   _Eq = eq_5 a1._Eq a2._Eq a3._Eq a4._Eq a5._Eq }

let hash_4 a1 a2 a3 a4 =
  {hash = (fun (l1, l2, l3, l4) state -> 
             a1.hash l1
               (a2.hash l2
                  (a3.hash l3
                     (a4.hash l4
                        state))));
   _Eq = eq_4 a1._Eq a2._Eq a3._Eq a4._Eq }

let hash_3 a1 a2 a3 =
  {hash = (fun (l1, l2, l3) state -> 
             a1.hash l1
               (a2.hash l2
                  (a3.hash l3
                     state)));
   _Eq = eq_3 a1._Eq a2._Eq a3._Eq }

let hash_2 a1 a2 =
  {hash = (fun (l1, l2) state -> 
             a1.hash l1
               (a2.hash l2
                  state));
   _Eq = eq_2 a1._Eq a2._Eq }
