open Eq

exception Done of int

type state = {
  mutable code : int ;
  mutable size : int
}

let default_depth = 5

let initial_state depth : state = {
  code = 0;
  size = depth
}

type 'a hash = {
  hash : state -> 'a -> unit ;
  _Eq  : 'a eq
}

let alpha = 65599

let combine n hash_accu = hash_accu * alpha + n

let (<!>) state value = 
  if state.size > 0 then
    begin
      state.code <- combine value state.code;
      state.size <- state.size - 1
    end
  else raise (Done state.code)

let primitive_hash : state -> 'a -> unit
  = fun state v -> state <!> Hashtbl.hash v

let hash hash ?(depth=default_depth) v = 
  let s = initial_state depth in
    try hash.hash s v; s.code
    with Done code -> code

let eq hash = hash._Eq.Eq.eq

let hash_int = { hash = primitive_hash ; _Eq = eq_int }
let hash_bool = { hash = primitive_hash ; _Eq = eq_bool }
let hash_float = { hash = primitive_hash ; _Eq = eq_float }
let hash_unit = { hash = primitive_hash ; _Eq = eq_unit }
let hash_char = { hash = primitive_hash ; _Eq = eq_char }
let hash_string = { hash = primitive_hash ; _Eq = eq_string }

let hash_ref h = { hash = (fun state v -> h.hash state (!v)) ; _Eq = eq_ref h._Eq }
let hash_array h = {
  hash = (fun state array -> 
            for i = 0 to Array.length array - 1 do
              h.hash state array.(i)
            done);
  _Eq = eq_array h._Eq }


let hash_option e =
  { hash = (fun state v -> match v with
              | None -> state <!> 1
              | Some v -> state <!> 2; e.hash state v);
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
  { hash = (fun state l -> match l with
              | [] -> state <!> 1
              | x::xs -> (state <!> 2; h.hash state x ; (hash_list h).hash state xs));
    _Eq = eq_list h._Eq }

let hash_num = { hash = primitive_hash ; _Eq = eq_num }

let hash_6 a1 a2 a3 a4 a5 a6 =
  {hash = (fun state (l1, l2, l3, l4, l5, l6) -> 
             a1.hash state l1;
             a2.hash state l2;
             a3.hash state l3;
             a4.hash state l4;
             a5.hash state l5;
             a6.hash state l6);
   _Eq = eq_6 a1._Eq a2._Eq a3._Eq a4._Eq a5._Eq a6._Eq }

let hash_5 a1 a2 a3 a4 a5 =
  {hash = (fun state (l1, l2, l3, l4, l5) -> 
             a1.hash state l1;
             a2.hash state l2;
             a3.hash state l3;
             a4.hash state l4;
             a5.hash state l5);
   _Eq = eq_5 a1._Eq a2._Eq a3._Eq a4._Eq a5._Eq }

let hash_4 a1 a2 a3 a4 =
  {hash = (fun state (l1, l2, l3, l4) -> 
             a1.hash state l1;
             a2.hash state l2;
             a3.hash state l3;
             a4.hash state l4);
   _Eq = eq_4 a1._Eq a2._Eq a3._Eq a4._Eq }

let hash_3 a1 a2 a3 =
  {hash = (fun state (l1, l2, l3) -> 
             a1.hash state l1;
             a2.hash state l2;
             a3.hash state l3);
   _Eq = eq_3 a1._Eq a2._Eq a3._Eq }

let hash_2 a1 a2 =
  {hash = (fun state (l1, l2) -> 
             a1.hash state l1;
             a2.hash state l2);
   _Eq = eq_2 a1._Eq a2._Eq }
