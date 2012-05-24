type 'a eq = {
  eq : 'a -> 'a -> bool
}

let eq eq = eq.eq

let eq_immutable = { eq = (=) }
let eq_mutable = { eq = (==) }

let eq_int = eq_immutable
let eq_bool = eq_immutable
let eq_float = eq_immutable
let eq_unit = eq_immutable
let eq_char = eq_immutable

let eq_string = eq_mutable
let eq_ref _ = eq_mutable
let eq_array _ = eq_mutable

let eq_option e =
  { eq = fun l r -> match l, r with
      | None, None -> true
      | Some l, Some r -> e.eq l r
      | _ -> false }

module Eq_map_s_t (M : Map.S) =
struct
  let eq v = { eq = M.equal v.eq }
end  

module Eq_set_s_t (S : Set.S) =
struct
  let eq = {eq = S.equal }
end  

let eq_list e =
  let rec eq l r = match l, r with
    | [], [] -> true
    | (lfst::lrst), (rfst::rrst) when e.eq lfst rfst -> eq lrst rrst
    | _ -> false
  in { eq = eq }

let eq_num = { eq = Num.eq_num }

let eq_6 a1 a2 a3 a4 a5 a6 =
  {eq = (fun (l1, l2, l3, l4, l5, l6) (r1, r2, r3, r4, r5, r6) ->
           a1.eq l1 r1 &&
           a2.eq l2 r2 &&
           a3.eq l3 r3 &&
           a4.eq l4 r4 &&
           a5.eq l5 r5 &&
           a6.eq l6 r6)}

let eq_5 a1 a2 a3 a4 a5 =
  {eq = (fun (l1, l2, l3, l4, l5) (r1, r2, r3, r4, r5) ->
           a1.eq l1 r1 &&
           a2.eq l2 r2 &&
           a3.eq l3 r3 &&
           a4.eq l4 r4 &&
           a5.eq l5 r5)}

let eq_4 a1 a2 a3 a4 =
  {eq = (fun (l1, l2, l3, l4) (r1, r2, r3, r4) ->
           a1.eq l1 r1 &&
           a2.eq l2 r2 &&
           a3.eq l3 r3 &&
           a4.eq l4 r4)}

let eq_3 a1 a2 a3 =
  {eq = (fun (l1, l2, l3) (r1, r2, r3) ->
           a1.eq l1 r1 &&
           a2.eq l2 r2 &&
           a3.eq l3 r3)}

let eq_2 a1 a2 =
  {eq = (fun (l1, l2) (r1, r2) ->
           a1.eq l1 r1 &&
           a2.eq l2 r2)}
