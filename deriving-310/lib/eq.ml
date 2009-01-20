(*pp deriving *)

module type Eq =
sig
  type a
  val eq : a -> a -> bool
end

module Defaults (E : Eq) = E

module Eq_immutable(S : sig type a end) :
  Eq with type a = S.a =
struct
  type a = S.a
  let eq = (=)
end

module Eq_mutable(S : sig type a end) :
  Eq with type a = S.a =
struct
  type a = S.a
  let eq = (==)
end

module Eq_int = Eq_immutable(struct type a = int end)
module Eq_bool = Eq_immutable(struct type a = bool end)
module Eq_float = Eq_immutable(struct type a = float end)
module Eq_unit = Eq_immutable(struct type a = unit end)
module Eq_char = Eq_immutable(struct type a = char end)

module Eq_string = Eq_mutable(struct type a = string end)
module Eq_ref (E : Eq) = Eq_mutable(struct type a = E.a ref end)
module Eq_array (E : Eq) = Eq_mutable(struct type a = E.a array end)

module Eq_option (E : Eq) 
  : Eq with type a = E.a option =
struct 
  type a = E.a option
  let eq l r = match l, r with
    | None, None -> true
    | Some l, Some r -> E.eq l r
    | _ -> false
end

module Eq_map_s_t (E : Eq) (M : Map.S)
  : Eq with type a = E.a M.t =
struct
  type a = E.a M.t
  let eq = M.equal (E.eq)
end  

module Eq_set_s_t (S : Set.S)
  : Eq with type a = S.t =
struct
  type a = S.t
  let eq = S.equal
end  

module Eq_list (E : Eq) :
  Eq with type a = E.a list =
struct
  type a = E.a list
  let rec eq l r = match l, r with
    | [], [] -> true
    | (lfst::lrst), (rfst::rrst) when E.eq lfst rfst -> eq lrst rrst
    | _ -> false
end

module Eq_num
  : Eq with type a = Num.num =
struct
  type a = Num.num
  let eq = Num.eq_num
end

module Eq_6 (A1 : Eq) (A2 : Eq) (A3 : Eq) (A4 : Eq) (A5 : Eq) (A6 : Eq) 
  : Eq with type a = A1.a * A2.a * A3.a * A4.a * A5.a * A6.a =
struct
  type a = A1.a * A2.a * A3.a * A4.a * A5.a * A6.a
  let eq (l1, l2, l3, l4, l5, l6) (r1, r2, r3, r4, r5, r6) =
    A1.eq l1 r1 &&
    A2.eq l2 r2 &&
    A3.eq l3 r3 &&
    A4.eq l4 r4 &&
    A5.eq l5 r5 &&
    A6.eq l6 r6
end

module Eq_5 (A1 : Eq) (A2 : Eq) (A3 : Eq) (A4 : Eq) (A5 : Eq) 
  : Eq with type a = A1.a * A2.a * A3.a * A4.a * A5.a =
struct
  type a = A1.a * A2.a * A3.a * A4.a * A5.a
  let eq (l1, l2, l3, l4, l5) (r1, r2, r3, r4, r5) =
    A1.eq l1 r1 &&
    A2.eq l2 r2 &&
    A3.eq l3 r3 &&
    A4.eq l4 r4 &&
    A5.eq l5 r5
end

module Eq_4 (A1 : Eq) (A2 : Eq) (A3 : Eq) (A4 : Eq)
  : Eq with type a = A1.a * A2.a * A3.a * A4.a =
struct
  type a = A1.a * A2.a * A3.a * A4.a
  let eq (l1, l2, l3, l4) (r1, r2, r3, r4) =
    A1.eq l1 r1 &&
    A2.eq l2 r2 &&
    A3.eq l3 r3 &&
    A4.eq l4 r4
end

module Eq_3 (A1 : Eq) (A2 : Eq) (A3 : Eq)
  : Eq with type a = A1.a * A2.a * A3.a =
struct
  type a = A1.a * A2.a * A3.a
  let eq (l1, l2, l3) (r1, r2, r3) =
    A1.eq l1 r1 &&
    A2.eq l2 r2 &&
    A3.eq l3 r3
end

module Eq_2 (A1 : Eq) (A2 : Eq)
  : Eq with type a = A1.a * A2.a =
struct
  type a = A1.a * A2.a
  let eq (l1, l2) (r1, r2) =
    A1.eq l1 r1 &&
    A2.eq l2 r2
end
