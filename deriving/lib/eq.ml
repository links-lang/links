
module type Eq =
sig
  type a
  val eq : a -> a -> bool
end

module Eq_defaults (E : Eq) = E

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
  let eq = (=)
end

module Eq_int = Eq_immutable(struct type a = int end)
module Eq_bool = Eq_immutable(struct type a = bool end)
module Eq_float = Eq_immutable(struct type a = float end)
module Eq_unit = Eq_immutable(struct type a = unit end)
module Eq_char = Eq_immutable(struct type a = char end)

module Eq_string = Eq_mutable(struct type a = string end)
module Eq_ref (E : Eq) = Eq_mutable(struct type a = E.a ref end)
module Eq_array (E : Eq) = Eq_mutable(struct type a = E.a array end)

module Eq_list (E : Eq) :
  Eq with type a = E.a list =
struct
  type a = E.a list
  let eq = List.for_all2 E.eq
end

module Eq_2 (E1 : Eq) (E2 : Eq) :
  Eq with type a = E1.a * E2.a =
struct
  type a = E1.a * E2.a
  let eq (l1,l2) (r1,r2) =
       E1.eq l1 r1 
    && E2.eq l2 r2 
end
module Eq_3 (E1 : Eq) (E2 : Eq) (E3 : Eq) :
  Eq with type a = E1.a * E2.a * E3.a =
struct
  type a = E1.a * E2.a * E3.a
  let eq (l1,l2,l3) (r1,r2,r3) =
       E1.eq l1 r1 
    && E2.eq l2 r2 
    && E3.eq l3 r3 
end
module Eq_4 (E1 : Eq) (E2 : Eq) (E3 : Eq) (E4 : Eq) :
  Eq with type a = E1.a * E2.a * E3.a * E4.a =
struct
  type a = E1.a * E2.a * E3.a * E4.a
  let eq (l1,l2,l3,l4) (r1,r2,r3,r4) =
       E1.eq l1 r1 
    && E2.eq l2 r2 
    && E3.eq l3 r3 
    && E4.eq l4 r4 
end
module Eq_5 (E1 : Eq) (E2 : Eq) (E3 : Eq) (E4 : Eq) (E5 : Eq) :
  Eq with type a = E1.a * E2.a * E3.a * E4.a * E5.a =
struct
  type a = E1.a * E2.a * E3.a * E4.a * E5.a
  let eq (l1,l2,l3,l4,l5) (r1,r2,r3,r4,r5) =
       E1.eq l1 r1 
    && E2.eq l2 r2 
    && E3.eq l3 r3 
    && E4.eq l4 r4 
    && E5.eq l5 r5 
end
module Eq_6 (E1 : Eq) (E2 : Eq) (E3 : Eq) (E4 : Eq) (E5 : Eq) (E6 : Eq) :
  Eq with type a = E1.a * E2.a * E3.a * E4.a * E5.a * E6.a =
struct
  type a = E1.a * E2.a * E3.a * E4.a * E5.a * E6.a
  let eq (l1,l2,l3,l4,l5,l6) (r1,r2,r3,r4,r5,r6) =
       E1.eq l1 r1 
    && E2.eq l2 r2 
    && E3.eq l3 r3 
    && E4.eq l4 r4 
    && E5.eq l5 r5 
    && E6.eq l6 r6 
end
module Eq_7 (E1 : Eq) (E2 : Eq) (E3 : Eq) (E4 : Eq) (E5 : Eq) (E6 : Eq) (E7 : Eq) :
  Eq with type a = E1.a * E2.a * E3.a * E4.a * E5.a * E6.a * E7.a =
struct
  type a = E1.a * E2.a * E3.a * E4.a * E5.a * E6.a * E7.a
  let eq (l1,l2,l3,l4,l5,l6,l7) (r1,r2,r3,r4,r5,r6,r7) =
       E1.eq l1 r1 
    && E2.eq l2 r2 
    && E3.eq l3 r3 
    && E4.eq l4 r4 
    && E5.eq l5 r5 
    && E6.eq l6 r6 
    && E7.eq l7 r7 
end
module Eq_8 (E1 : Eq) (E2 : Eq) (E3 : Eq) (E4 : Eq) (E5 : Eq) (E6 : Eq) (E7 : Eq) (E8 : Eq) :
  Eq with type a = E1.a * E2.a * E3.a * E4.a * E5.a * E6.a * E7.a * E8.a =
struct
  type a = E1.a * E2.a * E3.a * E4.a * E5.a * E6.a * E7.a * E8.a
  let eq (l1,l2,l3,l4,l5,l6,l7,l8) (r1,r2,r3,r4,r5,r6,r7,r8) =
       E1.eq l1 r1 
    && E2.eq l2 r2 
    && E3.eq l3 r3 
    && E4.eq l4 r4 
    && E5.eq l5 r5 
    && E6.eq l6 r6 
    && E7.eq l7 r7 
    && E8.eq l8 r8 
end
module Eq_9 (E1 : Eq) (E2 : Eq) (E3 : Eq) (E4 : Eq) (E5 : Eq) (E6 : Eq) (E7 : Eq) (E8 : Eq) (E9 : Eq) :
  Eq with type a = E1.a * E2.a * E3.a * E4.a * E5.a * E6.a * E7.a * E8.a * E9.a =
struct
  type a = E1.a * E2.a * E3.a * E4.a * E5.a * E6.a * E7.a * E8.a * E9.a
  let eq (l1,l2,l3,l4,l5,l6,l7,l8,l9) (r1,r2,r3,r4,r5,r6,r7,r8,r9) =
       E1.eq l1 r1 
    && E2.eq l2 r2 
    && E3.eq l3 r3 
    && E4.eq l4 r4 
    && E5.eq l5 r5 
    && E6.eq l6 r6 
    && E7.eq l7 r7 
    && E8.eq l8 r8 
    && E9.eq l9 r9 
end

module Eq_num
  : Eq with type a = Num.num =
struct
  type a = Num.num
  let eq = Num.eq_num
end

