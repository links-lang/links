module type Bounded = 
sig
  type a
  val minBound : a 
  val maxBound : a 
end

module Bounded_2 (B1 : Bounded) (B2 : Bounded) 
 : Bounded with type a = B1.a * B2.a
module Bounded_3 (B1 : Bounded) (B2 : Bounded) (B3 : Bounded) 
 : Bounded with type a = B1.a * B2.a * B3.a
module Bounded_4 (B1 : Bounded) (B2 : Bounded) (B3 : Bounded) (B4 : Bounded)
 : Bounded with type a = B1.a * B2.a * B3.a * B4.a
module Bounded_5 (B1 : Bounded) (B2 : Bounded) (B3 : Bounded) (B4 : Bounded) (B5 : Bounded)
 : Bounded with type a = B1.a * B2.a * B3.a * B4.a * B5.a
module Bounded_6 (B1 : Bounded) (B2 : Bounded) (B3 : Bounded) (B4 : Bounded) (B5 : Bounded) (B6 : Bounded)
 : Bounded with type a = B1.a * B2.a * B3.a * B4.a * B5.a * B6.a
module Bounded_7 (B1 : Bounded) (B2 : Bounded) (B3 : Bounded) (B4 : Bounded) (B5 : Bounded) (B6 : Bounded) (B7 : Bounded)
 : Bounded with type a = B1.a * B2.a * B3.a * B4.a * B5.a * B6.a * B7.a
module Bounded_8 (B1 : Bounded) (B2 : Bounded) (B3 : Bounded) (B4 : Bounded) (B5 : Bounded) (B6 : Bounded) (B7 : Bounded) (B8 : Bounded)
 : Bounded with type a = B1.a * B2.a * B3.a * B4.a * B5.a * B6.a * B7.a * B8.a
module Bounded_9 (B1 : Bounded) (B2 : Bounded) (B3 : Bounded) (B4 : Bounded) (B5 : Bounded) (B6 : Bounded) (B7 : Bounded) (B8 : Bounded) (B9 : Bounded)
 : Bounded with type a = B1.a * B2.a * B3.a * B4.a * B5.a * B6.a * B7.a * B8.a * B9.a
