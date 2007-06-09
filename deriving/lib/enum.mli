module type Enum =
  sig
    type a
    val succ : a -> a
    val pred : a -> a
    val toEnum : int -> a
    val fromEnum : a -> int
    val enumFrom : a -> a list
    val enumFromThen : a -> a -> a list
    val enumFromTo : a -> a -> a list
    val enumFromThenTo : a -> a -> a -> a list
  end

module EnumDefaults
  (E : sig type a val numbering : (a * int) list end)
  : Enum with type a = E.a

module EnumDefaults' 
  (E : sig type a val fromEnum : a -> int val toEnum : int -> a end) 
  (B : Bounded.Bounded with type a = E.a)
  : Enum with type a = B.a

module Enum_bool : Enum with type a = bool
module Enum_char : Enum with type a = char
module Enum_int  : Enum with type a = int
module Enum_unit : Enum with type a = unit
