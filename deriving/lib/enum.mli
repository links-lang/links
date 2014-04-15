type 'a enum = {
    succ : 'a -> 'a ;
    pred : 'a -> 'a ;
    to_enum : int -> 'a ;
    from_enum : 'a -> int ;
    enum_from : 'a -> 'a list ;
    enum_from_then : 'a -> 'a -> 'a list ;
    enum_from_to : 'a -> 'a -> 'a list ;
    enum_from_then_to : 'a -> 'a -> 'a -> 'a list 
}

val from_numbering : ('a * int) list -> 'a enum
val from_conversions : ('a -> int) -> (int -> 'a) -> 'a Bounded.bounded -> 'a enum

val enum_bool : bool enum
val enum_char : char enum
val enum_int  : int enum
val enum_unit : unit enum
