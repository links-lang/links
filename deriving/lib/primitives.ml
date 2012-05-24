open Bounded
open Enum
open Show

open Num

module Bool = struct
  type a = bool

  let eq l r = 
    match l, r with
      | false, false -> true
      | true,  true  -> true
      | _            -> false
          
  let le l r = 
    match l, r with 
      | true,  false -> false
      | _            -> true

  let minBound = false
  let maxBound = true

  let numbering = [false, 0; true, 1]

  let format formatter item =
    match item with
      | true  -> Format.pp_print_string formatter "true"
      | false -> Format.pp_print_string formatter "false"
end 

module Bounded_bool = (Bool : Bounded with type a = Bool.a)
module Enum_bool = EnumDefaults (Bool)
module Show_bool = ShowDefaults (Bool)

module Character = struct
  type a = char
  let eq l r = Char.compare l r = 0
  let le l r = Char.compare l r <= 0
  let minBound = Char.chr 0
  let maxBound = Char.chr 0xff (* Is this guaranteed? *)
  let fromEnum = Char.code
  let toEnum = Char.chr
  let format formatter item = Format.pp_print_string formatter ("'" ^ Char.escaped item ^ "'")
end 

module Bounded_char = (Character : Bounded with type a = Character.a)
module Enum_char = EnumDefaults' (Character) (Bounded_char)
module Show_char = ShowDefaults (Character)

module Int = struct
  type a = int
  let eq = (=)
  let le = (<=)
  let minBound = min_int
  and maxBound = max_int
  let fromEnum i = i
  let toEnum i = i
  let format formatter item = Format.pp_print_string formatter (string_of_int item)
end

module Bounded_int = (Int : Bounded with type a = Int.a)
module Enum_int = EnumDefaults' (Int) (Bounded_int)
module Show_int = ShowDefaults (Int)

module Num = struct
  type a = num
  let format formatter item = Format.pp_print_string formatter (string_of_num item)
end

module Show_num = ShowDefaults (Num)

module Float = struct
  type a = float
  let eq = (=)
  let le = (<=)
  let format formatter item = Format.pp_print_string formatter (string_of_float item)
end 

module Show_float = ShowDefaults (Float)
(* Can `instance Enum Float' be justified?
   For some floats `f' we have `succ f == f'. 
   Furthermore, float is wider than int, so fromEnum will necessarily
   give nonsense on many inputs. *)

module Strings = struct
  type a = string
  let eq l r = String.compare l r =  0
  let le l r = String.compare l r <= 0
  let format formatter item = 
    Format.pp_print_char formatter '"';
    Format.pp_print_string formatter (String.escaped item);
    Format.pp_print_char formatter '"'
end 
module Show_string = ShowDefaults (Strings)

module Unit = struct
  type a = unit
  let eq () () = true
  let le () () = true
  let minBound = ()
  let maxBound = ()
  let fromEnum () = 0
  let toEnum = function
    | 0 -> ()
    | _ -> raise (Invalid_argument "toEnum")
  let format formatter () = Format.pp_print_string formatter "()"
end 

module Bounded_unit = (Unit : Bounded with type a = Unit.a)
module Enum_unit = EnumDefaults' (Unit) (Bounded_unit)
module Show_unit = ShowDefaults (Unit)
