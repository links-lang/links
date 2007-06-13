(*pp deriving *)
open Bounded

let rec rassoc (rkey : 'b) : ('a * 'b) list -> 'a = function
  | []                     -> raise Not_found
  | (a,b)::_ when b = rkey -> a
  | _::xs                  -> rassoc rkey xs

let rec last : 'a list -> 'a = function
    | []    -> raise (Invalid_argument "last")
    | [x]   -> x
    | _::xs -> last xs

module Enum =
struct
(** Enum **)
module type Enum = sig
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

let startThenTo (start : int) (next : int) (until : int) : int list = 
  let step = next - start in
    if step <= 0 then invalid_arg "startThenTo" 
    else
      let rec upFrom current =
        if current > until then []
        else current :: upFrom (current+step)
      in
        upFrom start

let range : int -> int -> int list 
  = fun f t -> startThenTo f (f+1) t

module EnumDefaults 
  (E : (sig
          type a
          val numbering : (a * int) list
        end)) : Enum with type a = E.a =
struct
  let firstCon = fst (List.hd E.numbering)
  let lastCon = fst (last E.numbering)

  type a = E.a
  let fromEnum a = List.assoc a E.numbering
  let toEnum i = try rassoc i E.numbering with Not_found -> raise (Invalid_argument "toEnum")
  let succ s = try toEnum ((fromEnum s) + 1) with Invalid_argument "toEnum" -> raise (Invalid_argument "succ")
  let pred s = try toEnum ((fromEnum s) - 1) with Invalid_argument "toEnum" -> raise (Invalid_argument "pred")
  let enumFromTo x y = List.map toEnum (range (fromEnum x) (fromEnum y))
  let enumFromThenTo x y z = List.map toEnum (startThenTo (fromEnum x) (fromEnum y) (fromEnum z))
  let enumFromThen x y = (enumFromThenTo x y 
                            (if fromEnum y >= fromEnum x then lastCon
                             else firstCon))
  let enumFrom x = enumFromTo x lastCon
end


module EnumDefaults' 
  (E : (sig
          type a
          val fromEnum : a -> int
          val toEnum   : int -> a
        end))
  (B : Bounded with type a = E.a) : Enum with type a = E.a 
                                         and  type a = B.a =
struct
  include E
  let firstCon = B.minBound
  let lastCon = B.maxBound

  let succ s = try toEnum ((fromEnum s) + 1) with Invalid_argument "toEnum" -> raise (Invalid_argument "succ")
  let pred s = try toEnum ((fromEnum s) - 1) with Invalid_argument "toEnum" -> raise (Invalid_argument "pred")
  let enumFromTo x y = List.map toEnum (range (fromEnum x) (fromEnum y))
  let enumFromThenTo x y z = List.map toEnum (startThenTo (fromEnum x) (fromEnum y) (fromEnum z))
  let enumFromThen x y = (enumFromThenTo x y 
                            (if fromEnum y >= fromEnum x then lastCon
                             else firstCon))
  let enumFrom x = enumFromTo x lastCon
end

module Enum_bool = EnumDefaults(struct
  type a = bool
  let numbering = [false, 0; true, 1]
end)

module Enum_char = EnumDefaults'(struct
  type a = char
  let fromEnum = Char.code
  let toEnum = Char.chr
end) (Bounded_char)

module Enum_int = EnumDefaults' (struct
  type a = int
  let fromEnum i = i
  let toEnum i = i
end)(Bounded_int)

(* Can `instance Enum Float' be justified?
   For some floats `f' we have `succ f == f'. 
   Furthermore, float is wider than int, so fromEnum will necessarily
   give nonsense on many inputs. *)

module Enum_unit = EnumDefaults' (struct
  type a = unit
  let fromEnum () = 0
  let toEnum = function
    | 0 -> ()
    | _ -> raise (Invalid_argument "toEnum")
end) (Bounded_unit)
end
include Enum

type open_flag = Pervasives.open_flag  =
                 | Open_rdonly
                 | Open_wronly
                 | Open_append
                 | Open_creat
                 | Open_trunc
                 | Open_excl
                 | Open_binary
                 | Open_text
                 | Open_nonblock
                     deriving (Enum)

type fpclass = Pervasives.fpclass =
               | FP_normal
               | FP_subnormal
               | FP_zero
               | FP_infinite
               | FP_nan
                   deriving (Enum)
