open Bounded

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
  let lastCon = fst (Util.last E.numbering)

  type a = E.a
  let fromEnum a = List.assoc a E.numbering
  let toEnum i = try Util.rassoc i E.numbering with Not_found -> raise (Invalid_argument "toEnum")
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
