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

let from_numbering (numbering : ('a * int) list) : 'a enum =
  let firstCon = fst (List.hd numbering) in
  let lastCon = fst (last numbering) in
  let from_enum a = List.assoc a numbering in
  let to_enum i = try rassoc i numbering with Not_found -> raise (Invalid_argument "to_enum") in
  let enum_from_then_to x y z = List.map to_enum (startThenTo (from_enum x) (from_enum y) (from_enum z)) in
  let enum_from_to x y = List.map to_enum (range (from_enum x) (from_enum y)) in
    { from_enum = from_enum ;
      to_enum = to_enum ;
      succ = (fun s -> try to_enum ((from_enum s) + 1) with Invalid_argument "to_enum" -> raise (Invalid_argument "succ")) ;
      pred = (fun s -> try to_enum ((from_enum s) - 1) with Invalid_argument "to_enum" -> raise (Invalid_argument "pred")) ;
      enum_from_then_to = enum_from_then_to ;
      enum_from_to = enum_from_to ;
      enum_from_then = (fun x y -> (enum_from_then_to x y 
                                      (if from_enum y >= from_enum x then lastCon
                                       else firstCon))) ;
      enum_from = (fun x -> enum_from_to x lastCon)
    }

let from_conversions (from_enum : 'a -> int) (to_enum   : int -> 'a) (b : 'a bounded) : 'a enum =
  let firstCon = b.min_bound in
  let lastCon = b.max_bound in
  let enum_from_then_to x y z = List.map to_enum (startThenTo (from_enum x) (from_enum y) (from_enum z)) in
  let enum_from_to x y = List.map to_enum (range (from_enum x) (from_enum y)) in
    {
      from_enum = from_enum ;
      to_enum = to_enum ;
      succ = (fun s -> try to_enum ((from_enum s) + 1) with Invalid_argument "to_enum" -> raise (Invalid_argument "succ")) ;
      pred = (fun s -> try to_enum ((from_enum s) - 1) with Invalid_argument "to_enum" -> raise (Invalid_argument "pred")) ;
      enum_from_then_to = enum_from_then_to ;
      enum_from_to = enum_from_to ;
      enum_from_then = (fun x y -> (enum_from_then_to x y 
                                      (if from_enum y >= from_enum x then lastCon
                                       else firstCon))) ;
      enum_from = (fun x -> enum_from_to x lastCon) 
    }

let enum_bool = from_numbering [false, 0; true, 1]
let enum_char = from_conversions Char.code Char.chr bounded_char
let enum_int = from_conversions (fun x -> x) (fun x -> x) bounded_int

(* Can `instance Enum Float' be justified?
   For some floats `f' we have `succ f == f'. 
   Furthermore, float is wider than int, so from_enum will necessarily
   give nonsense on many inputs. *)

let enum_unit = from_conversions (fun () -> 0) 
  (function
     | 0 -> () 
     | _ -> raise (Invalid_argument "to_enum"))
  bounded_unit

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
