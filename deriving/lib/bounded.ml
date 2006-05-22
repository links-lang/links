(** Bounded **)
module type Bounded = sig
  type a
  val minBound : a
  val maxBound : a
end

module Bounded_2
  (B1 : Bounded)
  (B2 : Bounded)
  : Bounded with type a = B1.a * B2.a =
struct
  type a = B1.a * B2.a
  let minBound = B1.minBound, B2.minBound
  let maxBound = B1.maxBound, B2.maxBound
end
module Bounded_3
  (B1 : Bounded)
  (B2 : Bounded)
  (B3 : Bounded)
  : Bounded with type a = B1.a * B2.a * B3.a =
struct
  type a = B1.a * B2.a * B3.a
  let minBound = B1.minBound, B2.minBound, B3.minBound
  let maxBound = B1.maxBound, B2.maxBound, B3.maxBound
end
module Bounded_4
  (B1 : Bounded)
  (B2 : Bounded)
  (B3 : Bounded)
  (B4 : Bounded)
  : Bounded with type a = B1.a * B2.a * B3.a * B4.a =
struct
  type a = B1.a * B2.a * B3.a * B4.a
  let minBound = B1.minBound, B2.minBound, B3.minBound, B4.minBound
  let maxBound = B1.maxBound, B2.maxBound, B3.maxBound, B4.maxBound
end
module Bounded_5
  (B1 : Bounded)
  (B2 : Bounded)
  (B3 : Bounded)
  (B4 : Bounded)
  (B5 : Bounded)
  : Bounded with type a = B1.a * B2.a * B3.a * B4.a * B5.a =
struct
  type a = B1.a * B2.a * B3.a * B4.a * B5.a
  let minBound = B1.minBound, B2.minBound, B3.minBound, B4.minBound, B5.minBound
  let maxBound = B1.maxBound, B2.maxBound, B3.maxBound, B4.maxBound, B5.maxBound
end
module Bounded_6
  (B1 : Bounded)
  (B2 : Bounded)
  (B3 : Bounded)
  (B4 : Bounded)
  (B5 : Bounded)
  (B6 : Bounded)
  : Bounded with type a = B1.a * B2.a * B3.a * B4.a * B5.a * B6.a =
struct
  type a = B1.a * B2.a * B3.a * B4.a * B5.a * B6.a
  let minBound = B1.minBound, B2.minBound, B3.minBound, B4.minBound, B5.minBound, B6.minBound
  let maxBound = B1.maxBound, B2.maxBound, B3.maxBound, B4.maxBound, B5.maxBound, B6.maxBound
end
module Bounded_7
  (B1 : Bounded)
  (B2 : Bounded)
  (B3 : Bounded)
  (B4 : Bounded)
  (B5 : Bounded)
  (B6 : Bounded)
  (B7 : Bounded)
  : Bounded with type a = B1.a * B2.a * B3.a * B4.a * B5.a * B6.a * B7.a =
struct
  type a = B1.a * B2.a * B3.a * B4.a * B5.a * B6.a * B7.a
  let minBound = B1.minBound, B2.minBound, B3.minBound, B4.minBound, B5.minBound, B6.minBound, B7.minBound
  let maxBound = B1.maxBound, B2.maxBound, B3.maxBound, B4.maxBound, B5.maxBound, B6.maxBound, B7.maxBound
end
module Bounded_8
  (B1 : Bounded)
  (B2 : Bounded)
  (B3 : Bounded)
  (B4 : Bounded)
  (B5 : Bounded)
  (B6 : Bounded)
  (B7 : Bounded)
  (B8 : Bounded)
  : Bounded with type a = B1.a * B2.a * B3.a * B4.a * B5.a * B6.a * B7.a * B8.a =
struct
  type a = B1.a * B2.a * B3.a * B4.a * B5.a * B6.a * B7.a * B8.a
  let minBound = B1.minBound, B2.minBound, B3.minBound, B4.minBound, B5.minBound, B6.minBound, B7.minBound, B8.minBound
  let maxBound = B1.maxBound, B2.maxBound, B3.maxBound, B4.maxBound, B5.maxBound, B6.maxBound, B7.maxBound, B8.maxBound
end
module Bounded_9
  (B1 : Bounded)
  (B2 : Bounded)
  (B3 : Bounded)
  (B4 : Bounded)
  (B5 : Bounded)
  (B6 : Bounded)
  (B7 : Bounded)
  (B8 : Bounded)
  (B9 : Bounded)
  : Bounded with type a = B1.a * B2.a * B3.a * B4.a * B5.a * B6.a * B7.a * B8.a * B9.a =
struct
  type a = B1.a * B2.a * B3.a * B4.a * B5.a * B6.a * B7.a * B8.a * B9.a
  let minBound = B1.minBound, B2.minBound, B3.minBound, B4.minBound, B5.minBound, B6.minBound, B7.minBound, B8.minBound, B9.minBound
  let maxBound = B1.maxBound, B2.maxBound, B3.maxBound, B4.maxBound, B5.maxBound, B6.maxBound, B7.maxBound, B8.maxBound, B9.maxBound
end

(* Generate instances of Bounded. *)
let gen_bounded = Printf.sprintf "
module %s = (struct type a = %s let minBound = %s let maxBound = %s end : Bounded)
"
let bounded_name = Printf.sprintf "Bounded_%s"

let gen_bounded : Type.typedecl -> string = function
  | (name, [], `Sum ctors) -> gen_bounded (bounded_name name) name (List.hd ctors).Type.name (Util.last ctors).Type.name
  | _                      -> failwith ("instances of bounded must be enumeration types")
      (* TODO: support single constructors where the parameter is bounded *)


