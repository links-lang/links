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
  


(* Generate instances of Enum *)
let enum_template = Printf.sprintf "
module %s = EnumDefaults(struct type a = %s let numbering = %s end)
"

let enum_name = Printf.sprintf "Enum_%s"
let check_nullary ctor = 
  if List.length ctor.Type.args <> 0 then 
    failwith ("Only nullary constructors are allowed for instances of Enum.
  ("^ ctor.Type.name ^" is not nullary)")

let numbering ctors = 
  Printf.sprintf
    "[%s]" 
    (String.concat "; " 
       (List.map2
          (fun n d -> Printf.sprintf "%s, %d" n.Type.name d) ctors (range 0 (List.length ctors - 1))))

      
let gen_enum : Type.typedecl -> string = function
  | (name, [], `Sum ctors) -> (List.iter check_nullary ctors;
                               enum_template (enum_name name) name (numbering ctors))
  | _          -> failwith ("gen_enum can only currently generate instances for non-parametric sum types")


