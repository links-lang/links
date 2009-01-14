open Num

module StringMap = Map.Make (String);;

type value = 
  | Bool of bool
  | Char of char
  | Int of Num.num
  | Float of float
  | NativeString of string
  | Function of (value -> value)
  | Variant of string * value
  | Record of value StringMap.t
  | Lst of value list
  | Xml of string * (string * value) list * value list

(* This is quickly cobbled together so I can play with things. *)
let rec string_of_value = function
  | Bool x -> string_of_bool x
  | Int x -> Num.string_of_num x
  | Char x -> "'" ^ Char.escaped x ^ "'"
  | NativeString x -> x
  | Float x -> string_of_float x
  | Function x -> "Fun"
  | Lst l -> "[" ^ String.concat ", " (List.map string_of_value l) ^ "]"
  | Variant (s, v) -> s ^ "(" ^ string_of_value v ^ ")"
  | Record r -> "(" ^ String.concat ", "
      (StringMap.fold
         (fun n v l -> l @ [(n ^ "=" ^ string_of_value v)]) r []) ^ ")"
  | Xml (name, attrs, children) ->
      let attrs = List.fold_left (
        fun s attr ->
          match attr with
            | (n, NativeString v) -> s ^ " " ^ n ^ "=" ^ "\"" ^v^ "\""
            | _ -> assert false) "" attrs 
      in
        "<" ^ name ^ attrs ^ ">" ^
          String.concat "" (List.map string_of_value children) ^
          "</" ^ name ^ ">"

let box_bool x = Bool x
let unbox_bool = function
  | Bool x -> x
  | _ -> assert false

let box_int x = Int x
let unbox_int = function
  | Int x -> x
  | _ -> assert false

let box_char x = Char x
let unbox_char = function
  | Char x -> x
  | _ -> assert false

let box_string x = NativeString x
let unbox_string = function
  | NativeString x -> x
  | _ -> assert false
  
let box_float x = Float x
let unbox_float = function
  | Float x -> x
  | _ -> assert false

let box_func f = Function f
let unbox_func = function
  | Function x -> x
  | _ -> assert false

let box_variant (n, v)  = Variant (n, v)
let unbox_variant = function
  | Variant (n, v) -> (n, v)
  | _ -> assert false

let box_record r = Record r
let unbox_record = function
  | Record r -> r
  | _ -> assert false

let box_list l = Lst l
let unbox_list = function
  | Lst l -> l
  | _ -> assert false

let box_xml (n, a, c) = Xml (n, a, c)
let unbox_xml = function 
  | Xml (n, a, c) -> (n, a, c)
  | _ -> assert false

let project m k = StringMap.find (unbox_string k) (unbox_record m);;

let start = box_func (fun x -> x);;

let nil = box_list ([])

let equals = box_func (
  fun x -> box_func (
    fun y -> box_bool (x = y)))

let int_add = box_func (
  fun x -> box_func (
    fun y -> box_int ((unbox_int x) +/ (unbox_int y))))

let hd = box_func (
  fun k -> box_func (
    fun l -> (unbox_func k) (List.hd (unbox_list l))))

let tl = box_func (
  fun k -> box_func (
    fun l -> (unbox_func k) (box_list (List.tl (unbox_list l)))))

let cons = box_func (
  fun x -> box_func (
    fun l -> box_list (x::(unbox_list l))))

let stringToXml = box_func (
  fun s -> s)

let reifyK = box_func (
  fun k -> box_func (
    fun cont ->
      (unbox_func k) (
        NativeString (
          Netencoding.Base64.encode (
            Marshal.to_string cont [Marshal.Closures])))))

let exit = box_func (
  fun k -> box_func (
    fun v -> v
))

let run entry_f =
  let result =
    if Array.length Sys.argv > 1 then
      let k = (Marshal.from_string (Netencoding.Base64.decode Sys.argv.(1)) 0 : value) in
      let fake_k = (box_func (fun _ -> assert false)) in
        (unbox_func ((unbox_func k) fake_k))  (box_int (num_of_string "42"))
    else
      entry_f () in
    print_endline (string_of_value result)
