open Num

module StringMap = Map.Make (String);;

type xml =
  | Node of string * (string * value) list * value list
  | Text of string
and value = 
  | Bool of bool
  | Char of char
  | Int of Num.num
  | Float of float
  | NativeString of string
  | Function of (value -> value)
  | Variant of string * value
  | Record of value StringMap.t
  | Lst of value list
  | Xml of xml

(* This is quickly cobbled together so I can play with things. *)
let rec string_of_value = function
  | Bool x -> string_of_bool x
  | Int x -> Num.string_of_num x
  | Char x -> String.make 1 x
  | NativeString x -> x
  | Float x -> string_of_float x
  | Function x -> "Fun"
  | Lst l -> "[" ^ String.concat ", " (List.map string_of_value l) ^ "]"
  | Variant (s, v) -> s ^ "(" ^ string_of_value v ^ ")"
  | Record r -> "(" ^ String.concat ", "
      (StringMap.fold
         (fun n v l -> l @ [(n ^ "=" ^ string_of_value v)]) r []) ^ ")"
  | Xml x -> string_of_xml x
and      
    string_of_xml = function
      | Node (name, attrs, children) ->
      let attrs = List.fold_left (
        fun s attr ->
          match attr with
            | (n, NativeString v) -> s ^ " " ^ n ^ "=" ^ "\"" ^v^ "\""
            | _ -> assert false) "" attrs 
      in
        "<" ^ name ^ attrs ^ ">" ^
          String.concat "" (List.map string_of_value children) ^
          "</" ^ name ^ ">"
      | Text s -> s

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

let box_xml x = Xml x
let unbox_xml = function 
  | Xml x -> x
  | _ -> assert false

(* Internal functions used by compiler *)
let start = box_func (fun x -> x);;
let project m k = StringMap.find (unbox_string k) (unbox_record m);;
let build_xml (n, a, c) = Node (n, a, c)

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
  fun s -> box_xml (Text (unbox_string s)))

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

let getenv : string -> string option =
  fun name ->
    try Some (Sys.getenv name)
    with Not_found -> None

let handle_request entry_f =
  NativeString ("Content-type: text/html\n\n" ^ (string_of_value (entry_f ())))
    
let resume_with_cont cont arg =
  let cont = (Marshal.from_string (Netencoding.Base64.decode Sys.argv.(1)) 0 : value) in
  let k = (box_func (fun _ -> assert false)) in
    (unbox_func ((unbox_func cont) k)) arg
      
let run entry_f =  
  let result = 
    match getenv "REQUEST_METHOD" with
      | Some _ -> handle_request entry_f
      | None -> entry_f ()
  in
    print_endline (string_of_value result)
