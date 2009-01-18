open Num

module StringMap = Map.Make (String);;

exception InternalError of string

type xml = value list
and xmlitem = 
  | Text of string
  | Node of string * (string * string) list * xml
and value = 
  | Bool of bool
  | Char of char
  | Int of Num.num
  | Float of float
  | Function of (value -> value)
  | Variant of string * value
  | Record of value StringMap.t
  | Lst of value list
  | Xmlitem of xmlitem

(* Stolen from Utility.ml *)
let identity x = x

let (-<-) f g x = f (g x)

let xml_escape s = 
  Str.global_replace (Str.regexp "<") "&lt;" 
    (Str.global_replace (Str.regexp "&") "&amp;" s)

let mapstrcat glue f list = String.concat glue (List.map f list)

let rec drop n = if n = (num_of_int 0) then identity else function
  | []     -> []
  | _ :: t -> drop (pred_num n) t

let explode : string -> char list = 
  let rec explode' list n string = 
    if n = String.length string then list
    else explode' (string.[n] :: list) (n + 1) string
  in List.rev -<- (explode' [] 0)

let implode : char list -> string = 
  (String.concat "") -<- (List.rev -<- (List.rev_map (String.make 1)))

let getenv : string -> string option =
  fun name ->
    try Some (Sys.getenv name)
    with Not_found -> None

(* Boxing functions *)
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

let box_list l = Lst l
let unbox_list = function
  | Lst l -> l
  | _ -> assert false

let box_string x = box_list (List.map box_char (explode x))
let unbox_string = function
  | Lst x -> implode (List.map unbox_char x)
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

let box_xmlitem x = Xmlitem x
let unbox_xmlitem = function 
  | Xmlitem x -> x
  | _ -> assert false

(* Stolen from value.ml *)
let attr_escape =
   Str.global_replace (Str.regexp "\"") "\\\""

(* This is quickly cobbled together so I can play with things. *)
let rec string_of_value = function
  | Bool x -> string_of_bool x
  | Int x -> Num.string_of_num x
  | Char x -> "'" ^ Char.escaped x ^ "'"
  | Float x -> string_of_float x
  | Function x -> "Fun"
  | Lst l -> string_of_list l
  | Variant (s, v) -> s ^ "(" ^ string_of_value v ^ ")"
  | Record r -> "(" ^ String.concat ", "
      (StringMap.fold
         (fun n v l -> l @ [(n ^ "=" ^ string_of_value v)]) r []) ^ ")"
  | Xmlitem x -> string_of_xmlitem x
and
    string_of_list = function
      | [] -> "[]"
      | (x::xs) as l ->
          match x with
            | Char _ -> "\"" ^ String.escaped (unbox_string (Lst l)) ^ "\""
            | Xmlitem _ -> mapstrcat "" string_of_value l
            | _ ->  "[" ^ mapstrcat ", " string_of_value l ^ "]"
and
    (* Somewhat stolen from value.ml *)
    string_of_xmlitem = let format_attr (k, v) =
      k ^ "=\"" ^ attr_escape v ^ "\""
    in
    let format_attrs attrs =
      match mapstrcat " " format_attr attrs with
        | "" -> ""
        | a -> " " ^ a 
    in
      function
        | Text s -> xml_escape s
        | Node (name, attrs, children) ->
            match children with
              | [] -> "<" ^ name ^ format_attrs attrs ^ " />"
              | _ -> ("<" ^ name ^ format_attrs attrs ^ ">"
                         ^ mapstrcat "" string_of_value children
                         ^ "</" ^ name ^ ">")
          
(* Internal functions used by compiler *)
let id = box_func identity
let start = box_func (fun x -> x);;
let project m k = StringMap.find (unbox_string k) (unbox_record m);;
let build_xmlitem name attrs children =
    Node (name, List.map 
            (fun (n, v) -> (n, unbox_string v)) attrs, children)

(* Library functions *)

let l_nil = box_list ([])

let u_l_equals = (=)
let u_l_not_equals = (!=)

let u_l_and = (&&)
let u_l_or = (||)
let u__not = not

let u_l_int_add = (+/)
let u_l_int_minus = (-/)
let u_l_int_mult = ( */ )
let u_l_int_div = (//)
let u__negate = Num.minus_num

let u_l_int_gt = (>)
let u_l_int_lt = (<)
let u_l_int_gte = (>=)
let u_l_int_lte = (<=)

let u__mod = Num.mod_num

let u__hd = List.hd
let u__tl = List.tl

let u_l_cons v l = v::l
let u_l_concat l1 l2 = l1 @ l2

let u__drop = drop

let u__stringToXml s =
  [box_xmlitem (Text s)]

let u__reifyK k =
  Netencoding.Base64.encode (
    Marshal.to_string k [Marshal.Closures])

let u__error = failwith

let u__exit k v = v

(* Main stuff *)
let handle_request entry_f =
  box_string ("Content-type: text/html\n\n" ^ (string_of_value (entry_f ())))
    
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
