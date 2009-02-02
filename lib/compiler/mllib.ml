open Num

module StringMap = Map.Make (String);;

exception InternalError of string

let cgi_parameters = ref []

type xml = value list
and xmlitem = 
  | Text of string
  | Node of string * (string * string) list * xml
and value = 
  | Unit
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
  Str.global_replace (Str.regexp "<") "&lt;" ( 
    Str.global_replace (Str.regexp ">") "&lt;" 
    (Str.global_replace (Str.regexp "&") "&amp;" s))

let mapstrcat glue f list = String.concat glue (List.map f list)

let mapIndex (f : 'a -> int -> 'b) : 'a list -> 'b list =
  let rec mi i =
    function
      | [] -> []
      | (x :: xs) -> f x i :: mi (i+1) xs
  in
    mi 0

let rec drop n = if n = (num_of_int 0) then identity else function
  | []     -> []
  | _ :: t -> drop (pred_num n) t

let rec take n list = match n, list with 
  | 0, _ -> []
  | _, [] -> []
  | _, h :: t -> h :: take (n - 1) t

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
let start = id

let project m k = StringMap.find (unbox_string k) (unbox_record m);;
let build_xmlitem name attrs children =
    Node (name, List.map 
            (fun (n, v) -> (n, unbox_string v)) attrs, children)

let unwrap_pair m = 
  let m = unbox_record m in
    StringMap.find "1" m, StringMap.find "2" m

let unwrap_triple m = 
  let m = unbox_record m in
    StringMap.find "1" m, StringMap.find "2" m, StringMap.find "3" m

let marshal_value : value -> string =
  fun v -> Netencoding.Base64.encode (
    Marshal.to_string v [Marshal.Closures])

let apply_2 f a1 a2 = 
  (unbox_func ((unbox_func f) a1)) a2

let apply_3 f a1 a2 a3 =
  (unbox_func ((unbox_func ((unbox_func f) a1)) a2)) a3

let apply_4 f a1 a2 a3 a4 =
  (unbox_func ((unbox_func ((unbox_func ((unbox_func f) a1)) a2)) a3)) a4


(* After several time-consuming attempts to nicely pull this out of
   the compiled prelude, I decided to just duplicate the functionality
   here. Bleh. *)
let render_page page =
  let n, k, fs = unwrap_triple page in
  let ms, _ = unwrap_pair (apply_2 fs id (box_int (num_of_int 0))) in    
  let zs =
    mapIndex (fun m i zs -> apply_4 m id k zs (box_int (num_of_int i))) (unbox_list ms) in
  let b_zs = box_list (List.map box_func zs) in
    apply_2 k id (box_list  (List.map (fun z -> z b_zs) zs))
      
(* Library functions *)

let l_unit = Unit
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
let u__negate = minus_num

let u__tilde s r = assert false

let u_l_int_gt = (>)
let u_l_int_lt = (<)
let u_l_int_gte = (>=)
let u_l_int_lte = (<=)

let u__mod = mod_num

let u__hd = List.hd
let u__tl = List.tl

let u_l_cons v l = v::l
let u_l_concat l1 l2 = l1 @ l2
let u__take n l = take (int_of_num n) l

let u__drop = drop

let u__intToString = string_of_num
let u__stringToInt = num_of_string

let u__stringToFloat = float_of_string

let u__stringToXml s =
  [box_xmlitem (Text s)]

let u__intToXml i = 
  u__stringToXml (string_of_num i)

let add_attribute (s, v) = function
  | Xmlitem (Node (n, a, c)) ->
      Xmlitem (Node (n, ((unbox_string s), (unbox_string v))::a, c))
  | Xmlitem (Text s) -> Xmlitem (Text s)
  | _ -> assert false

let add_attributes = List.fold_right add_attribute

let u__addAttributes x attrs =
  let attrs = List.map unwrap_pair attrs in
    List.map (add_attributes attrs) x

let u__environment _ = 
  let is_internal s = Str.first_chars s 1 = "_" in
  let mkpair (x1, x2) = box_record (
    StringMap.add "1" (box_string x1)
      (StringMap.add "2" (box_string x2)
         StringMap.empty))
  in
    List.map mkpair (List.filter (not -<- is_internal -<- fst) !cgi_parameters)

let u__unsafePickleCont = marshal_value

let u__reifyK = marshal_value

let u__error = failwith

let u__redirect s = assert false

let u__exit k v = v

(* Main stuff--bits stolen from webif.ml *)

let is_multipart () =
  ((Cgi.safe_getenv "REQUEST_METHOD") = "POST" &&
      Cgi.string_starts_with (Cgi.safe_getenv "CONTENT_TYPE") "multipart/form-data")

let resume_with_cont cont =
  let cont = (Marshal.from_string (Netencoding.Base64.decode cont) 0 : value) in
    (unbox_func cont) start

let handle_request entry_f =
  let cgi_args =
    if is_multipart () then
      List.map (fun (name, {Cgi.value=value}) ->
                  (name, value)) (Cgi.parse_multipart_args ())
    else
      Cgi.parse_args () in

    cgi_parameters := cgi_args;

    let value =
      if List.mem_assoc "_k" cgi_args then
        resume_with_cont (List.assoc "_k" cgi_args)
      else
        entry_f ()
    in
      "Content-type: text/html\n\n" ^ (string_of_value (render_page value))
          
let run entry_f  =  
  let s = 
    match getenv "REQUEST_METHOD" with
      | Some _ -> handle_request entry_f
      | None -> string_of_value (entry_f ())
  in
    print_endline s
