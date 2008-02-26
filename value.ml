(*pp deriving *)
open Num
open Utility

type binop = [ 
| Syntaxutils.comparison
| `Union
| `App
| `RecExt of string
| `MkTableHandle of Types.row ]
  deriving (Show)

type xmlitem =   Text of string
               | Attr of (string * string)
               | Node of (string * xml)
and xml = xmlitem list
    deriving (Typeable, Show, Pickle, Eq, Shelve)

let is_attr = function
  | Attr _ -> true
  | _      -> false

let attrs = List.filter is_attr
and nodes = List.filter (not -<- is_attr)

let rec string_of_xml xml : string
    = String.concat "" (List.map string_of_item xml)
and string_of_item : xmlitem -> string = 
  let format_attrs attrs = match String.concat " " (List.map string_of_item attrs) with 
    | "" -> ""
    | a -> " " ^ a in
  let escape = Str.global_replace (Str.regexp "\"") "\\\""  in
    function
      | Attr (k, v) -> k ^ "=\"" ^ escape v ^ "\""
      | Text s -> xml_escape s
      | Node (tag, children) -> let attrs, nodes = attrs children, nodes children in
          match nodes with 
            | [] -> "<" ^ tag ^ format_attrs attrs ^ "/>"
            | _  -> ("<" ^ tag ^ format_attrs attrs ^ ">" 
                     ^ string_of_xml nodes
                     ^ "</" ^ tag ^ ">")

type table = (Result.database * string) * string * Types.row
  deriving (Show)    

type primitive_value = [
| `Bool of bool
| `Char of char
| `Database of (Result.database * string)
| `Table of table
| `Float of float
| `Int of num
| `XML of xmlitem 
| `NativeString of string ]
  deriving (Show)
        
type continuation = (Ir.var * env * Ir.computation) list
and t = [
| primitive_value
| `List of t list
| `Record of (string * t) list
| `Variant of string * t 
| `RecFunction of ((Ir.var * (Ir.var list * Ir.computation)) list * env * Ir.var)
| `PrimitiveFunction of string
| `ClientFunction of string
| `Abs of t
| `Continuation of continuation ]
and env = t IntMap.t
  deriving (Show)

let bind = IntMap.add
let lookup = IntMap.lookup
let shadow outers ~by = IntMap.fold IntMap.add by outers

let string_as_charlist s : t =
  `List (List.map (fun x -> (`Char x)) (explode s))

exception Match of string

let escape = 
  Str.global_replace (Str.regexp "\\\"") "\\\""

let string_of_cont = Show_continuation.show

exception Not_tuple

let rec char_of_primchar = function 
    `Char c -> c
  | o -> raise (Match (string_of_value o))

and charlist_as_string chlist = 
  match chlist with
    | `List elems -> 
        Utility.implode (List.map char_of_primchar elems)
    | _ -> raise (Match("Non-string " ^ string_of_value chlist
                        ^ " used as string."))

and string_of_value : t -> string = function
  | #primitive_value as p -> string_of_primitive p
  | `PrimitiveFunction (name) -> name
  | `ClientFunction (name) -> name
  | `Abs v -> "abs " ^ string_of_value v
    (* Choose from fancy or simple printing of functions: *)
  | `RecFunction(defs, env, name) -> 
      if Settings.get_value(Basicsettings.printing_functions) then
        "{ " ^ (mapstrcat " "
                  (fun (_name, (formals, body)) ->
                     "fun (" ^ String.concat "," (List.map Ir.string_of_var formals) ^ ") {" ^
                       Ir.string_of_computation body ^ "}")
                  defs) ^
          " " ^ Ir.string_of_var name ^ " }[" ^ string_of_environment env ^ "]"
      else
        "fun"
  | `Record fields ->
      (try string_of_tuple fields
       with Not_tuple ->
         "(" ^ mapstrcat "," (fun (label, value) ->
                                label ^ "=" ^ string_of_value value)
           (List.sort (fun (l,_) (r, _) -> compare l r) fields) ^ ")")
  | `Variant (label, `Record []) -> label ^ "()"
  | `Variant (label, value) -> label ^ "(" ^ string_of_value value ^ ")"
  | `List [] -> "[]"
  | `List (`Char _::_) as c  -> "\"" ^ escape (charlist_as_string c) ^ "\""
  | `List ((`XML _)::_ as elems) -> mapstrcat "" string_of_element_value elems
  | `List (elems) -> "[" ^ String.concat ", " (List.map string_of_value elems) ^ "]"
  | `Continuation cont -> "Continuation" ^ string_of_cont cont
and string_of_primitive : primitive_value -> string = function
  | `Bool value -> string_of_bool value
  | `Int value -> string_of_num value
  | `Float value -> string_of_float value
  | `Char c -> "'"^ Char.escaped c ^"'"
  | `XML x -> string_of_item x
  | `Database (_, params) -> "(database " ^ params ^")"
  | `Table (_, table_name, _) -> "(table " ^ table_name ^")"
  | `NativeString s -> "\"" ^ s ^ "\""
				
and string_of_tuple (fields : (string * t) list) : string = 
    let fields = List.map (function
                        | x, y when numberp x  -> (int_of_string x, y)
                        | _ -> raise Not_tuple) fields in
    let sorted = List.sort (fun (x,_) (y, _) -> compare x y) fields in
    let numbers, values = List.split sorted in 
      if ordered_consecutive numbers && List.length numbers > 1 && List.hd numbers = 1 then
        "(" ^ String.concat ", " (List.map string_of_value values) ^ ")"
      else raise Not_tuple 

and numberp s = try ignore(int_of_string s); true with _ -> false

and string_of_environment : env -> string = fun _env -> "[ENVIRONMENT]"

(* this looks a bit dodgey *)
and string_of_element_value = function 
  | `Char c -> String.make 1 c
  | otherwise -> string_of_value otherwise

let project name = function
  | (`Record fields) -> List.assoc name fields
  | _ -> failwith ("Match failure in record projection")

(** Given a Links tuple, returns an Ocaml list of the Links values in that
    tuple. *)
let untuple : t -> t list = 
  let rec aux n output = function
    | [] -> List.rev output
    | fields ->
        match List.partition (fst ->- (=)(string_of_int n)) fields with
          | [_,r], rest -> aux (n+1) (r::output) rest
          | _ -> assert false
  in function
    | `Record fields -> aux 1 [] fields
    | _ -> assert false

(* boxing and unboxing of primitive types *)
let box_bool b = `Bool b
and unbox_bool : t -> bool   = function
  | `Bool b  -> b | _ -> failwith "Type error unboxing bool"
and box_int i = `Int i      
and unbox_int  : t -> num    = function
  | `Int i   -> i
  | other -> failwith("Type error unboxing int")
and box_float f = `Float f  
and unbox_float : t -> float = function
  | `Float f -> f | _ -> failwith "Type error unboxing float"
and box_char c = `Char c    
and unbox_char :  t -> char = function
  | `Char f -> f | _ -> failwith "Type error unboxing char"
and box_xml x = `XML x      
and unbox_xml  :  t -> xmlitem = function
  | `XML x -> x | _ -> failwith "Type error unboxing xml"
and box_string = string_as_charlist
and unbox_string : t -> string = charlist_as_string
and box_list l = `List l
and unbox_list : t -> t list = function
  | `List l -> l | _ -> failwith "Type error unboxing list"
and box_unit : unit -> t 
  = fun () -> `Record []
and unbox_unit : t -> unit = function
  | `Record [] -> () | _ -> failwith "Type error unboxing unit"
let unbox_pair = function 
  | (`Record [(_, a); (_, b)]) -> (a, b)
  | _ -> failwith ("Match failure in pair conversion")

