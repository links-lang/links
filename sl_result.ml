(*open Str*)
open Num
open List

open Sl_pickle
open Sl_kind
open Query
open Sl_syntax
open Sl_utility

exception Runtime_failure of string
exception Runtime_exception of string

class type otherfield = 
object 
  method show : string
end

type db_field_type = BoolField | TextField | IntField | FloatField
		     | SpecialField of otherfield

let string_of_db_field_type = function
  | BoolField -> "bool"
  | TextField -> "text"
  | IntField -> "int"
  | FloatField -> "float"
  | SpecialField ft -> ft # show

type db_status = QueryOk | QueryError

class virtual dbresult = object
  method virtual status : db_status
  method virtual nfields : int
  method virtual fname : int -> string
  method virtual ftype : int -> db_field_type
  method virtual get_all_lst : string list list
  method virtual error : string
end

class virtual database = object
  method virtual exec : string -> dbresult
end

type db_construtor = string -> (database * string)

(* list of available database drivers.  Each driver registers itself
   at load time: the contents of the list depends on which drivers are
   built.. *)

let database_drivers = ref ([] : (string * db_construtor) list)

let register_driver : (string * db_construtor) -> unit
  = fun ((name, _) as pair) -> 
    debug ("registering driver for " ^ name);
    database_drivers := pair :: !database_drivers

let db_connect driver params =
  let constructor = 
    try assoc driver !database_drivers 
    with Not_found -> failwith ("No driver for database type `" ^ driver ^ "'")
  in constructor params

let parse_db_string : string -> (string * string) = 
  fun params -> 
    match Str.bounded_split (Str.regexp ":") params 2 with
      | [hd; tail] -> (hd, tail)
      | _ -> failwith ("Could not parse db connection string : " ^ params)
and reconstruct_db_string : (string * string) -> string =
  fun (x,y) -> x ^ ":" ^ y

type unop = MkColl of collection_type
            | MkVariant of string
            | MkDatabase
            | VrntSelect of (string * string * expression * string option * 
                               expression option)
            | QueryOp of (query * kind)
            | SortOp of bool

let string_of_coll_type = function
  | `Set -> "set"
  | `Bag -> "bag"
  | `List -> "list"
  | _ -> failwith("Internal error: an abstract collection type was stringified")

let string_of_unop = function
  | MkColl ctype -> "MkColl" ^ string_of_coll_type ctype
  | MkDatabase -> "MkDatabase"
  | MkVariant label -> "MkVariant " ^ label
  | SortOp up -> "SortOp" ^ if up then "Up" else "Down"
  | QueryOp _ -> "QueryOp(...)"
  | VrntSelect _ -> "VrntSelect(...)"

type binop = EqEqOp | NotEqOp | LessEqOp | LessOp | BeginsWithOp
	     | UnionOp of collection_type
	     | RecExtOp of string


type xmlitem =   Text of string
               | Attr of (string * string)
               | Node of (string * xml)
and xml = xmlitem list

let is_attr = function
  | Attr _ -> true
  | _      -> false

let attrs = filter is_attr
and nodes = filter (not @@ is_attr)

let rec string_of_xml xml : string
    = String.concat "" (map string_of_item xml)
and string_of_item : xmlitem -> string = 
  let format_attrs attrs = match String.concat " " (map string_of_item attrs) with 
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

let rec serialise_item : xmlitem serialiser = function
  | Text v -> serialise1 'a' (serialise_string) v
  | Attr v -> serialise2 'b' (serialise_string, serialise_string) v
  | Node v -> serialise2 'c' (serialise_string, serialise_xml) v
and serialise_xml x = 
  serialise1 'a' (serialise_list serialise_item) x

let rec deserialise_item : xmlitem deserialiser = 
  fun s -> let t, obj, rest = extract_object s in 
  let e = 
    (match t with 
       | 'a' -> Text (deserialise1 (deserialise_string ) obj)
       | 'b' -> Attr (deserialise2 (deserialise_string, deserialise_string ) obj)
       | 'c' -> Node (deserialise2 (deserialise_string, deserialise_xml) obj))
  in e, rest
and deserialise_xml : xml deserialiser = 
  fun s -> let t, obj, rest = extract_object s in
    match t with
      | 'a' -> deserialise1 (deserialise_list (deserialise_item)) obj, rest

type contin_frame = 
  | Definition of (environment * string)
  | FuncArg of (expression * environment) (* FIXME: This is twiddled *)
  | FuncApply of (result * environment )  (* FIXME: This is twiddled *)
  | LetCont of (environment * 
		  string * expression)
  | BranchCont of (environment * 
		     expression * expression)
  | BinopRight of (environment * 
		     binop * expression)
  | BinopApply of (environment * 
		     binop * result)
  | UnopApply of (environment * unop )
  | RecSelect of (environment * string * string * string * expression)
  | CollExtn of (environment * 
		   collection_type * 
		   string * expression * 
		   result list * result list)
  | StartCollExtn of (environment *
			string * expression)
  | XMLCont of (environment 
                * string                     (* tag *)
                * string option              (* attr name, if any *)
                * xml                        (* child nodes *)
                * (string * expression) list (* unevaluated attributes *)
                * expression list            (* unevaluated elements *)
                )
  | Ignore of (environment * expression )

  | Recv of (environment)

and primitive = [
  | `Bool of bool
  | `Int of num
  | `Float of float
  | `Char of char
  | `XML of xmlitem
  | `PFunction of (string * (((continuation -> result -> result) * continuation * result) -> result) option * result list)
]
and result = [
  | `Primitive of primitive 
  | `Function of (string * environment (*locals*)
                         * environment (*globals*) 
		         * expression)
  | `Record of ((string * result) list)
  | `Variant of (string * result)
  | `Collection of (collection_type * (result) list)
  | `Database of (database * string)
  | `Environment of (string (* url *) * environment)
  | `Continuation of continuation
]
and continuation = contin_frame list
and binding = (string * result)
and environment = (binding list)

let (toplevel: continuation) = [] 

let xmlitem_of : result -> xmlitem = function
  | (`Primitive(`XML x)) -> x
  | _ -> raise(Match_failure("",0,0))

let primfun name func = `Primitive (`PFunction (name, Some func, []))
let primfunappl name func args = `Primitive (`PFunction (name, Some func, args))
and bool b = `Primitive(`Bool b)
and int i = `Primitive(`Int i)
and float f = `Primitive(`Float f)
and char c = `Primitive(`Char c)
and listval es = `Collection(`List, es)
and xmlnodeval contents = `Primitive(`XML(Node contents))

exception NotARecord of result
 
let recfields = function
  | `Record (fields) -> fields
  | value -> raise(NotARecord(value))

exception Match of string

let string_as_charlist s : result =
  `Collection(`List, map (fun x -> (`Primitive (`Char x))) (explode s))

let pair_as_ocaml_pair = function 
  | (`Record [(_, a); (_, b)]) -> (a, b)
  | _ -> failwith ("Match failure in pair conversion")

let links_fst x = fst (pair_as_ocaml_pair x)
let links_snd x = snd (pair_as_ocaml_pair x)

let escape = 
  Str.global_replace (Str.regexp "\\\"") "\\\""

let rec strip_binding(name, value) = (name, strip_result value)
and strip_env env = map strip_binding env
and strip_continuation cont = map strip_cont_frame cont
and strip_cont_frame = function
  | FuncArg(expr, env) -> FuncArg(expr, strip_env env)
  | FuncApply(func, env) -> FuncApply(strip_result func, strip_env env)
  | LetCont(env, var, body) -> LetCont(strip_env env, var, body)
  | BranchCont(env, conseq, altern) -> BranchCont(strip_env env, 
                                                  conseq, altern)
  | BinopRight(env, op, rhs) -> BinopRight(strip_env env, op, rhs)
  | BinopApply(env, op, lhs) -> BinopApply(strip_env env, op, lhs)
  | UnopApply(env, op) -> UnopApply(strip_env env, op)
  | RecSelect(env, var, label, var2, body) -> RecSelect(strip_env env, 
                                                       var, label, var2, 
                                                       body)
  | CollExtn(env, ctype, var, body, results, source) ->
      CollExtn(strip_env env, ctype, var, body, 
               map strip_result results, map strip_result source)
  | StartCollExtn(env, var, body) -> StartCollExtn(strip_env env, var, body)
  | XMLCont(env, tagname, attrname, children, attrexprs, childexprs) ->
      XMLCont(strip_env env, tagname, attrname, children, attrexprs, childexprs)
  | Ignore(env, body) -> Ignore(strip_env env, body)
and strip_result = function
    (* And now, ladies and gents, the ONE essential case: *)
  | `Primitive(`PFunction(name, impl, pargs)) ->
      `Primitive(`PFunction(name, None, map strip_result pargs))
  | `Primitive(prim) -> `Primitive(prim)
  | `Function(name, locals, globals, body) -> 
      `Function(name, strip_env locals, strip_env globals, body)
  | `Record(fields) -> `Record(map strip_binding fields)
  | `Variant(label, value) ->  `Variant(label, strip_result value)
  | `Collection(ctype, elements)-> `Collection(ctype, map strip_result elements)
  | `Database(db, params) ->
      `Database(db, params)
  | `Environment(namespace, env) -> `Environment(namespace, strip_env env)
  | `Continuation(cont) -> `Continuation(strip_continuation cont)

let strip_cont cont = map strip_cont_frame cont


let serialise_continuation c = Marshal.to_string (strip_cont c) []



let deserialise_continuation resolve str = 
  debug "unmarshaling contin.";
  Marshal.from_string str 0

let rec pp_continuation = String.concat "=->" @@
  map (function
         | FuncArg _ -> "FuncArg"
         | FuncApply _ -> "FuncApply"
         | LetCont _ -> "LetCont"
         | BranchCont _ -> "BranchCont"
         | BinopRight _ -> "BinopRight"
         | BinopApply _ -> "BinopApply"
         | UnopApply _ -> "UnopApply"
         | RecSelect _ -> "RecSelect"
         | CollExtn _ -> "CollExtn"
         | StartCollExtn _ -> "StartCollExtn"
         | XMLCont _ -> "XMLCont"
         | Ignore _ -> "Ignore")
  
exception Not_tuple

open Netencoding

let rec char_of_primchar = function 
    (`Primitive (`Char c)) -> c
  | o -> raise (Match (string_of_result o))

and charlist_as_string chlist = 
  match chlist with
    | `Collection (`List, elems) -> 
        Sl_utility.implode (map char_of_primchar elems)
    | _ -> raise (Match("Non-string " ^ string_of_result chlist
                        ^ " used as string."))

    
and string_of_result : result -> string = function
  | `Primitive p -> string_of_primitive p
  | `Function _ -> "fun"
  | `Record fields -> (try string_of_tuple fields
                       with Not_tuple ->
                         "(" ^ (String.concat "," (map (function (label, value) -> label ^ "=" ^ (string_of_result value)) fields)) ^ ")")
  | `Variant (label, value) -> label ^ " " ^ string_of_result value
  | `Collection (coll_type, []) -> coll_prefix [] coll_type ^ "[]"
  | `Collection (`List, `Primitive(`Char _)::_) as c  -> "\"" ^ escape (charlist_as_string c) ^ "\""
  | `Collection (`List, (`Primitive(`XML _)::_ as elems))  -> String.concat "" (map string_of_xresult elems)
  | `Collection (coll_type, elems) -> coll_prefix [] coll_type ^ "[" ^ String.concat ", " (map string_of_result elems) ^ "]"
  | `Database (_, params) -> "(database " ^ params ^")"
  | `Environment (url, env) -> "Environment[" ^ url ^ "]: " ^ string_of_environment_ez env
  | `Continuation cont -> pp_continuation cont
and string_of_primitive : primitive -> string = function
  | `Bool value -> string_of_bool value
  | `Int value -> string_of_num value
  | `Float value -> string_of_float value
  | `Char c -> "'"^ Char.escaped c ^"'"
  | `XML x -> string_of_item x
  | `PFunction (name, _, _) -> name

and string_of_tuple (fields : (string * result) list) : string = 
    let fields = map (function
                        | x, y when numberp x  -> (int_of_string x, y)
                        | _ -> raise Not_tuple) fields in
    let sorted = sort (fun (x,_) (y, _) -> compare x y) fields in
    let numbers, values = split sorted in 
      if ordered_consecutive numbers && length numbers > 1 && hd numbers = 1 then
        "(" ^ String.concat ", " (map string_of_result values) ^ ")"
      else raise Not_tuple 

and numberp s = try ignore(int_of_string s); true with _ -> false

and string_of_binding : binding -> string = function (name, expr) ->
  name ^ " = " ^ (string_of_result expr)

and string_of_environment_ez : binding list -> string = fun env ->
                            String.concat ", " (map (string_of_binding) env)
and string_of_xresult = function 
  | `Primitive (`Char c) -> String.make 1 c
  | otherwise -> string_of_result otherwise

let rec serialise_primitive : primitive serialiser = 
  function
    | `Bool v      -> serialise1 'b' (serialise_bool) v
    | `Int v       -> serialise1 'i' (serialise_int) v
    | `Float v     -> serialise1 'f' (serialise_float) v
    | `Char v      -> serialise1 'c' (serialise_char) v
    | `PFunction (name, func, pargs) -> 
	serialise1 'p'
          (serialise_string)
	  (name)
    | `XML v       -> serialise1 'x' (serialise_item) v
and serialise_result : result serialiser = 
  let list, string = serialise_list, serialise_string in
    function
      | `Primitive v -> serialise1 'p' (serialise_primitive) v
          (* Remove self from bindings list to prevent infinite regress *)
      | `Function (var, locals, globals, expr) as f ->  
          (* I think this only works for top-level functions,
             anonymous functions and non-recursive local functions
             (i.e. everything except for recursive inner functions,
             which are hardly uncommon.  The business with names needs
             to be modified.  *)
          (try 
             (* temporarily remove filter to get a demo working *)
             let globals = globals (*List.filter (function (_, `Primitive _) -> false 
                                          | _ -> true) globals *) in
             let name = rassq f globals in
             let v = name, var, locals, Sl_utility.rremove_assq f globals, expr
             in
               serialise5 'f' (string, string, serialise_environment, 
                               serialise_environment, serialise_expression) v
           with Not_found ->
             serialise5 'f' (string, string, serialise_environment, 
                             serialise_environment, serialise_expression) 
               ("", var, locals, globals, expr))
      | `Record      v -> serialise1 'r' (list (serialise_binding)) v
      | `Variant     v -> serialise2 'v' (string, serialise_result) v
      | `Collection  v -> serialise2 'c' (serialise_colltype, list serialise_result) v
      | `Database    v -> serialise2 'd' (null_serialiser, string) v
      | `Environment v -> serialise2 'e' (null_serialiser, serialise_environment) v
      | `Continuation v -> serialise1 'C' (serialise_continuation) v
and serialise_binding b
    = serialise2 'B' (serialise_string, serialise_result) b
and serialise_environment env
    = serialise1 'E' (serialise_list serialise_binding) (minimize_env env)
and minimize_env env = 
  (* Make sure each name occurs at most once *)
  let rec aux = function
    | [], output -> rev output
    | (name, _) :: rest, output when mem_assoc name output -> aux (rest, output)
    | first :: rest, output -> aux (rest, (first :: output))
  in aux (env, [])

let rec deserialise_primitive (resolve : string -> result)  :  primitive deserialiser =
  (fun s -> let t, obj, rest = extract_object s in
  let r = 
    (match t with
       | 'b' -> `Bool (deserialise1 deserialise_bool obj)
       | 'i' -> `Int (deserialise1 deserialise_int obj)
       | 'f' -> `Float   (deserialise1 deserialise_float obj)
       | 'c' -> `Char    (deserialise1 deserialise_char obj)
       | 'p' -> (let name = 
                   deserialise1 (deserialise_string) obj 
                 in
                   match resolve name with
                     | `Primitive x -> x     (* FIXME: Include pargs*)
                     | _ -> failwith "Error resolving primitive name")
       | 'x' -> `XML     (deserialise1 deserialise_item obj)
       | _ -> failwith "Error deserialising primitive")
  in r, rest)
and deserialise_result resolve : result deserialiser =
  fun s -> 
    let t, obj, rest = extract_object s 
    and result = deserialise_result resolve
    and string = deserialise_string in
    let r = 
      (match t with  
         | 'p' -> `Primitive (deserialise1 (deserialise_primitive resolve) obj)
             (* Add the function back into its own environment *)
         | 'f' -> let name, var, locals, globals, body =
             (deserialise5 (string, string, 
                            deserialise_environment resolve, 
                            deserialise_environment resolve, 
                            deserialise_expression)
                obj) in
             (match name with 
                | "" -> `Function (var, locals, globals, body)
                | name -> (let rec f = `Function (var, locals, (name, f) :: globals, body) 
                           in f))
         | 'r' -> `Record (deserialise1 (deserialise_list (deserialise_binding resolve)) obj)
         | 'v' -> `Variant (deserialise2 (string, result) obj)
         | 'c' -> `Collection (deserialise2 (deserialise_colltype, deserialise_list (deserialise_result resolve)) obj)
         | 'd' -> let _, dbstring = deserialise2 (null_deserialiser (), string) obj in
	   let driver, params = parse_db_string dbstring in
             `Database (db_connect driver params)
	 | 'e' -> `Environment (deserialise2 (null_deserialiser "", deserialise_environment resolve) obj)
	 | 'C' -> `Continuation (deserialise1 (deserialise_continuation resolve) obj)
         | _ -> failwith ("Error deserialising result : unknown prefix " ^ (String.make 1 t)))
    in r, rest
and deserialise_binding resolve :  binding deserialiser = 
  fun s ->
      let t, obj, rest = extract_object s in
        match t with
          | 'B' -> deserialise2 (deserialise_string, deserialise_result resolve) obj, rest
          | x -> failwith ("Error deserialising binding header (expected 'B'; got '" ^ (String.make 1 x) ^ "')")
and deserialise_environment resolve : environment deserialiser
    = 
  fun s ->
      let t, obj, rest = extract_object s in
        match t with
          | 'E' -> deserialise1 (deserialise_list (deserialise_binding resolve)) obj, rest
          | x -> failwith ("Error deserialising environment header (expected 'E'; got '" ^ (String.make 1 x) ^ "')")

let label_of_expression expr =
  let (_, _, label) = expression_data expr in fromOption "TUNLABELED" label
    
let result_to_xml = function
  | `Primitive (`XML r) -> r
  | `Collection _ as r -> (charlist_as_string r) 
  | `Continuation cont -> Base64.encode(serialise_continuation cont)
  | _ -> "NOT IMPLEMENTED"


(* boxing and unboxing of primitive types *)
let box_bool b = `Primitive (`Bool b)
and unbox_bool : result -> bool   = function
  | `Primitive (`Bool b)  -> b | _ -> failwith "Type error unboxing bool"
and box_int i = `Primitive (`Int i)      
and unbox_int  : result -> num    = function
  | `Primitive (`Int i)   -> i | _ -> failwith "Type error unboxing int"
and box_float f = `Primitive (`Float f)  
and unbox_float : result -> float = function
  | `Primitive (`Float f) -> f | _ -> failwith "Type error unboxing float"
and box_char c = `Primitive (`Char c)    
and unbox_char :  result -> char = function
  | `Primitive (`Char f) -> f | _ -> failwith "Type error unboxing char"
and box_xml x = `Primitive (`XML x)      
and unbox_xml  :  result -> xmlitem = function
  | `Primitive (`XML x) -> x | _ -> failwith "Type error unboxing xml"
and box_string = string_as_charlist
and unbox_string = charlist_as_string

(* Retain only bindings in env named by members of `names' *)
let retain names env = filter (fun (x, _) -> mem x names) env
