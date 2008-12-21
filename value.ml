(*pp deriving *)
open Num
open Utility

exception Runtime_error of string

class type otherfield = 
object 
  method show : string
end

module Show_otherfield = Show.ShowDefaults(
  struct
    type a = otherfield
    let format formatter obj = Format.pp_print_string formatter (obj # show)
  end)

type db_status = [ `QueryOk | `QueryError of string ]
  deriving (Show)

class virtual dbvalue = object
  method virtual status : db_status
  method virtual nfields : int
  method virtual fname : int -> string
  method virtual get_all_lst : string list list
  method virtual error : string
end

class virtual database = object(self)
  method virtual driver_name : unit -> string
  method virtual escape_string : string -> string
  method virtual exec : string -> dbvalue
  method make_insert_query : (string * string list * string list list) -> string =
    fun (table_name, field_names, vss) ->
      "insert into " ^ table_name ^
        "("^String.concat "," field_names ^") values "^
        String.concat "," (List.map (fun vs -> "(" ^ String.concat "," vs ^")") vss)
  method make_insert_returning_query : (string * string list * string list list * string) -> string list =
    fun _ ->
      failwith ("insert ... returning is not yet implemented for the database driver: "^self#driver_name())
end

module Eq_database = Eq.Eq_mutable(struct type a = database end)
module Typeable_database = Typeable.Primitive_typeable(struct type t = database end)
module Show_database = Show_unprintable (struct type a = database end)

(* Here we could do something better, like pickling enough information
   about the database to be able to restore the connection on
   deserialisation *)
module Pickle_database = Pickle.Pickle_unpicklable (struct type a = database let tname = "Value.database" end)
module Shelve_database : Shelve.Shelve with type a = database = 
struct
  module Typeable = Typeable_database
  module Eq = Eq_database
  type a = database
  let shelve _ = failwith "shelve database nyi"
end

type db_constructor = string -> (database * string)

(** {1 Database Drivers and Values} *)

(** [database_drivers]: a list of available database drivers.  Each
    driver registers itself at load time: the contents of the list
    depends on which drivers are built.. *)

let database_drivers = ref ([] : (string * db_constructor) list)

(** [register_driver (name, driver)] registers a DB [driver] under the
    name [name]. *[driver] is a function taking the database params (a
    string with db-specific colon-separated fields) to a pair of
    database object and params (I guess the params could be modified
    by the driver?) *)
let register_driver : (string * db_constructor) -> unit
  = fun ((name, _) as pair) -> 
    Debug.print ("registering driver for " ^ name);
    database_drivers := pair :: !database_drivers

let db_connect driver params =
  let constructor = 
    try List.assoc driver !database_drivers 
    with NotFound _ -> failwith ("No driver for database type `" ^ driver ^ "'")
  in constructor params

let parse_db_string : string -> (string * string) = 
  fun params -> 
    match Str.bounded_split (Str.regexp ":") params 2 with
      | [hd; tail] -> (hd, tail)
      | _ -> failwith ("Could not parse db connection string : " ^ params)
and reconstruct_db_string : (string * string) -> string =
  fun (x,y) -> x ^ ":" ^ y

class null_database =
object
  inherit database
  method driver_name () = "null"
  method exec query : dbvalue = assert false
  method escape_string = assert false
end

let _ = register_driver ("null", fun args -> new null_database, reconstruct_db_string ("null", args))

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

type table = (database * string) * string * Types.row
  deriving (Show, Pickle)    

type primitive_value_basis =  [
| `Bool of bool
| `Char of char
| `Table of table
| `Float of float
| `Int of num
| `XML of xmlitem 
| `NativeString of string ]
  deriving (Show, Pickle)

type primitive_value = [
| primitive_value_basis
| `Database of (database * string)
]
  deriving (Show, Pickle)
        
type continuation = (Ir.scope * Ir.var * env * Ir.computation) list
and t = [
| primitive_value
| `List of t list
| `Record of (string * t) list
| `Variant of string * t 
| `RecFunction of ((Ir.var * (Ir.var list * Ir.computation)) list * env * Ir.var)
| `PrimitiveFunction of string
| `ClientFunction of string
| `Continuation of continuation ]
and env = (t * Ir.scope) Utility.intmap * Ir.closures
  deriving (Show, Pickle)

(* compressed types for more efficient pickling *)
type compressed_primitive_value = [
| primitive_value_basis
| `Database of string
]
  deriving (Show, Pickle)

type compressed_continuation = (Ir.var * compressed_env) list
and compressed_t = [
| compressed_primitive_value
| `List of compressed_t list
| `Record of (string * compressed_t) list
| `Variant of string * compressed_t 
| `RecFunction of (Ir.var list * compressed_env * Ir.var)
| `PrimitiveFunction of string
| `ClientFunction of string
| `Continuation of compressed_continuation ]
and compressed_env = (Ir.var * compressed_t) list
  deriving (Show, Pickle)

let compress_primitive_value : primitive_value -> [> compressed_primitive_value] =
  function
    | #primitive_value_basis as v -> v
    | `Database (_database, s) -> `Database s

let rec compress_continuation cont : compressed_continuation =
  List.map (fun (_scope, var, env, _body) -> (var, compress_env env)) cont
and compress_t v : compressed_t =
  let cv = compress_t in
    match v with
      | #primitive_value as v -> compress_primitive_value v
      | `List vs -> `List (List.map cv vs)
      | `Record fields -> `Record (List.map (fun (name, v) -> (name, cv v)) fields)
      | `Variant (name, v) -> `Variant (name, cv v)
      | `RecFunction (defs, env, f) ->
          let valenv, closures = env in
          let locals =
            List.fold_left
              (fun locals (f, (_xs, _body)) ->
                 IntSet.fold
                   (fun name locals ->
                      if IntMap.mem name valenv then
                        IntMap.add name (IntMap.find name valenv) locals
                      else
                        locals)
                   (IntMap.find f closures)
                   locals)
              IntMap.empty
              defs
          in                                                 
            `RecFunction (List.map (fun (f, (_xs, _body)) -> f) defs, compress_env (locals, closures), f)
      | `PrimitiveFunction f -> `PrimitiveFunction f
      | `ClientFunction f -> `ClientFunction f
      | `Continuation cont -> `Continuation (compress_continuation cont)
and compress_env (env, closures) : compressed_env =
  List.rev
    (IntMap.fold
       (fun name (v, _scope) compressed ->
          (name, compress_t v)::compressed)
       env
       [])

let uncompress_primitive_value : compressed_primitive_value -> [> primitive_value] =
  function
    | #primitive_value_basis as v -> v
    | `Database s ->
        let driver, params = parse_db_string s in
        let database = db_connect driver params in 
          `Database database

let rec uncompress_continuation (closures, scopes, conts, recs) cont : continuation =
  List.map
    (fun (var, env) ->
       let scope = IntMap.find var scopes in
       let body = IntMap.find var conts in
       let env = uncompress_env (closures, scopes, conts, recs) env in
         (scope, var, env, body))
    cont
and uncompress_t (closures, scopes, conts, funs) v : t =
  let uv = uncompress_t (closures, scopes, conts, funs) in
    match v with
      | #compressed_primitive_value as v -> uncompress_primitive_value v
      | `List vs -> `List (List.map uv vs)
      | `Record fields -> `Record (List.map (fun (name, v) -> (name, uv v)) fields)
      | `Variant (name, v) -> `Variant (name, uv v)
      | `RecFunction (defs, env, var) ->
          `RecFunction (List.map (fun f -> (f, IntMap.find var funs)) defs,
                        uncompress_env (closures, scopes, conts, funs) env,
                        var)
      | `PrimitiveFunction f -> `PrimitiveFunction f
      | `ClientFunction f -> `ClientFunction f
      | `Continuation cont -> `Continuation (uncompress_continuation (closures, scopes, conts, funs) cont)
and uncompress_env (closures, scopes, conts, funs) env : env =
  (List.fold_left
     (fun env (name, v) ->
        IntMap.add name (uncompress_t (closures, scopes, conts, funs) v, IntMap.find name scopes) env)
     IntMap.empty
     env), closures

type unmarshal_envs = Ir.ClosureTable.t * Ir.scope IntMap.t * Ir.computation IntMap.t * (Ir.var list * Ir.computation) IntMap.t

let build_unmarshal_envs ((_, closures), nenv, tyenv) program : unmarshal_envs =
  let tyenv =
    Env.String.fold
      (fun name t tyenv -> Env.Int.bind tyenv (Env.String.lookup nenv name, t))    
      tyenv.Types.var_env
      Env.Int.empty in
  let build =
  object (o)
    inherit Ir.Transform.visitor(tyenv) as super
      
    val scopes = IntMap.empty
    val conts = IntMap.empty
    val funs = IntMap.empty

    method with_scopes scopes =
      {< scopes = scopes >}

    method with_conts conts =
      {< conts = conts >}

    method with_funs funs =
      {< funs = funs >}
  
    method bind_scope xb =
      let x = Var.var_of_binder xb in
      let scopes = IntMap.add x (Var.scope_of_binder xb) scopes in
        o#with_scopes scopes

    method bind_cont (xb, e) =
      let x = Var.var_of_binder xb in
      let conts = IntMap.add x e conts in
        o#with_conts conts

    method bind_fun (fb, (xsb, e)) =
      let f = Var.var_of_binder fb in
      let xs = List.map Var.var_of_binder xsb in
      let funs = IntMap.add f (xs, e) funs in
        o#with_funs funs

    method binder =
      fun b ->
        let b, o = super#binder b in
          b, o#bind_scope b

    method computation =
      fun e ->
        let (bs, main), t, o = super#computation e in
        let rec bind o =
          function
            | [] -> o
            | (`Let (x, (_tyvars, e)))::bs ->
                bind (o#bind_cont (x, (bs, main))) bs
            | (`Fun (f, (_tyvars, xs, e), _))::bs ->
                bind (o#bind_fun (f, (xs, e))) bs
            | (`Rec defs)::bs ->
                let o =
                  List.fold_left
                    (fun o (f, (_tyvars, xs, e), _) ->
                       o#bind_fun (f, (xs, e)))
                    o
                    defs
                in
                  bind o bs
            | _::bs -> bind o bs
        in
          (bs, main), t, bind o bs

    method get_envs = (closures, scopes, conts, funs)
  end in
  let _, _, o = build#computation program in
    o#get_envs

let toplevel_cont : continuation = []

let empty_env = IntMap.empty, IntMap.empty
let bind name v (env, closures) = IntMap.add name v env, closures
let find name (env, _closures) = fst (IntMap.find name env)
let lookup name (env, _closures) = opt_map fst (IntMap.lookup name env)
let shadow (outers, closures) ~by:(by, closures') =
  let closures =
    IntMap.fold
      (fun name xs closures ->
         IntMap.add name xs closures)
      closures'
      closures
  in
    IntMap.fold (fun name v env -> IntMap.add name v env) by outers, closures
let globals (env, closures) =
  IntMap.fold (fun name ((_, scope) as v) globals ->
                 match scope with
                   | _ -> IntMap.add name v globals
                   | _ -> globals) env (IntMap.empty), closures
let with_closures (env, closures') closures =
  (env, IntMap.fold IntMap.add closures closures')

let string_as_charlist s : t =
  `List (List.map (fun x -> (`Char x)) (explode s))

exception Match of string

let escape = 
  Str.global_replace (Str.regexp "\\\"") "\\\""

let string_of_cont = Show_continuation.show

exception Not_tuple

let rec char_of_primchar = function 
    `Char c -> c
  | o ->
      raise (Match (Show_t.show o))

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

let links_fst x = fst (unbox_pair x)
let links_snd x = snd (unbox_pair x)

let links_project name = function
  | (`Record fields) -> List.assoc name fields
  | _ -> failwith ("Match failure in record projection")

let marshal_continuation (c : continuation) : string = 
  let pickle = Pickle_compressed_continuation.pickleS (compress_continuation c) in
    Debug.print("marshalled continuation size: " ^
                  string_of_int(String.length pickle));
    if (String.length pickle > 4096) then (
      prerr_endline "Marshalled continuation larger than 4K:";
      Debug.print("marshalling:"^ string_of_cont c)
    );
    let result = base64encode pickle in
      result

let marshal_value : t -> string =
  (fun v ->
(*      Debug.print "marshalling value"; *)
     let compressed_v = compress_t v in
     let r =
       (compress_t ->- Pickle_compressed_t.pickleS ->- base64encode)(v) in
(*        Debug.print "marshalled value"; *)
       r)

exception UnrealizableContinuation

let unmarshal_continuation (envs : unmarshal_envs) : string -> continuation =
  base64decode
  ->- Pickle_compressed_continuation.unpickleS
  ->- (uncompress_continuation envs)

let unmarshal_value envs : string -> t =
  base64decode
  ->- Pickle_compressed_t.unpickleS
  ->- (uncompress_t envs)

let minimize _ = assert false
