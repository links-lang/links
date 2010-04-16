(*pp deriving *)
open Num
open Utility
open Notfound

let serialiser = Settings.add_string ("serialiser", "Dump", `User)

class type otherfield = 
object 
  method show : string
end

let show_otherfield () = 
  { Show.format = fun formatter obj -> Format.pp_print_string formatter (obj # show) }

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

let eq_database = Eq.eq_mutable
let typeable_database  = { Typeable.type_rep = Typeable.TypeRep.mkFresh "database" [] }
let show_database = show_unprintable

(* Here we could do something better, like pickling enough information
   about the database to be able to restore the connection on
   deserialisation *)

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

let parse_db_string : string -> (string * string) = 
  fun params -> 
    match Str.bounded_split (Str.regexp ":") params 2 with
      | [hd; tail] -> (hd, tail)
      | _ -> failwith ("Could not parse db connection string : " ^ params)
and reconstruct_db_string : (string * string) -> string =
  fun (x,y) -> x ^ ":" ^ y

let database_connections = ref (StringMap.empty : (database * string) StringMap.t)

let db_connect driver params =
  let s = reconstruct_db_string (driver, params) in
  let constructor = 
    try List.assoc driver !database_drivers 
    with NotFound _ -> failwith ("No driver for database type `" ^ driver ^ "'")
  in
    match StringMap.lookup s !database_connections with
      | None ->
          let db = constructor params in
          database_connections := StringMap.add s db (!database_connections);
          db
      | Some db -> db

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
    deriving (Typeable, Show, Eq, Hash, Pickle, Dump)

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
  deriving (Show)    

type primitive_value_basis =  [
| `Bool of bool
| `Char of char
| `Float of float
| `Int of num
| `XML of xmlitem 
| `NativeString of string ]
  deriving (Show, Typeable, Eq, Hash, Pickle, Dump)

type primitive_value = [
| primitive_value_basis
| `Database of (database * string)
| `Table of table
]
  deriving (Show)
        
type continuation = (Ir.scope * Ir.var * env * Ir.computation) list
and t = [
| primitive_value
| `List of t list
| `Record of (string * t) list
| `Variant of string * t 
| `RecFunction of ((Ir.var * (Ir.var list * Ir.computation)) list *
                     env * Ir.var * Ir.scope)
| `FunctionPtr of (Ir.var * env)
| `PrimitiveFunction of string
| `ClientFunction of string
| `Continuation of continuation ]
and env = (t * Ir.scope) Utility.intmap * Ir.closures
(* and env = (int * (t * Ir.scope)) list * Ir.closures *)
  deriving (Show)

let toplevel_cont : continuation = []

(** {1 Environment stuff} *)
(** {2 IntMap-based implementation} *)
let empty_env closures = IntMap.empty, closures
let bind name v (env, closures) = IntMap.add name v env, closures
let find name (env, _closures) = fst (IntMap.find name env)
let lookup name (env, _closures) = opt_map fst (IntMap.lookup name env) 
let lookupS name (env, _closures) = IntMap.lookup name env
let extend env bs = IntMap.fold (fun k v r -> bind k v r) bs env
let shadow (outers, closures) ~by:(by, _closures') =
(* WARNING:
   The commented out code causes an enormous slowdown.
   (NOTE: This is not an acceptable description of the problem! --ez)
*)
(*   let closures = *)
(*     IntMap.fold *)
(*       (fun name xs closures -> *)
(*          IntMap.add name xs closures) *)
(*       closures' *)
(*       closures *)
(*   in *)
    IntMap.fold (fun name v env -> IntMap.add name v env) by outers, closures
let fold f (env, closures) a = IntMap.fold f env a
let globals (env, closures) =
  IntMap.fold (fun name ((_, scope) as v) globals ->
          match scope with
            | `Global -> IntMap.add name v globals
            | _ -> globals) env (IntMap.empty), closures
let get_closures (_, closures) = closures
let find_closure (_, closures) var = IntMap.find var closures
let with_closures (env, closures') closures =
  (env, IntMap.fold IntMap.add closures closures')

(** {2 List-based environment implementation (disabled)} *)
(* let empty_env closures = [], closures *)
(* let bind name v (env, closures) = (name, v)::env, closures *)
(* let find name (env, _closures) = *)
(*   fst (List.assoc name env) *)
(* let lookup name env = *)
(*   try Some (find name env) *)
(*   with NotFound _ -> None *)
(* let lookupS name (env, _closures) = *)
(*   try Some (List.assoc name env) *)
(*   with NotFound _ -> None *)
(* let shadow (outers, closures) ~by:(by, _closures') = *)
(*   by @ outers, closures *)
(* let fold f (env, _closures) a = List.fold_right (fun (name, v) -> f name v) env a *)
(* let globals (env, closures) = *)
(*   List.fold_right (fun (name, ((_, scope) as v)) globals -> *)
(*                      match scope with *)
(*                        | _ -> (name, v)::globals *)
(*                        | _ -> globals) env [], closures *)
(* let find_closure (_, closures) var = IntMap.find var closures *)
(* let with_closures (env, closures') closures = *)
(*   (env, IntMap.fold IntMap.add closures closures') *)


(** {1 Compressed values for more efficient pickling} *)
type compressed_primitive_value = [
| primitive_value_basis
| `Table of string * string * string
| `Database of string
]
  deriving (Show, Eq, Typeable, Hash, Pickle, Dump)

type compressed_continuation = (Ir.var * compressed_env) list
and compressed_t = [
| compressed_primitive_value
| `List of compressed_t list
| `Record of (string * compressed_t) list
| `Variant of string * compressed_t 
| `LocalFunction of (Ir.var list * compressed_env * Ir.var)
| `GlobalFunction of (Ir.var list * Ir.var)
| `PrimitiveFunction of string
| `ClientFunction of string
| `Continuation of compressed_continuation ]
and compressed_env = (Ir.var * compressed_t) list
  deriving (Show, Eq, Typeable, Dump, Hash, Pickle)

let compress_primitive_value : primitive_value -> [>compressed_primitive_value]=
  function
    | #primitive_value_basis as v -> v
    | `Table ((_database, db), table, row) ->
        `Table (db, table, Types.string_of_datatype (`Record row))
    | `Database (_database, s) -> `Database s

let localise env var =
  IntSet.fold
    (fun name locals ->
       match lookupS name env with
         | None
         | Some (_, `Global) -> locals
         | Some (v, `Local) ->
             bind name (v, `Local) locals)
    (find_closure env var)
    (empty_env (get_closures env))

let rec compress_continuation (cont:continuation) : compressed_continuation =
  List.map
    (fun (_scope, var, locals, _body) ->
       (var, compress_env locals)) cont
and compress_t (v : t) : compressed_t =
  let cv = compress_t in
    match v with
      | #primitive_value as v -> compress_primitive_value v
      | `List vs -> `List (List.map cv vs)
      | `Record fields -> `Record(List.map(fun(name, v) -> (name, cv v)) fields)
      | `Variant (name, v) -> `Variant (name, cv v)
      | `FunctionPtr(x, env) -> assert false    (* Should already be resolved. *)
      | `RecFunction (defs, locals, f, `Local) ->
          `LocalFunction (List.map (fun (f, (_xs, _body)) -> f) defs, 
                          compress_env locals, f)
      | `RecFunction (defs, _env, f, `Global) ->
          `GlobalFunction (List.map (fun (f, _) -> f) defs, f)
      | `PrimitiveFunction f -> `PrimitiveFunction f
      | `ClientFunction f -> `ClientFunction f
      | `Continuation cont -> `Continuation (compress_continuation cont)
and compress_env env : compressed_env =
  List.rev
    (fold
       (fun name (v, scope) compressed ->
          if scope = `Global then
            compressed
          else
            (name, compress_t v)::compressed)
       env
       [])

type unmarshal_envs =
    env * Ir.scope IntMap.t *
      Ir.computation IntMap.t * 
      (Ir.var list * Ir.computation) IntMap.t

let uncompress_primitive_value : compressed_primitive_value -> [> primitive_value] =
  function
    | #primitive_value_basis as v -> v
    | `Table (db_name, table_name, t) ->
        let row =
          match DesugarDatatypes.read ~aliases:DefaultAliases.alias_env t with
            | `Record row -> row
            | _ -> assert false in
        let driver, params = parse_db_string db_name in
        let database = db_connect driver params in
          `Table (database, table_name, row)
    | `Database s ->
        let driver, params = parse_db_string s in
        let database = db_connect driver params in
          `Database database

let rec uncompress_continuation ((_globals, scopes, conts, _funs) as envs) cont
    : continuation =
  List.map
    (fun (var, env) ->
       let scope = IntMap.find var scopes in
       let body = IntMap.find var conts in
       let env = uncompress_env envs env in
       let locals = localise env var in
         (scope, var, locals, body))
    cont
and uncompress_t ((globals, _scopes, _conts, funs) as envs:unmarshal_envs) v : t =
  let uv = uncompress_t envs in
    match v with
      | #compressed_primitive_value as v -> uncompress_primitive_value v
      | `List vs -> `List (List.map uv vs)
      | `Record fields -> `Record (List.map (fun (name, v) -> (name, uv v)) fields)
      | `Variant (name, v) -> `Variant (name, uv v)
      | `LocalFunction (defs, env, var) ->
          `RecFunction (List.map (fun f -> f, IntMap.find f funs) defs,
                        uncompress_env envs env,
                        var,
                        `Local)
      | `GlobalFunction (defs, f) ->
          `RecFunction (List.map (fun f -> f, IntMap.find f funs) defs,
                        localise globals f,
                        f,
                        `Global)
      | `PrimitiveFunction f -> `PrimitiveFunction f
      | `ClientFunction f -> `ClientFunction f
      | `Continuation cont -> `Continuation (uncompress_continuation envs cont)
and uncompress_env ((globals, scopes, _conts, _funs) as envs) env : env =
  try
  List.fold_left
    (fun env (name, v) ->
       bind name (uncompress_t envs v, IntMap.find name scopes) env)
    (empty_env (get_closures globals))
    env
  with NotFound str -> failwith("In uncompress_env: " ^ str)

let build_unmarshal_envs ((venv, closures), nenv, tyenv) program
    : unmarshal_envs =
  let tyenv =
    try
      Env.String.fold
        (fun name t tyenv-> Env.Int.bind tyenv (Env.String.lookup nenv name, t))
        tyenv.Types.var_env
        Env.Int.empty 
    with NotFound str -> failwith("In build_unmarshal_envs: " ^ str)
  in
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

    method get_envs = (scopes, conts, funs)
  end in
  let _, _, o = build#computation program in
  let scopes, conts, funs = o#get_envs in
  let globals = globals (venv, closures)
(*   let globals = *)
(*     ((IntMap.fold *)
(*         (fun name (v, scope) env -> *)
(*            if scope = `Global then *)
(*              IntMap.add name (v, scope) env *)
(*            else *)
(*              env) *)
(*         venv *)
(*         IntMap.empty), *)
(*      closures) *)
  in
    globals, scopes, conts, funs

let string_as_charlist s : t =
  `List (List.rev (List.rev_map (fun x -> `Char x) (explode s)))

let escape = 
  Str.global_replace (Str.regexp "\\\"") "\\\"" (* FIXME: Can this be right? *)

(** {1 Pretty-printing values} *)

let string_of_cont = Show.show show_continuation

exception Not_tuple

exception Match of string

let rec char_of_primchar = function 
    `Char c -> c
  | o ->
      raise (Match (Show.show show_t o))

and charlist_as_string chlist = 
  match chlist with
    | `List elems -> 
        Utility.implode (List.rev (List.rev_map char_of_primchar elems))
    | _ -> raise (Match("Non-string " ^ string_of_value chlist
                        ^ " used as string."))

and string_of_value : t -> string = function
  | #primitive_value as p -> string_of_primitive p
  | `FunctionPtr (x, env) -> string_of_int x ^ string_of_environment env
  | `PrimitiveFunction (name) -> name
  | `ClientFunction (name) -> name
  | `RecFunction(defs, env, var, _scope) -> 
      (* Choose from fancy or simple printing of functions: *)
      if Settings.get_value(Basicsettings.printing_functions) then
        "{ " ^ (mapstrcat " "
                  (fun (_name, (formals, body)) ->
                     "fun (" ^ String.concat "," (List.map Ir.string_of_var formals) ^ ") {" ^
                       Ir.string_of_computation body ^ "}")
                  defs) ^
          " " ^ Ir.string_of_var var ^ " }[" ^ string_of_environment env ^ "]"
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

(* TBD: Can someone explain how this works? *)
and string_of_element_value = function 
  | `Char c -> String.make 1 c
  | otherwise -> string_of_value otherwise

(** {1 Record manipulations} *)

(** [project field value] returns projects the field labeled [field] 
    from the Links value [value], provided [value] is a record. *)
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

(** {1 Boxing and unboxing of primitive types} *)
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

let intmap_of_record = function
  | `Record members ->
      Some(IntMap.from_alist(
             List.map (fun (k,v) -> int_of_string k, v ) members))
  | _ -> None

type 'a serialiser = {
  save : 'a -> string;
  load : string -> 'a;
}

(** {1 Serialization of values. } *)

let marshal_save : 'a -> string = fun v -> Marshal.to_string v []
and marshal_load : string -> 'a = fun v -> Marshal.from_string v 0

let continuation_serialisers : (string * compressed_continuation serialiser) list = [
  "Marshal",
  { save = marshal_save ; load = marshal_load };
  
  "Pickle",
  { save = Pickle.to_string pickle_compressed_continuation ; load = Pickle.from_string pickle_compressed_continuation };

  "Dump",
  { save = Dump.to_string dump_compressed_continuation ; load = Dump.from_string dump_compressed_continuation }
]

let value_serialisers : (string * compressed_t serialiser) list = [
  "Marshal",
  { save = marshal_save ; load = marshal_load };
  
  "Pickle",
  { save = Pickle.to_string pickle_compressed_t ; load = Pickle.from_string pickle_compressed_t };

  "Dump",
  { save = Dump.to_string dump_compressed_t ; load = Dump.from_string dump_compressed_t };
]

let retrieve_serialiser : (string * 'a serialiser) list -> 'a serialiser =
  fun serialisers ->
    let name = Settings.get_value serialiser in
    try List.assoc name serialisers
    with NotFound _ -> failwith ("Unknown serialisation method : " ^ name)

let continuation_serialiser : unit -> compressed_continuation serialiser =
  fun () -> retrieve_serialiser continuation_serialisers

let value_serialiser : unit -> compressed_t serialiser =
  fun () -> retrieve_serialiser value_serialisers

let marshal_continuation (c : continuation) : string = 
  let cs = compress_continuation c in
  let { save = save } = continuation_serialiser () in
  let pickle = save cs in
    if String.length pickle > 4096 then 
      prerr_endline "Marshalled continuation larger than 4K:";
    base64encode pickle

let marshal_value : t -> string =
  fun v ->
    let { save = save } = value_serialiser () in
      base64encode (save (compress_t v))

let unmarshal_continuation (envs : unmarshal_envs) : string -> continuation =
    let { load = load } = continuation_serialiser () in
    base64decode ->- load ->- uncompress_continuation envs

let unmarshal_value envs : string -> t =
  fun s ->
    let { load = load } = value_serialiser () in
      uncompress_t envs (load (base64decode s))

(** Return the continuation frame that evaluates the given expression
    in the given environment. *)
let expr_to_contframe env expr =
  ((`Local        : Ir.scope),
   (Var.dummy_var : Ir.var),
   (env           : env),
   (([], expr)    : Ir.computation))
