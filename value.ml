(*pp deriving *)
open Utility
open Notfound

let serialiser = Settings.add_string ("serialiser", "Dump", `User)

class type otherfield =
object
  method show : string
end

module Show_otherfield =
  Deriving_Show.Defaults
    (struct
      type a = otherfield
      let format formatter obj = Format.pp_print_string formatter (obj # show)
     end)

type db_status = [ `QueryOk | `QueryError of string ]
  deriving (Show)

class virtual dbvalue = object
  method virtual status : db_status
  method virtual nfields : int
  method virtual ntuples : int
  method virtual fname : int -> string
  method virtual get_all_lst : string list list
  method virtual map : 'a. ((int -> string) -> 'a) -> 'a list
  method virtual map_array : 'a. (string array -> 'a) -> 'a list
  method virtual fold_array : 'a. (string array -> 'a -> 'a) -> 'a -> 'a
  method virtual getvalue : int -> int -> string
  method virtual error : string
end

class virtual database = object(self)
  method virtual driver_name : unit -> string
  method virtual escape_string : string -> string
  method virtual quote_field : string -> string
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

module Eq_database = Deriving_Eq.Eq_mutable(struct type a = database end)
module Typeable_database = Deriving_Typeable.Primitive_typeable
  (struct
    type t = database
    let magic = "database"
   end)
module Show_database = Deriving_Show.Show_unprintable(struct type a = database end)

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
  method quote_field = assert false
end

let _ = register_driver ("null", fun args -> new null_database, reconstruct_db_string ("null", args))

type xmlitem =   Text of string
               | Attr of (string * string)
               | Node of (string * xml)
and xml = xmlitem list
    deriving (Typeable, Show, Eq, Pickle, Dump)

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

type table = (database * string) * string * string list list * Types.row
  deriving (Show)

(* type number = num *)
(* module Show_number = Deriving_num.Show_num *)
(* module Typeable_number = Deriving_num.Typeable_num *)
(* module Eq_number = Deriving_num.Eq_num *)
(* module Pickle_number = Deriving_num.Pickle_num *)
(* module Dump_number = Deriving_num.Dump_num *)

type primitive_value_basis =  [
| `Bool of bool
| `Char of char
| `Float of float
| `Int of int
| `XML of xmlitem
| `String of string ]
  deriving (Show, Typeable, Eq, Pickle, Dump)

type primitive_value = [
| primitive_value_basis
| `Database of (database * string)
| `Table of table
]
  deriving (Show)

module Show_in_channel = Deriving_Show.Show_unprintable(struct type a = in_channel end)
module Show_out_channel = Deriving_Show.Show_unprintable(struct type a = out_channel end)
(* not so sure about these two... *)
module Eq_in_channel = Deriving_Eq.Eq_mutable(struct type a = in_channel end)
module Eq_out_channel = Deriving_Eq.Eq_mutable(struct type a = out_channel end)
module Typeable_in_channel = Deriving_Typeable.Primitive_typeable
  (struct
    type t = in_channel
    let magic = "in_channel"
   end)
module Typeable_out_channel = Deriving_Typeable.Primitive_typeable
  (struct
    type t = out_channel
    let magic = "out_channel"
   end)

(*jcheney: Added function component to PrimitiveFunction *)
type continuation = (Ir.scope * Ir.var * env * Ir.computation) list
and t = [
| primitive_value
| `List of t list
| `Record of (string * t) list
| `Variant of string * t
| `FunctionPtr of (Ir.var * t option)
| `PrimitiveFunction of string * Var.var option
| `ClientFunction of string
| `Continuation of continuation
| `Pid of int * Sugartypes.location
| `Socket of in_channel * out_channel
]
and env = (t * Ir.scope) Utility.intmap  * (t * Ir.scope) Utility.intmap
  deriving (Show)

let toplevel_cont : continuation = []

(** {1 Environment stuff} *)
(** {2 IntMap-based implementation with global memoization} *)

let empty_env = (IntMap.empty, IntMap.empty)
let bind name (v,scope) (env, globals) =
  (* Maintains globals as submap of global bindings. *)
  match scope with
    `Local -> (IntMap.add name (v,scope) env, globals)
  | `Global -> (IntMap.add name (v,scope) env, IntMap.add name (v,scope) globals)
let find name (env, _globals) = fst (IntMap.find name env)
let mem name (env, _globals) = IntMap.mem name env
let lookup name (env, _globals) = opt_map fst (IntMap.lookup name env)
let lookupS name (env, _globals) = IntMap.lookup name env
let extend env bs = IntMap.fold (fun k v r -> bind k v r) bs env

let get_parameters (env, _globals) = env

let shadow env ~by:(by, _globals') =
(* Assumes that globals never change *)
    IntMap.fold (fun name v env -> bind name v env) by env

let fold f (env, _globals) a = IntMap.fold f env a
let globals (env, genv) = (genv, genv)

(** {1 Compressed values for more efficient pickling} *)
type compressed_primitive_value = [
| primitive_value_basis
| `Table of string * string * string list list * string
| `Database of string
]
  deriving (Show, Eq, Typeable, Pickle, Dump)

type compressed_continuation = (Ir.var * compressed_env) list
and compressed_t = [
| compressed_primitive_value
| `List of compressed_t list
| `Record of (string * compressed_t) list
| `Variant of string * compressed_t
| `FunctionPtr of (Ir.var * compressed_t option)
| `PrimitiveFunction of string
| `ClientFunction of string
| `Continuation of compressed_continuation ]
and compressed_env = (Ir.var * compressed_t) list
  deriving (Show, Eq, Typeable, Dump, Pickle)

let compress_primitive_value : primitive_value -> [>compressed_primitive_value]=
  function
    | #primitive_value_basis as v -> v
    | `Table ((_database, db), table, keys, row) ->
        `Table (db, table, keys, Types.string_of_datatype (`Record row))
    | `Database (_database, s) -> `Database s

let localise env var =
  (* let module M = Deriving_Show.Show_option(Show_intset) in *)
  (* Debug.print ("cont vars (" ^ string_of_int var ^ "): " ^ *)
  (*              M.show (Tables.lookup Tables.cont_vars var)); *)
  IntSet.fold
    (fun name locals ->
       match lookupS name env with
         | None
         | Some (_, `Global) -> locals
         | Some (v, `Local) ->
             bind name (v, `Local) locals)
    (Tables.find Tables.cont_vars var)
    empty_env


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
      | `FunctionPtr(x, fvs) ->
        `FunctionPtr (x, opt_map compress_t fvs)
      | `PrimitiveFunction (f,_op) -> `PrimitiveFunction f
      | `ClientFunction f -> `ClientFunction f
      | `Continuation cont -> `Continuation (compress_continuation cont)
      | `Pid (pid, location) -> assert false
      | `Socket (inc, outc) -> assert false (* wheeee! *)
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

(* let string_of_value : t -> string = *)
(*   fun v -> Show.show show_compressed_t (compress_t v) *)

let uncompress_primitive_value : compressed_primitive_value -> [> primitive_value] =
  function
    | #primitive_value_basis as v -> v
    | `Table (db_name, table_name, keys, t) ->
        let row =
          match DesugarDatatypes.read ~aliases:DefaultAliases.alias_env t with
            | `Record row -> row
            | _ -> assert false in
        let driver, params = parse_db_string db_name in
        let database = db_connect driver params in
          `Table (database, table_name, keys, row)
    | `Database s ->
        let driver, params = parse_db_string s in
        let database = db_connect driver params in
          `Database database

let rec uncompress_continuation globals cont
    : continuation =
  List.map
    (fun (var, env) ->
       let scope = Tables.find Tables.scopes var in
       let body = Tables.find Tables.cont_defs var in
       let env = uncompress_env globals env in
       let locals = localise env var in
         (scope, var, locals, body))
    cont
and uncompress_t globals (v : compressed_t) : t =
  let uv = uncompress_t globals in
    match v with
      | #compressed_primitive_value as v -> uncompress_primitive_value v
      | `List vs -> `List (List.map uv vs)
      | `Record fields -> `Record (List.map (fun (name, v) -> (name, uv v)) fields)
      | `Variant (name, v) -> `Variant (name, uv v)
      | `FunctionPtr (x, fvs) -> `FunctionPtr (x, opt_map uv fvs)
      | `PrimitiveFunction f -> `PrimitiveFunction (f,None)
      | `ClientFunction f -> `ClientFunction f
      | `Continuation cont -> `Continuation (uncompress_continuation globals cont)
and uncompress_env globals env : env =
  try
  List.fold_left
    (fun env (name, v) ->
       bind name (uncompress_t globals v, Tables.find Tables.scopes name) env)
    empty_env
    env
  with NotFound str -> failwith("In uncompress_env: " ^ str)

let string_as_charlist s : t =
  `List (List.rev (List.rev_map (fun x -> `Char x) (explode s)))

let escape =
  Str.global_replace (Str.regexp "\\\"") "\\\"" (* FIXME: Can this be right? *)

(** {1 Pretty-printing values} *)

let string_of_cont = Show_continuation.show 

exception Not_tuple

exception Match of string

let rec char_of_primchar = function
    `Char c -> c
  | o ->
      raise (Match (Show_t.show o))

and charlist_as_string chlist =
  match chlist with
    | `List elems ->
        Utility.implode (List.rev (List.rev_map char_of_primchar elems))
    | _ -> raise (Match("Non-string " ^ string_of_value chlist
                        ^ " used as string."))

and string_of_value : t -> string = function
  | #primitive_value as p -> string_of_primitive p
  | `FunctionPtr (x, fvs) ->
    if Settings.get_value (Basicsettings.printing_functions) then
      string_of_int x ^ opt_app string_of_value "" fvs
    else
      "fun"
  | `PrimitiveFunction (name,_op) -> name
  | `ClientFunction (name) -> name
  (* | `RecFunction(defs, env, var, _scope) -> *)
  (*     (\* Choose from fancy or simple printing of functions: *\) *)
  (*     if Settings.get_value(Basicsettings.printing_functions) then *)
  (*       "{ " ^ (mapstrcat " " *)
  (*                 (fun (_name, (formals, body, _z)) -> *)
  (*                    "fun (" ^ String.concat "," (List.map Ir.string_of_var formals) ^ ") {" ^ *)
  (*                      Ir.string_of_computation body ^ "}") *)
  (*                 defs) ^ *)
  (*         " " ^ Ir.string_of_var var ^ " }[" ^ string_of_environment env ^ "]" *)
  (*     else *)
  (*       "fun" *)
  | `Record fields ->
      (try string_of_tuple fields
       with Not_tuple ->
         "(" ^ mapstrcat "," (fun (label, value) ->
                                label ^ "=" ^ string_of_value value)
           (List.sort (fun (l,_) (r, _) -> compare l r) fields) ^ ")")
  | `Variant (label, `Record []) -> label
  | `Variant (label, value) -> label ^ "(" ^ string_of_value value ^ ")"
  | `List [] -> "[]"
  | `List ((`XML _)::_ as elems) -> mapstrcat "" string_of_value elems
  | `List (elems) -> "[" ^ String.concat ", " (List.map string_of_value elems) ^ "]"
  | `Continuation cont -> "Continuation" ^ string_of_cont cont
  | `Pid (pid, location) -> string_of_int pid ^ "@" ^ Sugartypes.string_of_location location
  | `Socket (_, _) -> "<socket>"
and string_of_primitive : primitive_value -> string = function
  | `Bool value -> string_of_bool value
  | `Int value -> string_of_int value
  | `Float value -> string_of_float' value
  | `Char c -> "'"^ Char.escaped c ^"'"
  | `XML x -> string_of_item x
  | `Database (_, params) -> "(database " ^ params ^")"
  | `Table (_, table_name, _, _) -> "(table " ^ table_name ^")"
  | `String s -> "\"" ^ s ^ "\""

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

and string_of_cont : continuation -> string =
  fun cont ->
    let frame (_scope, var, _env, body) =
      "(" ^ string_of_int var ^ ", " ^ Ir.Show_computation.show body ^ ")"
    in
      "[" ^ mapstrcat ", " frame cont ^ "]"

(* New pretty printer
   ==================

   `string_of_value` is a bit overloaded. It seems to be used in a
   number of places for serialising values. It is also used for
   "pretty" printing results on the REPL.

   Ideally, we would use p_value for both purposes. We could use two
   formatters: one for plain, predictable serialisation to strings; a
   second for formatted printing with line breaks and indentation.
 *)
open Format

let rec p_value (ppf : formatter) : t -> 'a = function
  | `Bool true -> fprintf ppf "true"
  | `Bool false -> fprintf ppf "false"
  | `Int i -> fprintf ppf "%i" i
  | `Char c -> fprintf ppf "'%c'" c
  | `String s -> fprintf ppf "\"%s\"" s
  | `Record fields -> begin
      try p_tuple ppf fields
      with Not_tuple ->
        fprintf ppf "(@[<hv 0>%a@])" p_record_fields (List.sort (fun (l,_) (r, _) -> compare l r) fields) end
  | `List [] -> fprintf ppf "[]"
  | `List [v] -> fprintf ppf "[%a]" p_value v
  | `List l -> fprintf ppf "[@[<hov 0>";
               p_list_elements ppf l
  | `FunctionPtr (x, fvs) as v ->
     if Settings.get_value Basicsettings.printing_functions
     then failwith ("New pretty printer is incomplete :/ "^string_of_value v)
     else fprintf ppf "fun"
  | `ClientFunction n -> fprintf ppf "%s" n
  | v -> failwith ("New pretty printer is incomplete :/ "^string_of_value v)
  (* | _ -> fprintf ppf "not implemented" *)
and p_record_fields ppf = function
  | [] -> fprintf ppf ""
  | [(l, v)] -> fprintf ppf "@[%a = %a@]"
                        p_record_label l
                        p_value v
  | (l, v)::xs -> fprintf ppf "%a = %a,@ "
                          p_record_label l
                          p_value v;
                  p_record_fields ppf xs
and p_record_label ppf = function
  (* TODO can we get a list of keywords from the lexer? *)
  | "client" -> fprintf ppf "\"client\""
  | "table" -> fprintf ppf "\"table\""
  (* TODO labels with spaces and other "weird" characters that would confuse the lexer. *)
  | s -> fprintf ppf "%s" s
and p_list_elements ppf = function
  | [] -> assert false (* We only call this with lists of at least one element *)
  | [v] -> fprintf ppf "%a]@]" p_value v
  | v::vs -> fprintf ppf "%a,@ " p_value v;
             p_list_elements ppf vs
and p_tuple ppf (fields : (string * t) list) =
  let fields = List.map (function
                          | x, y when numberp x  -> (int_of_string x, y)
                          | _ -> raise Not_tuple) fields in
  let sorted = List.sort (fun (x,_) (y, _) -> compare x y) fields in
  let numbers, values = List.split sorted in
  if ordered_consecutive numbers && List.length numbers > 1 && List.hd numbers = 1 then
    fprintf ppf "(@[<hv 0>%a@])" p_tuple_elements values
  else raise Not_tuple
and p_tuple_elements ppf = function
  | [] -> assert false
  | [v] -> fprintf ppf "%a" p_value v
  | v::vs -> fprintf ppf "%a,@ " p_value v;
             p_tuple_elements ppf vs


let mypprint : t -> string = fun v ->
  (* TODO make this configurable / read actual terminal width *)
  pp_set_margin str_formatter (Settings.get_value Basicsettings.terminal_width);
  p_value str_formatter v;
  flush_str_formatter ()

let pprint_value v = if Settings.get_value Basicsettings.new_pretty_printer then mypprint v else string_of_value v

(* let string_of_cont : continuation -> string = *)
(*   fun cont -> Show.show show_compressed_continuation (compress_continuation cont) *)

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
and unbox_int  : t -> int    = function
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
and box_string s = `String s
and unbox_string : t -> string = function
  | `String s -> s
  | v ->
     failwith ("Type error unboxing string: " ^ string_of_value v)
and box_list l = `List l
and unbox_list : t -> t list = function
  | `List l -> l | _ -> failwith "Type error unboxing list"
and box_record fields = `Record fields
and unbox_record : t -> (string * t) list = function
  | `Record fields -> fields | _ -> failwith "Type error unboxing record"
and box_unit : unit -> t 
  = fun () -> `Record []
and unbox_unit : t -> unit = function
  | `Record [] -> () | _ -> failwith "Type error unboxing unit"
let box_pair : t -> t -> t = fun a b -> `Record [("1", a); ("2", b)]
let unbox_pair = function
  | (`Record [(_, a); (_, b)]) -> (a, b)
  | _ -> failwith ("Match failure in pair conversion")
let box_pid (pid, location) = `Pid (pid, `Unknown)
let unbox_pid = function
  | `Pid (pid, location) -> (pid, location)
  | _ -> failwith "Type error unboxing pid"
let box_socket (inc, outc) = `Socket (inc, outc)
let unbox_socket = function
  | `Socket p -> p
  | _ -> failwith "Type error unboxing socket"

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
  { save = Pickle_compressed_continuation.to_string ;
    load = Pickle_compressed_continuation.from_string };

  "Dump",
  { save = Dump_compressed_continuation.to_string ;
    load = Dump_compressed_continuation.from_string }
]

let value_serialisers : (string * compressed_t serialiser) list = [
  "Marshal",
  { save = marshal_save ; load = marshal_load };

  "Pickle",
  { save = Pickle_compressed_t.to_string ;
    load = Pickle_compressed_t.from_string };

  "Dump",
  { save = Dump_compressed_t.to_string ;
    load = Dump_compressed_t.from_string };
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
    (* Debug.print ("marshalling: "^Show_t.show v); *)
      base64encode (save (compress_t v))

let unmarshal_continuation env : string -> continuation =
    let { load = load } = continuation_serialiser () in
    base64decode ->- load ->- uncompress_continuation (globals env)

let unmarshal_value env : string -> t =
  fun s ->
    let { load = load } = value_serialiser () in
    (* Debug.print ("unmarshalling string: " ^ s); *)
    let v = (load (base64decode s)) in
    (* Debug.print ("unmarshalling: " ^ Show_compressed_t.show v); *)
      uncompress_t (globals env) v

(** Return the continuation frame that evaluates the given expression
    in the given environment. *)
let expr_to_contframe env expr =
  ((`Local        : Ir.scope),
   (Var.dummy_var : Ir.var),
   (env           : env),
   (([], expr)    : Ir.computation))

let rec value_of_xml xs = `List (List.map value_of_xmlitem xs)
and value_of_xmlitem =
  function
    | Text s -> `Variant ("Text", box_string s)
    | Attr (name, value) -> `Variant ("Attr", `Record [("1", box_string name); ("2", box_string value)])
    | Node (name, children) -> `Variant ("Node", `Record [("1", box_string name); ("2", value_of_xml children)])
