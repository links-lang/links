(*pp deriving *)
open Utility
open Notfound
open ProcessTypes

let serialiser = Basicsettings.Serialisation.serialiser

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

class virtual dbvalue = object (self)
  method virtual status : db_status
  method virtual nfields : int
  method virtual ntuples : int
  method virtual fname : int -> string
  method virtual get_all_lst : string list list
  method map : 'a. ((int -> string) -> 'a) -> 'a list = fun f ->
      let max = self#ntuples in
      let rec do_map n acc =
	if n < max
	then (
	  do_map (n+1) (f (self#getvalue n)::acc)
	 )
	else acc
      in do_map 0 []
  method map_array : 'a. (string array -> 'a) -> 'a list = fun f ->
      let max = self#ntuples in
      let rec do_map n acc =
	if n < max
	then (
	  do_map (n+1) (f (self#gettuple n)::acc)
	 )
	else acc
      in do_map 0 []
  method fold_array : 'a. (string array -> 'a -> 'a) -> 'a -> 'a = fun f x ->
      let max = self#ntuples in
      let rec do_fold n acc =
	if n < max
	then (
	  do_fold (n+1) (f (self#gettuple n) acc)
	 )
	else acc
      in do_fold 0 x	    method virtual map : 'a. ((int -> string) -> 'a) -> 'a list
  method virtual getvalue : int -> int -> string
  method virtual gettuple : int -> string array
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
  method exec _query : dbvalue = assert false
  method escape_string = assert false
  method quote_field = assert false
end

let _ = register_driver ("null", fun args -> new null_database, reconstruct_db_string ("null", args))

type xmlitem =   Text of string
               | Attr of (string * string)
               | Node of (string * xml)
               | NsAttr of (string * string * string)
               | NsNode of (string * string * xml)
and xml = xmlitem list
    deriving (Typeable, Show, Eq, Pickle, Dump)


let is_attr = function
  | Attr _   -> true
  | NsAttr _ -> true
  | _        -> false

let attrs = List.filter is_attr
and nodes = List.filter (not -<- is_attr)

(* split top-level HTML into head and body components *)
let split_html : xml -> xml * xml =
  function
  | [Node ("html", xs)] ->
    List.fold_left
      (fun (hs, bs) ->
         function
         | Node ("body", ys) ->
           hs, bs @ ys
         | Node ("head", ys) ->
           hs @ ys, bs
         | x -> hs, bs @ [x])
      ([], []) xs
  | [Node ("body", xs)] -> [], xs
  | xs -> [], xs

type table = (database * string) * string * string list list * Types.row
  deriving (Show)

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

type spawn_location = [
  | `ClientSpawnLoc of client_id
  | `ServerSpawnLoc (* Will need to add in a server address when we go to n-tier *)
]
  deriving (Show)

type dist_pid = [
  | `ClientPid of (client_id * process_id)
  | `ServerPid of process_id (* Again, will need a server address here later *)
]
  deriving (Show)

type access_point = [
  | `ClientAccessPoint of (client_id * apid)
  | `ServerAccessPoint of apid
]
  deriving (Show)

type chan = (channel_id * channel_id)
  deriving (Show)

module type ENV =
sig
  type 'a t
     deriving (Show)
  val set_request_data : 'a t -> RequestData.request_data -> 'a t
  val request_data : 'a t -> RequestData.request_data
  val empty : 'a t
  val bind  : Ir.var -> ('a * Ir.scope) -> 'a t -> 'a t
  val find : Ir.var -> 'a t -> 'a
  val mem : Ir.var -> 'a t -> bool
  val lookup : Ir.var -> 'a t -> 'a option
  val lookupS : Ir.var -> 'a t -> ('a * Ir.scope) option
  val shadow : 'a t -> by:'a t -> 'a t
  val fold : (Ir.var -> ('a * Ir.scope) -> 'a -> 'a) -> 'a t -> 'a -> 'a
  val globals : 'a t -> 'a t
  (* used only by json.ml, webif.ml ... *)
  val get_parameters : 'a t -> ('a * Ir.scope) Utility.intmap
  val extend : 'a t -> ('a * Ir.scope) Utility.intmap -> 'a t
  val localise : 'a t -> Ir.var -> 'a t
end

module Env = struct
  type 'a t = {
      all : ('a * Ir.scope) Utility.intmap;
      globals : ('a * Ir.scope) Utility.intmap;
      request_data : RequestData.request_data
    }
    deriving (Show)

  let set_request_data env rd = { env with request_data = rd }
  let request_data env = env.request_data
  (** {1 Environment stuff} *)
  (** {2 IntMap-based implementation with global memoization} *)

  let empty = {
      all = IntMap.empty;
      globals = IntMap.empty;
      request_data = RequestData.new_empty_request_data ()
    }
  let bind name (v,scope) env =
    (* Maintains globals as submap of global bindings. *)
    match scope with
      `Local ->
      { env with all = IntMap.add name (v,scope) env.all }
    | `Global ->
       { env with
         all = IntMap.add name (v,scope) env.all;
         globals = IntMap.add name (v,scope) env.globals;
       }
  let find name env = fst (IntMap.find name env.all)
  let mem name env = IntMap.mem name env.all
  let lookup name env = opt_map fst (IntMap.lookup name env.all)
  let lookupS name env = IntMap.lookup name env.all
  let extend env bs = IntMap.fold (fun k v r -> bind k v r) bs env

  let get_parameters env = env.all

  let shadow env ~by:env_by =
    (* Assumes that globals never change *)
    let by_all = env_by.all in
    IntMap.fold (fun name v env -> bind name v env) by_all env

  let fold f env a = IntMap.fold f env.all a
  let globals env = { env with all = env.globals }

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
      empty

  (** Compression **)
  type 'cv compressed_t = (Ir.var * 'cv) list
        deriving (Show, Eq, Typeable, Dump, Pickle)
  let compress (compress_val : 'v -> 'cv) (env : 'v t)  : 'cv compressed_t =
    List.rev
    (fold
       (fun name (v, scope) compressed ->
          if scope = `Global then
            compressed
          else
            (name, compress_val v)::compressed)
       env
       [])

  let uncompress (uncompress_val : 'v t -> 'cv -> 'v) globals (env : 'cv compressed_t) : 'v t =
    try
      List.fold_left
        (fun env (name, v) ->
          bind name (uncompress_val globals v, Tables.find Tables.scopes name) env)
        empty
        env
    with NotFound str -> failwith("In uncompress_env: " ^ str)
end

module type FRAME = sig
  type 'v t

  val of_expr : 'v Env.t -> Ir.tail_computation -> 'v t
  val make : Ir.scope -> Ir.var -> 'v Env.t -> Ir.computation -> 'v t
end

module Frame = struct
  type 'v t = Ir.scope * Ir.var * 'v Env.t * Ir.computation
     deriving (Show)

  let make scope var env comp = (scope, var, env, comp)
  let of_expr env tc =
    make (`Local : Ir.scope) (Var.dummy_var : Ir.var) env (([], tc) : Ir.computation)

  (** Compression **)
  type 'cv compressed_t = Ir.var * 'cv Env.compressed_t
     deriving (Show, Eq, Typeable, Dump, Pickle)
  let compress (compress_val : 'v -> 'cv) (_, var, locals, _) : 'cv compressed_t =
    (var, Env.compress compress_val locals)
  let uncompress (uncompress_val : 'v Env.t -> 'cv -> 'v) globals (var, env) : 'v t =
    let scope = Tables.find Tables.scopes var in
    let body = Tables.find Tables.cont_defs var in
    let env = Env.uncompress uncompress_val globals env in
    let locals = Env.localise env var in
    (scope, var, locals, body)
end

module type CONTINUATION_EVALUATOR = sig
  type v
  type result
  type 'v t

  val apply : env:v Env.t ->            (* the current environment *)
              v t ->                    (* the continuation *)
              v ->                      (* the argument *)
              result

  val trap : v t ->                        (* the continuation *)
             (Ir.name * v) ->              (* operation name and its argument *)
             result
end

module type EVAL = sig
  type 'v t
  type v
  type result
  val error : string -> 'a
  val computation : v Env.t -> v t -> Ir.computation -> result
  val finish : v Env.t -> v -> result
  val reify : v t -> v
  end

module type CONTINUATION = sig
  type 'v t
     deriving (Show)

  module Frame : FRAME
  module Handler : sig
      type 'v t

      val make : env:'v Env.t -> clauses:Ir.clause Ir.name_map -> depth:[`Deep | `Shallow] -> 'v t
  end

  (* A continuation has a monoidal structure *)
  val empty : 'v t
  val (<>)  : 'v t -> 'v t -> 'v t          (* continuation composition *)
  val (&>)  : 'v Frame.t -> 'v t -> 'v t    (* continuation augmentation *)

  val set_trap_point : handler:'v Handler.t -> 'v t -> 'v t  (* installs a handler *)

  module Evaluation :
  functor(E : EVAL with type 'v t := 'v t) ->
  sig
    include CONTINUATION_EVALUATOR with type v = E.v and type result = E.result and type 'v t := 'v t
  end

  val to_string : 'v t -> string
end

module type COMPRESSABLE_CONTINUATION = sig
  include CONTINUATION

  type 'cv compressed_t
     deriving (Show, Eq, Typeable, Dump, Pickle)
  val compress : compress_val:('v -> 'cv) -> 'v t -> 'cv compressed_t
  val uncompress : uncompress_val:('v Env.t -> 'cv -> 'v) -> 'v Env.t -> 'cv compressed_t -> 'v t
end

module Pure_Continuation = struct
  type 'v t = ('v Frame.t) list
      deriving (Show)

  let empty = []
  let (<>) k k' = k @ k'
  let (&>) f k = f :: k

  let to_string _ = "pure_continuation"

  (** Compression **)
  type 'cv compressed_t = ('cv Frame.compressed_t) list
        deriving (Show, Eq, Typeable, Dump, Pickle)
  let compress ~compress_val cont =
    List.map (Frame.compress compress_val) cont
  let uncompress ~uncompress_val globals cont =
    List.map (Frame.uncompress uncompress_val globals) cont

  module Frame = Frame

  module Handler = struct
      type 'v t = unit

      let make : env:'v Env.t -> clauses:Ir.clause Ir.name_map -> depth:[`Deep | `Shallow] -> 'v t
        = fun ~env ~clauses ~depth -> ignore(env); ignore(clauses); ignore(depth); ()
  end

  let set_trap_point ~handler k = ignore(handler); k

  module Evaluation =
    functor(E : EVAL with type 'v t := 'v t) ->
  struct
    type v = E.v
    type result = E.result
    let apply  ~env k v =
    match k with
    | [] -> E.finish env v
    | (scope, var, locals, comp) :: cont ->
       let env = Env.bind var (v, scope) (Env.shadow env ~by:locals) in
       E.computation env cont comp

    let trap _ _ = E.error "no trap"
  end
end

module Eff_Handler_Continuation = struct
  module K = struct
    type 'v handler =
      | Identity
      | User_defined of 'v user_defined_handler
    and 'v user_defined_handler = {
        env: 'v Env.t;
        op_clauses: Ir.clause Ir.name_map;
        return_clause: Ir.binder * Ir.computation;
        depth: [`Deep | `Shallow];
      }
    and 'v k = ('v handler * 'v Frame.t list) list
    and 'v continuation =
      | Empty
      | Continuation of 'v k
      | ShallowContinuation of 'v Frame.t list * 'v k
      deriving (Show)

    type 'cv compressed_handler = 'cv Env.compressed_t * [`Deep | `Shallow]
    and 'cv ck = ('cv compressed_handler * ('cv Frame.compressed_t list)) list
    and 'cv compressed_continuation =
      | CompressedEmpty
      | CompressedContinuation of 'cv ck
      | CompressedShallowContinuation of 'cv Frame.compressed_t list * 'cv ck
      deriving (Show, Eq, Typeable, Dump, Pickle)
  end

  open K
  type 'v t = 'v continuation
      deriving (Show)

  module Debug = struct
    type debug_handler = {
        _op_names: string list;
        _depth:[`Deep | `Shallow];
        _kind:[`Identity | `User_defined];
    }
    and debug_handler_stack = {
         _handlers: debug_handler list;
    }

    let _debug_handler : 'v handler -> debug_handler =
      let operation_names : 'v K.handler -> string list = function
        | Identity -> []
        | User_defined h ->
           StringMap.fold (fun name _ names -> name :: names) h.op_clauses []
      in
      function
      | Identity -> { _op_names = operation_names Identity; _depth = `Deep; _kind = `Identity; }
      | User_defined h ->
         { _op_names = operation_names (User_defined h); _depth = h.depth; _kind = `User_defined }

    let _debug_handler_stack : 'v continuation -> debug_handler_stack =
      fun k ->
      let rec debug_stack = function
        | Empty
        | Continuation [] -> []
        | Continuation ((h, _) :: rest) ->
           let h' = _debug_handler h in
           h' :: (debug_stack (Continuation rest))
        | ShallowContinuation (_,k) -> debug_stack (Continuation k)
      in
      { _handlers = debug_stack k }

    let _string_of_debug_handler : debug_handler -> string
      = fun { _op_names; _depth; _kind; } ->
      let string_of_kind = function
        | `Identity -> "Identity" | `User_defined -> "User defined"
      in
      let string_of_depth = function `Deep -> "Deep" | `Shallow -> "Shallow" in
      let rec string_of_op_names = function
        | [] -> ""
        | [name] -> name
        | name :: names -> Printf.sprintf "%s, %s" name (string_of_op_names names)
      in
      Printf.sprintf "Handler:\n  Kind: %s\n  Depth: %s\n  Operations: %s\n%!" (string_of_kind _kind) (string_of_depth _depth) (string_of_op_names _op_names)

    let _string_of_debug_handler_stack : debug_handler_stack -> string
      = fun { _handlers } ->
      let rec string_of = function
        | [] -> ""
        | [h] -> Printf.sprintf "+%s" (_string_of_debug_handler h)
        | h :: rest -> Printf.sprintf "+%s\n%s" (_string_of_debug_handler h) (string_of rest)
      in
      string_of _handlers
  end

  let empty = Empty

  (* TODO: decide semantics for shallow cases *)
  let (&>) f = function
    | Empty
    | Continuation [] -> Continuation [(Identity, [f])]
    | Continuation ((h, fs) :: rest) -> Continuation ((h, f :: fs) :: rest)
    | ShallowContinuation (_f', []) -> assert false (*ShallowContinuation (f', [(Identity, [f])])*)
    | ShallowContinuation (_f', (_h, _fs) :: _rest) -> assert false (*ShallowContinuation (f', (h, f :: fs) :: rest)*)

  let rec (<>) k k' =
    let prepend_frames fs = function
      | Empty | Continuation [] | ShallowContinuation _ -> assert false
      | Continuation ((h, fs') :: rest) -> Continuation ((h, fs @ fs') :: rest)
    in
    match k, k' with
    | Empty, k
    | k, Empty -> k
    | Continuation k, Continuation k' -> Continuation (k @ k')
    | ShallowContinuation (_fs, _k), ShallowContinuation (_fs', _k') -> assert false (*ShallowContinuation (fs @ fs', k @ k')*)
    | ShallowContinuation (fs, k'), k ->
       let k'' = prepend_frames fs k in
       (Continuation k') <> k''
    | _k, ShallowContinuation (_fs, _k') -> assert false (*(Continuation k') <> (List.fold_left (fun k f -> f &> k) k fs)*)

  module Handler = struct
    open K
    type 'v t = 'v handler
      deriving (Show)

    let make ~env ~clauses ~depth =
      let ((_,b,comp), op_clauses) = StringMap.pop "Return" clauses in
      User_defined { env; op_clauses; return_clause = (b,comp); depth }

    let compress ~compress_val = function
      | User_defined h -> (Env.compress compress_val h.env, h.depth)
      | Identity -> (Env.compress compress_val Env.empty, `Deep)

    let uncompress ~uncompress_val globals h =
      let xb = Var.fresh_binder_of_type `Not_typed in
      let x  = Var.var_of_binder xb in
      User_defined
        { env = Env.uncompress uncompress_val globals (fst h);
        op_clauses = StringMap.empty;
        return_clause =
          (xb, ([], `Return (`Variable x)));
        depth = snd h; }
  end

  (* TODO: decide semantics for shallow case *)
  let set_trap_point ~handler = function
    | Empty -> Continuation [(handler,[])]
    | Continuation k -> Continuation ((handler, []) :: k)
    | ShallowContinuation (_pk,_k) -> assert false (*Continuation ((handler, pk) :: k)*)

  module Evaluation =
    functor (E : EVAL with type 'v t := 'v t) ->
    struct
      type v = E.v
      type result = E.result

      let return k h v =
        let ((var,_), comp) = h.return_clause in
        E.computation (Env.bind var (v, `Local) h.env) k comp

      let rec apply ~env k v =
        let k = match k with
          | Empty -> []
          | ShallowContinuation (fs, k) -> (Identity, fs) :: k
          | Continuation k -> k
        in
        match k with
        | [] -> E.finish env v
        | (Identity, []) :: k -> apply ~env (Continuation k) v
        | (User_defined h, []) :: k -> return (Continuation k) h v
        | (h, f :: pk) :: k ->
           let (scope, var, locals, comp) = f in
           let env = Env.bind var (v, scope) (Env.shadow env ~by:locals) in
           let k = (h, pk) :: k in
           E.computation env (Continuation k) comp

      let trap k (opname, arg) =
        let rec handle k' = function
          | (User_defined h, pk) :: k ->
             begin match StringMap.lookup opname h.op_clauses with
             | Some (kb, (var, _), comp) ->
                let env =
                  match kb with
                  | `ResumptionBinder rb ->
                     let resume =
                       match h.depth with
                       | `Shallow -> ShallowContinuation (pk, List.rev k')
                       | `Deep -> Continuation ( List.rev ((User_defined h,pk) :: k') )
                     in
                     Env.bind (Var.var_of_binder rb) (E.reify resume, `Local) h.env
                  | _ -> h.env
                in
                E.computation (Env.bind var (arg, `Local) env) (Continuation k) comp
             | None -> handle ((User_defined h, pk) :: k') k
             end
          | (identity, pk) :: k -> handle ((identity, pk) :: k') k
          | [] -> E.error (Printf.sprintf "no suitable handler for operation %s has been installed." opname)
        in match k with
        | Empty -> handle [] []
        | Continuation k -> handle [] k
        | ShallowContinuation (pk,k) -> handle [(Identity, pk)] k
    end
  let to_string _ = "generalised_continuation"

  type 'cv compressed_t = 'cv K.compressed_continuation
        deriving (Show, Eq, Typeable, Dump, Pickle)
  let compress ~compress_val =
    let compress_frame = Frame.compress compress_val in
    let compress (h, fs) = (Handler.compress ~compress_val h, List.map compress_frame fs) in
    function
    | Empty -> CompressedEmpty
    | Continuation k -> CompressedContinuation (List.map compress k)
    | ShallowContinuation (fs, k) -> CompressedShallowContinuation (List.map compress_frame fs, List.map compress k)
  let uncompress ~uncompress_val globals =
    let uncompress_frame = Frame.uncompress uncompress_val globals in
    let uncompress (h, fs) = (Handler.uncompress ~uncompress_val globals h, List.map uncompress_frame fs) in
    function
    | CompressedEmpty -> Empty
    | CompressedContinuation k -> Continuation (List.map uncompress k)
    | CompressedShallowContinuation (fs, k) -> ShallowContinuation (List.map uncompress_frame fs, List.map uncompress k)

  module Frame = Frame
end

module Continuation
  = (val (if not (Settings.get_value Basicsettings.Handlers.enabled) then
           (module Pure_Continuation : COMPRESSABLE_CONTINUATION)
         else
           (module Eff_Handler_Continuation : COMPRESSABLE_CONTINUATION)) : COMPRESSABLE_CONTINUATION)

type t = [
| primitive_value
| `List of t list
| `Record of (string * t) list
| `Variant of string * t
| `FunctionPtr of (Ir.var * t option)
| `PrimitiveFunction of string * Var.var option
| `ClientDomRef of int
| `ClientFunction of string
| `Continuation of continuation
| `ReifiedContinuation of continuation
| `Pid of dist_pid
| `AccessPointID of access_point
| `SessionChannel of chan
| `Socket of in_channel * out_channel
| `SpawnLocation of spawn_location
]
and continuation = t Continuation.t
and env = t Env.t
  deriving (Show)

type delegated_chan = (chan * (t list))

(** {1 Compressed values for more efficient pickling} *)
type compressed_primitive_value = [
| primitive_value_basis
| `Table of string * string * string list list * string
| `Database of string
]
  deriving (Show, Eq, Typeable, Pickle, Dump)

type compressed_continuation = compressed_t Continuation.compressed_t
and compressed_t = [
| compressed_primitive_value
| `List of compressed_t list
| `Record of (string * compressed_t) list
| `Variant of string * compressed_t
| `FunctionPtr of (Ir.var * compressed_t option)
| `PrimitiveFunction of string
| `ClientDomRef of int
| `ClientFunction of string
| `Continuation of compressed_continuation
| `ReifiedContinuation of compressed_continuation ]
and compressed_env = compressed_t Env.compressed_t
  deriving (Show, Eq, Typeable, Dump, Pickle)

let compress_primitive_value : primitive_value -> [>compressed_primitive_value]=
  function
    | #primitive_value_basis as v -> v
    | `Table ((_database, db), table, keys, row) ->
        `Table (db, table, keys, Types.string_of_datatype (`Record row))
    | `Database (_database, s) -> `Database s

let rec compress_continuation cont : compressed_continuation = Continuation.compress ~compress_val cont
and compress_val (v : t) : compressed_t =
  let cv = compress_val in
    match v with
      | #primitive_value as v -> compress_primitive_value v
      | `List vs -> `List (List.map cv vs)
      | `Record fields -> `Record(List.map(fun(name, v) -> (name, cv v)) fields)
      | `Variant (name, v) -> `Variant (name, cv v)
      | `FunctionPtr(x, fvs) ->
        `FunctionPtr (x, opt_map cv fvs)
      | `PrimitiveFunction (f,_op) -> `PrimitiveFunction f
      | `ClientDomRef i -> `ClientDomRef i
      | `ClientFunction f -> `ClientFunction f
      | `Continuation cont -> `Continuation (compress_continuation cont)
      | `ReifiedContinuation cont -> `ReifiedContinuation (compress_continuation cont)
      | `Pid _ -> assert false (* mmmmm *)
      | `Socket (_inc, _outc) -> assert false
      | `SessionChannel _ -> assert false (* mmmmm *)
      | `AccessPointID _ -> assert false (* mmmmm *)
      | `SpawnLocation _sl -> assert false (* wheeee! *)

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
  Continuation.uncompress ~uncompress_val globals cont
and uncompress_val globals (v : compressed_t) : t =
  let uv = uncompress_val globals in
    match v with
      | #compressed_primitive_value as v -> uncompress_primitive_value v
      | `List vs -> `List (List.map uv vs)
      | `Record fields -> `Record (List.map (fun (name, v) -> (name, uv v)) fields)
      | `Variant (name, v) -> `Variant (name, uv v)
      | `FunctionPtr (x, fvs) -> `FunctionPtr (x, opt_map uv fvs)
      | `PrimitiveFunction f -> `PrimitiveFunction (f,None)
      | `ClientDomRef i -> `ClientDomRef i
      | `ClientFunction f -> `ClientFunction f
      | `Continuation cont -> `Continuation (uncompress_continuation globals cont)
      | `ReifiedContinuation cont -> `ReifiedContinuation (uncompress_continuation globals cont)

let _escape =
  Str.global_replace (Str.regexp "\\\"") "\\\"" (* FIXME: Can this be right? *)

(** {1 Pretty-printing values} *)

open Format
exception Not_tuple

let keywords = StringSet.from_list (List.map fst Lexer.keywords)

let rec p_value (ppf : formatter) : t -> 'a = function
  | `Bool true -> fprintf ppf "true"
  | `Bool false -> fprintf ppf "false"
  | `Int i -> fprintf ppf "%i" i
  | `Float f -> fprintf ppf "%s" (string_of_float' f)
  | `Char c -> fprintf ppf "'%c'" c
  | `String s -> fprintf ppf "@{<string>\"%s\"@}" s
  | `Record fields -> begin
      try p_tuple ppf fields
      with Not_tuple ->
        fprintf ppf "(@[<hv 0>%a@])" p_record_fields (List.sort (fun (l,_) (r, _) -> compare l r) fields) end
  | `List [] -> fprintf ppf "[]"
  | `List ((`XML _)::_ as elems) ->
     fprintf ppf "@[<hv>%a@]" (pp_print_list p_value) elems
  | `List [v] -> fprintf ppf "[%a]" p_value v
  | `List l -> fprintf ppf "[@[<hov 0>";
               p_list_elements ppf l
  | `ClientDomRef i -> fprintf ppf "%i" i
  | `ClientFunction n -> fprintf ppf "%s" n
  | `PrimitiveFunction (name, _op) -> fprintf ppf "%s" name
  | `Variant (label, `Record []) -> fprintf ppf "@{<constructor>%s@}" label
  (* avoid duplicate parenthesis for Foo(a = 5, b = 3) *)
  | `Variant (label, (`Record _ as value)) -> fprintf ppf "@{<constructor>%s@}@[%a@]" label p_value value
  | `Variant (label, value) -> fprintf ppf "@{<constructor>%s@}(@[%a)@]" label p_value value
  | `FunctionPtr (x, fvs) -> if Settings.get_value Basicsettings.printing_functions then
                               match fvs with
                               | None -> fprintf ppf "%i" x (* ^ opt_app string_of_value "" fvs *)
                               | Some t -> fprintf ppf "%i%a" x p_value t
                             else
                               fprintf ppf "fun"
  | `Socket _ -> fprintf ppf "<socket>"
  | `Table (_, name, _, _) -> fprintf ppf "(table %s)" name
  | `Database (_, params) -> fprintf ppf "(database %s" params
  | `SessionChannel (ep1, ep2) ->
     fprintf ppf "Session channel. EP1: %s, EP2: %s"
             (ChannelID.to_string ep1)
             (ChannelID.to_string ep2)
  | `SpawnLocation (`ClientSpawnLoc cid) ->
     fprintf ppf "Spawn location: client %s" (ClientID.to_string cid)
  | `SpawnLocation `ServerSpawnLoc ->
     fprintf ppf "Spawn location: server"
  | `XML xml -> p_xmlitem ~close_tags:false ppf xml
  | `Continuation cont -> fprintf ppf "Continuation%s" (Continuation.to_string cont)
  | `ReifiedContinuation cont -> fprintf ppf "ReifiedContinuation %s" (Continuation.to_string cont)
  | `AccessPointID (`ClientAccessPoint (cid, apid)) ->
     fprintf ppf "Client access point on client %s, APID: %s" (ClientID.to_string cid) (AccessPointID.to_string apid)
  | `AccessPointID (`ServerAccessPoint (apid)) ->
     fprintf ppf "Server access point %s" (AccessPointID.to_string apid)
  | `Pid (`ServerPid i) -> fprintf ppf "Pid Server (%s)" (ProcessID.to_string i)
  | `Pid (`ClientPid (cid, i)) -> fprintf ppf "Pid Client num %s, process %s" (ClientID.to_string cid) (ProcessID.to_string i)
and p_record_fields ppf = function
  | [] -> fprintf ppf ""
  | [(l, v)] -> fprintf ppf "@[@{<recordlabel>%a@} = %a@]"
                        p_record_label l
                        p_value v
  | (l, v)::xs -> fprintf ppf "@{<recordlabel>%a@} = %a,@ "
                          p_record_label l
                          p_value v;
                  p_record_fields ppf xs
and p_record_label ppf = function
  | s when StringSet.mem s keywords -> fprintf ppf "\"%s\"" s
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
(* Is this function needed? *)
and p_xml ?(close_tags=false) ppf = fun (xml: xml) ->
  pp_print_list (p_xmlitem ~close_tags:close_tags) ppf xml
and p_xmlitem ?(close_tags=false) ppf: xmlitem -> unit = function
  | Attr (k, v) -> let escape = Str.global_replace (Str.regexp "\"") "\\\"" in
                   fprintf ppf "%s=\"%s\"" k (escape v)
  | Text s -> fprintf ppf "%s" (xml_escape s)
  | Node (tag, children) ->
     begin
       match attrs children, nodes children with
       | [], [] when not close_tags ->
          fprintf ppf "<%s/>" tag
       | attrs, [] when not close_tags ->
          fprintf ppf "<%s %a/>"
                  tag
                  (pp_print_list ~pp_sep:pp_print_space (p_xmlitem ~close_tags:close_tags)) attrs
       | [], nodes ->
          fprintf ppf "<%s>%a</%s>"
                  tag
                  (p_xml ~close_tags:close_tags) nodes
                  tag
       | attrs, nodes ->
          fprintf ppf "<%s %a>%a</%s>"
                  tag
                  (pp_print_list ~pp_sep:pp_print_space (p_xmlitem ~close_tags:close_tags)) attrs
                  (p_xml ~close_tags:close_tags) nodes
                  tag
      end
  | NsAttr (ns, k, v) -> p_xmlitem ppf (Attr (ns ^ ":" ^ k, v))
  | NsNode (ns, tag, children) -> p_xmlitem ppf (Node (ns ^ ":" ^ tag, children))

let string_of_pretty pretty_fun arg : string =
  let b = Buffer.create 200 in
  let f = Format.formatter_of_buffer b in
  let (out_string, out_flush) = pp_get_formatter_output_functions f () in
  (* Redefine the meaning of the pretty printing functions. The idea
     is to ignore newlines introduced by pretty printing as well as
     indentation. *)
  let out_functions = {out_string = out_string;
                       out_flush = out_flush;
                       out_newline = ignore;
                       out_spaces = function 0 -> () | _ -> out_string " " 0 1; } in
  pp_set_formatter_out_functions f out_functions;
  pretty_fun f arg;
  pp_print_flush f ();
  Buffer.contents b

(** Get a string representation of a value

    You might want to use `p_value` on a suitable output stream/formatter instead. *)
let string_of_value: t -> string =
  string_of_pretty p_value

let string_of_xml ?(close_tags = false): xml -> string =
  string_of_pretty (p_xml ~close_tags:close_tags)

(* and string_of_dist_pid = function *)
(*   | `ServerPid i -> "Server (" ^ (ProcessID.to_string i) ^ ")" *)
(*   | `ClientPid (cid, i) -> *)
(*       "Client num " ^ (ClientID.to_string cid) ^ ", process " ^ (ProcessID.to_string i) *)
(* and string_of_channel (ep1, ep2) = *)
(*   let ep1_str = ChannelID.to_string ep1 in *)
(*   let ep2_str = ChannelID.to_string ep2 in *)
(*   "Session channel. EP1: " ^ ep1_str ^ ", EP2: " ^ ep2_str *)

(* and string_of_access_point = function *)
(*   | `AccessPointID (`ClientAccessPoint (cid, apid)) -> *)
(*       "Client access point on client " ^ (ClientID.to_string cid) ^ ", " ^ *)
(*       "APID: " ^ (AccessPointID.to_string apid) *)
(*   | `AccessPointID (`ServerAccessPoint (apid)) -> *)
(*       "Server access point " ^ (AccessPointID.to_string apid) *)

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
  | _other -> failwith("Type error unboxing int")
and box_float f = `Float f
and unbox_float : t -> float = function
  | `Float f -> f
  (* This case happens due to the fact that JS serialises "1.0" as "1".
   * Therefore, JSONParse will deserialise something that has Float type
   * and box it as an integer. The alternative would be to box all floats
   * on the client, but this is easier and more performant (if a little hacky)
   *)
  | `Int i -> float_of_int i
  | _ -> failwith "Type error unboxing float"
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
  | `List l -> l | v -> failwith ("Type error unboxing list: " ^ string_of_value v)
and box_record fields = `Record fields
and unbox_record : t -> (string * t) list = function
  | `Record fields -> fields | _ -> failwith "Type error unboxing record"
and box_unit : unit -> t
  = fun () -> `Record []
and unbox_unit : t -> unit = function
  | `Record [] -> () | _ -> failwith "Type error unboxing unit"

let box_op : t list -> t -> t =
  fun ps k -> let box = List.fold_left
			  (fun (i, box) p -> let i = i + 1 in (i, ((string_of_int i, p) :: box)))
			  (0, []) ps
	      in
	      let box = (string_of_int ((fst box) + 1), k) :: (snd box) in
	      `Record (List.rev box)

let box : t list -> t = fun ps -> `Record (List.mapi (fun i p -> (string_of_int (i+1), p)) ps)

let box_pair : t -> t -> t = fun a b -> `Record [("1", a); ("2", b)]
let unbox_pair = function
  | (`Record [(_, a); (_, b)]) -> (a, b)
  | _ -> failwith ("Match failure in pair conversion")
let box_variant : string -> t -> t = fun l v -> `Variant (l, v)
let unbox_variant : t -> (string * t) = function
  | `Variant x -> x
  | _ -> failwith ("Type error unboxing variant")
let box_pid dist_pid = `Pid dist_pid
let unbox_pid = function
  | `Pid dist_pid -> dist_pid
  | _ -> failwith "Type error unboxing pid"
let box_socket (inc, outc) = `Socket (inc, outc)
let unbox_socket = function
  | `Socket p -> p
  | _ -> failwith "Type error unboxing socket"
let box_spawn_loc sl = `SpawnLocation sl
let unbox_spawn_loc = function
  | `SpawnLocation sl -> sl
  | _ -> failwith "Type error unboxing spawn location"
let box_channel ch = `SessionChannel ch
let unbox_channel = function
  | `SessionChannel x -> x
  | _ -> failwith "Type error unboxing session channel"
let box_access_point ap = `AccessPointID ap
let unbox_access_point = function
  | `AccessPointID x -> x
  | _ -> failwith "Type error unboxing access point"
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
  { save = Pickle.to_string<compressed_continuation> ;
    load = Pickle.from_string<compressed_continuation> };

  "Dump",
  { save = Dump.to_string<compressed_continuation> ;
    load = Dump.from_string<compressed_continuation> }
]

(*let continuation_serialisers : (string * compressed_continuation serialiser) list = [
  "Marshal",
  { save = marshal_save ; load = marshal_load };

  "Pickle",
  { save = Pickle_compressed_continuation.to_string ;
    load = Pickle_compressed_continuation.from_string };

  "Dump",
  { save = Dump_compressed_continuation.to_string ;
    load = Dump_compressed_continuation.from_string }
]*)

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
  let save = (continuation_serialiser ()).save in
  let pickle = save cs in
    if String.length pickle > 4096 then
      prerr_endline "Marshalled continuation larger than 4K:";
    base64encode pickle

let marshal_value : t -> string =
  fun v ->
    let save = (value_serialiser ()).save in
    (* Debug.print ("marshalling: "^Show_t.show v); *)
      base64encode (save (compress_val v))

let unmarshal_continuation env : string -> continuation =
    let load = (continuation_serialiser ()).load in
    base64decode ->- load ->- uncompress_continuation (Env.globals env)

let unmarshal_value env : string -> t =
  fun s ->
    let load = (value_serialiser ()).load in
    (* Debug.print ("unmarshalling string: " ^ s); *)
    let v = (load (base64decode s)) in
    (* Debug.print ("unmarshalling: " ^ Show_compressed_t.show v); *)
      uncompress_val (Env.globals env) v

(** Return the continuation frame that evaluates the given expression
    in the given environment. *)
(* let expr_to_contframe env expr = *)
(*   ((`Local        : Ir.scope), *)
(*    (Var.dummy_var : Ir.var), *)
(*    (env           : env), *)
(*    (([], expr)    : Ir.computation)) *)

let rec get_contained_channels v =
  let get_list_contained_channels xs =
    List.fold_left (fun acc x -> (get_contained_channels x) @ acc) [] xs in

  match v with
    | `List xs -> get_list_contained_channels xs
    | `Record xs -> get_list_contained_channels @@ List.map snd xs
    | `Variant (_, x) -> get_contained_channels x
    | `FunctionPtr (_, (Some x)) -> get_contained_channels x
    | `SessionChannel c -> [c]
    | _ -> []

let rec value_of_xml xs = `List (List.map value_of_xmlitem xs)
and value_of_xmlitem =
  function
    | Text s -> `Variant ("Text", box_string s)
    | Attr (name, value) -> `Variant ("Attr", `Record [("1", box_string name); ("2", box_string value)])
    | Node (name, children) -> `Variant ("Node", `Record [("1", box_string name); ("2", value_of_xml children)])
    | NsAttr (ns, name, value) -> `Variant ("NsAttr", `Record [("1", box_string ns); ("2", box_string name); ("3", box_string value)])
    | NsNode (ns, name, children) -> `Variant ("NsNode", `Record [("1", box_string ns); ("2", box_string name); ("3", value_of_xml children)])

let rec xml_of_variants vs = match vs with
  | (`List variant_items) -> List.map xmlitem_of_variant variant_items
  | _ -> failwith "Cannot construct xml from variants"
and xmlitem_of_variant =
  function
    | `Variant ("Text", boxed_string) ->
        Text (unbox_string(boxed_string))
    | `Variant ("Attr", `Record([ ("1", boxed_name); ("2", boxed_value) ])) ->
        Attr(unbox_string(boxed_name), unbox_string(boxed_value))
    | `Variant ("Node", `Record([ ("1", boxed_name); ("2", variant_children) ])) ->
        let name = unbox_string(boxed_name) in
        if (String.contains name ':')
        then failwith "Illegal character in tagname"
        else Node(unbox_string(boxed_name), xml_of_variants variant_children)
    | `Variant ("NsAttr", `Record([ ("1", boxed_ns); ("2", boxed_name); ("3", boxed_value) ])) ->
        let ns = unbox_string(boxed_ns) in
        let name = unbox_string(boxed_name) in
        if (String.contains ns ':') 
        then failwith "Illegal character in namespace"
        else if (String.contains name ':') 
        then failwith "Illegal character in attrname"
        else NsAttr(ns, name, unbox_string(boxed_value))
    | `Variant ("NsNode", `Record([ ("1", boxed_ns); ("2", boxed_name); ("3", variant_children) ])) ->
        let ns = unbox_string(boxed_ns) in
        let name = unbox_string(boxed_name) in
        if (String.contains ns ':') 
        then failwith "Illegal character in namespace"
        else if (String.contains name ':') 
        then failwith "Illegal character in tagname"
        else NsNode(ns, name, xml_of_variants variant_children)
    | _ -> failwith "Cannot construct xml from variant"
