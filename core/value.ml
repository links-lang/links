open CommonTypes
open Utility
open Notfound
open ProcessTypes
open Var

module E = Env

let internal_error message =
  Errors.internal_error ~filename:"value.ml" ~message

let runtime_error message =
  Errors.runtime_error message

(** Set this to [true] to print the body and environment of a
    function. When [false], functions are simply printed as [fun] *)
let printing_functions
  = Settings.(flag "printing_functions"
              |> synopsis "Prints the definition of function-values"
              |> convert parse_bool
              |> sync)

let session_exception_operation = "SessionFail"

class type otherfield =
object
  method show : string
end



type db_status = [ `QueryOk | `QueryError of string ]

class virtual dbvalue = object (self)
  method virtual status : db_status
  method virtual nfields : int
  method virtual ntuples : int
  method virtual fname : int -> string
  method map : 'a. ((int -> string) -> 'a) -> 'a list = fun f ->
      List.init (self#ntuples) (fun i -> f (self#getvalue i))
  method map_array : 'a. (string array -> 'a) -> 'a list = fun f ->
      List.init (self#ntuples) (fun i -> f (self#gettuple i))
  method fold_array : 'a. (string array -> 'a -> 'a) -> 'a -> 'a = fun f x ->
      let max = self#ntuples in
      let rec do_fold n acc =
        if n < max
        then (
          do_fold (n+1) (f (self#gettuple n) acc)
        )
        else acc
      in do_fold 0 x
  method virtual getvalue : int -> int -> string
  method virtual gettuple : int -> string array
  method virtual error : string
end

class virtual database (printer : Sql.printer) = object(self)
  method virtual driver_name : unit -> string
  method virtual escape_string : string -> string
  method virtual exec : string -> dbvalue
  (* SJF: This is still a bit of a kludge. It would be nice to incorporate
   * insert-returning statements into the query processing code. *)
  method make_insert_returning_query : string -> Sql.query -> string list =
    fun _ _ ->
    raise (raise (internal_error ("insert ... returning is not yet implemented for the database driver: "^self#driver_name())))
  method virtual supports_shredding : unit -> bool

  method string_of_query : ?range:(Sql.range option) -> Sql.query -> string =
    fun ?(range=None) q ->
      printer#string_of_query ~range  q
  method sql_printer = printer
end

let equal_database db1 db2 = db1 == db2
(**module Typeable_database = Deriving_Typeable.Primitive_typeable
  (struct
    type t = database
    let magic = "database"
   end)**)
let pp_database = fun fmt _ -> Utility.format_omission fmt (** Supress output **)

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

let resolve_database_driver driver_name =
  match List.assoc_opt driver_name !database_drivers with
  | Some driver -> driver
  | None ->
     DatabaseDriver.load driver_name;
     (* Loading driver should make it register*)
     List.assoc driver_name !database_drivers

let parse_db_string : string -> (string * string) =
  fun params ->
    match Str.bounded_split (Str.regexp ":") params 2 with
      | [hd; tail] -> (hd, tail)
      | _ -> raise (raise (internal_error ("Could not parse db connection string : " ^ params)))
and reconstruct_db_string : (string * string) -> string =
  fun (x,y) -> x ^ ":" ^ y

let database_connections = ref (StringMap.empty : (database * string) StringMap.t)

let db_connect driver params =
  let s = reconstruct_db_string (driver, params) in
  let constructor = resolve_database_driver driver  in
    match StringMap.lookup s !database_connections with
      | None ->
          let db = constructor params in
          database_connections := StringMap.add s db (!database_connections);
          db
      | Some db -> db

class null_database =
object
  inherit database (Sql.default_printer (fun _ -> assert false))
  method driver_name () = "null"
  method exec _query : dbvalue = assert false
  method supports_shredding () = assert false
  method escape_string _ = assert false
end

let _ = register_driver ("null", fun args -> new null_database, reconstruct_db_string ("null", args))

type xmlitem =   Text of string
               | Attr of (string * string)
               | Node of (string * xml)
               | NsAttr of (string * string * string)
               | NsNode of (string * string * xml)
and xml = xmlitem list
    [@@deriving show,yojson]


let is_attr = function
  | Attr _   -> true
  | NsAttr _ -> true
  | _        -> false

let is_node = function
  | Node _   -> true
  | NsNode _ -> true
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

type table = (database * string) * string * string list list * Types.row'
  [@@deriving show]

type primitive_value_basis =  [
| `Bool of bool
| `Char of char
| `Float of float
| `Int of int
| `XML of xmlitem
| `String of string ]
  [@@deriving show,yojson]

let _show_primitive_value_basis = show_primitive_value_basis (* Generated by ppx_deriving show *)

type primitive_value = [
| primitive_value_basis
| `Database of (database * string)
| `Table of table
]
  [@@deriving show]

let pp_in_channel = fun fmt _ -> Utility.format_omission fmt (** Supress output**)
let pp_out_channel = fun fmt _ -> Utility.format_omission fmt (** Supress output**)

type spawn_location = [
  | `ClientSpawnLoc of client_id
  | `ServerSpawnLoc (* Will need to add in a server address when we go to n-tier *)
]
  [@@deriving show]

type dist_pid = [
  | `ClientPid of (client_id * process_id)
  | `ServerPid of process_id (* Again, will need a server address here later *)
]
  [@@deriving show]

type access_point = [
  | `ClientAccessPoint of (client_id * apid)
  | `ServerAccessPoint of apid
]
  [@@deriving show]

type chan = (channel_id * channel_id)
  [@@deriving show]

let _show_chan = show_chan (* Generated by ppx_deriving show *)

module type ENV =
sig
  type 'a t
     [@@deriving show]
  val set_request_data : 'a t -> RequestData.request_data -> 'a t
  val request_data : 'a t -> RequestData.request_data
  val empty : 'a t
  val bind  : Ir.var -> ('a * Ir.scope) -> 'a t -> 'a t
  val find : Ir.var -> 'a t -> 'a
  val mem : Ir.var -> 'a t -> bool
  val lookup : Ir.var -> 'a t -> 'a option
  val lookupS : Ir.var -> 'a t -> ('a * Ir.scope) option
  val shadow : 'a t -> by:'a t -> 'a t
  val fold : (Ir.var -> ('a * Ir.scope) -> 'b -> 'b) -> 'a t -> 'b -> 'b
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
    [@@deriving show]

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
      Scope.Local ->
      { env with all = IntMap.add name (v,scope) env.all }
    | Scope.Global ->
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
          | Some (_, Scope.Global) -> locals
        | Some (v, Scope.Local) ->
           bind name (v, Scope.Local) locals)
      (Tables.find Tables.cont_vars var)
      empty
end

module Trap = struct
  type ('v, 'r) result =
    | Trap of (unit -> 'r)
    | SessionTrap of ('v, 'r) session_result
    | UnhandledSessionException of (Ir.computation list)
  and ('v, 'r) session_result = {
    handle_env: 'v Env.t;
    frames: Ir.computation list;
    continuation_thunk: (unit -> 'r)
  }
end

module type FRAME = sig
  type 'v t

  val of_expr : 'v Env.t -> Ir.tail_computation -> 'v t
  val make : Ir.scope -> Ir.var -> 'v Env.t -> Ir.computation -> 'v t
  val decompose : 'v t -> (Ir.scope * Ir.var * 'v Env.t * Ir.computation)
end

module Frame = struct
  type 'v t = Ir.scope * Ir.var * 'v Env.t * Ir.computation
    [@@deriving show]
  let _show = show (* Generated by ppx_deriving show *)

  let make scope var env comp = (scope, var, env, comp)
  let of_expr env tc =
    make (Scope.Local : Ir.scope) (Var.dummy_var : Ir.var) env (([], tc) : Ir.computation)
  let decompose f = f
end

module type CONTINUATION_EVALUATOR = sig
  type v
  type result
  type 'v t
  type 'v resumption
  type trap_result = (v, result) Trap.result

  val apply : env:v Env.t ->            (* the current environment *)
              v t ->                    (* the continuation *)
              v ->                      (* the argument *)
              result

  val resume : env:v Env.t ->
               v t ->
               v resumption ->
               v list ->
               result

  val trap : v t ->                        (* the continuation *)
             (Name.t * v) ->              (* operation name and its argument *)
             trap_result
end

module type EVAL = sig
  type 'v t
  type 'v resumption
  type v
  type result
  val error : string -> 'a
  val computation : v Env.t -> v t -> Ir.computation -> result
  val finish : v Env.t -> v -> result
  val reify : v resumption -> v
end

module type CONTINUATION = sig
  type 'v t
  and 'v resumption
     [@@deriving show]

  module Frame : FRAME
  module Handler : sig
      type 'v t

      val make : env:'v Env.t -> return:(Ir.binder * Ir.computation) -> clauses:Ir.effect_case Ir.name_map -> depth:[`Deep of Ir.var list | `Shallow] -> 'v t
  end

  (* A continuation has a monoidal structure *)
  val empty : 'v t
  val (<>)  : 'v t -> 'v t -> 'v t          (* continuation composition *)
  val (&>)  : 'v Frame.t -> 'v t -> 'v t    (* continuation augmentation *)

  val set_trap_point : handler:'v Handler.t -> 'v t -> 'v t  (* installs a handler *)

  module Evaluation :
  functor(E : EVAL with type 'v t := 'v t and type 'v resumption := 'v resumption) ->
  sig
    include CONTINUATION_EVALUATOR with
      type v = E.v
      and type result = E.result
      and type 'v t := 'v t
      and type 'v resumption := 'v resumption
  end

  val to_string : 'v t -> string

  module Inspection: sig
    type persistence =
      | Persistent
      | Intermittent
    type 'v result =
      | Frame of 'v Frame.t
      | Trap of { env: 'v Env.t;
                  name: Ir.var;
                  persistence: persistence }

    val inspect : ('v result -> 'a -> 'a) -> 'v t -> 'a -> 'a
  end
end

module Inspection_typedefs = struct
  type persistence =
    | Persistent
    | Intermittent

  type 'v result =
    | Frame of 'v Frame.t
    | Trap of { env: 'v Env.t;
                name: Ir.var;
                persistence: persistence }
end

module Pure_Continuation = struct
  type 'v t = ('v Frame.t) list
  and 'v resumption = 'v t
      [@@deriving show]

  let empty = []
  let (<>) k k' = k @ k'
  let (&>) f k = f :: k

  let to_string _ = "pure_continuation"

  module Frame = Frame

  module Handler = struct
      type 'v t = unit

      let make : env:'v Env.t -> return:(Ir.binder * Ir.computation) -> clauses:Ir.effect_case Ir.name_map -> depth:[`Deep of Ir.var list | `Shallow] -> 'v t
        = fun ~env ~return ~clauses ~depth -> ignore(env); ignore(return); ignore(clauses); ignore(depth); ()
  end

  let set_trap_point ~handler k = ignore(handler); k

  module Evaluation =
    functor(E : EVAL with type 'v t := 'v t and type 'v resumption := 'v resumption) ->
  struct
    type v = E.v
    type result = E.result
    type trap_result = (v, result) Trap.result

    let apply ~env k v =
    match k with
    | [] -> E.finish env v
    | (scope, var, locals, comp) :: cont ->
       let env = Env.bind var (v, scope) (Env.shadow env ~by:locals) in
       E.computation env cont comp

    let resume ~env _ _ _ = ignore(env); E.error "Continuation applied to multiple arguments"

    let trap _ _ = E.error "no trap"
  end

  module Inspection = struct
    include Inspection_typedefs

    let inspect f k z =
      List.fold_right
        (fun frame acc -> f (Frame frame) acc)
        k z
  end
end

module Eff_Handler_Continuation = struct
  module K = struct
    type 'v handler =
      | Identity
      | User_defined of 'v user_defined_handler
    and 'v user_defined_handler = {
        env: 'v Env.t;
        cases: Ir.effect_case Ir.name_map;
        return: Ir.binder * Ir.computation;
        depth: [`Deep of Ir.var list | `Shallow];
      }
    and 'v k = ('v handler * 'v Frame.t list) list
    and 'v r =
      | Deep of 'v k
      | Shallow of 'v Frame.t list * 'v k
      [@@deriving show]

    (* The following show functions are generated by ppx_deriving show *)
    let _show_handler = show_handler
    and _show_user_defined_handler = show_user_defined_handler
    and _show_k = show_k
    and _show_r = show_r
  end

  open K
  type 'v t = 'v k
  and 'v resumption = 'v r
      [@@deriving show]

  (* module Debug = struct *)
  (*   type debug_handler = { *)
  (*       _op_names: string list; *)
  (*       _depth:[`Deep of Ir.var list | `Shallow]; *)
  (*       _kind:[`Identity | `User_defined]; *)
  (*   } *)
  (*   and debug_handler_stack = { *)
  (*        _handlers: debug_handler list; *)
  (*   } *)

  (*   let _debug_handler : 'v handler -> debug_handler = *)
  (*     let operation_names : 'v K.handler -> string list = function *)
  (*       | Identity -> [] *)
  (*       | User_defined h -> *)
  (*          StringMap.fold (fun name _ names -> name :: names) h.cases [] *)
  (*     in *)
  (*     function *)
  (*     | Identity -> { _op_names = operation_names Identity; _depth = `Deep []; _kind = `Identity; } *)
  (*     | User_defined h -> *)
  (*        { _op_names = operation_names (User_defined h); _depth = h.depth; _kind = `User_defined } *)

  (*   let _debug_handler_stack : 'v continuation -> debug_handler_stack = *)
  (*     fun k -> *)
  (*     let rec debug_stack = function *)
  (*       | Empty *)
  (*       | Continuation [] -> [] *)
  (*       | Continuation ((h, _) :: rest) -> *)
  (*          let h' = _debug_handler h in *)
  (*          h' :: (debug_stack (Continuation rest)) *)
  (*       | ShallowContinuation (_,k) -> debug_stack (Continuation k) *)
  (*     in *)
  (*     { _handlers = debug_stack k } *)

  (*   let _string_of_debug_handler : debug_handler -> string *)
  (*     = fun { _op_names; _depth; _kind; } -> *)
  (*     let string_of_kind = function *)
  (*       | `Identity -> "Identity" | `User_defined -> "User defined" *)
  (*     in *)
  (*     let string_of_depth = function `Deep _ -> "Deep" | `Shallow -> "Shallow" in *)
  (*     let rec string_of_op_names = function *)
  (*       | [] -> "" *)
  (*       | [name] -> name *)
  (*       | name :: names -> Printf.sprintf "%s, %s" name (string_of_op_names names) *)
  (*     in *)
  (*     Printf.sprintf "Handler:\n  Kind: %s\n  Depth: %s\n  Operations: %s\n%!" (string_of_kind _kind) (string_of_depth _depth) (string_of_op_names _op_names) *)

  (*   let _string_of_debug_handler_stack : debug_handler_stack -> string *)
  (*     = fun { _handlers } -> *)
  (*     let rec string_of = function *)
  (*       | [] -> "" *)
  (*       | [h] -> Printf.sprintf "+%s" (_string_of_debug_handler h) *)
  (*       | h :: rest -> Printf.sprintf "+%s\n%s" (_string_of_debug_handler h) (string_of rest) *)
  (*     in *)
  (*     string_of _handlers *)
  (* end *)

  let empty = []

  let (&>) f = function
    | [] -> [(Identity, [f])]
    | (h, fs) :: rest -> (h, f :: fs) :: rest

  let (<>) k k' =
    match k, k' with
    | [], k | k, [] -> k
    | k, k' -> k @ k'

  module Handler = struct
    open K
    type 'v t = 'v handler

    let make ~env ~return ~clauses ~depth =
      User_defined { env; return; cases = clauses; depth }
  end

  let set_trap_point ~handler k = (handler, []) :: k

  module Evaluation =
    functor (E : EVAL with type 'v t := 'v t and type 'v resumption := 'v resumption) ->
    struct
      type v = E.v
      type result = E.result
      type trap_result = (v, result) Trap.result

      let return k h v =
        let (b, comp) = h.return in
        let var = Var.var_of_binder b in
        E.computation (Env.bind var (v, Scope.Local) h.env) k comp

      let rec apply ~env k v =
        match k with
        | [] -> E.finish env v
        | (Identity, []) :: k -> apply ~env k v
        | (User_defined h, []) :: k -> return k h v
        | (h, f :: pk) :: k ->
           let (scope, var, locals, comp) = f in
           let env = Env.bind var (v, scope) (Env.shadow env ~by:locals) in
           let k = (h, pk) :: k in
           E.computation env k comp

      let resume ~env k r = function
        | [] -> E.error "Resumption applied to zero arguments"
        | v :: vs ->
           let k = match r with
             | Deep k' ->
                begin match k' with
                | (User_defined h, fs) :: rest ->
                   begin match h.depth with
                   | `Deep xs ->
                      let params =
                        List.fold_left2
                          (fun acc x v ->
                            Env.bind x (v, Scope.Local) acc)
                          Env.empty xs vs
                      in
                      let env = Env.shadow h.env ~by:params in
                      let k' = List.rev ((User_defined { h with env = env }, fs) :: rest) in
                      k' <> k
                   | _ -> assert false
                   end
                | _ -> assert false
                end
             | Shallow (fs', k') ->
                let prepend_frames fs = function
                  | [] -> assert false
                  | (h, fs') :: rest -> (h, fs @ fs') :: rest
                in
                match k with
                | [] -> assert false
                | k ->
                   let k = prepend_frames fs' k in
                   k' <> k
           in
           apply ~env k v

      let session_exn_enabled = Settings.get Basicsettings.Sessions.exceptions_enabled
      let trap k (opname, arg) =
        let open Trap in
        let rec handle k' = function
          | ((User_defined h, pk) :: k) ->
             begin match StringMap.lookup opname h.cases with
             | Some (b, _, comp)
                  when session_exn_enabled && opname = session_exception_operation ->
                let var = Var.var_of_binder b in
                let continuation_thunk =
                  fun () -> E.computation (Env.bind var (arg, Scope.Local) h.env) k comp
                in
                let comps = List.map (fun (_, _, _, c) -> c) pk in
                SessionTrap ({
                      handle_env = h.env;
                      frames = comps;
                      continuation_thunk = continuation_thunk
                  })
             | Some (b, resumeb, comp) ->
                let var = Var.var_of_binder b in
                let env =
                  let resume =
                    match h.depth with
                    | `Shallow -> Shallow (pk, List.rev k')
                    | `Deep _ -> Deep ((User_defined h,pk) :: k')
                  in
                  let env = Env.bind var (arg, Scope.Local) h.env in
                  Env.bind (Var.var_of_binder resumeb) (E.reify resume, Scope.Local) env
                in
                Trap (fun () -> E.computation env k comp)
             | None -> handle ((User_defined h, pk) :: k') k
             end
          | (identity, pk) :: k -> handle ((identity, pk) :: k') k
          | [] when session_exn_enabled && opname = session_exception_operation ->
              (* If this is a session exception operation, we need to gather all
               * of the computations in the pure continuation stack, so we can inspect
               * their free variables. *)
             let comps =
               match (List.rev k') with
               | [] -> []
               | (User_defined _, pk) :: _
               | (_, pk) :: _ -> List.map (fun (_, _, _, comp) -> comp) pk
             in
             UnhandledSessionException comps
          | [] ->
             Trap (fun () -> E.error (Printf.sprintf "no suitable handler for operation %s has been installed." opname))
        in handle [] k

    end
  let to_string _ = "generalised_continuation"

  module Inspection = struct
    include Inspection_typedefs

    let inspect f k z =
      (* FIXME TODO implement reflection of handlers. *)
      List.fold_right
        (fun (_, fs) acc ->
          List.fold_right (fun frame -> f (Frame frame)) fs acc)
        k z
  end

  module Frame = Frame
end

module Continuation
  = (val (if not (Settings.get Basicsettings.Handlers.enabled) then
           (module Pure_Continuation : CONTINUATION)
         else
           (module Eff_Handler_Continuation : CONTINUATION)) : CONTINUATION)

type t = [
| primitive_value
| `Lens of Lens.Database.t * Lens.Value.t
| `List of t list
| `Record of (string * t) list
| `Variant of string * t
| `FunctionPtr of (Ir.var * t option)
| `PrimitiveFunction of string * Var.var option
| `ClientDomRef of int
| `ClientFunction of string
| `Continuation of continuation
| `Resumption of resumption
| `Pid of dist_pid
| `AccessPointID of access_point
| `SessionChannel of chan
| `Socket of in_channel * out_channel
| `SpawnLocation of spawn_location
| `Alien
]
and continuation = t Continuation.t
and resumption = t Continuation.resumption
and env = t Env.t
  [@@deriving show]

type delegated_chan = (chan * (t list))

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
  | `ClientFunction _n -> fprintf ppf "fun"
  | `PrimitiveFunction (name, _op) -> fprintf ppf "%s" name
  | `Variant (label, `Record []) -> fprintf ppf "@{<constructor>%s@}" label
  (* avoid duplicate parenthesis for Foo(a = 5, b = 3) *)
  | `Variant (label, (`Record _ as value)) -> fprintf ppf "@{<constructor>%s@}@[%a@]" label p_value value
  | `Variant (label, value) -> fprintf ppf "@{<constructor>%s@}(@[%a)@]" label p_value value
  | `FunctionPtr (x, fvs) -> if Settings.get printing_functions then
                               match fvs with
                               | None -> fprintf ppf "%i" x (* ^ opt_app string_of_value "" fvs *)
                               | Some t -> fprintf ppf "%i%a" x p_value t
                             else
                               fprintf ppf "fun"
  | `Socket _ -> fprintf ppf "<socket>"
  | `Lens (_,l) -> fprintf ppf "(%a)" Lens.Value.pp l
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
  | `XML xml -> fprintf ppf "@[<hv>%a@]" (p_xmlitem ~close_tags:false) xml
  | `Continuation cont -> fprintf ppf "Continuation%s" (Continuation.to_string cont)
  | `Resumption _res -> fprintf ppf "Resumption"
  | `AccessPointID (`ClientAccessPoint (cid, apid)) ->
     fprintf ppf "Client access point on client %s, APID: %s" (ClientID.to_string cid) (AccessPointID.to_string apid)
  | `AccessPointID (`ServerAccessPoint (apid)) ->
     fprintf ppf "Server access point %s" (AccessPointID.to_string apid)
  | `Pid (`ServerPid i) -> fprintf ppf "Pid Server (%s)" (ProcessID.to_string i)
  | `Pid (`ClientPid (cid, i)) -> fprintf ppf "Pid Client num %s, process %s" (ClientID.to_string cid) (ProcessID.to_string i)
  | `Alien -> fprintf ppf "alien"
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
  let (out_string, _out_flush) = pp_get_formatter_output_functions f () in
  (* We set margin/max indent to absurdly large values to avoid the need
     for newlines. *)
  pp_set_margin f 1000000;
  pp_set_max_indent f 990000;
  (* Redefine the meaning of the pretty printing functions. The idea
     is to ignore newlines introduced by pretty printing as well as
     indentation.
     Newlines can arise as a result of space hints so we need to generate
     at least one space for a newline. *)
  let existing_functions = pp_get_formatter_out_functions f () in
  let out_functions =
    let one_space = function
      | 0 -> ()
      | _ -> out_string " " 0 1
    in
    { existing_functions with
        out_newline = (fun () -> one_space 1);
        out_spaces  = one_space;
        out_indent  = one_space }
  in
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

let type_error ?(action="unbox") expected value =
  Printf.sprintf "Attempting to %s %s (need %s instead)" action (string_of_value value) expected
  |> internal_error

(** {1 Record manipulations} *)

(** [project field value] returns projects the field labeled [field]
    from the Links value [value], provided [value] is a record. *)
let project name = function
  | (`Record fields) -> List.assoc name fields
  | v -> raise (type_error ~action:"project" "record" v)

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
  | `Bool b  -> b
  | v -> raise (type_error "boolean" v)
and box_int i = `Int i
and unbox_int  : t -> int    = function
  | `Int i   -> i
  | v -> raise (type_error "int" v)
and box_float f = `Float f
and unbox_float : t -> float = function
  | `Float f -> f
  (* This case happens due to the fact that JS serialises "1.0" as "1".
   * Therefore, JSONParse will deserialise something that has Float type
   * and box it as an integer. The alternative would be to box all floats
   * on the client, but this is easier and more performant (if a little hacky)
   *)
  | `Int i -> float_of_int i
  | v -> raise (type_error "float" v)
and box_char c = `Char c
and unbox_char :  t -> char = function
  | `Char f -> f
  | v -> raise (type_error "char" v)
and box_xml x = `XML x
and unbox_xml  :  t -> xmlitem = function
  | `XML x -> x
  | v -> raise (type_error "char" v)
and box_string s = `String s
and unbox_string : t -> string = function
  | `String s -> s
  | v -> raise (type_error "string" v)
and box_list l = `List l
and unbox_list : t -> t list = function
  | `List l -> l
  | v -> raise (type_error "list" v)
and box_record fields = `Record fields
and unbox_record : t -> (string * t) list = function
  | `Record fields -> fields
  | v -> raise (type_error "record" v)
and box_unit : unit -> t
  = fun () -> `Record []
and unbox_unit : t -> unit = function
  | `Record [] -> ()
  | v -> raise (type_error "unit" v)

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
  | v -> raise (type_error "pair" v)
let box_variant : string -> t -> t = fun l v -> `Variant (l, v)
let unbox_variant : t -> (string * t) = function
  | `Variant x -> x
  | v -> raise (type_error "variant" v)
let box_pid dist_pid = `Pid dist_pid
let unbox_pid = function
  | `Pid dist_pid -> dist_pid
  | v -> raise (type_error "pid" v)
let box_socket (inc, outc) = `Socket (inc, outc)
let unbox_socket = function
  | `Socket p -> p
  | v -> raise (type_error "socket" v)
let box_spawn_loc sl = `SpawnLocation sl
let unbox_spawn_loc = function
  | `SpawnLocation sl -> sl
  | v -> raise (type_error "SpawnLocation" v)
let box_channel ch = `SessionChannel ch
let unbox_channel = function
  | `SessionChannel x -> x
  | v -> raise (type_error "SessionChannel" v)
let box_access_point ap = `AccessPointID ap
let unbox_access_point = function
  | `AccessPointID x -> x
  | v -> raise (type_error "AccessPointID" v)
let intmap_of_record = function
  | `Record members ->
      Some(IntMap.from_alist(
             List.map (fun (k,v) -> int_of_string k, v ) members))
  | _ -> None

let is_channel = function
  | `SessionChannel _ -> true
  | _ -> false

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
  | v -> raise (type_error ~action:"construct XML from" "list" v)
and xmlitem_of_variant =
  function
    | `Variant ("Text", boxed_string) ->
        Text (unbox_string(boxed_string))
    | `Variant ("Attr", `Record([ ("1", boxed_name); ("2", boxed_value) ])) ->
        Attr(unbox_string(boxed_name), unbox_string(boxed_value))
    | `Variant ("Node", `Record([ ("1", boxed_name); ("2", variant_children) ])) ->
        let name = unbox_string(boxed_name) in
        if (String.contains name ':')
        then raise (runtime_error "Illegal character in tagname")
        else Node(unbox_string(boxed_name), xml_of_variants variant_children)
    | `Variant ("NsAttr", `Record([ ("1", boxed_ns); ("2", boxed_name); ("3", boxed_value) ])) ->
        let ns = unbox_string(boxed_ns) in
        let name = unbox_string(boxed_name) in
        if (String.contains ns ':')
        then raise (runtime_error "Illegal character in namespace")
        else if (String.contains name ':')
        then raise (runtime_error "Illegal character in attrname")
        else NsAttr(ns, name, unbox_string(boxed_value))
    | `Variant ("NsNode", `Record([ ("1", boxed_ns); ("2", boxed_name); ("3", variant_children) ])) ->
        let ns = unbox_string(boxed_ns) in
        let name = unbox_string(boxed_name) in
        if (String.contains ns ':')
        then raise (runtime_error "Illegal character in namespace")
        else if (String.contains name ':')
        then raise (runtime_error "Illegal character in tagname")
        else NsNode(ns, name, xml_of_variants variant_children)
    | v ->
        raise (type_error ~action:"construct XML from" "variant" v)

(* Some utility functions for databases used by insertion *)

let row_columns_values v =
  let row_columns : t -> string list = function
    | `List ((`Record fields)::_) -> List.map fst fields
    | v -> raise (type_error ~action:"form query columns from" "a list of records" v)
  in
  let row_values = function
    | `List records ->
    (List.map (function
          | `Record fields -> List.map snd fields
          | v -> raise (type_error ~action:"form query field from" "record" v)) records)
    | v -> raise (type_error ~action:"form query row from" "list" v)
  in
  (row_columns v, row_values v)


