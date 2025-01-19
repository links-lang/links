(* Modular Settings.

   This compilation unit implements an abstraction that allow settings
   to be defined locally on a per compilation unit basis. All settings
   (except for "config") are settable via configuration files. In
   addition, some settings may be settable via the
   commandline. Settings may be defined at any point during program
   evaluation -- even after configuration files and CLI arguments have
   been processed.

   The principal idea is to separate the definition of settings and
   the handling of commandline arguments and configuration key-value
   pairs. Handling of an argument is delayed until a corresponding
   setting has been defined. One can view handling of commandline
   arguments and configuration key-value pairs as giving rise to a
   "stream of unhandled commands".

   An "unhandled command" can be a read or write to some setting. When
   a setting is "synchronised" any pending reads and writes arising
   are performed.

   An intricate problem is how to deal with settings that may be set
   multiple times, e.g. directly on the command line and indirectly
   via a config file. To ensure that the valuation of a setting takes
   on its "latest" value, every unhandled command is timestamped
   according to when it was parsed. During synchronisation unhandled
   commands are ordered according to their discrete time in ascending
   order.

  *)

module Printexc = Utility.Printexc

type privilege = [`User | `System]

(* Error handling. *)
exception Insufficient_privileges of string
exception Conversion_failure of exn * string * string
exception Missing_argument of string
exception Unknown_option of string
exception Unknown_setting of string
exception Invalid_keyvalue_pair of int * string
exception Cannot_set_readonly of string

let is_known_exn : exn -> bool = function
  | Insufficient_privileges _
  | Conversion_failure _
  | Missing_argument _
  | Unknown_option _
  | Unknown_setting _
  | Cannot_set_readonly _
  | Invalid_keyvalue_pair _ -> true
  | _ -> false

let msg_of_exn : exn -> string option = function
  | Missing_argument option_name ->
     Some (Printf.sprintf "Missing argument for option '%s'" option_name)
  | Conversion_failure (exn, setting_name, value) ->
     Some (Printf.sprintf "Setting '%s': cannot parse value '%s' (%s)" setting_name value (Printexc.to_string exn))
  | Insufficient_privileges setting_name ->
     Some (Printf.sprintf "Cannot change system setting '%s'" setting_name)
  | Unknown_option option_name ->
     let option_name =
       (if String.length option_name = 1
        then "-" else "--") ^ option_name
     in
     Some (Printf.sprintf "Unknown option '%s'" option_name)
  | Unknown_setting setting_name ->
     Some (Printf.sprintf "Unknown setting '%s'" setting_name)
  | Invalid_keyvalue_pair (_, entry) ->
     Some (Printf.sprintf "Cannot parse '%s' as a key-value pair" entry)
  | Cannot_set_readonly name ->
     let kind =
       if Char.equal '-' name.[0]
       then "Option" else "Setting"
     in
     Some (Printf.sprintf "%s '%s' is read-only" kind name)
  | _ -> None

let classify : exn -> string = function
  | Unknown_setting _
  | Insufficient_privileges _ -> "warning"
  | exn when is_known_exn exn -> "error"
  | _ -> raise (Invalid_argument "classify")

let print_exn : ?always_error:bool -> exn -> unit
  = fun ?(always_error=false) exn ->
  if is_known_exn exn
  then ((if always_error
         then Printf.fprintf stderr "error: %s\n" (Utility.val_of (msg_of_exn exn))
         else Printf.fprintf stderr "%s: %s\n" (classify exn) (Utility.val_of (msg_of_exn exn)));
        flush stderr)
  else raise (Invalid_argument "print_exn")

let default_handler : exn -> Printexc.raw_backtrace -> unit
  = fun exn stacktrace ->
  let open Utility in
  if is_known_exn exn
  then print_exn ~always_error:true exn
  else let () = Printf.fprintf stderr "Fatal error: exception %s\n" (Printexc.to_string exn) in
       let () = if Printexc.print_backtraces
                then Printexc.print_raw_backtrace stderr stacktrace
       in
       flush stderr

let _ = Printexc.set_uncaught_exception_handler default_handler

let no_conv : string -> 'a
  = fun _ -> failwith "No converter"

let generic_string_of_option : _ option -> string
  = function
  | None -> "(none)"
  | Some _ -> "(some)"

let generic_string_of_list : _ list -> string
  = function
  | [] -> "(empty)"
  | xs -> Printf.sprintf "(nonempty:%d)" (List.length xs)

let no_action : 'a -> unit
  = fun _ -> ()

let from_string_option : string option -> string
  = fun opt -> Utility.from_option "" opt

let string_of_paths xs = String.concat "," xs
let parse_paths value =
  let parts =
    List.(concat
            (map
               (String.split_on_char ':')
               (String.split_on_char ',' value)))
  in
  List.map Utility.Sys.expand parts


let parse_bool = function
  | "true"
  | "yes"
  | "oui"
  | "ja"
  | "tak"
  | "on"
  | "sand"
  | "sure"
  | "yep"
  | "totally" -> true
  | "false"
  | "falsk"
  | "no"
  | "non"
  | "nein"
  | "nej"
  | "nie"
  | "off"
  | "nope"
  | "negative"
  | "uncool" -> false
  | _ -> raise (Invalid_argument "parse_bool")

exception Name_clash of string

(* Discrete clock. *)
module Clock = struct
  type t = int ref
  let clock : t = ref 0

  let tick () =
    incr clock; !clock

  let shift steps =
    clock := !clock + steps
end

type raw_arg =
  | ShortParameter of string
  | LongParameter of string
  | LongParameterValue of string * string
  | KeyValuePair of string * string
  | Value of string

type unhandled =
  { privilege': privilege;
    time: int;
    argument: raw_arg }

(* let string_of_unhandled { time; argument; _ } =
 *   match argument with
 *   | ShortParameter name ->
 *      Printf.sprintf "(ShortParameter %s, %d)" name time
 *   | LongParameter name ->
 *      Printf.sprintf "(LongParameter %s, %d)" name time
 *   | LongParameterValue (name, value) ->
 *      Printf.sprintf "(LongParameterValue (%s, %s), %d)" name value time
 *   | KeyValuePair (name, value) ->
 *      Printf.sprintf "(KeyValuePair (%s, %s), %d)" name value time
 *   | Value value ->
 *      Printf.sprintf "(Value %s, %d)" value time
 *
 * let dump_unhandled : unhandled list -> unit
 *   = fun xs ->
 *   List.iter (fun x -> Printf.printf "%s\n%!" (string_of_unhandled x)) xs *)

let order : unhandled list -> unhandled list
  = fun xs ->
  List.sort (fun { time; _ } { time = time'; _ } -> compare time time') xs

let make_unhandled : ?time:int -> raw_arg -> privilege -> unhandled
  = fun ?time argument privilege' ->
  { privilege'; argument;
    time = match time with None -> Clock.tick () | Some v -> v }

(* Splits a flag group into individual short parameters. *)
let split_unhandled : unhandled -> unhandled list = function
  | { argument = ShortParameter grp; privilege'; time } ->
     let parameters =
       List.(map (fun x -> ShortParameter (String.make 1 x)) (tl (Utility.StringUtils.explode grp)))
     in
     List.map (fun argument -> make_unhandled ~time argument privilege') parameters
  | _ -> assert false

(* Backing store for unhandled arguments. *)
module Store = struct
  type t = (string, unhandled list) Hashtbl.t

  let store : t
    = Hashtbl.create 17

  let add : string -> unhandled -> unit
    = fun key arg ->
    try
      let args = Hashtbl.find store key in
      Hashtbl.replace store key (arg :: args)
    with Not_found ->
      Hashtbl.add store key [arg]

  let fetch : string list -> unhandled list
    = fun keys ->
    let result =
      List.fold_left
        (fun acc key ->
          try
            let result = Hashtbl.find store key in
            Hashtbl.replace store key [];
            result :: acc
          with Not_found -> acc)
        [] keys
    in order (List.concat result)

  let clear : unit -> unit
    = fun () -> Hashtbl.reset store

  let is_empty : unit -> bool
    = fun () -> Hashtbl.length store = 0

  let fetch_all : unit -> unhandled list
    = fun () ->
    let result =
      List.concat
        (Hashtbl.fold (fun _ vs vss -> vs :: vss) store [])
    in
    clear (); assert (is_empty ()); order result

  let first : unit -> unhandled
    = fun () ->
    if is_empty () then raise (Invalid_argument "first")
    else let exception Break of unhandled in
         let f =
           try
             (* Slightly roundabout way of retrieving the "first"
                (really an arbitrary) element of the store. *)
             Hashtbl.fold (fun _ v _ -> raise (Break (List.hd v))) store (fun () -> assert false)
           with Break v -> (fun () -> v)
         in f ()

  let long_key long = Printf.sprintf "--%s" long
  let short_key key = Printf.sprintf "-%c" key.[0]
end

(* Anonymous arguments and rest arguments. *)
let anonymous_arguments : unhandled list ref = ref []
let add_anonymous_argument : unhandled -> unit
  = fun unhandled ->
  anonymous_arguments := unhandled :: !anonymous_arguments
let get_anonymous_arguments : unit -> string list
  = fun () ->
  List.rev_map
    (function
     | { argument = Value value; _ } -> value
     | _ -> assert false)
    !anonymous_arguments

let rest_arguments      : string list ref = ref []

let set_rest_arguments : string list -> unit
  = fun rest ->
  rest_arguments := rest

let get_rest_arguments : unit -> string list
  = fun () -> !rest_arguments

(* Main settings abstraction. *)
module Settings = struct
  type 'a payload = {
      name: string;
      default: 'a;
      mutable value: 'a;
      mutable privilege: privilege;
      mutable synopsis: string;
      mutable to_string: 'a -> string;
      mutable of_string: string -> 'a;
      mutable action: 'a -> unit;
      mutable arg_hint: string;
      mutable hidden: bool;
      mutable show_default: bool }

  type _ setting =
    | Flag : { common: bool payload;
               mutable depends: bool setting list } -> bool setting
    | Option : { common: 'a option payload;
                 readonly: bool } -> 'a option setting
    | MultiOption : { common: 'a list payload;
                      mutable initial : bool;
                      mutable keep_default : bool }  -> 'a list setting
  (* Existential wrapper for [setting]. *)
  and packed = Pack : 'a setting -> packed

  let settings : (string, packed) Hashtbl.t = Hashtbl.create 17

  let get_name : type a. a setting -> string = function
    | Flag payload -> payload.common.name
    | Option payload -> payload.common.name
    | MultiOption payload -> payload.common.name

  let register : type a. a setting -> unit
    = fun setting ->
    let name = get_name setting in
    if Hashtbl.mem settings name
    then raise (Name_clash name)
    else Hashtbl.add settings name (Pack setting)

  exception Bad_setting_name of string
  let is_valid_name : string -> bool
    = fun name ->
    let open Utility in
    try if String.length name > 0
        then (String.iteri
                (fun i c ->
                  if (i = 0 && not (Char.isAlnum c)) || (i > 0 && not (Char.isAlnum c || Char.equal c '_' || Char.equal c '\''))
                  then raise (Bad_setting_name name))
                name; true)
        else false
    with Bad_setting_name _ -> false

  let of_string : type a. (string -> a) -> a setting -> a setting
    = fun of_string setting ->
    (match setting with
     | Flag payload -> payload.common.of_string <- of_string
     | Option payload -> payload.common.of_string <- of_string
     | MultiOption payload -> payload.common.of_string <- of_string);
    setting

  let payload : ?action:('a -> unit) ->
                ?privilege:privilege ->
                ?synopsis:string ->
                ?to_string:('a -> string) ->
                ?of_string:(string -> 'a) ->
                ?arg_hint:string ->
                ?hidden:bool ->
                string -> 'a -> 'a payload
    = fun ?(action=no_action) ?(privilege=`User) ?(synopsis="")
          ?(to_string=(fun _ -> "<unknown>")) ?(of_string=no_conv) ?(arg_hint="")
          ?(hidden=false) name value ->
    if is_valid_name name
    then { name; default = value; value; privilege; synopsis;
           to_string; of_string; action; arg_hint; hidden; show_default = true }
    else raise (Bad_setting_name name)

  let from_string : type a. a setting -> string -> a
    = fun setting vstring ->
    match setting with
    | Flag payload -> payload.common.of_string vstring
    | Option payload -> payload.common.of_string vstring
    | MultiOption payload -> payload.common.of_string vstring

  let get : type a. a setting -> a = function
    | Flag { common; _ } -> common.value
    | Option { common; _ } -> common.value
    | MultiOption { common; _ } -> common.value

  let get_privilege : type a. a setting -> privilege = function
    | Flag payload -> payload.common.privilege
    | Option payload -> payload.common.privilege
    | MultiOption payload -> payload.common.privilege

  let invoke : type a. a setting -> a -> unit
    = fun setting value ->
    match setting with
    | Flag payload -> payload.common.action value
    | Option payload -> payload.common.action value
    | MultiOption payload -> payload.common.action value

  let rec set : type a. ?privilege:privilege -> a setting -> a -> unit
    = fun ?(privilege=`System) setting value ->
    match get_privilege setting, privilege with
    | `System, `User -> raise (Insufficient_privileges (get_name setting))
    | _ ->
       match setting with
       | Flag { common; depends } ->
          let prev_value = get setting in
          common.value <- value;
          (* Only play the action if the bit was flipped. *)
          (if prev_value <> value then invoke setting value);
          (if value (* Only ever switch dependencies ON. Never OFF. *)
           then List.iter (fun dependee -> if not (get dependee) then set ~privilege dependee true) depends)
       | Option payload ->
          if not payload.readonly
          then (payload.common.value <- value;
                invoke setting value)
          else raise (Cannot_set_readonly (get_name setting))
       | MultiOption payload ->
          payload.common.value <- value;
          payload.initial <- false;
          invoke setting value

  let trigger : type a. privilege -> a setting -> unit
    = fun privilege setting ->
    match get_privilege setting, privilege with
    | `System, `User -> raise (Insufficient_privileges (get_name setting))
    | _ -> invoke setting (get setting)

  let append : type a. ?privilege:privilege -> a list setting -> a list -> unit
    = fun ?(privilege=`System) ((MultiOption payload) as setting) value ->
    (if payload.initial && not payload.keep_default
     then set ~privilege setting value
     else set ~privilege setting ((get setting) @ value))

  let keep_default : type a. a list setting -> a list setting = function
    | (MultiOption payload) as setting ->
       payload.keep_default <- true; setting

  let convert (type a) : (string -> a) -> a setting -> a setting
    = fun conv setting ->
    let name = get_name setting in
    let conv v =
      try conv v
      with exn -> raise (Conversion_failure (exn, name, v))
    in
    of_string conv setting

  let hint : type a. string -> a setting -> a setting
    = fun arg_hint setting ->
    (match setting with
     | Flag payload -> payload.common.arg_hint <- arg_hint
     | Option payload -> payload.common.arg_hint <- arg_hint
     | MultiOption payload -> payload.common.arg_hint <- arg_hint);
    setting

  let hidden : type a. a setting -> a setting
    = fun setting ->
    (match setting with
     | Flag payload -> payload.common.hidden <- true
     | Option payload -> payload.common.hidden <- true
     | MultiOption payload -> payload.common.hidden <- true);
    setting

  let is_hidden : type a. a setting -> bool
    = fun setting ->
    match setting with
    | Flag payload -> payload.common.hidden
    | Option payload -> payload.common.hidden
    | MultiOption payload -> payload.common.hidden

  let get_hint : type a. a setting -> string
    = fun setting ->
    match setting with
    | Flag payload -> payload.common.arg_hint
    | Option payload -> payload.common.arg_hint
    | MultiOption payload -> payload.common.arg_hint

  let action : type a. (a -> unit) -> a setting -> a setting
    = fun action setting ->
    (match setting with
     | Flag payload -> payload.common.action <- action
     | Option payload -> payload.common.action <- action
     | MultiOption payload -> payload.common.action <- action);
    setting

  let flag : ?default:bool -> string -> bool setting
    = fun ?(default=false) name ->
    let to_string b = if b then "true" else "false" in
    let setting = Flag { common = payload ~to_string name default; depends = [] } in
    register setting; setting

  let depends : bool setting -> bool setting -> bool setting
    = fun dependee ((Flag payload) as setting) ->
    payload.depends <- dependee :: payload.depends;
    setting

  let option : type a. ?default:a option -> ?readonly:bool -> string -> a option setting
    = fun ?(default=None) ?(readonly=false) name ->
    let setting = Option { common = (payload ~to_string:generic_string_of_option name default);
                           readonly }
    in register setting; setting

  let multi_option : type a. ?default:a list -> string -> a list setting
    = fun ?(default=[]) name ->
    let setting = MultiOption ({ initial = true; keep_default = false; common = payload ~to_string:generic_string_of_list name default }) in
    register setting; setting

  let get_default : type a. a setting -> a = function
   | Flag payload -> payload.common.default
   | Option payload -> payload.common.default
   | MultiOption payload -> payload.common.default

  let toggle : ?privilege:privilege -> bool setting -> unit
    = fun ?(privilege=`System) setting -> set ~privilege setting (not (get_default setting))

  let synopsis : type a. string -> a setting -> a setting
    = fun synopsis setting ->
    (match setting with
     | Flag payload -> payload.common.synopsis <- synopsis
     | Option payload -> payload.common.synopsis <- synopsis
     | MultiOption payload -> payload.common.synopsis <- synopsis);
    setting

  let to_string : type a. (a -> string) -> a setting -> a setting
    = fun to_string setting ->
    (match setting with
     | Flag payload -> payload.common.to_string <- to_string
     | Option payload -> payload.common.to_string <- to_string
     | MultiOption payload -> payload.common.to_string <- to_string);
    setting

  let as_string : type a. a setting -> a -> string
    = fun setting value ->
    match setting with
    | Flag payload -> payload.common.to_string value
    | Option payload -> payload.common.to_string value
    | MultiOption payload -> payload.common.to_string value

  let privilege : type a. privilege -> a setting -> a setting
    = fun privilege setting ->
    (match setting with
     | Flag payload -> payload.common.privilege <- privilege
     | Option payload -> payload.common.privilege <- privilege
     | MultiOption payload -> payload.common.privilege <- privilege);
    setting

  let get_synopsis : type a. a setting -> string = function
    | Flag payload -> payload.common.synopsis
    | Option payload -> payload.common.synopsis
    | MultiOption payload -> payload.common.synopsis

  let is_readonly : type a. a setting -> bool = function
    | Option payload -> payload.readonly
    | _ -> false

  let get_show_default : type a. a setting -> bool = function
   | Flag payload -> payload.common.show_default
   | Option payload -> payload.common.show_default
   | MultiOption payload -> payload.common.show_default

  let show_default : type a. bool -> a setting -> a setting
    = fun bit setting ->
    (match setting with
     | Flag payload -> payload.common.show_default <- bit
     | Option payload -> payload.common.show_default <- bit
     | MultiOption payload -> payload.common.show_default <- bit);
    setting

  let print_settings oc =
    let print_setting : out_channel -> string * packed -> unit
      = fun oc setting ->
      let value =
        match snd setting with
        | Pack (Flag payload) ->
           payload.common.to_string payload.common.value
        | Pack (Option payload) ->
           payload.common.to_string payload.common.value
        | Pack (MultiOption payload) ->
           payload.common.to_string payload.common.value
      in
      Printf.fprintf oc "  %-32s %-7s\n%!" (fst setting) value
    in
    let partition_settings : string -> packed -> ((string * packed) list * (string * packed) list) -> ((string * packed) list * (string * packed) list)
      = fun name (Pack setting) (user, sys) ->
      if is_hidden setting then (user, sys)
      else match get_privilege setting with
           | `User -> ((name, Pack setting) :: user, sys)
           | `System -> (user, (name, Pack setting) :: sys)
    in
    let (user, sys) =
      Hashtbl.fold partition_settings settings ([], [])
    in
    Printf.fprintf oc "User settings\n%!";
    List.iter (print_setting oc) (List.sort (fun (name, _) (name', _) -> String.compare name name') user);
    Printf.fprintf oc "\nSystem settings\n%!";
    List.iter (print_setting oc) (List.sort (fun (name, _) (name', _) -> String.compare name name') sys)
end

module Config = struct
  type t = (string * string) list
  (* Ad-hoc config format parser. *)
  module Parser = struct
    type state =
      { mutable result: (string * string) list }

    let parse : in_channel -> t
      = fun ic ->
      let open Utility in
      let strip_comment s =
        if String.contains s '#'
        then let i = String.index s '#' in
             String.sub s 0 i
        else s
      in
      let is_empty s =
        let exception NonEmpty in
        try
          for i = 0 to String.length s - 1 do
            if Char.isAlpha s.[i] then raise NonEmpty
          done; true
        with NonEmpty -> false
      in
      let parse_line n s st =
        let s = strip_comment s in
        if not (is_empty s) then
          (* ignore 'empty' lines *)
          begin
            if String.contains s '=' then
              begin
                let i = String.index s '=' in
                let name = String.sub s 0 i in
                let value = String.sub s (i+1) ((String.length s) - (i+1))
                in
                if String.equal name "config" (* Ensures that another config, or the same config isn't loaded twice... *)
                then raise (Invalid_keyvalue_pair (n, s))
                else st.result <- (name, value) :: st.result
              end
            else raise (Invalid_keyvalue_pair (n, s))
          end
      in
      let rec parse_lines n st =
        parse_line n (input_line ic) st;
        parse_lines (n+1) st
      in
      let st = { result = [] } in
      (try parse_lines 1 st
       with End_of_file -> ());
      List.rev st.result
  end

  let parse : string -> t
    = fun file ->
    let ic = open_in file in
    let result =
      try Parser.parse ic
      with exn ->
        let stacktrace = Printexc.get_raw_backtrace () in
        close_in ic;
        Printexc.raise_with_backtrace exn stacktrace
    in
    close_in ic; result
end

module CLI = struct
  exception Invalid_CLI_parameter_name of string
  let is_parameter_name : string -> bool
    = fun src ->
    let open Utility in
    let len = String.length src in
    if len = 0 then false
    else
      let exception Invalid in
      try
        for i = 0 to len - 1 do
          if not (Char.isAlnum src.[i] || Char.equal src.[i] '-' || Char.equal src.[i] '_')
          then raise Invalid
        done; true
      with Invalid -> false

  (* A GNU (convention) compliant CLI parser. *)
  module Parser = struct

    type arg = raw_arg

    type result =
      { arguments: arg list;
        rest: string list }

    type t =
      { input: string array;
        mutable cell_ptr: int;
        mutable args: arg list;
        mutable rest_: string list }

    let incr : t -> unit
      = fun st -> st.cell_ptr <- st.cell_ptr + 1

    let make : ?start:int -> string array -> t
      = fun ?(start=0) input ->
      { input; cell_ptr = start; args = []; rest_ = [] }

    let is_done : t -> bool
      = fun st -> st.cell_ptr >= Array.length st.input

    let push : arg -> t -> unit
      = fun arg st ->
      st.args <- arg :: st.args

    let set_rest : string list -> t -> unit
      = fun rest st ->
      st.rest_ <- rest

    let rec anything : t -> t
      = fun st ->
      let src = st.input.(st.cell_ptr) in
      let len = String.length src in
      if len = 0
      then (incr st; parse st)
      else if len = 1 then value st
      else if len = 2 && String.equal src "--" then (incr st; rest st)
      else parameter_or_value st
    and parameter_or_value : t -> t
      = fun st ->
      let src = st.input.(st.cell_ptr) in
      if Char.equal src.[0] '-' && Char.equal src.[1] '-'
      then long_parameter st
      else if Char.equal src.[0] '-'
      then short_parameter st
      else value st
    and long_parameter : t -> t
      = fun st ->
      let src = st.input.(st.cell_ptr) in
      (try
        let idx = String.index src '=' in
        let name = String.sub src 2 (idx - 2) in
        if is_parameter_name name
        then let value = String.sub src (idx+1) (String.length src - idx - 1) in
             push (LongParameterValue (name, value)) st
        else push (Value src) st
       with Not_found ->
         let name = String.sub src 2 (String.length src - 2) in
         if is_parameter_name name
         then push (LongParameter name) st
         else push (Value src) st);
      incr st; parse st
    and short_parameter : t -> t
      = fun st ->
      let src = st.input.(st.cell_ptr) in
      let name = String.sub src 1 (String.length src - 1) in
      if is_parameter_name name
      then (push (ShortParameter name) st; incr st; parse st)
      else (push (Value name) st; incr st; parse st)
    and value : t -> t
      = fun st ->
      let value = st.input.(st.cell_ptr) in
      push (Value value) st; incr st; parse st
    and rest : t -> t
      = fun st ->
      (if st.cell_ptr < Array.length st.input
       then set_rest Array.(to_list (sub st.input st.cell_ptr (length st.input - st.cell_ptr))) st);
      st
    and parse : t -> t
      = fun st ->
      if is_done st
      then st
      else anything st

    let parse : int -> string array -> result
      = fun start input ->
      let st = parse (make ~start input) in
      { arguments = List.rev st.args; rest = st.rest_ }

    let parse_argv : unit -> result
      = fun () ->
      parse 1 Sys.argv
  end

  type arg = Settings.packed

  let shorts : (char  , arg) Hashtbl.t = Hashtbl.create 17
  let longs  : (string, arg) Hashtbl.t = Hashtbl.create 17
  let cli_forms : (string, string) Hashtbl.t = Hashtbl.create 17

  let add : (arg -> arg) -> 'a Settings.setting -> 'a Settings.setting
    = fun f setting ->
    let setting' = Hashtbl.find Settings.settings (Settings.get_name setting) in
    ignore (f setting'); (* This action may endow with the [setting] with short and long CLI parameter names. *)
    setting

  let short : char -> arg -> arg
    = fun c ((Settings.Pack setting) as arg) ->
    (if not (Utility.Char.isAlnum c)
     then raise (Invalid_CLI_parameter_name (String.make 1 c)));
    (if Hashtbl.mem shorts c
     then raise (Name_clash (String.make 1 c))
     else (Hashtbl.add shorts c arg;
           Hashtbl.add cli_forms (Settings.get_name setting) ("-" ^ (String.make 1 c))));
    arg

  let long : string -> arg -> arg
    = fun s ((Settings.Pack setting) as arg) ->
    (if not (is_parameter_name s)
     then raise (Invalid_CLI_parameter_name s));
    (if Hashtbl.mem longs s
     then raise (Name_clash s)
     else (Hashtbl.add longs s arg;
           Hashtbl.add cli_forms (Settings.get_name setting) ("--" ^ s)));
    arg

  let (<&>) f g = fun x -> g (f x)
end

(* This function performs all the heavy-lifting of argument
   handling. It will write to the backing store as a
   side-effect. TODO: Maybe return a list of unhandled argument
   instead of writing directly to the store? *)
let handle_all : unhandled list -> unit
  = fun xs ->
  let handle_short_option : type a. privilege -> string -> a Settings.setting -> unhandled list -> unhandled list
    = fun privilege key setting rest ->
    let handle value =
        match setting with
        | Settings.Flag _ -> assert false
        | Settings.Option _ ->
           Settings.set ~privilege setting (Settings.from_string setting value)
        | Settings.MultiOption _ ->
           Settings.append ~privilege setting (Settings.from_string setting value)
    in
    (* If the key is a group, then interpret the tail as a value argument... *)
    if String.length key > 1
    then let value = String.sub key 1 (String.length key - 1) in
         handle value; rest
    else (* ... otherwise expect the next element in the command stream to be a value argument. *)
      (match rest with
       | { argument = Value value; _ } :: rest' ->
          handle value; rest'
       | _ -> raise (Missing_argument ("-" ^ key)))
  in
  let handle_long_option : type a. privilege -> string -> a Settings.setting -> unhandled list -> unhandled list
    = fun privilege key setting rest ->
    match rest with
    | { argument = Value value; _ } :: rest' ->
       (match setting with
        | Settings.Flag _ -> assert false
        | Settings.Option _ ->
           Settings.set ~privilege setting (Settings.from_string setting value)
        | Settings.MultiOption _ ->
           Settings.append ~privilege setting (Settings.from_string setting value));
       rest'
    | _ -> raise (Missing_argument ("--" ^ key))
  in
  let rec handle xs =
    match xs with
    | [] -> ()
    | unhandled :: rest ->
       let privilege = unhandled.privilege' in
       let rest =
         match unhandled.argument with
         | ShortParameter key ->
            (* A short parameter may consist of a single character or
               a group of characters. If it is a group of characters,
               then there are two possible interpretations:

               1) If the head of the group is a flag, then the tail consists of flags too.
               2) If the head of the group is an option, then the tail is an argument.  *)
            (try
               assert (String.length key > 0);
               let c = key.[0] in (* Always dispatch on the on head. *)
               let (Settings.Pack setting) = Hashtbl.find CLI.shorts c in
               match setting with
               | Settings.Flag _ ->
                  Settings.toggle ~privilege setting;
                  if String.length key > 1
                  then (split_unhandled unhandled) @ rest
                  else rest
               | Settings.Option _ when Settings.is_readonly setting ->
                  if String.length key > 1
                  then raise (Cannot_set_readonly (Printf.sprintf "-%c" c))
                  else Settings.trigger privilege setting; rest
               | Settings.Option _ ->
                  handle_short_option privilege key setting rest
               | Settings.MultiOption _ ->
                  handle_short_option privilege key setting rest
             with Not_found ->
                   let key = Store.short_key key in
                   Store.add key unhandled;
                   (* At this point we do not know whether the short
                      parameter expects a value, thus if the next
                      element in the command stream is a value
                      argument, then we must pair it this short
                      parameter for the time being. *)
                   match rest with
                   | ({ argument = Value _; _ } as value) :: rest' ->
                      Store.add key value; rest'
                   | _ -> rest)
         | LongParameter key ->
            (try
               let (Settings.Pack setting) = Hashtbl.find CLI.longs key in
               match setting with
               | Settings.Flag _ ->
                  Settings.toggle ~privilege setting; rest
               | Settings.Option _ when Settings.is_readonly setting ->
                  Settings.trigger privilege setting; rest
               | Settings.Option _ ->
                  handle_long_option privilege key setting rest
               | Settings.MultiOption _ ->
                  handle_long_option privilege key setting rest
             with Not_found ->
                   let key = Store.long_key key in
                   Store.add key unhandled;
                   (* We do not know yet whether this long parameter
                      expects a value... *)
                   match rest with
                   | ({ argument = Value _; _ } as value) :: rest' ->
                      Store.add key value; rest'
                   | _ -> rest)
         | LongParameterValue (key, value) ->
            (try
               let (Settings.Pack setting) = Hashtbl.find CLI.longs key in
               if Settings.is_readonly setting
               then raise (Cannot_set_readonly (Printf.sprintf "--%s" key));
               match setting with
               | Settings.MultiOption _ ->
                  (* Writing to a multi option via the CLI is
                     interpreted as appending. *)
                  Settings.append ~privilege setting (Settings.from_string setting value)
               | _ ->
                  Settings.set ~privilege setting (Settings.from_string setting value)
             with Not_found ->
               let key = Store.long_key key in
               Store.add key unhandled); rest
         | KeyValuePair (key, value) ->
            (try
               let (Settings.Pack setting) = Hashtbl.find Settings.settings key in
               if Settings.is_readonly setting
               then raise (Cannot_set_readonly (Printf.sprintf "%s" key));
               (* Note: Writing to a multi option via a key-value pair
                  is interpreted as _overriding_ the current value
                  rather than appending. *)
               Settings.set ~privilege setting (Settings.from_string setting value)
             with Not_found ->
               Store.add key unhandled); rest
         | Value _ ->
            (* Any lone value argument is interpreted as an anonymous argument. *)
            add_anonymous_argument unhandled; rest
       in handle rest
  in
  handle xs

(* Include the [Settings] module here to fulfil the interface of this
   compilation unit. *)
include Settings
let sync : type a. a setting -> a setting
  = fun setting ->
  let name = get_name setting in
  let updates = Store.fetch (name :: Hashtbl.find_all CLI.cli_forms name) in
  handle_all updates; setting

module Reflection = struct
  type t =
    { name: string;
      current_value: string option;
      default: string option;
      value_hint: string option;
      synopsis: string option;
      kind: [`Flag | `Option | `MultiOption ]
    }

  let make kind name current_value default value_hint synopsis =
    { name; current_value; default; value_hint; synopsis; kind }

  (* Lifts a string into a option type. The empty string maps to None. *)
  let coerce = function "" -> None | s -> Some s

  let reflect : string -> t
    = fun setting_name ->
    try
      let (Pack setting) = Hashtbl.find settings setting_name in
      let kind = match setting with
        | Flag _ -> `Flag
        | Option _ -> `Option
        | MultiOption _ -> `MultiOption
      in
      make
        kind
        (Settings.get_name setting)
        (coerce (Settings.as_string setting (Settings.get setting)))
        (coerce (Settings.as_string setting (Settings.get_default setting)))
        (coerce (Settings.get_hint setting))
        (coerce (Settings.get_synopsis setting))
    with Not_found -> raise (Unknown_setting setting_name)
end

(* Pretty prints CLI options to [oc]. *)
let print_cli_options oc =
  let options =
    let open Settings in
    let settings = Hashtbl.create 17 in
    Hashtbl.iter
      (fun long (Pack setting) ->
        Hashtbl.add settings
          (Settings.get_name setting)
          (None, Some long, Pack setting))
      CLI.longs;
    Hashtbl.iter
      (fun short (Pack setting) ->
        try
          let (_, long, synopsis) =
            Hashtbl.find settings (Settings.get_name setting)
          in
          Hashtbl.replace settings (Settings.get_name setting) (Some short, long, synopsis)
        with Not_found ->
          Hashtbl.add settings (Settings.get_name setting) (Some short, None, Pack setting))
      CLI.shorts;
    List.sort
      (fun (x, _) (y, _) -> String.compare x y)
      (Hashtbl.fold (fun name desc descs -> (name, desc) :: descs) settings [])
  in
  let extended_synopsis (Pack setting) =
    let default =
      if get_show_default setting then
        match setting with
        | Flag _ ->
           Some (Settings.(as_string setting (get_default setting)))
        | Option _ ->
           (match Settings.get_default setting with
            | None -> None
            | (Some _) as default ->
               Some (Settings.as_string setting default))
        | MultiOption _ ->
           match Settings.get_default setting with
           | [] -> None
           | xs -> Some (Settings.as_string setting xs)
      else None
    in
    match default with
    | None -> Settings.get_synopsis setting
    | Some default ->
       Printf.sprintf "%s (default: %s)" (Settings.get_synopsis setting) default
  in
  let print_option (_name, (short, long, Pack setting)) =
    let synopsis = extended_synopsis (Pack setting) in
    let hint = Settings.get_hint setting in
    let has_hint = hint <> "" in
    match short, long with
    | (Some short, None) ->
       Printf.fprintf oc " -%c %-28s %-7s\n" short hint synopsis
    | (None, Some long) ->
       let long =
         if has_hint
         then Printf.sprintf "%s=%s" long hint
         else long
       in
       Printf.fprintf oc "%-4s --%-25s %-7s\n" "" long synopsis
    | (Some short, Some long) ->
       let long =
         if has_hint
         then Printf.sprintf "%s=%s" long hint
         else long
       in
       Printf.fprintf oc " -%c, --%-25s %-7s\n" short long synopsis
    | (None, None) -> assert false (* Cannot happen. *)
  in
  List.iter print_option options

(* Loads a sequence of key-value pairs. *)
let load_config : privilege -> string -> unit
  = fun privilege file ->
  let results = List.map (fun (key, value) -> make_unhandled (KeyValuePair (key, value)) privilege) (Config.parse file) in
  handle_all results

(* Parses and sets a single key-value pair. *)
let parse_and_set : privilege -> string -> unit
  = fun privilege value ->
  let (ic, oc) =
    let (idescr, odescr) = Unix.pipe () in
    Unix.(in_channel_of_descr idescr, out_channel_of_descr odescr)
  in
  try
    output_string oc (Printf.sprintf "%s\n" value);
    close_out oc;
    let result = Config.Parser.parse ic in
    handle_all (List.map (fun (x, y) -> make_unhandled (KeyValuePair (x, y)) privilege) result);
    close_in ic
  with
  | exn ->
     let stacktrace = Printexc.get_raw_backtrace () in
     close_in ic;
     if is_known_exn exn
     then print_exn exn
     else Printexc.raise_with_backtrace exn stacktrace

let parse_and_set_user : string -> string -> unit
  = fun setting_name value ->
  let keyvalue = Printf.sprintf "%s=%s" setting_name value in
  assert (Store.is_empty ());
  parse_and_set `User keyvalue;
  if not (Store.is_empty ())
  then match Store.first () with
       | { argument = KeyValuePair (name, _); _ } ->
          Store.clear (); print_exn (Unknown_setting name)
       | _ -> assert false


(* Install default settings. *)
let _ = Settings.(option "set"
                  |> synopsis "Sets the value of a particular setting via the commandline"
                  |> privilege `System
                  |> hint "<setting=value>"
                  |> action (fun opt -> parse_and_set `System (Utility.val_of opt))
                  |> hidden (* TODO: Do we want this to show up in the @settings listing? *)
                  |> convert (fun x -> Some x)
                  |> CLI.(add (long "set")))


(* The [config] setting needs special support for two reasons

   1) It may insert key-value pairs into the middle of a command stream.

   2) In case the user does not supply a config, then Links should try
   to load the default config, if it exists.
 *)
(* The following bit flag is used to indicate whether a config has
   been loaded. *)
let config_loaded = ref false
let config =
  (* Path to the default config file. *)
  let default_config_file =
    (* If there exists a precomputed path then use it as the default config. *)
    match Linkspath.config with
    | None ->
       (* If LINKS_CONFIG is defined then use it as the default config. *)
       (match Utility.getenv "LINKS_CONFIG" with
       | Some path -> Some (Filename.concat path "config")
       | None -> None)
    | Some path -> Some path
  in
  (* Load default is the action attached to the [config] setting. If
     [config_loaded] is false it will try to load whatever argument it
     is given (which will always be the default config). *)
  let load_default = function
    | None -> ()
    | Some file ->
       if not !config_loaded
       then (load_config `System file; config_loaded := true)
  in
  Settings.(option ~default:default_config_file "config"
            |> synopsis "Initialises Links according to the given configuration file"
            |> privilege `System
            |> hint "<file>"
            |> to_string from_string_option
            |> action load_default
            |> convert Utility.some
            |> CLI.(add (long "config")))

(* Synchronises every setting. If any CLI argument is left unhandled
   then it print an error message and exits, if a config key-value
   pair is left unhandled then it prints a warning. *)
let ensure_all_synchronised : unit -> unit
  = fun () ->
  handle_all (Store.fetch_all ());
  let loop xs =
    List.fold_left
      (fun bit { argument; _ } ->
        match argument with
        | ShortParameter name
        | LongParameter name
        | LongParameterValue (name, _) ->
           print_exn (Unknown_option name); true
        | KeyValuePair (name, _) ->
           print_exn (Unknown_setting name); bit
        | Value _ -> bit)
      false xs
  in
  if not (Store.is_empty ())
  then if (loop (Store.fetch_all ()))
       then exit 1

let synchronise_defined : unit -> unit
  = fun () ->
  handle_all (Store.fetch_all ());
  Store.clear ()

(* Parses CLI arguments. It handles `--config=file` argument specially
   as whatever key-value pairs are produced by parsing [file] need to
   be inserted into the command stream directly following [--config]. *)
let () =
  (* Parses a config file, the resulting key-value pairs are given
     time relative to [time]. Also this function shifts the global
     [Clock]. *)
  let parse_config time file =
    let results =
      List.mapi (fun i (key, value) -> make_unhandled ~time:(time + i) (KeyValuePair (key, value)) `System) (Config.parse file)
    in
    let delta = List.length results in
    Clock.shift delta; (delta, results)
  in
  let shift delta unhandled acc =
    { unhandled with time = unhandled.time + delta } :: acc
  in
  (* Preprocesses the unhandled "config" arguments before they are
     given to [handle_all]. *)
  let rec preprocess = function
    | [] -> []
    | arg :: args ->
       match arg.argument with
       | LongParameter "config" ->
          (match args with
           | { argument = Value file; _ } :: args' ->
              config_loaded := true; (* Important to play this side-effect before the set. *)
              Settings.set config (Settings.from_string config file);
              let (delta, results) = parse_config arg.time file in
              let args'' = List.fold_right (shift delta) args' [] in
              results @ (preprocess args'')
           | _ -> arg :: preprocess args)
       | LongParameterValue ("config", file) ->
          config_loaded := true; (* Important to play this side-effect before the set. *)
          Settings.set config (Settings.from_string config file);
          let (delta, results) = parse_config arg.time file in
          let args' = List.fold_right (shift delta) args [] in
          results @ (preprocess args')
       | _ -> arg :: preprocess args
  in
  let result = CLI.Parser.parse_argv () in
  set_rest_arguments result.CLI.Parser.rest;
  let arguments = List.map (fun x -> make_unhandled x `System) result.CLI.Parser.arguments in
  handle_all (preprocess arguments);
  (* Force load the default config, if no other config has been loaded. *)
  ignore (sync config |> trigger `System);
