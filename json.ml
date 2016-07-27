open Notfound
open Utility

let show_json = Settings.add_bool("show_json", false, `User)

(*
  SL:
    Having implemented jsonisation of database values, I'm now
    unsure if this is what we really want. From a security point
    of view it certainly isn't a very good idea to pass this kind of
    information to the client.
*)
let json_of_db (db, params) =
  let driver = db#driver_name() in
  let (name, args) = Value.parse_db_string params in
    "{_db:{driver:\"" ^ driver ^ "\",name:\"" ^ name ^ "\", args:\"" ^ args ^ "\"}}"

(*
WARNING:
  May need to be careful about free type variables / aliases in row
*)
let json_of_table ((db, params), name, keys, row) =
  let json_of_key k = "[" ^ (mapstrcat ", " (fun x -> x) k) ^ "]" in
  let json_of_keylist ks = "[" ^ (mapstrcat ", " json_of_key ks) ^ "]" in
    "{_table:{db:'" ^ json_of_db (db, params) ^ "',name:\"" ^ name ^
      "\",row:\"" ^ Types.string_of_datatype (`Record row) ^
      "\",keys:\"" ^ json_of_keylist keys ^ "\"}}"

let js_dq_escape_string str = 
  (* escape for placement in double-quoted string *)
  Str.global_replace (Str.regexp_string "\"") "\\\""
    (Str.global_replace (Str.regexp_string "\n") "\\n"
       (Str.global_replace (Str.regexp_string "\\") "\\\\"
          str))

(** Escape the argument for inclusion in a double-quoted string. *)
let js_dq_escape_char =
  function
    '"' -> "\\\""
  | '\\' -> "\\\\"
  | ch -> String.make 1 ch

let jsonize_location : Ir.location -> string = function
  | `Client  -> "client"
  | `Server  -> "server"
  | `Native  -> "native"
  | `Unknown -> "unknown"

(* Presumably we need only send an event handler to the client if it
   occurs in a value being sent to the client.

   On the other hand, we might expect to send all newly spawned client
   processes to the client whenever we visit the client.

   Similarly, we might transmit all client messages to the client
   whenever we visit the client.
*)

(** collect all of the pids and event handlers that occur in a value *)
module PidsAndEventHandlers =
struct
  type t = IntSet.t * IntSet.t

  let add_pid : int -> t -> t =
    fun pid (ps, es) -> IntSet.add pid ps, es
  let add_event_handler : int -> t -> t =
    fun event_handler (ps, es) -> ps, IntSet.add event_handler es

  let rec value : t -> Value.t -> t = fun env -> function
    | `PrimitiveFunction _
    | `Continuation _ -> assert false
    | `FunctionPtr (f, None) -> env
    | `FunctionPtr (f, Some fvs) -> value env fvs
    | `ClientFunction _ -> env
    | #Value.primitive_value as v -> primitive env v
    | `Variant (_label, v) -> value env v
    | `Record [] -> env
    | `Record ((_, v)::fs) -> value (value env v) (`Record fs)
    | `List vs -> values env vs
    | `Pid (pid, `Client) -> add_pid pid env
    | `Pid (_pid, _) -> assert false
    | `Socket _ -> assert false

  and values : t -> Value.t list -> t = fun env -> function
    | []      -> env
    | (v::vs) -> values (value env v) vs

  and primitive : t -> Value.primitive_value -> t = fun env -> function
    | `XML x -> xml env x
    | _ -> env

  and xml : t -> Value.xmlitem -> t = fun env -> function
    | Value.Text _ -> env
    | Value.Node (_tag, xs) ->
      xmls env xs
    | Value.Attr (label, v) ->
      if label = "key" then
        add_event_handler (int_of_string v) env
      else
        env

  and xmls : t -> Value.xmlitem list -> t = fun env -> function
    | []      -> env
    | (x::xs) -> xmls (xml env x) xs
end

(* sets of client processes and event handlers that need to be serialized *)
type json_state = {processes: IntSet.t; handlers: IntSet.t}
let empty_state = {processes = IntSet.empty; handlers = IntSet.empty}
let add_pid : int -> json_state -> json_state =
  fun pid state -> {state with processes = IntSet.add pid state.processes}
let pid_state : int -> json_state =
  fun pid -> add_pid pid empty_state
let add_event_handler : int -> json_state -> json_state =
  fun h state -> {state with handlers = IntSet.add h state.handlers}
let merge_states s1 s2 = {processes = IntSet.union s1.processes s2.processes;
                          handlers =  IntSet.union s1.handlers s2.handlers}
let diff_state s1 s2 = {processes = IntSet.diff s1.processes s2.processes;
                        handlers = IntSet.diff s1.handlers s2.handlers}


type auxiliary = [`Process of int | `Handler of int]
let auxiliaries_of_state : json_state -> auxiliary list =
  fun state -> List.map (fun i -> `Handler i) (IntSet.elements state.handlers)
             @ List.map (fun i -> `Process i) (IntSet.elements state.processes)

let rec jsonize_value : Value.t -> string * json_state =
  function
  | `PrimitiveFunction _
  | `Continuation _
      as r ->
      failwith ("Can't yet jsonize " ^ Value.string_of_value r);
  | `FunctionPtr (f, fvs) ->
    let (_, _, _, location) = Tables.find Tables.fun_defs f in
    let location = jsonize_location location in
    let env_string, state =
      match fvs with
      | None     -> "", empty_state
      | Some fvs ->
        let s, state = jsonize_value fvs in
        ", \"environment\":" ^ s, state in
    "{\"func\":\"" ^ Js.var_name_var f ^ "\"," ^
    " \"location\":\"" ^ location ^ "\"" ^ env_string ^ "}", state
  | `ClientFunction name -> "{\"func\":\"" ^ name ^ "\"}", empty_state
  | #Value.primitive_value as p -> jsonize_primitive p
  | `Variant (label, value) ->
    let s, state = jsonize_value value in
    "{\"_label\":\"" ^ label ^ "\",\"_value\":" ^ s ^ "}", state
  | `Record fields ->
    let ls, vs = List.split fields in
    let ss, state = jsonize_values vs in
      "{" ^
        mapstrcat "," (fun (kj, s) -> "\"" ^ kj ^ "\":" ^ s) (List.combine ls ss)
      ^ "}", state
  | `List [] -> "[]", empty_state
  | `List (elems) ->
    let ss, state = jsonize_values elems in
      "[" ^ String.concat "," ss ^ "]", state
  | `Pid (pid, `Client) ->
    "{\"pid\":" ^ string_of_int pid ^ "}", pid_state pid
  | `Pid (pid, _) -> failwith "Cannot yet jsonize non-client proceses"
  | `Socket _ -> failwith "Cannot jsonize sockets"
and jsonize_primitive : Value.primitive_value -> string * json_state = function
  | `Bool value -> string_of_bool value, empty_state
  | `Int value -> string_of_int value, empty_state
  | `Float value -> string_of_float' value, empty_state
  | `Char c -> "{\"_c\":\"" ^ (js_dq_escape_char c) ^"\"}", empty_state
  | `Database db -> json_of_db db, empty_state
  | `Table t -> json_of_table t, empty_state
  | `XML xmlitem -> json_of_xmlitem xmlitem
  | `String s -> "\"" ^ js_dq_escape_string s ^ "\"", empty_state
and json_of_xmlitem = function
  | Value.Text s ->
      "[\"TEXT\",\"" ^ js_dq_escape_string (s) ^ "\"]", empty_state
  (* TODO: check that we don't run into problems when HTML containing
     an event handler is copied *)
  | Value.Node (tag, xml) ->
      let attrs, body, state =
        List.fold_right (fun xmlitem (attrs, body, state) ->
            match xmlitem with
            | Value.Attr (label, value) ->
              if label = "key" then
                begin
                  let key = int_of_string value in
                  let state' = add_event_handler key state in
                  ("\"" ^label ^ "\" : " ^ "\"" ^ js_dq_escape_string value ^ "\"") :: attrs, body, state'
                end
              else
                ("\"" ^label ^ "\" : " ^ "\"" ^ js_dq_escape_string value ^ "\"") :: attrs, body, state
            | _ ->
              let s, state' = json_of_xmlitem xmlitem in
              attrs, s :: body, merge_states state state') xml ([], [], empty_state)
      in
        "[\"ELEMENT\",\"" ^ tag ^
            "\",{" ^ String.concat "," attrs ^"},[" ^ String.concat "," body ^ "]]", state
  | Value.Attr _ -> assert false
and jsonize_values : Value.t list -> string list * json_state =
  fun vs ->
    let ss, state =
      List.fold_left
        (fun (ss, state) v ->
           let s, state' = jsonize_value v in
           s::ss, merge_states state state') ([], empty_state) vs in
    List.rev ss, state

let jsonize_value_with : json_state -> Value.t -> string * json_state =
  fun state v ->
    let s, state' = jsonize_value v in
    s, merge_states state state'

let encode_continuation (cont : Value.continuation) : string =
  Value.marshal_continuation cont

let rec resolve_auxiliaries : json_state -> auxiliary list -> (string IntMap.t * string IntMap.t) -> (string IntMap.t * string IntMap.t) =
  fun state pids ((pmap, hmap) as maps) ->
    match pids with
    | [] -> maps
    | `Process pid :: auxs ->
      begin
        match Proc.Proc.lookup_client_process pid with
        | Some process ->
          let messages = Proc.Mailbox.pop_all_messages_for pid in

          (* jsonizing these values may yield further state *)
          let ps, pstate = jsonize_value process in
          let ms, mstate = jsonize_value (`List messages) in

          let s =
            "{\"pid\":" ^ string_of_int pid ^ "," ^
            " \"process\":" ^ ps ^ "," ^
            " \"messages\":" ^ ms ^ "}" in

          let new_state = merge_states pstate mstate in

          resolve_auxiliaries
            (merge_states state new_state)
            (auxs @ auxiliaries_of_state (diff_state new_state state))
            (IntMap.add pid s pmap, hmap)
        | None -> (* this process is already active on the client *)
          resolve_auxiliaries state auxs maps
      end
    | `Handler key :: auxs ->
      let hs = EventHandlers.find key in
      let hs_string, new_state = jsonize_value hs in

      let s = "{\"key\": " ^ string_of_int key ^ "," ^
              " \"eventHandlers\":" ^ hs_string ^ "}" in

      resolve_auxiliaries
        (merge_states state new_state)
        (auxs @ auxiliaries_of_state (diff_state new_state state))
        (pmap, IntMap.add key s hmap)

(* serialise the json_state as a string *)
let resolve_state : json_state -> string =
  fun state ->
    let pmap, hmap = resolve_auxiliaries state (auxiliaries_of_state state) (IntMap.empty, IntMap.empty) in
    "{\"processes\":" ^ "[" ^ String.concat "," (IntMap.to_list (fun _ s -> s) pmap) ^ "]" ^ "," ^
     "\"handlers\":" ^ "[" ^ String.concat "," (IntMap.to_list (fun _ s -> s) hmap) ^ "]}"

(* FIXME: Currently we only send inactive client processes if they
   appear in the serialised value. We might consider sending *all*
   inactive client processes even if they don't appear in the
   serialised value.

   However, the whole thing will become problematic if we start
   connecting multiple clients to the same server.
*)

let jsonize_state value =
  let _v, state = jsonize_value value in
  state

  (* let s = resolve_state state in *)
  (* Debug.if_set show_json (fun () -> "json state: " ^ s); *)
  (* s *)

let value_with_state v s =
  "{\"value\":" ^ v ^ ",\"state\":" ^ s ^ "}"

let jsonize_value_with_state value =
  let v, state = jsonize_value value in
  value_with_state v (resolve_state state)

(** [jsonize_call] creates the JSON object representing a client call,
    its server-side continuation, and the complete state of the
    scheduler. Note that [continuation], [name] and [args] arguments are
    all [Value]-style objects, while the last argument,
    [sched_state_json], is already JSONized--a nonuniform interface. This
    is because the [proc.ml] file keeps the scheduler state as an abstract
    type so we can't inspect it here. Consider changing this.
*)
let jsonize_call continuation name args =
  let vs, state = jsonize_values args in
  let v =
    "{\"__continuation\":\"" ^ (encode_continuation continuation) ^"\"," ^
    "\"__name\":\"" ^ name ^ "\"," ^
    "\"__args\":[" ^ String.concat ", " vs ^ "]}" in
  value_with_state v (resolve_state state)

let jsonize_value value =
  Debug.if_set show_json
    (fun () -> "jsonize_value => " ^ Value.string_of_value value);
  let rv = jsonize_value_with_state value in
    Debug.if_set show_json
      (fun () -> "jsonize_value <= " ^ rv);
    rv

let parse_json str =
  Jsonparse.parse_json Jsonlex.jsonlex (Lexing.from_string str)

let parse_json_b64 str = parse_json(Utility.base64decode str)
