(* Serialisation of Value.t inhabitants. *)
module E = Env
open Value
open CommonTypes
open Utility

let internal_error message =
  Errors.internal_error ~filename:"serialisation.ml" ~message

module type COMPRESSION_INTERFACE = sig
  type 'v t
  type compressed_t
       [@@deriving yojson]

  val compress : Value.t t -> compressed_t
  val decompress : ?globals:Value.t Env.t -> compressed_t -> Value.t t
end

module type COMPRESSIBLE_FRAME = sig
  (* include FRAME with type 'v t = 'v Continuation.Frame.t *)
  include COMPRESSION_INTERFACE with type 'v t := 'v Continuation.Frame.t
end

module type COMPRESSIBLE_ENVIRONMENT = sig
  (* include ENV with type 'v t = 'v Env.t *)
  include COMPRESSION_INTERFACE with type 'v t := 'v Env.t
end

module type COMPRESSIBLE_CONTINUATION = sig
  (* include CONTINUATION with type 'v t = 'v Continuation.t *)
  include COMPRESSION_INTERFACE with type 'v t := 'v Continuation.t

  type compressed_r
     [@@deriving yojson]
  val compress_r : Value.t Continuation.resumption -> compressed_r
  val decompress_r : ?globals:Value.t Env.t -> compressed_r -> Value.t Continuation.resumption
end

module type COMPRESSIBLE_VALUE = sig
  (* include module type of Value with type t = Value.t *)
  type compressed_t
       [@@deriving yojson]
  val compress : t -> compressed_t
  val decompress : ?globals:Value.t Env.t -> compressed_t -> t
end

module Compressible = struct
  module Frame (E : COMPRESSIBLE_ENVIRONMENT) : COMPRESSIBLE_FRAME = struct
    module F = Continuation.Frame
    (* include F *)

    type compressed_t = Ir.var * E.compressed_t
                        [@@deriving yojson]

    let compress frame =
      let (_, var, locals, _) = F.decompose frame in
      (var, E.compress locals)
    let decompress ?(globals=Env.empty) (var, env) =
      let scope = Tables.find Tables.scopes var in
      let body = Tables.find Tables.cont_defs var in
      let env = E.decompress ~globals env in
      let locals = Env.localise env var in
      F.make scope var locals body
  end

  module Environment(V : COMPRESSIBLE_VALUE) : COMPRESSIBLE_ENVIRONMENT = struct
    module Scope = Var.Scope
    module E = Value.Env
    (* include E *)
    type compressed_t = (Ir.var * V.compressed_t) list
                        [@@deriving yojson]

    let compress env =
      List.rev
        (E.fold
           (fun name (v, scope) compressed ->
             if Scope.is_global scope
             then compressed
             else (name, V.compress v) :: compressed)
           env [])

    let decompress ?(globals=Env.empty) compressed =
      try
        List.fold_left
          (fun env (name, v) ->
            E.bind name (V.decompress ~globals v, Tables.find Tables.scopes name) env)
          E.empty compressed
      with Notfound.NotFound str ->
        raise (internal_error (Printf.sprintf "In decompress_env: %s"  str))
  end

  module Value(K : COMPRESSIBLE_CONTINUATION) : COMPRESSIBLE_VALUE = struct
    (* module V = Value *)
    (* include V *)

    type compressed_timestamp = [
      | `Infinity
      | `MinusInfinity
      | `Timestamp of float (* UTC UNIX timestamp *)
    ]
    [@@deriving yojson]

    (** {1 Compressed values for more efficient pickling} *)
    type compressed_primitive_value = [
      | `Bool of bool
      | `Char of char
      | `Float of float
      | `Int of int
      | `XML of xmlitem
      | `String of string
      | `Table of
          string (* database name *) *
          string (* table name *) *
          string list list (* keys *) *
          Temporality.t (* temporality *) *
          (string * string) option (* temporal fields *) *
          string (* serialised datatype *)
      | `Database of string
      | `DateTime of compressed_timestamp
      ]
      [@@deriving yojson]

    type compressed_t = [
      | compressed_primitive_value
      | `Lens of string * string
      | `List of compressed_t list
      | `Record of (string * compressed_t) list
      | `Variant of string * compressed_t
      | `FunctionPtr of (Ir.var * compressed_t option)
      | `PrimitiveFunction of string
      | `ClientDomRef of int
      | `ClientFunction of string
      | `ClientClosure of int
      | `Continuation of K.compressed_t
      | `Resumption of K.compressed_r
      | `Alien ]
      [@@deriving yojson]

    let compress_primitive_value : primitive_value -> [>compressed_primitive_value]=
      function
      | `Bool b -> `Bool b
      | `Char c -> `Char c
      | `Float f -> `Float f
      | `Int i -> `Int i
      | `XML x -> `XML x
      | `String s -> `String s
      | `Table Value.Table.({ database = (_, db); name; keys; temporality; temporal_fields; row }) ->
          let type_str =
            Types.string_of_datatype (Types.Record (Types.Row row))
          in
          `Table (db, name, keys, temporality, temporal_fields, type_str)
      | `Database (_database, s) -> `Database s
      | `DateTime Timestamp.Infinity -> `DateTime `Infinity
      | `DateTime Timestamp.MinusInfinity -> `DateTime `MinusInfinity
      | `DateTime (Timestamp.Timestamp ts) ->
          `DateTime (`Timestamp (CalendarShow.to_unixfloat ts))

    let rec compress : t -> compressed_t = function
      | #primitive_value as v -> compress_primitive_value v
      | `Lens (d,l) ->
          let open Lens.Database in
          `Lens (d.serialize (), Lens.Value.serialize l)
      | `List vs -> `List (List.map compress vs)
      | `Record fields -> `Record (List.map (fun (name, v) -> (name, compress v)) fields)
      | `Variant (name, v) -> `Variant (name, compress v)
      | `FunctionPtr(x, fvs) ->
         `FunctionPtr (x, Utility.opt_map compress fvs)
      | `PrimitiveFunction (f, _op) -> `PrimitiveFunction f
      | `ClientDomRef i -> `ClientDomRef i
      | `ClientFunction f -> `ClientFunction f
      | `ClientClosure i  -> `ClientClosure i
      | `Continuation cont -> `Continuation (K.compress cont)
      | `Resumption r -> `Resumption (K.compress_r r)
      | `Pid _ -> assert false (* mmmmm *)
      | `Socket (_inc, _outc) -> assert false
      | `SessionChannel _ -> assert false (* mmmmm *)
      | `AccessPointID _ -> assert false (* mmmmm *)
      | `SpawnLocation _sl -> assert false (* wheeee! *)
      | `Alien -> `Alien

    let decompress_primitive : compressed_primitive_value -> [> primitive_value] = function
      | #primitive_value_basis as v -> v
      | `Table (db_name, table_name, keys, temporality, temporal_fields, t) ->
         let row =
           match DesugarDatatypes.read ~aliases:DefaultAliases.alias_env t with
           | Types.Record (Types.Row row) -> row
           | _ -> assert false in
         let driver, params = parse_db_string db_name in
         let database = db_connect driver params in
         let tbl =
           Value.make_table ~database ~name:table_name ~keys
             ~temporality ~temporal_fields ~row
         in
         `Table tbl
      | `Database s ->
         let driver, params = parse_db_string s in
         let database = db_connect driver params in
         `Database database
      | `DateTime `Infinity -> `DateTime Timestamp.Infinity
      | `DateTime `MinusInfinity -> `DateTime Timestamp.MinusInfinity
      | `DateTime (`Timestamp ts) ->
          `DateTime (Timestamp.timestamp (CalendarShow.from_unixfloat ts))

    let rec decompress ?(globals=Env.empty) (compressed : compressed_t) : Value.t =
      let decompress x = decompress ~globals x in
      match compressed with
      | #compressed_primitive_value as v -> decompress_primitive v
      | `List vs -> `List (List.map decompress vs)
      | `Record fields -> `Record (List.map (fun (name, v) -> (name, decompress v)) fields)
      | `Variant (name, v) -> `Variant (name, decompress v)
      | `FunctionPtr (x, fvs) -> `FunctionPtr (x, Utility.opt_map decompress fvs)
      | `PrimitiveFunction f -> `PrimitiveFunction (f,None)
      | `ClientDomRef i -> `ClientDomRef i
      | `ClientFunction f -> `ClientFunction f
      | `ClientClosure i -> `ClientClosure i
      | `Continuation cont -> `Continuation (K.decompress ~globals cont)
      | `Resumption res -> `Resumption (K.decompress_r ~globals res)
      | `Alien -> `Alien
      | `Lens (cstr, l) ->
        let db = decompress (`Database cstr) in
        let db =
            (match db with
            | `Database (db, cstr) -> Lens_database_conv.lens_db_of_db cstr db
            | _ -> assert false) in
        let l = Lens.Value.deserialize l in
        `Lens (db, l)
  end

  module Continuation(F : COMPRESSIBLE_FRAME)(E : COMPRESSIBLE_ENVIRONMENT) : COMPRESSIBLE_CONTINUATION = struct
    (* FIXME TODO: Continuation serialisation only works for the
       [Pure_Continuation] in [value.ml] as a serialisation scheme for
       handlers is yet to be devised... *)
    module K = Continuation
    (* include K *)

    type compressed_t = F.compressed_t list
                        [@@deriving yojson]
    let compress k =
      let compress x compressed =
        let open K.Inspection in
        match x with
        | Frame frame ->
           F.compress frame :: compressed
        | Trap _ -> assert false (* TODO see comment above. *)
      in
      K.Inspection.inspect compress k []

    let decompress ?(globals=Env.empty) compressed =
      let decompress compressed k =
        let open K in
        let f = F.decompress ~globals compressed in
        f &> k
      in
      List.fold_right decompress compressed K.empty

    type compressed_r = unit (* TODO see comment above. *)
                        [@@deriving yojson]
    let compress_r _r = ()
    let decompress_r ?(globals=Env.empty) _compressed =
      ignore(globals); assert false
  end
end

module rec ContinuationCompressor : COMPRESSIBLE_CONTINUATION = Compressible.Continuation(FrameCompressor)(EnvironmentCompressor)
   and EnvironmentCompressor : COMPRESSIBLE_ENVIRONMENT = Compressible.Environment(ValueCompressor)
   and FrameCompressor : COMPRESSIBLE_FRAME = Compressible.Frame(EnvironmentCompressor)
   and ValueCompressor : COMPRESSIBLE_VALUE = Compressible.Value(ContinuationCompressor)

module Compress = struct
  type compressed_t = ValueCompressor.compressed_t
  let value : Value.t -> compressed_t
    = fun v -> ValueCompressor.compress v

  type compressed_continuation_t = ContinuationCompressor.compressed_t
  let continuation : Value.t Continuation.t -> compressed_continuation_t
    = fun k -> ContinuationCompressor.compress k
end

module Decompress = struct
  let value : ?globals:Value.t Env.t -> Compress.compressed_t -> Value.t
    = fun ?(globals=Env.empty) compressed ->
    ValueCompressor.decompress ~globals compressed

  let continuation : ?globals:Value.t Env.t -> Compress.compressed_continuation_t -> Value.t Continuation.t
    = fun ?(globals=Env.empty) compressed ->
    ContinuationCompressor.decompress ~globals compressed
end

type serialised_t = string

module type SERIALISER = sig
  type s
  module Continuation: sig
    val save : Value.t Continuation.t -> s
    val load : ?globals:Value.t Env.t -> s -> Value.t Continuation.t
  end

  module Value: sig
    val save : Value.t -> s
    val load : ?globals:Value.t Env.t -> s -> Value.t
  end
end

module type S = SERIALISER with type s := serialised_t

module MarshalSerialiser : S = struct
  let marshal : ('a -> 'b) -> 'a -> string
    = fun compress x ->
    let cx = compress x in
    let pickle = Marshal.to_string cx [] in
    Debug.eval_l
      (lazy (if String.length pickle > 4096
             then Printf.fprintf stderr "Marshalled continuation larger than 4K\n%!"));
    Utility.base64encode pickle

  let unmarshal : ('b -> 'a) -> string -> 'a
    = fun decompress s ->
    let data = Marshal.from_string (Utility.base64decode s) 0 in
    decompress data

  module Continuation = struct
    let save k = marshal Compress.continuation k
    let load ?(globals=Env.empty) s = unmarshal (Decompress.continuation ~globals) s
  end

  module Value = struct
    let save v = marshal Compress.value v
    let load ?(globals=Env.empty) s = unmarshal (Decompress.value ~globals) s
  end
end

module YojsonSerialiser : S = struct
  module Continuation = struct
    let save k =
      Yojson.Safe.to_string (ContinuationCompressor.compressed_t_to_yojson (Compress.continuation k))

    let load ?(globals=Env.empty) s =
      let open Yojson.Safe in
      match ContinuationCompressor.compressed_t_of_yojson (from_string s) with
      | Ok v -> Decompress.continuation ~globals v
      | Error msg ->
         raise (internal_error (Printf.sprintf "deserialisation error [yojson]: %s" msg))
  end

  module Value = struct
    let save v =
      Yojson.Safe.to_string (ValueCompressor.compressed_t_to_yojson (Compress.value v))

    let load ?(globals=Env.empty) s =
      let open Yojson.Safe in
      match ValueCompressor.compressed_t_of_yojson (from_string s) with
      | Ok v -> Decompress.value ~globals v
      | Error msg ->
         raise (internal_error (Printf.sprintf "deserialisation error [yojson]: %s" msg))
  end
end


module UnsafeJsonSerialiser : SERIALISER with type s := Yojson.Basic.t = struct
  module Deserialisation = struct
    open ProcessTypes
    (* JSON deserialisation *)
    let error err = Errors.runtime_error err
    (* The JSON spec says that the fields in an object must be unordered.
     * Therefore, for objects with more than one field, it's best to do
     * individual field lookups. We can match directly on ones with single
     * fields though. *)
    let rec value_of_json (json: Yojson.Basic.t) : Value.t =
      let from_json = value_of_json in
      let unwrap_string = function
        | `String str -> str
        | x -> raise (
                   error ("JSON type error. Expected string, got " ^
                            Yojson.Basic.to_string x)) in

      let unwrap_int = function
        | `Int i -> i
        | x -> raise (
                  error ("JSON type error. Expected int, got " ^
                           Yojson.Basic.to_string x)) in

      let unwrap_list = function
        | `List xs -> xs
        | x -> raise (
                   error ("JSON type error. Expected string, got " ^
                            Yojson.Basic.to_string x)) in

      let assoc_string key xs = unwrap_string (List.assoc key xs) in

      let parse_list xs () =
        match (List.assoc_opt "_head" xs, List.assoc_opt "_tail" xs) with
        | (Some hd, Some tl) ->
           begin
             match from_json tl with
             | `List xs -> Some (`List ((from_json hd) :: xs))
             | _ ->
                raise (error ("JSON type error -- expected list, got " ^
                                (Yojson.Basic.to_string tl)))
           end
        | _ -> None in

      let parse_variant xs () =
        match (List.assoc_opt "_label" xs, List.assoc_opt "_value" xs) with
        | (Some k, Some v) ->
           Some (box_variant (unwrap_string k) (from_json v))
        | _ -> None in

      let parse_client_ap xs () =
        match (List.assoc_opt "_clientAPID" xs, List.assoc_opt "_clientId" xs) with
        | (Some apid, Some cid) ->
           let apid = unwrap_string apid |> AccessPointID.of_string in
           let cid =  unwrap_string cid  |> ClientID.of_string in
           Some (`AccessPointID (`ClientAccessPoint (cid, apid)))
        | _ -> None in

      let parse_client_pid xs () =
        match (List.assoc_opt "_clientPid" xs, List.assoc_opt "_clientId" xs) with
        | (Some pid_str, Some id_str) ->
           let pid = unwrap_string pid_str |> ProcessID.of_string in
           let id =  unwrap_string id_str  |> ClientID.of_string in
           Some (`Pid (`ClientPid (id, pid)))
        | _ -> None in

      let parse_session_channel xs () =
        match (List.assoc_opt "_sessEP1" xs, List.assoc_opt "_sessEP2" xs) with
        | (Some ep1, Some ep2) ->
           let ep1 = unwrap_string ep1 |> ChannelID.of_string in
           let ep2 = unwrap_string ep2 |> ChannelID.of_string in
           Some (`SessionChannel (ep1, ep2))
        | _ -> None in

      let parse_server_func xs () =
        match (List.assoc_opt "_serverFunc" xs, List.assoc_opt "_env" xs) with
        | (Some func_id, None)
          | (Some func_id, Some (`List [])) ->
           Some (`FunctionPtr (unwrap_int func_id, None))
        | (Some func_id, Some fvs) ->
           Some (`FunctionPtr (unwrap_int func_id, Some (from_json fvs)))
        | _ -> None in

      let parse_record xs = `Record (List.map (fun (k, v) -> (k, from_json v)) xs) in

      let parse_date xs () =
          match (List.assoc_opt "_type" xs, List.assoc_opt "_value" xs) with
            | (Some (`String "infinity"), _) ->
                Some (Timestamp.infinity |> Value.box_datetime)
            | (Some (`String "-infinity"), _) ->
                Some (Timestamp.minus_infinity |> Value.box_datetime)
            | (Some (`String "timestamp"), Some (`Int utc_time)) ->
                Some
                  (utc_time
                    |> float_of_int
                    |> UnixTimestamp.to_utc_calendar
                    |> Timestamp.timestamp
                    |> Value.box_datetime)
            | _, _ -> None
      in

      let parse_table bs =
        let database =
          begin
            match List.assoc "db" bs |> from_json with
            | `Database db -> db
            | _ -> raise (error ("first argument to a table must be a database"))
          end
        in
        let name = assoc_string "name" bs in
        let keys = List.assoc "keys" bs |> unwrap_list in
        let keys =
          List.map (function
              | `List part_keys -> List.map unwrap_string part_keys
              | _ -> raise (error "keys must be lists of strings")) keys in
        let temporality =
          match assoc_string "temporality" bs with
            | "current" -> Temporality.current
            | "transaction_time" -> Temporality.transaction
            | "valid_time" -> Temporality.valid
            | _ -> raise (error "Temporality must be one of current, transaction_time, or valid_time")
        in
        let temporal_fields =
          match List.assoc_opt "temporal_fields" bs with
            | Some (`Assoc temporal_fields) ->
                Some (
                  assoc_string "from_field" temporal_fields,
                  assoc_string "to_field" temporal_fields)
            | Some _ ->
                raise (error "Temporal fields must be an association list")
            | _ -> None
        in
        let row_type =
          DesugarDatatypes.read
            ~aliases:E.String.empty
            (assoc_string "row" bs) in
        let row =
          begin
            match row_type with
            | Types.Record (Types.Row row) -> row
            | _ -> raise (error ("tables must have record type"))
          end
        in
        Value.make_table ~database ~name ~keys ~temporality ~temporal_fields ~row
      in
      let parse_client_closure xs () =
        match List.assoc_opt "_closureTable" xs with
        | Some index -> Some (`ClientClosure (unwrap_int index))
        | None -> None
      in

      let (<|>) (o1: unit -> t option) (o2: unit -> t option) : unit -> t option =
        match o1 () with
        | Some x -> (fun () -> Some x)
        | None -> o2 in

      match json with
      | `Null -> `List []
      | `Int i -> box_int i
      | `Float f -> box_float f
      | `String s -> box_string s
      | `Bool b -> box_bool b
      | `List xs -> `List (List.map from_json xs)
      | `Assoc [] -> box_record [] (* Unit tuple *)
      | `Assoc [("_c", `String c)] -> box_char (c.[0])
      | `Assoc [("_c", nonsense)] ->
         raise (error (
                    "char payload should be a string. Got: " ^ (Yojson.Basic.to_string nonsense)))
      | `Assoc [("_serverAPID", `String apid_str)] ->
         let apid = AccessPointID.of_string apid_str in
         `AccessPointID (`ServerAccessPoint (apid))
      | `Assoc [("_serverPid", `String pid_str)] ->
         `Pid (`ServerPid (ProcessID.of_string pid_str))
      | `Assoc [("_clientSpawnLoc", `String client_id_str)] ->
         let client_id = ClientID.of_string client_id_str in
         `SpawnLocation (`ClientSpawnLoc (client_id))
      | `Assoc [("_serverAPID", nonsense)]
        | `Assoc [("_serverPid", nonsense)]
        | `Assoc [("_clientSpawnLoc", nonsense)] ->
         raise (error (
                    "process / AP ID payload should be a string. Got: " ^ (Yojson.Basic.to_string nonsense)))
      | `Assoc [("_serverSpawnLoc", _)] ->
         `SpawnLocation (`ServerSpawnLoc)
      | `Assoc ["_lens", `Assoc assoc] ->
         let cstr = assoc_string "db" assoc in
         let driver, params = parse_db_string cstr in
         let db, _ = db_connect driver params in
         let lens = assoc_string "lens" assoc in
         let lens = Lens.Value.deserialize lens in
         let db = Lens_database_conv.lens_db_of_db cstr db in
         `Lens (db, lens)
      | `Assoc ["_lens", nonsense] ->
        raise (error (
            "lens should be an assoc list. Got: " ^ (Yojson.Basic.to_string nonsense)))
      | `Assoc ["_db", `Assoc assoc] ->
         let driver = assoc_string "driver" assoc in
         let params =
           reconstruct_db_string
             (assoc_string "name" assoc,
              assoc_string "args" assoc) in
         `Database (db_connect driver params)
      | `Assoc [("_db", nonsense)] ->
         raise (error (
                    "db should be an assoc list. Got: " ^ (Yojson.Basic.to_string nonsense)))
      | `Assoc [("_table", `Assoc bs)] ->
         `Table (parse_table bs)
      | `Assoc [("_table", nonsense)] ->
         raise (error (
                    "table should be an assoc list. Got: " ^ (Yojson.Basic.to_string nonsense)))
      | `Assoc [("_xml", `Assoc xs)] ->
         let elem_type = assoc_string "type" xs in
         begin
           match elem_type with
           | "TEXT" -> `XML (Text (assoc_string "text" xs))
           | "ELEMENT" ->
              let tag = assoc_string "tagname" xs in
              let attrs = List.assoc "attributes" xs in
              let attrs = match attrs with
                | `Assoc attrs -> attrs
                | _ ->
                   raise (error ("xml attributes should be an assoc list")) in
              let attrs = List.fold_left (fun attrs (label, value) ->
                              Attr (label, unwrap_string value) :: attrs) [] attrs in
              let body = List.assoc "body" xs |> unwrap_list in
              let body = List.map (fun x ->
                             let val_body = from_json x in
                             match val_body with
                             | `XML body -> body
                             | _ -> raise (error ("xml body should be a list of xmlitems"))
                           ) body
              in `XML (Node (tag, attrs @ body))
           | _ -> raise
                    (error ("xml of unknown type in jsonparse. Got type " ^ elem_type))
         end
      | `Assoc [("_xml", nonsense)] ->
         raise (error (
                    "xml should be an assoc list. Got: " ^ (Yojson.Basic.to_string nonsense)))
      | `Assoc ["_domRefKey", `Int id] -> `ClientDomRef id
      | `Assoc ["_domRefKey", nonsense] ->
         raise (error (
                    "dom ref key should be an integer. Got: " ^ (Yojson.Basic.to_string nonsense)))
      | `Assoc xs ->
         (* For non-singleton assoc lists, try each of these in turn.
          * If all else fails, parse as a record. *)
         let result =
           (parse_list xs)
           <|> (parse_variant xs)
           <|> (parse_client_ap xs)
           <|> (parse_client_pid xs)
           <|> (parse_session_channel xs)
           <|> (parse_server_func xs)
           <|> (parse_date xs)
           <|> (parse_client_closure xs)
         in
         begin
           match result () with
           | Some v -> v
           | None -> parse_record xs
         end
  end

  module Continuation = struct
    let save _ = assert false
    let load ?(globals=Env.empty) _ =
      ignore(globals); assert false
  end

  module Value = struct
    let save v = Json.jsonize_value v
    let load ?(globals=Env.empty) s =
      ignore(globals); Deserialisation.value_of_json s
  end
end


let serialisers : (string * (module S)) list
  = [ "Marshal", (module MarshalSerialiser)
    ; "Yojson" , (module YojsonSerialiser) ]


let serialiser
  = Settings.(option ~default:(Some "Yojson") "serialiser"
              |> synopsis "Selects the backend used for serialising data between server and client"
              |> to_string from_string_option
              |> convert Utility.some
              |> sync)

let get_serialiser : unit -> (module S)
  = fun () ->
  let name = Utility.val_of (Settings.get serialiser) in
  try List.assoc name serialisers
  with Notfound.NotFound _ ->
    raise (internal_error (Printf.sprintf "Unknown serialisation method: %s" name))

module Continuation = struct
  let serialise k =
    let (module S) = get_serialiser () in
    S.Continuation.save k

  let deserialise data =
    let (module S) = get_serialiser () in
    S.Continuation.load data
end

module Value = struct
  let serialise v =
    let (module S) = get_serialiser () in
    S.Value.save v

  let deserialise data =
    let (module S) = get_serialiser () in
    S.Value.load data
end
