let show_json = Settings.add_bool("show_json", false, `User)

(*
  [REMARK][SL]
    Having implemented jsonisation of database values, I'm now
    unsure if this is what we really want. From a security point
    of view it certainly isn't a very good idea to pass this kind of
    information to the client.
*)
let json_of_db (db, params) =
  let driver = db#driver_name() in
  let (name, args) = Result.parse_db_string params in
    "{ _db : { driver : '" ^ driver ^ "', name : '" ^ name ^ "', args : '" ^ args ^ "'} }"


let jsonize_primitive : Result.primitive_value -> string = function
  | `Bool value -> string_of_bool value
  | `Int value -> Num.string_of_num value
  | `Float value -> string_of_float value
  | `Char c -> "\"" ^ String.escaped (Char.escaped c) ^"\"" (* FIXME: what does [escaped] escape? *)
  | `XML _
  | `Table _ as p -> prerr_endline ("Can't yet jsonize " ^ Result.string_of_primitive p); ""
  | `Database db -> json_of_db db

let rec jsonize_result : Result.result -> string = function
  | `PFunction _
  | `Continuation _
  | `List ((`XML _)::_)
  | `Function _ as r -> prerr_endline ("Can't yet jsonize " ^ Result.string_of_result r); ""
  | #Result.primitive_value as p -> jsonize_primitive p
  | `Variant (label, value) -> Printf.sprintf "{\"_label\" : \"%s\",\"_value\":%s}" label (jsonize_result value)
  | `Record fields -> "{" ^ String.concat ", " (List.map (fun (kj, v) -> "\"" ^ kj ^ "\" : " ^ jsonize_result v) fields) ^ "}"
  | `List [] -> "[]"
  | `List (elems) ->
      "[" ^ String.concat ", " (List.map jsonize_result elems) ^ "]"

let encode_continuation (cont : Result.continuation) : string =
  Utility.base64encode (Marshal.to_string cont [Marshal.Closures])

let rec jsonize_call continuation name arg = 
  Printf.sprintf "{\"__continuation\":\"%s\",\"__name\":\"%s\",\"__arg\":%s}" (encode_continuation continuation) name (jsonize_result arg)

let jsonize_result result = 
  Debug.if_set show_json
    (fun () -> "jsonize_result => " ^ Result.string_of_result result);
  let rv = jsonize_result result in
    Debug.if_set show_json
      (fun () -> "jsonize_result <= " ^ rv);
    rv
