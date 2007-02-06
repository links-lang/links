let show_json = Settings.add_bool("show_json", false, true)

let jsonize_primitive : Result.primitive_value -> string = function
  | `Bool value -> string_of_bool value
  | `Int value -> Num.string_of_num value
  | `Float value -> string_of_float value
  | `Char c -> "\"" ^ String.escaped (Char.escaped c) ^"\"" (* FIXME: what does [escaped] escape? *)
  | `XML _
  | `Table _
  | `Database _ as p -> prerr_endline ("Can't yet jsonize " ^ Result.string_of_primitive p); ""

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
