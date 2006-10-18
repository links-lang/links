let jsonize_primitive : Result.primitive_value -> string = function
  | `Bool value -> string_of_bool value
  | `Int value -> Num.string_of_num value
  | `Float value -> string_of_float value
  | `Char c -> "\"" ^ String.escaped (Char.escaped c) ^"\"" (* FIXME: what does [escaped] escape? *)
  | `XML _
  | `Table _
  | `Database _ as p -> prerr_endline ("Can't yet jsonize " ^ Result.string_of_primitive p); ""

let rec jsonize_result : Result.result -> string = function
  | `Variant _
  | `PFunction _
  | `Continuation _
  | `List ((`XML _)::_)
  | `Function _ as r -> prerr_endline ("Can't yet jsonize " ^ Result.string_of_result r); ""
  | #Result.primitive_value as p -> jsonize_primitive p
  | `Record fields -> "{" ^ String.concat ", " (List.map (fun (kj, v) -> "\"" ^ kj ^ "\" : " ^ jsonize_result v) fields) ^ "}"
  | `List [] -> "[]"
  | `List (elems) ->
      "[" ^ String.concat ", " (List.map jsonize_result elems) ^ "]"

let encode_continuation (cont : Result.continuation) : string =
  Utility.base64encode (Marshal.to_string cont [Marshal.Closures])

let rec jsonize_call continuation name arg = 
  Printf.sprintf "{\"__continuation\":\"%s\",\"__name\":\"%s\",\"__arg\":%s}" (encode_continuation continuation) name (jsonize_result arg)
