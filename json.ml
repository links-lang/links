let jsonize_primitive : Result.primitive_value -> string = function
  | `Bool value -> string_of_bool value
  | `Int value -> Num.string_of_num value
  | `Float value -> string_of_float value
  | `Char c -> "'"^ Char.escaped c ^"'"
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
  | `Record fields -> "{" ^ String.concat ", " (List.map (fun (k, v) -> "\"" ^ k ^ "\" : " ^ jsonize_result v) fields) ^ "}"
  | `List [] -> "[]"
  | `List ((`Char _)::_) as c  -> "\"" ^ Result.escape (Result.charlist_as_string c) ^ "\""
  | `List (elems) -> "[" ^ String.concat ", " (List.map jsonize_result elems) ^ "]"

