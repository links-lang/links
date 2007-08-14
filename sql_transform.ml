open Utility
open List

(** Convert a LIKE expression to a string. *)
let rec like_as_string env le = 
  let quote = Str.global_replace (Str.regexp_string "%") "\\%" in
  let rec like_as_string' env =
    function
      | `percent -> "%"
      | `underscore -> "_"
      | `caret -> ""
      | `dollar -> ""
      | `string s -> quote s
      | `variable v -> quote (Result.unbox_string (assoc v env))
      | `seq rs -> mapstrcat "" (like_as_string' env) rs in
  let result =  like_as_string' env le in
  result
