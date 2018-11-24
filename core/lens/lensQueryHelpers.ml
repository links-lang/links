open Utility
open Value

module Constant = Lens_constant

exception Runtime_error = Errors.Runtime_error

let is_null name = (name = "null")
let translate_op_to_sql = function
    | "==" -> "="
    | "&&" -> "AND"
    | "||" -> "OR"
    | a -> a

module Phrase = struct
    let case var = `Case var
end

(* execution, as taken from database.ml (this cannot be reused though, as
      database.ml is lower down in the compile chain) *)

let value_of_db_string (value:string) t =
  match TypeUtils.concrete_type t with
    | `Primitive `Bool ->
        (* HACK:

           This should probably be part of the database driver as
           different databases have different representations of
           booleans.

           mysql appears to use 0/1 and postgres f/t
        *)
        Value.box_bool (value = "1" || value = "t" || value = "true")
    | `Primitive `Char -> Value.box_char (String.get value 0)
    | `Primitive `String -> Value.box_string value
    | `Primitive `Int  -> Value.box_int (int_of_string value)
    | `Primitive `Float -> (if value = "" then Value.box_float 0.00      (* HACK HACK *)
                            else Value.box_float (float_of_string value))
    | t -> failwith ("value_of_db_string: unsupported datatype: '" ^
                        Types.string_of_datatype t(*string_of_datatype t*)^"'")

let result_signature field_types result =
    let n = result#nfields in
    let rec rs i =
      if i >= n then
        [],true
      else
        let name = result#fname i in
          if start_of ~is:"order_" name then
            (* ignore ordering fields *)
            rs (i+1)
          else if List.mem_assoc name field_types then
            let fields,null_query = rs (i+1) in
	    (name, (List.assoc name field_types, i)) :: fields,
	    null_query && is_null(name)
          else
            failwith("Column " ^ name ^
                        " had no type info in query's type spec: " ^
                        mapstrcat ", " (fun (name, t) -> name ^ ":" ^
                          Types.string_of_datatype t)
                        field_types)
    in let rs, null_query = rs 0
    in if null_query then [] else rs

(* builds record given a row field accessor function *)
 let build_record (rs: (string * (Types.datatype * int)) list) (row:int -> string) =
    let rec build rs =
      match rs with
      | [] -> []
      | (name,(t,i))::rs' ->
	  (name,value_of_db_string (row i) t)::build rs'
    in build rs

let execute_select_result
    (field_types:(string * Types.datatype) list) (query:string) (db: database)  =
  let result = (db#exec query) in
    (match result#status with
       | `QueryOk -> result, result_signature field_types result
       | `QueryError msg -> raise (Runtime_error ("An error occurred executing the query " ^ query ^ ": " ^ msg)))


let build_result ((result:Value.dbvalue),rs) =
  `List (result#map (fun row ->
                     `Record (build_record rs row))
	   )

let execute_select
    (field_types:(string * Types.datatype) list) (query:string) (db : database)
    : Value.t =
  let result,rs = execute_select_result field_types query db in
  build_result (result,rs)
