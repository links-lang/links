open Num
open List

open Utility
open Result
open Sql
open Kind
open Syntax

class virtual db_args from_str = object
  val strval : string = from_str
  method virtual from_string : string -> unit
end

let value_from_db_string (value:string) = function
  | BoolField -> Result.bool (value = "true")
  | TextField -> Result.string_as_charlist value
  | IntField  -> Result.int (num_of_string value)
  | FloatField -> (if value = "" then Result.float 0.00      (* HACK HACK *)
                   else Result.float (float_of_string value))

let kind_from_db_type = function
  | BoolField -> `Primitive `Bool
  | TextField -> `Collection (`List, `Primitive `Char)
  | IntField -> `Primitive `Int
  | FloatField -> `Primitive `Float
  | _ -> failwith "Unsupported kind"

let execute_select  (kind:kind) (query:string) (db: database) : result =
  let coll_type, fields = (match kind with
			     | `Collection (coll_type, `Record (field_env, _)) ->
				 (coll_type, StringMap.fold
				    (fun label field_spec fields ->
				       match field_spec with
					 | `Present t -> (label, t) :: fields
					 | `Absent -> raise (Runtime_failure "SQ072")) field_env [])
								 
(*
                             | `Collection (coll_type, `Record fields) ->
                                 (coll_type, map (function `Field_present props -> props | _ -> raise (Runtime_failure "SQ072")) fields)
*)
                                   (* Quick hack to avoid reliance on RTTI*)
                             | x -> (`List, []))
  in 
    (*raise (Runtime_failure ("SQ074 : " ^ (string_of_kind x))) *)
    
  let result = (db#exec query) in

    (match result#status with
       | QueryOk -> let row_fields =
	   (let temp_fields = ref [] in
           for count = result#nfields - 1 downto 0 do 
             temp_fields := (result#fname count, result#ftype count) :: !temp_fields (*blech*)
           done;
              !temp_fields) in
         let is_null = (fun (name, db_type) ->
                          if name = "null" then true
                          else if mem_assoc name fields then
                            (if assoc name fields = kind_from_db_type db_type then false 
			     else raise (Runtime_exception ("Database did not provide results compatible with specified type (query was '" ^ query ^ "')")))
                          else false (*raise (Runtime_failure ("SQ094 " ^ name)) *)
                            (* Quick kludge because I don't know what's wrong *)) in
         let null_query = exists is_null row_fields in
           if null_query then
             `Collection (coll_type, map (fun row -> `Record []) result#get_all_lst)
           else
             `Collection (coll_type, 
			  map (fun row ->
                                 `Record (map2 (fun (name, db_type) value -> 
			                          name, value_from_db_string value db_type)
                                            row_fields row))
                            result#get_all_lst)
       | QueryError msg -> raise (Runtime_exception ("An error occurred executing the query " ^ query ^ ": " ^ msg)))
