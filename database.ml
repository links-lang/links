open Num
open List

open Utility
open Result
open Sql
open Syntax

class virtual db_args from_str = object
  val strval : string = from_str
  method virtual from_string : string -> unit
end

let value_of_db_string (value:string) = function
  | BoolField -> Result.bool (value = "true")
  | TextField -> Result.string_as_charlist value
  | IntField  -> Result.int (num_of_string value)
  | FloatField -> (if value = "" then Result.float 0.00      (* HACK HACK *)
                   else Result.float (float_of_string value))
  | _ -> failwith "value_of_db_string: unsupported datatype"

let datatype_of_db_type = function
  | BoolField -> `Primitive `Bool
  | TextField -> `List (`Primitive `Char)
  | IntField -> `Primitive `Int
  | FloatField -> `Primitive `Float
  | _ -> failwith "datatype_of_db_type: unsupported datatype"

let execute_command  (query:string) (db: database) : result =
  let result = (db#exec query) in
    (match result#status with
       | QueryOk -> `Record []
       | QueryError msg -> raise (Runtime_error ("An error occurred executing the query " ^ query ^ ": " ^ msg)))

let execute_select  (datatype:Types.datatype) (query:string) (db: database) : result =
  let fields = (match datatype with
		  | `List (`Record (field_env, _)) ->
		      (StringMap.fold (* refactor this as stringmap_to_pairlist *)
			 (fun label field_spec fields ->
			    match field_spec with
			      | `Present t -> (label, t) :: fields
			      | `Absent -> raise (Runtime_error "SQ072"))
			 field_env [])
                  | _ -> failwith "internal error: unexpected type in select")
  in 
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
                            (if assoc name fields = datatype_of_db_type db_type then false 
			     else raise (Runtime_error ("Database did not provide results compatible with specified type (query was '" ^ query ^ "')")))
                          else false (*raise (Runtime_error ("SQ094 " ^ name)) *)
                            (* Quick kludge because I don't know what's wrong *)) in
         let null_query = exists is_null row_fields in
           if null_query then
             `List (map (fun _ -> `Record []) result#get_all_lst)
           else
             `List (map (fun row ->
                           `Record (map2 (fun (name, db_type) value -> 
			                    name, value_of_db_string value db_type)
                                      row_fields row))
                      result#get_all_lst)
       | QueryError msg -> raise (Runtime_error ("An error occurred executing the query " ^ query ^ ": " ^ msg)))
