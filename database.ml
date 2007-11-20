open Num
open List

open Utility
open Result
open Syntax

class virtual db_args from_str = object
  val strval : string = from_str
  method virtual from_string : string -> unit
end

let value_of_db_string (value:string) = function
  | `Primitive `Bool -> Result.bool (value = "true")
  | `Application ("String", [])
  | `Application ("List", [`Primitive `Char]) -> Result.string_as_charlist value
  | `Primitive `Int  -> Result.int (num_of_string value)
  | `Primitive `Float -> (if value = "" then Result.float 0.00      (* HACK HACK *)
                   else Result.float (float_of_string value))
  | t -> failwith ("value_of_db_string: unsupported datatype: '" ^ Types.string_of_datatype t ^"'")

let datatype_of_db_type = function
  | BoolField -> `Primitive `Bool
  | TextField -> `Application ("String", [])
  | IntField -> `Primitive `Int
  | FloatField -> `Primitive `Float
  | _ -> failwith "datatype_of_db_type: unsupported datatype"

let execute_command  (query:string) (db: database) : result =
  let result = (db#exec query) in
    (match result#status with
       | QueryOk -> `Record []
       | QueryError msg -> raise (Runtime_error ("An error occurred executing the query " ^ query ^ ": " ^ msg)))

let execute_insert (table_name, field_names, vss) db =
  execute_command (db#make_insert_query (table_name, field_names, vss)) db

let execute_select (field_types:(string * Types.datatype) list) (query:string) (db: database)
    : result =
  let result = (db#exec query) in
    (match result#status with
       | QueryOk -> 
           let row_fields =
	     (let temp_fields = ref [] in
                (* TBD: factor this out as 
                   result_sig : dbresult -> (string * dbtype * datatype) list *)
                for count = result#nfields - 1 downto 0 do 
                  try 
                    temp_fields := 
                      (result#fname count, 
                       result#ftype count, 
                       (List.assoc (result#fname count) field_types))
                    :: !temp_fields
                  with NotFound _ -> (* Could probably remove this. *)
                    failwith("Column " ^ (result#fname count) ^ 
                               " had no type info in query's type spec: " ^
                               mapstrcat ", " (fun (fld, typ) -> fld ^ ":" ^ 
                                                 Types.string_of_datatype typ)
                               field_types)
                done;
                !temp_fields) in
           let is_null = (fun (name, db_type, real_type) ->
                            if name = "null" then true
                            else if mem_assoc name field_types then
                              if (db#equal_types real_type db_type) then
                                false 
			      else raise (Runtime_error ("Database did not provide results compatible with specified type (query was '" ^ query ^ "')"))
                            else assert false) in
           let null_query = exists is_null row_fields in
             if null_query then
               `List (map (fun _ -> `Record []) result#get_all_lst)
             else
               `List (map (fun rowvalue ->
                             `Record (map2 (fun (name, db_type, real_type) fldvalue -> 
			                      name, value_of_db_string fldvalue real_type)
                                        row_fields rowvalue))
                        result#get_all_lst)
       | QueryError msg -> raise (Runtime_error ("An error occurred executing the query " ^ query ^ ": " ^ msg)))
      
