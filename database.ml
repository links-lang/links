open List

open Utility

type database = Value.database
exception Runtime_error = Errors.Runtime_error

class virtual db_args from_str = object
  val strval : string = from_str
  method virtual from_string : string -> unit
end

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
                        Types.Show_datatype.show t(*string_of_datatype t*)^"'")

let execute_command  (query:string) (db: database) : Value.t =
  let result = (db#exec query) in
    begin
      match result#status with
        | `QueryOk -> `Record []
        | `QueryError msg -> raise (Runtime_error ("An error occurred executing the query " ^ query ^ ": " ^ msg))
    end

let execute_insert (table_name, field_names, vss) db =
  execute_command (db#make_insert_query (table_name, field_names, vss)) db

let execute_insert_returning (table_name, field_names, vss, returning) db =
  let qs = db#make_insert_returning_query (table_name, field_names, vss, returning) in
  let rec run =
    function
      | [] -> assert false
      | [q] ->
          let result = db#exec q in
            begin
              match result#status with
               | `QueryOk ->
                   let rows = result#get_all_lst in
                     begin
                       match rows with
                         | [[id]] -> Value.box_int (int_of_string id)
                         | _ -> raise (Runtime_error ("Returned the wrong number of results executing " ^ q))
                     end
               | `QueryError msg -> raise (Runtime_error ("An error occurred executing the query " ^ q ^ ": " ^ msg))
            end
      | q :: qs ->
          execute_command q db;
          run qs
  in
    run qs


let is_null name = (name = "null");;

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
;;


(* builds record given a row field accessor function *)
 let build_record (rs: (string * (Types.datatype * int)) list) (row:int -> string) = 
    let rec build rs l = 
      match rs with
      | [] -> l
      | (name,(t,i))::rs' -> 
	  build rs' ((name,value_of_db_string (row i) t)::l)
      | _ -> assert false
    in build rs []
;;

(*experimental *)
 let _build_record_array (rs: (string * (Types.datatype * int)) list) (row:string array) = 
    let rec build rs l = 
      match rs with
      | [] -> l
      | (name,(t,i))::rs' -> 
	  build rs' ((name,value_of_db_string (Array.get row i) t)::l)
      | _ -> assert false
    in build rs []
;;



(*
Old version, retained in case of regression *)
(*

let execute_select_old
    (field_types:(string * Types.datatype) list) (query:string) (db : database)
    : Value.t =

 (* BUG: Lists can be too big for List.map; need to be careful about recursion *)

  let result = db#exec query in

    match result#status with
      | `QueryError msg -> 
        raise(Runtime_error("An error occurred executing the query " ^ query ^
                               ": " ^ msg))
      | `QueryOk -> 
          Debug.debug_time "QueryOK" (fun () -> 
	    match field_types with
          | [] ->
                (* Ignore any dummy fields introduced to work around
                   SQL's inability to handle empty column lists *)
                `List (map (fun _ -> `Record []) result#get_all_lst)
            | _ ->
               
		let fields = result_signature field_types result in

                let is_null (name, _) =
                  if name = "null" then true
                  else if mem_assoc name fields then false
                  else assert false in
                let null_query =  exists is_null fields in

                  if null_query then
                    `List (map (fun _ -> `Record []) result#get_all_lst)
                  else
                    `List (map
                             (fun row ->
                               `Record (
                                List.fold_right
                                   (fun (name, (t, i)) fields ->
                                     (name, value_of_db_string (List.nth row i) t)::fields)
                                   fields
                                   []
			       ))
                      	(Debug.debug_time "get_all_lst" (fun () -> result#get_all_lst))
		      )
)

*)

(* BUG: Lists can be too big for List.map; need to be careful about recursion *)

let execute_select_result
    (field_types:(string * Types.datatype) list) (query:string) (db: database)  =
  let result = (db#exec query) in
    (match result#status with
       | `QueryOk -> 
           result,
	   result_signature field_types result
       | `QueryError msg -> raise (Runtime_error ("An error occurred executing the query " ^ query ^ ": " ^ msg)))

(* Experimental code to build result using getvalue *)
let _build_result_deforest ((result:Value.dbvalue),rs) = 
  let max = result#ntuples in
  let rec do_map n acc = 
    let rec build rs l = 
      match rs with
      | [] -> l
      | (name,(t,i))::rs' -> 
	  build rs' ((name,value_of_db_string (result#getvalue n i) t)::l)
      | _ -> assert false in
    if n < max
    then do_map (n+1) (`Record(build rs [])::acc)
    else `List (acc)
  in do_map 0 []  
    ;;
    
let build_result ((result:Value.dbvalue),rs) = 
  `List (result#map (fun row ->
                     `Record (build_record rs row))
	   )
    ;;

(* experimental *)
let _build_result_array ((result:Value.dbvalue),rs) = 
  `List (result#map_array (fun row ->
                     `Record (_build_record_array rs row))
	   )
    ;;

let _execute_select_old
    (field_types:(string * Types.datatype) list) (query:string) (db : database)
    : Value.t =
  let result = db#exec query in
  
  match result#status with
  | `QueryError msg -> 
      raise(Runtime_error("An error occurred executing the query " ^ query ^
                          ": " ^ msg))
  | `QueryOk -> 
      Debug.debug_time "QueryOK" (fun () -> 
	let rs = result_signature field_types result in
	`List (result#map
                 (fun row ->
                   `Record (build_record rs row)
		     )
		 )
	  )
	
    
	


let execute_select
    (field_types:(string * Types.datatype) list) (query:string) (db : database)
    : Value.t =
  let result,rs = execute_select_result field_types query db in 
  build_result (result,rs)
;;
    

let execute_untyped_select (query:string) (db: database) : Value.t =
  let result = (db#exec query) in
    (match result#status with
       | `QueryOk -> 
           `List (map (fun row -> `List (map Value.box_string row)) result#get_all_lst)
       | `QueryError msg -> raise (Runtime_error ("An error occurred executing the query " ^ query ^ ": " ^ msg)))




