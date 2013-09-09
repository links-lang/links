open Num
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
	  (* num_of_string is expensive *)
(*    | `Primitive `Int  -> Value.box_int (num_of_string value) *)
    | `Primitive `Int  -> Value.box_int (Num.Int (int_of_string(value)))
    | `Primitive `Float -> (if value = "" then Value.box_float 0.00      (* HACK HACK *)
                            else Value.box_float (float_of_string value))
    | t -> failwith ("value_of_db_string: unsupported datatype: '" ^ 
                     Show.show Types.show_datatype t(*string_of_datatype t*)^"'")

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
                         | [[id]] -> Value.box_int (Num.num_of_string id)
                         | _ -> raise (Runtime_error ("Returned the wrong number of results executing " ^ q))
                     end
               | `QueryError msg -> raise (Runtime_error ("An error occurred executing the query " ^ q ^ ": " ^ msg))
            end
      | q :: qs ->
          execute_command q db;
          run qs
  in
    run qs

(*
let execute_select
    (field_types:(string * Types.datatype) list) (query:string) (db : database)
    : Value.t =

  let result_signature result =
    let n = result#nfields in
    let rec rs i =
      if i >= n then
        []
      else
        let name = result#fname i in
          if start_of ~is:"order_" name then
            (* ignore ordering fields *)
            rs (i+1)
          else if List.mem_assoc name field_types then
            (name, (List.assoc name field_types, i)) :: rs (i+1)
          else
            failwith("Column " ^ name ^
                        " had no type info in query's type spec: " ^
                        mapstrcat ", " (fun (name, t) -> name ^ ":" ^
                          Types.string_of_datatype t)
                        field_types)
    in
      rs 0 in

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
               
		  let fields = result_signature result in

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


let execute_select
    (field_types:(string * Types.datatype) list) (query:string) (db : database)
    : Value.t =

(* build result signature: a list of optional name,type pairs
None means skip the field, Some (n,t) means build field n with type t *)

  let result_signature result =
    (* TODO: Avoid clash with fields genuinely named "null" *)
    let is_null name = (name = "null") in
    
    let n = result#nfields in
    let rec rs i =
      if i >= n then
        [],false
      else
	let fields,null_query = rs (i+1) in
        let name = result#fname i in
          if start_of ~is:"order_" name then
            (* ignore ordering fields *)
            (None :: fields), null_query
          else if List.mem_assoc name field_types then
	    (Some (name, List.assoc name field_types) :: fields), 
	    null_query || is_null(name)
          else
            failwith("Column " ^ name ^
                        " had no type info in query's type spec: " ^
                        mapstrcat ", " (fun (name, t) -> name ^ ":" ^
                          Types.string_of_datatype t)
                        field_types)
    in
      rs 0 in

(* apply result signature to a row *)
  let build_record rs row = 
    let rec build rs i = 
      match rs with
      | [] -> []
      | None::rs' -> build rs' (i+1)
      | Some(name,t)::rs' -> 
	  (name,value_of_db_string (row i) t)::(build rs' (i+1))
      | _ -> assert false
    in build rs 0
  in 
  let result = db#exec query in

    match result#status with
      | `QueryError msg -> 
        raise(Runtime_error("An error occurred executing the query " ^ query ^
                               ": " ^ msg))
      | `QueryOk -> 
	    match field_types with
          | [] ->
                (* Ignore any dummy fields introduced to work around
                   SQL's inability to handle empty column lists *)
                `List (result#map (fun _ -> `Record []))
            | _ ->
		  let rs, null_query = result_signature result in
  		  if null_query then
                    `List (result#map (fun _ -> `Record []))
                  else
		    (Debug.debug_time "map" (fun () -> 
                      `List (result#map 
			(fun row ->
                        `Record (build_record rs  row))
			))
		       )
		      



let execute_untyped_select (query:string) (db: database) : Value.t =
  let result = (db#exec query) in
    (match result#status with
       | `QueryOk -> 
           `List (map (fun row -> `List (map Value.box_string row)) result#get_all_lst)
       | `QueryError msg -> raise (Runtime_error ("An error occurred executing the query " ^ query ^ ": " ^ msg)))
