open List
open CommonTypes
open Utility

type database = Value.database
let runtime_error str = (Errors.runtime_error str)

class virtual db_args from_str = object
  val strval : string = from_str
  method virtual from_string : string -> unit
end

let value_of_db_string (value:string) t =
  match TypeUtils.concrete_type t with
    | `Primitive Primitive.Bool ->
        (* HACK:

           This should probably be part of the database driver as
           different databases have different representations of
           booleans.

           mysql appears to use 0/1 and postgres f/t
        *)
        Value.box_bool (value = "1" || value = "t" || value = "true")
    | `Primitive Primitive.Char -> Value.box_char (String.get value 0)
    | `Primitive Primitive.String -> Value.box_string value
    | `Primitive Primitive.Int  ->
        (* HACK: Currently Links does not properly handle integers
         * if they are null. This is a temporary workaround (hack) to
         * allow us to at least interface with DBs containing nulls,
         * until we manage to do the research required to do something
         * more principled.
         * If "coerce_null_integers" is true and a null integer is found,
         * then instead of crashing, "null_integer" is used instead. *)
        if value = "" then
          if Settings.get_value (Basicsettings.Database.coerce_null_integers) then
            Value.box_int (Settings.get_value Basicsettings.Database.null_integer)
          else
            raise (Errors.RuntimeError ("Attempted to read null integer from the database"))
        else
          Value.box_int (int_of_string value)
    | `Primitive Primitive.Float ->
       if value = "" then Value.box_float 0.00      (* HACK HACK *)
       else Value.box_float (float_of_string value)
    | t -> raise (runtime_error
      ("value_of_db_string: unsupported datatype: '" ^
        Types.string_of_datatype t ^"'"))

let execute_command  (query:string) (db: database) : Value.t =
  let result = (db#exec query) in
    begin
      match result#status with
        | `QueryOk -> `Record []
        | `QueryError msg ->
            raise (runtime_error
              ("An error occurred executing the query " ^ query ^ ": " ^ msg))
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
                         | _ ->
                             raise (runtime_error ("Returned the wrong number of results executing " ^ q))
                     end
               | `QueryError msg ->
                   raise (runtime_error ("An error occurred executing the query " ^ q ^ ": " ^ msg))
            end
      | q :: qs ->
        let _unit = execute_command q db in
        run qs
  in
    run qs


let is_null name = (name = "null")

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
	  else if start_of ~is:"@unit@" name then
            let fields,null_query = rs (i+1) in
            (name, (Types.unit_type,i)) :: fields,
            null_query
          else if List.mem_assoc name field_types then
            let fields,null_query = rs (i+1) in
            (name, (List.assoc name field_types, i)) :: fields,
            null_query && is_null(name)
          else
            raise (runtime_error
              ("Column " ^ name ^
               " had no type info in query's type spec: " ^
               mapstrcat ", " (fun (name, t) -> name ^ ":" ^
                 Types.string_of_datatype t)
               field_types))
    in let rs, null_query = rs 0
    in if null_query then [] else rs


(* builds record given a row field accessor function *)
 let build_record (rs: (string * (Types.datatype * int)) list) (row:int -> string) =
    let rec build rs l =
      match rs with
      | [] -> l
      | (name,(t,i))::rs' ->
      build rs' ((name,value_of_db_string (row i) t)::l)
    in build rs []


(* BUG: Lists can be too big for List.map; need to be careful about recursion *)

let execute_select_result
    (field_types:(string * Types.datatype) list) (query:string) (db: database)  =
  let _ = Debug.print ("Running query: \n" ^ query) in
  let result = (db#exec query) in
    (match result#status with
       | `QueryOk ->
           result,
       result_signature field_types result
       | `QueryError msg ->
           raise (runtime_error
             ("An error occurred executing the query " ^ query ^ ": " ^ msg)))


let build_result ((result:Value.dbvalue),rs) =
  `List (result#map (fun row ->
                     `Record (build_record rs row))
       )

let execute_select
    (field_types:(string * Types.datatype) list) (query:string) (db : database)
    : Value.t =
  let result,rs = execute_select_result field_types query db in
  build_result (result,rs)


let execute_untyped_select (query:string) (db: database) : Value.t =
  let result = (db#exec query) in
    (match result#status with
       | `QueryOk ->
           `List (map (fun row -> `List (map Value.box_string row)) result#get_all_lst)
       | `QueryError msg ->
           raise (runtime_error ("An error occurred executing the query " ^ query ^ ": " ^ msg)))
