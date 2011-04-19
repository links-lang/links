open Utility

(* M5_database 
   Implements the Value.database interface
   for MonetDB5 back ends *)

let error_str = from_option "no error string"

class m5_dbresult (conn : Mapi.t) (handle : Mapi.handle) = object
  inherit Value.dbvalue
  val conn = conn
  val handle = handle
  method status : Value.db_status =
    match Mapi.error conn with
      | Mapi.MOK -> `QueryOk
      | _ -> `QueryError (from_option "no error string" (Mapi.error_str conn))
        
  method nfields : int = Mapi.get_field_count handle

  method fname : int -> string = fun index ->
    match Mapi.field_name handle index with
      | Some name -> name
      | None -> failwith ("MonetDB get_field_name failed")

  method get_all_lst : string list list = 
    match Mapi.seek_row handle Int64.zero Mapi.SEEK_SET with
      | Mapi.MOK -> 
	  let nr_rows = Int64.to_int (Mapi.get_row_count handle) in
	  let rec loop i l =
	    if i = nr_rows then
	      List.rev l
	    else
	      begin
		match Mapi.fetch_row handle with
		  | None -> failwith "MonetDB fetch_row failed"
		  | Some _ -> 
		      begin
			match Mapi.fetch_field_list handle with
			  | Some fields -> loop (i + 1) (fields :: l)
			  | None -> failwith ("MonetDB fetch_field_list failed")
		      end
	      end
	  in
	    loop 0 []
      | _ -> 
	  failwith ("MonetDB seek_row failed: "^(error_str (Mapi.result_error handle)))
	  
  method error : string = error_str (Mapi.error_str conn)

end

class m5_database host port dbname user password = object(_self)
  inherit Value.database

  val connection =
      match Mapi.connect ~host:host ~port:(int_of_string port) ~user:user ~passwd:password ~lang:Mapi.SQL ~db:dbname with
	| Some conn when Mapi.connection_ok conn ->
	    conn
	| Some conn -> 
	    failwith ("MonetDB connect failed: " ^(error_str (Mapi.error_str conn)))
	| None ->
	    failwith ("MonetDB connect failed: unknown error")
      
  method driver_name () = "monetdb5"
  method exec : string -> Value.dbvalue = fun query ->
    match Mapi.query connection query with
      | Some handle when Mapi.connection_ok connection -> 
	  begin
	    match Mapi.fetch_all_rows handle with
	      | Some _ -> new m5_dbresult connection handle
	      | None -> failwith ("MonetDB fetch_all_rows failed")
	  end
      | Some _handle -> 
	  failwith ("MonetDB query failed: "^(error_str (Mapi.error_str connection)))
      | None -> 
	  failwith ("MonetDB query failed: unknown error")
  method escape_string s = Mapi.quote s
  method make_insert_query (table_name, field_names, vss) =
    "insert into " ^ table_name ^
      "("^String.concat "," field_names ^") "^
      String.concat " union all " (List.map (fun vs -> "select " ^ 
                                               String.concat "," vs) vss)
end

let driver_name = "monetdb5"

let get_m5_database_by_string args =
  match Utility.split_string args ':' with
    | (name::host::port::user::pass::_) ->
	(new m5_database host port name user pass,
	 Value.reconstruct_db_string (driver_name, args))
    | _ ->
        failwith "Insufficient arguments when establishing MonetDB5 connection"

let _ = Value.register_driver (driver_name, get_m5_database_by_string)
