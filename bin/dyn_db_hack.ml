(* This module automatically discovers and loads database
   drivers. This is a temporary hack until we have a proper
   interface for loading plugins. *)
open Utility
let load () =
  let exception Plugin_not_found in
  let sanitise f = (* Adapts file extensions depending on running mode [byte,native] *)
    Dynlink.adapt_filename (replace "-" "_" f)
  in
  (* Construct search paths *)
  let search_paths =
    let user_paths = (* User defined search paths *)
      try
        split_string (Sys.expand "$LINKS_LD_LIBRARY_PATH") ':'
      with _ -> []
    in
    let dblib_paths = (* construct a path for each database *)
      let subdirs = ["postgresql"; "mysql"; "sqlite3"] in
      let subdirs =
        (List.map (Printf.sprintf "%s-%s" "links") subdirs) @ subdirs
      in
      List.map
        (fun base ->
          (List.map (Filename.concat base) subdirs))
        (Findlib.search_path ())
    in
    (* Ensure that user-defined paths have higher precedence than
       auto-discovered library paths *)
    user_paths @ (List.concat dblib_paths)
  in
  let rec try_load plugin = function
    | [] -> raise Plugin_not_found
    | d :: dirs ->
       let abs_path = Printf.sprintf "%s/%s" d plugin in
       if Sys.file_exists abs_path
       then (Debug.print (Printf.sprintf "Loading shared library %s" abs_path);
             Dynlink.loadfile abs_path)
       else try_load plugin dirs
  in
  let backends = (* Supported database drivers and their dependencies (assume byte mode) *)
    [["postgresql.cmo";
      "links_postgresql.cmo"];
     ["mysql.cmo";
      "links_mysql.cmo"];
     ["sqlite3.cmo";
      "links_sqlite3.cmo"]]
  in
  let load deps =
    try
      List.iter
        (fun dep -> try_load (sanitise dep) search_paths) deps
    with Plugin_not_found -> ()
    | Dynlink.Error e -> prerr_endline (Dynlink.error_message e)
  in
  List.iter load backends
