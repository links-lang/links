let () = Findlib.init ()

(** Convention: For a driver named mydriver, we expect the corresponding
     library file to be named links_mydriver.cma or links_myriver.cmxs **)
let driver_file_name driver_name =
  Dynlink.adapt_filename (Printf.sprintf "links_%s.cma" driver_name)

(** Convention: For a driver named mydriver, we expect a file named
    links_mydriver_dependencies.json to contain the dependencies of that driver **)
let deps_file_name driver_name =
  Printf.sprintf "links_%s_dependencies.json" driver_name

(** Convention: Given a driver named mydriver, expect the corresponding opam package
     to be named links-mydriver (if it exists) **)
let opam_package_name driver_name =
  Printf.sprintf "links-%s" driver_name

let opam_lib_folder_for_package pkg =
  try Some (Findlib.package_directory pkg)
  with Findlib.No_such_package _ -> None

let searchpaths_user_provided _ =
  let driverpaths_setting = Settings.get_value Basicsettings.db_driver_path in
  if driverpaths_setting = "" then
    []
  else
    List.map Utility.Sys.expand (String.split_on_char ':' driverpaths_setting)

let searchpaths_opam_provided driver_name =
  let driver_opam_package_name = opam_package_name driver_name in
  match opam_lib_folder_for_package driver_opam_package_name with
  | None -> []
  | Some f ->
     (* Changes folder path f from  some/path/lib/links-mydriver to some/path/share/links-mydriver
        Fun fact: OCaml needs better path manipulation facilities *)
     let lib_regexp =  ("\\(/lib/\\)\\(" ^ driver_opam_package_name ^ "/?\\)") in
     let share_folder_for_package =
       Str.global_replace (Str.regexp lib_regexp) "/share/\\2" f in
     [f;share_folder_for_package]

exception No_such_file
exception Dynlink_Error of string * Dynlink.error

type driver_dependency = {
    opam_package : string;
    files : string list
  }

(* The order determines the precedence *)
let search_path_generators = [
      searchpaths_user_provided;
      searchpaths_opam_provided
    ]

let get_dependencies driver_name =
  let unyojson_string = function
    | `String s -> s
    | _ -> failwith "ill-formed dependency JSON" in
  let unyojson_list_of_strings = function
    | `List json -> List.map unyojson_string json
    | _ -> failwith "ill-formed dependency JSON" in
  let unyojson_deps   = function
    | `List json -> List.map (function
                        | `Assoc map -> {
                            opam_package = unyojson_string (List.assoc "opam_package" map);
                            files = unyojson_list_of_strings (List.assoc "files" map);
                          }
                        | _ -> failwith "ill-formed dependency JSON"
                      ) json
    | _ -> failwith "ill-formed dependency JSON" in

  let rec getdeps = function
    | [] -> raise No_such_file
    | f::fs ->
       if Sys.file_exists f then
         begin
           Debug.print ("Loading dependency json file " ^ f);
           unyojson_deps (Yojson.Safe.from_file f)
         end
        else
          getdeps fs in
  let search_paths = List.flatten (List.map (fun f -> f driver_name) search_path_generators) in
  let dep_files = List.map (fun f -> Filename.concat f (deps_file_name driver_name)) search_paths in
  try
    getdeps dep_files
  with No_such_file ->
    Debug.print ("Potential paths for json dependency file we've  tried:");
    List.iter (Debug.print) dep_files;
    failwith "Could not find dependency json file"



let dynload_first_existing_file files =
  let rec try_loading loaded_already = function
    | [] -> if loaded_already then () else raise No_such_file
    | f::fs ->
       if Sys.file_exists f then
         begin
           if loaded_already then
             Debug.print (Printf.sprintf "Warning: Alternative candidate for dynloading file %s exists: %s" (Filename.basename f)  f)
           else
             begin
               try
                 Debug.print ("Loading " ^ f);
                 Dynlink.loadfile f
               with
               | Dynlink.Error e -> raise (Dynlink_Error (f, e))
             end;
           try_loading true fs
         end
       else
         try_loading loaded_already fs in
  try_loading false files

let load_dependency driver_name dep =
    let folder = match opam_lib_folder_for_package dep.opam_package with
      | Some f -> f
      | None -> failwith (Printf.sprintf "Driver %s depends on package %s, but it is not intalled" driver_name dep.opam_package) in
    List.iter (fun depfile ->
        let file = Dynlink.adapt_filename (depfile ^ ".cma") in
        let path = Filename.concat folder file in
        try
          dynload_first_existing_file [path]
        with | No_such_file -> failwith (Printf.sprintf "Could not find file %s, which is a dependency of driver %s"  path  driver_name)
             | Dynlink_Error (f, e) -> failwith (Printf.sprintf "Error while loading dependency file %s:\n%s" f (Dynlink.error_message e))
      ) dep.files

let load_driver driver_name =
  let search_paths = List.flatten (List.map (fun f -> f driver_name) search_path_generators) in
  let driver_files = List.map (fun f -> Filename.concat f (driver_file_name driver_name)) search_paths in
  try
    dynload_first_existing_file driver_files
  with | No_such_file -> failwith (Printf.sprintf "Could not find file for driver %s" driver_name)
       | Dynlink_Error (f, e) -> failwith (Printf.sprintf "Error while loading driver file %s:\n%s" f (Dynlink.error_message e))

let load driver_name =
  let dependencies = get_dependencies driver_name in
  List.iter (load_dependency driver_name) dependencies;
  load_driver driver_name
