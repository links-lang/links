open Utility

exception LocateFailure of string
exception IllformedPluginDescription of string
exception DependencyLoadFailure of string * Dynlink.error
exception LoadFailure of string * Dynlink.error

(* There are two artifacts associated with a dynamic loadable database
   driver:

      1) An OCaml plugin that implements the database driver
         interface.
      2) The second artifact is a description of the plugin's
         dependencies (OCaml packages).

   The description format is an ad-hoc JSON format. The plugin is
   assumed to have name 'links_<driver>.cma' where the extension is
   (Dynlink) adaptable. The dependency file is assumed to be named
   'links_driver_dependencies.json'. The plugin is assumed to live in
   directory named 'lib/links-<driver>', whilst the dependency
   description is assumed to live in 'share/links-<driver>'.  *)

let plugin_file basic_name =
  Dynlink.adapt_filename (Printf.sprintf "%s.cma" basic_name)

module Locator = struct
  type plugin =
    { cma: string;
      dependencies: string }

  module Glob = Glob.Make(DefaultPolicy)

  let plugin_dirname driver_name =
    Printf.sprintf "links-%s" driver_name

  let plugin_filename driver_name =
    Printf.sprintf "links_%s" driver_name

  let findlib_initialised = ref false
  let package_directory : string -> string
    = fun pkg_name ->
    (if not !findlib_initialised then Findlib.init ());
    try Findlib.package_directory pkg_name
    with Findlib.No_such_package _ -> raise Not_found

  let subst prefix filename =
    let basename = Filename.basename filename in
    let basedir = Filename.(dirname (dirname filename)) in
    Filename.concat basedir (String.concat Filename.dir_sep [prefix; basename])

  let share = subst "share"
  let lib = subst "lib"

  let opam_share driver_name =
    try
      let dir =
        package_directory (plugin_dirname driver_name)
      in
      let dir = share dir in
      if Sys.is_directory dir then [dir]
      else raise Not_found
    with Not_found -> []

  let plugin_dir dependency_file =
    let libdir = lib (Filename.dirname dependency_file) in
    if Sys.file_exists libdir && Sys.is_directory libdir then Some libdir
    else None

  (* Convention: For a driver named mydriver, we expect the corresponding
     dependency file to be named links_mydriver_dependencies.json. *)
  let dependency_pattern : string -> Str.regexp
    = fun driver_name ->
    Str.regexp (Printf.sprintf "links_%s_dependencies\\.json$" driver_name)

  let locate : ?opam_fallback:bool -> string list -> string -> plugin list
    = fun ?(opam_fallback=true) paths driver_name ->
    let rec loop fallback pattern = function
      | [] when fallback ->
         loop false pattern (opam_share driver_name)
      | [] -> raise Not_found
      | path :: paths ->
         let files =
           try Glob.files path pattern with
             | Disk.AccessError _ -> [] in
         match files with
         | [] -> loop fallback pattern paths
         | fs -> fs
    in
    let results = loop opam_fallback (dependency_pattern driver_name) paths in
    let results =
      List.fold_right
        (fun file acc ->
          let dependency_file = Disk.File.to_filename file in
          match plugin_dir dependency_file with
          | None -> acc
          | Some dir ->
             let driver_cma = plugin_file (plugin_filename driver_name) in
             let target = Filename.concat dir driver_cma in
             if Sys.file_exists target
             then { cma = target; dependencies = dependency_file } :: acc
             else acc)
        results []
    in
    match results with
    | [] -> raise Not_found
    | _ -> results
end

module Adhoc_dependency_parser = struct
  exception Illformed
  type dependency =
    { pkg: string;
      cmas: string list }
  let parse : string -> dependency list
    = fun filename ->
    let open Yojson.Safe.Util in
    try
      let json = Yojson.Safe.from_file filename in
      let transform json =
        let pkg =
          member "opam_package" json
          |> to_string
        in
        let cmas =
          member "files" json
          |> to_list
          |> List.map to_string
        in
        { pkg; cmas }
      in
      convert_each transform json
    with Yojson.Safe.Util.Type_error _ -> raise Illformed
end

module Loader = struct

  let load_dependency : Adhoc_dependency_parser.dependency -> unit
    = fun dependency ->
    let open Adhoc_dependency_parser in
    let basedir = Locator.package_directory dependency.pkg in
    let load basedir name =
      let filename = Filename.concat basedir (plugin_file name) in
      try
        Dynlink.loadfile filename
      with Dynlink.Error e -> raise (DependencyLoadFailure (filename, e))
    in
    List.iter (load basedir) dependency.cmas


  let load : ?opam_fallback:bool -> ?path:string list -> string -> unit
    = fun ?(opam_fallback=true) ?(path=[]) driver_name ->
    let candidates =
      try Locator.locate ~opam_fallback path driver_name
      with Not_found -> raise (LocateFailure driver_name)
    in
    let open Locator in
    let target = match candidates with
      | [] -> assert false
      | [f] ->
         Debug.print (Printf.sprintf "debug: found loadable database driver\n- %s\n- %s\n%!" f.cma f.dependencies);
         f
      | f :: _ ->
         Printf.fprintf stderr "warning: ambiguous database driver match. Arbitrarily picking %s.\n%!" f.cma; f
    in
    let open Adhoc_dependency_parser in
    let dependencies =
      try parse target.dependencies
      with Illformed ->
        raise (IllformedPluginDescription target.dependencies)
    in
    List.iter load_dependency dependencies;
    try
      Dynlink.loadfile target.cma
    with Dynlink.Error e -> raise (LoadFailure (target.cma, e))
end


let load driver_name =
  let path =
    let settings_path =
      Settings.get_value Basicsettings.DatabaseDrivers.path in
    if settings_path = "" then [] else String.split_on_char ':' settings_path
  in
  Loader.load ~path driver_name
