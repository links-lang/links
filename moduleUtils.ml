open Utility

let module_sep = "."

(* TODO: Unix-specific one for the moment, but no easy way to get this
 * from OCaml Filename module... *)
let path_sep = ":"

let try_parse_file filename =
  (* First, get the list of directories, with trailing slashes stripped *)
  let poss_dirs =
    let path_setting = Settings.get_value Basicsettings.links_file_paths in
    let split_dirs = Str.split (Str.regexp path_sep) path_setting in
    "" :: "." :: (List.map (fun path ->
      let dir_sep = Filename.dir_sep in
      if Filename.check_suffix path dir_sep then
        Filename.chop_suffix path dir_sep
      else
        path) split_dirs) in

  (* Loop through, trying to open the module with each path *)
  let rec loop = (function
    | [] -> failwith ("Could not find file " ^ filename)
    | x :: xs ->
        let candidate_filename =
          if x = "" then filename else (x ^ Filename.dir_sep ^ filename) in
        if Sys.file_exists candidate_filename then
          Parse.parse_file Parse.program candidate_filename
        else
          loop xs) in
  loop poss_dirs


let has_no_modules =
object
  inherit SugarTraversals.predicate as super

  val has_no_modules = true
  method satisfied = has_no_modules

  method bindingnode = function
    | `QualifiedImport _
    | `Module _ -> {< has_no_modules = false >}
    | b -> super#bindingnode b
end

let contains_modules prog = true (* (not ((has_no_modules#program prog)#satisfied)) *)
