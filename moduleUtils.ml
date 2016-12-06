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

  method datatype = function
    | `QualifiedTypeApplication _ -> {< has_no_modules = false >}
    | dt -> super#datatype dt

  method phrasenode = function
    | `QualifiedVar _ -> {< has_no_modules = false >}
    | pn -> super#phrasenode pn
end

let rec separate_modules = function
  | [] -> ([], [])
  | (`Module bs, pos) :: xs ->
      let (mods', remainder) = separate_modules xs in
      ((`Module bs, pos) :: mods', remainder)
  | xs -> ([], xs)


let contains_modules prog = not ((has_no_modules#program prog)#satisfied)
