open Utility

let module_sep = ":::"

type binding_stack_node = [
  | `OpenStatement of string
  | `LocalVarBinding of string
]

(* Given a name and a prefix, appends the prefix (with separator)
 * as long as the prefix is not the empty string *)
let prefixWith name prefix =
  if prefix = "" then name else prefix ^ module_sep ^ name

let print_stack_node = function
 | `OpenStatement mn -> "module: " ^ mn
 | `LocalVarBinding lvb -> "var: " ^ lvb

let print_module_stack s = print_list (List.map print_stack_node s)

let rec moduleInScopeInner seen_modules binding_stack module_name =
  match binding_stack with
    | [] ->
        if StringSet.mem module_name seen_modules then Some(module_name) else None
    | (`LocalVarBinding _)::xs -> moduleInScopeInner seen_modules xs module_name
    | (`OpenStatement x)::xs ->
        let fully_qual = prefixWith module_name x in
        if StringSet.mem fully_qual seen_modules then
          Some(fully_qual)
        else
          moduleInScopeInner seen_modules xs module_name

let moduleInScope seen_modules binding_stack module_name =
  moduleInScopeInner seen_modules binding_stack module_name

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
