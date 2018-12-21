open Utility
open Sugartypes
open SugarConstructors.SugartypesPositions
open CommonTypes


let path_sep = ":"





(* Turns a program corresponding to a file into a module by binding the potential expression to a variable *)
let modulify_program
    toplevel_module_name
    program
      : Sugartypes.binding =
  let (bindings, phraseopt) = program in
  let combined_bindings = match phraseopt with
    | None -> bindings
    | Some phrase ->
      let any_pattern_node : Pattern.with_pos = with_dummy_pos Pattern.Any in
      let phrase_binding : Sugartypes.binding =
        with_dummy_pos
          (Val (any_pattern_node, ([], phrase), Location.Unknown, None)) in
      bindings @ [phrase_binding] in
  with_dummy_pos (Module (toplevel_module_name, None, combined_bindings) )

let try_parse_file module_name filename : string * Sugartypes.binding * SourceCode.source_code =
  (* First, get the list of directories, with trailing slashes stripped *)
  let check_n_chop path =
    let dir_sep = Filename.dir_sep in
    if Filename.check_suffix path dir_sep then
      Filename.chop_suffix path dir_sep else path in

  let poss_stdlib_dir =
    let stdlib_path = Settings.get_value Basicsettings.StdLib.stdlib_path in
    if Settings.get_value Basicsettings.StdLib.use_stdlib then
      if stdlib_path <> "" then
        [check_n_chop stdlib_path]
      else
        (* Otherwise, follow the same logic as for the prelude.
         * Firstly, check the current directory.
         * Secondly, check OPAM *)
        let chopped_path = check_n_chop @@ Basicsettings.locate_file "stdlib" in
        [Filename.concat chopped_path "stdlib"]
    else [] in

  let poss_dirs =
    let path_setting = Settings.get_value Basicsettings.links_file_paths in
    let split_dirs = Str.split (Str.regexp path_sep) path_setting in
    "" :: "." :: poss_stdlib_dir @ (List.map (check_n_chop) split_dirs) in

  (* Loop through, trying to open the module with each path *)
  let rec loop = (function
    | [] -> failwith ("Could not find file " ^ filename)
    | x :: xs ->
        let candidate_filename =
          if x = "" then filename else (x ^ Filename.dir_sep ^ filename) in
        if Sys.file_exists candidate_filename then
          let program, pos_context = Parse.parse_file Parse.program candidate_filename in
          (candidate_filename, modulify_program module_name program, pos_context)
        else
          loop xs) in
  loop poss_dirs
