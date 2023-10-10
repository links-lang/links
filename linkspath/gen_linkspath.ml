(** This module builds another ML module, which contains various
   computed paths used by the Links runtime. The interface of the
   generated module is as follows:

   val config : string option (* If `Some fname` then `fname` is the
                                 absolute filename of the default config file. *)

   val jslib : string         (* Absolute path to the Links JavaScript runtime directory. *)
   val examples : string      (* Absolute path to the Links examples directory. *)
   val stdlib : string        (* Absolute path to the Links standard library directory. *)
   val prelude : string       (* Absolute filename for `prelude.links`. *)

   The generated module is meant for inclusion in Links core.  *)

module C = Configurator.V1

let is_git_repository () =
  let ic = Unix.open_process_in "git rev-parse --is-inside-work-tree 2> /dev/null" in
  let ans =
    try String.equal "true" (input_line ic) with _ -> false
  in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> ans
  | _ -> false

let check_opam =
  Option.is_some (Sys.getenv_opt "LINKS_BUILT_BY_OPAM")

let _ =

  let[@warning "-8"] [config; jslib; examples; stdlib; prelude]  =
    if check_opam (* If Links is being built by OPAM *)
    then let lib = input_line (Unix.open_process_in "opam var lib") in
         let etc = input_line (Unix.open_process_in "opam var etc") in
         let share = input_line (Unix.open_process_in "opam var share") in
         let[@warning "-8"] [etc; lib; share] =
           List.map (fun dir -> Filename.concat dir "links") [etc; lib; share]
         in
         let config = Printf.sprintf "Some \"%s\"" (Filename.concat etc "config") in
         let jslib = Filename.concat lib "js" in
         let examples = Filename.concat share "examples" in
         let stdlib = Filename.concat lib "stdlib" in
         let prelude = Filename.concat lib "prelude.links" in
         [config; jslib; examples; stdlib; prelude]
    else if is_git_repository () (* If Links is being built from the git repository sources *)
    then let root = input_line (Unix.open_process_in "git rev-parse --show-toplevel") in
         let config = "None" in
         let paths =
           List.map (Filename.concat root) ["lib/js"; "examples"; "lib/stdlib"; "prelude.links"]
         in
         config :: paths
    else (* If Links is being built from other sources, then we do a
            best effort guess based of the current working
            directory. *)
         let cwd = Sys.getcwd () in
         let config = "None" in
         let paths =
           List.map (Filename.concat cwd) ["../lib/js"; "../examples"; "../lib/stdlib"; "../prelude.links"]
         in
         config :: paths
  in
  let oc = open_out "linkspath.ml" in
  Printf.fprintf oc "let config = %s\n" config;
  Printf.fprintf oc "let jslib = \"%s\"\n" jslib;
  Printf.fprintf oc "let examples = \"%s\"\n" examples;
  Printf.fprintf oc "let stdlib = \"%s\"\n" stdlib;
  Printf.fprintf oc "let prelude = \"%s\"\n" prelude;
  close_out oc
