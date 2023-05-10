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

let check_opam =
  Option.is_some (Sys.getenv_opt "LINKS_BUILT_BY_OPAM")

let _ =
  let oc = open_out "linkspath.ml" in
      if check_opam
      then
        (* If Links is built by OPAM *)
        let lib_path = input_line (Unix.open_process_in "opam var lib") in
        let etc_path = input_line (Unix.open_process_in "opam var etc") in
        let share_path = input_line (Unix.open_process_in "opam var share") in
          Printf.fprintf oc "let config = Some \"%s/links/config\"\n" etc_path;
          Printf.fprintf oc "let jslib = \"%s/links/js\"\n" lib_path;
          Printf.fprintf oc "let examples = \"%s/links/examples\"\n" share_path;
          Printf.fprintf oc "let stdlib = \"%s/links/stdlib\"\n" lib_path;
          Printf.fprintf oc "let prelude = \"%s/links/prelude.links\"\n" lib_path;
          close_out oc
      else
        (* If Links is complied from source *)
        let git_path =  input_line (Unix.open_process_in "git rev-parse --show-toplevel") in
          Printf.fprintf oc "let config = %s\n" "None";
          Printf.fprintf oc "let jslib = \"%s/lib/js\"\n" git_path;
          Printf.fprintf oc "let examples = \"%s/examples\"\n" git_path;
          Printf.fprintf oc "let stdlib = \"%s/lib/stdlib\"\n" git_path;
          Printf.fprintf oc "let prelude = \"%s/prelude.links\"\n" git_path;
        close_out oc
