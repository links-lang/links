module C = Configurator.V1

let check_opam = 
  try
    Sys.getenv "LINKS_BUILT_BY_OPAM"
  with
    Not_found -> "0"

let _ =
  let oc = open_out "linkspath.ml" in 
      print_string "LINKS_BUILT_BY_OPAM =  ";
      print_string check_opam;
      (* Printf.fprintf oc "let prelude = \"prelude.links\"\n"; *)

      if (check_opam = "1")
      then
        (* If Links is built by OPAM*)
        let lib_path = input_line (Unix.open_process_in "opam var lib") in
        let etc_path = input_line (Unix.open_process_in "opam var etc") in
        let share_path = input_line (Unix.open_process_in "opam var share") in
          Printf.fprintf oc "let config = Some \"%s/links/config\"" etc_path;
          Printf.fprintf oc "let jslib = \"%s/links/js\"\nlet examples = \"%s/links/examples\"\nlet stdlib = \"%s/links/stdlib\"\nlet prelude = \"%s/links/prelude.links\"\n" lib_path share_path lib_path lib_path
          (* Printf.fprintf oc "let lib_path = \"%s/links\"\nlet etc_path = \"%s/links\"\nlet share_path = \"%s/links\"\n" lib_path etc_path share_path *)
      else 
        (* If Links is not built by OPAM*)
        let git_path =  input_line (Unix.open_process_in "git rev-parse --show-toplevel") in
        Printf.fprintf oc "let jslib = \"%s/lib/js\"\nlet examples = \"%s/examples\"\nlet stdlib = \"%s/lib/stdlib\"\nlet prelude = \"%s/prelude.links\"\n" git_path git_path git_path git_path;
        Printf.fprintf oc "let config = %s" "None";
      close_out oc

