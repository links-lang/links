module C = Configurator.V1

let check_opam = 
  try
    Sys.getenv "LINKS_BUILT_BY_OPAM"
  with
    Not_found -> "0"

let _ =
  let oc = open_out "opam.ml" in 
      if (check_opam = "0")
      then
      Printf.fprintf oc "let built_by_opam = false\n"
      else 
      Printf.fprintf oc "let built_by_opam = true\n";
      close_out oc
