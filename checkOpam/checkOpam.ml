module C = Configurator.V1

let built_by_opam =
   if Sys.file_exists ".opam" then true
   else false

let _ =
  C.main ~name:"checkOpam"
  ~args:[
    "-init", Arg.Unit (fun () -> let oc = open_out ".opam" in close_out oc ),
    "Create .opam file"
  ; "-check", Arg.Unit (fun () -> (),
    "Fetch the built_by_opam indicator"
  ; "-clean-up", Arg.Unit (fun () -> Sys.remove ".opam"),
    "Delete .opam file" ]
  (fun _ ->
    (* Just for debugging *)
    print_string (Printf.sprintf "built_by_opam: %s\n" (Bool.to_string (built_by_opam)))
  )


