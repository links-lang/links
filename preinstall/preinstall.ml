module C = Configurator.V1

let rec get_content path =
  let join prefix file = if prefix="" then file else Printf.sprintf "%s/%s" prefix file in
  if Sys.is_directory path then
    Array.map (join path) (Sys.readdir path)
    |> Array.fold_left (fun acc path ->
        let content = get_content path in
        Array.append acc content) [||]
  else
    [|path|]

let make_stanza section files =
  Printf.sprintf
    "(install\n\
    \  (package links)\n\
    \  (section %s)\n\
    \  (files %s))\n"
    section files

let install_examples contents =
  contents
  |> Array.to_list
  |> List.map (fun file -> Printf.sprintf {|(%s as %s)|} file file)
  |> String.concat "\n         "
  |> make_stanza "share"

let install_config =
  make_stanza "etc" "config"

let write_content filename =
  let examples =
    get_content "examples"
    |> install_examples
    |> String.split_on_char '\n'
  in
  let config =
    install_config
    |> String.split_on_char '\n'
  in
  C.Flags.write_lines filename (List.concat [examples; config])


let config libdir = [
  "jsliburl=/lib/js";
  Printf.sprintf "jslibdir=%s/js" libdir;
  Printf.sprintf "stdlib_path=%s/stdlib" libdir;
  "#database_driver=postgresql";
  "#database_args=localhost:5432:user:pass" ]

let write_config libdir =
  config libdir
  |> C.Flags.write_lines "config"

let _ =
  let libdir = ref None in
  let incfile = ref "install.inc" in
  C.main ~name:"preinstall"
    ~args:[
      "-libdir", Arg.String (fun s -> libdir := Some s),
      "Path to the opam package libdir"
    ; "-output", Arg.String (fun s -> incfile := s),
      "Name for the install sexp output (default install.inc)"]
    (fun _ ->
       begin match !libdir with
         | Some libdir -> write_config libdir
         | _ -> failwith "-libdir is needed"
       end;
       write_content !incfile;
    )

