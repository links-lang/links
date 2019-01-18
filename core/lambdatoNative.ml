
(* Initialises the OCaml backend *)
let initialise ?(includes=[]) () =
  (*  Clflags.inlining_report := false;*)
  Clflags.include_dirs :=
    includes @ !Clflags.include_dirs; (* Paths for module lookup *)
  Clflags.nopervasives := false;      (* Whether to include the Pervasives module *)
  Clflags.dump_lambda := true;       (* Dump lambda ir *)
  Clflags.dump_clambda := true;      (* Dump clambda ir *)
  Clflags.dump_cmm := true;          (* Dump c-- code *)
  Clflags.keep_asm_file := false;
  Clflags.native_code := true;        (* Whether to use the native or byte code compilation pipeline *)
  Clflags.opaque := true;
  Compmisc.init_path true;            (* true for native code compilation *)
  Ident.reinit ()

let initial_env () =
  Compmisc.initial_env ()

let compile modident prog out =
  let ppf = Format.std_formatter in
  let init_env ((modname, _, _, _) as comp_unit) =
    Env.set_unit_name modname;
    Compilenv.reset ?packname:!Clflags.for_package modname;
    comp_unit
  in
  let simplify_lambda (modname, prog, cmxfile, cmifile) =
    let open Lambda in
    let prog =
      { prog with code = Simplif.simplify_lambda modname prog.code }
    in
    (modname, prog, cmxfile, cmifile)
  in
  let gen_cmifile (modname, prog, cmxfile, _) =
    let prefix = Misc.chop_extensions cmxfile in
    let _ = Env.save_signature ~deprecated:None [] modname cmxfile in
    let cmifile = Printf.sprintf "%s.cmi" prefix in
    (modname, prog, cmxfile, cmifile)
  in
  let save_cmxfile ((_, _, cmxfile, _) as comp_unit) =
    Compilenv.save_unit_info cmxfile;
    comp_unit
  in
  let asm_compile ((_, prog, cmxfile, _) as comp_unit) =
    let target_wo_ext = Misc.chop_extensions cmxfile in
    Asmgen.compile_implementation_clambda target_wo_ext ppf prog;
    comp_unit
  in
  let assemble_exec cmxs out =
    Printf.printf "cmds: %s\n%!" (List.hd cmxs);
    Printf.printf "pwd: %s\n%!" (Sys.getcwd());
    Printf.printf "out: %s\n%!" out;
    Printf.printf "ls: %!";
    (*Sys.command (Printf.sprintf "ls -al %s" (List.hd cmxs));
    Sys.command "sleep 2";
    Sys.command (Printf.sprintf "cat %s" (List.hd cmxs));*)
    (Asmlink.link ppf cmxs out)
  (* Pipeline *)
  in
  let comp_unit =
    let modname = Ident.name modident in
    let basedir = Filename.dirname out in
    let cmxfile = (Sys.getcwd  () ^ (String.uncapitalize_ascii modname) ^ ".cmx") in
    (modname, prog, cmxfile, "<none>")
  in
  let (_, _, cmxfile, _) =
    init_env comp_unit
    |> simplify_lambda
    |> gen_cmifile
    |> save_cmxfile
    |> asm_compile
  in
  assemble_exec [cmxfile] out
