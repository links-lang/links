(*pp deriving *)
(*
([`Fun ((1790, ((String) ~> (), "myprint", `Global )),
         ([(6429, (`Unl , `Any ), `Row ...)],
           [(1789, (String, "", `Local ))],
           ([`Let ((1788, (String, "s", `Local )),
                    ([], `Return `Variable 1789))],
             `Apply (`TApp (`TAbs ([(6427, (`Unl , `Any ), `Row ...)],
                                    `TApp (`Variable 61,
                                            [`Row ({}, ..., false)])),
                             [`Row ({}, ..., false)]), [`Variable 1788]))),
         None, `Unknown )],
  `Apply (`TApp (`TAbs ([(6431, (`Unl , `Any ), `Row ...)],
                         `TApp (`Variable 1790, [`Row ({}, ..., false)])),
                  [`Row ({}, ..., false)]),
           [`Constant `String "Hello World!"]))

 *)			     

let string_of_environment env =
  let module Show_IntStringEnv = Env_links.Int.Show_t(Deriving_Show.Show_string) in
  Show_IntStringEnv.show env			   

let invert env =
  let module Env = Env_links in
  Env.String.fold
    (fun name var env ->
      if Env.Int.has env var then
        failwith ("(invert_env) duplicate variable in environment")
      else
        Env.Int.bind env (var, name))
    env Env.Int.empty
    		
let ocaml_of_ir _ _ _ =
  (*  let v = Lambda.Pidentity in*)
  let print_hello : Lambda.lambda =
  Lambda.(
    Compmisc.init_path false;
    let () = Ident.reinit () in
    let print_endline, _ = Env.lookup_value
			     (Longident.(Ldot (Lident "Pervasives", "print_endline")))
			     Env.empty
    in
    Lsequence ((Lapply (transl_path ~loc:Location.none Env.empty print_endline,
			[Lconst (Const_immstring "Hello, world")],
			no_apply_info))
	      , Lprim (Pmakeblock (0, Asttypes.Immutable), []))
	      
  )
  in
  let hello_world : Lambda.lambda =
    Lambda.(
      Compmisc.init_path false;
      let () = Ident.reinit () in
      Lsequence (Lconst (Const_immstring "Hello, world")
		, Lprim (Pmakeblock (0, Asttypes.Immutable), []))
    )
  in
  let bytecomp () =
    let impl = Bytegen.compile_implementation "helloworld" print_hello in
    let fd = open_out "helloworld.cmo" in
    begin
      Emitcode.to_file fd "helloworld" "helloworld.cmo" impl;
      close_out fd
    end
  in
(*  let nativecomp () =
    Compmisc.init_path true;
    let modulename = "Helloworld" in
    (* Proc.init (); *)
    (* Reg.reset (); *)
    Env.set_unit_name modulename;
    Compilenv.reset modulename;
    let impl = Simplif.simplify_lambda print_hello in
    let () = Asmgen.compile_implementation "helloworld" Format.err_formatter (0, impl) in
    Compilenv.save_unit_info "helloworld.cmx"
  in*)
  let _ = Format.fprintf Format.err_formatter "%a@\n@." Printlambda.lambda hello_world in
  nativecomp(); "TEST"
