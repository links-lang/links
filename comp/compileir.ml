(* Compiler driver *)
open Irtolambda

let dump_lambda lam =
  if Settings.get_value Basicsettings.show_lambda_ir
  then Format.fprintf Format.err_formatter "%a@\n@." Printlambda.lambda lam
  else ()      

let bytecomp module_name lambda_program  =
  let objfile = module_name ^ ".cmo" in
  let impl = Bytegen.compile_implementation module_name lambda_program in
  let fd = open_out objfile in
  begin
    Emitcode.to_file fd module_name objfile impl;
    close_out fd
  end (* TODO: Handle errors; add clean-up code, etc... *)

let compile env prelude program =
  let () = if Settings.get_value Basicsettings.show_compiled_ir
	   then print_endline (Ir.Show_program.show program)
	   else ()
  in
  let lam = lambda_of_ir env program in
  dump_lambda lam;
  if Settings.get_value Basicsettings.dry_run
  then ()
  else if Settings.get_value Basicsettings.bytecomp
       then bytecomp "helloworld" lam
       else ()
