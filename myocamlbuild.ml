(* OASIS_START *)
(* DO NOT EDIT (digest: d41d8cd98f00b204e9800998ecf8427e) *)
(* OASIS_STOP *)

(** Custom Links build code **)

open Ocamlbuild_plugin

(** Check whether a given flag is set to true in _oasis file **)
let oasis_flag env flag = BaseEnvLight.var_get flag env = "true"

let () = dispatch (fun hook ->

  (* If libraries required by database backends are not installed then
     ocamlbuild will display a bogus warning that a tag relating to that library
     is unused.  This silences the warnings. *)
  let _ = List.map mark_tag_used [ "pkg_postgresql"; "pkg_mysql"; "pkg_sqlite";
                                   "pkg_sqlite3"; "pkg_mapi" ] in

  (* Run the normal build process *)
  dispatch_default hook;

  let env_filename = Pathname.basename BaseEnvLight.default_filename in
  (* JSTOLAREK: I think I can just skip named arguments *)
  let env = BaseEnvLight.load ~filename:env_filename ~allow_empty:true () in ()

(*
  if oasis_flag env "postgres" then begin
      print_string "postgres"
    end
  else if oasis_flag env "mysql" then begin
      print_string "mysql"
    end;
  print_string "foo"

  match hook with
  | After_rules ->
     if oasis_flag env "is_native" then
       Command.execute (mv "links.native" "links")
     else
       Command.execute (mv "links.byte" "links")
  | _ -> ()
 *)
 )
