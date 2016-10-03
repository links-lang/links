(* OASIS_START *)
(* OASIS_STOP *)

(** Custom Links build code **)

open Ocamlbuild_plugin

(** Oasis should expose a tag ["use_obj"^lib].
    At the moment it doesn't, this is unused.
*)
let use s = Printf.sprintf "use_obj_%s" s

(** We tag the final binary with dependencies toward the external ocamlfind library. *)
let pkg s = Printf.sprintf "package(%s)" s

(** Check whether a given flag is set to true in _oasis file

    {!BaseEnvLight} allows use to look inside setup.data for
    configuration information.
*)
let oasis_flag env flag = BaseEnvLight.var_get flag env = "true"

(** The path to the main file, without extension. *)
let main = "links"

(** For a flag "foo", the internal library is of the form "links-foo". *)
let lib_of_flag flag = "links-"^flag

(** When a flag is enabled, we tag the main file with a dependency towards
    the internal corresponding library.
*)
let link_conditionally env (flag, file) =
  let _lib = lib_of_flag flag in
  if oasis_flag env flag then begin
    (* We need conditional linking, so only the binaries need the
       conditional link, not the source file.
    *)
    tag_file (main-.-"native") [pkg flag ];
    tag_file (main-.-"byte")   [pkg flag ];

    (** We plug in dependencies manually. See comment on {!use}.
        We tag the cm[xo] in order to force the link order.
    *)
    dep ["file:"^main-.-"cmx"] [file-.-"cmx"] ;
    dep ["file:"^main-.-"cmo"] [file-.-"cmo"] ;
  end

(** The list of flag that have associated libraries
    that we want conditionally linked. *)
let flags = [
  "postgresql", "lib/postgresql/pg_database" ;
  "mysql"     , "lib/mysql/mysql_database"   ;
  "sqlite"    , "lib/lite/lite_database"     ;
  "sqlite3"   , "lib/lite3/lite3_database"   ;
  "monetdb"   , "lib/monetdb/m5_database"    ;
]

let () = dispatch (fun hook ->

    (* Run the normal build process *)
    dispatch_default hook ;

    let env = BaseEnvLight.load () in
    List.iter (link_conditionally env) flags ;
  )
