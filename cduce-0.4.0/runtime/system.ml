(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Operators
open Builtin_defs
open Ident

let variant_type_ascii l =
  List.fold_left
    (fun accu (l,t) ->
       Types.cup accu
	 (Types.times 
	    (Types.cons (Types.atom (Atoms.atom (Atoms.V.mk_ascii l))))
	    (Types.cons t)))
    Types.empty
    l

let record_type_ascii l =
  Types.record_fields (false,
    (LabelMap.from_list_disj 
       (List.map (fun (l,t) -> Value.label_ascii l, Types.cons t) l)))

module Reader = struct
  let b = Buffer.create 10240
  let buf = String.create 1024
  
  let rec read_loop ic =
    let i = input ic buf 0 (String.length buf) in
    if i > 0 then (Buffer.add_string b (String.sub buf 0 i); read_loop ic)

  let ic ic =
    read_loop ic;
    let s = Buffer.contents b in
    Buffer.clear b;
    s
end

let run_process cmd =
  let (sout,sin,serr) as h = Unix.open_process_full cmd (Unix.environment()) in
  close_out sin;
  let sout = Reader.ic sout in
  let serr = Reader.ic serr in
  sout,serr, Unix.close_process_full h

let process_status = function
  | Unix.WEXITED n ->
      Value.Pair (Value.atom_ascii "exited", Value.ocaml2cduce_int n)
  | Unix.WSTOPPED n ->
      Value.Pair (Value.atom_ascii "stopped", Value.ocaml2cduce_int n)
  | Unix.WSIGNALED n ->
      Value.Pair (Value.atom_ascii "signaled", Value.ocaml2cduce_int n)


let system_out =
  record_type_ascii [
    "stdout", string_latin1;
    "stderr", string_latin1;
    "status", variant_type_ascii [
      "exited", int;
      "stopped", int;
      "signaled", int
    ]
  ]
    

let () = register_fun "system" string_latin1 system_out
  (fun v ->
     Location.protect_op "system";
     let cmd = Value.get_string_latin1 v in
     let sout,serr,ps = run_process cmd in
     Value.record_ascii [
       "stdout", Value.string_latin1 sout;
       "stderr", Value.string_latin1 serr;
       "status", process_status ps
     ]
  )
  
let () = register_fun "exit" byte_int Types.empty
  (fun v -> Location.protect_op "exit"; exit (Value.cduce2ocaml_int v))

let exn_not_found =
  Value.CDuceExn (Value.Atom (Atoms.V.mk_ascii "Not_found"))

let () = register_fun "getenv" string_latin1 string_latin1
  (fun e ->
     Location.protect_op "getenv";
     let var = Value.get_string_latin1 e in
     try Value.string_latin1 (Sys.getenv var)
     with Not_found -> raise exn_not_found);;

let () = register_fun "argv" nil (Sequence.star string_latin1)
  (fun e ->
     Location.protect_op "argv";
     !Builtin.argv);;
