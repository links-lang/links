(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Location
open Ident


exception InconsistentCrc of U.t
exception InvalidObject of string
exception CannotOpen of string
exception NoImplementation of U.t

let run_loaded = ref false

type t = {
  name: U.t;
  descr: Compunit.t;

  typing: Typer.t;
  compile: Compile.env;
  code: Lambda.code_item list;
  ext_info: Externals.ext_info option;

  mutable digest: Digest.t option;
  vals: Value.t array; (* Exported values *)
  mutable exts: Value.t array;
  mutable depends: (U.t * string) list;


  mutable status: [ `Evaluating | `Unevaluated | `Evaluated ];
}

let digest c = match c.digest with None -> assert false | Some x -> x


module Tbl = Hashtbl.Make(U)
let tbl = Tbl.create 64

module CTbl = Hashtbl.Make(Compunit)
let ctbl = CTbl.create 64

let mk name descr typing compile code ext_info depends =
  { name = name;
    descr = descr;
    typing = typing;
    compile = compile;
    code = code;
    ext_info = ext_info;
    digest = None;
    vals = Array.make (Compile.global_size compile) Value.Absent;
    exts = [| |];
    depends = depends;
    status = `Unevaluated;
  }

let magic = "CDUCE:compunit:00007"

let obj_path = ref [ "" ]


let has_obj n =
  let base = U.to_string n ^ ".cdo" in
  List.exists (fun p -> Sys.file_exists (Filename.concat p base)) !obj_path

let find_obj n = 
  let base = U.to_string n ^ ".cdo" in
  let p = 
    List.find (fun p -> Sys.file_exists (Filename.concat p base)) !obj_path in
  Filename.concat p base
  
let check_digest c dig = 
  if digest c <> dig then raise (InconsistentCrc c.name)

let show ppf id t v =
  match id with
    | Some id ->
	Format.fprintf ppf "@[val %a : @[%a@]@."
	Ident.print id
	Types.Print.print t 
    | None -> ()


let compile verbose name src =
  protect_op "Compile external file";
  let ic = 
    if src = "" then (Location.push_source `Stream; stdin)
    else
      try Location.push_source (`File src); open_in src
      with Sys_error _ -> raise (CannotOpen src) in
  let input = Stream.of_channel ic in
  let p = 
    try Parser.prog input 
    with
      | Stdpp.Exc_located (_, (Location _ | Ulexer.Error _ as e)) -> raise e
      | Stdpp.Exc_located ((i,j), e) -> 
	  raise_loc i.Lexing.pos_cnum j.Lexing.pos_cnum e
  in
  if src <> "" then close_in ic;

  let show =
    if verbose 
    then Some (show Format.std_formatter)
    else None in
  Compunit.enter ();
  let descr = Compunit.current () in
  let (ty_env,c_env,code) =
    Compile.comp_unit 
      ?show
      Builtin.env
      (Compile.empty descr)
      p in
  Compunit.leave ();
  let ext = Externals.get () in
  let depends = Tbl.fold (fun name c accu -> (name,digest c) :: accu) tbl [] in

  mk name descr ty_env c_env code ext depends

let set_hash c =
  let h = Hashtbl.hash_param 1000 10000 (c.typing,c.name) in
  let max_rank = 
    Tbl.fold 
      (fun _ c accu -> max accu (fst (Compunit.get_hash c.descr))) tbl 0 in
  Compunit.set_hash c.descr (succ max_rank) h
  (* This invalidates all hash tables on types ! *)


let compile_save verbose name src out =
  protect_op "Save compilation unit";

  let c = compile verbose name src in
  set_hash c;
  let pools = Value.extract_all () in

  let oc = open_out out in
  output_string oc magic;
  
  Marshal.to_channel oc (pools,c) [];
  let digest = Digest.file out in
  Marshal.to_channel oc digest [];
  close_out oc

let from_descr descr : t =
  try CTbl.find ctbl descr
  with Not_found -> 
    let i1,i2 = Compunit.get_hash descr in
    failwith (Printf.sprintf "Can't find cu(%i,%i)" i1 i2)

let register c =
  (* Look for an already loaded unit with the same descriptor *)
  if CTbl.mem ctbl c.descr then failwith "Collision on unit descriptors";
  CTbl.add ctbl c.descr c
 
let reg_types = ref true


let rec real_load src =
  let ic = 
    try open_in src
    with Sys_error _ -> raise (CannotOpen src) in
  try
    let s = String.copy magic in
    really_input ic s 0 (String.length s);
    if s <> magic then raise (InvalidObject src);
    let pools,c = Marshal.from_channel ic in
    let digest = Marshal.from_channel ic in
    c.digest <- Some digest;
    Value.intract_all pools;
    close_in ic;
    c
  with Failure _ | End_of_file -> raise (InvalidObject src)

and load name =
  protect_op "Load compiled compilation unit";
  try Tbl.find tbl name
  with Not_found -> 
    let src = 
      try find_obj name
      with Not_found -> raise (NoImplementation name) in
    let c = real_load src in
    register c;
    (* Register types *)
    if !reg_types then 
      Typer.register_types (U.to_string c.name ^ ".") c.typing;
    (* Load dependencies *)
    List.iter (fun (name,dig) -> check_digest (load name) dig) c.depends;
    Tbl.add tbl name c;
    c

let rec run c =
  match c.status with
    | `Unevaluated ->
	if (c.ext_info != None) && (Array.length c.exts = 0) then
	  failwith (Printf.sprintf 
	    "The CDuce unit `%s' needs externals"
	     (U.to_string c.name));

	(* Run dependencies *)
	List.iter (fun (name,_) -> run (load name)) c.depends;

	c.status <- `Evaluating;
	Eval.eval_unit c.vals c.code;
	c.status <- `Evaluated
    | `Evaluating -> 
	failwith 
	  ("Librarian.run. Already running:" ^ (U.to_string c.name))
    | `Evaluated -> ()

let compile_run verbose name src = 
  let c = compile verbose name src in
  register c;
  run c

let load_run name = reg_types := false; run (load name)

let static_externals = Hashtbl.create 17
let register_static_external n v = 
  Hashtbl.add static_externals n v

let get_builtins () =
  List.sort Pervasives.compare 
    (Hashtbl.fold (fun n _ accu  -> n::accu) static_externals [])

let () =
  Typer.from_comp_unit := (fun d -> (from_descr d).typing);
  Typer.load_comp_unit := (fun name -> 
			     if has_obj name then 
			       let cu = load name in
			       if !run_loaded then run cu;
			       cu.descr
			     else raise Not_found);
  Typer.has_static_external :=  Hashtbl.mem static_externals;
  Compile.from_comp_unit := (fun d -> (from_descr d).compile);
  Eval.get_globals := (fun d -> (from_descr d).vals);
  Eval.get_external := (fun d i -> (from_descr d).exts.(i));
  Eval.get_builtin := Hashtbl.find static_externals


let stub_ml = ref (fun _ _ _ _ _ -> assert false)

let prepare_stub src =
  let c = real_load src in
  
  (* Create stub types in a fresh compilation unit *)
  Compunit.enter ();
  let i1,i2 = Compunit.get_hash c.descr in
  Compunit.set_hash (Compunit.current ()) (-i1) i2;
  !stub_ml (U.get_str c.name) c.typing c.compile c.ext_info
    (fun types ->
       Compunit.leave ();
       Marshal.to_string (Value.extract_all (), types, c) [])
(* TODO: could remove typing and compile env *)

let ocaml_stub stub =
  let pools, types, (c : t) = Marshal.from_string stub 0 in
  if Tbl.mem tbl c.name then
    failwith ("CDuce unit " ^ (U.get_str c.name) ^ " already loaded");
  Value.intract_all pools;
  register c;
  List.iter 
    (fun (name,dig) -> 
       let c = 
	 try Tbl.find tbl name
	 with Not_found ->
	   failwith ("CDuce unit " ^ (U.get_str name) ^ " not loaded")
       in
       check_digest c dig) c.depends;
  Tbl.add tbl c.name c;
  types,
  (fun a -> c.exts <- a),
  c.vals,
  (fun () -> run c)

let name d = (from_descr d).name
let run d = run (from_descr d)

let make_wrapper = ref (fun _ ->
			  failwith "OCaml/CDuce interface not available")
