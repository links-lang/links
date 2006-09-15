(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Location
open Ident

let () = Stats.gettimeofday := Unix.gettimeofday

exception Escape of exn
exception InvalidInputFilename of string
exception InvalidObjectFilename of string

let extra_specs = ref []

(* if set to false toplevel exception aren't cought. 
 * Useful for debugging with OCAMLRUNPARAM="b" *)
let catch_exceptions = true

(* retuns a filename without the suffix suff if any *) 
let prefix filename suff =
  if Filename.check_suffix filename suff then
    try
      Filename.chop_extension filename
    with Invalid_argument filename -> failwith "Not a point in the suffix?"
  else filename

let toplevel = ref false
let verbose = ref false
let silent = ref false

let typing_env = ref Builtin.env
let compile_env = ref Compile.empty_toplevel

let get_global_value cenv v =
  Eval.eval_var (Compile.find v !compile_env)

let get_global_type v =
  Typer.find_value v !typing_env

let rec is_abstraction = function
  | Ast.Abstraction _ -> true
  | Ast.LocatedExpr (_,e) -> is_abstraction e
  | _ -> false

let print_norm ppf d = 
  Types.Print.print ppf ((*Types.normalize*) d)

let print_sample ppf s =
  Sample.print ppf s

let print_protect ppf s =
  Format.fprintf ppf "%s" s

let print_value ppf v =
  Value.print ppf v

let dump_value ppf x t v =
  Format.fprintf ppf "@[val %a : @[%a = %a@]@]@."
    Ident.print x print_norm t print_value v

let dump_env ppf tenv cenv =
  Format.fprintf ppf "Types:%a@." Typer.dump_types tenv;
  Format.fprintf ppf "Namespace prefixes:@\n%a" Typer.dump_ns tenv;
  Format.fprintf ppf "Namespace prefixes used for pretty-printing:@.%t"
    Ns.InternalPrinter.dump;
  Format.fprintf ppf "Values:@.";
  Typer.iter_values tenv
    (fun x t -> dump_value ppf x t (get_global_value cenv x))

let directive_help ppf =
  Format.fprintf ppf
"Toplevel directives:
  #quit;;                 quit the interpreter
  #env;;                  dump current environment
  #reinit_ns;;            reinitialize namespace processing
  #help;;                 shows this help message
  #print_type <type>;;
  #silent;;               turn off outputs from the toplevel
  #verbose;;              turn on outputs from the toplevel
  #builtins;;             shows embedded OCaml values
"

let rec print_exn ppf = function
  | Location (loc, w, exn) ->
      Location.print_loc ppf (loc,w);
      Location.html_hilight (loc,w); 
      print_exn ppf exn
  | Value.CDuceExn v ->
      Format.fprintf ppf "Uncaught CDuce exception: @[%a@]@."
        print_value v
  | Typer.WrongLabel (t,l) ->
      Format.fprintf ppf "Wrong record selection; field %a " 
        Label.print_attr l;
      Format.fprintf ppf "not present in an expression of type:@.%a@."
        print_norm t
  | Typer.ShouldHave (t,msg) ->
      Format.fprintf ppf "This expression should have type:@.%a@.%a@." 
        print_norm t
        print_protect msg
  | Typer.ShouldHave2 (t1,msg,t2) ->
      Format.fprintf ppf "This expression should have type:@.%a@.%a %a@." 
        print_norm t1
        print_protect msg
        print_norm t2
  | Typer.Error s ->
      Format.fprintf ppf "%a@." print_protect s
  | Typer.Constraint (s,t) ->
      Format.fprintf ppf "This expression should have type:@.%a@." 
        print_norm t;
      Format.fprintf ppf "but its inferred type is:@.%a@." 
        print_norm s;
      Format.fprintf ppf "which is not a subtype, as shown by the sample:@.%a@." 
	print_sample (Sample.get (Types.diff s t))
  | Typer.NonExhaustive t ->
      Format.fprintf ppf "This pattern matching is not exhaustive@.";
      Format.fprintf ppf "Residual type:@.%a@."
	print_norm t;
      Format.fprintf ppf "Sample:@.%a@." print_sample (Sample.get t)
  | Typer.UnboundId (x,tn) ->
      Format.fprintf ppf "Unbound identifier %a%s@." Ident.print x
        (if tn then " (it is a type name)" else "")
  | Typer.UnboundExtId (cu,x) ->
      Format.fprintf ppf "Unbound external identifier %a:%a@." 
	U.print (Librarian.name cu)
	Ident.print x
  | Ulexer.Error (i,j,s) ->
      let loc = Location.loc_of_pos (i,j), `Full in
      Location.print_loc ppf loc;
      Location.html_hilight loc;
      Format.fprintf ppf "%s" s
  | Parser.Error s | Stream.Error s -> 
      Format.fprintf ppf "Parsing error: %a@." print_protect s
  | Librarian.InconsistentCrc name ->
      Format.fprintf ppf "Link error:@.";
      Format.fprintf ppf "Inconsistent checksum (compilation unit: %a)@."
	U.print name
  | Librarian.NoImplementation name ->
      Format.fprintf ppf "Link error:@.";
      Format.fprintf ppf "No implementation found for compilation unit: %a@."
	U.print name
  | InvalidInputFilename f ->
      Format.fprintf ppf "Compilation error:@.";
      Format.fprintf ppf "Source filename must have extension .cd@.";
  | InvalidObjectFilename f ->
      Format.fprintf ppf "Compilation error:@.";
      Format.fprintf ppf "Object filename must have extension .cdo and no path@.";
  | Librarian.InvalidObject f ->
      Format.fprintf ppf "Invalid object file %s@." f
  | Librarian.CannotOpen f ->
      Format.fprintf ppf "Cannot open file %s@." f
  | Location.Generic s ->
      Format.fprintf ppf "%a@." print_protect s
  | Ns.Label.Not_unique ((ns1,s1),(ns2,s2)) ->
      Format.fprintf ppf "Collision on label hash: {%a}:%a, {%a}:%a" 
	U.print (Ns.Uri.value ns1) 
	U.print s1 
	U.print (Ns.Uri.value ns2) 
	U.print s2
  | Ns.Uri.Not_unique (ns1,ns2) ->
      Format.fprintf ppf "Collision on namespaces hash: %a, %a" 
	U.print ns1 
	U.print ns2
  | Sequence.Error (Sequence.CopyTag (t,expect)) ->
      Format.fprintf ppf "Tags in %a will be copied, but only %a are allowed.@.Counter-example:%a@."
	Types.Print.print t
	Types.Print.print expect
	Sample.print (Sample.get (Types.diff t expect))
  | Sequence.Error (Sequence.CopyAttr (t,expect)) ->
      Format.fprintf ppf "Attributes in %a will be copied, but only %a are allowed.@.Counter-example:%a@."
	Types.Print.print t
	Types.Print.print expect
	Sample.print (Sample.get (Types.diff t expect))
  | Sequence.Error (Sequence.UnderTag (t,exn)) ->
      Format.fprintf ppf "Under tag %a:@." Types.Print.print t;
      print_exn ppf exn
      
  | exn ->
(*      raise exn *)
      Format.fprintf ppf "%a@." print_protect (Printexc.to_string exn)


let eval_quiet tenv cenv e = 
  let (e,_) = Typer.type_expr tenv e in
  Compile.compile_eval_expr cenv e

let debug ppf tenv cenv = function
  | `Subtype (t1,t2) ->
      Format.fprintf ppf "[DEBUG:subtype]@.";
      let t1 = Types.descr (Typer.typ tenv t1)
      and t2 = Types.descr (Typer.typ tenv t2) in
      let s = Types.subtype t1 t2 in
      Format.fprintf ppf "%a %a %a : %b@." print_norm t1 print_protect "<=" print_norm t2 s
  | `Sample t ->
      Format.fprintf ppf "[DEBUG:sample]@.";
      (try
	 let t = Types.descr (Typer.typ tenv t) in
	 Format.fprintf ppf "%a@." print_sample (Sample.get t)
       with Not_found ->
	 Format.fprintf ppf "Empty type : no sample !@.")
  | `Filter (t,p) -> 
      let t = Typer.typ tenv t
      and p = Typer.pat tenv p in
      Format.fprintf ppf "[DEBUG:filter t=%a p=%a]@."
	Types.Print.print (Types.descr t)
	Patterns.Print.print (Patterns.descr p);
      let f = Patterns.filter (Types.descr t) p in
      IdMap.iteri (fun x t ->
		   Format.fprintf ppf " %a:%a@." 
		     Ident.print x
		     print_norm (Types.descr t)) f
  | `Accept p ->
      Format.fprintf ppf "[DEBUG:accept]@.";
      let p = Typer.pat tenv p in
      let t = Patterns.accept p in
      Format.fprintf ppf " %a@." Types.Print.print (Types.descr t)
  | `Compile (t,pl) ->
      Format.fprintf ppf "[DEBUG:compile]@.";
      let no = ref (-1) in
      let t = Types.descr (Typer.typ tenv t) 
      and pl = List.map (fun p -> incr no; (Typer.pat tenv p, !no)) pl in

      let (state,rhs) = Patterns.Compile.make_branches t pl in
      Array.iteri (fun i r ->
		     Format.fprintf ppf "Return code %i:" i;
		     match r with
		     | Auto_pat.Fail -> Format.fprintf ppf "Fail@."
		     | Auto_pat.Match (_,n) -> Format.fprintf ppf "Pat(%i)@." n
		  ) rhs;
      Format.fprintf ppf "@.Dispatcher:@.%a@." Print_auto.print_state state
  | `Single t ->
      Format.fprintf ppf "[DEBUG:single]@.";
      let t = Typer.typ tenv t in
      (try 
	 let c = Sample.single (Types.descr t) in
	 Format.fprintf ppf "Constant:%a@." Types.Print.print_const c
       with
	 | Exit -> Format.fprintf ppf "Non constant@." 
	 | Not_found ->  Format.fprintf ppf "Empty@.")


let flush_ppf ppf = Format.fprintf ppf "@."

let directive ppf tenv cenv = function
  | `Debug d ->
      debug ppf tenv cenv d
  | `Quit ->
      (if !toplevel then raise End_of_file)
  | `Env ->
      dump_env ppf tenv cenv
  | `Print_type t ->
      let t = Typer.typ tenv t in
      Format.fprintf ppf "%a@." Types.Print.print_noname (Types.descr t)
  | `Reinit_ns ->
      Typer.set_ns_table_for_printer tenv
  | `Help ->
      directive_help ppf
  | `Dump pexpr ->
      Value.dump_xml ppf (eval_quiet tenv cenv pexpr);
      flush_ppf ppf
  | `Silent ->
      silent := true
  | `Verbose ->
      silent := false
  | `Builtins ->
      let b = Librarian.get_builtins () in
      Format.fprintf ppf "Embedded OCaml value: ";
      List.iter (fun s -> Format.fprintf ppf "%s " s) b;
      Format.fprintf ppf "@."

let print_id_opt ppf = function
  | None -> Format.fprintf ppf "-"
  | Some id -> Format.fprintf ppf "val %a" Ident.print id

let print_value_opt ppf = function
  | None -> ()
  | Some v -> Format.fprintf ppf " = %a" print_value v

let show ppf id t v =
  if !silent then ()
  else Format.fprintf ppf "@[%a : @[%a%a@]@]@."
    print_id_opt id 
    print_norm t 
    print_value_opt v

let phrases ppf phs =
  let (tenv,cenv,_) = 
    Compile.comp_unit 
      ~run:true ~show:(show ppf) 
      ~directive:(directive ppf)
      !typing_env !compile_env phs in
  typing_env := tenv;
  compile_env := cenv

let catch_exn ppf_err exn =
  if not catch_exceptions then raise exn;
  match exn with
  | (End_of_file | Failure _ | Not_found | Invalid_argument _ | Sys.Break) 
      as e -> 
      raise e
  | exn -> 
      print_exn ppf_err exn;
      Format.fprintf ppf_err "@."

let parse rule input =
  try Parser.localize_exn (fun () -> rule input)
  with e -> Parser.sync (); raise e

let run rule ppf ppf_err input =
  try phrases ppf (parse rule input); true
  with Escape exn -> raise exn | exn -> catch_exn ppf_err exn; false

let topinput = run Parser.top_phrases
let script = run Parser.prog


let compile src out_dir =
  try
    if not (Filename.check_suffix src ".cd")
    then raise (InvalidInputFilename src);
    let cu = Filename.chop_suffix (Filename.basename src) ".cd" in
    let out_dir = 
      match out_dir with
	| None -> Filename.dirname src
	| Some x -> x in
    let out = Filename.concat out_dir (cu ^ ".cdo") in
    let name = U.mk_latin1 cu in
    Librarian.compile_save !verbose name src out;
    exit 0
  with exn -> catch_exn Format.err_formatter exn; exit 1
  
let compile_run src =
  try
    let name = 
      if src = "" then "<stdin>"
      else
	if not (Filename.check_suffix src ".cd")
	then raise (InvalidInputFilename src)
	else Filename.chop_suffix (Filename.basename src) ".cd" in
    let name = U.mk_latin1 name in
    Librarian.compile_run !verbose name src;
  with exn -> catch_exn Format.err_formatter exn; exit 1

let run obj =  
  try
    if not (Filename.check_suffix obj ".cdo") || (Filename.basename obj <> obj)
    then raise (InvalidObjectFilename obj);
    let name = Filename.chop_suffix (Filename.basename obj) ".cdo" in
    let name = U.mk_latin1 name in
    Librarian.load_run name
  with exn -> catch_exn Format.err_formatter exn; exit 1


let dump_env ppf = dump_env ppf !typing_env !compile_env

let eval s =
  let st = Stream.of_string s in
  let phs = parse Parser.prog st in
  let vals = ref [] in
  let show id t v =
    match id,v with
      | Some id, Some v -> 
	  vals := (Some (Atoms.V.mk id),v) :: !vals
      | None, Some v ->
	  vals := (None,v) :: !vals
      | _ -> assert false
  in
  ignore (Compile.comp_unit 
	    ~run:true ~show Builtin.env Compile.empty_toplevel phs);
  List.rev !vals
  
let eval s =
  try eval s
  with exn -> 
    let b = Buffer.create 1024 in
    let ppf = Format.formatter_of_buffer b in
    print_exn ppf exn;
    Format.fprintf ppf "@.";
    Value.failwith' (Buffer.contents b)
	   

let () =        
  Operators.register_fun "eval_expr" Builtin_defs.string_latin1 Types.any
  (fun v ->
     match eval (Value.cduce2ocaml_string v) with
       | [ (None,v) ] -> v
       | _ -> Value.failwith' "eval: the string must evaluate to a single value"
  )

