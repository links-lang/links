(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Builtin_defs

let eval = ref (fun ppf err s -> assert false)

(* Types *)

let stringn = Types.cons string
let namespaces = 
  Sequence.star (Types.times stringn stringn)

let types =
  [ 
    "Empty",   Types.empty;
    "Any",     any;
    "Int",     int;
    "Char",    Types.char Chars.any;
    "Byte",    char_latin1;
    "Atom",    atom;
    "Pair",    Types.Product.any;
    "Arrow",   Types.Arrow.any;
    "Record",  Types.Record.any;
    "String",  string;
    "Latin1",  string_latin1;
    "Bool",    bool;
    "Float",   float;
    "AnyXml",  any_xml;
    "Namespaces", namespaces;
  ]

let env =
  List.fold_left
    (fun accu (n,t) -> 
       let n = (Ns.empty, Ident.U.mk n) in
       Types.Print.register_global "" n t;
       Typer.enter_type (Ident.ident n) t accu
    )
    Typer.empty_env
    types

(* Operators *)

open Operators

let binary_op_gen = register_binary

let unary_op_gen = register_unary


let binary_op name t1 t2 f run =
  binary_op_gen
    name
    (fun arg1 arg2 constr precise ->
       f (arg1 t1 true) (arg2 t2 true))
    run

let binary_op_cst = register_op2


let binary_op_warning2 name t1 t2 w2 t run =
  binary_op_gen name
    (fun arg1 arg2 constr precise ->
       ignore (arg1 t1 false); 
       let r = arg2 t2 true in
       if not (Types.subtype r w2) then
	 raise (Typer.Warning ("This operator may fail", t));
       t)
    run

let unary_op_warning name targ w t run =
  unary_op_gen name
    (fun arg constr precise ->
       let res = arg targ true in
       if not (Types.subtype res w) then
	 raise (Typer.Warning ("This operator may fail",t));
       t)
    run

open Ident

let exn_load_file_utf8 = lazy (
  Value.CDuceExn (
    Value.Pair (
      Value.Atom (Atoms.V.mk_ascii "load_file_utf8"),
      Value.string_latin1 "File is not a valid UTF-8 stream"))
)

let exn_int_of =  lazy (
  Value.CDuceExn (
    Value.Pair (
      Value.Atom (Atoms.V.mk_ascii "Invalid_argument"),
      Value.string_latin1 "int_of"))
)

let exn_char_of = lazy (
  Value.CDuceExn (
    Value.Pair (
      Value.Atom (Atoms.V.mk_ascii "Invalid_argument"),
      Value.string_latin1 "char_of"))
)

let exn_float_of = lazy (
  Value.CDuceExn (
    Value.Pair (
      Value.Atom (Atoms.V.mk_ascii "Invalid_argument"),
      Value.string_latin1 "float_of"))
)

let exn_namespaces = lazy (
  Value.CDuceExn (
    Value.Pair (
      Value.Atom (Atoms.V.mk_ascii "Invalid_argument"),
      Value.string_latin1 "namespaces"))
)


let eval_load_file ~utf8 e =
  Location.protect_op "load_file";
  let fn = Value.get_string_latin1 e in
  let s = Url.load_url fn in
  if utf8 then 
    match U.mk_check s with 
      | Some s -> Value.string_utf8 s 
      | None -> raise (Lazy.force exn_load_file_utf8)
  else Value.string_latin1 s


let () = ();;

(* Comparison operators *)

binary_op "=" 
  any any 
  (fun t1 t2 ->
     if Types.is_empty (Types.cap t1 t2) then false_type
     else bool)
  (fun v1 v2 ->
     Value.vbool (Value.compare v1 v2 == 0));;
  
binary_op_cst "<=" 
  any any bool
  (fun v1 v2 -> Value.vbool (Value.compare v1 v2 <= 0));;
  
binary_op_cst "<" 
  any any bool
  (fun v1 v2 -> Value.vbool (Value.compare v1 v2 < 0));;
  
binary_op_cst ">=" 
  any any bool
  (fun v1 v2 -> 
     Value.vbool (Value.compare v1 v2 >= 0));;
  
binary_op_cst ">" 
  any any bool
  (fun v1 v2 -> 
     Value.vbool (Value.compare v1 v2 > 0));;

(* I/O *)

register_fun "char_of_int"
  int (Types.char Chars.any)
  (function
     | Value.Integer x ->
	 (try Value.Char (Chars.V.mk_int (Intervals.V.get_int x))
	 with Failure _ -> raise (Lazy.force exn_int_of))
     | _ -> assert false);;

  
register_fun "string_of" 
  any string_latin1
  (fun v -> 
     let b = Buffer.create 16 in
     let ppf = Format.formatter_of_buffer b in
     Value.print ppf v;
     Format.pp_print_flush ppf ();
     Value.string_latin1 (Buffer.contents b)
  );;

register_fun "load_xml"
  string_latin1 any_xml
  (fun v -> Location.protect_op "load_xml"; Load_xml.load_xml (Value.get_string_latin1 v));;
  
register_fun "!load_xml"
  string_latin1 any_xml
  (fun v -> Location.protect_op "load_xml"; Load_xml.load_xml ~ns:true
     (Value.get_string_latin1 v));;


register_fun "load_html"
  string_latin1 Sequence.any
  (fun v -> Location.protect_op "load_html"; Load_xml.load_html  (Value.get_string_latin1 v));;

register_fun "load_file_utf8" 
  string_latin1 string 
  (eval_load_file ~utf8:true);;

register_fun "load_file" 
  string_latin1 string_latin1 
  (eval_load_file ~utf8:false);;


let argv = ref Value.Absent;;



register_fun "print_xml" 
  Types.any string_latin1
  (fun v -> Print_xml.print_xml ~utf8:false !Eval.ns_table v);;

register_fun "print_xml_utf8" 
  Types.any string
  (fun v -> Print_xml.print_xml ~utf8:true !Eval.ns_table v);;


register_fun "print"
  string_latin1 nil
  (fun v ->
     Location.protect_op "print";
     print_string (Value.get_string_latin1 v);
     flush stdout;
     Value.nil
  );;

register_fun "print_utf8"
  string nil
  (fun v ->
     Location.protect_op "print";
     let s = Value.cduce2ocaml_string_utf8 v in
     print_string (U.get_str s);
     flush stdout;
     Value.nil
  );;

unary_op_warning "int_of"
  string intstr int
  (fun v ->
     let (s,_) = Value.get_string_utf8 v in
      let str = U.get_str s in
        try let modifier = str.[(String.index str '0')+1] in
	    if ( modifier = 'x' ||
                 modifier = 'X' ||
                 modifier = 'b' ||
                 modifier = 'B' ||
                 modifier = 'o' ||
                 modifier = 'O') 
             then  
               Value.Integer (Intervals.V.from_int (int_of_string(str)))
             else
	       Value.Integer (Intervals.V.mk (str))
         with _ -> 
                (try Value.Integer (Intervals.V.mk (str))
                 with Failure _ -> raise (Lazy.force exn_int_of)));;

(*  It was like that                                    *)
(*     try Value.Integer (Intervals.V.mk (U.get_str s)) *)
(*                 UTF-8 is ASCII compatible !          *)
(* modified to allow 0x 0b 0o notations                 *)


(*
register_fun "atom_of"
  string atom
  (fun v ->
     let (s,_) = Value.get_string_utf8 v in 
     Value.Atom (Atoms.V.mk Ns.empty s));;
*)

register_fun "split_atom"
  atom (Types.times stringn stringn)
  (function 
     | Value.Atom q ->
	 let (ns,l) = Atoms.V.value q in
	 Value.Pair(
	   Value.string_utf8 (Ns.Uri.value ns),
	   Value.string_utf8 l)
     | _ -> assert false);;

register_fun "make_atom"
  (Types.times stringn stringn) atom
  (fun v ->
     let v1,v2 =Value.get_pair v in
     let ns,_ = Value.get_string_utf8 v1 in
     let l,_ = Value.get_string_utf8 v2 in
     (* TODO: check that l is a correct Name wrt XML *)
     Value.Atom (Atoms.V.mk (Ns.Uri.mk ns, l)));;


   

binary_op_warning2 "dump_to_file"
  string_latin1 string string_latin1 nil
  (fun f v ->
     Location.protect_op "dump_to_file";
     let oc = open_out (Value.get_string_latin1 f) in
     output_string oc (Value.get_string_latin1 v);
     close_out oc;
     Value.nil);;
    
binary_op_cst "dump_to_file_utf8"
  string_latin1 string nil
  (fun f v ->
     Location.protect_op "dump_to_file_utf8";
     let oc = open_out (Value.get_string_latin1 f) in
     let (v,_) = Value.get_string_utf8 v in
     output_string oc (U.get_str v);
     close_out oc;
     Value.nil);;

(* Integer operators *)

binary_op_gen "+"
  (fun arg1 arg2 constr precise ->
     let t1 = arg1 (Types.cup int Types.Record.any) true in
     if Types.subtype t1 int 
     then (
       let t2 = arg2 int true in
       Types.interval
	 (Intervals.add (Types.Int.get t1) (Types.Int.get t2))
     )
     else if Types.subtype t1 Types.Record.any 
     then (
       let t2 = arg2 Types.Record.any true in 
       Types.Record.merge t1 t2
     )
     else raise (Typer.Error "The first argument mixes integers and records"))
  Value.add;;
      
binary_op "-"
  int int
  (fun t1 t2 ->
     Types.interval 
     (Intervals.sub (Types.Int.get t1) (Types.Int.get t2)))
  (fun v1 v2 -> match (v1,v2) with
     | (Value.Integer x, Value.Integer y) -> Value.Integer (Intervals.V.sub x y)
     | _ -> assert false);;

binary_op "*"
  int int
  (fun t1 t2 ->
     Types.interval 
     (Intervals.mul (Types.Int.get t1) (Types.Int.get t2)))
  (fun v1 v2 -> match (v1,v2) with
     | (Value.Integer x, Value.Integer y) -> Value.Integer (Intervals.V.mult x y)
     | _ -> assert false);;

binary_op_warning2 "/"
  int int non_zero_int int
  (fun v1 v2 -> match (v1,v2) with
     | (Value.Integer x, Value.Integer y) -> Value.Integer (Intervals.V.div x y)
     | _ -> assert false);;

binary_op_warning2 "mod"
  int int non_zero_int int
  (fun v1 v2 -> match (v1,v2) with
     | (Value.Integer x, Value.Integer y) -> Value.Integer (Intervals.V.modulo x y)
     | _ -> assert false);;


binary_op_gen "@"
  (fun arg1 arg2 constr precise ->
     let constr' = Sequence.star 
		     (Sequence.approx (Types.cap Sequence.any constr)) in
     let exact = Types.subtype constr' constr in
     if exact then
       let t1 = arg1 constr' precise
       and t2 = arg2 constr' precise in
       if precise then Sequence.concat t1 t2 else constr
     else
       (* Note:
	  the knownledge of t1 may makes it useless to
	  check t2 with 'precise' ... *)
       let t1 = arg1 constr' true
       and t2 = arg2 constr' true in
       Sequence.concat t1 t2)
  Value.concat;;

unary_op_gen "flatten"
  Typer.flatten
  Value.flatten;;
  

register_fun "raise" any Types.empty
  (fun v -> raise (Value.CDuceExn v));;

register_fun "namespaces" any_xml 
  namespaces
  (function 
       Value.XmlNs (_,_,_,ns) ->
	 Value.sequence_rev 
	   (List.map 
	      (fun (pr,ns) ->
		 Value.Pair (Value.string_utf8 pr,
			     Value.string_utf8 (Ns.Uri.value ns)))
	      (Ns.get_table ns))
     | Value.Xml _ -> raise (Lazy.force exn_namespaces)
     | _ -> assert false);;

register_fun2 "set_namespaces"
  namespaces any_xml any_xml
  (fun ns -> function
     | Value.XmlNs(v1,v2,v3,_) | Value.Xml (v1,v2,v3) ->
	 let ns = Value.get_sequence_rev ns in
	 let ns = List.map (fun v ->
			      let (pr,ns) = Value.get_pair v in
			      let pr,_ = Value.get_string_utf8 pr in
			      let ns,_ = Value.get_string_utf8 ns in
			      (pr,Ns.Uri.mk ns)) ns in
	 Value.XmlNs(v1,v2,v3,Ns.mk_table  ns)
     | _ -> assert false);;

(* Float *)

register_fun "float_of" string float
  (fun v ->
     let (s,_) = Value.get_string_utf8 v in
     try Value.float (float_of_string (U.get_str s))
     with Failure _ -> raise (Lazy.force exn_float_of));;
