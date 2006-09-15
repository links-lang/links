(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

exception Error of string

module Loc = Location
open Caml_cduce
open Caml_cduce.Types

(* Unfolding of OCaml types *)

exception PolyAbstract of string

let ocaml_env = ref Env.initial

type t = { uid : int; mutable recurs : int; mutable def : def }
and def =
  | Link of t
  | Arrow of string * t * t
  | Tuple of t list
  | PVariant of (string * t option) list  (* Polymorphic variant *)
  | Variant of string * (string * t list) list * bool
  | Record of string * (string * t) list * bool
  | Builtin of string * t list
  | Abstract of string
  | Var of int

module IntMap = 
  Map.Make(struct type t = int let compare : t -> t -> int = compare end)
module IntSet = 
  Set.Make(struct type t = int let compare : t -> t -> int = compare end)
module StringSet =   Set.Make(struct type t = string let compare : t -> t -> int = compare end)


let rec print_sep f sep ppf = function
  | [] -> ()
  | [x] -> f ppf x
  | x::tl -> Format.fprintf ppf "%a%s" f x sep; print_sep f sep ppf tl

let printed = ref IntMap.empty

let rec print_slot ppf slot =
  if slot.recurs > 0 then
    (
      if IntMap.mem slot.uid !printed then
	Format.fprintf ppf "X%i" slot.uid
      else (
	printed := IntMap.add slot.uid () !printed;
	Format.fprintf ppf "X%i:=%a" slot.uid print_def slot.def
      )
    )
  else 
    print_def ppf slot.def

and print_def ppf = function
  | Link t -> print_slot ppf t
  | Arrow (l,t,s) -> Format.fprintf ppf "%s:%a -> %a" l print_slot t print_slot s
  | Tuple tl -> Format.fprintf ppf "(%a)" (print_sep print_slot ",") tl
  | PVariant l -> Format.fprintf ppf "[%a]" (print_sep print_palt " | ") l
  | Variant (p,l,_) -> Format.fprintf ppf "[%s:%a]" p (print_sep print_alt " | ") l
  | Record (p,l,_) -> Format.fprintf ppf "{%s:%a}" p (print_sep print_field " ; ") l
  | Builtin (p,tl) -> Format.fprintf ppf "%s(%a)" p (print_sep print_slot ",") tl
  | Abstract s -> Format.fprintf ppf "%s" s
  | Var i -> Format.fprintf ppf "'a%i" i


and print_palt ppf = function
  | lab, None -> Format.fprintf ppf "`%s" lab
  | lab, Some t -> Format.fprintf ppf "`%s of %a" lab print_slot t
   
and print_alt ppf = function
  | (lab,[]) ->
      Format.fprintf ppf "%s" lab
  | (lab,l) ->
      Format.fprintf ppf "%s of [%a]" lab (print_sep print_slot ",") l

and print_field ppf (lab,t) =
  Format.fprintf ppf "%s:%a" lab print_slot t


let print = print_slot

let counter = ref 0
let new_slot () =
  incr counter;
  { uid = !counter; recurs = 0; def = Abstract "DUMMY" }

let reg_uid t =
  let saved = ref [] in
  let rec aux t =
    if t.recurs < 0 then () else begin
      if t.uid > !counter then counter := t.uid;
      saved := (t,t.recurs) :: !saved;
      t.recurs <- (-1);
      match t.def with
	| Link t -> aux t
	| Arrow (_,t1,t2) -> aux t1; aux t2
	| Tuple tl -> List.iter aux tl
	| PVariant pl -> List.iter (function (_,Some t) -> aux t | _ -> ()) pl
	| Variant (_,pl,_) -> List.iter (fun (_,tl) -> List.iter aux tl) pl
	| Record (_,tl,_) ->  List.iter (fun (_,t) -> aux t) tl
	| Builtin (_,tl) -> List.iter aux tl
	| _ -> ()
    end
  in
  aux t;
  List.iter (fun (t,recurs) -> t.recurs <- recurs) !saved

let builtins =
  List.fold_left (fun m x -> StringSet.add x m) StringSet.empty
    [
      "list"; "Pervasives.ref"; 
      "unit"; "array";
      "Big_int.big_int";
      "option";
      "Cduce_lib.Value.t"; 
      "Cduce_lib.Encodings.Utf8.t";
      "Cduce_lib.Atoms.V.t";
    ]

let vars = ref []

let get_var id = 
  try List.assq id !vars
  with Not_found -> 
    let i = List.length !vars in
    vars := (id,i) :: !vars; 
    i

let constr_table = Hashtbl.create 1024

type env = { constrs: StringSet.t; seen: IntSet.t; vars: t IntMap.t }

let rec unfold_constr env p args =
  let args = List.map (unfold env) args in
  let pn = Path.name p in
  if StringSet.mem pn builtins 
  then ( let slot = new_slot () in slot.def <- Builtin (pn,args); slot )
  else
    let args_id = List.map (fun t -> t.uid) args in
    let k = (pn,args_id) in
    try Hashtbl.find constr_table k
    with Not_found ->
      if StringSet.mem pn env.constrs then
	failwith "Polymorphic recursion forbidden";
      let slot = new_slot () in
      slot.recurs <- 1;
      Hashtbl.add constr_table k slot;

      let decl = 
	try Env.find_type p !ocaml_env
	with Not_found -> failwith ("Cannot resolve path " ^ pn) in

      let env = 
	{ env with 
	    constrs = StringSet.add pn env.constrs;
	    vars = 
	    List.fold_left2 
	      (fun vars a t -> IntMap.add a.id t vars) 
	      env.vars decl.type_params args } in

      let prefix = match p with
	| Path.Pident _ -> ""
	| Path.Pdot (p,_,_) -> Path.name p ^ "."
	| _ -> assert false in

      slot.def <- 
	(match decl.type_kind, decl.type_manifest with
	   | Type_variant (cstrs,pub), _ ->
	       let cstrs =
		 List.map 
		   (fun (cst,f) -> (cst,List.map (unfold env) f)) cstrs in
	       Variant (prefix, cstrs, pub = Caml_cduce.Asttypes.Public)
	   | Type_record (f,_,pub), _ ->
	       let f = List.map (fun (l,_,t) -> (l,unfold env t)) f in
	       Record (prefix, f, pub = Caml_cduce.Asttypes.Public)
	   | Type_abstract, Some t ->
	       Link (unfold env t)
	   | Type_abstract, None ->
	       (match args with
		  | [] -> Abstract pn
		  | l ->raise (PolyAbstract pn)));
      slot
	
and unfold env ty =
  if IntSet.mem ty.id env.seen then failwith "Unguarded recursion";
  let env = { env with seen = IntSet.add ty.id env.seen } in 
  let slot = new_slot () in
  slot.def <-
    (match ty.desc with
       | Tarrow (l,t1,t2,_) -> 
	   let t1 = unfold env t1 in
	   let t2 = unfold env t2 in 
	   Arrow (l, t1,t2)
       | Ttuple tyl -> Tuple (List.map (unfold env) tyl)
       | Tvariant rd ->
	   let fields = 
	     List.fold_left
	       (fun accu (lab,f) -> 
		  match f with
		    | Rpresent (Some t) 
		    | Reither(true, [t], _, _) -> 
			(lab, Some (unfold env t)) :: accu
		    | Rpresent None 
		    | Reither(true, [], _, _) -> (lab, None) :: accu
		    | Rabsent -> Printf.eprintf "Warning: Rabsent not supported"; accu
		    | Reither _ -> Printf.eprintf "Warning: Reither not supported"; accu
	       ) []
	       rd.row_fields in
	   PVariant fields
       | Tvar -> 
	   (try Link (IntMap.find ty.id env.vars)
	    with Not_found -> Var (get_var ty.id))
       | Tconstr (p,args,_) ->
	   Link (unfold_constr env p args)
       | _ -> failwith "Unsupported feature"
    );
  slot

let unfold ty = 
  vars := [];
  Hashtbl.clear constr_table; (* Get rid of that (careful with exceptions) *)
  let t = unfold { seen = IntSet.empty; constrs = StringSet.empty;
		   vars = IntMap.empty } ty in
  let n = List.length !vars in
  vars := [];
  (t,n)

(* Reading .cmi *)

let unsupported s =
  raise (Error (Printf.sprintf "Unsupported feature (%s) found in .cmi" s))

let has_cmi name =
  Config.load_path := Config.standard_library :: !Librarian.obj_path;
  try ignore (Misc.find_in_path_uncap !Config.load_path (name ^ ".cmi")); true
  with Not_found -> false

let find_value v =
  Config.load_path := Config.standard_library :: !Librarian.obj_path;
  let li = Longident.parse v in
  ocaml_env := Env.initial;
  let (_,vd) = Env.lookup_value li Env.initial in
  unfold vd.val_type

let values_of_sig name sg =
  List.fold_left
    (fun accu v -> match v with
       | Tsig_value (id,_) -> 
	   let id = Ident.name id in
	   (match id.[0] with
	     | 'a'..'z' | '_' ->
		 let n = name ^ "." ^ id in
		 (try (n, (fst (find_value n))) :: accu
		  with PolyAbstract _ -> accu)
	     | _ -> accu (* operator *))
       | _ -> accu
    ) [] sg


let load_module name = 
  Config.load_path := Config.standard_library :: !Librarian.obj_path;
  let li = Longident.parse name in
  ocaml_env := Env.initial;
  let (_,mty) = Env.lookup_module li Env.initial in
  match mty with
    | Tmty_signature sg -> values_of_sig name sg
    | _ -> raise (Loc.Generic 
		    (Printf.sprintf "Module %s is not a structure" name))

(*
  let filename = Misc.find_in_path_uncap !Config.load_path (name ^ ".cmi") in
  let sg = Env.read_signature name filename in
  values_of_sig sg
*)

let load_module name =
  try load_module name
  with Env.Error e ->
    Env.report_error Format.str_formatter e;
    let s = Format.flush_str_formatter () in
    let s = Printf.sprintf "Error while reading OCaml interface %s: %s"
	      name s in
    raise (Loc.Generic s)

let read_cmi name =
  Config.load_path := Config.standard_library :: !Librarian.obj_path;
  let filename = Misc.find_in_path_uncap !Config.load_path (name ^ ".cmi") in
  let sg = Env.read_signature name filename in
  ocaml_env := Env.add_signature sg Env.initial;
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let values = ref [] in
  List.iter
    (function
       | Tsig_value (id, {val_type=t;val_kind=Val_reg}) -> 
	   let (unf,n) = unfold t in
	   if n !=0 then unsupported "polymorphic value";
	   values := (Ident.name id, t, unf) :: !values
       | Tsig_type (id,t,rs) -> 
	   Format.fprintf ppf "%a@."
	     !Oprint.out_sig_item (Printtyp.tree_of_type_declaration id t rs);
       | Tsig_value _ -> unsupported "external value"
       | Tsig_exception _ -> unsupported "exception"
       | Tsig_module _ -> unsupported "module"
       | Tsig_modtype _ -> unsupported "module type"
       | Tsig_class _ -> unsupported "class"
       | Tsig_cltype _ -> unsupported "class type"
    ) sg;
  (Buffer.contents buf, !values)

let read_cmi name =
  try read_cmi name
  with Env.Error e ->
    Env.report_error Format.str_formatter e;
    let s = Format.flush_str_formatter () in
    let s = Printf.sprintf "Error while reading OCaml interface %s: %s"
	      name s in
    raise (Loc.Generic s)


let print_ocaml = Printtyp.type_expr


let rec dump_li = function
  | Longident.Lident s -> print_endline s
  | Longident.Ldot (li,s) -> dump_li li; print_endline s
  | _ -> assert false

