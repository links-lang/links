(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Location
open Ast
open Ident

let (=) (x:int) y = x = y
let (<=) (x:int) y = x <= y
let (<) (x:int) y = x < y
let (>=) (x:int) y = x >= y
let (>) (x:int) y = x > y

let warning loc msg =
  let v = Location.get_viewport () in
  let ppf = if Html.is_html v then Html.ppf v else Format.err_formatter in
  Location.print_loc ppf (loc,`Full);
  Location.html_hilight (loc,`Full);
  Format.fprintf ppf "Warning: %s@." msg

exception NonExhaustive of Types.descr
exception Constraint of Types.descr * Types.descr
exception ShouldHave of Types.descr * string
exception ShouldHave2 of Types.descr * string * Types.descr
exception WrongLabel of Types.descr * label
exception UnboundId of id * bool
exception UnboundExtId of Compunit.t * id
exception Error of string
exception Warning of string * Types.t

let raise_loc loc exn = raise (Location (loc,`Full,exn))
let raise_loc_str loc ofs exn = raise (Location (loc,`Char ofs,exn))
let error loc msg = raise_loc loc (Error msg)

type schema = {
  sch_uri: string;
  sch_ns: Ns.Uri.t;
  sch_comps: (Types.t * Schema_validator.t) Ident.Env.t;
}

type item =
(* These are really exported by CDuce units: *)
  | Type of Types.t
  | Val of Types.t
  | ECDuce of Compunit.t
  | ESchema of schema
  | ENamespace of Ns.Uri.t
(* These are only used internally: *)
  | EVal of Compunit.t * id * Types.t
  | EOCaml of string
  | EOCamlComponent of string
  | ESchemaComponent of (Types.t * Schema_validator.t)

type t = {
  ids : item Env.t;
  ns: Ns.table;
  keep_ns: bool
}

(* Namespaces *)

let set_ns_table_for_printer env = 
  Ns.InternalPrinter.set_table env.ns

let get_ns_table tenv = tenv.ns


let type_keep_ns env k =
  { env with keep_ns = k }

let protect_error_ns loc f x =
  try f x
  with Ns.UnknownPrefix ns ->
    raise_loc_generic loc 
    ("Undefined namespace prefix " ^ (U.to_string ns))

let qname env loc t = 
  protect_error_ns loc (Ns.map_tag env.ns) t
    
let ident env loc t =
  protect_error_ns loc (Ns.map_attr env.ns) t

let parse_atom env loc t = Atoms.V.mk (qname env loc t)
 
let parse_ns env loc ns =
  protect_error_ns loc (Ns.map_prefix env.ns) ns

let parse_label env loc t =
  Label.mk (protect_error_ns loc (Ns.map_attr env.ns) t)

let parse_record env loc f r =
  let r = List.map (fun (l,x) -> (parse_label env loc l, f x)) r in
  LabelMap.from_list (fun _ _ -> raise_loc_generic loc "Duplicated record field") r



let load_schema = ref (fun _ _ -> assert false)
let from_comp_unit = ref (fun _ -> assert false)
let load_comp_unit = ref (fun _ -> assert false)
let has_ocaml_unit = ref (fun _ -> false)
let has_static_external = ref (fun _ -> assert false)

let type_schema env loc name uri =
  let x = ident env loc name in
  let (ns,sch) = !load_schema (U.to_string name) uri in
  let sch = { sch_uri = uri; sch_comps = sch; sch_ns = ns } in
  { env with ids = Env.add x (ESchema sch) env.ids }

let empty_env = {
  ids = Env.empty;
  ns = Ns.def_table;
  keep_ns = false
}

let enter_id x i env =
  { env with ids = Env.add x i env.ids }


let type_using env loc x cu = 
  try 
    let cu = !load_comp_unit cu in
    enter_id (ident env loc x) (ECDuce cu) env
  with Not_found ->
    error loc ("Cannot find external unit " ^ (U.to_string cu))

let enter_type id t env = enter_id id (Type t) env
let enter_types l env =
  { env with ids = 
      List.fold_left (fun accu (id,t) -> Env.add id (Type t) accu) env.ids l }

let find_id env0 env loc head x =
  let id = ident env0 loc x in
  try Env.find id env.ids
  with Not_found when head ->
    try ECDuce (!load_comp_unit x)
    with Not_found ->
      error loc "Cannot resolve this identifier"

let find_id_comp env0 env loc x =
  if ((match (U.get_str x).[0] with 'A'..'Z' -> true | _ -> false)
    && !has_ocaml_unit x)
  then EOCaml (U.get_str x)
  else find_id env0 env loc true x


let enter_value id t env = 
  { env with ids = Env.add id (Val t) env.ids }
let enter_values l env =
  { env with ids = 
      List.fold_left (fun accu (id,t) -> Env.add id (Val t) accu) env.ids l }
let enter_values_dummy l env =
  { env with ids = 
      List.fold_left (fun accu id -> Env.add id (Val Types.empty) accu) env.ids l }

let value_name_ok id env =
  try match Env.find id env.ids with
    | Val _ | EVal _ -> true
    | _ -> false
  with Not_found -> true

let iter_values env f =
  Env.iter (fun x ->
	      function Val t -> f x t;
		| _ -> ()) env.ids


let register_types cu env =
  Env.iter (fun x t -> match t with
	      | Type t -> Types.Print.register_global cu (Ident.value x) t
	      | _ -> ()) env.ids



let rec const env loc = function
  | LocatedExpr (loc,e) -> const env loc e
  | Pair (x,y) -> Types.Pair (const env loc x, const env loc y)
  | Xml (x,y) -> Types.Xml (const env loc x, const env loc y)
  | RecordLitt x -> Types.Record (parse_record env loc (const env loc) x)
  | String (i,j,s,c) -> Types.String (i,j,s,const env loc c)
  | Atom t -> Types.Atom (parse_atom env loc t)
  | Integer i -> Types.Integer i
  | Char c -> Types.Char c
  | Const c -> c
  | _ -> raise_loc_generic loc "This should be a scalar or structured constant"

(* I. Transform the abstract syntax of types and patterns into
      the internal form *)


let find_schema_component sch name =
  try ESchemaComponent (Env.find name sch.sch_comps)
  with Not_found ->    
    raise (Error (Printf.sprintf "No component named '%s' found in schema '%s'"
		    (Ns.QName.to_string name) sch.sch_uri))


let navig loc env0 (env,comp) id =
  match comp with
    | ECDuce cu ->
	let env = !from_comp_unit cu in
	let c = 
	  try find_id env0 env loc false id
	  with Not_found -> error loc "Unbound identifier" in
	let c = match c with
	  | Val t -> EVal (cu,ident env0 loc id,t)
	  | c -> c in
	env,c
    | EOCaml cu ->
	let s = cu ^ "." ^ (U.get_str id) in
	(match (U.get_str id).[0] with
	   | 'A'..'Z' -> env,EOCaml s
	   | _ -> env,EOCamlComponent s)
    | ESchema sch ->
	env,find_schema_component sch (ident env0 loc id)
    | Type _ -> error loc "Types don't have components"
    | Val _ | EVal _ -> error loc "Values don't have components"
    | ENamespace _ -> error loc "Namespaces don't have components"
    | EOCamlComponent _ -> error loc "Caml values don't have components"
    | ESchemaComponent _ -> error loc "Schema components don't have components"
(*
    | _ -> error loc "Invalid dot access"
*)
	

let rec find_global env loc ids =
  match ids with
    | id::rest ->
	let comp = find_id env env loc true id in
	snd (List.fold_left (navig loc env) (env,comp) rest)
    | _ -> assert false

let eval_ns env loc = function
  | `Uri ns -> ns
  | `Path ids ->
      match find_global env loc ids with
	| ENamespace ns -> ns
	| ESchema sch -> sch.sch_ns
	| _ -> error loc "This path does not refer to a namespace or schema"

let type_ns env loc p ns =
  (* TODO: check that p has no prefix *)
  let ns = eval_ns env loc ns in
  { env with 
      ns = Ns.add_prefix p ns env.ns;
      ids = Env.add (Ns.empty,p) (ENamespace ns) env.ids }
      


let find_global_type env loc ids =
  match find_global env loc ids with
    | Type t | ESchemaComponent (t,_) -> t
    | _ -> error loc "This path does not refer to a type"

let find_global_schema_component env loc ids =
  match find_global env loc ids with
    | ESchemaComponent c -> c
    | _ -> error loc "This path does not refer to a schema component"
  

let find_local_type env loc id =
  match Env.find id env.ids with
    | Type t -> t
    | _ -> raise Not_found

let find_value id env =
  match Env.find id env.ids with
    | Val t | EVal (_,_,t) -> t
    | _ -> raise Not_found

let do_open env cu =
  let env_cu = !from_comp_unit cu in
  let ids = 
    Env.fold
      (fun n d ids ->
	 let d = match d with
	   | Val t -> EVal (cu,n,t)
	   | d -> d in
	 Env.add n d ids)
      env_cu.ids
      env.ids in
  { env with 
      ids = ids;
      ns = Ns.merge_tables env.ns env_cu.ns }
      

let type_open env loc ids =
  match find_global env loc ids with
    | ECDuce cu -> do_open env cu
    | _ -> error loc "This path does not refer to a CDuce unit"

module IType = struct
  open Typepat

(* From AST to the intermediate representation *)

  type penv = {
    penv_tenv : t;
    penv_derec : node Env.t;
  }

  let penv tenv = { penv_tenv = tenv; penv_derec = Env.empty }

  let all_delayed = ref []

  let clean_on_err () = all_delayed := []

  let delayed loc =
    let s = mk_delayed () in
    all_delayed := (loc,s) :: !all_delayed;
    s

  let check_one_delayed (loc,p) =
    if not (check_wf p) then error loc "Ill-formed recursion"
    
  let check_delayed () =
    let l = !all_delayed in
    all_delayed := []; 
    List.iter check_one_delayed l

  let rec derecurs env p = match p.descr with
    | PatVar ids -> derecurs_var env p.loc ids
    | Recurs (p,b) -> derecurs (fst (derecurs_def env b)) p
    | Internal t -> mk_type t
    | NsT ns -> 
	mk_type (Types.atom (Atoms.any_in_ns (parse_ns env.penv_tenv p.loc ns)))
    | Or (p1,p2) -> mk_or (derecurs env p1) (derecurs env p2)
    | And (p1,p2) -> mk_and (derecurs env p1) (derecurs env p2)
    | Diff (p1,p2) -> mk_diff (derecurs env p1) (derecurs env p2)
    | Prod (p1,p2) -> mk_prod (derecurs env p1) (derecurs env p2)
    | XmlT (p1,p2) -> mk_xml (derecurs env p1) (derecurs env p2)
    | Arrow (p1,p2) -> mk_arrow (derecurs env p1) (derecurs env p2)
    | Optional p -> mk_optional (derecurs env p)
    | Record (o,r) -> 
	let aux = function
	  | (p,Some e) -> (derecurs env p, Some (derecurs env e))
	  | (p,None) -> derecurs env p, None in
	mk_record o (parse_record env.penv_tenv p.loc aux r)
    | Constant (x,c) -> 
	mk_constant (ident env.penv_tenv p.loc x) (const env.penv_tenv p.loc c)
    | Cst c -> mk_type (Types.constant (const env.penv_tenv p.loc c))
    | Regexp r -> rexp (derecurs_regexp env r)
    | Concat (p1,p2) ->  mk_concat (derecurs env p1) (derecurs env p2)
    | Merge (p1,p2) -> mk_merge (derecurs env p1) (derecurs env p2)
	  
  and derecurs_regexp env = function
    | Epsilon -> mk_epsilon
    | Elem p -> mk_elem (derecurs env p)
    | Guard p -> mk_guard (derecurs env p)
    | Seq (p1,p2) -> mk_seq (derecurs_regexp env p1) (derecurs_regexp env p2)
    | Alt (p1,p2) -> mk_alt (derecurs_regexp env p1) (derecurs_regexp env p2)
    | Star p -> mk_star (derecurs_regexp env p)
    | WeakStar p -> mk_weakstar (derecurs_regexp env p)
    | SeqCapture (loc,x,p) -> mk_seqcapt (ident env.penv_tenv loc x) (derecurs_regexp env p)
	  
  and derecurs_var env loc ids =
    match ids with
      | [v] ->
	  let v = ident env.penv_tenv loc v in
	  (try Env.find v env.penv_derec 
	   with Not_found -> 
	     try mk_type (find_local_type env.penv_tenv loc v)
	     with Not_found -> mk_capture v)
      | ids ->
	  mk_type (find_global_type env.penv_tenv loc ids)
	      
  and derecurs_def env b =
    let seen = ref IdSet.empty in
    let b = 
      List.map 
	(fun (loc,v,p) -> 
	   let v = ident env.penv_tenv loc v in
	   if IdSet.mem !seen v then 
	     raise_loc_generic loc
	       ("Multiple definitions for the type identifer " ^ 
		  (Ident.to_string v));
	   seen := IdSet.add v !seen;
	   (v,p,delayed loc))
	b in

    let n = 
      List.fold_left (fun env (v,p,s) -> Env.add v s env) env.penv_derec b in
    let env = { env with penv_derec = n } in
    List.iter (fun (v,p,s) -> link s (derecurs env p)) b;
    (env, b)

  let derec penv p =
    let d = derecurs penv p in
    elim_concats ();
    check_delayed ();
    internalize d;
    d


(* API *)

  let check_no_fv loc n =
    match peek_fv n with
      | None -> ()
      | Some x ->
	  raise_loc_generic loc 
	    ("Capture variable not allowed: " ^ (Ident.to_string x))

  let type_defs env b =
    let _,b' = derecurs_def (penv env) b in
    elim_concats ();
    check_delayed ();
    let aux loc d =
      internalize d;
      check_no_fv loc d;
      try typ d
      with Patterns.Error s -> raise_loc_generic loc s
    in
    let b = 
      List.map2 
	(fun (loc,v,p) (v',_,d) ->
	   let t = aux loc d in
	   if (loc <> noloc) && (Types.is_empty t) then
	     warning loc 
	       ("This definition yields an empty type for " ^ (U.to_string v));
	   (v',t)) b b' in
    List.iter (fun (v,t) -> Types.Print.register_global "" v t) b;
    enter_types b env

  let type_defs env b =
    try type_defs env b
    with exn -> clean_on_err (); raise exn

  let typ env t = 
    try
      let d = derec (penv env) t in
      check_no_fv t.loc d;
      try typ_node d
      with Patterns.Error s -> raise_loc_generic t.loc s
    with exn -> clean_on_err (); raise exn

  let pat env t = 
    try
      let d = derec (penv env) t in
      try pat_node d
      with Patterns.Error s -> raise_loc_generic t.loc s
    with exn -> clean_on_err (); raise exn

end

let typ = IType.typ
let pat = IType.pat
let type_defs = IType.type_defs

let dump_types ppf env =
  Env.iter (fun v -> 
	      function 
		  (Type _) -> Format.fprintf ppf " %a" Ident.print v
		| _ -> ()) env.ids

let dump_ns ppf env =
  Ns.dump_table ppf env.ns




(* II. Build skeleton *)


type type_fun = Types.t -> bool -> Types.t

module Fv = IdSet

type branch = Branch of Typed.branch * branch list

let cur_branch : branch list ref = ref []

let exp' loc e = 
  { Typed.exp_loc = loc; Typed.exp_typ = Types.empty; Typed.exp_descr = e; }

let exp loc fv e = fv, exp' loc e

let exp_nil = exp' noloc (Typed.Cst Sequence.nil_cst)

let pat_true = 
  let n = Patterns.make Fv.empty in
  Patterns.define n (Patterns.constr Builtin_defs.true_type);
  n

let pat_false =   
  let n = Patterns.make Fv.empty in
  Patterns.define n (Patterns.constr Builtin_defs.false_type);
  n


let ops = Hashtbl.create 13
let register_op op arity f = Hashtbl.add ops op (arity,f)
let typ_op op = snd (Hashtbl.find ops op)

let fun_name env a =
  match a.fun_name with
    | None -> None
    | Some (loc,s) -> Some (ident env loc s)

let is_op env s = 
  if (Env.mem s env.ids) then None
  else
    let (ns,s) = s in
    if Ns.Uri.equal ns Ns.empty then
      let s = U.get_str s in
      try 
	let o = Hashtbl.find ops s in
	Some (s, fst o)
      with Not_found -> None
    else None

let rec expr env loc = function
  | LocatedExpr (loc,e) -> expr env loc e
  | Forget (e,t) ->
      let (fv,e) = expr env loc e and t = typ env t in
      exp loc fv (Typed.Forget (e,t))
  | Check (e,t) ->
      let (fv,e) = expr env loc e and t = typ env t in
      exp loc fv (Typed.Check (ref Types.empty,e,t))
  | Var s -> var env loc s
  | Apply (e1,e2) -> 
      let (fv1,e1) = expr env loc e1 and (fv2,e2) = expr env loc e2 in
      let fv = Fv.cup fv1 fv2 in
      (match e1.Typed.exp_descr with
	 | Typed.Op (op,arity,args) when arity > 0 -> 
	     exp loc fv (Typed.Op (op,arity - 1,args @ [e2]))
	 | _ ->
	     exp loc fv (Typed.Apply (e1,e2)))
  | Abstraction a -> abstraction env loc a
  | (Integer _ | Char _ | Atom _ | Const _) as c -> 
      exp loc Fv.empty (Typed.Cst (const env loc c))
  | Pair (e1,e2) ->
      let (fv1,e1) = expr env loc e1 and (fv2,e2) = expr env loc e2 in
      exp loc (Fv.cup fv1 fv2) (Typed.Pair (e1,e2))
  | Xml (e1,e2) ->
      let (fv1,e1) = expr env loc e1 and (fv2,e2) = expr env loc e2 in
      let n = if env.keep_ns then Some env.ns else None in
      exp loc (Fv.cup fv1 fv2) (Typed.Xml (e1,e2,n))
  | Dot _ as e ->
      dot loc env e []
  | TyArgs (Dot _ as e, args) ->
      dot loc env e args
  | TyArgs _ ->
      error loc "Only OCaml external can have type arguments"
  | RemoveField (e,l) ->
      let (fv,e) = expr env loc e in
      exp loc fv (Typed.RemoveField (e,parse_label env loc l))
  | RecordLitt r -> 
      let fv = ref Fv.empty in
      let r = parse_record env loc
		(fun e -> 
		   let (fv2,e) = expr env loc e 
		   in fv := Fv.cup !fv fv2; e)
		r in
      exp loc !fv (Typed.RecordLitt r)
  | String (i,j,s,e) ->
      let (fv,e) = expr env loc e in
      exp loc fv (Typed.String (i,j,s,e))
  | Match (e,b) -> 
      let (fv1,e) = expr env loc e
      and (fv2,b) = branches env b in
      exp loc (Fv.cup fv1 fv2) (Typed.Match (e, b))
  | Map (e,b) ->
      let (fv1,e) = expr env loc e
      and (fv2,b) = branches env b in
      exp loc (Fv.cup fv1 fv2) (Typed.Map (e, b))
  | Transform (e,b) ->
      let (fv1,e) = expr env loc e
      and (fv2,b) = branches env b in
      exp loc (Fv.cup fv1 fv2) (Typed.Transform (e, b))
  | Xtrans (e,b) ->
      let (fv1,e) = expr env loc e
      and (fv2,b) = branches env b in
      exp loc (Fv.cup fv1 fv2) (Typed.Xtrans (e, b))
  | Validate (e,ids) ->
      let (fv,e) = expr env loc e in
      let (t,v) = find_global_schema_component env loc ids  in
      exp loc fv (Typed.Validate (e, t, v))
  | SelectFW (e,from,where) ->
      select_from_where env loc e from where
  | Try (e,b) ->
      let (fv1,e) = expr env loc e
      and (fv2,b) = branches env b in
      exp loc (Fv.cup fv1 fv2) (Typed.Try (e, b))
  | NamespaceIn (pr,ns,e) ->
      let env = type_ns env loc pr ns in
      expr env loc e
  | KeepNsIn (k,e) ->
      expr (type_keep_ns env k) loc e
  | Ref (e,t) ->
      let (fv,e) = expr env loc e and t = typ env t in
      exp loc fv (Typed.Ref (e,t))

and if_then_else loc cond yes no =
  let b = {
    Typed.br_typ = Types.empty;
    Typed.br_branches = [
      { Typed.br_loc = yes.Typed.exp_loc;
	Typed.br_used = false;
	Typed.br_vars_empty = Fv.empty;
	Typed.br_pat = pat_true;
	Typed.br_body = yes };
      { Typed.br_loc = no.Typed.exp_loc;
	Typed.br_used = false;
	Typed.br_vars_empty = Fv.empty;
	Typed.br_pat = pat_false;
	Typed.br_body = no } ];
    Typed.br_accept = Builtin_defs.bool;
  } in
  exp' loc (Typed.Match (cond,b))


and dot loc env0 e args =
  let dot_access loc (fv,e) l =
    exp loc fv (Typed.Dot (e,parse_label env0 loc l)) in

  let no_args () =
    if args <> [] then
      error loc "Only OCaml externals can have type arguments" in
  let rec aux loc = function
    | LocatedExpr (loc,e) -> aux loc e
    | Dot (e,id) ->
	(match aux loc e with
	   | `Val e -> `Val (dot_access loc e id)
	   | `Comp c -> `Comp (navig loc env0 c id))
    | Var id -> 
	(match find_id_comp env0 env0 loc id with
	   | Val _ -> `Val (var env0 loc id)
	   | c -> `Comp (env0,c))
    | e -> `Val (expr env0 loc e)
  in
  match aux loc e with
    | `Val e -> no_args (); e
    | `Comp (_,EVal (cu,id,t)) -> 
	no_args (); exp loc Fv.empty (Typed.ExtVar (cu,id,t))
    | `Comp (_,EOCamlComponent s) -> extern loc env0 s args
    | _ -> error loc "This dot notation does not refer to a value"
	
and extern loc env s args = 
  let args = List.map (typ env) args in
  try
    let (i,t) =
      if !has_static_external s then
	(`Builtin s, Externals.typ s args)
      else
	let (i,t) = Externals.resolve s args in
	(`Ext i, t) in
    exp loc Fv.empty (Typed.External (t,i))
  with exn -> raise_loc loc exn
    
and var env loc s =
  let id = ident env loc s in
  match is_op env id with
    | Some (s,arity) -> 
	let e = match s with 
	  | "print_xml" | "print_xml_utf8" ->
	      Typed.NsTable (env.ns,Typed.Op (s, arity, []))
	  | "load_xml" when env.keep_ns ->
	      Typed.Op ("!load_xml",arity,[])
	  | _ -> Typed.Op (s, arity, []) 
	in
	exp loc Fv.empty e
    | None ->
	try match Env.find id env.ids with
	  | Val _ -> exp loc (Fv.singleton id) (Typed.Var id)
	  | EVal (cu,id,t) -> exp loc Fv.empty (Typed.ExtVar (cu,id,t))
	  | _ ->  error loc "This identifier does not refer to a value"
	with Not_found -> error loc "Unbound identifier"


and abstraction env loc a =
  let iface = 
    List.map 
      (fun (t1,t2) -> (typ env t1, typ env t2)) a.fun_iface in
  let t = 
    List.fold_left 
      (fun accu (t1,t2) -> Types.cap accu (Types.arrow t1 t2)) 
      Types.any iface in
  let iface = 
    List.map 
      (fun (t1,t2) -> (Types.descr t1, Types.descr t2)) 
      iface in
  let fun_name = fun_name env a in
  let env' = 
    match fun_name with 
      | None -> env
      | Some f -> enter_values_dummy [ f ] env
  in
  let (fv0,body) = branches env' a.fun_body in
  let fv = match fun_name with
    | None -> fv0
    | Some f -> Fv.remove f fv0 in
  let e = Typed.Abstraction 
	    { Typed.fun_name = fun_name;
	      Typed.fun_iface = iface;
	      Typed.fun_body = body;
	      Typed.fun_typ = t;
	      Typed.fun_fv = fv
	    } in
  exp loc fv e
    
and branches env b = 
  let fv = ref Fv.empty in
  let accept = ref Types.empty in
  let branch (p,e) = 
    let cur_br = !cur_branch in
    cur_branch := [];
    let ploc = p.loc in
    let p = pat env p in
    let fvp = Patterns.fv p in
    let (fv2,e) = expr (enter_values_dummy fvp env) noloc e in
    let br_loc = merge_loc ploc e.Typed.exp_loc in
    (match Fv.pick (Fv.diff fvp fv2) with
       | None -> ()
       | Some x ->
	   let x = Ident.to_string x in
	   warning br_loc 
	     ("The capture variable " ^ x ^ 
	      " is declared in the pattern but not used in the body of this branch. It might be a misspelled or undeclared type or name (if it isn't, use _ instead)."));
    let fv2 = Fv.diff fv2 fvp in
    fv := Fv.cup !fv fv2;
    accept := Types.cup !accept (Types.descr (Patterns.accept p));
    let br = 
      { 
	Typed.br_loc = br_loc;
	Typed.br_used = br_loc == noloc;
	Typed.br_vars_empty = fvp;
	Typed.br_pat = p;
	Typed.br_body = e } in
    cur_branch := Branch (br, !cur_branch) :: cur_br;
    br in
  let b = List.map branch b in
  (!fv, 
   { 
     Typed.br_typ = Types.empty; 
     Typed.br_branches = b; 
     Typed.br_accept = !accept;
   } 
  )

and select_from_where env loc e from where =
  let env = ref env in
  let all_fv = ref Fv.empty in
  let bound_fv = ref Fv.empty in
  let clause (p,e) =
    let ploc = p.loc in
    let p = pat !env p in
    let fvp = Patterns.fv p in
    let (fv2,e) = expr !env noloc e in
    env := enter_values_dummy fvp !env;
    all_fv := Fv.cup (Fv.diff fv2 !bound_fv) !all_fv;
    bound_fv := Fv.cup fvp !bound_fv;
    (ploc,p,fvp,e) in
  let from = List.map clause from in
  let where = List.map (expr !env noloc) where in

  let put_cond rest (fv,cond) = 
    all_fv := Fv.cup (Fv.diff fv !bound_fv) !all_fv;
    if_then_else loc cond rest exp_nil in
  let aux (ploc,p,fvp,e) (where,rest) = 
    (* Put here the conditions that depends on variables in fvp *)
    let (above,here) = List.partition (fun (v,_) -> Fv.disjoint v fvp) where in
    (* if cond then ... else [] *)
    let rest = List.fold_left put_cond rest here in
    (* transform e with p -> ... *)
    let br = { Typed.br_loc = ploc;
	  Typed.br_used = false;
	  Typed.br_vars_empty = fvp;
	  Typed.br_pat = p;
	  Typed.br_body = rest } in
    cur_branch := [ Branch (br, !cur_branch) ];
    let b = {
      Typed.br_typ = Types.empty;
      Typed.br_branches = [ br ];
      Typed.br_accept = Types.descr (Patterns.accept p);
    } in
    let br_loc = merge_loc ploc e.Typed.exp_loc in
    (above,exp' br_loc (Typed.Transform (e, b)))
  in
  let cur_br = !cur_branch in
  cur_branch := [];
  let (fv,e) = expr !env noloc (Pair(e,cst_nil)) in
  cur_branch := !cur_branch @ cur_br;
  let (where,rest) = List.fold_right aux from (where,e) in
  (* The remaining conditions are constant. Gives a warning for that. *)
  (match where with
     | (_,e) :: _ ->
	 warning e.Typed.exp_loc
	   "This 'where' condition does not depend on any captured variable"
     | _ -> ());
  let rest = List.fold_left put_cond rest where in
  (Fv.cup !all_fv (Fv.diff fv !bound_fv)), rest

let expr env e = snd (expr env noloc e)

let let_decl env p e =
  { Typed.let_pat = pat env p;
    Typed.let_body = expr env e }


(* Hide global "typing/parsing" environment *)


(* III. Type-checks *)

open Typed

let localize loc f x =
  try f x
  with 
    | (Error _ | Constraint (_,_)) as exn -> raise (Location.Location (loc,`Full,exn))
    | Warning (s,t) -> warning loc s; t

let require loc t s = 
  if not (Types.subtype t s) then raise_loc loc (Constraint (t, s))

let verify loc t s = 
  require loc t s; t

let verify_noloc t s =
  if not (Types.subtype t s) then raise (Constraint (t, s));
  t

let check_str loc ofs t s = 
  if not (Types.subtype t s) then raise_loc_str loc ofs (Constraint (t, s));
  t

let should_have loc constr s = 
  raise_loc loc (ShouldHave (constr,s))

let should_have_str loc ofs constr s = 
  raise_loc_str loc ofs (ShouldHave (constr,s))

let flatten arg constr precise =
  let constr' = Sequence.star 
		  (Sequence.approx (Types.cap Sequence.any constr)) in
  let sconstr' = Sequence.star constr' in
  let exact = Types.subtype constr' constr in
  if exact then
    let t = arg sconstr' precise in
    if precise then Sequence.flatten t else constr
  else
    let t = arg sconstr' true in
    verify_noloc (Sequence.flatten t) constr

let rec type_check env e constr precise = 
  let d = type_check' e.exp_loc env e.exp_descr constr precise in
  let d = if precise then d else constr in
  e.exp_typ <- Types.cup e.exp_typ d;
  d

and type_check' loc env e constr precise = match e with
  | Forget (e,t) ->
      let t = Types.descr t in
      ignore (type_check env e t false);
      verify loc t constr

  | Check (t0,e,t) ->
      let te = type_check env e Types.any true in
      t0 := Types.cup !t0 te;
      verify loc (Types.cap te (Types.descr t)) constr

  | Abstraction a ->
      let t =
	try Types.Arrow.check_strenghten a.fun_typ constr 
	with Not_found -> 
	  should_have loc constr
	    "but the interface of the abstraction is not compatible"
      in
      let env = match a.fun_name with
	| None -> env
	| Some f -> enter_value f a.fun_typ env in
      List.iter 
	(fun (t1,t2) ->
	   let acc = a.fun_body.br_accept in 
	   if not (Types.subtype t1 acc) then
	     raise_loc loc (NonExhaustive (Types.diff t1 acc));
	   ignore (type_check_branches loc env t1 a.fun_body t2 false)
	) a.fun_iface;
      t

  | Match (e,b) ->
      let t = type_check env e b.br_accept true in
      type_check_branches loc env t b constr precise

  | Try (e,b) ->
      let te = type_check env e constr precise in
      let tb = type_check_branches loc env Types.any b constr precise in
      Types.cup te tb

  | Pair (e1,e2) ->
      type_check_pair loc env e1 e2 constr precise

  | Xml (e1,e2,_) ->
      type_check_pair ~kind:`XML loc env e1 e2 constr precise

  | RecordLitt r ->
      type_record loc env r constr precise

  | Map (e,b) ->
      type_map loc env false e b constr precise

  | Transform (e,b) ->
      localize loc (flatten (type_map loc env true e b) constr) precise

  | Apply (e1,e2) ->
      let t1 = type_check env e1 Types.Arrow.any true in
      let t1 = Types.Arrow.get t1 in
      let dom = Types.Arrow.domain t1 in
      let res =
	if Types.Arrow.need_arg t1 then
	  let t2 = type_check env e2 dom true in
	  Types.Arrow.apply t1 t2
	else
	  (ignore (type_check env e2 dom false); Types.Arrow.apply_noarg t1)
      in
      verify loc res constr

  | Var s -> 
      verify loc (find_value s env) constr

  | ExtVar (cu,s,t) ->
      verify loc t constr
  | Cst c -> 
      verify loc (Types.constant c) constr

  | String (i,j,s,e) ->
      type_check_string loc env 0 s i j e constr precise

  | Dot (e,l) ->
      let expect_rec = Types.record l (Types.cons constr) in
      let expect_elt = 
	Types.xml 
	  Types.any_node 
	  (Types.cons (Types.times (Types.cons expect_rec) Types.any_node)) in
      let t = type_check env e (Types.cup expect_rec expect_elt) precise in
      let t_elt =
	let t = Types.Product.pi2 (Types.Product.get ~kind:`XML t) in
	let t = Types.Product.pi1 (Types.Product.get t) in
	t in
      if not precise then constr
      else
	(try Types.Record.project (Types.cup t t_elt) l
	 with Not_found -> assert false)

  | RemoveField (e,l) ->
      let t = type_check env e Types.Record.any true in
      let t = Types.Record.remove_field t l in
      verify loc t constr

  | Xtrans (e,b) ->
      let t = type_check env e Sequence.any true in
      let t = 
	try
	  Sequence.map_tree constr
	    (fun cstr t ->
	       let resid = Types.diff t b.br_accept in
	       let res = type_check_branches loc env t b cstr true in
	       (res,resid)
	    ) t
	with (Sequence.Error _) as exn -> 
	  let rec find_loc = function
	    | Location.Location (loc,precise,exn) ->
		(loc,precise), exn
	    | Sequence.Error (Sequence.UnderTag (t,exn)) ->
		let (l,exn) = find_loc exn in
		l, Sequence.Error (Sequence.UnderTag (t,exn))
	    | exn -> raise Not_found
	  in
	  try 
	    let (loc,precise), exn = find_loc exn in
	    raise (Location.Location (loc,precise,exn))
	  with Not_found ->
	    raise_loc loc exn
      in
      verify loc t constr

  | Validate (e, t, _) ->
      ignore (type_check env e Types.any false);
      verify loc t constr

  | Ref (e,t) ->
      ignore (type_check env e (Types.descr t) false);
      verify loc (Builtin_defs.ref_type t) constr

  | External (t,_) ->
      verify loc t constr

  | Op (op,_,args) ->
      let args = List.map (type_check env) args in
      let t = localize loc (typ_op op args constr) precise in
      verify loc t constr

  | NsTable (ns,e) ->
      type_check' loc env e constr precise

and type_check_pair ?(kind=`Normal) loc env e1 e2 constr precise =
  let rects = Types.Product.normal ~kind constr in
  if Types.Product.is_empty rects then 
    (match kind with
      | `Normal -> should_have loc constr "but it is a pair"
      | `XML -> should_have loc constr "but it is an XML element");
  let need_s = Types.Product.need_second rects in
  let t1 = type_check env e1 (Types.Product.pi1 rects) (precise || need_s) in
  let c2 = Types.Product.constraint_on_2 rects t1 in
  if Types.is_empty c2 then 
    raise_loc loc (ShouldHave2 (constr,"but the first component has type",t1));
  let t2 = type_check env e2 c2 precise in

  if precise then 
    match kind with
      | `Normal -> Types.times (Types.cons t1) (Types.cons t2)
      | `XML -> Types.xml (Types.cons t1) (Types.cons t2)
  else
    constr

and type_check_string loc env ofs s i j e constr precise =
  if U.equal_index i j then type_check env e constr precise
  else
    let rects = Types.Product.normal constr in
    if Types.Product.is_empty rects 
    then should_have_str loc ofs constr "but it is a string"
    else
      let (ch,i') = U.next s i in
      let ch = Chars.V.mk_int ch in
      let tch = Types.constant (Types.Char ch) in
      let t1 = check_str loc ofs tch (Types.Product.pi1 rects) in
      let c2 = Types.Product.constraint_on_2 rects t1 in
      let t2 = type_check_string loc env (ofs + 1) s i' j e c2 precise in
      if precise then Types.times (Types.cons t1) (Types.cons t2)
      else constr

and type_record loc env r constr precise =
(* try to get rid of precise = true for values of fields *)
(* also: the use equivalent of need_second to optimize... *)
  if not (Types.Record.has_record constr) then
    should_have loc constr "but it is a record";
  let (rconstr,res) = 
    List.fold_left
      (fun (rconstr,res) (l,e) ->
	 (* could compute (split l e) once... *)
	 let pi = Types.Record.project_opt rconstr l in
	 if Types.is_empty pi then 
	   (let l = Label.string_of_attr l in
	    should_have loc constr
	      (Printf.sprintf "Field %s is not allowed here." l));
	 let t = type_check env e pi true in
	 let rconstr = Types.Record.condition rconstr l t in
	 let res = (l,Types.cons t) :: res in
	 (rconstr,res)
      ) (constr, []) (LabelMap.get r)
  in
  if not (Types.Record.has_empty_record rconstr) then
    should_have loc constr "More fields should be present";
  let t = 
    Types.record_fields (false, LabelMap.from_list (fun _ _ -> assert false) res)
  in
  verify loc t constr


and type_check_branches loc env targ brs constr precise =
  if Types.is_empty targ then Types.empty
  else (
    brs.br_typ <- Types.cup brs.br_typ targ;
    branches_aux loc env targ 
      (if precise then Types.empty else constr) 
      constr precise brs.br_branches
  )
    
and branches_aux loc env targ tres constr precise = function
  | [] -> tres
  | b :: rem ->
      let p = b.br_pat in
      let acc = Types.descr (Patterns.accept p) in

      let targ' = Types.cap targ acc in
      if Types.is_empty targ' 
      then branches_aux loc env targ tres constr precise rem
      else 
	( b.br_used <- true;
	  let res = Patterns.filter targ' p in
	  let res = IdMap.map Types.descr res in
	  
	  b.br_vars_empty <-
	    IdMap.domain (
	      IdMap.filter (fun x t -> Types.subtype t Sequence.nil_type)
		(IdMap.restrict res b.br_vars_empty));

	  let env' = enter_values (IdMap.get res) env in
	  let t = type_check env' b.br_body constr precise in
	  let tres = if precise then Types.cup t tres else tres in
	  let targ'' = Types.diff targ acc in
	  if (Types.non_empty targ'') then 
	    branches_aux loc env targ'' tres constr precise rem 
	  else
	    tres
	)

and type_map loc env def e b constr precise = 
  let acc = if def then Sequence.any else Sequence.star b.br_accept in
  let t = type_check env e acc true in

  let constr' = Sequence.approx (Types.cap Sequence.any constr) in
  let exact = Types.subtype (Sequence.star constr') constr in
  (* Note: 
     - could be more precise by integrating the decomposition
     of constr inside Sequence.map.
  *)
  let res = 
    Sequence.map 
      (fun t ->
	 let res = 
	   type_check_branches loc env t b constr' (precise || (not exact)) in
	 if def && not (Types.subtype t b.br_accept) 
	 then (require loc Sequence.nil_type constr'; Types.cup res Sequence.nil_type)
	 else res)
      t in
  if exact then res else verify loc res constr

and type_let_decl env l =
  let acc = Types.descr (Patterns.accept l.let_pat) in
  let t = type_check env l.let_body acc true in
  let res = Patterns.filter t l.let_pat in
  IdMap.mapi_to_list (fun x t -> (x, Types.descr t)) res

and type_rec_funs env l =
  let typs = 
    List.fold_left
      (fun accu -> function  
	 | { exp_descr=Abstraction { fun_typ = t; fun_name = Some f };
	     exp_loc=loc } ->
	     if not (value_name_ok f env) then
	       error loc "This function name clashes with another kind of identifier";
	     (f,t)::accu
	 | _ -> assert false
      ) [] l
  in
  let env = enter_values typs env in
  List.iter (fun e -> ignore (type_check env e Types.any false)) l;
  typs

let rec unused_branches b =
  List.iter
    (fun (Branch (br,s)) -> 
       if not br.br_used 
       then warning br.br_loc "This branch is not used"
       else (
	 if not (IdSet.is_empty br.br_vars_empty)
	 then (
	   let msg = 
	     try
	       let l = 
		 List.map 
		   (fun x ->
		      let x = Ident.to_string x in
		      if (String.compare x "$$$" = 0) then raise Exit else x)
		   (IdSet.get br.br_vars_empty) in
	       let l = String.concat "," l in
	       "The following variables always match the empty sequence: " ^
		 l 
	     with Exit ->
	       "This projection always returns the empty sequence"
	   in
	   warning br.br_loc msg
	 );
	 unused_branches s
       )
    )
    b

let report_unused_branches () =
  unused_branches !cur_branch;
  cur_branch := []
  
let clear_unused_branches () =
  cur_branch := []



(* API *)

let type_expr env e =
  clear_unused_branches ();
  let e = expr env e in
  let t = type_check env e Types.any true in
  report_unused_branches ();
  (e,t)

let type_let_decl env p e =
  clear_unused_branches ();
  let decl = let_decl env p e in
  let typs = type_let_decl env decl in
  report_unused_branches ();
  let env = enter_values typs env in
  (env,decl,typs)

let type_let_funs env funs =
  clear_unused_branches ();
  let rec id = function
    | Ast.LocatedExpr (_,e) -> id e
    | Ast.Abstraction a -> fun_name env a
    | _ -> assert false
  in
  let ids =
    List.fold_left (fun accu f -> match id f with Some x -> x::accu | None -> accu)
      [] funs in
  let env' = enter_values_dummy ids env in
  let funs = List.map (expr env') funs in
  let typs = type_rec_funs env funs in
  report_unused_branches ();
  let env = enter_values typs env in
  (env,funs,typs)

(*
let find_cu x env =
  match find_cu noloc x env with
    | ECDuce cu -> cu
    | _ -> raise (Error ("Cannot find external unit " ^ (U.to_string x)))
*)
