(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Ident
open Lambda

type env = {
  cu: Compunit.t option;  (* None: toplevel *)
  vars: var_loc Env.t;
  stack_size: int;
  max_stack: int ref;
  global_size: int
}

let global_size env = env.global_size

let mk cu = { cu = cu; vars = Env.empty; stack_size = 0; max_stack = ref 0; global_size = 0 }
let empty_toplevel = mk None
let empty x = mk (Some x)

let find x env =
  try Env.find x env.vars
  with Not_found -> 
    failwith ("Compile: cannot find " ^ (Ident.to_string x))

let find_slot x env =
  match find x env with
    | Ext (_,slot) -> slot
    | _ -> assert false
 
let from_comp_unit = ref (fun cu -> assert false)

let find_ext cu x =
  let env = !from_comp_unit cu in
  find x env

let enter_local env x =
  let new_size = env.stack_size + 1 in
  if new_size > !(env.max_stack) then (env.max_stack) := new_size;
  { env with 
      vars = Env.add x (Local env.stack_size) env.vars;
      stack_size = new_size }

let enter_global_toplevel env x =
  { env with 
      vars = Env.add x (Global env.global_size) env.vars;
      global_size = env.global_size + 1 }

let enter_global_cu cu env x =
  { env with 
      vars = Env.add x (Ext (cu,env.global_size)) env.vars;
      global_size = env.global_size + 1 }

let rec compile env e = compile_aux env e.Typed.exp_descr
and compile_aux env = function
  | Typed.Forget (e,_) -> compile env e
  | Typed.Check (t0,e,t) -> 
      let d = Patterns.Compile.make_checker !t0 (Types.descr t) in
      Check (compile env e, d)
  | Typed.Var x -> Var (find x env)
  | Typed.ExtVar (cu,x,_) -> Var (find_ext cu x)
  | Typed.Apply (e1,e2) -> Apply (compile env e1, compile env e2)
  | Typed.Abstraction a -> compile_abstr env a
  | Typed.Cst c -> Const (Value.const c)
  | Typed.Pair (e1,e2) -> Pair(compile env e1, compile env e2)
  | Typed.Xml (e1, { Typed.exp_descr = Typed.Pair (e2,e3) }, None) -> 
      Xml (compile env e1, compile env e2, compile env e3)
  | Typed.Xml (e1, { Typed.exp_descr = Typed.Pair (e2,e3) }, Some t) -> 
      XmlNs (compile env e1, compile env e2, compile env e3,t)
  | Typed.Xml _ -> assert false
  | Typed.RecordLitt r -> 
      let r = List.map (fun (l,e) -> (Upool.int l, compile env e)) 
	(LabelMap.get r)
      in
      Record (Imap.create (Array.of_list r))
  | Typed.String (i,j,s,q) -> String (i,j,s,compile env q)
  | Typed.Match (e,brs) -> Match (compile env e, compile_branches env brs)
  | Typed.Map (e,brs) -> Map (compile env e, compile_branches env brs)
  | Typed.Transform (e,brs) -> Transform (compile env e, compile_branches env brs)
  | Typed.Xtrans (e,brs) -> Xtrans (compile env e, compile_branches env brs)
  | Typed.Validate (e,_,validator) -> Validate (compile env e, validator)
  | Typed.RemoveField (e,l) -> RemoveField (compile env e,l)
  | Typed.Dot (e,l) -> Dot (compile env e, l)
  | Typed.Try (e,brs) -> Try (compile env e, compile_branches env brs)
  | Typed.Ref (e,t) ->  Ref (compile env e, t)
  | Typed.External (t,`Ext i) -> 
      (match env.cu with
	 | Some cu -> Var (External (cu,i))
	 | None -> failwith "Cannot compile externals in the toplevel")
  | Typed.External (t,`Builtin s) -> 
      Var (Builtin s)
  | Typed.Op (op,_,args) -> 
      let rec aux = function
	| [arg] -> [ compile env arg ]
	| arg::l -> (compile env arg) :: (aux l)
	| [] -> [] in
      Op (op, aux args)
  | Typed.NsTable (ns,e) ->
      NsTable (ns, compile_aux env e)

and compile_abstr env a =
  let fun_env = 
    match a.Typed.fun_name with
      | Some x -> Env.add x (Env 0) Env.empty
      | None -> Env.empty in

  let (slots,nb_slots,fun_env) = 
    List.fold_left 
      (fun (slots,nb_slots,fun_env) x ->
	 match find x env with
	   | (Local _ | Env _) as p -> 
	       p::slots,
	       succ nb_slots,
	       Env.add x (Env nb_slots) fun_env;
	   | Global _ | Ext _ | External _ | Builtin _ as p -> 
	       slots,
	       nb_slots,
	       Env.add x p fun_env
	   | Dummy -> assert false
      )
      ([Dummy],1,fun_env) (IdSet.get a.Typed.fun_fv) in


  let slots = Array.of_list (List.rev slots) in  
  let env = { env with vars = fun_env; stack_size = 0; max_stack = ref 0 } in
  let body = compile_branches env a.Typed.fun_body in
  Abstraction (slots, a.Typed.fun_iface, body, !(env.max_stack))

and compile_branches env (brs : Typed.branches) =
  (* Don't compile unused branches, because they have not been
     type checked. *)
  let used = List.filter (fun br -> br.Typed.br_used) brs.Typed.br_branches in
  let b = List.map (compile_branch env) used in
  let (disp,rhs) = Patterns.Compile.make_branches brs.Typed.br_typ b in
  { brs_stack_pos = env.stack_size;
    brs_accept_chars = not (Types.Char.is_empty brs.Typed.br_accept);
    brs_disp = disp;
    brs_rhs = rhs }

and compile_branch env br =
  let env = List.fold_left enter_local env (Patterns.fv br.Typed.br_pat) in
  (br.Typed.br_pat, compile env br.Typed.br_body)

let enter_globals env n =  match env.cu with
  | None -> List.fold_left enter_global_toplevel env n
  | Some cu -> List.fold_left (enter_global_cu cu) env n

let compile_expr env e =
  let env = { env with max_stack = ref 0; stack_size = 0 } in
  let e = compile env e in
  (e,!(env.max_stack))

let compile_let_decl env decl =
  let pat = decl.Typed.let_pat in
  let e,lsize = compile_expr env decl.Typed.let_body in
  let env = enter_globals env (Patterns.fv pat) in

  let comp = 
    Patterns.Compile.make_branches 
      (Types.descr (Patterns.accept pat)) [ pat, () ] in
  let (disp, n) = 
    match comp with
      | (disp, [| Auto_pat.Match (n, ()) |]) -> (disp,n)
      | _ -> assert false in
  (env, [ LetDecls (e,lsize,disp,n) ])

let compile_rec_funs env funs =
  let fun_name = function
    | { Typed.exp_descr=Typed.Abstraction{Typed.fun_name = Some x}} -> x
    | _ -> assert false in
  let fun_a env e =
    let e,lsize = compile_expr env e in
    LetDecl (e,lsize) in
  let env = enter_globals env (List.map fun_name funs) in
  let code= List.map (fun_a env) funs in
  (env, code)


(****************************************)

open Location

let eval ~run ~show (tenv,cenv,codes) e =
  let (e,t) = Typer.type_expr tenv e in
  let e,lsize = compile_expr cenv e in
  if run then
    let v = Eval.expr e lsize in
    show None t (Some v)
  else
    show None t None;
  (tenv,cenv, Eval (e,lsize) :: codes)

let run_show ~run ~show tenv cenv codes ids =
  if run then
    let () = Eval.eval_toplevel codes in
    List.iter 
      (fun (id,_) -> show (Some id) 
	 (Typer.find_value id tenv)
	 (Some (Eval.eval_var (find id cenv)))) ids
  else
    List.iter 
      (fun (id,_) -> show (Some id) 
	 (Typer.find_value id tenv)
	 None) ids
  
let let_decl ~run ~show (tenv,cenv,codes) p e =
  let (tenv,decl,ids) = Typer.type_let_decl tenv p e in
  let (cenv,code) = compile_let_decl cenv decl in
  run_show ~run ~show tenv cenv code ids;
  (tenv,cenv,List.rev_append code codes)
  
let let_funs ~run ~show (tenv,cenv,codes) funs =
  let (tenv,funs,ids) = Typer.type_let_funs tenv funs in
  let (cenv,code) = compile_rec_funs cenv funs in
  run_show ~run ~show tenv cenv code ids;
  (tenv,cenv,List.rev_append code codes)
  
let type_defs (tenv,cenv,codes) typs =
  let tenv = Typer.type_defs tenv typs in
  (tenv,cenv,codes)

let namespace (tenv,cenv,codes) loc pr ns =
  let tenv = Typer.type_ns tenv loc pr ns in
  (tenv,cenv,codes)

let keep_ns (tenv,cenv,codes) k =
  let tenv = Typer.type_keep_ns tenv k in
  (tenv,cenv,codes)

let schema (tenv,cenv,codes) loc x sch =
  let tenv = Typer.type_schema tenv loc x sch in
  (tenv,cenv,codes)

let using (tenv,cenv,codes) loc x cu =
  let tenv = Typer.type_using tenv loc x cu in
  (tenv,cenv,codes)

let do_open (tenv,cenv,codes) loc path =
  let tenv = Typer.type_open tenv loc path in
  (tenv,cenv,codes)

let rec collect_funs accu = function
  | { descr = Ast.FunDecl e } :: rest -> collect_funs (e::accu) rest
  | rest -> (accu,rest)

let rec collect_types accu = function
  | { descr = Ast.TypeDecl ((loc,x),t) } :: rest -> 
      collect_types ((loc,x,t) :: accu) rest
  | rest -> (accu,rest)

let rec phrases ~run ~show ~directive =
  let rec loop accu phs =
    match phs with
      | { descr = Ast.FunDecl _ } :: _ -> 
	  let (funs,rest) = collect_funs [] phs in
	  loop (let_funs ~run ~show accu funs) rest
      | { descr = Ast.TypeDecl (_,_) } :: _ ->
	  let (typs,rest) = collect_types [] phs in
	  loop (type_defs accu typs) rest
      | { descr = Ast.SchemaDecl (name, uri); loc = loc } :: rest ->
	  loop (schema accu loc name uri) rest
      | { descr = Ast.Namespace (pr,ns); loc = loc } :: rest ->
	  loop (namespace accu loc pr ns) rest
      | { descr = Ast.KeepNs b } :: rest ->
	  loop (keep_ns accu b) rest
      | { descr = Ast.Using (x,cu); loc = loc } :: rest ->
	  loop (using accu loc x cu) rest
      | { descr = Ast.Open path; loc = loc } :: rest ->
	  loop (do_open accu loc path) rest
      | { descr = Ast.EvalStatement e } :: rest ->
	  loop (eval ~run ~show accu e) rest
      | { descr = Ast.LetDecl (p,e) } :: rest ->
	  loop (let_decl ~run ~show accu p e) rest
      | { descr = Ast.Directive d } :: rest ->
	  let (tenv,cenv,_) = accu in
	  directive tenv cenv d;
	  loop accu rest
      | [] -> 
	  accu
  in
  loop

let comp_unit ?(run=false) 
  ?(show=fun _ _ _ -> ()) 
  ?(directive=fun _ _ _ -> ())  tenv cenv phs =
  let (tenv,cenv,codes) = phrases ~run ~show ~directive (tenv,cenv,[]) phs in
  (tenv,cenv,List.rev codes)


let compile_eval_expr env e =
  let e,lsize = compile_expr env e in
  Eval.expr e lsize
