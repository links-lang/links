let internal_error message = Errors.internal_error ~filename:"irtowasm.ml" ~message

module Builtins = struct
	open Utility
	
	let unops = StringMap.from_alist ["-", (Wasm.Type.(NumT I64T), None); "-.", (Wasm.Type.(NumT F64T), None)]
	let binops = StringMap.from_alist [
		"+",  (Wasm.Type.(NumT I64T), Wasm.Instruction.(Binop (Wasm.Value.I64 IntOp.Add)));
		"+.", (Wasm.Type.(NumT F64T), Wasm.Instruction.(Binop (Wasm.Value.F64 FloatOp.Add)));
		"-",  (Wasm.Type.(NumT I64T), Wasm.Instruction.(Binop (Wasm.Value.I64 IntOp.Sub)));
		"-.", (Wasm.Type.(NumT F64T), Wasm.Instruction.(Binop (Wasm.Value.F64 FloatOp.Sub)));
		"*",  (Wasm.Type.(NumT I64T), Wasm.Instruction.(Binop (Wasm.Value.I64 IntOp.Mul)));
		"*.", (Wasm.Type.(NumT F64T), Wasm.Instruction.(Binop (Wasm.Value.F64 FloatOp.Mul)));
		"/",  (Wasm.Type.(NumT I64T), Wasm.Instruction.(Binop (Wasm.Value.I64 IntOp.DivS)));
		"/.", (Wasm.Type.(NumT F64T), Wasm.Instruction.(Binop (Wasm.Value.F64 FloatOp.Div)));
		"%",  (Wasm.Type.(NumT I64T), Wasm.Instruction.(Binop (Wasm.Value.I64 IntOp.RemS)));
	]
	
	let is op = StringMap.mem op unops || StringMap.mem op binops || String.equal op "=="
	let compile op args =
		if String.equal op "==" then begin Wasm.Type.(NumT I32T), match args with
			| [Wasm.Type.(NumT I32T), arg1; Wasm.Type.(NumT I32T), arg2] -> arg1 @ arg2 @ Wasm.Instruction.[Relop (Wasm.Value.I32 IntOp.Eq)]
			| [Wasm.Type.(NumT I64T), arg1; Wasm.Type.(NumT I64T), arg2] -> arg1 @ arg2 @ Wasm.Instruction.[Relop (Wasm.Value.I64 IntOp.Eq)]
			| [Wasm.Type.(NumT F32T), arg1; Wasm.Type.(NumT F32T), arg2] -> arg1 @ arg2 @ Wasm.Instruction.[Relop (Wasm.Value.F32 FloatOp.Eq)]
			| [Wasm.Type.(NumT F64T), arg1; Wasm.Type.(NumT F64T), arg2] -> arg1 @ arg2 @ Wasm.Instruction.[Relop (Wasm.Value.F64 FloatOp.Eq)]
			| [_; _] -> failwith "TODO: generic Builtins.compile '=='"
			| _ -> raise (internal_error (Printf.sprintf "Unrecognized primitive operation '==' with arity %d\n" (List.length args)))
		end else match args with
		| [_, arg1] ->
			if String.equal op "-" then begin
				let open Wasm.Instruction in
				let open Wasm.Value in
				Wasm.Type.NumT Wasm.Type.I64T, Const (I64 (I64.of_int_s 0)) :: arg1 @ [Binop (I64 IntOp.Sub)]
			end else let opt, op = StringMap.find op unops in opt, arg1 @ [val_of op]
		| [_, arg1; _, arg2] -> let opt, op = StringMap.find op binops in opt, arg1 @ arg2 @ [op]
		| _ -> raise (internal_error (Printf.sprintf "Unrecognised primitive operation '%s' with arity %d\n" op (List.length args)))
end

let rec _convert_type map_cons map_fun map_row (t : Types.typ) = match t with
	| Types.Primitive CommonTypes.Primitive.Bool -> map_cons Wasm.Type.(NumT I32T)
	| Types.Primitive CommonTypes.Primitive.Int -> map_cons Wasm.Type.(NumT I64T)
	| Types.Primitive CommonTypes.Primitive.Float -> map_cons Wasm.Type.(NumT F64T)
	| Types.Record t -> _convert_type map_cons map_fun map_row t
	| Types.Function (args, eff, ret) -> map_fun args eff ret
	| Types.Row (fsm, mrv, b) -> map_row fsm mrv b
	| Types.Present t -> _convert_type map_cons map_fun map_row t
	| Types.Meta t -> _convert_type map_cons map_fun map_row (Unionfind.find t)
	| Types.ForAll (_, t) -> _convert_type map_cons map_fun map_row t (* Ignore polymorphism *)
  | Types.Not_typed -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Not_typed@\n%a" Types.pp_datatype t))
  | Types.Var _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Var@\n%a" Types.pp_datatype t))
  | Types.Recursive _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Recursive@\n%a" Types.pp_datatype t))
  | Types.Alias _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Alias@\n%a" Types.pp_datatype t))
  | Types.Application _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Application@\n%a" Types.pp_datatype t))
  | Types.RecursiveApplication _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type RecursiveApplication@\n%a" Types.pp_datatype t))
  | Types.Primitive _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Primitive@\n%a" Types.pp_datatype t))
  | Types.Lolli _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Lolli@\n%a" Types.pp_datatype t))
  | Types.Variant _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Variant@\n%a" Types.pp_datatype t))
  | Types.Table _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Table@\n%a" Types.pp_datatype t))
  | Types.Lens _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Lens@\n%a" Types.pp_datatype t))
  | Types.Effect _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Effect@\n%a" Types.pp_datatype t))
  | Types.Operation _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Operation@\n%a" Types.pp_datatype t))
  | Types.Closed -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Closed@\n%a" Types.pp_datatype t))
  | Types.Absent -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Absent@\n%a" Types.pp_datatype t))
  | Types.Input _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Input@\n%a" Types.pp_datatype t))
  | Types.Output _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Output@\n%a" Types.pp_datatype t))
  | Types.Select _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Select@\n%a" Types.pp_datatype t))
  | Types.Choice _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Choice@\n%a" Types.pp_datatype t))
  | Types.Dual _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type Dual@\n%a" Types.pp_datatype t))
  | Types.End -> raise (internal_error (Format.asprintf "TODO: Irtowasm._convert_type End@\n%a" Types.pp_datatype t))
let convert_type t : Wasm.Type.val_type = _convert_type
	(fun v -> v)
	(fun _ _ _ -> Wasm.Type.(RefT (Null, FuncHT))) (* TODO: check this *)
	(fun _ _ _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm.convert_type Row@\n%a" Types.pp_datatype t)))
	t
let convert_types t : Wasm.Type.val_type list = _convert_type
	(fun v -> [v])
	(fun _ _ _ -> Wasm.Type.[RefT (Null, FuncHT)]) (* TODO: check this *)
	(fun fsm _ _ ->
		if Utility.StringMap.is_empty fsm then [] else failwith (Format.asprintf "TODO: Irtowasm.convert_types Row@\n%a@." Types.pp_datatype t)
		(* let types = fsm
			|> Utility.StringMap.bindings
			|> List.fast_sort (fun (s1, _) (s2, _) -> String.compare s1 s2)
			|> List.map (fun (_, v) -> Wasm.Type.(FieldT (Cons, ValStorageT (_convert_type v)))) in
		Wasm.Type.(RefT (NoNull, DefHT (DefT (RecT [SubT (Final, [], DefStructT (StructT types))], 0l)))) *))
	t
let default_value_of (t : Wasm.Type.val_type) : Wasm.Instruction.t list =
	let open Wasm.Type in
	let open Wasm.Instruction in
	match t with
	| NumT I32T -> [Const Wasm.Value.(I32 (I32.of_bits 0l))]
	| NumT I64T -> [Const Wasm.Value.(I64 (I64.of_bits 0L))]
	| NumT F32T -> [Const Wasm.Value.(F32 (F32.of_float 0.))]
	| NumT F64T -> [Const Wasm.Value.(F64 (F64.of_float 0.))]
	| RefT (Null, ht) -> [RefNull ht]
	| _ -> raise (internal_error (Format.asprintf "TODO: Irtowasm.default_value_of@\n%s" (Wasm.Type.string_of_val_type t)))

let sort_name_map (nm : 'a Ir.name_map) : (string * 'a) list =
	nm
		|> Utility.StringMap.bindings
		(* Since bindings are unique, there is no issue using an unstable sort *)
		|> List.fast_sort (fun (s1, _) (s2, _) -> String.compare s1 s2)

module LEnv : sig
	type t
	val empty : string Env.Int.t -> (Wasm.Type.val_type * string) Env.Int.t -> t
	
	type restorer
	val new_func : t -> restorer * t
	val add_param : t -> Var.var -> Wasm.Type.val_type -> t
	val restore : restorer -> t -> t
	
	type var_info = int32 * Wasm.Type.val_type
	type v =
		| LocalVar of var_info
		| GlobalVar of var_info
		| Function of {
			fn_name: string;
			fn_idx: int32;
			fn_typeidx: int32;
			fn_clostypeidx: int32 option;
			fn_ret: Wasm.Type.val_type;
		}
		| Var of string
	val get_env : t -> Var.var -> v
	val is_global : t -> Var.var -> int32 option
	val set_global : t -> Var.var -> Wasm.Instruction.t list -> t
	val add_local : t -> Var.var -> Wasm.Type.val_type -> int32 * t
	val add_fun : t -> int32 -> int32 option -> (Var.var * string) option -> int option -> Wasm.Type.val_type -> int32 * t
	val add_funtype : t -> Wasm.Type.func_type -> int32 * int32 option * t
	val add_raw_funtype : t -> Wasm.Type.func_type -> int32 * t
	val add_closure : t -> Ir.binder -> int32 * int32 * t
	
	val get_return_of_closure : t -> int32 -> Wasm.Type.val_type * int32
	val add_func_ref : t -> int32 -> t
	
	type clval = Normal of Wasm.Type.val_type | Closure of int32 * Wasm.Type.val_type
	type closure = (int32 * clval) Utility.StringMap.t
	val get_closure_from_var : t -> Var.var -> int32 * int32 * closure
	val get_closure_from_fun : t -> Var.var -> int32 * int32 * closure
	
	val get_locals : t -> Wasm.Type.val_type list
	val to_wasm : t -> Wasm.fundef list -> Wasm.raw_code -> Wasm.module_ (* Functions must be given in reverse order *)
	
	val type_emptyclosure : int32 (* (struct) *)
	(* val type_funclosure : int32   (* (struct (field (ref func)) (field (ref eq))) *) *)
	val type_nothingfunc : int32  (* (func) *)
end = struct
	module Locals : sig
		type t
		
		val empty : t
		val new_func : t
		val add_param : t -> int32 * t
		val add_local : Wasm.Type.val_type -> t -> int32 * t
		val to_wasm : t -> Wasm.Type.val_type list
	end = struct
		type t = Wasm.Type.val_type list * int32
		
		let empty : t = [], 0l
		let new_func : t = [], 1l
		let add_param (env : t) : int32 * t =
			let ls, len = env in
			len, (ls, Int32.succ len)
		let add_local (lt : Wasm.Type.val_type) (env : t) : int32 * t =
			let ls, len = env in
			len, (lt :: ls, Int32.succ len)
		let to_wasm (v, _) = List.rev v
	end
	type locals = Locals.t
	
	type var_info = int32 * Wasm.Type.val_type
	type v =
		| LocalVar of var_info
		| GlobalVar of var_info
		| Function of {
			fn_name: string;
			fn_idx: int32;
			fn_typeidx: int32;
			fn_clostypeidx: int32 option;
			fn_ret: Wasm.Type.val_type;
		}
		| Var of string
	type clval = Normal of Wasm.Type.val_type | Closure of int32 * Wasm.Type.val_type
	type closure = (int32 * clval) Utility.StringMap.t
	type t = {
		locs: locals;
		nfuns: int32;
		venv: v Env.Int.t; (* Variable ENVironment *)
		do_global: Utility.IntSet.t;
		gbs: (Wasm.Type.val_type * int32 * Wasm.Instruction.t list option ref * string) Env.Int.t; (* GlobalBindingS *)
		ntypes: int32;
		types: Wasm.Type.rec_type list;
		closures: (int32 * int32 * closure) Env.Int.t;
		clidx_of_funidx : Env.Int.name Env.Int.t;
	}
	
	let empty (genv: string Env.Int.t) (gbs: (Wasm.Type.val_type * string) Env.Int.t) : t =
		let gbs = fst (Env.Int.fold (fun v (t, n) (acc, idx) -> Env.Int.bind v (t, idx, ref None, n) acc, Int32.succ idx) gbs (Env.Int.empty, 0l)) in
		{
			locs = Locals.empty;
			nfuns = 0l;
			venv = Env.Int.extend (* The second argument overrides the first in case of conflict *)
				(Env.Int.map (fun v -> Var v) genv)
				(Env.Int.map (fun (vt, idx, _, _) -> GlobalVar (idx, vt)) gbs);
			do_global = Utility.IntSet.empty;
			gbs;
			ntypes = 2l;
			types = Wasm.Type.[
				RecT [SubT (Final, [], DefFuncT (FuncT ([], [])))];
				(* RecT [SubT (Final, [], DefStructT (StructT [
					FieldT (Cons, ValStorageT (RefT (NoNull, FuncHT)));
					FieldT (Cons, ValStorageT (RefT (NoNull, EqHT)));
				]))]; *)
				RecT [SubT (Final, [], DefStructT (StructT []))];
			];
			closures = Env.Int.empty;
			clidx_of_funidx = Env.Int.empty;
		}
	
	let type_emptyclosure : int32 = 0l
	(* let type_funclosure : int32 = 1l *)
	let type_nothingfunc : int32 = 1l
	
	type restorer = locals
	let new_func (env : t) : restorer * t =
		env.locs, { env with locs = Locals.new_func }
	let add_param (env : t) (v : Var.var) (vt : Wasm.Type.val_type) : t =
		let i, locs = Locals.add_param env.locs in
		let venv = Env.Int.bind v (LocalVar (i, vt)) env.venv in
		{ env with locs; venv }
	let restore (rs : restorer) (env : t) : t = { env with locs = rs }
	
	let get_env ({ venv; _ }: t) (v: Var.var) : v =
		Env.Int.find v venv
	
	let is_global ({ gbs; _ }: t) (v: Var.var) : int32 option = match Env.Int.find_opt v gbs with
		| Some (_, i, _, _) -> Some i
		| None -> None
	(* Actually updates the environment in-place, which is fine in the context of global initialization *)
	let set_global ({ gbs; _ } as env: t) (v: Var.var) (i: Wasm.Instruction.t list) : t =
		let _, _, roi, _ = Env.Int.find v gbs in
		roi := Some i;
		env
	
	let add_local ({ locs; venv; _ } as env: t) (v: Var.var) (lt : Wasm.Type.val_type) : int32 * t =
		let i, locs = Locals.add_local lt locs in
		i, { env with locs; venv = Env.Int.bind v (LocalVar (i, lt)) venv }
	let add_fun ({ nfuns; venv; _ } as env: t) (fn_typeidx: int32) (fn_clostypeidx: int32 option) (ovn: (Var.var * string) option)
	            (obind: int option) (fn_ret : Wasm.Type.val_type) : int32 * t =
		nfuns, { env with
			nfuns = Int32.succ nfuns;
			venv = (match ovn with
				| Some (v, fn_name) ->
					Env.Int.bind v
						(Function {fn_name; fn_idx = nfuns; fn_typeidx; fn_clostypeidx; fn_ret})
						venv
				| None -> venv);
			clidx_of_funidx = (match obind with Some bidx -> Env.Int.bind (Int32.to_int nfuns) bidx env.clidx_of_funidx
			                     | None -> env.clidx_of_funidx);
		}
	
	let add_type ({ ntypes; types; _ } as env: t) (rt: Wasm.Type.rec_type): int32 * t =
		ntypes, { env with ntypes = Int32.succ ntypes; types = rt :: types }
	let add_raw_funtype (env: t) (ft: Wasm.Type.func_type): int32 * t =
		add_type env Wasm.Type.(RecT [SubT (Final, [], DefFuncT ft)])
	let add_funtype (env: t) (ft: Wasm.Type.func_type): int32 * int32 option * t =
		let ftidx, env = add_raw_funtype env ft in
		match ft with
		| Wasm.Type.(FuncT (RefT (NoNull, EqHT) :: _, _)) ->
			(* Also add the closure type for this function type *)
			let clidx, env = add_type env Wasm.Type.(
				RecT [SubT (Final, [], DefStructT (StructT
					[FieldT (Cons, ValStorageT (RefT (NoNull, VarHT (StatX ftidx))));
					 FieldT (Cons, ValStorageT (RefT (NoNull, EqHT)))]))]) in
			ftidx, Some clidx, env
		| _ -> ftidx, None, env
	
	let get_type_idx (env: t) (tidx: int32) : Wasm.Type.rec_type = List.nth env.types (Int32.to_int (Int32.sub env.ntypes tidx) - 1)
	let get_return_of_closure (env: t) (tidx: int32) : Wasm.Type.val_type * int32 = match get_type_idx env tidx with
		| Wasm.Type.(RecT [SubT (Final, [], DefStructT (StructT
			[FieldT (Cons, ValStorageT (RefT (NoNull, VarHT (StatX ftidx))));
			 FieldT (Cons, ValStorageT (RefT (NoNull, EqHT)))]))]) -> begin match get_type_idx env ftidx with
			| Wasm.Type.(RecT [SubT (Final, [], DefFuncT (FuncT (_, [rt])))]) -> rt, ftidx
			| _ -> raise (internal_error "get_return_of_closure called with a type ID which does not correspond to a closure")
			end
		| _ -> raise (internal_error "get_return_of_closure called with a type ID which does not correspond to a closure")
	let add_func_ref (env: t) (fidx: int32) : t = { env with do_global = Utility.IntSet.add (Int32.to_int fidx) env.do_global }
	
	let convert_type_clos env t : t * clval = _convert_type (fun v -> env, Normal v) (fun args eff ret ->
		let args = convert_types args
		in let _eff = match eff with
			| Types.Row (fsm, _, _) when Utility.StringMap.is_empty fsm -> ()
			| _ -> failwith "TODO: convert_type_clos with effect"
		in
		let ret = convert_type ret in
		let fidx, env = add_raw_funtype env (Wasm.Type.FuncT (Wasm.Type.(RefT (NoNull, EqHT)) :: args, [ret])) in
		env, Closure (fidx, ret)) (fun _ _ _ -> raise (internal_error "convert_type_clos called on non-closure type")) t
	let add_closure (env: t) (cl: Ir.binder): int32 * int32 * t =
		let t = Var.info_type (Var.info_of_binder cl) in
		let env, wasmtype, clmap =
			let convert_field_spec_map env fsm =
				let env, types, map =
					let rec inner env l acctypes accmap i = match l with
						| [] -> env, List.rev acctypes, accmap
						| (n, v) :: tl ->
							let env, cv = convert_type_clos env v in
							match cv with
							| Normal vt ->
								inner env tl
									(Wasm.Type.(FieldT (Cons, ValStorageT vt)) :: acctypes)
									(Utility.StringMap.add n (i, cv) accmap)
									(Int32.succ i)
							| Closure (tidx, _) ->
								inner env tl
									(Wasm.Type.(FieldT (Cons, ValStorageT (RefT (NoNull, EqHT)))) ::
									 Wasm.Type.(FieldT (Cons, ValStorageT (RefT (NoNull, VarHT (StatX tidx))))) :: acctypes)
									(Utility.StringMap.add n (i, cv) accmap)
									(Int32.add i 2l)
					in inner env (sort_name_map fsm) [] Utility.StringMap.empty 0l in
					(* sort_name_map fsm
					|> List.mapi (fun i (n, v) ->
						let vts = convert_type_clos v in
						List.map (fun vt -> Wasm.Type.(FieldT (Cons, ValStorageT vt))) vts, (n, (Int32.of_int i, vts)))
					|> List.split in
				let types = List.flatten types in
				let map = Utility.StringMap.of_list map in *)
				env, Wasm.Type.StructT types, map
			in match t with
				| Types.(Record (Row (fsm, _, _))) -> convert_field_spec_map env fsm
				| _ -> raise (internal_error "unexpected closure type")
		in let tidx, env = add_type env Wasm.Type.(RecT [SubT (Final, [], DefStructT wasmtype)])
		in let vidx, env = add_local env (Var.var_of_binder cl) Wasm.Type.(RefT (NoNull, VarHT (StatX tidx)))
		in let env = { env with closures = Env.Int.bind (Var.var_of_binder cl) (tidx, vidx, clmap) env.closures }
		in tidx, vidx, env
	let get_closure_from_var (env: t) (v: Var.var) : int32 * int32 * closure =
		Env.Int.find v env.closures
	let get_closure_from_fun (env: t) (v: Var.var) : int32 * int32 * closure =
		Env.Int.find (Env.Int.find v env.clidx_of_funidx) env.closures
	
	let startup_name = Some "_init"
	let function_of_startup ((_, locals, instrs): Wasm.raw_code)
			(initc : Wasm.Instruction.t list) (vinitidx : int32) : Wasm.fundef =
		let initc = initc @ Wasm.Instruction.[GlobalSet vinitidx] in
		Wasm.{
			fn_name = startup_name;
			fn_type = type_nothingfunc;
			fn_locals = locals;
			fn_code = instrs @ initc;
		}
	
	let get_locals ({ locs; _ }: t) : Wasm.Type.val_type list = Locals.to_wasm locs
	let to_wasm (env: t) (funs : Wasm.fundef list) (init : Wasm.raw_code) : Wasm.module_ =
		let initrt, initc, initdef, env =
			let (rets, _, _) = init in
			let initrt, initc, initdef, env = rets, [], default_value_of rets, env
				(* | _ ->
					let rinitidx, env =
						add_type
							env
							Wasm.Type.(RecT [
								SubT (Final, [],
									DefStructT (StructT (List.map (fun t -> FieldT (Cons, ValStorageT t)) rets)))
								]
							) in
					let ht = Wasm.Type.(VarHT (StatX rinitidx)) in
					Wasm.Type.(RefT (Null, ht)), Wasm.Instruction.[StructNew (rinitidx, Explicit)], Wasm.Instruction.[RefNull ht], env *)
			in initrt, initc, initdef, env in
		let { nfuns; gbs; types; do_global; _ } = env in
		let globals = Env.Int.bindings gbs in
		let nglobals = List.length globals in
		let globals =
			let default_globals =
				Utility.IntSet.fold
					(fun i acc -> (Wasm.Type.(GlobalT (Cons, RefT (NoNull, FuncHT))), Wasm.Instruction.[RefFunc (Int32.of_int i)], None) :: acc)
					do_global []
			in let rec inner i acc globals =
				if i = 0l then acc
				else let i = Int32.pred i in
					let rec find globals acc = match globals with
						| [] -> raise (internal_error ("failed to find constant number " ^ Int32.to_string i))
						| ((_, (vt, idx, oinit, name)) as hd) :: tl ->
							if Int32.equal idx i then match !oinit with
								| Some init -> (Wasm.Type.(GlobalT (Cons, vt)), init, Some name), List.rev_append acc tl
								| None -> (Wasm.Type.(GlobalT (Var, vt)), default_value_of vt, Some name), List.rev_append acc tl
							else find tl (hd :: acc)
					in let v, globals = find globals [] in inner i (v :: acc) globals
			in let default_globals = (Wasm.Type.(GlobalT (Var, initrt)), initdef, Some "_main_result") :: default_globals in
			inner (Int32.of_int nglobals) default_globals globals
		in
		let init = function_of_startup init initc (Int32.of_int nglobals) in
		(* TODO: suppress startup code if it is empty *)
		Wasm.{
			types = List.rev types;
			globals = globals;
			funs = List.rev (init :: funs);
			init = Some nfuns;
		}
end
type env = LEnv.t

let rec skip_toplevel_poly v = let open Ir in match v with
	| TAbs (_, v) | TApp (v, _) -> skip_toplevel_poly v
	| _ -> v

let compile_const (c : CommonTypes.Constant.t) : Wasm.Type.num_type * Wasm.Value.num =
	let open CommonTypes.Constant in let open Wasm.Value in
	match c with
	| Float f -> Wasm.Type.F64T, F64 (F64.of_float f)
	| Int i -> Wasm.Type.I64T, I64 (I64.of_int_s i)
	| Bool b -> Wasm.Type.I32T, I32 (if b then I32.one else I32.zero)
	| String _ -> failwith "TODO: compile_const String"
	| Char c -> Wasm.Type.I32T, I32 (I32.of_int_u (Char.code c))
	| DateTime _ -> failwith "TODO: compile_const DateTime"

let compile_var (env : env) (v : Var.var) =
	let open Wasm.Instruction in
	let open LEnv in
	match get_env env v with
	| LocalVar (i, lt) -> env, lt, [LocalGet i]
	| GlobalVar (i, lt) -> env, lt, [GlobalGet i]
	| Function {fn_name = name; _} -> failwith ("TODO: compile_var function " ^ name)
	| Var name -> failwith ("TODO: compile_var global " ^ name)

let rec compile_value (env : env) ~(split_funclos (* = false *)) v : env * Wasm.Type.val_type * Wasm.Instruction.t list =
	let open Ir in let open Wasm.Instruction in
	match v with
	| Constant c -> let t, c = compile_const c in env, Wasm.Type.NumT t, [Const c]
	| Variable v -> compile_var env v
	| Extend _ -> failwith "TODO: compile_value Extend"
	| Project (id, v) -> begin match v with
		| Variable v ->
			let cltidx, clidx, clmap = LEnv.get_closure_from_var env v in
			let idx, t = Utility.StringMap.find id clmap in begin
			match t with
			| LEnv.Normal t ->
				env, t, [LocalGet clidx; StructGet (cltidx, idx, None)]
			| LEnv.Closure (tidx, _) ->
				env, Wasm.Type.(RefT (NoNull, VarHT (StatX tidx))),
					[LocalGet clidx; StructGet (cltidx, idx, None);
					 LocalGet clidx; StructGet (cltidx, Int32.succ idx, None)]
			end
		| _ -> failwith "TODO: compile_value Project with no Variable"
		end
	| Erase _ -> failwith "TODO: compile_value Erase"
	| Inject _ -> failwith "TODO: compile_value Inject"
	| TAbs (_, v)
	| TApp (v, _) -> compile_value env ~split_funclos v
	| XmlNode _ -> failwith "TODO: compile_value XmlNode"
	| ApplyPure (op, args) -> begin
		let op = skip_toplevel_poly op in
		let args = List.map (fun v -> let _, t, v = compile_value env ~split_funclos:false v in t, v) args in
		match op with
		| Variable v -> begin match LEnv.get_env env v with
			| LEnv.LocalVar (i, _lt) -> failwith ("TODO: compile_value ApplyPure local variable " ^ (Int32.to_string i))
			| LEnv.GlobalVar (i, _lt) -> failwith ("TODO: compile_value ApplyPure global variable " ^ (Int32.to_string i))
			| LEnv.Function {fn_name = name; _} -> failwith ("TODO: compile_value ApplyPure function " ^ name)
			| LEnv.Var name ->
				if Builtins.is name then let r, is = Builtins.compile name args in env, r, is
				else failwith ("TODO: compile_value ApplyPure non-builtin variable " ^ name)
			end
		| _ ->
			let env, ts, hd = compile_value env ~split_funclos op in
			env, ts, hd @ List.concat_map snd args @ [failwith "TODO: compile_value non-variable ApplyPure"]
		end
	| Closure (f, _, clos) -> begin match LEnv.get_env env f with
			| LEnv.LocalVar (i, _lt) -> failwith ("TODO: compile_value Closure local variable " ^ (Int32.to_string i))
			| LEnv.GlobalVar (i, _lt) -> failwith ("TODO: compile_value Closure global variable " ^ (Int32.to_string i))
			| LEnv.Function {fn_idx = fidx; fn_clostypeidx = Some fctidx; _} ->
				let env, cc = compile_closure env fidx clos in
				let env = LEnv.add_func_ref env fidx in
				env, Wasm.Type.(RefT (NoNull, VarHT (StatX fctidx))),
					if split_funclos then
					     Wasm.Instruction.(RefFunc fidx :: cc)
					else Wasm.Instruction.(RefFunc fidx :: cc @ [StructNew (fctidx, Explicit)])
			| LEnv.Function {fn_name = name; fn_clostypeidx = None; _} ->
				failwith ("TODO: compile_value Closure function w/o closure " ^ name)
			| LEnv.Var name ->
				failwith ("TODO: compile_value Closure maybe-builtin variable " ^ name)
			end
	| Coerce (v, _) -> compile_value env ~split_funclos v (* Assume coercion never needs to occur, which is unfortunately unlikely *)

and compile_closure (env : env) (funid : int32) (closure : Ir.value) : env * Wasm.Instruction.t list =
	let cltidx, _, _ = LEnv.get_closure_from_fun env (Int32.to_int funid) in
	match closure with
	| Ir.Extend (vs, None) ->
		let env = ref env in
		let vs = sort_name_map vs |> List.concat_map (fun (_, v) -> let nenv, _, v = compile_value !env ~split_funclos:true v in env := nenv; v) in
		let env = !env in
		env, vs @ Wasm.Instruction.[StructNew (cltidx, Explicit)]
	| Ir.Extend (_, Some _) -> failwith "TODO: compile_closure with Extend (_, Some _)"
	| _ -> failwith "TODO: compile_closure without Extend"

(* Locals are invalid, functions are in reverse order *)
let rec compile_tail_computation (code : Ir.tail_computation) (is_toplevel : bool) (env : env) : Wasm.fundef list * Wasm.raw_code * env =
	let open Ir in match code with
	| Return v -> let env, t, v = compile_value env ~split_funclos:false v in [], (t, [], v), env
	| Apply (op, args) -> begin
		let op = skip_toplevel_poly op in
		let args = List.map (fun v -> let _, t, v = compile_value env ~split_funclos:true v in t, v) args in
		match op with
		| Variable v -> begin match LEnv.get_env env v with
			| LEnv.LocalVar (i, lt) ->
				let realt = match lt with
					| Wasm.Type.(RefT (_, VarHT (StatX i))) -> i
					| _ -> raise (internal_error "invalid Apply of LocalVar Variable")
				in
				let ret, ftidx = LEnv.get_return_of_closure env realt in
				[], (ret, [], Wasm.Instruction.[LocalGet i; StructGet (realt, 1l, None); LocalGet i; StructGet (realt, 0l, None);
					(if is_toplevel then CallRef ftidx else ReturnCallRef ftidx)]), env
			| LEnv.GlobalVar (i, _lt) -> failwith ("TODO: compile_tail_computation Apply global variable " ^ (Int32.to_string i))
			| LEnv.Function {fn_idx = funid; fn_ret = ret; _} ->
				(* Load any closure then proceed with the arguments and function call *)
				let load_closure =
					if is_toplevel then Wasm.Instruction.(StructNew (LEnv.type_emptyclosure, Explicit))
					else Wasm.Instruction.LocalGet 0l
				in [], (ret, [], load_closure :: List.concat_map snd args @ [Wasm.Instruction.Call funid]), env
			| LEnv.Var name ->
				if Builtins.is name then
					let rt, c = Builtins.compile name args in
					[], (rt, [], c), env
				else failwith ("TODO: compile_tail_computation Apply non-builtin variable " ^ name ^ " (maybe invalid)")
			end
		| Closure (v, _, cl) -> begin match LEnv.get_env env v with
			| LEnv.LocalVar _ -> failwith "TODO: compile_tail_computation Apply Closure LocalVar"
			| LEnv.GlobalVar _ -> failwith "TODO: compile_tail_computation Apply Closure GlobalVar"
			| LEnv.Function {fn_idx = funid; fn_ret = ret; _} ->
				let env, start = compile_closure env funid cl in
				[], (ret, [], start @ List.concat_map snd args @ [Wasm.Instruction.Call funid]), env
			| LEnv.Var _ -> failwith "TODO: compile_tail_computation Apply Closure Var"
			end
		| Project (id, v) -> begin match v with
			| Variable v ->
				let cltidx, clidx, clmap = LEnv.get_closure_from_var env v in
				let idx, t = Utility.StringMap.find id clmap in begin match t with
				| LEnv.Normal _ -> raise (internal_error "invalid Apply Project: member is not a closure")
				| LEnv.Closure (tidx, tret) ->
					let open Wasm.Instruction in
					[], (tret, [],
						[LocalGet clidx; StructGet (cltidx, Int32.succ idx, None);
						 LocalGet clidx; StructGet (cltidx, idx, None);
						 (if is_toplevel then CallRef tidx else ReturnCallRef tidx)]), env
				(* ignore (cltidx, clidx, idx); failwith "TODO: compile_tail_computation Apply Project Variable" *)
				(* t, Wasm.Instruction.[LocalGet clidx; StructGet (cltidx, idx, None)] *)
				end
			| _ -> failwith "TODO: compile_tail_computation Apply Project with no Variable"
			end
		| _ ->
			let env, ts, hd = compile_value env ~split_funclos:false op in
			[], (ts, [], hd @ List.concat_map snd args @ [failwith "TODO: compile_tail_computation non-variable Apply"]), env
		end
	| Special (DoOperation (cons, _, _)) -> failwith ("TODO: compile_tail_computation Special Do " ^ cons)
	| Special _ -> failwith "TODO: compile_tail_computation Special"
	| Case _ -> failwith "TODO: compile_tail_computation Case"
	| If (v, t, f) ->
		let funst, (ret, _, codet), env = compile_program false t env in
		let funsf, (_,   _, codef), env = compile_program false f env in
		let env, _, vc = compile_value env ~split_funclos:false v in
		funsf @ funst, (ret, [], Wasm.Instruction.(vc @ [If (Wasm.Type.ValBlockType (Some ret), codet, codef)])), env

(* Returns the functions in reverse order *)
and compile_program (is_toplevel : bool) ((bindings, code) : Ir.program) (env : env) : Wasm.fundef list * Wasm.raw_code * env =
	let open Ir in
	let rec compile_bindings bindings env acc_funs acc_code = match bindings with
		| [] -> acc_funs, List.fold_left (fun acc v -> v @ acc) [] acc_code, env
		| Let (i, (tvl, c)) :: bs ->
			assert (tvl = []);
			let vi = Var.var_of_binder i in begin
			match LEnv.is_global env vi with
			| Some idx -> begin match c with
				(* TODO: add some cases for constant constructions *)
				| Return (Constant c) ->
					let _, c = compile_const c in
					let env = LEnv.set_global env vi [Wasm.Instruction.Const c] in
					compile_bindings bs env acc_funs acc_code
				| _ ->
					let funs, (_, _, c), env = compile_tail_computation c is_toplevel env in
					compile_bindings bs env (funs @ acc_funs) ([Wasm.Instruction.GlobalSet idx] :: c :: acc_code)
				end
			| None ->
				let funs, (lvt, _, c), env = compile_tail_computation c is_toplevel env in
				let j, env = LEnv.add_local env vi lvt in
				compile_bindings bs env (funs @ acc_funs) ([Wasm.Instruction.LocalSet j] :: c :: acc_code)
			end
		| Fun fd :: bs ->
			(* let fname = Var.name_of_binder fd.fn_binder in
			let fname = if fname = "" then Js.name_binder fd.fn_binder else fname in *)
			let fname = Js.name_binder fd.fn_binder in
			(* Initialize the environment *)
			let rs, env = LEnv.new_func env in
			let rec bind_args env (nargs, args) vars = match vars with
				| [] -> nargs, List.rev args, env
				| var :: vars ->
					let vid = Var.var_of_binder var in
					let vt = convert_type (Var.type_of_binder var) in
					let env = LEnv.add_param env vid vt in
					bind_args env (Int32.succ nargs, vt :: args) vars
			in let nargs, args, env = bind_args env (0l, []) fd.fn_params in
			(* Done, env is now valid and new locals can be added to it *)
			let clargs = Wasm.Type.(RefT (NoNull, EqHT)) :: args in
			let begcode, env, oclidx = match fd.fn_closure with
				| None -> [], env, None
				| Some cl ->
					let closidx, locidx, env = LEnv.add_closure env cl in
					Wasm.Instruction.[LocalGet 0l; RefCast Wasm.Type.(NoNull, VarHT (StatX closidx)); LocalSet locidx], env, Some (Var.var_of_binder cl)
			in
			let auxfs, (rets, locs, c), env = compile_program false fd.fn_body env in
			let env = LEnv.restore rs env in
			let funtyp, oclos, env = LEnv.add_funtype env (Wasm.Type.FuncT (clargs, [rets])) in
			let f : Wasm.fundef = Wasm.{
				fn_name = None;
				fn_type = funtyp;
				fn_locals = locs;
				fn_code = begcode @ c
			} in
			let lowlevelidx, env = LEnv.add_fun env funtyp oclos (Some (Var.var_of_binder fd.fn_binder, fname)) oclidx rets in
			let auxfs = f :: auxfs in
			let auxfs, env =
				let do_export = match fd.fn_closure with
					| None -> is_toplevel
					| Some _ -> false
				in
				if do_export then
					let funtyp, oclos, env = LEnv.add_funtype env (Wasm.Type.FuncT (args, [rets])) in
					let f = Wasm.{
						fn_name = Some fname;
						fn_type = funtyp;
						fn_locals = [];
						fn_code =
							let rec inner nargs acc =
								if Int32.equal nargs 0l then acc
								else
									let nargs = Int32.pred nargs in
									inner nargs (Wasm.Instruction.LocalGet nargs :: acc)
							in let remaining_code = inner nargs Wasm.Instruction.[ReturnCall lowlevelidx] in
							Wasm.Instruction.(StructNew (LEnv.type_emptyclosure, Explicit)) :: remaining_code
					} in let _, env = LEnv.add_fun env funtyp oclos None None rets in f :: auxfs, env
				else auxfs, env
			in compile_bindings bs env (auxfs @ acc_funs) acc_code
		| Rec _ :: _ -> failwith "TODO: compile_program binding Rec"
		| Alien _ :: _ -> failwith "TODO: compile_program binding Alien"
		| Module _ :: _ -> failwith "TODO: compile_program binding Module"
	in let funs, curr_comp, env = compile_bindings bindings env [] [] in
	let funs2, (rets, _, code), env = compile_tail_computation code is_toplevel env in
	funs2 @ funs, (rets, LEnv.get_locals env, curr_comp @ code), env

let find_global_binders ((bindings, _) : Ir.program) : (Wasm.Type.val_type * string) Env.Int.t =
	let rec inner bindings acc = let open Ir in match bindings with
		| [] ->
			Env.String.fold
				(fun _ b acc -> Env.Int.bind (Var.var_of_binder b) (convert_type (Var.type_of_binder b), Var.name_of_binder b) acc)
				acc Env.Int.empty
		| Let (b, (_, _)) :: bs ->
			let name = Var.name_of_binder b in
			inner bs (if name = "" then acc else Env.String.bind name b acc)
		| Fun _ :: bs -> inner bs acc
		| Rec _ :: bs -> inner bs acc
		| Alien _ :: bs -> inner bs acc
		| Module _ :: bs -> inner bs acc
	in inner bindings Env.String.empty

let compile (prog : Ir.program) (env : string Env.Int.t) : Wasm.module_ =
	let global_binders = find_global_binders prog in
	let env = LEnv.empty env global_binders in
	let funs, init, env = compile_program true prog env in
	LEnv.to_wasm env funs init
