(*pp deriving *)

open Utility

let complete_tyenv tyenv p =
    let _, _, o = (new Ir.Transform.visitor tyenv)#computation p in
        o#get_type_environment

let prelude_primitive_names = 
    ["concatMap"; "map"; "sortByBase"; "asList"; "zip"; "unzip"; 
     "select"; "groupByBase"; "sum"; "concat"; "and"; "or"; 
     "max"; "min"; "avg"; "takeWhile"; "dropWhile"; "nubBase";
     "reverse"]

let prelude_primitive_vars = ref None
let prelude_primitive_namemap = ref None

let prelude_primitives () =
  match !Lib.prelude_nenv with
    | Some nenv ->
	let vars, name_map = 
	  List.fold_left 
	    (fun (s, m) n -> 
	       let v = Env.String.lookup nenv n in
		 (IntSet.add v s,
		  IntMap.add v n m))
	    (IntSet.empty, IntMap.empty)
	    prelude_primitive_names
	in
	  prelude_primitive_vars := Some vars;
	  prelude_primitive_namemap := Some name_map
    | None -> assert false

type scope = Var.scope
  deriving (Show)

(* term variables *)
type var = Var.var
  deriving (Show, Eq, Hash, Typeable, Pickle, Dump)
type var_info = Var.var_info
  deriving (Show)
type binder = Var.binder
  deriving (Show)

(* type variables *)
type tyvar = Types.quantifier
  deriving (Show)
type tyarg = Types.type_arg
  deriving (Show)

type name = string
  deriving (Show)

type name_set = Utility.stringset
  deriving (Show)
type 'a name_map = 'a Utility.stringmap
  deriving (Show)

let var_of_binder (x, _) = x

type constant = Constant.constant
  deriving (Show)

type qr =
  [ `Constant of constant
  | `Variable of var
  | `Extend of qr name_map * qr option
  | `Project of name * qr
  | `Erase of name_set * qr
  | `Inject of name * qr * Types.datatype

  | `TApp of qr * tyarg list
  | `TAbs of tyvar list * qr
  
  | `Database of Value.database * string
  | `Table of Value.table
  | `List of qr list
  | `Apply of qr * qr list
  | `Case of qr * (binder * qr) name_map * (binder * qr) option
  | `If of qr * qr * qr
  
  | `Computation of binding list * qr

  | `Wrong of Types.datatype ]
and funct = binder * binder list * qr * tyvar list
and binding = 
  [ `Let of binder * tyvar list * qr
  | `PFun of binder * qr option
  | `Fun of funct ]
and env = qr Env.Int.t 
    deriving (Show)

let var_of_binding = 
  function
    | `Let ((name, _), _, _) 
    | `Fun ((name, _), _, _, _)
    | `PFun ((name, _), _) -> name

let rec computation (bs, tc) : qr =
  let bs = bindings bs in
  let e = tail_computation tc in
    `Computation (bs, e)

and bindings bs = List.map binding bs

and binding (b : Ir.binding) : binding =
  match b with
    | `Let (binder, (tyvars, tc)) ->
	`Let (binder, tyvars, tail_computation tc)
    | `Fun (binder, (tyvars, binders, body), _loc) ->
	(* FIXME: really have tyvars on `Fun AND binding? *)
	`Fun (binder, binders, computation body, tyvars)
    | _ -> failwith "foo"

and value v =
  match v with
    | `Constant c -> `Constant c
    | `Variable v -> `Variable v
    | `Extend (extend_fields, r) ->
	let extend_fields = StringMap.map value extend_fields in
	let r = opt_map value r in
	  `Extend (extend_fields, r)
    | `Project (label, r) -> `Project (label, value r)
    | `Erase (labels, r) -> `Erase (labels, value r)
    | `Inject (tag, v, t) -> `Inject (tag, value v, t)
    | `TAbs (tyvars, v) -> `TAbs (tyvars, value v)
    | `TApp (v, tyargs) -> `TApp (value v, tyargs)
    | `ApplyPure (f, args) -> `Apply (value f, List.map value args)
    | _ -> failwith "unsupported value"

and tail_computation tc =
  match tc with
    | `Return v -> value v
    | `Apply (f, args) -> `Apply (value f, List.map value args)
    | `Special s -> special s
    | `Case (v, cases, default) ->
	let v = value v in
	let case (binder, body) = (binder, computation body) in
	let cases = StringMap.map case cases in
	let default = opt_map case default in
	  `Case (v, cases, default)
    | `If (c, t, e) -> `If (value c, computation t, computation e)

and special s =
  match s with
    | `Wrong t -> `Wrong t
(*    | `Database
    | `Table
    | `Query *)
    | _ -> failwith "unsupported special"

let local_freevars tyenv xs comp =
  let bound_vars = IntSet.from_list xs in
  let freevars = Ir.FreeVars.computation tyenv bound_vars comp in
  let freevars = IntSet.diff freevars Lib.primitive_vars in
    IntSet.diff freevars (val_of !prelude_primitive_vars)

let restrict m s = IntMap.filter (fun k _ -> IntSet.mem k s) m

let rec qr_of_value t tyenv env : Value.t -> (qr * Types.datatype Env.Int.t) =
 function
   | `Bool b -> `Constant (`Bool b), tyenv
   | `Char c -> `Constant (`Char c), tyenv
   | `Float f -> `Constant (`Float f), tyenv
   | `Int i -> `Constant (`Int i), tyenv
   | `String s -> `Constant (`String s), tyenv
   | `Database (db, s) -> `Database (db, s), tyenv
   | `Table t -> `Table t, tyenv
   | `PrimitiveFunction (fs, _) -> 
       let name = Env.String.lookup Lib.nenv fs in
       let t = Env.Int.lookup tyenv name in
       let binder = (name, (t, fs, `Local)) in
       (* dispatch is added after defunctionalization *)
       let binding = `PFun (binder, None) in
	 (* FIXME: add TAbs if there are type variables *)
	 (* better: add tyvars to `PFun if the primitive type is quantified over *)
	 `Computation ([binding], `Variable name), tyenv
       
   | `RecFunction ([(f, _)], _, _, _) when IntSet.mem f (val_of !prelude_primitive_vars) ->
       let s = IntMap.find f (val_of !prelude_primitive_namemap) in
       let t = Env.Int.lookup tyenv f in
       let binder = (f, (t, s, `Local)) in
       (* dispatch is added after defunctionalization *)
       let binding = `PFun (binder, None) in
	 (* FIXME: add TAbs if there are type variables *)
	 `Computation ([binding], `Variable f), tyenv

   | `RecFunction ([(f, (xs, body))], locals, f', _scope) ->
       assert (f = f');
       (* Debug.f "qr_of_value tyenv %d" f; *)
       let t = Env.Int.lookup tyenv f in
       let tyvars = TypeUtils.quantifiers t in
       let arg_binders = List.map (fun (x, t) -> (x, (t, "", `Local))) (List.combine xs (TypeUtils.arg_types t)) in
       let freevars = local_freevars tyenv xs body in
       (* Debug.print ("closure freevars " ^ (Show.show IntSet.show_t freevars)); *)
       let bindings, new_names, tyenv = bindings_from_closure_env tyenv freevars env locals in
       let body = Ir.ReplaceVars.computation tyenv new_names body in
       let body = computation body in
       let tyenv = 
	 Env.Int.fold
	   (fun name t tyenv ->
	      match IntMap.lookup name new_names with
		| Some new_name -> 
		    let tyenv = Env.Int.bind tyenv (new_name, t) in
		      Env.Int.bind tyenv (name, t)
		| None -> Env.Int.bind tyenv (name, t))
	   tyenv
	   Env.Int.empty
       in
       let fun_binder, fun_var = Var.fresh_var (t, "", `Local) in
       let (fun_binding : binding) = `Fun (fun_binder, arg_binders, body, tyvars) in
       let tyenv = Env.Int.bind tyenv (fun_var, t) in
	 `Computation (bindings @ [fun_binding], `Variable fun_var), tyenv
	 
   | `RecFunction _ -> failwith "t_of_value: mutually recursive functions"
   | `List l -> 
       let elementt = TypeUtils.element_type t in
       let tyenv, elts =
	 List.fold_right
	   (fun elt (tyenv, elts) ->
	      let elt, tyenv = qr_of_value elementt tyenv env elt in
		tyenv, elt :: elts)
	   l
	   (tyenv, [])
       in
       `List elts, tyenv
   | `Record fs ->
       let m, tyenv = 
	 List.fold_right
	   (fun (label, value) (m, tyenv) ->
	      let fieldt = TypeUtils.project_type label t in
	      let value, tyenv = qr_of_value fieldt tyenv env value in
		StringMap.add label value m, tyenv)
	   fs
	   (StringMap.empty, tyenv)
       in
	 `Extend (m, None), tyenv
   | `Variant (tag, value) -> 
       let vt = TypeUtils.variant_at tag t in
       let v, tyenv = qr_of_value vt tyenv env value in
	 `Inject (tag, v, t), tyenv
   | v -> failwith ("t_of_value: unsupported value " ^ (Show.show Value.show_t v))

and bindings_from_closure_env tyenv freevars valenv closure_env =
  let env = Value.shadow valenv ~by:closure_env in
  let env = fst3 env in
  (*  Debug.print ("closure env domain " ^ (Show.show (Show.show_list Show.show_int) (IntMap.domain env))); *)
  let env = restrict env freevars in
  (*  Debug.print ("closure env domain filtered " ^ (Show.show (Show.show_list Show.show_int) (IntMap.domain env))); *)

  let qr name (value, _) (qr_env, new_names, tyenv) =
    let t = Env.Int.lookup tyenv name in
    let value', tyenv = qr_of_value t tyenv valenv value in
    let new_name = Var.fresh_raw_var () in
    let qr_env = Env.Int.bind qr_env (new_name, value') in
    let tyenv = Env.Int.bind tyenv (new_name, t) in
    let new_names = IntMap.add name new_name new_names in
      qr_env, new_names, tyenv

  in
  let qr_env, new_names, tyenv = IntMap.fold qr env (Env.Int.empty, IntMap.empty, tyenv) in

  let binding name qr bindings =
    (* Debug.f "bindings_from_closure_env binding tyenv %d" name; *)
    let t = Env.Int.lookup tyenv name in
    let tyvars = TypeUtils.quantifiers t in
    let binder = (name, (t, "", `Local)) in
      (`Let (binder, tyvars, qr)) :: bindings
  in
  let bindings = List.rev (Env.Int.fold binding qr_env []) in
    bindings, new_names, tyenv

let type_constant c =
  match c with
    | `Bool _ -> Types.bool_type
    | `Int _ -> Types.int_type
    | `Char _ -> Types.char_type
    | `String _ -> Types.string_type
    | `Float _ -> Types.float_type

let type_binder tyenv (var, (t, _, _)) = Env.Int.bind tyenv (var, t)

let type_list xs type_value =
  List.map type_value xs

let type_name_map map type_value =
  StringMap.map
    (fun v -> type_value v)
    map

let bindings tyenv bs =
  List.fold_left
    (fun tyenv binding -> 
       match binding with
	 | `PFun (binder, _)
	 | `Let (binder, _, _)
	 | `Fun (binder, _, _, _) -> 
	     type_binder tyenv binder) 
    tyenv 
    bs

(* reconstruct types of qr expressions *)
let rec type_qr : Types.datatype Env.Int.t -> qr -> Types.datatype = 
  fun tyenv q ->
    let lookup_type = Env.Int.lookup tyenv in
    let t = 
    match q with 
      | `Constant c -> type_constant c
      | `Variable var -> lookup_type var
      | `Extend (fields, base) ->
	  let (type_field : qr -> Types.datatype) = type_qr tyenv in
	  let (field_types : Types.datatype StringMap.t) = type_name_map fields type_field in
	  let base_type = opt_map (type_qr tyenv) base in
	    begin
              match base_type with
                | None -> Types.make_record_type field_types
                | Some t ->
                    begin
                      match TypeUtils.concrete_type t with
                        | `Record row ->
                            `Record (Types.extend_row field_types row)
                        | _ -> assert false
                    end
	    end
      | `Project (label, r) ->
	  let rt = type_qr tyenv r in
	    TypeUtils.project_type label rt
      | `Erase (names, r) ->
	  let rt = type_qr tyenv r in
	    TypeUtils.erase_type_poly names rt
      | `Inject (_, _, t) -> t
      | `TApp (v, ts) ->
	  let t = type_qr tyenv v in
            begin try
              Instantiate.apply_type t ts 
            with
                Instantiate.ArityMismatch ->
                  prerr_endline ("Arity mismatch in type application (Qr.type_qr)");
                  prerr_endline ("expression: "^Show.show show_qr (`TApp (v, ts)));
                  prerr_endline ("type: "^Types.string_of_datatype t);
		  prerr_endline ("raw type: "^Show.show Types.show_typ t);
                  prerr_endline ("tyargs: "^String.concat "," (List.map Types.string_of_type_arg ts));
                  failwith "fatal internal error"
	    end
      | `TAbs (tyvars, v) ->
	  let t = type_qr tyenv v in
	    Types.for_all (tyvars, t)
      | `Database (_db, _name) ->
	  `Primitive `DB
      | `Table (_, _, _, row_type) ->
	  Types.make_table_type (`Record row_type, `Record row_type, `Record row_type)
      | `List xs ->
	  let ts = type_list xs (type_qr tyenv) in
	    begin
	      match ts with
		| t :: ts ->
		    assert (List.for_all (fun t' -> t = t') ts);
		    Types.make_list_type t
		| [] -> 
		    Env.String.lookup Lib.type_env "Nil"
	    end
      | `Apply (f, _args) ->
	  let ft = type_qr tyenv f in
	    TypeUtils.return_type ft
      | `Case (_v, cases, default) ->
	  let type_case (b, body) = type_qr (type_binder tyenv b) body in
	  let case_types = type_name_map cases type_case in
	  let default_type = opt_map type_case default in
            if not (StringMap.is_empty case_types) then
              (StringMap.to_alist ->- List.hd ->- snd) case_types
            else
              val_of default_type
      | `If (_, then_branch, _) ->
	  type_qr tyenv then_branch
      | `Computation (bs, tc) ->
	  type_qr (bindings tyenv bs) tc
      | `Wrong t -> t
    in
      Debug.print ("q expr " ^ (Show.show show_qr q));
      Debug.print ("of type " ^ (Show.show Types.show_datatype t));
      t

let qr_of_query tyenv env comp =
  let freevars = Ir.FreeVars.computation tyenv IntSet.empty comp in
  let restricted_env = restrict (fst3 env) freevars in
  let binding name (value, _) (bindings, tyenv) =
    (* Debug.f "qr_of_query tyenv %d" name; *)
    let t = Env.Int.lookup tyenv name in
    (* FIXME: really no tyvars on value-bindings?
       rationale: type variables in env-values can only stem from function types,
       those are already quantified over in the function's type *)
    (* let tyvars = TypeUtils.quantifiers t in *)
    let tyvars = [] in
    let binder = (name, (t, "", `Local)) in
    let qr, tyenv = qr_of_value t tyenv env value in
    let bindings' = 
      match qr with
	(* FIXME: this needs to be tested thoroughly *)
	| `Computation (bindings, `Variable new_name) ->
	    begin
	      match List.rev bindings with
		| `Fun ((new_name', _), arg_binders, body, tyvars) :: closenv_bindings when new_name = new_name' ->
		    closenv_bindings @ [`Fun (binder, arg_binders, body, tyvars)]
		| `PFun ((new_name', _), dispatch) :: closenv_bindings when new_name = new_name' ->
		    closenv_bindings @ [`PFun (binder, dispatch)]
		| _ -> [`Let (binder, tyvars, qr)]
	    end
	| _ -> [`Let (binder, tyvars, qr)]
    in
      bindings' @ bindings, tyenv
  in
  let free_bindings, tyenv = IntMap.fold binding restricted_env ([], tyenv) in

  let primitive_free_vars = IntSet.inter freevars Lib.primitive_vars in
    
  let primitive var bindings =
    let t = Env.Int.lookup tyenv var in
    let stub = Lib.primitive_stub_by_code var in
    let qr, _tyenv = qr_of_value t tyenv env stub in
    let binding =
      match qr with
	| `Computation ([`PFun ((new_name, info), dispatch)], `Variable new_name') when new_name = new_name' ->
	    `PFun ((var, info), dispatch)
	| _ -> failwith ("unexpected primitive value: " ^ (Show.show show_qr qr))
    in
      binding :: bindings
  in

  let primitive_bindings = IntSet.fold primitive primitive_free_vars [] in
    
  let qr_comp = computation comp in
    match qr_comp with
      | `Computation (local_bindings, tc) ->
	  `Computation (primitive_bindings @ free_bindings @ local_bindings, tc), tyenv
      | _ -> assert false
    
