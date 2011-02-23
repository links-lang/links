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
  | `PrimitiveFun of string * Var.var option
(*  | `Fun of binder list * qr * Types.datatype * env *)
(*  | `Fun of binder list * tyvar list * qr * Types.datatype *)

  | `Apply of qr * qr list
  | `Case of qr * (binder * qr) name_map * (binder * qr) option
  | `If of qr * qr * qr
  
  | `Computation of binding list * qr

  | `Wrong of Types.datatype ]
and binding = 
  [ `Let of (binder * tyvar list * qr)
  | `Fun of (binder * binder list * qr * tyvar list) ]
and env = qr Env.Int.t 
    deriving (Show)

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
(* FIXME eta-expand primitive functions? *)
   | `PrimitiveFunction f -> `PrimitiveFun f, tyenv
   | `RecFunction ([(f, _)], _, _, _) when IntSet.mem f (val_of !prelude_primitive_vars) ->
       let s = IntMap.find f (val_of !prelude_primitive_namemap) in
       `PrimitiveFun (s, None), tyenv
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

(*
module type TRANSFORM =
sig
  type environment = Types.datatype Env.Int.t

  class visitor : environment -> 
  object ('self_type)

    val tyenv : environment

    method lookup_type : var -> Types.datatype

    method list :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a list -> 'a list * Types.datatype list * 'self_type

    method name_map :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a name_map -> 'a name_map * Types.datatype name_map * 'self_type        

    method option :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a option -> 'a option * Types.datatype option * 'self_type

    method with_tyenv : environment -> 'self_type

    method var : var -> var * 'self_type

    method bindings : bindings -> bindings * 'self_type

    method binders : binder list -> 'self_type
    method binder : binder -> 'self_type

    method constant : constant -> constant * Types.datatype * 'self_type

    method qr : qr -> (qr * Types.datatype * 'self_type)
  end
end

module Transform : TRANSFORM =
struct
  
  type environment = Types.datatype Env.Int.t

  class visitor (tyenv : environment) =
  object (o : 'self_type)
    val tyenv = tyenv

    method lookup_type : var -> Types.datatype = fun var ->
      Env.Int.lookup tyenv var

    method var : var -> var * 'self_type = fun var ->
      var, o

    method list :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a list -> 'a list * Types.datatype list * 'self_type =
      fun f v ->
        let vs, ts, o =
          List.fold_left
            (fun (vs, ts, o) v ->
               let (v, t, o) = f o v in
                 v::vs, t::ts, o)
            ([], [], o)
            v
        in
          List.rev vs, List.rev ts, o

    method name_map :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a name_map -> 'a name_map * Types.datatype name_map * 'self_type =
      fun f vmap ->
        StringMap.fold
          (fun name v (vmap, tmap, o) ->
             let (v, t, o) = f o v in
               (StringMap.add name v vmap,
                StringMap.add name t tmap,
                o))
          vmap
          (StringMap.empty, StringMap.empty, o)

    method option :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a option -> 'a option * Types.datatype option * 'self_type =
      fun f v ->
        match v with
          | None -> None, None, o
          | Some v ->
              let v, t, o = f o v in
                Some v, Some t, o

    method with_tyenv : environment -> 'self_type = fun tyenv -> {< tyenv = tyenv >}

    method bindings : bindings -> bindings * 'self_type = fun bs ->
      List.fold_right
	(fun (((x, (t, _, _)) as binder, tyvars, tc)) (bs, o) ->
	   (* FIXME: really need to update the tyenv? should already be complete *)
	   let env = Env.Int.bind tyenv (x, t) in
	   let tc, _, o = o#qr tc in
	     (binder, tyvars, tc) :: bs, o#with_tyenv env)
	bs
	([], o)

    method binders : binder list -> 'self_type = fun binders ->
      List.fold_right
	(fun (v, (t, _, _)) o -> o#with_tyenv (Env.Int.bind tyenv (v, t)))
	binders
	o

    method binder : binder -> 'self_type = 
      fun (var, (t, _, _)) -> 
	o#with_tyenv (Env.Int.bind tyenv (var, t))

    method constant : constant -> (constant * Types.datatype * 'self_type) = fun c ->
      match c with
        | `Bool _ -> c, Types.bool_type, o
        | `Int _ -> c, Types.int_type, o
        | `Char _ -> c, Types.char_type, o
        | `String _ -> c, Types.string_type, o
        | `Float _ -> c, Types.float_type, o

    method qr : qr -> (qr * Types.datatype * 'self_type) = fun e ->
      match e with
	| `Constant c -> 
	    let c, t, o = o#constant c in
	      `Constant c, t, o
	| `Variable var -> 
	    let t = o#lookup_type var in
	    let var, o = o#var var in
	      `Variable var, t, o
	| `Extend (fields, base) ->
            let (fields, field_types, o) = o#name_map (fun o -> o#qr) fields in
            let (base, base_type, o) = o#option (fun o -> o#qr) base in

            let t =
              match base_type with
                | None -> Types.make_record_type field_types
                | Some t ->
                    begin
                      match TypeUtils.concrete_type t with
                        | `Record row ->
                            `Record (Types.extend_row field_types row)
                        | _ -> assert false
                    end
            in
              `Extend (fields, base), t, o
	| `Project (label, r) ->
            let (r, rt, o) = o#qr r in
              `Project (label, r), TypeUtils.project_type label rt, o
	| `Erase (names, r) ->
            let (r, rt, o) = o#qr r in
            let t = TypeUtils.erase_type_poly names rt in
              `Erase (names, r), t, o
	| `Inject (tag, v, t) -> 
	    let v, _, o = o#qr v in
	      `Inject (tag, v, t), t, o
        | `TApp (v, ts) ->
            let v, t, o = o#qr v in
              begin try
                let t = Instantiate.apply_type t ts in
                  `TApp (v, ts), t, o
              with
                  Instantiate.ArityMismatch ->
                    prerr_endline ("Arity mismatch in type application (Qr.Transform)");
                    prerr_endline ("expression: "^Show.show show_qr (`TApp (v, ts)));
                    prerr_endline ("type: "^Types.string_of_datatype t);
		    prerr_endline ("raw type: "^Show.show Types.show_typ t);
                    prerr_endline ("tyargs: "^String.concat "," (List.map Types.string_of_type_arg ts));
                    failwith "fatal internal error"
              end
	| `TAbs (tyvars, v) ->
            let v, t, o = o#qr v in
            let t = Types.for_all (tyvars, t) in
              `TAbs (tyvars, v), t, o
	| `Database (db, name) ->
	    `Database (db, name), `Primitive `DB, o
	| `Table (db, name, keys, row_type) ->
	    
	    let t = Types.make_table_type (`Record row_type, `Record row_type, `Record row_type) in
	      `Table (db, name, keys, row_type), t, o
	| `List xs ->
	    let xs, ts, o = o#list (fun o -> o#qr) xs in
	    let t =
	      match ts with
		| t :: ts ->
		    assert (List.for_all (fun t' -> t = t') ts);
		    Types.make_list_type t
		| [] -> 
		    Env.String.lookup Lib.type_env "Nil"
	    in
	      `List xs, t, o
	      
	| `PrimitiveFun (f, _) ->
	    let t = Env.String.lookup Lib.type_env f in
	      e, t, o
	| `Fun (binders, tyvars, body, t) ->
	    (* TODO: is this the right thing to do? *)
	    let o = o#binders binders in
	    let body, _, o = o#qr body in
	      `Fun (binders, tyvars, body, t), t, o
	| `Apply (f, args) ->
	    begin
	      try
		let f, ft, o = o#qr f in
		let args, _, o = o#list (fun o -> o#qr) args in
		let t = TypeUtils.return_type ft in
		  `Apply (f, args), t, o
	      with
		  TypeUtils.TypeDestructionError m -> 
		    begin
		      Debug.print ("foo " ^ (Show.show show_qr (`Apply (f, args))));
			failwith m
		    end
	    end

	| `Case (v, cases, default) ->
            let v, _, o = o#qr v in
            let cases, case_types, o =
              o#name_map
                (fun o (b, c) ->
                   let o = o#binder b in
                   let c, t, o = o#qr c in
                     (b, c), t, o) cases in
            let default, default_type, o =
              o#option (fun o (b, c) ->
                          let o = o#binder b in
                          let c, t, o = o#qr c in
                            (b, c), t, o) default in
            let t =
              if not (StringMap.is_empty case_types) then
                (StringMap.to_alist ->- List.hd ->- snd) case_types
              else
                val_of default_type
            in
              `Case (v, cases, default), t, o
	| `If (c, then_branch, else_branch) ->
	    let c, _, o = o#qr c in
	    let then_branch, t, o = o#qr then_branch in
	    let else_branch, _, o = o#qr else_branch in
	      `If (c, then_branch, else_branch), t, o
	| `Let (bindings, comp) ->
	    let bindings, o = o#bindings bindings in
	    let comp, t, o = o#qr comp in
	      begin
		match bindings with
		  | [] -> comp, t, o
		  | bindings ->
		      begin
			match comp with
			  | `Let (bsi, tci) ->
			      `Let (bindings @ bsi, tci), t, o
			  | comp ->
			      `Let (bindings, comp), t, o
		      end
	      end
	| `Wrong t ->
	    e, t, o
  end
end
*)

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
      | `PrimitiveFun (f, _) ->
	  Env.String.lookup Lib.type_env f
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
(*
	| `Computation (closenv_bindings :: (`Fun ((new_name, (ft, _, _)), arg_binders, body, tyvars)) :: [], `Variable new_name') when new_name = new_name' ->
	    closenv_bindings @ [`Fun ((name, (ft, "", `Local)), arg_binders, body, tyvars)]
*)
	| `Computation (bindings, `Variable new_name) ->
	    begin
	      match List.rev bindings with
		| `Fun ((new_name', _), arg_binders, body, tyvars) :: closenv_bindings when new_name = new_name' ->
		    closenv_bindings @ [`Fun (binder, arg_binders, body, tyvars)]
		| _ -> [`Let (binder, tyvars, qr)]
	    end
	| _ -> [`Let (binder, tyvars, qr)]
    in
      bindings' @ bindings, tyenv
  in
  let free_bindings, tyenv = IntMap.fold binding restricted_env ([], tyenv) in
  let qr_comp = computation comp in
    match qr_comp with
	(*
      | `Let (local_bindings, comp) ->
	  `Let (free_bindings @ local_bindings, comp), tyenv
	*)
      | `Computation (local_bindings, tc) ->
	  `Computation (free_bindings @ local_bindings, tc), tyenv
      | _ -> assert false
    
