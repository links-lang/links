(*pp deriving *)

open Utility

let prelude_primitive_names = 
    ["concatMap"; "map"; "sortByFlat"; "asList"; "zip"; "unzip"; 
     "select"; "groupByFlat"; "sum"; "concat"; "and"; "or"; 
     "max"; "min"; "avg"; "takeWhile"; "dropWhile"; "nubFlat";
     "reverse"; "filter"; "ignore"]

let prelude_primitive_vars = ref None
let prelude_primitive_namemap = ref None

(* HACK: global variable which stores the database on which to execute
   the query (or none) *)
let used_database = ref None

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

(* term variables *)
type var = Var.var
  deriving (Show, Eq, Hash, Typeable, Pickle, Dump)

type name = string
  deriving (Show)

type name_set = Utility.stringset
  deriving (Show)
type 'a name_map = 'a Utility.stringmap
  deriving (Show)

type constant = Constant.constant
  deriving (Show)

type qr =
  [ `Constant of constant
  | `Variable of var
  | `Extend of qr name_map * qr option
  | `Project of name * qr
  | `Erase of name_set * qr
  | `Inject of name * qr

  | `Table of Value.table
  | `Singleton of qr
  | `Concat of qr list
  | `Apply of qr * qr list
  | `Case of qr * (var * qr) name_map * (var * qr) option
  | `If of qr * qr * qr option
  
  | `Let of binding list * qr
  | `Primitive of string
  | `Lambda of var list * qr

  | `Wrong ]
and binding = var * qr
    deriving (Show)

let rec computation (bs, tc) : qr =
  let bs = bindings bs in
  let e = tail_computation tc in
    match e with
      | `Let (bs', e) ->
	  `Let (bs @ bs', e)
      | _ -> `Let (bs, e)

and bindings bs = List.map binding bs

and binding (b : Ir.binding) : binding =
  match b with
    | `Let (binder, (_tyvars, tc)) ->
	(Var.var_of_binder binder, tail_computation tc)
    | `Fun (binder, (_tyvars, binders, body), _loc) ->
	(Var.var_of_binder binder, `Lambda ((List.map Var.var_of_binder binders), computation body))
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
    | `Inject (tag, v, _) -> `Inject (tag, value v)
    | `ApplyPure (f, args) -> `Apply (value f, List.map value args)
    | `TApp (v, _) | `TAbs (_, v) -> value v
    | _ -> 
	Debug.print (Show.show Ir.show_value v);
	failwith "unsupported value"

and tail_computation tc =
  match tc with
    | `Return v -> value v
    | `Apply (f, args) -> `Apply (value f, List.map value args)
    | `Special s -> special s
    | `Case (v, cases, default) ->
	let v = value v in
	let case (binder, body) = (Var.var_of_binder binder, computation body) in
	let cases = StringMap.map case cases in
	let default = opt_map case default in
	  `Case (v, cases, default)
    | `If (c, t, e) -> `If (value c, computation t, Some (computation e))

and special s =
  match s with
    | `Wrong _ -> `Wrong
    | `Query (_range, q, _t) -> computation q
    | _ -> failwith "unsupported special"

let restrict m s = IntMap.filter (fun k _ -> IntSet.mem k s) m

let rec inline_primitive env (q : qr) : qr =
  let rec binding (name, e) = (name, replace e) 
  and replace q =
    match q with
      | `Constant _ | `Table _ | `Wrong -> q
      | `Project (label, v) -> `Project (label, replace v)
      | `Extend (extend_fields, r) -> `
	  Extend (StringMap.map replace extend_fields, opt_map replace r)
      | `Erase (labels, v) -> `Erase (labels, replace v)
      | `Inject (tag, v) -> `Inject (tag, replace v)
      | `Concat xs -> `Concat (List.map replace xs)
      | `Apply (f, args) -> `Apply (replace f, List.map replace args)
      | `Case (v, cases, default) ->
	  let v = replace v in
	  let case (binder, body) = (binder, replace body) in
	  let cases = StringMap.map case cases in
	  let default = opt_map case default in
	    `Case (v, cases, default)
      | `If (c, t, e) -> `If (replace c, replace t, opt_map replace e)
      | `Let (bs, tc) -> 
	  `Let (List.map binding bs, replace tc)
      | `Variable var ->
	  begin
	    match IntMap.lookup var env with
	      | Some v -> v
	      | None -> `Variable var
	  end
      | `Primitive s -> `Primitive s
      | `Lambda (xs, body) -> `Lambda (xs, replace body)
      | `Singleton v -> `Singleton (replace v)
  in
    replace q

module FreeVars =
struct

  let rec name_map bound proj map =
    StringMap.fold
      (fun _ v free -> 
	 let free' = fv bound (proj v) in
	   IntSet.union free' free)
      map
      IntSet.empty

  and bindings bound bs =
    List.fold_left
      (fun (free, bound) (var, tc) ->
	 let bound' = IntSet.add var bound in
	 let free = IntSet.union (fv bound tc) free in
	   (free, bound'))
      (IntSet.empty, bound)
      bs

  and fv bound = function
    | `Variable name -> 
	if not (IntSet.mem name bound) then
	  IntSet.add name IntSet.empty
	else
	  IntSet.empty
    | `Extend (extend_fields, base) ->
	let free = 
	  StringMap.fold
	    (fun _ v free -> IntSet.union (fv bound v) free)
	    extend_fields
	    IntSet.empty
	in
	  begin
	    match base with
	      | Some r -> IntSet.union (fv bound r) free
	      | None -> free
	  end
    | `Project (_, value) | `Erase (_, value) | `Inject (_, value) 
    | `Singleton value ->
	fv bound value
    | `Let (bs, tc) ->
	let free, bound = bindings bound bs in
	  IntSet.union (fv bound tc) free
    | `Lambda (xs, body) ->
	let bound = List.fold_right IntSet.add xs bound in
	  fv bound body
    | `Case (v, cases, default) ->
	let case _ (var, body) free =
	  IntSet.union (fv (IntSet.add var bound) body) free
	in
	let v_free = fv bound v in
	let cases_free = StringMap.fold case cases IntSet.empty in
	let default_free = 
	  opt_app 
	    (fun (var, body) -> fv (IntSet.add var bound) body) 
	    IntSet.empty
	    default 
	in
	  IntSet.union_all [v_free; cases_free; default_free]
    | `Concat xs -> IntSet.union_all (List.map (fv bound) xs)
    | `If (c, t, Some e) -> IntSet.union_all [fv bound c; fv bound t; fv bound e]
    | `If (c, t, None) -> IntSet.union_all [fv bound c; fv bound t]
    | `Table _ | `Primitive _ | `Wrong | `Constant _ ->
	IntSet.empty
    | `Apply (f, args) ->
	IntSet.union_all ((fv bound f) :: (List.map (fv bound) args))

  let freevars = fv

  let boundvars =
    function
      | `Fun (binders, _, _, _) -> 
	  List.fold_right IntSet.add (List.map fst binders) IntSet.empty
      | `Let (bindings, _) ->
	  List.fold_right IntSet.add (List.map (fst -<- fst) bindings) IntSet.empty 
      | `Case (_, cases, default) ->
	  let case_bound = 
	    StringMap.fold 
	      (fun _ ((name, _), _) bound -> IntSet.add name bound)
	      cases
	      IntSet.empty
	  in
	  let default_bound = opt_app 
	    (fun ((var, _), _) -> IntSet.singleton var) 
	    IntSet.empty 
	    default 
	  in
	    IntSet.union case_bound default_bound
      | _ -> IntSet.empty
end

let local_freevars xs comp =
  let bound_vars = IntSet.from_list xs in
  let freevars = FreeVars.fv bound_vars comp in
    freevars
(*   let freevars = IntSet.diff freevars Lib.primitive_vars in
    IntSet.diff freevars (val_of !prelude_primitive_vars) *)

let rec qr_of_value env : Value.t -> qr =
 function
   | `Bool b -> `Constant (`Bool b)
   | `Char c -> `Constant (`Char c)
   | `Float f -> `Constant (`Float f)
   | `Int i -> `Constant (`Int i)
   | `String s -> `Constant (`String s)
   | `Table (((db, _), _, _, _) as t) -> 
       used_database := Some db;
       `Table t 
   | `PrimitiveFunction (fs, _) -> `Primitive fs
       
   | `RecFunction ([(f, _)], _, _, _) when IntSet.mem f (val_of !prelude_primitive_vars) ->
       let s = IntMap.find f (val_of !prelude_primitive_namemap) in
	 `Primitive s

   | `RecFunction ([(f, (xs, body))], locals, f', _scope) ->
       assert (f = f');
       let body = computation body in
       let freevars = local_freevars xs body in
	 Debug.print ("closure freevars " ^ (Show.show IntSet.show_t freevars));
       let env' : (Value.t * Ir.scope) IntMap.t = restrict (fst3 (Value.shadow env ~by:locals)) freevars in
	 Debug.print ("closure restricted env domain " ^ (Show.show (Show.show_list Show.show_int) (IntMap.domain env')));
       let env' = IntMap.map ((qr_of_value env) -<- fst) env' in
       let body = inline_primitive env' body in
	 `Lambda (xs, body)
	 
   | `RecFunction _ -> failwith "t_of_value: mutually recursive functions"
   | `List l -> `Concat (List.map (fun v -> `Singleton (qr_of_value env v)) l)
   | `Record fs ->
       let m = 
	 List.fold_right
	   (fun (label, value) m ->
	      let value = qr_of_value env value in
		StringMap.add label value m)
	   fs
	   StringMap.empty
       in
	 `Extend (m, None)
   | `Variant (tag, value) -> 
       let v = qr_of_value env value in
	 `Inject (tag, v)
   | v -> failwith ("t_of_value: unsupported value " ^ (Show.show Value.show_t v))

let binding_freevars bindings =
  let lambda_freevars freevars (name, value) = 
    IntSet.union freevars (FreeVars.fv IntSet.empty value)
  in
    List.fold_left lambda_freevars IntSet.empty bindings

let qr_of_query env _range comp =
  let qr_comp = computation comp in
  let freevars =  FreeVars.fv IntSet.empty qr_comp in
  let restricted_env = restrict (fst3 env) freevars in
  let binding name (value, _) bindings = 
    let b = (name, qr_of_value env value) in
      b :: bindings
  in
  let free_bindings = IntMap.fold binding restricted_env [] in

  let remaining_freevars = binding_freevars free_bindings in
    Debug.print ("remaining freevars " ^ (Show.show IntSet.show_t remaining_freevars));

  let primitive_free_vars = IntSet.inter (IntSet.union freevars remaining_freevars) Lib.primitive_vars in

    Debug.print ("primitive_free_vars " ^ (Show.show IntSet.show_t primitive_free_vars));
    
  let primitive var bindings =
    let stub = Lib.primitive_stub_by_code var in
    let qr = qr_of_value env stub in
      (var, qr) :: bindings
  in

  let primitive_bindings = IntSet.fold primitive primitive_free_vars [] in
    match qr_comp with
      | `Let (local_bindings, tc) ->
	  `Let (primitive_bindings @ free_bindings @ local_bindings, tc)
      | _ -> assert false
    
module Census =
struct
  let merge maps = 
    let aux _ a b =
      match a, b with
	| Some a, Some b -> Some (a + b)
	| None, Some b -> Some b
	| Some a, None -> Some a
	| None, None -> assert false
    in
      match maps with
	| m :: ms -> 
	    List.fold_left (IntMap.merge aux) m ms
	| [] ->
	    IntMap.empty

  let rec bindings (bs : binding list)=
    let binding  (_, body) = count body
    in
      List.map binding bs
	
  and count q =
    match q with
      | `Variable var -> 
	  Debug.f "encountered %d" var;
	  IntMap.add var 1 IntMap.empty
      | `Constant _ | `Table _ | `Primitive _
      | `Wrong -> IntMap.empty
      | `Extend (extend_fields, r) ->
	  let cm = StringMap.fold (fun _ f cm -> merge [cm; count f]) extend_fields IntMap.empty in
	    opt_app (fun r -> merge [cm; count r]) cm r 
      | `Singleton v
      | `Lambda (_, v)
      | `Project (_, v) 
      | `Erase (_, v) 
      | `Inject (_, v) -> count v
      | `Concat xs -> merge (List.map count xs)
      | `Apply (f, args) ->
	  merge ((count f) :: (List.map count args))
      | `Case (v, cases, default) ->
	  let cm = count v in
	  let cm = 
	    StringMap.fold 
	      (fun _ (_, body) cm -> merge [cm; count body]) 
	      cases 
	      cm 
	  in
	    opt_app (fun (_, body) -> merge [cm; count body]) cm default
      | `If (c, t, Some e) -> merge [count c; count t; count e]
      | `If (c, t, None) -> merge [count c; count t]
      | `Let (bs, tc) ->
	  merge ((count tc) :: (bindings bs))
end

module ElimDeadDefs =
struct
  let bindings cm bs =
    let filter (name, _) =
	      match IntMap.lookup name cm with
		| Some c when c > 0 -> true
		| _ -> false
    in
      List.filter filter bs

  let rec eliminate cm q = 
    match q with
      | `Constant _ | `Variable _ | `Table _
      | `Wrong -> q
      | `Project (label, v)  -> `Project (label, eliminate cm v)
      | `Erase (labels, v) -> `Erase (labels, eliminate cm v)
      | `Extend (extend_fields, r) ->
	  let extend_fields = StringMap.map (eliminate cm) extend_fields in
	  let r = opt_map (eliminate cm) r in
	    `Extend (extend_fields, r)
      | `Inject (tag, v) -> `Inject (tag, eliminate cm v)
      | `Concat xs -> `Concat (List.map (eliminate cm) xs)
      | `Apply (f, args) -> `Apply (eliminate cm f, List.map (eliminate cm) args)
      | `If (c, t, e) -> `If (eliminate cm c, eliminate cm t, opt_map (eliminate cm) e)
      | `Case (v, cases, default) ->
	  let case (binder, body) = (binder, eliminate cm body) in
	  let v = eliminate cm v in
	  let cases = StringMap.map case cases in
	  let default = opt_map case default in
	    `Case (v, cases, default)
      | `Let (bs, tc) ->
	  begin
	    let tc = eliminate cm tc in
	      match bindings cm bs with
		| [] -> tc
		| bs -> `Let (bs, tc)
	  end
      | `Lambda (xs, body) -> `Lambda (xs, eliminate cm body)
      | `Primitive f -> `Primitive f
      | `Singleton v -> `Singleton (eliminate cm v)
end

(* FIXME: adapt QueryRegex to native strings *)
module QueryRegex = struct
  let rec unbox_list =
    function
      | `Concat vs -> concat_map unbox_list vs
      | `Singleton v -> [v]
      | _ -> failwith ("failed to unbox list")

  let unbox_pair = function
    | `Extend (r, None) ->
	begin
	  try
	    StringMap.find "2" r, StringMap.find "1" r
	  with
	      _ -> failwith "failed to unbox pair"
	end
    | _ -> failwith "failed to unbox pair"

  let is_dotstar p = 
    match unbox_pair p with
      | `Inject ("Any", _), `Inject ("Star", _) -> true
      | _ -> false

  let quote p = 
    match p with
      | `Constant `String s -> 
	  let special = ['%'; '_'; '*'; '?'; '('; ')'; '['; ']'] in
	  let contains c = List.exists (fun x -> x = c) special in
	  let l = 
	    List.map 
	      (function 
		 | x when contains x -> "\\" ^ (string_of_char x)
		 | x -> string_of_char x)
	      (explode s)
	  in
	    `Constant (`String (mapstrcat "" identity l))
      | p ->
	  `Apply (`Primitive "quote", [p])

  let unquote = function
      | `Apply (`Primitive "quote", [p']) -> p'
      | p -> p

    let append_patterns p1 p2 =
      match p1, p2 with
      | `Constant (`String s1), `Constant (`String s2) -> `Constant (`String (s1 ^ s2))
      | `Constant (`String _), _ 
      | _, `Constant (`String _) -> `Apply (`Primitive "^^", [p1; p2])
      |  p1, p2 -> quote (`Apply (`Primitive "^^", [unquote p1; unquote p2]))

  let rec similarify p = 
    match p with
      | `Inject ("Seq", l) -> 
	  let ps = List.map similarify (unbox_list l) in
	    assert ((List.length ps) >= 1);
	    List.fold_left append_patterns (List.hd ps) (drop 1 ps)
      | `Inject ("Range", p) -> 
	  let f, s = unbox_pair p in
	    append_patterns f s
      | `Inject ("Simply", e) ->
	  e
      | `Inject ("Quote", s) ->
	  quote (similarify s)
      | `Inject ("Any", _) -> 
	  `Constant (`String "_")
      | `Inject ("StartAnchor", _) -> 
	  `Constant (`String "")
      | `Inject ("EndAnchor", _) -> 
	  `Constant (`String "")
      | `Inject ("Alternate", p) ->
	  let f, s = unbox_pair p in
	    append_patterns (similarify f) (append_patterns (`Constant (`String "|")) (similarify s))
      | `Inject ("Group", s) ->
	  append_patterns (`Constant (`String "(")) (append_patterns (similarify s) (`Constant (`String ")")))
      | `Inject ("Repeat", p) when is_dotstar p ->
	  `Constant (`String "%")
      | `Inject ("Repeat", p) ->
	  let f, s = unbox_pair p in
	    append_patterns (similarify f) (similarify s)
      | `Inject ("Plus", _) ->
	  `Constant (`String "+")
      | `Inject ("Question", _) ->
	  `Constant (`String "?")
      | `Inject ("Star", _) ->
	  `Constant (`String "*")
      | t -> Debug.print (Show.show show_qr t);
	  assert false
	    end

module Inliner =
struct

  type ctx = { venv : qr Env.Int.t; 
	       census : int IntMap.t }

  (* FIXME: differentiate between functions and other values based on type *)
  (* FIXME: differentiate between function inlining and value propagation *)
  let conservative ctx name =
    (* Debug.print (Show.show IntSet.show_t (Env.Int.domain ctx.venv)); *)
    match Env.Int.find ctx.venv name with
      | Some value ->
	  begin
	    match value with
	      | `Table _ | `Constant _ | `Primitive _ | `Wrong 
	      | `Singleton _ | `Concat [] | `Variable _ -> 
		  (* Debug.f "inline %d (small)" name; *)
		  value
	      | `Extend _ | `Project _ | `Erase _ | `Inject _
	      | `Concat _ | `Apply _ | `Case _ | `If _
	      | `Let _ ->
		  begin
		    match IntMap.lookup name ctx.census with
		      | Some c when c < 2 -> 
			  (* Debug.f "inline %d (c = %d)" name c; *)
			  value
		      | _ -> 
			  (* Debug.f "not inline %d" name; *)
			  `Variable name
		  end
	      | `Lambda _ -> value
	  end
      | None -> 
	  (* Debug.f "not inlined %d (not in env)" name; *)
	  `Variable name

  let reduce_append vs =
    let vs' = 
      (concat_map
	 (function
	    | `Concat vs -> vs
	    | v -> [v])
	 vs)
    in
      match vs' with
	| [`Singleton v] -> `Singleton v
	| vs -> `Concat vs

  let rewrite_primitive = function
    | ("asList" | "AsList"), [t] -> t
    | "filter", [p; l] -> 
	let x = Var.fresh_raw_var () in
	  `Apply 
	    (`Primitive "concatMap", 
	     [`Lambda 
		([x], 
		 `If 
		   (`Apply (p, [`Variable x]), 
		    `Singleton (`Variable x), 
		    None)); 
	      l])
    | "all", [p; l] ->
	let x = Var.fresh_raw_var () in
	  `Apply
	    (`Primitive "and",
	     [`Apply
		(`Primitive "concatMap",
		 [`Lambda
		    ([x],
		     `Apply (p, [`Variable x]));
		  l])])
    | "any", [p; l] ->
	let x = Var.fresh_raw_var () in
	  `Apply
	    (`Primitive "or",
	     [`Apply
		(`Primitive "concatMap",
		 [`Lambda
		    ([x],
		     `Apply (p, [`Variable x]));
		  l])])
    | "Cons", [x; `Concat []] -> `Singleton x
    | "Cons", [x; xs] -> reduce_append [`Singleton x; xs]
    | "Concat", [xs; ys] -> reduce_append [xs; ys]
    | "tilde", [s; p] -> 
	let pattern = QueryRegex.similarify p in
	`Apply (`Primitive "tilde", [s; pattern])
    | "<", [e1; e2] ->
	`Apply (`Primitive ">", [e2; e1])
    | ">=", [e1; e2] ->
	`Apply (`Primitive "not", [`Apply (`Primitive ">", [e2; e1])])
    | "<=", [e1; e2] ->
	`Apply (`Primitive "not", [`Apply (`Primitive ">", [e1; e2])])
    | "negate", [i] ->
	`Apply (`Primitive "*", [i; `Constant (`Int (Num.Int (-1)))])
    | "negatef", [i] ->
	`Apply (`Primitive "*.", [i; `Constant (`Float (-1.0))])
    | f, args -> `Apply (`Primitive f, args)
	    
  let rec inline test (ctx : ctx) (q : qr) =
    let inl = inline test ctx in
      match q with
	| `Variable name -> test ctx name
	| `Extend (extend_fields, r) ->
	    let extend_fields = StringMap.map inl extend_fields in
	    let r = opt_map inl r in
	      `Extend (extend_fields, r)
	| `Project (label, r) ->
	    `Project (label, inl r)
	| `Erase (names, r) ->
	    `Erase (names, inl r)
	| `Inject (tag, value) ->
	    `Inject (tag, inl value)
	| `Let ([], tc) -> inl tc
	| `Let (bs, tc) ->
	    let bs, ctx = bindings test ctx bs in
	    let tc = inline test ctx tc in
	      `Let (bs, tc)
	| `Concat xs -> 
	    `Concat (List.map inl xs)
	| `Apply (f, args) ->
	    apply test ctx (inl f) (List.map inl args)
	| `Case (v, cases, default) ->
	    let case (binder, body) =
	      (binder, inl body)
	    in
	    let v = inl v in
	    let cases = StringMap.map case cases in
	    let default = opt_map case default in
	      begin
		match inl v with
		  | `Inject (tag, value) ->
		      let (x, body) =
			begin
			  match StringMap.lookup tag cases, default with
			    | Some case, _ -> case
			    | None, Some default -> default
			    | None, None -> failwith "Inline.inline: neither matching case nor default case"
			end
		      in
			beta test ctx [x] [value] body
		  | _ -> `Case (v, cases, default)
	      end
	      
	| `If (c, t, e) ->
	    let c = inl c in
	    let t = inl t in
	    let e = opt_map inl e in
	      begin
		match c with
		  | `Constant (`Bool true) -> t
		  | `Constant (`Bool false) -> from_option (`Concat []) e
		  | _ -> `If (inl c, inl t, opt_map inl e)
	      end
	| `Constant _
	| `Wrong
	| `Table _
	| `Primitive _ -> q
	| `Lambda (xs, body) -> `Lambda (xs, inl body)
	| `Singleton v -> `Singleton (inl v)

  and bindings test ctx bs =
    let binding (bs, ctx) (name, tc) =
      let tc = inline test ctx tc in
      let b = (name, tc) in
      let venv = Env.Int.bind ctx.venv (name, tc) in
	b :: bs, { ctx with venv = venv }
    in
      List.fold_left binding ([], ctx) bs 

  and beta test ctx xs args body =
    try
      let arg_bindings = List.map2 (fun x arg -> (x, arg)) xs args in
	inline test ctx (`Let (arg_bindings, body))
    with Invalid_argument _ -> failwith "arity mismatch in function inlining"

  and apply test ctx f args =
      match f with
(*	| `Variable fvar -> `Apply *)
	| `Lambda (xs, body) -> beta test ctx xs args body
	| `Case (v, cases, default) -> 
	    let case (var, body) = (var, apply test ctx body args) in
	    let cases' = StringMap.map case cases in
	    let default' = opt_map case default in
	      `Case (v, cases', default')
	      
	| `If (c, t, e) ->
	    let t' = apply test ctx t args in
	    let e' = opt_map (fun e -> apply test ctx e args) e in
	      `If (c, t', e')
	| `Primitive s -> rewrite_primitive (s, args)
	| _ -> `Apply (f, args)

end

module ImpType = struct
  type imptype = [`Atom | `List] deriving (Show)

  type tqr =
      [ `Lambda of ((Var.var list * tqr) * imptype)
      | `If of (tqr * tqr * tqr option) * imptype
      | `Table of Value.table * imptype
      | `Singleton of tqr * imptype 
      | `Concat of tqr list * imptype
      | `Extend of tqr name_map * tqr option * imptype
      | `Project of (string * tqr) * imptype
      | `Erase of (name_set * tqr) * imptype
      | `Inject of (string * tqr) * imptype
      | `Apply of (tqr * tqr list) * imptype
      | `Primitive of string 
      | `Variable of Var.var * imptype
      | `Constant of Constant.constant * imptype
      | `Box of tqr * imptype
      | `Unbox of tqr * imptype
      | `Case of (tqr * (Var.var * tqr) name_map * (Var.var * tqr) option) * imptype
      | `Let of binding list * tqr * imptype
      | `Wrong of imptype ]
  and binding = Var.var * tqr
      deriving (Show)

  let string_of_tqr = Show.show show_tqr

  let typeof_tqr = function
(*    | `For (_, t) -> t *)
    | `Lambda (_, t) -> t
    | `If (_, t) -> t 
    | `Table (_, t) -> t 
    | `Singleton (_, t) -> t
    | `Concat (_, t) -> t 
    | `Project (_, t) -> t 
    | `Erase (_, t) -> t 
    | `Extend (_, _, t) -> t
    | `Inject (_, t) -> t 
    | `Apply (_, t) -> t 
    | `Primitive _ -> assert false
    | `Variable (_, t) -> t 
    | `Constant (_, t) -> t 
    | `Box (_, t) -> t 
    | `Unbox (_, t) -> t
    | `Case (_, t) -> t
    | `Wrong t -> t
    | `Let (_, _, t) -> t

  let annotate want (q : tqr) : tqr =
    match (want, typeof_tqr q) with
      | `Atom, `Atom | `List, `List -> q
      | `Atom, `List -> `Box (q, want)
      | `List, `Atom -> `Unbox (q, want)

  let rec enforce_shape env args shape =
    let aux e = function
      | `Any -> transform env e
      | `List -> aot `List env e
      | `Atom -> aot `Atom env e
    in
      List.map2 aux args shape

  and aot want env e = annotate want (transform env e)

  and binding (env, bs) (var, tc) =
    let tc' = transform env tc in
    let env' = Env.Int.bind env (var, typeof_tqr tc') in
    let b = (var, tc') in
      (env', b :: bs)


  and transform env (q : qr) : tqr =
    let enforce_shape = enforce_shape env in
      match q with
	| `Let (bs, tc) ->
	    let env, bs = List.fold_left binding (env, []) bs in
	    let bs = List.rev bs in
	    let tc = transform env tc in
	      `Let (bs, tc, typeof_tqr tc)
	| `Constant c -> `Constant (c, `Atom)
	| `Table t -> `Table (t, `List) 
	| `Inject (tag, t) ->
	    let t' = aot `Atom env t in
	      `Inject ((tag, t'), `Atom)
	| `If (c, t, Some e) -> 
	    let c' = transform env c in
	    let t' = transform env t in
	    let e' = transform env e in
	      `If ((c', t', Some e'), (typeof_tqr t'))
	| `If (c, t, None) ->
	    let c' = transform env c in
	    let t' = transform env t in
	      `If ((c', t', None), (typeof_tqr t'))
	| `Singleton e ->
	    (* row or table? *)
	    `Singleton ((aot `Atom env e), `List)
	| `Concat xs ->
	    let xs' = List.map (aot `List env) xs in
	      `Concat (xs', `List)
	| `Project (label, r) ->
	    let r' = transform env r in
	      `Project ((label, r'), `Atom)
	| `Erase (erase_fields, r) ->
	    `Erase ((erase_fields, (aot `Atom env r)), `Atom)
	| `Extend (ext_fields, r) ->
	    let ext_fields' = StringMap.map (aot `Atom env) ext_fields in
	    let r' = opt_map (transform env) r in
	      `Extend (ext_fields', r', `Atom)
(*
	| `For (source, os, `Lambda ([x], body)) ->
	    let source' = aot `List env source in
	    let env' = Env.Int.bind env (x, `Atom) in
	    let os' = List.map (fun o -> transform env' o) os in
	    let body' = aot `List env' body in
	      `For ((source', os', `Lambda (([x], body'), `Atom)), `List)
*)
	| `Lambda (xs, body) -> 
	    let env' = List.fold_left (fun env' x -> Env.Int.bind env' (x, `Atom)) env xs in
	    let body' = aot `Atom env' body in
	      `Lambda ((xs, body'), `Atom)
	| `Variable x -> 
	    `Variable (x, Env.Int.lookup env x) 
	| `Case (v, cases, default) ->
	    let v' = transform env v in
	    let case (x, c) =
	      let env' = Env.Int.bind env (x, `Atom) in
	      let c' = aot `Atom env' c in
		(x, c')
	    in
	    let cases' = StringMap.map case cases in
	    let default' = opt_map case default in
	      `Case ((v', cases', default'), `Atom)

	| `Wrong -> `Wrong `Atom

	| `Apply ((`Primitive "concatMap"), [`Lambda (xs, body); l]) ->
	    let l' = aot `List env l in
	    let env' = List.fold_left (fun env x -> Env.Int.bind env (x, `Atom)) env xs in
	    let body' = aot `List env' body in
	      `Apply (((`Primitive "concatMap"), [`Lambda ((xs, body'), `List); l']), `List)

(*
	| `Apply ((`Primitive "concatMap"), [f, l] ->
*)

	| `Apply ((`Primitive f), args) ->
	    let fail_arg f = failwith ("Annotate.transform: wrong number of arguments for " ^ f) in
	    let shape, typ =
	      begin
		match f with
		  | "limit" ->
		      begin
			let e : qr =
			  match args with 
			    | [_; _; e] -> e
			    | _ -> fail_arg "limit"
			in
			  [`Atom; `Atom; `Any], (typeof_tqr (transform env e))
		      end
		  | "+" | "+." | "-" | "-." | "*" | "*." 
		  | "/" | "/." | "^^" | "tilde" | "quote" ->
		      (* `Atom -> `Atom -> `Atom *)
		      [`Atom; `Atom], `Atom
		  | "not" ->
		      [`Atom], `Atom
		  | "<>" | "==" | ">" | "<" ->
		      (* arguments can have any type because we can compare
			 atomic values, records and lists. boxed lists are
			 unboxed in compileQuery so we need no annotation
			 here *)
		      (* a -> b -> `Atom *)
		      [`Any; `Any], `Atom
		  | "select" ->
		      (* `Atom -> `List -> `Atom *)
		      [`List; `Atom], `Atom
		  | "take" | "drop" | "dropWhile" | "takeWhile" | "groupByFlat" | "filter" | "sortByFlat" | "map" ->
		      (* `Atom -> `List -> `List *)
		      [`Atom; `List], `List
		  | "zip" ->
		      (* `List -> `List -> `List *)
		      [`List; `List], `List
		  | "length" | "unzip" | "sum" | "and" | "or" | "empty" | "max" | "min" | "avg" | "hd" ->
		      (* `List -> `Atom *)
		      [`List], `Atom
		  | "concat" | "tl" | "nubFlat" | "reverse" ->
		      (* `List -> `List *)
		      [`List], `List
		  | "floatToInt" ->
		      [`Atom], `Atom
		  | _ -> failwith ("Annotate.transform: function " ^ f ^ " not implemented")
	      end
	    in
	    let p : tqr = `Primitive f in
(*	      `Apply ((`Primitive f, enforce_shape args shape), typ) *)
	    let args : tqr list = enforce_shape args shape in
	    let a : tqr = `Apply ((p, args), typ) in
	      a
	| `Apply (e, args) -> 
	    let shape = List.map (fun _ -> `Atom) args in
	    let a : tqr = `Apply ((transform env e, enforce_shape args shape), `Atom) in
	      a
	| e -> failwith ("Query2.Annotate.transform: " ^ (Show.show show_qr e) ^ "not implemented")
	    
end

let optphase q =
  let census = Census.count q in
    Debug.print ("census " ^ (Show.show (IntMap.show_t show_int) census));
    Debug.print ">>>>> inliner";
    let ctx = { Inliner.venv = Env.Int.empty; 
		Inliner.census = census; } 
    in
    let q = Inliner.inline Inliner.conservative ctx q in
    let census = Census.count q in
      Debug.print ("census " ^ (Show.show (IntMap.show_t show_int) census));
      let q = ElimDeadDefs.eliminate census q in
	Debug.print ("inlined\n" ^ (Show.show show_qr q));
	q

let rec applyn f arg n =
  if n = 1 then
    f arg
  else if n > 1 then
    f (applyn f arg (n-1))
  else
    arg

let pipeline env range comp =
  let q = qr_of_query env range comp in
    Debug.print (">>>>> before\n" ^ (Show.show show_qr q));
    let q_opt = applyn optphase q 1 in
    let qt = ImpType.transform Env.Int.empty q_opt in
      Debug.print (">>>>> boxed\n" ^ (Show.show ImpType.show_tqr qt));
      (q, qt)
