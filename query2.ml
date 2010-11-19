(*pp deriving *)
open Utility

(* FIXME: this module needs a serious rewrite *)

(* HACK: global variable which stores the database on which to execute
   the query (or none) *)
let used_database = ref None

type 'a name_map = 'a Utility.stringmap
    deriving (Show)

type name_set = Utility.stringset
    deriving (Show)

type var_set = Utility.intset
    deriving (Show)

module VarSet = Utility.IntSet

type bound_vars = var_set

type t =
    [ `For of t * t list * t
    | `If of t * t * t option
    | `Table of Value.table
    | `Singleton of t | `Append of t list
    | `Record of t name_map 
    | `Project of t * string 
    | `Erase of t * name_set 
    | `Extend of t option * t name_map 
    | `Variant of string * t
    | `XML of Value.xmlitem
    | `Apply of t * t list
    | `Lambda of (Var.var list * t)
    | `Primitive of string
    | `Var of Var.var 
    | `Constant of Constant.constant 
    | `Case of t * (Var.var * t) name_map * (Var.var * t) option
    | `Wrong ]
deriving (Show)

let string_of_t = Show.show show_t

type query_env = t Env.Int.t deriving (Show)
type env = Value.env * query_env

let unbox_xml =
  function
    | `XML xmlitem -> xmlitem
    | _ -> failwith ("failed to unbox XML")

let rec unbox_list =
  function
    | `Append vs -> concat_map unbox_list vs
    | `Singleton v -> [v]
    | _ -> failwith ("failed to unbox list")

let unbox_pair : t -> t * t = function
  | `Extend (None, r) ->
      begin
	try
	  StringMap.find "2" r, StringMap.find "1" r
	with
	    _ -> failwith "failed to unbox pair"
      end
  | _ -> failwith "failed to unbox pair"

(* FIXME: adapt QueryRegex to native strings *)
module QueryRegex = struct

  let is_dotstar p = 
    match unbox_pair p with
      | `Variant ("Any", _), `Variant ("Star", _) -> true
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

  let rec similarify (p : t) : t = 
    match p with
      | `Variant ("Seq", l) -> 
	  let ps = List.map similarify (unbox_list l) in
	    assert ((List.length ps) >= 1);
	    List.fold_left append_patterns (List.hd ps) (drop 1 ps)
      | `Variant ("Range", p) -> 
	  let f, s = unbox_pair p in
	    append_patterns f s
      | `Variant ("Simply", e) ->
	  e
      | `Variant ("Quote", s) ->
	  quote (similarify s)
      | `Variant ("Any", _) -> 
	  `Constant (`String "_")
      | `Variant ("StartAnchor", _) -> 
	  `Constant (`String "")
      | `Variant ("EndAnchor", _) -> 
	  `Constant (`String "")
      | `Variant ("Alternate", p) ->
	  let f, s = unbox_pair p in
	    append_patterns (similarify f) (append_patterns (`Constant (`String "|")) (similarify s))
      | `Variant ("Group", s) ->
	  append_patterns (`Constant (`String "(")) (append_patterns (similarify s) (`Constant (`String ")")))
      | `Variant ("Repeat", p) when is_dotstar p ->
	  `Constant (`String "%")
      | `Variant ("Repeat", p) ->
	  let f, s = unbox_pair p in
	    append_patterns (similarify f) (similarify s)
      | `Variant ("Plus", _) ->
	  `Constant (`String "+")
      | `Variant ("Question", _) ->
	  `Constant (`String "?")
      | `Variant ("Star", _) ->
	  `Constant (`String "*")
      | t -> Debug.print (string_of_t t);
	  assert false
	    end

module Eval =
struct
  exception DbEvaluationError of string

  let rec replace_var old_var new_var e =
    let rep = replace_var old_var new_var in
      match e with
	| `If (c, t, Some e) -> `If (rep c, rep t, Some (rep e))
	| `If (c, t, None) -> `If (rep c, rep t, None)
	| `Singleton e -> `Singleton (rep e)
	| `Append l -> `Append (List.map rep l)
	| `Record fields -> `Record (StringMap.map rep fields)
	| `Project (e, s) -> `Project (rep e, s)
	| `Erase (e, labels) -> `Erase (rep e, labels)
	| `Extend (record, ext_fields) -> `Extend (opt_map rep record, StringMap.map rep ext_fields)
	| `Variant (s, t) -> `Variant (s, rep t)
	| `Apply (f, args) -> `Apply (rep f, List.map rep args)
	| `Var v when v = old_var -> `Var new_var
	| n -> n

  let nil = `Append []

  let eval_error fmt = 
    let error msg = raise (DbEvaluationError msg) in
      Printf.kprintf error fmt

  let reduce_append vs =
    let vs' = 
      (concat_map
	 (function
	    | `Append vs -> vs
	    | v -> [v])
	 vs)
    in
      match vs' with
	| [`Singleton v] -> `Singleton v
	| vs -> `Append vs

  let env_of_value_env value_env = (value_env, Env.Int.empty)

  let (++) (venv, eenv) (venv', eenv') =
    Value.shadow venv ~by:venv', Env.Int.extend eenv eenv'  

  let bind (val_env, exp_env) (x, v) =
    (val_env, Env.Int.bind exp_env (x, v))

  let identity_exp x = `Lambda ([x], `Var x)

  let is_identity_exp f = 
    match f with
      | `Lambda ([x], `Var x') when x = x' -> true
      | _ -> false

  let prelude_venv = ref None

  let reverse_prelude_nenv nenv =
    Env.String.fold 
      (fun name var venv -> Env.Int.bind venv (var, name))
      nenv
      Env.Int.empty

  let prelude_primitives = 
    List.fold_right 
      StringSet.add 
      ["concatMap"; "map"; "sortByBase"; "asList"; "zip"; "unzip"; 
       "nth"; "groupByBase"; "sum"; "concat"; "and"; "or"; "all";
       "any"; "max"; "min"; "avg"; "takeWhile"; "dropWhile"; "nubBase";]
      StringSet.empty

  let rec lookup bound (val_env, exp_env) var =
    if VarSet.mem var bound then
      `Var var
    else
      try
	begin
	  match Value.lookup var val_env, Env.Int.find exp_env var with
	    | None, Some v -> v
	    | Some v, None -> expression_of_value (val_env, exp_env) bound v
	    | None, None -> expression_of_value (val_env, exp_env) bound (Lib.primitive_stub (Lib.primitive_name var))
	    | Some _, Some v -> v (*eval_error "Variable %d bound twice" var*)
	end
      with
	  (NotFound x) ->
	    (*Debug.print (Show.show VarSet.show_t bound); *)
	    raise (NotFound x)
	    

  and expression_of_value env bound : Value.t -> t =
    function
      | `Bool b -> `Constant (`Bool b)
      | `Int i -> `Constant (`Int i)
      | `Char c -> `Constant (`Char c)
      | `Float f -> `Constant (`Float f)
      | `Table (((db, _), _, _, _) as t) -> 
	  used_database := Some db;
	  `Table t 
      | `List vs ->
          `Append (List.map (fun v -> `Singleton (expression_of_value env bound v)) vs)
      | `Record fields ->
          `Record
            (List.fold_left
               (fun fields (name, v) -> StringMap.add name (expression_of_value env bound v) fields)
               StringMap.empty
               fields)
      | `Variant (name, v) -> `Variant (name, expression_of_value env bound v)
      | `XML xmlitem -> `XML xmlitem
      | `RecFunction ([(f, (xs, body))], locals, f', _scope) ->
	  
          assert (f=f');
	  let lambda xs body =
	    let (valenv, tenv) = env in
	    let valenv' = Value.shadow valenv ~by:locals in
	    let env' = (valenv', tenv) in
	    let bound' = VarSet.union bound (VarSet.from_list xs) in
	    let body = computation env' bound' None body in
	      `Lambda (xs, body)
	  in
	    begin

	      (* check if the `RecFunction is actually a prelude function
		 that is a Ferry primitive function *)
	      match Env.Int.find (val_of !prelude_venv) f with
		| Some name ->
		    if StringSet.mem name prelude_primitives then
		      let vars_t = List.map (fun x -> `Var x) xs in
			`Lambda (xs, `Apply ((`Primitive name), vars_t))
		    else
		      lambda xs body
		| None ->
		    lambda xs body
	    end
	      
      (* FIXME: what is the second tuple component (Var.var option)? *)
      | `PrimitiveFunction (f, _) -> 
	  (* eta-expand primitive functions so that they can be used as first-class values *)
	  let arity = val_of (Lib.primitive_arity f) in
	  let fresh_vars = List.map (fun _ -> Var.fresh_raw_var ()) (fromTo 0 arity) in
	  let fresh_vars_t = List.map (fun x -> `Var x) fresh_vars in
	    `Lambda (fresh_vars, `Apply ((`Primitive f), fresh_vars_t))
      | _ -> failwith "Cannot convert value to expression"

  and value env bound : Ir.value -> t = function
    | `Constant c -> `Constant c
    | `Variable var ->
	(* Debug.f "lookup var %d" var; *)
	if VarSet.mem var bound then
	  begin  
	    (* Debug.f "%d is bound" var; *)
	    `Var var
	  end
	else
	  begin
	    (* Debug.f "%d is not bound" var; *)
	    lookup bound env var
	  end
    | `Extend (ext_fields, r) -> 
	let r = opt_map (value env bound) r in
	  `Extend (r, (StringMap.fold 
			 (fun label v fields -> StringMap.add label (value env bound v) fields)
			 ext_fields
			 StringMap.empty))
    | `Project (label, r) ->
        `Project (value env bound r, label)
    | `Erase (labels, r) ->
        `Erase (value env bound r, labels)
    | `Inject (label, v, _t) -> `Variant (label, value env bound v)
    | `TAbs (_, v) -> value env bound v
    | `TApp (v, _) -> value env bound v

    (* FIXME: handle XmlNode somehow *)
    | `XmlNode _ -> assert false

    (*
    | `XmlNode (tag, attrs, children) ->
        (* TODO: deal with variables in XML *)
        let children =
          List.fold_right
            (fun v children ->
               let v = value env v in
                 List.map unbox_xml (unbox_list v) @ children)
            children [] in
        let children =
          StringMap.fold
            (fun name v attrs ->
               Value.Attr (name, unbox_string (value env v)) :: attrs)
            attrs children
        in
          `Singleton (`XML (Value.Node (tag, children)))
    *)

    | `ApplyPure (f, ps) -> 
        apply env bound (value env bound f, List.map (value env bound) ps)
    | `Coerce (v, _) -> value env bound v

  and beta_reduce bound (parameters, body) env args =
    let env = List.fold_left bind env (List.combine parameters args) in
    let rec reduce bound e = 
      match e with
	| `Var x -> lookup bound env x 
	| `For (l, os, `Lambda ([x], body)) -> 
	    let l' = reduce bound l in
	    let os' = List.map (reduce (VarSet.add x bound)) os in
	    let body' = reduce bound (`Lambda ([x], body)) in
	      `For (l', os', body')
	| `If (c, t, e) -> `If (reduce bound c, reduce bound t, opt_map (reduce bound) e)
	| `Singleton t -> `Singleton (reduce bound t)
	| `Append ts -> `Append (List.map (reduce bound) ts)
	| `Record r -> `Record (StringMap.map (reduce bound) r)
	| `Project (r, field) -> `Project (reduce bound r, field)
	| `Erase (r, fields) -> `Erase (reduce bound r, fields)
	| `Extend (r, fields) -> `Extend (opt_map (reduce bound) r, StringMap.map (reduce bound) fields)
	| `Variant (tag, value) -> `Variant (tag, reduce bound value)
	| `Apply (f, args) -> apply env bound (reduce bound f, List.map (reduce bound) args)
	| `Lambda (args, body) ->
	    let bound' = VarSet.union bound (VarSet.from_list args) in
	      `Lambda (args, reduce bound' body)
	| `Case (v, cases, default) -> 
	    let case (x, body) = 
	      let bound' = VarSet.add x bound in
		(x, reduce bound' body) 
	    in
	      `Case (reduce bound v, StringMap.map case cases, opt_map case default)
	| e -> e 
    in
      reduce bound body

  and rewrite_primitive env bound : string * t list -> t = function
    | "AsList", [xs] | "asList", [xs] ->
        xs
    | "Cons", [x; `Append []] ->
	`Singleton x
    | "Cons", [x; xs] ->
	reduce_append [`Singleton x; xs]
    | "Concat", [xs; ys] ->
	reduce_append [xs; ys]
    | "concatMap", [f; xs] -> reduce_for_source env f xs
    | "map", [f; xs] ->
        begin
          match f with
            | `Lambda ([x], body) ->
		reduce_for_source env (`Lambda ([x], `Singleton body)) xs
            | _ -> assert false
        end
    | "sortByBase", [f; xs] ->
	begin
	  match f with
	    | `Lambda ([x], body) ->
		begin
		    match body with
		      | `Extend (_r, ext_fields) ->
			  let l = StringMap.fold (fun k v l -> (int_of_string k, v) :: l) ext_fields [] in
			  let l = List.sort compare l in
			  let os = List.map snd l in
			    `For (xs, os, identity_exp x)
		      | _ -> assert false
		  end
	    | _ -> assert false
	end
    | "all", [p; l] ->
	(* all(p, l) = and(map p l) *)
	`Apply (`Primitive "and", [(apply env bound (`Primitive "Map", [p; l]))])
    | "any", [p; l] -> 
	(* any(p, l) = or(map p l) *)
	`Apply (`Primitive "or", [(apply env bound (`Primitive "Map", [p; l]))])
    | "<", [e1; e2] ->
	`Apply (`Primitive ">", [e2; e1])
    | ">=", [e1; e2] ->
	`If (`Apply (`Primitive ">", [e1; e2]),
	     `Constant (`Bool true),
	     Some (`Apply (`Primitive "==", [e1; e2])))
    | "<=", [e1; e2] ->
	`If (`Apply (`Primitive ">", [e2; e1]),
	     `Constant (`Bool true),
	     Some (`Apply (`Primitive "==", [e1; e2])))
    | "tilde", [s; p] -> 
	let pattern = QueryRegex.similarify p in
	`Apply (`Primitive "tilde", [s; pattern])
    | f, args ->
	`Apply (`Primitive f, args)

  and apply env bound : t * t list -> t = function
    | `Lambda (parameters, body), args -> beta_reduce bound (parameters, body) env args
(*    | `Lambda (parameters, body), args -> `Apply ((`Lambda (parameters, body)), args)  *)
    | `Primitive f, args -> rewrite_primitive env bound (f, args) 
    | `Var x, args -> `Apply (lookup bound env x, args)
    | `If (c, t, e), args -> 
	let e' = opt_map (fun e -> apply env bound (e, args)) e in
	`If (c, apply env bound (t, args), e')
    | `Case (v, cases, default), args -> 
	let case (var, body) =
	  let bound' = VarSet.add var bound in
	  (var, apply env bound' (body, args))
	in
	let cases' = StringMap.map case cases in
	let default' = opt_map case default in
	  `Case (v, cases', default')
    | f, args -> `Apply (f, args)

  and computation env bound range (binders, tailcomp) : t =
    match binders with
      | [] -> 
	  (* Debug.print "tailcomp"; *)
	  tail_computation env bound range tailcomp
      | b::bs ->
          begin
            match b with
              | `Let (xb, (_, tc)) ->
                  let x = Var.var_of_binder xb in
		  let value = tail_computation env bound None tc in
		    (* Debug.f "let %d -> %s" x (string_of_t value); *)
		    computation (bind env (x, value)) bound range (bs, tailcomp)
              | `Fun ((_f, _) as _fb, (_, _args, _body), (`Client | `Native)) ->
                  eval_error "Client function"
              | `Fun ((f, _) as _fb, (_, args, body), _) ->
		  Debug.f "fun %d" f;
		  let arg_vars = List.map fst args in
		  let bound' = VarSet.union bound (VarSet.from_list arg_vars) in
		  let body = computation env bound' None body in
		  let lambda = `Lambda (arg_vars, body) in
		    (* Debug.f "fun %d -> %s" f (string_of_t lambda); *)
		    computation
		      (bind env (f, lambda))
		      bound
		      range
		      (bs, tailcomp)
              | `Rec _defs ->
                  eval_error "Recursive function"
              | `Alien _ 
              | `Alias _ -> (* just skip it *)
                  computation env bound range (bs, tailcomp)
              | `Module _ -> failwith "Not implemented modules yet"
          end

  and tail_computation env bound range tailcomp : t =
    let tc = function
      | `Return v -> value env bound v
      | `Apply (f, args) ->
	  let f = value env bound f in
	  let args = List.map (value env bound) args in
	    (* Debug.f "apply %s\n %s" (string_of_t f) ("[" ^ (mapstrcat ", " string_of_t args) ^ "]"); *)
	    apply env bound (f, args)
      | `Special (`Query (None, e, _)) -> computation env bound range e
      | `Special `Wrong _ -> `Wrong
      | `Special _s -> failwith "special not allowed in query block"
      | `Case (v, cases, default) ->
	  let v' = value env bound v in
	  let case ((x, _), c) = (x, computation env (VarSet.add x bound) None c) in
	  let cases' = StringMap.map case cases in
	  let default' = opt_map case default in
	    `Case (v', cases', default')

      | `If (c, t, e) ->
          let c = value env bound c in
          let t = computation env bound None t in
          let e = computation env bound None e in
	    match e with
	      | `Append [] -> `If (c, t, None)
	      | e -> `If (c, t, Some e)
    in
    let exp = tc tailcomp in
      match range with
	| None -> exp
	| Some (limit, offset) ->
	    `Apply (`Primitive "limit", [`Constant (`Int limit); `Constant (`Int offset); exp])

  and reduce_for_source _env f source =
    let x, _body =
      match f with
	| `Lambda ([x], body) -> x, body
	| _ -> assert false
    in
    let for_expr = 
      match source with
	  (* merge for-comprehension with its orderby clause *)
	| `For (source', ((_ :: _) as os), (`Lambda ([y], _) as f')) when is_identity_exp f' ->
	    `For (source', (List.map (replace_var y x) os), f)
        | `Singleton _ | `Append _ | `If _ | `For _ | `Table _ | `Apply _ | `Project _  | `Var _ ->
	    `For (source, [], f)
        | v -> eval_error "Bad source in for comprehension: %s" (string_of_t v)
    in
      match for_expr with
	(* use an optimization if the else branch is the empty list. 
	   this occurs for where-clauses in for-comprehensions *)
	| `For (source, os, `Lambda ([x], `If (c, t, (Some (`Append []))))) ->
	    `For (source, os, `Lambda ([x], `If (c, t, None)))
	| e -> 
	    e

  let eval env range e = 
    prelude_venv := Some (reverse_prelude_nenv (val_of !Lib.prelude_nenv));
    computation (env_of_value_env env) VarSet.empty range e
end

module Annotate = struct
  type implementation_type = [`Atom | `List] deriving (Show)

  type typed_t =
      [ `For of (typed_t * typed_t list * typed_t) * implementation_type
      | `Lambda of ((Var.var list * typed_t) * implementation_type)
      | `If of (typed_t * typed_t * typed_t option) * implementation_type
      | `Table of Value.table * implementation_type
      | `Singleton of typed_t * implementation_type 
      | `Append of typed_t list * implementation_type
      | `Record of typed_t name_map * implementation_type
      | `Project of (typed_t * string) * implementation_type
      | `Erase of (typed_t * name_set) * implementation_type
      | `Extend of (typed_t option * typed_t name_map) * implementation_type
      | `Variant of (string * typed_t) * implementation_type
      | `XML of Value.xmlitem * implementation_type
      | `Apply of (typed_t * typed_t list) * implementation_type
      | `Primitive of string 
      | `Var of Var.var * implementation_type
      | `Constant of Constant.constant * implementation_type
      | `Box of typed_t * implementation_type
      | `Unbox of typed_t * implementation_type
      | `Case of (typed_t * (Var.var * typed_t) name_map * (Var.var * typed_t) option) * implementation_type
      | `Wrong of implementation_type ]
  deriving (Show)

  let string_of_typed_t = Show.show show_typed_t

  let typeof_typed_t = function
    | `For (_, t) -> t
    | `Lambda (_, t) -> t
    | `If (_, t) -> t 
    | `Table (_, t) -> t 
    | `Singleton (_, t) -> t
    | `Append (_, t) -> t 
    | `Record (_, t) -> t 
    | `Project (_, t) -> t 
    | `Erase (_, t) -> t 
    | `Extend (_, t) -> t
    | `Variant (_, t) -> t 
    | `XML (_, t) -> t 
    | `Apply (_, t) -> t 
    | `Primitive _ -> assert false
    | `Var (_, t) -> t 
    | `Constant (_, t) -> t 
    | `Box (_, t) -> t 
    | `Unbox (_, t) -> t
    | `Case (_, t) -> t
    | `Wrong t -> t

  let annotate want (expression : typed_t) : typed_t =
    match (want, typeof_typed_t expression) with
      | `Atom, `Atom | `List, `List -> expression
      | `Atom, `List -> `Box (expression, want)
      | `List, `Atom -> `Unbox (expression, want)

  let rec enforce_shape env args shape =
    let aux e = function
      | `Any -> transform env e
      | `List -> aot `List env e
      | `Atom -> aot `Atom env e
    in
      List.map2 aux args shape

  and aot want env e = annotate want (transform env e)

  and transform env (expression : t) : typed_t =
    let enforce_shape = enforce_shape env in
    match expression with
      | `Constant c -> `Constant (c, `Atom)
      | `Table t -> `Table (t, `List) 
      | `Variant (tag, t) ->
	  let t' = transform env t in
	    `Variant ((tag, t'), `Atom)
      | `If (c, t, Some e) -> 
	  let c' = transform env c in
	  let t' = transform env t in
	  let e' = transform env e in
	    `If ((c', t', Some e'), (typeof_typed_t t'))
      | `If (c, t, None) ->
	  let c' = transform env c in
	  let t' = transform env t in
	    `If ((c', t', None), (typeof_typed_t t'))
      | `Singleton e ->
	  (* row or table? *)
	  `Singleton ((aot `Atom env e), `List)
      | `Append xs ->
	  let xs' = List.map (aot `List env) xs in
	    `Append (xs', `List)
      | `Project (e, field) ->
	  let e' = transform env e in
	    `Project ((e', field), `Atom)
      | `Record fieldmap ->
	  `Record ((StringMap.map (aot `Atom env) fieldmap), `Atom)
      | `Erase (r, erase_fields) ->
	  `Erase (((aot `Atom env r), erase_fields), `Atom)
      | `Extend (r, ext_fields) ->
	  let ext_fields' = StringMap.map (aot `Atom env) ext_fields in
	  let r' = opt_map (fun r -> transform env r) r in
	    `Extend ((r' ,ext_fields'), `Atom)
      | `For (source, os, `Lambda ([x], body)) ->
	  let source' = aot `List env source in
	  let env' = Env.Int.bind env (x, `Atom) in
	  let os' = List.map (fun o -> transform env' o) os in
	  let body' = aot `List env' body in
	    `For ((source', os', `Lambda (([x], body'), `Atom)), `List)
      | `Lambda (xs, body) -> 
	  let env' = List.fold_left (fun env' x -> Env.Int.bind env' (x, `Atom)) env xs in
	  let body' = aot `Atom env' body in
	    `Lambda ((xs, body'), `Atom)
      | `Var x -> 
	  `Var (x, Env.Int.lookup env x) 
      | `Case (v, cases, default) ->
	  let v' = transform env v in
	  let typ = typeof_typed_t v' in
	  let case (x, c) =
	    let env' = Env.Int.bind env (x, typ) in
	    let c' = transform env' c in
	      (x, c')
	  in
	  let cases' = StringMap.map case cases in
	  let default' = opt_map case default in
	  let case_typ = 
	    (* there must be at least one case or a default case *)
	    match default' with
	      | Some (_, c) -> typeof_typed_t c
	      | None ->
		  (match StringMap.to_list (fun _ c -> c) cases' with
		     | (_, c) :: _ -> typeof_typed_t c
		     | [] -> assert false)
	  in
	    `Case ((v', cases', default'), case_typ)

      | `Wrong -> `Wrong `Atom


      | `Apply ((`Primitive f), args) ->
	  let fail_arg f = failwith ("Annotate.transform: wrong number of arguments for " ^ f) in
	  let shape, typ =
	    begin
	      match f with
		| "limit" ->
		    let e =
		      match args with 
			| [_; _; e] -> e
			| _ -> fail_arg "limit"
		    in
		      [`Atom; `Atom; `Any], (typeof_typed_t (transform env e))
		| "+" | "+." | "-" | "-." | "*" | "*." 
		| "/" | "/." | "^^" | "not" | "tilde" | "quote" -> 
		    (* `Atom -> `Atom -> `Atom *)
		      [`Atom; `Atom], `Atom
		| "<>" | "==" | ">" ->
		    (* arguments can have any type because we can compare
		       atomic values, records and lists. boxed lists are
		       unboxed in compileQuery so we need no annotation
		       here *)
		    (* a -> b -> `Atom *)
		      [`Any; `Any], `Atom
		| "nth" ->
		    (* `Atom -> `List -> `Atom *)
		    [`Atom; `List], `Atom
		| "take" | "drop" | "dropWhile" | "takeWhile" | "groupByBase" ->
		    (* `Atom -> `List -> `List *)
		    [`Atom; `List], `List
		| "zip" ->
		    (* `List -> `List -> `List *)
		    [`List; `List], `List
		| "length" | "unzip" | "sum" | "and" | "or" | "empty" | "max" | "min" | "avg" | "hd" ->
		    (* `List -> `Atom *)
		    [`List], `Atom
		| "concat" | "tl" | "nubBase" ->
		    (* `List -> `List *)
		    [`List], `List
		| "floatToInt" ->
		    [`Atom], `Atom
		| _ -> failwith ("Annotate.transform: function " ^ f ^ " not implemented")
	    end
	  in
	    `Apply (((`Primitive f), (enforce_shape args shape)), typ)
      | `Apply (e, args) -> 

	  let shape = List.map (fun _ -> `Atom) args in
	  `Apply ((transform env e, enforce_shape args shape), `Atom)
      | e -> failwith ("Query2.Annotate.transform: " ^ (string_of_t e) ^ "not implemented")
	  
end

let compile : Value.env -> (Num.num * Num.num) option -> Ir.computation -> (Annotate.typed_t * Annotate.implementation_type) =
  fun env range e ->
    if Settings.get_value Basicsettings.Ferry.output_ir_dot then
      Irtodot.output_dot e env "ir_query.dot";
    let v = Eval.eval env range e in
    let v_annot = Annotate.transform Env.Int.empty v in
      if Settings.get_value Basicsettings.Ferry.print_backend_expression then
	begin
	  print_endline ("query2:\n "^string_of_t v);
	  print_endline ("query2 annotated:\n "^Annotate.string_of_typed_t v_annot);
	  Exptodot.output_dot v_annot "exp_query.dot"
	end;
      (v_annot, (Annotate.typeof_typed_t v_annot))
