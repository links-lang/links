(*pp deriving *)
open Utility

(* HACK: global variable which stores the database on which to execute
   the query (or none) *)
let used_database = ref None

type t =
    [ `For of (Var.var * t) list * t list * t
    | `GroupBy of (Var.var * t) * t
    | `If of t * t * t option
    | `Table of Value.table
    | `Singleton of t | `Append of t list
    | `Record of t StringMap.t | `Project of t * string | `Erase of t * StringSet.t | `Extend of t option * t StringMap.t
    | `Variant of string * t
    | `XML of Value.xmlitem
    | `Apply of string * t list
    | `Closure of (Ir.var list * Ir.computation) * env
    | `Primitive of string
    | `Var of Var.var | `Constant of Constant.constant ]
and env = Value.env * t Env.Int.t
    deriving (Show)

let unbox_xml =
  function
    | `XML xmlitem -> xmlitem
    | _ -> failwith ("failed to unbox XML")

let rec unbox_list =
  function
    | `Append vs -> concat_map unbox_list vs
    | `Singleton v -> [v]
    | _ -> failwith ("failed to unbox list")

let unbox_string =
  function
    | `Constant (`String s) -> s
    | (`Append _ | `Singleton _) as v ->
        implode
          (List.map
             (function
                | `Constant (`Char c) -> c
                | _ -> failwith ("failed to unbox string"))
             (unbox_list v))
    | _ -> failwith ("failed tounbox_string")

(** Returns which database was used if any.

   Currently this assumes that at most one database is used.
*)
(*
let used_database v : Value.database option =
  let rec generators =
    function
      | [] -> None
      | (_x, source)::gs ->
          begin
            match used source with
              | None -> generators gs
              | Some db -> Some db
          end
  and used =
    function
      | `For (gs, _os, _body) -> generators gs
      | `Table ((db, _), _, _) -> Some db
      | _ -> None in
  let rec comprehensions =
    function
      | [] -> None
      | v::vs ->
          begin
            match used v with
              | None -> comprehensions vs
              | Some db -> Some db
          end
  in
    match v with
      | `Append vs -> comprehensions vs
      | v -> used v
*)

module S =
struct
  (** [pt]: A printable version of [t] *)
  type pt =
    [ `For of (Var.var * pt) list * pt list * pt
    | `GroupBy of (Var.var * pt) * pt
    | `If of pt * pt * pt option
    | `Table of Value.table
    | `Singleton of pt | `Append of pt list
    | `Record of pt StringMap.t | `Project of pt * string | `Erase of pt * StringSet.t | `Extend of pt option * pt StringMap.t
    | `Variant of string * pt
    | `XML of Value.xmlitem
    | `Apply of string * pt list
    | `Lam of Ir.var list * Ir.computation
    | `Primitive of string
    | `Var of Var.var | `Constant of Constant.constant ]
      deriving (Show)

  let rec pt_of_t : t -> pt = fun v ->
    let bt = pt_of_t in
      match v with
        | `For (gs, os, b) -> 
            `For (List.map (fun (x, source) -> (x, bt source)) gs, 
                  List.map bt os, 
                  bt b)
	| `GroupBy ((x, group_exp), source) ->
	    `GroupBy ((x, bt group_exp), bt source)
        | `If (c, t, Some e) -> `If (bt c, bt t, Some (bt e))
        | `If (c, t, None) -> `If (bt c, bt t, None)
        | `Table t -> `Table t
        | `Singleton v -> `Singleton (bt v)
        | `Append vs -> `Append (List.map bt vs)
        | `Record fields -> `Record (StringMap.map bt fields)
	| `Extend (r, ext_fields) -> `Extend (opt_map bt r, StringMap.map bt ext_fields)
        | `Variant (name, v) -> `Variant (name, bt v)
        | `XML xmlitem -> `XML xmlitem
        | `Project (v, name) -> `Project (bt v, name)
        | `Erase (v, names) -> `Erase (bt v, names)
        | `Apply (f, vs) -> `Apply (f, List.map bt vs)
        | `Closure ((xs, e), _) -> `Lam (xs, e)
        | `Primitive f -> `Primitive f
        | `Var v -> `Var v
        | `Constant c -> `Constant c
          
  let t = Show.show show_pt -<- pt_of_t
end
let string_of_t = S.t

(** Return the type of rows associated with a top-level non-empty expression *)
(*
let rec type_of_expression : t -> Types.datatype = fun v ->
  let rec generators env : _ -> Types.datatype Env.Int.t =
    function
      | [] -> env
      | (x, `Table (_, _, row))::gs ->
          generators (Env.Int.bind env (x, `Record row)) gs
      | _ -> assert false in
  let rec base env : t -> Types.datatype =
    function
      | `Constant (`Bool _) -> Types.bool_type
      | `Constant (`Int _) -> Types.int_type
      | `Constant (`Char _) -> Types.char_type
      | `Constant (`Float _) -> Types.float_type
      | `Constant (`String _) -> Types.string_type
      | `Project (`Var x, name) ->
          TypeUtils.project_type name (Env.Int.lookup env x)
      | `If (_, t, _) -> base env t
      | `Apply (f, _) -> TypeUtils.return_type (Env.String.lookup Lib.type_env f)
      | `Append (xs) when List.for_all
          (function `Singleton `Constant `Char _ -> true|_->false) xs ->
          Types.string_type
      | e -> Debug.print(Show.show show_t e); assert false in
  let record env fields : Types.datatype =
    Types.make_record_type (StringMap.map (base env) fields) in
  let rec tail env : t -> Types.datatype =
    function
      | `Singleton (`Record fields) -> record env fields
      | `If (_c, t, `Append []) -> tail env t
      | `Table (_, _, row) -> `Record row
      | _ -> assert false
  in
    match v with
      | `Append (v :: _) -> type_of_expression v
      | `For (gens, _os, body) -> tail (generators Env.Int.empty gens) body
      | _ -> tail Env.Int.empty v
*)
let rec value_of_expression : t -> Value.t = fun v ->
  let ve = value_of_expression in
  let value_of_singleton = fun s ->
    match s with
      | `Singleton v -> ve v
      | _ -> assert false
  in
    match v with
      | `Constant (`Bool b) -> `Bool b
      | `Constant (`Int i) -> `Int i
      | `Constant (`Char c) -> `Char c
      | `Constant (`Float f) -> `Float f
      | `Constant (`String s) -> Value.box_string s
      | `Table t -> `Table t
      | `Append vs -> `List (List.map value_of_singleton vs)
      | `Variant (name, v) -> `Variant (name, ve v)
      | `XML xmlitem -> `XML xmlitem
      | `Record fields ->
          `Record (List.rev (StringMap.fold (fun name v fields ->
                                               (name, ve v)::fields) 
                               fields []))
      | _ -> assert false

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
	| `Apply (f, args) -> `Apply (f, List.map rep args)
	| `Var v when v = old_var -> `Var new_var
	| n -> n

  let nil = `Append []

  (* takes a normal form expression and returns true iff it has list type *)
  let is_list =
    function
      | `For _
      | `Table _
      | `Singleton _
      | `Append _
      | `If (_, _, `Append []) -> true
      | _ -> false    

  let eval_error fmt = 
    let error msg = raise (DbEvaluationError msg) in
      Printf.kprintf error fmt

  let env_of_value_env value_env = (value_env, Env.Int.empty)
  let (++) (venv, eenv) (venv', eenv') =
    Value.shadow venv ~by:venv', Env.Int.extend eenv eenv'  

  let rec expression_of_value : Value.t -> t =
    function
      | `Bool b -> `Constant (`Bool b)
      | `Int i -> `Constant (`Int i)
      | `Char c -> `Constant (`Char c)
      | `Float f -> `Constant (`Float f)
      | `Table (((db, _), _, _, _) as t) -> 
	  used_database := Some db;
	  `Table t 
      | `List vs ->
          `Append (List.map (fun v -> `Singleton (expression_of_value v)) vs)
      | `Record fields ->
          `Record
            (List.fold_left
               (fun fields (name, v) -> StringMap.add name (expression_of_value v) fields)
               StringMap.empty
               fields)
      | `Variant (name, v) -> `Variant (name, expression_of_value v)
      | `XML xmlitem -> `XML xmlitem
      | `RecFunction ([(f, (xs, body))], env, f', _scope) ->
          assert (f=f');
          `Closure ((xs, body), env_of_value_env env)
      | `PrimitiveFunction f -> `Primitive f
          (*     | `NativeString of string ] *)
          (*     | `ClientFunction f ->  *)
          (*     | `Continuation cont ->  *)
      | _ -> failwith "Cannot convert value to expression"

  let bind (val_env, exp_env) (x, v) =
    (val_env, Env.Int.bind exp_env (x, v))

  let lookup (val_env, exp_env) var =
    match Value.lookup var val_env, Env.Int.find exp_env var with
      | None, Some v -> v
      | Some (`RecFunction ([(_, _)], _, f, _)), None when Env.String.lookup (val_of !Lib.prelude_nenv) "concatMap" = f ->
          `Primitive "ConcatMap"
      | Some (`RecFunction ([(_, _)], _, f, _)), None when Env.String.lookup (val_of !Lib.prelude_nenv) "map" = f ->
          `Primitive "Map"
      | Some (`RecFunction ([(_, _)], _, f, _)), None when Env.String.lookup (val_of !Lib.prelude_nenv) "sortByBase" = f ->
          `Primitive "SortBy"
      | Some (`RecFunction ([(_, _)], _, f, _)), None when Env.String.lookup (val_of !Lib.prelude_nenv) "asList" = f ->
	  `Primitive "AsList"
      | Some (`RecFunction ([(_, _)], _, f, _)), None when Env.String.lookup (val_of !Lib.prelude_nenv) "zip" = f ->
	  `Primitive "zip"
      | Some (`RecFunction ([(_, _)], _, f, _)), None when Env.String.lookup (val_of !Lib.prelude_nenv) "unzip" = f ->
	  `Primitive "unzip"
      | Some (`RecFunction ([(_, _)], _, f, _)), None when Env.String.lookup (val_of !Lib.prelude_nenv) "nth" = f ->
	  `Primitive "nth"
      | Some (`RecFunction ([(_, _)], _, f, _)), None when Env.String.lookup (val_of !Lib.prelude_nenv) "groupBy" = f ->
	  `Primitive "groupBy"
      | Some (`RecFunction ([(_, _)], _, f, _)), None when Env.String.lookup (val_of !Lib.prelude_nenv) "sum" = f ->
	  `Primitive "sum"
      | Some (`RecFunction ([(_, _)], _, f, _)), None when Env.String.lookup (val_of !Lib.prelude_nenv) "concat" = f ->
	  `Primitive "concat"
      | Some (`RecFunction ([(_, _)], _, f, _)), None when Env.String.lookup (val_of !Lib.prelude_nenv) "and" = f ->
	  `Primitive "and"
      | Some v, None -> expression_of_value v
      | None, None -> expression_of_value (Lib.primitive_stub (Lib.primitive_name var))
      | Some _, Some v -> v (*eval_error "Variable %d bound twice" var*)

  let lookup_lib_fun (val_env, _exp_env) var =
    match Value.lookup var val_env with
      | Some v -> expression_of_value v
      | None -> expression_of_value (Lib.primitive_stub (Lib.primitive_name var))

  let rec value env : Ir.value -> t = function
    | `Constant c -> `Constant c
    | `Append xs when List.for_all (* HACKISH: handle Links string constants *)
        (function `Singleton `Constant `Char _c -> true|_->false) xs ->
        `Constant (`String(mapstrcat ""
                             (function `Singleton `Constant `Char x ->
                                string_of_char x)
                             xs))
    | `Variable var ->
        begin
          match lookup env var with
            | `Primitive "Nil" -> nil
            | v -> v
        end
    | `Extend (ext_fields, r) -> 
	let r = opt_map (value env) r in
	  `Extend (r, (StringMap.fold 
		       (fun label v fields -> StringMap.add label (value env v) fields)
		       ext_fields
		       StringMap.empty))
    | `Project (label, r) ->
        `Project (value env r, label)
    | `Erase (labels, r) ->
        `Erase (value env r, labels)
    | `Inject (label, v, _t) -> `Variant (label, value env v)
    | `TAbs (_, v) -> value env v
    | `TApp (v, _) -> value env v

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

    | `ApplyPure (f, ps) -> 
        apply env (value env f, List.map (value env) ps)
    | `Coerce (v, _) -> value env v

  and apply env : t * t list -> t = function
    | `Closure ((xs, body), closure_env), args ->
        let env = env ++ closure_env in
        let env = List.fold_right2 (fun x arg env ->
                                      bind env (x, arg)) xs args env in
          computation env body
    | `Primitive "AsList", [xs] ->
        xs
    | `Primitive "Cons", [x; `Append []] ->
	`Singleton x
    | `Primitive "Cons", [x; xs] ->
	`Append [`Singleton x; xs]
    | `Primitive "Concat", [xs; ys] ->
	`Append [xs; ys]
    | `Primitive "ConcatMap", [f; xs] ->
        begin
          match f with
            | `Closure (([x], body), closure_env) ->
                let env = env ++ closure_env in
                  reduce_for_source
                    env
                    (fun env (x, v, body) -> computation (bind env (x, v)) body)
                    (x, xs, body)
            | _ -> assert false
        end
    | `Primitive "Map", [f; xs] ->
        begin
          match f with
            | `Closure (([x], body), closure_env) ->
                let env = env ++ closure_env in
                  reduce_for_source
                    env
                    (fun env (x, v, body) -> `Singleton (computation (bind env (x, v)) body)) 
                    (x, xs, body)
            | _ -> assert false
        end
    | `Primitive "SortBy", [f; xs] ->
	begin
	  match f with
	    | `Closure (([x], body), closure_env) ->
		let env = env ++ closure_env in
		let os_fun = computation (bind env (x, `Var x)) body in
		  begin
		    match os_fun with
		      | `Extend (_r, ext_fields) ->
			  let l = StringMap.fold (fun k v l -> (int_of_string k, v) :: l) ext_fields [] in
			  let l = List.sort compare l in
			  let os = List.map snd l in
			    `For ([x, xs], os, `Var x)
		      | _ -> assert false
		  end
	    | _ -> assert false
	end
    | `Primitive "groupBy", [f; source] ->
	begin
	  match f with
	    | `Closure (([x], body), closure_env) ->
		let env = env ++ closure_env in
		let group_exp = computation (bind env (x, `Var x)) body in
		  `GroupBy ((x, group_exp), source)
	    | _ -> assert false
	end
    | `Primitive "<", [e1; e2] ->
	`Apply (">", [e2; e1])
    | `Primitive ">=", [e1; e2] ->
	`If (`Apply (">", [e1; e2]),
	     `Constant (`Bool true),
	     Some (`Apply ("==", [e1; e2])))
    | `Primitive "<=", [e1; e2] ->
	`If (`Apply (">", [e2; e1]),
	     `Constant (`Bool true),
	     Some (`Apply ("==", [e1; e2])))
    | `Primitive "tl", args ->
	`Apply ("drop", (`Constant (`Int (Num.Int 1))) :: args)
    | `Primitive "hd", args ->
	(* need to apply nth to take in order to have the correct implementation type `Atom *)
	`Apply ("nth", [`Constant (`Int (Num.Int 1)); `Apply ("take", (`Constant (`Int (Num.Int 1))) :: args)])
    | `Primitive f, args ->
        `Apply (f, args)

    | `If (c, t, Some e), args ->
        `If (c, apply env (t, args), Some (apply env (e, args)))
    | `If (c, t, None), args ->
	`If (c, apply env (t, args), None)
    | `Apply (f, args), args' ->
        `Apply (f, args @ args')
(*    | `Closure (bound_vars, computation),  *)
    | (f, args) -> 
	List.iter (fun t -> Debug.print (string_of_t t)) (f :: args);
	eval_error "Application of non-function"
  and computation env (binders, tailcomp) : t =
    match binders with
      | [] -> tail_computation env tailcomp
      | b::bs ->
          begin
            match b with
              | `Let (xb, (_, tc)) ->
                  let x = Var.var_of_binder xb in
                    computation (bind env (x, tail_computation env tc)) (bs, tailcomp)
              | `Fun ((_f, _) as _fb, (_, _args, _body), (`Client | `Native)) ->
                  eval_error "Client function"
              | `Fun ((f, _) as _fb, (_, args, body), _) ->
                  computation
                    (bind env (f, `Closure ((List.map fst args, body), env)))
                    (bs, tailcomp)
              | `Rec _defs ->
                  eval_error "Recursive function"
              | `Alien _ 
              | `Alias _ -> (* just skip it *)
                  computation env (bs, tailcomp)
              | `Module _ -> failwith "Not implemented modules yet"
          end
  and tail_computation env : Ir.tail_computation -> t = function
    | `Return v -> value env v
    | `Apply (f, args) ->
        apply env (value env f, List.map (value env) args)
    | `Special (`Query (None, e, _)) -> computation env e
    | `Special _s -> failwith "special not allowed in query block"
    | `Case (v, cases, default) ->
        let rec reduce_case (v, cases, default) =
          match v with
            | `Variant (label, v) as w ->
                begin
                  match StringMap.lookup label cases, default with
                    | Some ((x, _), c), _ ->
                        computation (bind env (x, v)) c
                    | None, Some ((z, _), c) ->
                        computation (bind env (z, w)) c
                    | None, None -> eval_error "Pattern matching failed"
                end
            | `If (c, t, Some e) ->
                `If
                  (c,
                   reduce_case (t, cases, default),
                   Some (reduce_case (e, cases, default)))
	    | `If (c, t, None) ->
		`If
		  (c,
		   reduce_case (t, cases, default),
		   None)
            |  _ -> assert false
        in
          reduce_case (value env v, cases, default)
    | `If (c, t, e) ->
        let c = value env c in
        let t = computation env t in
        let e = computation env e in
	  match e with
	    | `Append [] -> `If (c, t, None)
	    | e -> `If (c, t, Some e)
            (*     | `Special (`For (x, source, body)) -> *)
            (*         reduce_for_source env computation (Var.var_of_binder x, value env source, body) *)
  and reduce_for_source env eval_body (x, source, body) =
    let for_expr = 
      match source with
	  (* merge for-comprehension with its orderby clause *)
	| `For ([y, source'], ((_ :: _) as os), (`Var y')) when y = y' ->
	    `For ([x, source'], (List.map (replace_var y x) os), eval_body env (x, `Var x, body))
        | `Singleton _ 
        | `Append _ 
        | `If _ 
        | `For _ 
        | `Table _ 
	| `Apply _
	| `GroupBy _ 
	| `Project _ ->
	    `For ([x, source], [], eval_body env (x, `Var x, body))
        | v -> eval_error "Bad source in for comprehension: %s" (string_of_t v)
    in
      match for_expr with
	(* use an optimization if the else branch is the empty list. 
	   this occurs for where-clauses in for-comprehensions *)
	| `For (gs, os, `If (c, t, (Some (`Append [])))) ->
	    `For (gs, os, `If (c, t, None))
	| e -> 
	    e

  let eval env e = computation (env_of_value_env env) e
end


module Annotate = struct
  type implementation_type = [`Atom | `List]

  type typed_t =
      [ `For of ((Var.var * typed_t) list * typed_t list * typed_t) * implementation_type
      | `GroupBy of ((Var.var * typed_t) * typed_t) * implementation_type
      | `If of (typed_t * typed_t * typed_t option) * implementation_type
      | `Table of Value.table * implementation_type
      | `Singleton of typed_t * implementation_type 
      | `Append of typed_t list * implementation_type
      | `Record of typed_t StringMap.t * implementation_type
      | `Project of (typed_t * string) * implementation_type
      | `Erase of (typed_t * StringSet.t) * implementation_type
      | `Extend of (typed_t option * typed_t StringMap.t) * implementation_type
      | `Variant of (string * typed_t) * implementation_type
      | `XML of Value.xmlitem * implementation_type
      | `Apply of (string * typed_t list) * implementation_type
      | `Primitive of string * implementation_type
      | `Var of Var.var * implementation_type
      | `Constant of Constant.constant * implementation_type
      | `Box of typed_t * implementation_type
      | `Unbox of typed_t * implementation_type ]

  let typeof_typed_t = function
    | `For (_, t) -> t
    | `GroupBy (_, t) -> t
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
    | `Primitive (_, t) -> t
    | `Var (_, t) -> t 
    | `Constant (_, t) -> t 
    | `Box (_, t) -> t 
    | `Unbox (_, t) -> t

  let annotate want (expression : typed_t) : typed_t =
    match (want, typeof_typed_t expression) with
      | `Atom, `Atom | `List, `List -> expression
      | `Atom, `List -> `Box (expression, want)
      | `List, `Atom -> `Unbox (expression, want)

  let rec transform env (expression : t) : typed_t =
    let aot want env = (fun e -> (annotate want (transform env e))) in
    match expression with
      | `Constant c -> `Constant (c, `Atom)
      | `Table t -> `Table (t, `List) 
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
      | `For (gs, os, body) ->
	  let env' = List.fold_left (fun m (x, _) -> Env.Int.bind m (x, `Atom)) env gs in
	  let gs' = List.map (fun (x, source) -> (x, aot `List env source)) gs in
	  let os' = List.map (fun o -> transform env' o) os in
	  let body' = aot `List env' body in
	    `For ((gs', os', body'), `List)
      | `GroupBy ((x, group_exp), source) ->
	  let env' = Env.Int.bind env (x, `Atom) in
	  let group_exp' = aot `Atom env' group_exp in
	  let source' = aot `List env source in
	    `GroupBy (((x, group_exp'), source'), `List)
      | `Var x -> 
	  `Var (x, Env.Int.lookup env x) 
      | `Apply (f, args) ->
	  let fail_arg f = failwith ("Annotate.transform: invalid argument number for " ^ f) in
	  (match f with
	    | "+" | "+." | "-" | "-." | "*" | "*." 
	    | "/" | "/." | "not" -> 
		(* these operators are only ever applied to atomic
		   values, so no need to annotate the arguments *)
		(* `Atom -> `Atom -> `Atom *)
		`Apply ((f, List.map (fun arg -> transform env arg) args), `Atom)
	    | "<>" | "==" | ">" ->
		(* arguments can have any type because we can compare
		   atomic values, records and lists. boxed lists are
		   unboxed in compileQuery so we need no annotation
		   here *)
		(* a -> b -> `Atom *)
		`Apply ((f, List.map (fun arg -> transform env arg) args), `Atom)
	    | "nth" ->
		(* `Atom -> `List -> `Atom *)
		let (n, l) = 
		  (match args with 
		    | [a1; a2] -> (a1, a2)
		    | _ -> fail_arg "nth")
		in
		let n' = transform env n in
		let l' = aot `List env l in
		  `Apply ((f, [n'; l']), `Atom)
	    | "take" | "drop" ->
		(* `Atom -> `List -> `List *)
		let (n, l) = 
		  (match args with 
		     | [a1; a2] -> (a1, a2)
		     | _ -> fail_arg "take/drop")
		in
		let n' = transform env n in
		let l' = aot `List env l in
		  `Apply ((f, [n'; l']), `List)
	    | "length" | "unzip" | "sum" | "and" ->
		(* `List -> `Atom *)
		let l = 
		  (match args with 
		     | [a1] -> a1
		     | _ -> fail_arg "length")
		in
		let l' = aot `List env l in
		  `Apply ((f, [l']), `Atom)
	    | "concat" ->
		(* `List -> `List *)
		let l =
		  (match args with
		     | [a] -> a
		     | _ -> fail_arg "concat")
		in
		let l' = aot `List env l in
		  `Apply ((f, [l']), `List)
	    | "zip" ->
		(* `List -> `List -> `List *)
		let (l, r) =
		  (match args with 
		     | [a1; a2] -> (a1, a2)
		     | _ -> fail_arg "zip")
		in
		let l' = aot `List env l in
		let r' = aot `List env r in
		  `Apply ((f, [l'; r']), `List)
	    | _ -> failwith ("Annotate.transform: function " ^ f ^ " not implemented"))
      | _ -> failwith "Query2.Annotate.transform: not implemented"
    
end

let compile : Value.env -> (Num.num * Num.num) option * Ir.computation -> (Annotate.typed_t * Annotate.implementation_type)=
  fun env (_range, e) ->
    if Settings.get_value Basicsettings.Ferry.output_ir_dot then
      Irtodot.output_dot e env "ir_query.dot";
    let v = Eval.eval env e in
      let v_annot = Annotate.transform Env.Int.empty v in
	if Settings.get_value Basicsettings.Ferry.print_backend_expression then
	  begin
	    print_endline ("query2:\n "^string_of_t v);
	    (* print_endline ("query2 annotated:\n "^string_of_t (fst v_annot)); *)
	  end;
	(v_annot, (Annotate.typeof_typed_t v_annot))
