(*pp deriving *)
open Utility

module NormalForms =
struct
  (*
     This module gives the datatype of normal forms for query
     expressions.

     Instead of using normal forms we use a single datatype t as it
     makes the implementation considerably simpler.

     At some point it might be interesting to try to target the normal
     form directly.
  *)

  type base =
      [ `If of base * base * base
      | `Project of (Var.var * StringSet.t) * string | `Erase of (Var.var * StringSet.t) * string
      | `Apply of string * base list
      | `Constant of Constant.constant ]

  type tail =
      [ `Where of base * tail
      | `SingletonRecord of base StringMap.t ]

  type generator = Var.var * Value.table
  type comprehension = generator list * base list * tail
  type query = comprehension list
end

      
type tag = int
     deriving (Show)

type t =
    [ `For of tag option * (Var.var * t) list * t list * t
    | `If of t * t * t
    | `Table of Value.table
    | `Singleton of t | `Concat of t list
    | `Record of t StringMap.t | `Project of t * string | `Erase of t * StringSet.t
    | `Variant of string * t
    | `XML of Value.xmlitem
    | `Apply of string * t list
    | `Closure of (Ir.var list * Ir.computation) * env
    | `Primitive of string
    | `Var of (Var.var * Types.datatype StringMap.t) | `Constant of Constant.constant ]
and env = Value.env * t Env.Int.t
    deriving (Show)

(* takes a normal form expression and returns true iff it has list type *)
let is_list =
  function
    | `For _
    | `Table _
    | `Singleton _
    | `Concat _
    | `If (_, _, `Concat []) -> true
    | _ -> false    


(* generate a unique tag for each comprehension in
   a normalised query
*)
let tag_query : t -> t =
  fun e ->
    let r = ref 1 in
    let next () =
      let v = !r in
        r := v+1;
        v in
    let rec tag =
      function
        | `For (_, gs, os, body) ->
          `For (Some (next()), gs, os, tag body)
        | `If (c, t, e) ->
          `If (tag c, tag t, tag e)
        | `Table t -> `Table t
        | `Singleton e -> `Singleton (tag e)
        | `Concat es ->
          `Concat (List.map tag es)
        | `Record fields -> `Record (StringMap.map tag fields)
        | `Project (e, l) -> `Project (tag e, l)
        | `Erase (e, fields) -> `Erase (tag e, fields)
        | `Variant (l, e) -> `Variant (l, tag e)
        | `XML v -> `XML v
        | `Apply (f, es) -> `Apply (f, List.map tag es)
        | `Closure ((xs, body), env) -> `Closure ((xs, body), env)
        | `Primitive p -> `Primitive p
        | `Var (x, t) -> `Var (x, t)
        | `Constant c -> `Constant c
    in
      tag e

let tuple xs = `Record (snd
                          (List.fold_left
                             (fun (i, fields) x ->
                               (i+1, StringMap.add (string_of_int i) x fields))
                             (1, StringMap.empty)
                             xs))
let pair x y = tuple [x; y]

module Shred =
struct
  type nested_type =
    [ `Primitive of Types.primitive
    | `Record of nested_type StringMap.t
    | `List of nested_type ]
      deriving (Show)

  type shredded_type =
    [ `Primitive of Types.primitive
    | `Record of shredded_type StringMap.t ]
      deriving (Show)

  type flat_type =
    [ `Primitive of Types.primitive
    | `Record of Types.primitive StringMap.t ]
      deriving (Show)
      
  type 'a package =
    [ `Primitive of Types.primitive
    | `Record of 'a package StringMap.t
    | `List of 'a package * 'a ]
      deriving (Show)

  type step = [ `List | `Record of string ]
  type path = step list

  (* this seems a bit dodgey...  what if we have a polymorphic type,
     for instance?

     maybe we're ok if we always infer the type from a normalised
     expression *)
  let rec nested_type_of_type : Types.typ -> nested_type =
    function
      | `Primitive t -> `Primitive t
      | `Record (fields, _, _) -> `Record (StringMap.map (function
                                                           | `Present t -> nested_type_of_type t
                                                           | _ -> assert false) fields)
      | `Application (l, [`Type t]) when l = Types.list ->
        `List (nested_type_of_type t)
      | _ -> assert false

  (* erase annotations from a package to obtain the underlying type *)
  let rec erase : 'a package -> nested_type =
    function
      | `Primitive t   -> `Primitive t
      | `Record fields -> `Record (StringMap.map erase fields)
      | `List (t, _)   -> `List (erase t)

  (* map over a package *)
  let rec pmap : ('a -> 'b) -> 'a package -> 'b package =
    fun f ->
      function
        | `Primitive t   -> `Primitive t
        | `Record fields -> `Record (StringMap.map (pmap f) fields)
        | `List (t, a)   -> `List (pmap f t, f a)

  (* construct a package using a shredding function f *)
  let package : (path -> 'a) -> nested_type -> 'a package = fun f ->
    let rec package f p =
      function
        | `Primitive t   -> `Primitive t
        | `Record fields -> `Record (StringMap.fold
                                       (fun name t fields ->
                                         StringMap.add name (package f (p @ [`Record name]) t) fields)
                                       fields
                                       StringMap.empty)
        | `List t        -> `List (package f (p @ [`List]) t, f p)
    in
      package f []
      
  let rec pzip : 'a package -> 'b package -> ('a * 'b) package =
    fun p1 p2 ->
      match p1, p2 with
        | `Primitive t1, `Primitive t2 -> `Primitive t1
        | `Record fields1, `Record fields2 ->
          `Record
            (StringMap.fold
               (fun name t1 fields ->
                 let t2 = StringMap.find name fields2 in
                   StringMap.add name (pzip t1 t2) fields)
               fields1
               StringMap.empty)
        | `List (t1, a1), `List (t2, a2) ->
          `List
            (pzip t1 t2, (a1, a2))

  let top = 0

  let rec split_conditions f =
    function
      | `If (c, t, `Concat []) ->
        split_conditions (fun body -> f (`If (c, body, `Concat []))) t
      | `Singleton body -> f, body
      | _ -> assert false

  let static_in = `Primitive "in"
  let static_out = `Primitive "out"

  let dyn_index a = `Constant (`Int a)

  let in_index a = pair (dyn_index a) static_in
  let out_index a = pair (dyn_index a) static_out

  (* inner shredding function *)
  let rec shinner a =
    function
      | `Record fields ->
        `Record (StringMap.map (shinner a) fields)
      | e when is_list e ->
        in_index a
      | e -> e

  (* outer shredding function *)
  let rec shouter a p : t -> t list =
    function
      | `Concat cs ->
        concat_map (shouter a p) cs
      | `Record fields ->
        begin
          match p with
            | (`Record l :: p) ->
              shouter a p (StringMap.find l fields)
            | _ -> assert false                
        end
      | `For (Some b, gs, os, body) ->
        let f, body = split_conditions (fun x -> x) body in        
          begin
            match p with
              | [] ->
                [`For (Some b, gs, os, f (`Singleton (pair (out_index a) (shinner b body))))]
              | (`List :: p) ->
                List.map
                  (fun c -> `For (Some b, gs, os, f c))
                  (shouter b p body)
              | _ -> assert false
          end
      | e ->
         Debug.print ("Can't apply shouter to: " ^ Show_t.show e);
         assert false

  let shred_outer q p = `Concat (shouter top p q)

  let shred_query : t -> nested_type -> t package =
    fun q t -> package (shred_outer q) t


  let rec shred_inner_type : nested_type -> shredded_type =
    function
      | `Primitive p   -> `Primitive p
      | `Record fields -> `Record (StringMap.map shred_inner_type fields)
      | `List t        ->
        `Record
          (StringMap.add "1" (`Primitive `Int)
            (StringMap.add "2" (`Primitive `Int) StringMap.empty))

  let rec shred_outer_type : nested_type -> path -> shredded_type =
    fun t p ->      
      match t, p with
        | `List t, [] ->
          `Record
            (StringMap.add "1"
               (`Record
                   (StringMap.add "1" (`Primitive `Int)
                      (StringMap.add "2" (`Primitive `Int)
                         StringMap.empty)))
               (StringMap.add "2" (shred_inner_type t)
                  StringMap.empty))          
        | `List t, `List :: p ->
          shred_outer_type t p
        | `Record fields, `Record l :: p ->
          shred_outer_type (StringMap.find l fields) p
        | _ -> assert false

  let shred_query_type : nested_type -> shredded_type package =
    fun t -> package (shred_outer_type t) t               

  let rec stitch : Value.t -> Value.t package -> Value.t =
    fun v t ->
      match v, t with
        | c, `Primitive t -> c
        | `Record fs, `Record fts ->
          `Record
            (List.map (fun (l, v) -> (l, stitch v (StringMap.find l fts))) fs)
        | `Record [("1", `Int a); ("2", `Int d)], `List (t, `List vs) ->
          `List
            (List.fold_right
               (function
                 | `Record [("1", (`Record [("1", `Int a'); ("2", `Int d')])); ("2", w)] ->
                   fun ws ->
                     if a = a' && d = d' then
                       (stitch w t) :: ws
                     else
                       ws
                 | _ -> assert false)
               vs
               [])
        | _, _ -> assert false

  let stitch_query : Value.t package -> Value.t =
    fun p ->
      stitch (`Record [("1", `Int top); ("2", `Int 1)]) p
end




let unbox_xml =
  function
    | `XML xmlitem -> xmlitem
    | _ -> failwith ("failed to unbox XML")

let unbox_pair =
  function
    | `Record fields ->
        let x = StringMap.find "1" fields in
        let y = StringMap.find "2" fields in
          x, y
    | _ -> failwith ("failed to unbox pair")

let rec unbox_list =
  function
    | `Concat vs -> concat_map unbox_list vs
    | `Singleton v -> [v]
    | _ -> failwith ("failed to unbox list")

let unbox_string =
  function
    | `Constant (`String s) -> s
    | (`Concat _ | `Singleton _) as v ->
        implode
          (List.map
             (function
                | `Constant (`Char c) -> c
                | _ -> failwith ("failed to unbox string"))
             (unbox_list v))
    | _ -> failwith ("failed to unbox string")

(** Returns which database was used if any.

   Currently this assumes that at most one database is used.
*)
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
      | `For (_, gs, os, _body) -> generators gs
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
      | `Concat vs -> comprehensions vs
      | v -> used v

module S =
struct
  (** [pt]: A printable version of [t] *)
  type pt =
    [ `For of (Var.var * pt) list * pt list * pt
    | `If of pt * pt * pt
    | `Table of Value.table
    | `Singleton of pt | `Concat of pt list
    | `Record of pt StringMap.t | `Project of pt * string | `Erase of pt * StringSet.t
    | `Variant of string * pt
    | `XML of Value.xmlitem
    | `Apply of string * pt list
    | `Lam of Ir.var list * Ir.computation
    | `Primitive of string
    | `Var of (Var.var * Types.datatype StringMap.t) | `Constant of Constant.constant ]
      deriving (Show)

  let rec pt_of_t : t -> pt = fun v ->
    let bt = pt_of_t in
      match v with
        | `For (_, gs, os, b) -> 
            `For (List.map (fun (x, source) -> (x, bt source)) gs, 
                  List.map bt os, 
                  bt b)
        | `If (c, t, e) -> `If (bt c, bt t, bt e)
        | `Table t -> `Table t
        | `Singleton v -> `Singleton (bt v)
        | `Concat vs -> `Concat (List.map bt vs)
        | `Record fields -> `Record (StringMap.map bt fields)
        | `Variant (name, v) -> `Variant (name, bt v)
        | `XML xmlitem -> `XML xmlitem
        | `Project (v, name) -> `Project (bt v, name)
        | `Erase (v, names) -> `Erase (bt v, names)
        | `Apply (f, vs) -> `Apply (f, List.map bt vs)
        | `Closure ((xs, e), _) -> `Lam (xs, e)
        | `Primitive f -> `Primitive f
        | `Var v -> `Var v
        | `Constant c -> `Constant c

  let t = Show_pt.show -<- pt_of_t
end
let string_of_t = S.t

let rec tail_of_t : t -> t = fun v ->
  let tt = tail_of_t in
    match v with
      | `For (_, _gs, _os, `Singleton (`Record fields)) -> `Record fields
      | `For (_tag, _gs, _os, `If (c, t, `Concat [])) -> tt (`For (_tag, _gs, _os, t))
      | _ -> (* Debug.print ("v: "^string_of_t v); *) assert false

(** Return the type associated with an expression *)
(* Inferring the type of an expression is straightforward because all
   variables are annotated with their types. *)
let rec type_of_expression : t -> Types.datatype = fun v ->
  let te = type_of_expression in
  let record fields : Types.datatype =
    Types.make_record_type (StringMap.map te fields)
  in
    match v with
      | `Concat (v::vs) -> te v
      | `For (_, gens, _os, body) -> te body
      | `Singleton t -> Types.make_list_type (te t)
      | `Record fields -> record fields
      | `If (_, t, _) -> te t
      | `Table (_, _, row) -> `Record row
      | `Constant (`Bool b) -> Types.bool_type
      | `Constant (`Int i) -> Types.int_type
      | `Constant (`Char c) -> Types.char_type
      | `Constant (`Float f) -> Types.float_type
      | `Constant (`String s) -> Types.string_type
      | `Project (`Var (x, field_types), name) -> StringMap.find name field_types
      | `Apply ("Empty", _) -> Types.bool_type (* HACK *)
      | `Apply (f, _) -> TypeUtils.return_type (Env.String.lookup Lib.type_env f)
      | e -> Debug.print("Can't deduce type for: " ^ Show_t.show e); assert false

let default_of_base_type : Types.primitive -> t =
  function
    | `Bool   -> `Constant (`Bool false)
    | `Int    -> `Constant (`Int 42)
    | `Char   -> `Constant (`Char '?')
    | `Float  -> `Constant (`Float 0.0)
    | `String -> `Constant (`String "")
    | _       -> assert false

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
      | `Concat vs -> `List (List.map value_of_singleton vs)
      | `Variant (name, v) -> `Variant (name, ve v)
      | `XML xmlitem -> `XML xmlitem
      | `Record fields ->
          `Record (List.rev (StringMap.fold (fun name v fields ->
                                               (name, ve v)::fields)
                               fields []))
      | _ -> assert false

let rec freshen_for_bindings : Var.var Env.Int.t -> t -> t =
  fun env v ->
    let ffb = freshen_for_bindings env in
      match v with
      | `For (tag, gs, os, b) ->
        let gs', env' =
          List.fold_left
            (fun (gs', env') (x, source) ->
              let y = Var.fresh_raw_var () in
                ((y, ffb source)::gs', Env.Int.bind env' (x, y)))
            ([], env)
            gs
        in
          `For (tag, List.rev gs', List.map (freshen_for_bindings env') os, freshen_for_bindings env' b)
      | `If (c, t, e) -> `If (ffb c, ffb t, ffb e)
      | `Table t -> `Table t
      | `Singleton v -> `Singleton (ffb v)
      | `Concat vs -> `Concat (List.map ffb vs)
      | `Record fields -> `Record (StringMap.map ffb fields)
      | `Variant (name, v) -> `Variant (name, ffb v)
      | `XML xmlitem -> `XML xmlitem
      | `Project (v, name) -> `Project (ffb v, name)
      | `Erase (v, names) -> `Erase (ffb v, names)
      | `Apply (f, vs) -> `Apply (f, List.map ffb vs)
      | `Closure c ->
        (* we don't attempt to freshen closure bindings *)
        `Closure c
      | `Primitive f -> `Primitive f
      | `Var (x, ts) ->
        begin
          match Env.Int.find env x with
          | None -> `Var (x, ts)
          | Some y -> `Var (y, ts)
        end
      | `Constant c -> `Constant c

let labels_of_field_types field_types =
  StringMap.fold
    (fun name _ labels' ->
      StringSet.add name labels')
    field_types
    StringSet.empty

let record_field_types (t : Types.datatype) : Types.datatype StringMap.t =
  let (field_spec_map, _, _) = TypeUtils.extract_row t in
  StringMap.map (function
                  | `Present t -> t
                  | _ -> assert false) field_spec_map
  
let table_field_types (_, _, (fields, _, _)) =
  StringMap.map (function
                  | `Present t -> t
                  | _ -> assert false) fields

let rec field_types_of_list =
  function
    | `Concat (v::vs) -> field_types_of_list v
    | `Singleton (`Record fields) -> StringMap.map type_of_expression fields
    | `Table table -> table_field_types table
    | _ -> assert false

module Eval =
struct
  exception DbEvaluationError of string

  let nil = `Concat []

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
      | `String s -> `Constant (`String s)
      | `Table t -> `Table t
      | `List vs ->
          `Concat (List.map (fun v -> `Singleton (expression_of_value v)) vs)
      | `Record fields ->
          `Record
            (List.fold_left
               (fun fields (name, v) -> StringMap.add name (expression_of_value v) fields)
               StringMap.empty
               fields)
      | `Variant (name, v) -> `Variant (name, expression_of_value v)
      | `XML xmlitem -> `XML xmlitem
      | `FunctionPtr (f, env) ->
        (* Debug.print ("Converting function pointer: " ^ string_of_int f ^ " to query closure"); *)
        let (_finfo, (xs, body), _z, _location) as def = Tables.find Tables.fun_defs f in
        `Closure ((xs, body), env_of_value_env env)
      | `PrimitiveFunction (f,_) -> `Primitive f
          (*     | `ClientFunction f ->  *)
          (*     | `Continuation cont ->  *)
      | _ -> failwith "Cannot convert value to expression"

  let bind (val_env, exp_env) (x, v) =
    (val_env, Env.Int.bind exp_env (x, v))

  let lookup (val_env, exp_env) var =
    match Tables.lookup Tables.fun_defs var with
    | Some (finfo, (xs, body), None, location) ->
      begin
        match Var.name_of_binder (var, finfo) with
        | "concatMap" ->
          `Primitive "ConcatMap"
        | "map" ->
          `Primitive "Map"
        | "empty" ->
          `Primitive "Empty"
        | "sortByBase" ->
          `Primitive "SortBy"
        | _ ->
          begin
            match location with
            | `Server | `Unknown ->
              (* Debug.print ("looked up function: "^Var.Show_binder.show (var, finfo)); *)
              `Closure ((xs, body), env_of_value_env Value.empty_env)
            | `Client ->
              failwith ("Attempt to use client function: " ^ Js.var_name_binder (var, finfo) ^ " in query")
            | `Native ->
              failwith ("Attempt to use native function: " ^ Var.Show_binder.show (var, finfo) ^ " in query")
          end
      end
    | None ->
      begin
        match Value.lookup var val_env, Env.Int.find exp_env var with
        | None, Some v -> v
        | Some v, None -> expression_of_value v
        | Some _, Some v -> v (*eval_error "Variable %d bound twice" var*)
        | None, None ->
          begin
            try expression_of_value (Lib.primitive_stub (Lib.primitive_name var)) with
            | NotFound _ -> failwith ("Variable " ^ string_of_int var ^ " not found");
          end
      end

  let lookup_lib_fun (val_env, _exp_env) var =
    match Value.lookup var val_env with
      | Some v -> expression_of_value v
      | None -> expression_of_value (Lib.primitive_stub (Lib.primitive_name var))

  let eta_expand_var (x, field_types) =
    `Record
      (StringMap.fold
         (fun name _t fields ->
            StringMap.add name (`Project (`Var (x, field_types), name)) fields)
         field_types
         StringMap.empty)

  let eta_expand_list xs =
    let x = Var.fresh_raw_var () in
    let field_types = field_types_of_list xs in
      ([x, xs], [], `Singleton (eta_expand_var (x, field_types)))

  let rec value env : Ir.value -> t = function
    | `Constant c -> `Constant c
    | `Variable var ->
        begin
          match lookup env var with
            | `Var (x, field_types) ->
                (* eta-expand record variables *)
                eta_expand_var (x, field_types)
            | `Primitive "Nil" -> nil
            (* We could consider detecting and eta-expand tables here.
               The only other possible sources of table values would
               be `Special or built-in functions that return table
               values. (Currently there are no pure built-in functions
               that return table values.)

               Currently eta-expansion happens later, in the SQL
               module.

               On second thoughts, we *never* need to explicitly
               eta-expand tables, as it is not possible to call
               "AsList" directly. The "asList" function in the prelude
               is defined as:

               fun asList(t) server {for (x <-- t) [x]}
            *)
            | v ->
              (* In order to maintain the invariant that each
                 bound variable is unique we freshen all for-bound
                 variables in v here.

                 This is necessary in order to ensure that each
                 instance of a table in a self-join is given a
                 distinct alias, as the alias is generated from the
                 name of the variable binding the table.

                 We are assuming that any closure-bound variables will
                 be eliminated anyway.
              *)
              (* Debug.print ("env v: "^string_of_int var^" = "^string_of_t v); *)
              freshen_for_bindings (Env.Int.empty) v
        end
    | `Extend (ext_fields, r) ->
      begin
        match opt_app (value env) (`Record StringMap.empty) r with
          | `Record fields ->
            `Record (StringMap.fold
                       (fun label v fields ->
                         if StringMap.mem label fields then
                           eval_error
                             "Error adding fields: label %s already present"
                             label
                         else
                           StringMap.add label (value env v) fields)
                       ext_fields
                       fields)
          | _ -> eval_error "Error adding fields: non-record"
      end
    | `Project (label, r) ->
      let rec project (r, label) =
        match r with
          | `Record fields ->
            assert (StringMap.mem label fields);
            StringMap.find label fields
          | `If (c, t, e) ->
            `If (c, project (t, label), project (e, label))
          | `Var (x, field_types) ->
            assert (StringMap.mem label field_types);
            `Project (`Var (x, field_types), label)
          | _ -> eval_error "Error projecting from record"
      in
        project (value env r, label)
    | `Erase (labels, r) ->
      let rec erase (r, labels) =
        match r with
          | `Record fields ->
            assert (StringSet.for_all
                      (fun label -> StringMap.mem label fields) labels);
            `Record
              (StringMap.fold
                 (fun label v fields ->
                   if StringSet.mem label labels then
                     fields
                   else
                     StringMap.add label v fields)
                 fields
                 StringMap.empty)
          | `If (c, t, e) ->
            `If (c, erase (t, labels), erase (e, labels))
          | `Var (x, field_types) ->
            assert (StringSet.subset labels (labels_of_field_types field_types));
            `Erase (`Var (x, field_types), labels)
          | _ -> eval_error "Error erasing from record"
      in
        erase (value env r, labels)
    | `Inject (label, v, t) -> `Variant (label, value env v)
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
    | `Closure (f, v) ->
      let (_finfo, (xs, body), Some z, _location) = Tables.find Tables.fun_defs f in
      (* Debug.print ("Converting evalir closure: " ^ Var.Show_binder.show (f, _finfo) ^ " to query closure"); *)
      (* yuck! *)
      let env' = bind (Value.empty_env, Env.Int.empty) (z, value env v) in
      `Closure ((xs, body), env')
      (* (\* Debug.print("looking up query closure: "^string_of_int f); *\) *)
      (* begin *)
      (*   match value env (`Variable f) with *)
      (*   | `Closure ((z::xs, body), closure_env) -> *)
      (*     (\* Debug.print("binding query closure parameter: "^string_of_int z); *\) *)
      (*     (\* partially apply the closure to bind the closure *)
      (*        environment *\) *)
      (*     `Closure ((xs, body), bind closure_env (z, value env v)) *)
      (*   | _ -> *)
      (*     failwith "ill-formed closure in query compilation" *)
      (* end *)
    | `Coerce (v, _) -> value env v

  and apply env : t * t list -> t = function
    | `Closure ((xs, body), closure_env), args ->
      (* Debug.print ("Applying closure"); *)
      (* Debug.print ("body: " ^ Ir.Show_computation.show body); *)
      (* Debug.print("Applying query closure: " ^ Show_t.show (`Closure ((xs, body), closure_env))); *)
      (* Debug.print("args: " ^ mapstrcat ", " Show_t.show args); *)
        let env = env ++ closure_env in
        let env = List.fold_right2 (fun x arg env ->
            bind env (x, arg)) xs args env in
        (* Debug.print("Applied"); *)
          computation env body
    | `Primitive "AsList", [xs] ->
        xs
    | `Primitive "Cons", [x; xs] ->
        reduce_concat [`Singleton x; xs]
    | `Primitive "Concat", [xs; ys] ->
        reduce_concat [xs; ys]
    | `Primitive "ConcatMap", [f; xs] ->
        begin
          match f with
            | `Closure (([x], body), closure_env) ->
                let env = env ++ closure_env in
                  reduce_for_source
                    (xs, fun v -> computation (bind env (x, v)) body)
            | _ -> assert false
        end
    | `Primitive "Map", [f; xs] ->
        begin
          match f with
            | `Closure (([x], body), closure_env) ->
                let env = env ++ closure_env in
                  reduce_for_source
                    (xs, fun v -> `Singleton (computation (bind env (x, v)) body))
            | _ -> assert false
        end
    | `Primitive "SortBy", [f; xs] ->
        begin
          match xs with
            | `Concat [] -> `Concat []
            | _ ->
                let gs, os', body =
                  match xs with
                    | `For (_, gs, os', body) -> gs, os', body
                    | `Concat (_::_)
                    | `Singleton _
                    | `Table _ ->
                        (* I think we can omit the `Table case as it
                           can never occur *)
                        (* eta-expand *)
                        eta_expand_list xs
                    | _ -> assert false in
                let xs = `For (None, gs, os', body) in
                  begin
                    match f with
                      | `Closure (([x], os), closure_env) ->
                          let os =
                            let env = env ++ closure_env in
                              let o = computation (bind env (x, tail_of_t xs)) os in
                                match o with
                                  | `Record fields ->
                                      List.rev (StringMap.fold (fun _ o os -> o::os) fields [])
                                  | _ -> assert false
                          in
                            `For (None, gs, os @ os', body)
                      | _ -> assert false
                  end
        end
    | `Primitive "not", [v] ->
      reduce_not (v)
    | `Primitive "&&", [v; w] ->
      reduce_and (v, w)
    | `Primitive "||", [v; w] ->
      reduce_or (v, w)
    | `Primitive "==", [v; w] ->
      reduce_eq (v, w)
    | `Primitive f, args ->
        `Apply (f, args)
    | `If (c, t, e), args ->
        reduce_if_condition (c, apply env (t, args), apply env (e, args))
    | `Apply (f, args), args' ->
        `Apply (f, args @ args')
    | _ -> eval_error "Application of non-function"
  and computation env (binders, tailcomp) : t =
    match binders with
      | [] -> tail_computation env tailcomp
      | b::bs ->
          begin
            match b with
              | `Let (xb, (_, tc)) ->
                  let x = Var.var_of_binder xb in
                    computation (bind env (x, tail_computation env tc)) (bs, tailcomp)
              | `Fun ((f, _) as fb, (_, args, body), _, (`Client | `Native)) ->
                  eval_error "Client function"
              | `Fun ((f, _) as fb, (_, args, body), z, _) ->
                (* This should never happen now that we have closure conversion*)
                failwith ("Function definition in query: " ^ string_of_int f)
                (* let args = *)
                (*   match z with *)
                (*   | None -> args *)
                (*   | Some z -> z :: args *)
                (* in *)
                (* computation *)
                (*   (bind env (f, `Closure ((List.map fst args, body), env))) *)
                (*   (bs, tailcomp) *)
              | `Rec defs ->
                  eval_error "Recursive function"
              | `Alien _ -> (* just skip it *)
                  computation env (bs, tailcomp)
              | `Module _ -> failwith "Not implemented modules yet"
          end
  and tail_computation env : Ir.tail_computation -> t = function
    | `Return v -> value env v
    | `Apply (f, args) ->
        apply env (value env f, List.map (value env) args)
    | `Special (`Query (None, e, _)) -> computation env e
    | `Special _s ->
      (* FIXME:

         There's no particular reason why we can't allow
         table declarations in query blocks.

         Same goes for database declarations. (However, we do still
         have the problem that we currently have no way of enforcing
         that only one database be used inside a query block - see
         SML#.)  *)
      failwith "special not allowed in query block"
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
          | `If (c, t, e) ->
            `If
              (c,
               reduce_case (t, cases, default),
               reduce_case (e, cases, default))
          |  _ -> assert false
      in
        reduce_case (value env v, cases, default)
    | `If (c, t, e) ->
      let c = value env c in
      let t = computation env t in
      let e = computation env e in
        reduce_if_condition (c, t, e)
  and reduce_concat vs =
    let vs =
      concat_map
        (function
          | `Concat vs -> vs
          | v -> [v])
        vs
    in
      match vs with
        | [v] -> v
        | vs -> `Concat vs
  and reduce_for_source : t * (t -> t) -> t =
    fun (source, body) ->
      let rs = fun source -> reduce_for_source (source, body) in
        match source with
          | `Singleton v -> body v
          | `Concat vs ->
            reduce_concat (List.map rs vs)
          | `If (c, t, `Concat []) ->
            reduce_for_source
              (t, fun v -> reduce_where_then (c, body v))
          | `For (_, gs, os, v) ->
            (* NOTE:

               We are relying on peculiarities of the way we manage
               the environment in order to avoid having to
               augment it with the generator bindings here.

               In particular, we rely on the fact that if a variable
               is not found on a lookup then we return the eta
               expansion of that variable rather than complaining that
               it isn't bound in the environment.

            *)
            reduce_for_body (gs, os, rs v)
          | `Table table ->
            let field_types = table_field_types table in
            (* we need to generate a fresh variable in order to
               correctly handle self joins *)
            let x = Var.fresh_raw_var () in
              (* Debug.print ("fresh variable: " ^ string_of_int x); *)
              reduce_for_body ([(x, source)], [], body (`Var (x, field_types)))
          | v -> eval_error "Bad source in for comprehension: %s" (string_of_t v)
  and reduce_for_body (gs, os, body) =
    match body with
      | `For (_, gs', os', body') -> `For (None, gs @ gs', os @ os', body')
      | _                         -> `For (None, gs, os, body)
  and reduce_if_condition (c, t, e) =
    match c with
      | `Constant (`Bool true) -> t
      | `Constant (`Bool false) -> e
      | `If (c', t', e') ->
        reduce_if_body
          (reduce_or (reduce_and (c', t'),
                      reduce_and (reduce_not c', t')),
           t,
           e)
      | _ ->
        if is_list t then
          if e = nil then
            reduce_where_then (c, t)
          else
            reduce_concat [reduce_where_then (c, t);
                           reduce_where_then (reduce_not c, e)]
        else
          reduce_if_body (c, t, e)
  and reduce_where_then (c, t) =
    match t with
      (* optimisation *)
      | `Constant (`Bool true) -> t
      | `Constant (`Bool false) -> `Concat []

      | `Concat vs ->
        reduce_concat (List.map (fun v -> reduce_where_then (c, v)) vs)
      | `For (_, gs, os, body) ->
        `For (None, gs, os, reduce_where_then (c, body))
      | `If (c', t', `Concat []) ->
        reduce_where_then (reduce_and (c, c'), t')
      | _ ->
        `If (c, t, `Concat [])
  and reduce_if_body (c, t, e) =
    match t with
      | `Record then_fields ->
        begin match e with
          | `Record else_fields ->
            assert (StringMap.equal (fun _ _ -> true) then_fields else_fields);
            `Record
              (StringMap.fold
                 (fun name t fields ->
                   let e = StringMap.find name else_fields in
                     StringMap.add name (reduce_if_body (c, t, e)) fields)
                 then_fields
                 StringMap.empty)
          (* NOTE: this relies on any record variables having
             been eta-expanded by this point *)
          | _ -> eval_error "Mismatched fields"
        end
      | _ ->
        begin
          match t, e with
            | `Constant (`Bool true), _ ->
              reduce_or (c, e)
            | _, `Constant (`Bool false) ->
              reduce_and (c, t)
            | _ ->
              `If (c, t, e)
        end
  (* simple optimisations *)
  and reduce_and (a, b) =
    match a, b with
      | `Constant (`Bool true), x
      | x, `Constant (`Bool true)
      | (`Constant (`Bool false) as x), _
      | _, (`Constant (`Bool false) as x) -> x
      | _ -> `Apply ("&&", [a; b])
  and reduce_or (a, b) =
    match a, b with
      | (`Constant (`Bool true) as x), _
      | _, (`Constant (`Bool true) as x)
      | `Constant (`Bool false), x
      | x, `Constant (`Bool false) -> x
      | _ -> `Apply ("||", [a; b])
  and reduce_not a =
    match a with
      | `Constant (`Bool false) -> `Constant (`Bool true)
      | `Constant (`Bool true)  -> `Constant (`Bool false)
      | _                       -> `Apply ("not", [a])
  and reduce_eq (a, b) =
    let bool x = `Constant (`Bool x) in
    let eq_constant =
      function
        | (`Bool a  , `Bool b)   -> bool (a = b)
        | (`Int a   , `Int b)    -> bool (a = b)
        | (`Float a , `Float b)  -> bool (a = b)
        | (`Char a  , `Char b)   -> bool (a = b)
        | (`String a, `String b) -> bool (a = b)
        | (a, b)                 -> `Apply ("==", [`Constant a; `Constant b])
    in
      match a, b with
        | (`Constant a, `Constant b) -> eq_constant (a, b)
        | (`Variant (s1, a), `Variant (s2, b)) ->
          if s1 <> s2 then
            `Constant (`Bool false)
          else
            reduce_eq (a, b)
        | (`Record lfields, `Record rfields) ->
          List.fold_right2
            (fun (s1, v1) (s2, v2) e ->
              reduce_and (reduce_eq (v1, v2), e))
            (StringMap.to_alist lfields)
            (StringMap.to_alist rfields)
            (`Constant (`Bool true))
        | (a, b) -> `Apply ("==", [a; b])

  let eval env e =
(*    Debug.print ("e: "^Ir.Show_computation.show e); *)
    computation (env_of_value_env env) e
end

(* Hoist concatenation to the top-level and lower conditionals to the
   tails of comprehensions, yielding a collection of canonical
   comprehensions.
   
   This process doesn't necessarily respect list ordering. The order
   module generalises it in order to support various degrees of
   list-ordering.

   The intention is that Split.query == Order.unordered_query.
*)
module Split =
struct
  type gen = Var.var * t

  let rec query : gen list -> t list -> t -> t -> t list =
    fun gs os cond ->
      function
        | `Singleton r ->
          [`For (None, gs, os, Eval.reduce_where_then (cond, `Singleton (inner r)))]
        | `Concat vs ->
          concat_map (query gs os cond) vs
        | `If (cond', v, `Concat []) ->
          query gs os (Eval.reduce_and (cond, cond')) v
        | `For (_, gs', os', body) ->
          query (gs @ gs') (os @ os') cond body
        | _ -> assert false

  and inner =
    function
      | `If (c, t, e) ->
        `If (inner c, inner t, inner e)
      | `Record fields -> `Record (StringMap.map inner fields)
      | `Project (e, l) -> `Project (inner e, l)
      | `Apply (f, es) -> `Apply (f, List.map inner es)
      | `Primitive p -> `Primitive p
      | `Var (x, t) -> `Var (x, t)
      | `Constant c -> `Constant c
      | e when is_list e ->
        `Concat (query [] [] (`Constant (`Bool true)) e)
      | _ -> assert false

  let query : t -> t list =
    query [] [] (`Constant (`Bool true))
end


module LetInsertion = 
struct
  type let_clause = Var.var * t * Var.var * t
      deriving (Show)
  type query = let_clause list
      deriving (Show)

  type cond = t option
  type gen = Var.var * t

  let where c e =
    match c with
      | None -> e
      | Some c ->        
        `If (c, e, `Concat [])

  let index = `Primitive "index"

  let position_of x =
    let rec position_of x i =
      function
        | [] -> None
        | (y::ys) ->
          if y = x then Some i
          else
            position_of x (i+1) ys
    in
      position_of x 1

  let rec init =
    function
      | [x]   -> []
      | x::y::xs -> x::(init (y::xs))

  let rec last =
    function
      | [x]      -> x
      | x::y::xs -> last (y::xs)

  let rec gens : t -> (gen list) list =
    function
      | `Singleton _           -> []
      | `If (_, t, `Concat []) -> gens t
      | `For (_, gs, _, e)     -> gs :: gens e
      | _                      -> assert false

  let rec orders : t -> (t list) list =
    function
      | `Singleton _           -> []
      | `If (_, t, `Concat []) -> orders t
      | `For (_, _, os, e)     -> os :: orders e
      | _                      -> assert false

  let rec conds : t -> cond list =
    function
      | `Singleton _                           -> []
      | `For (_, _, _, `If (c, t, `Concat [])) -> Some c :: conds t
      | `For (_, _, _, e)                      -> None :: conds e
      | _                                      -> assert false

  let rec body : t -> t =
    function
      | `Singleton e           -> e
      | `If (_, t, `Concat []) -> body t
      | `For (_, _, _, e)      -> body e
      | _                      -> assert false
  

  let fields_of_list : string list -> StringSet.t =
    List.fold_left
      (fun fields l ->
        StringSet.add l fields)
      StringSet.empty    

  (* dynamic index type *)
  let index_type = Types.int_type

  let rec lins_inner (z, z_fields) ys : t -> t =
    function
      | `Project (`Var (x, fields), l) ->
        begin
          match position_of x ys with
            | None -> `Project (`Var (x, fields), l)
            | Some i ->
              (* z.1.i.l *)
              `Project
                (`Project
                    (`Project (`Var (z, z_fields), "1"), string_of_int i), l)
        end
      | `Apply (f, es) ->
        `Apply (f, List.map (lins_inner (z, z_fields) ys) es)
      | `Record fields ->
        `Record (StringMap.map (lins_inner (z, z_fields) ys) fields)
      | `Primitive "out" ->
        (* z.2 *)
        `Project (`Var (z, z_fields), "2")
      | `Primitive "in"  -> `Primitive "index"
      | `Constant c      -> `Constant c
      | _                -> assert false

  let lins c : let_clause =
    let gs_out = List.concat (init (gens c)) in

    let ys = List.map fst gs_out in

    let x_out =
      List.fold_right
        (fun x y ->
          match x, y with
            | None,   _       -> y
            | _   ,   None    -> x
            | Some c, Some c' -> Some (Eval.reduce_and (c, c')))
        (init (conds c))
        None in

    let r_out = 
      tuple (List.map
               (fun (x, source) ->
                 match source with
                   | `Table t ->
                     Eval.eta_expand_var (x, table_field_types t)
                   | _ -> assert false)
               gs_out) in
   let r_out_type =
    Types.make_tuple_type
      (List.map
         (fun (x, source) ->
           match source with
             | `Table (_, _, row) ->
               `Record row
             | _ -> assert false)
         gs_out) in

    let gs_in = last (gens c) in
    let x_in = last (conds c) in

    let os = List.concat (orders c) in
    let q = Var.fresh_raw_var () in
    let z = Var.fresh_raw_var () in
    let z_fields =
      record_field_types
        (Types.make_tuple_type
           [r_out_type; index_type])
    in
      (q, `For (None, gs_out, [], where x_out (`Singleton (pair r_out index))),
       z, `For (None, gs_in, os,
                where
                  (opt_map (lins_inner (z, z_fields) ys) x_in)
                  (`Singleton (lins_inner (z, z_fields) ys (body c)))))
   
  let lins_query : t -> query =
    function
      | `Concat cs -> List.map lins cs
      | _          -> assert false

end


module FlattenRecords =
struct
  type let_clause = LetInsertion.let_clause
  type query = LetInsertion.query

  let rec flatten_inner : t -> t =
    function
      | `Constant c    -> `Constant c
      | `Primitive p   -> `Primitive p
      | `Apply (f, es) -> `Apply (f, List.map flatten_inner es) 
      | `If (c, t, e)  ->
        `If (flatten_inner c, flatten_inner t, flatten_inner e)
      | `Project (`Var x, l) -> `Project (`Var x, l)
      | `Project (`Project (`Project (`Var z, "1"), i), l) ->
        (* HACK: this keeps z annotated with its original unflattened type *)
        `Project (`Var z, "1"^"_"^i^"_"^l)
      | `Record fields ->
        (* concatenate labels of nested records *)
        `Record
          (StringMap.fold
             (fun name body fields ->
               match flatten_inner body with
                 | `Record inner_fields ->
                   StringMap.fold
                     (fun name' body fields ->
                       StringMap.add (name ^ "_" ^ name') body fields)
                     inner_fields
                     fields
                 | body ->
                   StringMap.add name body fields)
             fields
             StringMap.empty)
      | e ->
        Debug.print ("Can't apply flatten_inner to: " ^ Show_t.show e);
        assert false

  let rec flatten_comprehension : t -> t =
    function
      | `For (tag, gs, os, body) ->
        `For (tag, gs, os, flatten_comprehension body)
      | `If (c, e, `Concat []) ->
        `If (flatten_inner c, flatten_comprehension e, `Concat [])
      | `Singleton e ->
        let e' =
          (* lift base expressions to records *)
          match flatten_inner e with
            | `Record fields -> `Record fields
            | p -> `Record (StringMap.add "_" p StringMap.empty)
        in
          `Singleton e'
      | _ -> assert false

  let flatten_let_clause : LetInsertion.let_clause -> let_clause =
    function
      | (q, outer, z, inner) ->
        (q, flatten_comprehension outer, z, flatten_comprehension inner)
      | _ -> assert false
     
  let flatten_query : LetInsertion.query -> query =
    fun q ->
(*      Debug.print ("Unflattened query: " ^ Show.show LetInsertion.show_query q); *)
      let q' = List.map flatten_let_clause q in
(*        Debug.print ("flattened query: " ^ Show.show LetInsertion.show_query q');*)
        q'

  let rec flatten_type : Shred.shredded_type -> Shred.flat_type =
    function
      | `Primitive p -> `Primitive p
      | `Record fields ->
        `Record
          (StringMap.fold
             (fun name t fields ->
               match flatten_type t with
                 | `Record inner_fields ->
                   StringMap.fold
                     (fun name' t fields ->
                       StringMap.add (name ^ "_" ^ name') t fields)
                     inner_fields
                     fields
                 | `Primitive p ->
                   StringMap.add name p fields)
             fields
             StringMap.empty)
      | _ -> assert false

  let flatten_query_type : Shred.shredded_type -> Shred.flat_type = flatten_type

  type nested_value = [`Val of Value.t | `Record of nested_value StringMap.t]

  let rec add_nested_field fields v =
    function
      | [s] -> StringMap.add s (`Val v) fields
      | s::s'::ss ->
        let fields' =
          if StringMap.mem s fields then
            let w = StringMap.find s fields in
              match w with
                | `Record fields' -> fields'
                | _ -> assert false
          else
            StringMap.empty in
        let fields' = add_nested_field fields' v (s'::ss) in
          StringMap.add s (`Record fields') fields   

  let rec unflatten_record : nested_value StringMap.t -> (string * Value.t) list -> Value.t =
    let rec listify_records : nested_value -> Value.t =
      function
        | `Record fields -> `Record (StringMap.to_alist (StringMap.map listify_records fields))
        | `Val v -> v
    in
      fun fields' ->
        function
          | [] -> listify_records (`Record fields')
          | [("_", v)] -> v            
          | (name, v) :: fields ->
            let ss = split_string name '_' in
              unflatten_record (add_nested_field fields' v ss) fields

  let rec unflatten_value : Value.t -> Value.t =
    function
      | `Record fields ->
        unflatten_record StringMap.empty fields
      | `List vs ->
        `List (List.map unflatten_value vs)
      | v -> v
    
end

module Sql =
struct
  type query =
    [ `UnionAll of query list * int
    | `Select of (base * string) list * (string * Var.var) list * base * base list ]
  and base =
    [ `Case of (base * base * base)
    | `Constant of Constant.constant
    | `Project of Var.var * string
    | `Apply of string * base list
    | `Empty of query
    | `Length of query ]
      deriving (Show)

  (* Table variables that are actually used are always bound in a for
     comprehension. In this case the IR variable from the for
     comprehension is used to generate the table variable.

     e.g. if the IR variable is 1485 then the table variable is t1485
  *)
  let fresh_table_var : unit -> Var.var = Var.fresh_raw_var
  let string_of_table_var var = "t" ^ string_of_int var

  (* Because of limitations of SQL we sometimes need to generate dummy
     table variables. These have the prefix "dummy" and have their own
     name source. *)
  let dummy_counter = ref 0
  let reset_dummy_counter () = dummy_counter := 0
  let fresh_dummy_var () =
    incr dummy_counter;
    "dummy" ^ string_of_int (!dummy_counter)

  let string_of_label label =
    if Str.string_match (Str.regexp "[0-9]+") label 0 then
      "\"" ^ label ^ "\""     (* The SQL-standard way to quote an identifier;
                                 works in MySQL and PostgreSQL *)
    else
      label

  module Arithmetic :
  sig
    val is : string -> bool
    val gen : (string * string * string) -> string
  end =
  struct
    let builtin_ops =
      StringMap.from_alist
        [ "+",   Some "+"  ;
          "+.",  Some "+"  ;
          "-",   Some "-"  ;
          "-.",  Some "-"  ;
          "*",   Some "*"  ;
          "*.",  Some "*"  ;
          "/",   None      ;
          "^",   None      ;
          "^.",  None      ;
          "/.",  Some "/"  ;
          "mod", Some "%"  ;
	  (* FIXME: The SQL99 || operator is supported in PostgreSQL and
	     SQLite but not in MySQL, where it denotes the logical or
	     operator *)
	  "^^",  Some "||" ]

    let is x = StringMap.mem x builtin_ops
    let sql_name op = val_of (StringMap.find op builtin_ops)
    let gen (l, op, r) =
      match op with
        | "/" -> "floor("^l^"/"^r^")"
        | "^" -> "floor(pow("^l^","^r^"))"
        | "^." -> "pow("^l^","^r^")"
        | _ -> "("^l^sql_name op^r^")"
  end

  module SqlFuns :
  sig
    val is : string -> bool
    val name : string -> string
  end =
  struct
    let funs =
      StringMap.from_alist
        [ "toUpper",  "upper";
          "toLower",  "lower";
          "ord",      "ord";
          "chr",      "char";
          "random",   "rand" ]

    let is f = StringMap.mem f funs
    let name f = StringMap.find f funs
  end

  let order_by_clause n =
    if n == 0 then
      ""
    else
      let rec order i n =
        if i > n then
          []
        else
          ("order_" ^ string_of_int i) :: order (i+1) n
      in
        " order by " ^ String.concat "," (order 1 n)

  (* For `Empty and `Length we don't care about the actual data
     returned. This allows these operators to take lists that have any
     element type at all. *)

  let rec string_of_query db ignore_fields q =
    let sq = string_of_query db ignore_fields in
    let sb = string_of_base db false in
    let string_of_fields fields =
      if ignore_fields then
        "0 as dummy" (* SQL doesn't support empty records! *)
      else
        match fields with
          | [] -> "0 as dummy" (* SQL doesn't support empty records! *)
          | fields ->
            mapstrcat ","
              (fun (b, l) ->
                "(" ^ sb b ^ ") as "^ db#quote_field l) (* string_of_label l) *)
              fields
    in
      match q with
        | `UnionAll ([], _) -> assert false
        | `UnionAll ([q], n) -> sq q ^ order_by_clause n
        | `UnionAll (qs, n) ->
          mapstrcat " union all " (fun q -> "(" ^ sq q ^ ")") qs ^ order_by_clause n
        | `Select (fields, [], `Constant (`Bool true), _os) ->
            let fields = string_of_fields fields in
              "select " ^ fields
        | `Select (fields, [], condition, _os) ->
            let fields = string_of_fields fields in
              "select * from (select " ^ fields ^ ") as " ^ fresh_dummy_var () ^ " where " ^ sb condition
        | `Select (fields, tables, condition, os) ->
            let tables = mapstrcat "," (fun (t, x) -> t ^ " as " ^ (string_of_table_var x)) tables in
            let fields = string_of_fields fields in
            let orderby =
              match os with
                | [] -> ""
                | _ -> " order by " ^ mapstrcat "," sb os in
            let where =
              match condition with
                | `Constant (`Bool true) -> ""
                | _ ->  " where " ^ sb condition
            in
              "select " ^ fields ^ " from " ^ tables ^ where ^ orderby
  and string_of_base db one_table b =
    let sb = string_of_base db one_table in
      match b with
        | `Case (c, t, e) ->
            "case when " ^ sb c ^ " then " ^sb t ^ " else "^ sb e ^ " end"
        | `Constant c -> Constant.string_of_constant c
        | `Project (var, label) ->
            if one_table then
              db#quote_field label
            else
              string_of_table_var var ^ "." ^ (db#quote_field label)
        | `Apply (op, [l; r]) when Arithmetic.is op
            -> Arithmetic.gen (sb l, op, sb r)
        | `Apply (("intToString" | "stringToInt" | "intToFloat" | "floatToString"
                  | "stringToFloat"), [v]) -> sb v
        | `Apply ("floatToInt", [v]) -> "floor("^sb v^")"

        (* optimisation *)
        | `Apply ("not", [`Empty q]) -> "exists (" ^ string_of_query db true q ^ ")"

        | `Apply ("not", [v]) -> "not (" ^ sb v ^ ")"
        | `Apply (("negate" | "negatef"), [v]) -> "-(" ^ sb v ^ ")"
        | `Apply ("&&", [v; w]) -> "(" ^ sb v ^ ")" ^ " and " ^ "(" ^ sb w ^ ")"
        | `Apply ("||", [v; w]) -> "(" ^ sb v ^ ")" ^ " or " ^ "(" ^ sb w ^ ")"
        | `Apply ("==", [v; w]) -> "(" ^ sb v ^ ")" ^ " = " ^ "(" ^ sb w ^ ")"
        | `Apply ("<>", [v; w]) -> "(" ^ sb v ^ ")" ^ " <> " ^ "(" ^ sb w ^ ")"
        | `Apply ("<", [v; w]) -> "(" ^ sb v ^ ")" ^ " < " ^ "(" ^ sb w ^ ")"
        | `Apply (">", [v; w]) -> "(" ^ sb v ^ ")" ^ " > " ^ "(" ^ sb w ^ ")"
        | `Apply ("<=", [v; w]) -> "(" ^ sb v ^ ")" ^ " <= " ^ "(" ^ sb w ^ ")"
        | `Apply (">=", [v; w]) -> "(" ^ sb v ^ ")" ^ " >= " ^ "(" ^ sb w ^ ")"
        | `Apply ("RLIKE", [v; w]) -> "(" ^ sb v ^ ")" ^ " RLIKE " ^ "(" ^ sb w ^ ")"
        | `Apply ("LIKE", [v; w]) -> "(" ^ sb v ^ ")" ^ " LIKE " ^ "(" ^ sb w ^ ")"
        | `Apply (f, args) when SqlFuns.is f -> SqlFuns.name f ^ "(" ^ String.concat "," (List.map sb args) ^ ")"
        | `Apply (f, args) -> f ^ "(" ^ String.concat "," (List.map sb args) ^ ")"
        | `Empty q -> "not exists (" ^ string_of_query db true q ^ ")"
        | `Length q -> "select count(*) from (" ^ string_of_query db true q ^ ") as " ^ fresh_dummy_var ()

  let string_of_query db range q =
    let range =
      match range with
        | None -> ""
        | Some (limit, offset) -> " limit " ^ string_of_int limit^" offset "^ string_of_int offset
    in
      string_of_query db false q ^ range

  let prepare_clauses : t -> t list =
    function
      | `Concat vs -> vs
      | v -> [v]

  let rec clause : Value.database -> t -> query = fun db v ->
   (* Debug.print ("clause: "^string_of_t v); *)
    match v with
      | `Concat _ -> assert false
      | `For (_, [], _, body) ->
          clause db body
      | `For (_, (x, `Table (_db, table, _row))::gs, os, body) ->
          let body = clause db (`For (None, gs, [], body)) in
          let os = List.map (base db) os in
            begin
              match body with
                | `Select (fields, tables, condition, []) ->
                    `Select (fields, (table, x)::tables, condition, os)
                | _ -> assert false
            end
      | `If (c, body, `Concat []) ->
        (* Turn conditionals into where clauses. We might want to do
           this earlier on.  *)
        let c = base db c in
        let body = clause db body in
          begin
            match body with
              | `Select (fields, tables, c', os) ->
                let c =
                  match c, c' with
                    (* optimisations *)
                    | `Constant (`Bool true), c
                    | c, `Constant (`Bool true) -> c
                    | `Constant (`Bool false), _
                    | _, `Constant (`Bool false) -> `Constant (`Bool false)
                    (* default case *)
                    | c, c' -> `Apply ("&&", [c; c'])
                in
                  `Select (fields, tables, c, os)
              | _ -> assert false
          end
      | `Table (_db, table, (fields, _, _)) ->
        (* eta expand tables. We might want to do this earlier on.  *)
        (* In fact this should never be necessary as it is impossible
           to produce non-eta expanded tables. *)
        let var = fresh_table_var () in
        let fields =
          List.rev
            (StringMap.fold
               (fun name _ fields ->
                 (`Project (var, name), name)::fields)
               fields
               [])
        in
          `Select (fields, [(table, var)], `Constant (`Bool true), [])
      | `Singleton (`Record fields) ->
        let fields =
          List.rev
            (StringMap.fold
               (fun name v fields ->
                 (base db v, name)::fields)
               fields
               [])
        in
          `Select (fields, [], `Constant (`Bool true), [])

      | `Singleton _ ->
        (* If we're inside an `Empty or a `Length it's safe to
           ignore any fields here. Otherwise this line should be
           unreachable. *)
        `Select ([], [], `Constant (`Bool true), [])
      | _ -> assert false
  and base : Value.database -> t -> base = fun db ->
    function
      | `If (c, t, e) ->
        `Case (base db c, base db t, base db e)
      | `Apply ("tilde", [s; r]) ->
        begin
          match likeify r with
            | Some r ->
              `Apply ("LIKE", [base db s; `Constant (`String r)])
            | None ->
              let r =
                    (* HACK:

                       this only works if the regexp doesn't include any variables bound by the query
                    *)
                    `Constant (`String (Regex.string_of_regex (Linksregex.Regex.ofLinks (value_of_expression r))))
                  in
                    `Apply ("RLIKE", [base db s; r])
          end
      | `Apply ("Empty", [v]) ->
          `Empty (outer_query db v)
      | `Apply ("length", [v]) ->
          `Length (outer_query db v)
      | `Apply (f, vs) ->
          `Apply (f, List.map (base db) vs)
      | `Project (`Var (x, _field_types), name) ->
          `Project (x, name)
      | `Constant c -> `Constant c
      | _ -> assert false

  (* convert a regexp to a like if possible *)
  and likeify v =
    let quote = Str.global_replace (Str.regexp_string "%") "\\%" in
      match v with
        | `Variant ("Repeat", pair) ->
            begin
              match unbox_pair pair with
                | `Variant ("Star", _), `Variant ("Any", _) -> Some ("%")
                | _ -> None
            end
        | `Variant ("Simply", `Constant (`String s)) -> Some (quote s)
        | `Variant ("Quote", `Variant ("Simply", v)) ->
            (* TODO:

               detect variables and convert to a concatenation operation
               (this needs to happen in RLIKE compilation as well)
            *)
           let rec string =
              function
                | `Constant (`String s) -> Some s
                | `Singleton (`Constant (`Char c)) -> Some (string_of_char c)
                | `Concat vs ->
                    let rec concat =
                      function
                        | [] -> Some ""
                        | v::vs ->
                            begin
                              match string v with
                                | None -> None
                                | Some s ->
                                    begin
                                      match concat vs with
                                        | None -> None
                                        | Some s' -> Some (s ^ s')
                                    end
                            end
                    in
                      concat vs
                | _ -> None
            in
              opt_map quote (string v)
        | `Variant ("Seq", rs) ->
            let rec seq =
              function
                | [] -> Some ""
                | r::rs ->
                    begin
                      match likeify r with
                        | None -> None
                        | Some s ->
                            begin
                              match seq rs with
                                | None -> None
                                | Some s' -> Some (s^s')
                            end
                    end
            in
              seq (unbox_list rs)
        | `Variant ("StartAnchor", _) -> Some ""
        | `Variant ("EndAnchor", _) -> Some ""
        | _ -> assert false
  and outer_query db v =
    `UnionAll (List.map (clause db) (prepare_clauses v), 0)

  let unordered_query db range v =
    (* Debug.print ("v: "^string_of_t v); *)
    reset_dummy_counter ();
    let vs = Split.query v in
    (* Debug.print ("concat vs: "^string_of_t (`Concat vs)); *)
    let q = `UnionAll (List.map (clause db) vs, 0) in
      string_of_query db range q

  let update db ((x, table), where, body) =
    reset_dummy_counter ();
    let base = (base db) ->- (string_of_base db true) in
    let where =
      match where with
        | None -> ""
        | Some where ->
            " where (" ^ base where ^ ")" in
    let fields =
      match body with
        | `Record fields ->
            String.concat ","
              (List.map
                 (fun (label, v) -> db#quote_field label ^ " = " ^ base v)
                 (StringMap.to_alist fields))
        | _ -> assert false
    in
      "update "^table^" set "^fields^where

  let delete db ((x, table), where) =
    reset_dummy_counter ();
    let base = base db ->- (string_of_base db true) in
    let where =
      match where with
        | None -> ""
        | Some where ->
            " where (" ^ base where ^ ")"
    in
      "delete from "^table^where
end

module ShreddedSql =
struct
  type query =
    [ `UnionAll of query list * int
    | `Select of (base * string) list * (string * Var.var) list * base * base list
    | `With of Var.var * query * Var.var * query ]
  and base =
    [ `Case of (base * base * base)
    | `Constant of Constant.constant
    | `Project of Var.var * string
    | `Apply of string * base list
    | `Empty of query
    | `Length of query
    | `RowNumber of (Var.var * string) list]
      deriving (Show)

  (* Table variables that are actually used are always bound in a for
     comprehension. In this case the IR variable from the for
     comprehension is used to generate the table variable.
     
     e.g. if the IR variable is 1485 then the table variable is t1485
  *)
  let fresh_table_var : unit -> Var.var = Var.fresh_raw_var
  let string_of_table_var var = "t" ^ string_of_int var
  let string_of_subquery_var var = "q" ^ string_of_int var

  (* Because of limitations of SQL we sometimes need to generate dummy
     table variables. These have the prefix "dummy" and have their own
     name source. *)
  let dummy_counter = ref 0
  let reset_dummy_counter () = dummy_counter := 0
  let fresh_dummy_var () =
    incr dummy_counter;     
    "dummy" ^ string_of_int (!dummy_counter)

  let string_of_label label =
    if Str.string_match (Str.regexp "[0-9]+") label 0 then
      "\"" ^ label ^ "\""     (* The SQL-standard way to quote an identifier; 
                                 works in MySQL and PostgreSQL *)
    else
      label

  module Arithmetic :
  sig
    val is : string -> bool
    val gen : (string * string * string) -> string
  end =
  struct
    let builtin_ops =
      StringMap.from_alist
        [ "+",   Some "+"  ;
          "+.",  Some "+"  ;
          "-",   Some "-"  ;
          "-.",  Some "-"  ;
          "*",   Some "*"  ;
          "*.",  Some "*"  ;
          "/",   None      ;
          "^",   None      ;
          "^.",  None      ;
          "/.",  Some "/"  ;
          "mod", Some "%"  ;
	  (* FIXME: The SQL99 || operator is supported in PostgreSQL and
	     SQLite but not in MySQL, where it denotes the logical or
	     operator *)
	  "^^",  Some "||" ]

    let is x = StringMap.mem x builtin_ops
    let sql_name op = val_of (StringMap.find op builtin_ops)
    let gen (l, op, r) =
      match op with
        | "/" -> "floor("^l^"/"^r^")"
        | "^" -> "floor(pow("^l^","^r^"))"
        | "^." -> "pow("^l^","^r^")"
        | _ -> "("^l^sql_name op^r^")"
  end

  module SqlFuns :
  sig
    val is : string -> bool
    val name : string -> string
  end =
  struct
    let funs =
      StringMap.from_alist
        [ "toUpper",  "upper";
          "toLower",  "lower";
          "ord",      "ord";
          "chr",      "char";
          "random",   "rand" ]

    let is f = StringMap.mem f funs
    let name f = StringMap.find f funs
  end

  let order_by_clause n =
    if n == 0 then
      ""
    else
      let rec order i n =
        if i > n then
          []
        else
          ("order_" ^ string_of_int i) :: order (i+1) n
      in
        " order by " ^ String.concat "," (order 1 n)

  (* For `Empty and `Length we don't care about the actual data
     returned. This allows these operators to take lists that have any
     element type at all. *)

  let rec string_of_query db ignore_fields q =
    let sq = string_of_query db ignore_fields in
    let sb = string_of_base db false in
    let string_of_fields fields =
      if ignore_fields then
        "0 as dummy" (* SQL doesn't support empty records! *)
      else
        match fields with
          | [] -> "0 as dummy" (* SQL doesn't support empty records! *)
          | fields ->
            mapstrcat ","
              (fun (b, l) ->
                "(" ^ sb b ^ ") as "^ db#quote_field l) (* string_of_label l) *)
              fields
    in
      match q with
        | `UnionAll ([], _) -> assert false
        | `UnionAll ([q], n) -> sq q ^ order_by_clause n
        | `UnionAll (qs, n) ->
          mapstrcat " union all " (fun q -> "(" ^ sq q ^ ")") qs ^ order_by_clause n
        | `Select (fields, [], `Constant (`Bool true), _os) ->
            let fields = string_of_fields fields in
              "select " ^ fields
        | `Select (fields, [], condition, _os) ->
            let fields = string_of_fields fields in
              "select * from (select " ^ fields ^ ") as " ^ fresh_dummy_var () ^ " where " ^ sb condition
        | `Select (fields, tables, condition, os) ->
            let tables = mapstrcat "," (fun (t, x) -> t ^ " as " ^ (string_of_table_var x)) tables in
            let fields = string_of_fields fields in
            let orderby =
              match os with
                | [] -> ""
                | _ -> " order by " ^ mapstrcat "," sb os in
            let where =
              match condition with
                | `Constant (`Bool true) -> ""
                | _ ->  " where " ^ sb condition
            in
              "select " ^ fields ^ " from " ^ tables ^ where ^ orderby
        | `With (x, q, z, q') ->
          let q' =
            (* HACK: pretend that the subquery name x is a table name *)
            match q' with
              | `Select (fields, tables, condition, os) ->
                `Select (fields, (string_of_subquery_var x, z) :: tables, condition, os)
              | _ -> assert false
          in
            "with " ^ string_of_subquery_var x ^ " as (" ^ sq q ^ ") " ^ sq q'
          
  and string_of_base db one_table b =
    let sb = string_of_base db one_table in
      match b with
        | `Case (c, t, e) ->
            "case when " ^ sb c ^ " then " ^sb t ^ " else "^ sb e ^ " end"
        | `Constant c -> Constant.string_of_constant c
        | `Project (var, label) -> string_of_projection db one_table (var, label)
        | `Apply (op, [l; r]) when Arithmetic.is op
            -> Arithmetic.gen (sb l, op, sb r)
        | `Apply (("intToString" | "stringToInt" | "intToFloat" | "floatToString"
                  | "stringToFloat"), [v]) -> sb v
        | `Apply ("floatToInt", [v]) -> "floor("^sb v^")"

        (* optimisation *)
        | `Apply ("not", [`Empty q]) -> "exists (" ^ string_of_query db true q ^ ")"

        | `Apply ("not", [v]) -> "not (" ^ sb v ^ ")"
        | `Apply (("negate" | "negatef"), [v]) -> "-(" ^ sb v ^ ")"
        | `Apply ("&&", [v; w]) -> "(" ^ sb v ^ ")" ^ " and " ^ "(" ^ sb w ^ ")"
        | `Apply ("||", [v; w]) -> "(" ^ sb v ^ ")" ^ " or " ^ "(" ^ sb w ^ ")"
        | `Apply ("==", [v; w]) -> "(" ^ sb v ^ ")" ^ " = " ^ "(" ^ sb w ^ ")"
        | `Apply ("<>", [v; w]) -> "(" ^ sb v ^ ")" ^ " <> " ^ "(" ^ sb w ^ ")"
        | `Apply ("<", [v; w]) -> "(" ^ sb v ^ ")" ^ " < " ^ "(" ^ sb w ^ ")"
        | `Apply (">", [v; w]) -> "(" ^ sb v ^ ")" ^ " > " ^ "(" ^ sb w ^ ")"
        | `Apply ("<=", [v; w]) -> "(" ^ sb v ^ ")" ^ " <= " ^ "(" ^ sb w ^ ")"
        | `Apply (">=", [v; w]) -> "(" ^ sb v ^ ")" ^ " >= " ^ "(" ^ sb w ^ ")"
        | `Apply ("RLIKE", [v; w]) -> "(" ^ sb v ^ ")" ^ " RLIKE " ^ "(" ^ sb w ^ ")"
        | `Apply ("LIKE", [v; w]) -> "(" ^ sb v ^ ")" ^ " LIKE " ^ "(" ^ sb w ^ ")"
        | `Apply (f, args) when SqlFuns.is f -> SqlFuns.name f ^ "(" ^ String.concat "," (List.map sb args) ^ ")"
        | `Apply (f, args) -> f ^ "(" ^ String.concat "," (List.map sb args) ^ ")"
        | `Empty q -> "not exists (" ^ string_of_query db true q ^ ")"
        | `Length q -> "select count(*) from (" ^ string_of_query db true q ^ ") as " ^ fresh_dummy_var ()
        | `RowNumber [] -> "1"
        | `RowNumber ps ->
          "row_number() over (order by " ^ String.concat "," (List.map (string_of_projection db one_table) ps) ^ ")"
  and string_of_projection db one_table (var, label) =
    if one_table then
      db#quote_field label
    else
      string_of_table_var var ^ "." ^ (db#quote_field label)

  let string_of_query db range q =
    let range =
      match range with
        | None -> ""
        | Some (limit, offset) -> " limit " ^string_of_int limit^" offset "^string_of_int offset
    in
      string_of_query db false q ^ range

  let rec prepare_clauses : t -> t list =
    function
      | `Concat vs -> vs
      | v -> [v]

  type index = (Var.var * string) list

  let gens_index gs =
    let table_index (x, source) =
      let t = match source with `Table t -> t | _ -> assert false in
      let field_types = table_field_types t in
      let labels = labels_of_field_types field_types in
        List.rev
          (StringSet.fold
             (fun name ps -> (x, name) :: ps)
             labels
             [])
    in
      concat_map table_index gs

  let outer_index gs_out = gens_index gs_out
  let inner_index z gs_in =
    (* it's just a dynamic index! *)
    (z, "2") :: gens_index gs_in
(*    (z, "2_1") :: (z, "2_2") :: gens_index gs_in *)

  let extract_gens =
    function
      | `For (_, gs, _, _) -> gs
      | _ -> assert false

  let rec let_clause : Value.database -> FlattenRecords.let_clause -> query =
    fun db (q, outer, z, inner) ->
      let gs_out = extract_gens outer in
      let gs_in = extract_gens inner in
        `With (q,
               clause db (outer_index gs_out) false outer,
               z,
               clause db (inner_index z gs_in) false inner)
  and clause : Value.database -> index -> bool -> t -> query = fun db index unit_query v ->
(*    Debug.print ("clause: "^string_of_t v); *)
    match v with
      | `Concat _ -> assert false
      | `For (_, [], _, body) ->
          clause db index unit_query body
      | `For (_, (x, `Table (_db, table, _row))::gs, os, body) ->
          let body = clause db index unit_query (`For (None, gs, [], body)) in
          let os = List.map (base db index) os in
            begin
              match body with
                | `Select (fields, tables, condition, []) ->
                    `Select (fields, (table, x)::tables, condition, os)
                | _ -> assert false
            end
      | `If (c, body, `Concat []) ->
        (* Turn conditionals into where clauses. We might want to do
           this earlier on.  *)
        let c = base db index c in
        let body = clause db index unit_query body in
          begin
            match body with
              | `Select (fields, tables, c', os) ->
                let c =
                  match c, c' with
                    (* optimisations *)
                    | `Constant (`Bool true), c
                    | c, `Constant (`Bool true) -> c
                    | `Constant (`Bool false), _
                    | _, `Constant (`Bool false) -> `Constant (`Bool false)
                    (* default case *)
                    | c, c' -> `Apply ("&&", [c; c'])
                in
                  `Select (fields, tables, c, os)
              | _ -> assert false
          end
      | `Table (_db, table, (fields, _, _)) ->
        (* eta expand tables. We might want to do this earlier on.  *)
        (* In fact this should never be necessary as it is impossible
           to produce non-eta expanded tables. *)
        let var = fresh_table_var () in
        let fields =
          List.rev
            (StringMap.fold
               (fun name _ fields ->
                 (`Project (var, name), name)::fields)
               fields
               [])
        in
          `Select (fields, [(table, var)], `Constant (`Bool true), [])
      | `Singleton _ when unit_query ->
        (* If we're inside an `Empty or a `Length it's safe to ignore
           any fields here. *)
        `Select ([], [], `Constant (`Bool true), [])
      | `Singleton (`Record fields) ->
        let fields =
          List.rev
            (StringMap.fold
               (fun name v fields ->
                 (base db index v, name)::fields)
               fields
               [])
        in
          `Select (fields, [], `Constant (`Bool true), [])
      | _ -> assert false
  and base : Value.database -> index -> t -> base = fun db index ->
    function
      | `If (c, t, e) ->
        `Case (base db index c, base db index t, base db index e)
      | `Apply ("tilde", [s; r]) ->
        begin
          match likeify r with
            | Some r ->
              `Apply ("LIKE", [base db index s; `Constant (`String r)])
            | None ->
              let r =
                    (* HACK:
                       
                       this only works if the regexp doesn't include any variables bound by the query
                    *)
                    `Constant (`String (Regex.string_of_regex (Linksregex.Regex.ofLinks (value_of_expression r))))
                  in
                    `Apply ("RLIKE", [base db index s; r])
          end
      | `Apply ("Empty", [v]) ->
          `Empty (unit_query db v)
      | `Apply ("length", [v]) ->
          `Length (unit_query db v)
      | `Apply (f, vs) ->
          `Apply (f, List.map (base db index) vs)
      | `Project (`Var (x, _field_types), name) ->
          `Project (x, name)
      | `Constant c -> `Constant c
      | `Primitive "index" -> `RowNumber index
      | _ -> assert false

  (* convert a regexp to a like if possible *)
  and likeify v =
    let quote = Str.global_replace (Str.regexp_string "%") "\\%" in
      match v with
        | `Variant ("Repeat", pair) ->
            begin
              match unbox_pair pair with
                | `Variant ("Star", _), `Variant ("Any", _) -> Some ("%")
                | _ -> None
            end
        | `Variant ("Simply", `Constant (`String s)) -> Some (quote s)
        | `Variant ("Quote", `Variant ("Simply", v)) ->
            (* TODO:
               
               detect variables and convert to a concatenation operation
               (this needs to happen in RLIKE compilation as well)
            *)
           let rec string =
              function
                | `Constant (`String s) -> Some s
                | `Singleton (`Constant (`Char c)) -> Some (string_of_char c)
                | `Concat vs ->
                    let rec concat =
                      function
                        | [] -> Some ""
                        | v::vs ->
                            begin
                              match string v with
                                | None -> None
                                | Some s ->
                                    begin
                                      match concat vs with
                                        | None -> None
                                        | Some s' -> Some (s ^ s')
                                    end
                            end
                    in
                      concat vs
                | _ -> None
            in
              opt_map quote (string v)
        | `Variant ("Seq", rs) ->
            let rec seq =
              function
                | [] -> Some ""
                | r::rs ->
                    begin
                      match likeify r with
                        | None -> None
                        | Some s ->
                            begin
                              match seq rs with
                                | None -> None
                                | Some s' -> Some (s^s')
                            end
                    end
            in
              seq (unbox_list rs)
        | `Variant ("StartAnchor", _) -> Some ""
        | `Variant ("EndAnchor", _) -> Some ""
        | _ -> assert false
  and unit_query db v =
    (* queries passed to Empty and Length
       (where we don't care about what data they return)
    *)
    `UnionAll (List.map (clause db [] true) (prepare_clauses v), 0)

  and query : Value.database -> FlattenRecords.query -> query =
    fun db cs ->
      `UnionAll (List.map (let_clause db) cs, 0)

  let unordered_query_package db (range: (int * int) option) t v =
    let t = Shred.nested_type_of_type t in
    (* Debug.print ("v: "^string_of_t v); *)
    reset_dummy_counter ();
    let w = `Concat (Split.query v) in
      (* Debug.print ("w: "^string_of_t w); *)
    let tagged_w = tag_query w in
    let shredded_w = Shred.shred_query tagged_w t in
    let lins_w = Shred.pmap (LetInsertion.lins_query) shredded_w in
    let flat_w = Shred.pmap (FlattenRecords.flatten_query) lins_w in
    let query_package = Shred.pmap ((string_of_query db range) -<- (query db)) flat_w in

    let shredded_t = Shred.shred_query_type t in
    let query_type_package = Shred.pmap (FlattenRecords.flatten_query_type) shredded_t in

    let typed_query_package = Shred.pzip query_package query_type_package in
      typed_query_package

    (* Debug.print ("concat vs: "^string_of_t (`Concat vs)); *)
    (* let q = `UnionAll (List.map (let_clause db) vs, 0) in *)
    (*   string_of_query db range q *)

  (* let update db ((x, table), where, body) = *)
  (*   reset_dummy_counter (); *)
  (*   let base = (base db) ->- (string_of_base db true) in *)
  (*   let where = *)
  (*     match where with *)
  (*       | None -> "" *)
  (*       | Some where -> *)
  (*           " where (" ^ base where ^ ")" in *)
  (*   let fields = *)
  (*     match body with *)
  (*       | `Record fields -> *)
  (*           String.concat "," *)
  (*             (List.map *)
  (*                (fun (label, v) -> db#quote_field label ^ " = " ^ base v) *)
  (*                (StringMap.to_alist fields)) *)
  (*       | _ -> assert false *)
  (*   in *)
  (*     "update "^table^" set "^fields^where *)

  (* let delete db ((x, table), where) = *)
  (*   reset_dummy_counter (); *)
  (*   let base = base db ->- (string_of_base db true) in *)
  (*   let where = *)
  (*     match where with *)
  (*       | None -> "" *)
  (*       | Some where -> *)
  (*           " where (" ^ base where ^ ")" *)
  (*   in *)
  (*     "delete from "^table^where *)
end
   

let compile_shredded : Value.env -> (int * int) option * Ir.computation
                       -> (Value.database * (string * Shred.flat_type) Shred.package) option =
  fun env (range, e) ->
    let v = Eval.eval env e in
      match used_database v with
        | None    -> None
        | Some db ->
          let t = type_of_expression v in
          let p = ShreddedSql.unordered_query_package db range t v in
            Some (db, p)

let compile : Value.env -> (int * int) option * Ir.computation
              -> (Value.database * string * Types.datatype) option =
  fun env (range, e) ->
    (* Debug.print ("e: "^Ir.Show_computation.show e); *)
    let v = Eval.eval env e in
      (* Debug.print ("v: "^string_of_t v); *)
      match used_database v with
        | None -> None
        | Some db ->
            let t = type_of_expression v in
            let q = Sql.unordered_query db range v in
              Debug.print ("Generated query: "^q);
              Some (db, q, t)

let compile_update : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option * Ir.computation) -> string =
  fun db env ((x, table, field_types), where, body) ->
    let env = Eval.bind (Eval.env_of_value_env env) (x, `Var (x, field_types)) in
(*      let () = opt_iter (fun where ->  Debug.print ("where: "^Ir.Show_computation.show where)) where in*)
    let where = opt_map (Eval.computation env) where in
(*       Debug.print ("body: "^Ir.Show_computation.show body); *)
    let body = Eval.computation env body in
    let q = Sql.update db ((x, table), where, body) in
      Debug.print ("Generated update query: "^q);
      q

let compile_delete : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option) -> string =
  fun db env ((x, table, field_types), where) ->
    let env = Eval.bind (Eval.env_of_value_env env) (x, `Var (x, field_types)) in
    let where = opt_map (Eval.computation env) where in
    let q = Sql.delete db ((x, table), where) in
      Debug.print ("Generated update query: "^q);
      q
