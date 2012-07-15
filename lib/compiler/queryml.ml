open Utility
open Irquery

let (++) = Env.Int.extend

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
      | `For (gs, os, _body) -> generators gs
      | `Table (_, db, _) -> Some db
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
  match (match v with
    | `Concat vs -> comprehensions vs
    | v -> used v
  ) with
	 | Some s ->
		  let driver, params = Value.parse_db_string s in Some (fst (Value.db_connect driver params))
	 | None -> None


let rec tail_of_t : query -> query = fun v ->
  let tt = tail_of_t in
    match v with
      | `For (_gs, _os, `Singleton (`Record fields)) -> `Record fields
      | `For (_gs, _os, `If (c, t, `Concat [])) -> tt (`For (_gs, _os, t))
      | _ -> (* Debug.print ("v: "^string_of_t v); *) assert false

let labels_of_field_types field_types =
  StringMap.fold
    (fun name _ labels' ->
      StringSet.add name labels')
    field_types
    StringSet.empty

let table_field_types (_t, _db, fields) = fields

let rec field_types_of_list =
  function
    | `Concat (v::vs) -> field_types_of_list v
    | `Singleton (`Record fields) -> StringMap.fold (fun k _ s -> StringSet.add k s) fields StringSet.empty
    | `Table table -> table_field_types table
    | _ -> assert false

(*
let rec field_types_of_list =
  function
    | `Concat (v::vs) -> field_types_of_list v
    | `Singleton (`Record fields) -> StringMap.map type_of_expression fields
    | `Table table -> table_field_types table
    | _ -> assert false
*)

(** Not too stupid error handling *)

exception DbEvaluationError of string

let query_error fmt = 
  let error msg = raise (DbEvaluationError msg) in
  Printf.kprintf error fmt
	


module IRquery2query =
struct



  let nil = `Concat []

  (* takes a normal form expression and returns true iff it has list type *)
  let is_list =
    function
      | `For _
      | `Table _
      | `Singleton _
      | `Concat _
      | `If (_, _, `Concat []) -> true
      | _ -> false    


  (** η-expansion for variables and lists *)
  let eta_expand_var (x, field_types) =
    `Record
      (StringSet.fold
         (fun name fields ->
            StringMap.add name (`Project (`Var (x, field_types),name)) fields)
         field_types
         StringMap.empty)

  let eta_expand_list xs =
    let x = Var.fresh_raw_var () in
    let field_types = field_types_of_list xs in
      ([x, xs], [], `Singleton (eta_expand_var (x, field_types)))


  (* Some usefull functions *)
  let bind env (x,v) =
    Env.Int.bind env (x, v)

  let lookup env x =
	 match Env.Int.find env x with
		| Some v -> v
		| None -> failwith "unkonwn variable" (* TODO *)


  (** Those three functions visit the IR **)
  let rec value env : Irquery.value -> query = function
    | Constant c -> `Constant c
    | Variable var ->
		  lookup env var
	 | Primitive f -> `Primitive f
    | Extend (ext_fields, r) -> 
      begin
        match opt_app (value env) (`Record StringMap.empty) r with
          | `Record fields ->
            `Record (StringMap.fold 
                       (fun label v fields ->
                         if StringMap.mem label fields then 
                           query_error
                             "Error adding fields: label %s already present"
                             label
                         else
                           StringMap.add label (value env v) fields)
                       ext_fields
                       fields)
          | _ -> query_error "Error adding fields: non-record"
      end
    | Project (label,r) -> `Project (value env r,label)
    | Erase (labels, r) -> `Erase (labels, value env r)
    | Inject (label, v) -> `Variant (label, value env v)
    | ApplyPure (f, ps) -> apply env ((value env f), List.map (value env) ps)
	 | Table (t,db,fields) -> begin match (value env t),(value env db) with
		  | `Constant (String t), `Database db -> `Table (t,db,fields)
		  | _ -> assert false
	 end
	 | Database db -> begin match value env db with
		  | `Constant (String s) -> `Database s
		  | _ -> assert false
	 end

  and computation env (binders, tailcomp) : query =
    match binders with
      | [] -> tail_computation env tailcomp
      | b::bs ->
          begin match b with
            | Let (x, tc) ->
                computation (bind env (x, computation env tc)) (bs, tailcomp)
            | Fun (f, args, body) | FunQ (f,args,body) ->
                computation
                  (bind env (f, `Closure ((args, body), env)))
                    (bs, tailcomp)
          end

  and tail_computation env : Irquery.tail_computation -> query = function
    | Return v -> value env v
    | Apply (f, args)
    | ApplyDB (f, args) ->
        apply env (value env f, List.map (value env) args)
    | Case (v, cases, default) -> reduce_case env (value env v, cases, default)
    | If (c, t, e) ->
      let c = value env c in
      let t = computation env t in
      let e = computation env e in
        reduce_if_condition (c, t, e)

			 


  (** β-reduction inside the type query **)
  and apply env : query * query list -> query = function
    | `Closure ((xs, body), closure_env), args ->
        let env = env ++ closure_env in
        let env = List.fold_right2 (fun x arg env -> bind env (x, arg)) xs args env in
        computation env body
    | `Primitive "AsList", [xs] -> xs
    | `Primitive "Cons", [x; xs] -> reduce_concat [`Singleton x; xs]
    | `Primitive "Concat", [xs; ys] -> reduce_concat [xs; ys]
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
                    | `For (gs, os', body) -> gs, os', body
                    | `Concat (_::_)
                    | `Singleton _
                    | `Table _ ->
                        (* I think we can omit the `Table case as it
                           can never occur *)
                        (* eta-expand *)
                        eta_expand_list xs
                    | _ -> assert false in
                let xs = `For (gs, os', body) in
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
                            `For (gs, os @ os', body)
                      | _ -> assert false
                  end
        end
    | `Primitive "not", [v] ->   reduce_not (v)
    | `Primitive "&&", [v; w] -> reduce_and (v, w)
    | `Primitive "||", [v; w] -> reduce_or (v, w)
    | `Primitive "==", [v; w] -> reduce_eq (v, w)
    | `Primitive f, args -> `Apply (f, args)
    | `If (c, t, e), args ->  reduce_if_condition (c, apply env (t, args), apply env (e, args))
    | `Apply (f, args), args' -> `Apply (f, args @ args')
    | _ -> query_error "Application of non-function"


  (** Bunch of optimizing function **)
  and reduce_case env (v, cases, default) =
    match v with
      | `Variant (label, v) as w ->
          begin
            match StringMap.lookup label cases, default with
              | Some (x, c), _ ->
                  computation (bind env (x, v)) c
              | None, Some (z, c) ->
                  computation (bind env (z, w)) c
              | None, None -> query_error "Pattern matching failed"
          end
      | `If (c, t, e) ->
          `If
            (c,
             reduce_case env (t, cases, default),
             reduce_case env (e, cases, default))
      |  _ -> assert false
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
  and reduce_for_source : query * (query -> query) -> query =
    fun (source, body) ->
      let rs = fun source -> reduce_for_source (source, body) in
        match source with
          | `Singleton v -> body v
          | `Concat vs ->
            reduce_concat (List.map rs vs)
          | `If (c, t, `Concat []) ->
            reduce_for_source
              (t, fun v -> reduce_where_then (c, body v))
          | `For (gs, os, v) ->
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
              reduce_for_body ([(x, source)], [], body (`Var (x, field_types)))
          | v -> query_error "Bad source in for comprehension"
  and reduce_for_body (gs, os, body) =
    match body with
      | `For (gs', os', body') -> `For (gs @ gs', os @ os', body')
      | _                      -> `For (gs, os, body)
  and reduce_if_condition (c, t, e) =
    match c with
      | `Constant (Bool true) -> t
      | `Constant (Bool false) -> e
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
      | `Constant (Bool true) -> t
      | `Constant (Bool false) -> `Concat []

      | `Concat vs ->
        reduce_concat (List.map (fun v -> reduce_where_then (c, v)) vs)
      | `For (gs, os, body) ->
        `For (gs, os, reduce_where_then (c, body))
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
          | _ -> query_error "Mismatched fields"
        end
      | _ ->
        begin
          match t, e with
            | `Constant (Bool true), _ ->
              reduce_or (c, e)
            | _, `Constant (Bool false) ->
              reduce_and (c, t)
            | _ ->
              `If (c, t, e)
        end
  (* simple optimisations *)
  and reduce_and (a, b) =
    match a, b with
      | `Constant (Bool true), x
      | x, `Constant (Bool true)
      | (`Constant (Bool false) as x), _
      | _, (`Constant (Bool false) as x) -> x
      | _ -> `Apply ("&&", [a; b])
  and reduce_or (a, b) =
    match a, b with
      | (`Constant (Bool true) as x), _
      | _, (`Constant (Bool true) as x)
      | `Constant (Bool false), x
      | x, `Constant (Bool false) -> x
      | _ -> `Apply ("||", [a; b])
  and reduce_not a =
    match a with
      | `Constant (Bool false) -> `Constant (Bool true)
      | `Constant (Bool true)  -> `Constant (Bool false)
      | _                       -> `Apply ("not", [a])
  and reduce_eq (a, b) =
    let bool x = `Constant (Bool x) in
    let eq_constant =
      function
        | (Bool a  , Bool b)   -> bool (a = b)
        | (Int a   , Int b)    -> bool (Num.eq_num a b)
        | (Float a , Float b)  -> bool (a = b)
        | (Char a  , Char b)   -> bool (a = b)
        | (String a, String b) -> bool (a = b)
        | (a, b)                 -> `Apply ("==", [`Constant a; `Constant b])
    in begin
      match a, b with
        | (`Constant a, `Constant b) -> eq_constant (a, b)
        | (`Variant (s1, a), `Variant (s2, b)) ->
          if s1 <> s2 then
            `Constant (Bool false)
          else
            reduce_eq (a, b)              
        | (`Record lfields, `Record rfields) -> 
          List.fold_right2
            (fun (s1, v1) (s2, v2) e ->
              reduce_and (reduce_eq (v1, v2), e))
            (StringMap.to_alist lfields)
            (StringMap.to_alist rfields)
            (`Constant (Bool true))
        | (a, b) -> `Apply ("==", [a; b])
	 end

  let eval e =
	 (*    Debug.print ("e: "^Irquery.Show_computation.show e); *)
    computation Env.Int.empty e

end


(* Introducing ordering indexes in order to support a list
   semantics. *)
module Order =
struct
  type gen = Var.var * query
  type context = gen list


  type query_tree = [ `Node of (int * query_tree) list
                    | `Leaf of (context * query) ]

  type path = int list

  type preclause = (path * (context * query)) * query_tree
  type clause = context * query

  let gen : (Var.var * query) -> query list =
    function
      | (x, `Table t) ->
        let field_types = table_field_types t in 
          List.rev
            (StringSet.fold
               (fun name es ->
                 `Project (`Var (x, field_types), name) :: es
               ) field_types [])
      | _ -> assert false

  let gens : (Var.var * query) list -> query list = concat_map gen

  let rec query : context -> query -> query -> query_tree =
    fun gs cond ->
      function
        | `Concat vs ->
          let cs = queries gs cond vs in
            `Node []
        | `If (cond', v, `Concat []) ->
          query gs (IRquery2query.reduce_and (cond, cond')) v
        | `For (gs', os, `Concat vs) ->
          let cs = queries (gs @ gs') cond vs in
            `Node cs
        | `For (gs', os, body) ->
          `Leaf (gs @ gs',
                  IRquery2query.reduce_where_then (cond, body))
        | `Singleton r ->
          `Leaf (gs, IRquery2query.reduce_where_then (cond, `Singleton r))
        | _ -> assert false
  and queries : context -> query -> query list -> (int * query_tree) list =
    fun gs cond vs ->
      let i, cs =
        List.fold_left
          (fun (i, cs) v ->
            let c = query gs cond v in
              (i+1, (i, c)::cs))
          (1, [])
          vs
      in
        List.rev cs

  (* convert all order indexes to default values *)
  let rec mask : query_tree -> query_tree =
    let dv =
      List.map
        (function
          | `Gen g -> `DefGen g
          | `TailGen g -> `DefTailGen g
          | _ -> assert false)
    in
      function
        | `Node cs -> `Node (mask_children cs)
        | `Leaf x  -> `Leaf x
  and mask_children : (int * query_tree) list -> (int * query_tree) list =
    fun cs ->
      List.map (fun (branch, tree) -> (branch, mask tree)) cs

  (* decompose a query tree into a list of preclauses
     (path, query, tree) *)
  let rec decompose : query_tree -> preclause list =
    function
      | `Leaf q -> [(([], q), `Leaf q)]
      | `Node cs ->
        List.map
          (fun ((path, q), cs) ->
            ((path, q), `Node cs))
          (decompose_children [] cs)
  and decompose_children prefix : (int * query_tree) list
      -> ((int list * (context * query)) * (int * query_tree) list) list =
    function
      | [] -> []
      | (branch, tree) :: cs ->
        let xs = decompose tree in
        let m = mask tree in
        let ms = mask_children cs in
          List.map
            (fun ((path, q), tree) ->
              ((branch :: path, q), prefix @ (branch, tree) :: ms))
            xs
          @ decompose_children (prefix @ [(branch, m)]) cs

  (* compute the order indexes for the specified query tree along a
     path *)

  (* flatten a query tree as a list of subqueries *)
  let flatten_tree q =
    List.map
      (fun ((path, (gs, body)), tree) ->
        (gs, body))
      (decompose q)

  let query =
    fun v ->
      let q = query [] (`Constant (Bool true)) v in
      let ss = flatten_tree q in
      ss
			 
  (* FIXME:

     Be more careful about ensuring that the order index field names
     do not clash with existing field names *)
  
  let unordered_query_of_clause (gs, body) =
    match gs with
      | [] -> body
      | _  -> `For (gs, [], body)

  let unordered_query v =
    List.map unordered_query_of_clause (query v)
end

module Sql =
struct
  type sql_query =
    [ `UnionAll of sql_query list * int
    | `Select of (base * string) list * (string * Var.var) list * base * base list ]
  and base =
    [ `Case of (base * base * base)
    | `Constant of constant
    | `Project of Var.var * string
    | `Apply of string * base list
    | `Empty of sql_query
    | `Length of sql_query ]

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
        | `Select (fields, [], `Constant (Bool true), _os) ->
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
                | `Constant (Bool true) -> ""
                | _ ->  " where " ^ sb condition
            in
              "select " ^ fields ^ " from " ^ tables ^ where ^ orderby
  and string_of_base db one_table b =
    let sb = string_of_base db one_table in
      match b with
        | `Case (c, t, e) ->
            "case when " ^ sb c ^ " then " ^sb t ^ " else "^ sb e ^ " end"
        | `Constant c -> string_of_constant c
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
        | Some (limit, offset) -> " limit " ^Num.string_of_num limit^" offset "^Num.string_of_num offset
    in
      string_of_query db false q ^ range

  let rec prepare_clauses : query -> query list =
    function
      | `Concat vs -> vs
      | v -> [v]

  let rec clause : Value.database -> query -> sql_query = fun db v ->
(*    Debug.print ("clause: "^string_of_t v); *)
    match v with
      | `Concat _ -> assert false
      | `For ([], _, body) ->
          clause db body
      | `For ((x, `Table (table, _db, _fields))::gs, os, body) ->
          let body = clause db (`For (gs, [], body)) in
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
                    | `Constant (Bool true), c
                    | c, `Constant (Bool true) -> c
                    | `Constant (Bool false), _
                    | _, `Constant (Bool false) -> `Constant (Bool false)
                    (* default case *)
                    | c, c' -> `Apply ("&&", [c; c'])
                in
                  `Select (fields, tables, c, os)
              | _ -> assert false
          end
      | `Table (table, db, fields ) ->
        (* eta expand tables. We might want to do this earlier on.  *)
        (* In fact this should never be necessary as it is impossible
           to produce non-eta expanded tables. *)
        let var = fresh_table_var () in
        let fields =
          List.rev
            (StringSet.fold
               (fun name fields ->
                 (`Project (var, name), name)::fields)
               fields
               [])
        in
          `Select (fields, [(table, var)], `Constant (Bool true), [])
      | `Singleton (`Record fields) ->
        let fields =
          List.rev
            (StringMap.fold
               (fun name v fields ->
                 (base db v, name)::fields)
               fields
               [])
        in
          `Select (fields, [], `Constant (Bool true), [])

      | `Singleton _ ->
        (* If we're inside an `Empty or a `Length it's safe to
           ignore any fields here. Otherwise this line should be
           unreachable. *)
        `Select ([], [], `Constant (Bool true), [])
      | _ -> assert false
  and base : Value.database -> query -> base = fun db ->
    function
      | `If (c, t, e) ->
        `Case (base db c, base db t, base db e)
      | `Apply ("tilde", [s; r]) ->
        begin
          match likeify r with
            | Some r ->
              `Apply ("LIKE", [base db s; `Constant (String r)])
            | None ->
              let r =
                    (* HACK:
                       
                       this only works if the regexp doesn't include any variables bound by the query
                    *)
                    `Constant (String (Regex.string_of_regex (Lregex.Regex.ofLinks  r)))
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
        | `Variant ("Simply", `Constant (String s)) -> Some (quote s)
        | `Variant ("Quote", `Variant ("Simply", v)) ->
            (* TODO:
               
               detect variables and convert to a concatenation operation
               (this needs to happen in RLIKE compilation as well)
            *)
           let rec string =
              function
                | `Constant (String s) -> Some s
                | `Singleton (`Constant (Char c)) -> Some (string_of_char c)
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
    let vs = Order.unordered_query v in
    (* Debug.print ("concat vs: "^string_of_t (`Concat vs)); *)
    let q = `UnionAll (List.map (clause db) vs, 0) in
      string_of_query db range q

  let wonky_query db range v =
(*     Debug.print ("v: "^string_of_t v); *)
    reset_dummy_counter ();
    let q = outer_query db v in
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

let compile : (Num.num * Num.num) option * Irquery.computation -> (Value.database * string) option =
  fun (range, e) ->
    (* Debug.print ("e: "^Show.show Irquery.show_computation e); *)
    let v = IRquery2query.eval e in
      (* Debug.print ("v: "^string_of_t v); *)
    match used_database v with
      | None -> None
      | Some db ->
          let q = Sql.unordered_query db range v in
          Debug.print ("Generated query: "^q);
          Some (db, q)

(*		
let compile_update : Value.database -> 
  ((Irquery.var * string * Types.datatype StringMap.t) * Irquery.computation option * Irquery.computation) -> string =
  fun db ((x, table, field_types), where, body) ->
    let env = IRquery2query.bind (IRquery2query.env_of_value_env env) (x, `Var (x, field_types)) in
	 (*let () = opt_iter (fun where ->  Debug.print ("where: "^Irquery.Show_computation.show where)) where in*)
    let where = opt_map (IRquery2query.computation env) where in
	 (*Debug.print ("body: "^Irquery.Show_computation.show body); *)
    let body = IRquery2query.computation env body in
    let q = Sql.update db ((x, table), where, body) in
    Debug.print ("Generated update query: "^q);
    q
		
let compile_delete : Value.database ->
  ((Irquery.var * string * Types.datatype StringMap.t) * Irquery.computation option) -> string =
  fun db ((x, table, field_types), where) ->
    let env = IRquery2query.bind (IRquery2query.env_of_value_env env) (x, `Var (x, field_types)) in
    let where = opt_map (IRquery2query.computation env) where in
    let q = Sql.delete db ((x, table), where) in
    Debug.print ("Generated update query: "^q);
    q
*)
