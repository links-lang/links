open Utility
module Q = Query

(* generate a unique tag for each comprehension in
   a normalised query
*)

let tag_query : Q.t -> Q.t =
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
        | `Database db -> `Database db
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
      [@@deriving show]

  type 'a shredded = [`Primitive of 'a | `Record of ('a shredded) StringMap.t]
      [@@deriving show]
  type shredded_type = Types.primitive shredded
      [@@deriving show]
  type shredded_value = Value.t shredded
      [@@deriving show]

  type flat_type =
    [ `Primitive of Types.primitive
    | `Record of Types.primitive StringMap.t ]
      [@@deriving show]

  type 'a package =
    [ `Primitive of Types.primitive
    | `Record of 'a package StringMap.t
    | `List of 'a package * 'a ]
      [@@deriving show]

  type step = [ `List | `Record of string ]
  type path = step list

  (* This seems a bit dodgey...  what if we have a polymorphic type,
     for instance?

     We should be OK, as we always infer the type from a normalised
     expression. *)
  let rec nested_type_of_type : Types.datatype -> nested_type =
    fun t ->
    match TypeUtils.concrete_type t with
    | `Primitive t -> `Primitive t
    | `Record (fields, _, _) -> `Record (StringMap.map (function
                                                         | `Present t -> nested_type_of_type t
                                                         | _ -> assert false) fields)
    | `Application (l, [`Type t]) when l = Types.list ->
       `List (nested_type_of_type t)
    | t ->
       Debug.print ("Can't convert to nested_type: " ^ Types.string_of_datatype t);
       assert false

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
        | `Primitive t1, `Primitive _ -> `Primitive t1
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
        | _, _ -> assert false

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
      | `Project p -> `Project p
      | `Apply ("Empty", [e]) -> `Apply ("Empty", [shred_outer e []])
      | `Apply ("length", [e]) -> `Apply ("length", [shred_outer e []])
      | `Apply (f, vs) -> `Apply (f, List.map (shinner a) vs)
      | `Record fields ->
        `Record (StringMap.map (shinner a) fields)
      | e when Q.is_list e ->
        in_index a
      | e -> e

  (* outer shredding function *)
  and shouter a p : Q.t -> Q.t list =
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
         Debug.print ("Can't apply shouter to: " ^ Q.show e);
         assert false

  and shred_outer q p = `Concat (shouter top p q)

  let shred_query : Q.t -> nested_type -> Q.t package =
    fun q t -> package (shred_outer q) t


  let rec shred_inner_type : nested_type -> shredded_type =
    function
      | `Primitive p   -> `Primitive p
      | `Record fields -> `Record (StringMap.map shred_inner_type fields)
      | `List _        ->
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
  type gen = Var.var * Q.t

  let rec query : gen list -> Q.t list -> Q.t -> Q.t -> Q.t list =
    fun gs os cond ->
      function
        | `Singleton r ->
          [`For (None, gs, os, Q.Eval.reduce_where_then (cond, `Singleton (inner r)))]
        | `Concat vs ->
          concat_map (query gs os cond) vs
        | `If (cond', v, `Concat []) ->
          query gs os (Q.Eval.reduce_and (cond, cond')) v
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
      | e when Q.is_list e ->
        `Concat (query [] [] (`Constant (`Bool true)) e)
      | _ -> assert false

  let query : Q.t -> Q.t list =
    query [] [] (`Constant (`Bool true))
end


module LetInsertion =
struct
  type let_clause = Var.var * Q.t * Var.var * Q.t
      [@@deriving show]
  type query = let_clause list
      [@@deriving show]

  type cond = Q.t option
  type gen = Var.var * Q.t

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
      | [_]      -> []
      | x::y::xs -> x::(init (y::xs))
      | []       -> assert false

  let rec last =
    function
      | [x]      -> x
      | _::y::xs -> last (y::xs)
      | []       -> assert false

  let rec gens : Q.t -> (gen list) list =
    function
      | `Singleton _           -> []
      | `If (_, t, `Concat []) -> gens t
      | `For (_, gs, _, e)     -> gs :: gens e
      | _                      -> assert false

  let rec orders : Q.t -> (Q.t list) list =
    function
      | `Singleton _           -> []
      | `If (_, t, `Concat []) -> orders t
      | `For (_, _, os, e)     -> os :: orders e
      | _                      -> assert false

  let rec conds : Q.t -> cond list =
    function
      | `Singleton _                           -> []
      | `For (_, _, _, `If (c, t, `Concat [])) -> Some c :: conds t
      | `For (_, _, _, e)                      -> None :: conds e
      | _                                      -> assert false

  let rec body : Q.t -> Q.t =
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

  let rec lins_inner (z, z_fields) ys : Q.t -> Q.t =
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
      | `Apply ("Empty", [e]) -> `Apply ("Empty", [lins_inner_query (z, z_fields) ys e])
      | `Apply ("length", [e]) -> `Apply ("length", [lins_inner_query (z, z_fields) ys e])
      | `Apply (f, es) ->
        `Apply (f, List.map (lins_inner (z, z_fields) ys) es)
      | `Record fields ->
        `Record (StringMap.map (lins_inner (z, z_fields) ys) fields)
      | `Primitive "out" ->
        (* z.2 *)
        `Project (`Var (z, z_fields), "2")
      | `Primitive "in"  -> `Primitive "index"
      | `Constant c      -> `Constant c
      | e ->
        Debug.print ("Can't apply lins_inner to: " ^ Q.show e);
        assert false

  and lins_inner_query (z, z_fields) ys : Q.t -> Q.t =
    fun e ->
      let li = lins_inner (z, z_fields) ys in
      let liq = lins_inner_query (z, z_fields) ys in
        match e with
          | `Concat es -> `Concat (List.map liq es)
          | `For (tag, gs, os, body) ->
            `For (tag, gs, List.map li os, liq body)
          | `If (c, t, `Concat []) -> `If (li c, liq t, `Concat [])
          (* OPTIMISATION:

             For Empty and length we don't care about what the body
             returns.
          *)
          | `Singleton _ -> `Singleton (`Record StringMap.empty)
          | e ->
            Debug.print ("Can't apply lins_inner_query to: " ^ Q.show e);
            assert false

  let rec lins c : let_clause =
    let gs_out = List.concat (init (gens c)) in

    let ys = List.map fst gs_out in

    let x_out =
      List.fold_right
        (fun x y ->
          match x, y with
            | None,   _       -> y
            | _   ,   None    -> x
            | Some c, Some c' -> Some (Q.Eval.reduce_and (c, c')))
        (init (conds c))
        None in

    let r_out =
      tuple (List.map
               (fun (x, source) ->
                 match source with
                   | `Table t ->
                     Q.Eval.eta_expand_var (x, Q.table_field_types t)
                   | _ -> assert false)
               gs_out) in
    let r_out_type =
      Types.make_tuple_type
        (List.map
           (fun (_, source) ->
             match source with
               | `Table (_, _, _, row) ->
                 `Record row
               | _ -> assert false)
           gs_out) in

    let gs_in = last (gens c) in
    let x_in = last (conds c) in

    let os = List.concat (orders c) in
    let q = Var.fresh_raw_var () in
    let z = Var.fresh_raw_var () in
    let z_fields =
      Q.record_field_types
        (Types.make_tuple_type
           [r_out_type; index_type])
    in
      (q, `For (None, gs_out, [], where x_out (`Singleton (pair r_out index))),
       z, `For (None, gs_in, os,
                where
                  (opt_map (lins_inner (z, z_fields) ys) x_in)
                  (`Singleton (lins_inner (z, z_fields) ys (body c)))))

  and lins_query : Q.t -> query =
    function
      | `Concat cs -> List.map lins cs
      | _          -> assert false

end


module FlattenRecords =
struct
  open Shred

  type let_clause = LetInsertion.let_clause
  type query = LetInsertion.query


  let rec flatten_inner : Q.t -> Q.t =
    function
      | `Constant c    -> `Constant c
      | `Primitive p   -> `Primitive p
      | `Apply ("Empty", [e]) -> `Apply ("Empty", [flatten_inner_query e])
      | `Apply ("length", [e]) -> `Apply ("length", [flatten_inner_query e])
      | `Apply (f, es) -> `Apply (f, List.map flatten_inner es)
      | `If (c, t, e)  ->
        `If (flatten_inner c, flatten_inner t, flatten_inner e)
      | `Project (`Var x, l) -> `Project (`Var x, l)
      | `Project (`Project (`Project (`Var z, "1"), i), l) ->
        (* HACK: this keeps z annotated with its original unflattened type *)
        `Project (`Var z, "1"^"@"^i^"@"^l)
      | `Record fields ->
        (* concatenate labels of nested records *)
        `Record
          (StringMap.fold
             (fun name body fields ->
               match flatten_inner body with
                 | `Record inner_fields ->
                   StringMap.fold
                     (fun name' body fields ->
                       StringMap.add (name ^ "@" ^ name') body fields)
                     inner_fields
                     fields
                 | body ->
                   StringMap.add name body fields)
             fields
             StringMap.empty)
      | e ->
        Debug.print ("Can't apply flatten_inner to: " ^ Q.show e);
        assert false

  and flatten_inner_query : Q.t -> Q.t = fun e -> flatten_comprehension e

  and flatten_comprehension : Q.t -> Q.t =
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
            | p -> `Record (StringMap.add "@" p StringMap.empty)
        in
          `Singleton e'
      (* HACK: not sure if `Concat is supposed to appear here...
         but it can do inside "Empty" or "Length". *)
      | `Concat es ->
        `Concat (List.map flatten_comprehension es)
      | e ->
        Debug.print ("Can't apply flatten_comprehension to: " ^ Q.show e);
        assert false

  let flatten_let_clause : LetInsertion.let_clause -> let_clause =
    function
      | (q, outer, z, inner) ->
        (q, flatten_comprehension outer, z, flatten_comprehension inner)

  let flatten_query : LetInsertion.query -> query =
    fun q ->
(*      Debug.print ("Unflattened query: " ^ Show.show LetInsertion.show_query q); *)
      let q' = List.map flatten_let_clause q in
(*        Debug.print ("flattened query: " ^ Show.show LetInsertion.show_query q');*)
        q'

  let rec flatten_type : shredded_type -> flat_type =
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
                       StringMap.add (name ^ "@" ^ name') t fields)
                     inner_fields
                     fields
                 | `Primitive p ->
                   StringMap.add name p fields)
             fields
             StringMap.empty)


  let flatten_query_type : shredded_type -> flat_type = flatten_type

  (* add a flattened field to an unflattened record (type or value) *)
  let rec unflatten_field : string list -> 'a -> ('a shredded) StringMap.t -> ('a shredded) StringMap.t =
    fun names v fields ->
      match names with
        | [name] -> StringMap.add name (`Primitive v) fields
        | name::name'::names ->
          let fields' =
            if StringMap.mem name fields then
              let w = StringMap.find name fields in
                match w with
                  | `Record fields' -> fields'
                  | _ -> assert false
            else
              StringMap.empty in
          let fields' = unflatten_field (name'::names) v fields' in
            StringMap.add name (`Record fields') fields
        | [] -> assert false

  (* fill in any unit fields that are apparent from the type but not
     present in the flattened value *)
  let rec fill : shredded_type -> shredded_value -> shredded_value =
    fun t v ->
      match t, v with
        | `Primitive _, `Primitive v -> `Primitive v
        | `Record fts, `Record fs ->
          `Record
            (StringMap.fold
               (fun name t fields ->
                 let v =
                   if StringMap.mem name fs then
                     StringMap.find name fs
                   else
                     `Record (StringMap.empty)
                 in
                   StringMap.add name (fill t v) fields)
               fts
               StringMap.empty)
        | _ -> assert false

  let unflatten_type : flat_type -> shredded_type =
    function
      | `Primitive p -> `Primitive p
      | `Record fields ->
        `Record
          (StringMap.fold
             (fun name p fields ->
               let names = split_string name '@' in
                 unflatten_field names p fields)
             fields
             StringMap.empty)

(*
Fast unflattening.
1. Following definition of unflatten_type, build a record template that shows how to construct each unflattened record by copying fields from flattened record.  (This can bake-in the "fill" operation too.)
2. Define a function that takes a flattened record and template and constructs the corresponding unflattened record.
3. Map across the list of records.
*)

(* code used in fast version of unflatten_list and Stitch *)
  type 'a template =
    [ `Primitive of 'a
    | `Record of (string * 'a template) list
    ]

  let rec template_map f tmpl =
    match tmpl with
      `Primitive p -> `Primitive (f p)
    | `Record r -> `Record (List.map (fun (n,t) -> (n,template_map f t)) r)

  let make_template : shredded_type -> string template =
    fun ty ->
    let rec make_tmpl_inner name t =
      match t with
      `Primitive _ -> `Primitive name
    | `Record rcd ->
	`Record (List.map (fun (nm,t') ->
	  (nm,make_tmpl_inner (name ^"@"^nm) t'))
		   (StringMap.to_alist rcd))
    and make_tmpl_outer t =
      match t with
	`Primitive _ -> `Primitive ""
      |	`Record rcd -> `Record (List.map (fun (nm,t') ->
	  (nm,make_tmpl_inner nm t'))
		   (StringMap.to_alist rcd))
    in make_tmpl_outer ty

  let build_unflattened_record : string template -> Value.t -> Value.t =
      fun template v_record  ->
	let record =
	  match v_record with
	    `Record r -> r
	  | _ -> assert false
	in
	let rec build t =
	  match t with
	    `Record rcd -> `Record (List.map (fun (n,t') -> (n,build t')) rcd)
	  | `Primitive field -> List.assoc field record

	in build template

  let unflatten_list : (Value.t * flat_type) -> Value.t =
    fun (v, t) ->
      match v with
        | `List vs ->
	    let st = unflatten_type t in
	    let tmpl = make_template st in
	  `List (List.map (build_unflattened_record tmpl) vs)
        | _ -> assert false

end


    (* TODO: Untangle dependency on FlattenRecords.template *)
module Stitch =
struct

  (* Stitching *)
(* First, we traverse Value.t package from bottom up and convert lists to value maps indexed by a,d pairs. *)

  let lookup (a,d) m =
    if IntPairMap.mem(a,d) m
    then IntPairMap.find (a,d) m
    else []

  let insert (a,d) w m =
    IntPairMap.add (a,d) (w::lookup(a,d) m) m

  let empty = IntPairMap.empty

  let rec stitch : Value.t -> Value.t list IntPairMap.t Shred.package -> Value.t =
    fun v t ->
      match v, t with
        | c, `Primitive _ -> c
        | `Record fs, `Record fts ->
          `Record
            (List.map (fun (l, v) -> (l, stitch v (StringMap.find l fts))) fs)
        | `Record [("1", `Int a); ("2", `Int d)], `List (t, m) ->
          (*`List (List.map (fun w -> stitch w t)
		   (lookup (a, d) m))*)
          `List (List.fold_left (fun l w -> stitch w t::l) []
		   (lookup (a, d) m))
        | _, _ -> assert false


(* Builds maps from int.  This can be fed into the fast version of shredding above.  It may be possible to do better by having the incoming tables sorted in the database and building the maps by partitioning the tables in one pass.
Avoiding unnecessary static indexes, or multiplexing pairs (a,d) where a is usually small into a single integer, would also be a good optimization but would make things less uniform.  *)

  let build_unflattened_record_from_array
      : (Types.datatype * int) FlattenRecords.template -> string array -> Value.t =
    fun template array ->
    let rec build t =
      match t with
	`Record rcd -> `Record (List.map (fun (n,t') -> (n,build t')) rcd)
      | `Primitive (ty,idx) -> Database.value_of_db_string (Array.get array idx) ty

    in build template


  let build_stitch_map (((vs:Value.dbvalue),rs),t) =
    let st = FlattenRecords.unflatten_type t in
    let tmpl = FlattenRecords.make_template st in
    let tmpl' = FlattenRecords.template_map (fun x -> List.assoc x rs) tmpl in
    let (a_idx,d_idx,w_tmpl) =
       match tmpl' with
      | `Record [("1", (`Record [ ("1", `Primitive(_,a_idx));
				  ("2", `Primitive(_,d_idx))]));
		  ("2", w_tmpl)] -> (a_idx,d_idx,w_tmpl)
      |	 _ -> assert false in
    let add_row_to_map row m =
      let w = build_unflattened_record_from_array w_tmpl row in
      let a = int_of_string(Array.get row a_idx) in
      let d = int_of_string(Array.get row d_idx) in
      insert (a,d) w m
    in vs#fold_array add_row_to_map IntPairMap.empty

  let stitch_mapped_query : Value.t list IntPairMap.t Shred.package -> Value.t =
      stitch (`Record [("1", `Int Shred.top); ("2", `Int 1)])


end

(* TODO: Unify / subsume Query.Sql *)
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
      [@@deriving show]

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
            let tables = mapstrcat "," (fun (t, x) -> db#quote_field t ^ " as " ^ (string_of_table_var x)) tables in
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
        | `With (_, q, z, q') ->
            let q' =
              (* Inline the query *)
	      match q' with
	      | `Select (fields, tables, condition, os) ->
                  `Select (fields, ("(" ^ sq q ^ ")", z) :: tables, condition, os)
	      | _ -> assert false
	    in
	    sq q'


  and string_of_base db one_table b =
    let sb = string_of_base db one_table in
      match b with
        | `Case (c, t, e) ->
            "case when " ^ sb c ^ " then " ^sb t ^ " else "^ sb e ^ " end"
        | `Constant c -> Constant.string_of_constant c
        | `Project (_var, _label as p) -> string_of_projection db one_table p
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

  let prepare_clauses : Q.t -> Q.t list =
    function
      | `Concat vs -> vs
      | v -> [v]

  type index = (Var.var * string) list


  let gens_index gs  =
    let all_fields t =
      let field_types = Q.table_field_types t in
      Q.labels_of_field_types field_types
    in
(* Use keys if available *)
    let key_fields t =
      match t with
	(_, _, (ks::_), _) -> StringSet.from_list ks
      |	_ -> all_fields t
    in
    let table_index get_fields (x, source) =
      let t = match source with `Table t -> t | _ -> assert false in
      let labels = get_fields t in
        List.rev
          (StringSet.fold
             (fun name ps -> (x, name) :: ps)
             labels
             [])
    in
    if Settings.get_value Basicsettings.use_keys_in_shredding
    then concat_map (table_index key_fields) gs
    else concat_map (table_index all_fields) gs

  let outer_index gs_out = gens_index gs_out
  let inner_index z gs_in =
    (* it's just a dynamic index! *)
    (z, "2") :: gens_index gs_in

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
  and clause : Value.database -> index -> bool -> Q.t -> query = fun db index unit_query v ->
(*    Debug.print ("clause: "^string_of_t v); *)
    match v with
      | `Concat _ -> assert false
      | `For (_, [], _, body) ->
          clause db index unit_query body
      | `For (_, (x, `Table (_db, table, _keys, _row))::gs, os, body) ->
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
      | `Table (_db, table, _keys, (fields, _, _)) ->
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
        (* We currently detect this earlier, so the unit_query stuff here
           is redundant. *)
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
  and base : Value.database -> index -> Q.t -> base = fun db index ->
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
                    `Constant (`String (Regex.string_of_regex (Linksregex.Regex.ofLinks (Q.value_of_expression r))))
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
      | e ->
        Debug.print ("Not a base expression: " ^ Q.show e);
        assert false

  (* convert a regexp to a like if possible *)
  and likeify v =
    let quote = Str.global_replace (Str.regexp_string "%") "\\%" in
      match v with
        | `Variant ("Repeat", pair) ->
            begin
              match Q.unbox_pair pair with
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
              seq (Q.unbox_list rs)
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

  (* FIXME:

     either deal with the range argument properly or get rid of it
  *)
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


end


let compile_shredded : Value.env -> (int * int) option * Ir.computation
                       -> (Value.database * (string * Shred.flat_type) Shred.package) option =
  fun env (range, e) ->
    let v = Q.Eval.eval env e in
      match Q.used_database v with
        | None    -> None
        | Some db ->
          let t = Q.type_of_expression v in
          let p = ShreddedSql.unordered_query_package db range t v in
            Some (db, p)

