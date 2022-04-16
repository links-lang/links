open Utility
open CommonTypes
module Q = Query
module QL = QueryLang

(* generate a unique tag for each comprehension in
   a normalised query
*)

let tag_query : QL.t -> QL.t =
  fun e ->
    let r = ref 1 in
    let next () =
      let v = !r in
        r := v+1;
        v in
    let rec tag =
    let open QL in
      function
        | For (_, gs, os, body) ->
          For (Some (next()), gs, os, tag body)
        | If (c, t, e) ->
          If (tag c, tag t, tag e)
        | Table t -> Table t
        | Singleton e -> Singleton (tag e)
        | Concat es ->
          Concat (List.map tag es)
        | Dedup t -> Dedup (tag t)
        | Prom t -> Prom (tag t)
        | Record fields -> Record (StringMap.map tag fields)
        | Project (e, l) -> Project (tag e, l)
        | Erase (e, fields) -> Erase (tag e, fields)
        | Variant (l, e) -> Variant (l, tag e)
        | XML v -> XML v
        | Apply (u, vs) -> Apply (tag u, List.map tag vs)
        | Closure ((xs, body), env) -> Closure ((xs, body), env)
        | Case (u, cs, d) -> Case (tag u, StringMap.map (fun (x,y) -> (x, tag y)) cs, opt_app (fun (x,y) -> Some (x, tag y)) None d)
        | Primitive p -> Primitive p
        | Var (x, t) -> Var (x, t)
        | Constant c -> Constant c
        | Database db -> Database db
    in
      tag e

let tuple xs = QL.Record (snd
                          (List.fold_left
                             (fun (i, fields) x ->
                               (i+1, StringMap.add (string_of_int i) x fields))
                             (1, StringMap.empty)
                             xs))
let pair x y = tuple [x; y]

module Shred =
struct
  type nested_type =
    [ `Primitive of Primitive.t
    | `Record of nested_type StringMap.t
    | `List of nested_type ]
      [@@deriving show]

  type 'a shredded = [`Primitive of 'a | `Record of ('a shredded) StringMap.t]
      [@@deriving show]
  type shredded_type = Primitive.t shredded
      [@@deriving show]
  type shredded_value = Value.t shredded
      [@@deriving show]

  type flat_type =
    [ `Primitive of Primitive.t
    | `Record of Primitive.t StringMap.t ]
      [@@deriving show]

  type 'a package =
    [ `Primitive of Primitive.t
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
    let open Types in
    match TypeUtils.concrete_type t with
    | Types.Primitive t -> `Primitive t
    | Types.Record row ->
        let (fields, _, _) = TypeUtils.extract_row_parts row in
        `Record (StringMap.map
          (function
             | Present t -> nested_type_of_type t
             | _ -> assert false) fields)
    | Types.Application (l, [(primary_kind, t)]) when l = Types.list ->
       assert (primary_kind = PrimaryKind.Type);
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
      | QL.If (c, t, QL.Concat []) ->
        split_conditions (fun body -> f (QL.If (c, body, QL.Concat []))) t
      | QL.Singleton body -> f, body
      | _ -> assert false

  let static_in = QL.Primitive "in"
  let static_out = QL.Primitive "out"

  let dyn_index a = QL.Constant (Constant.Int a)

  let in_index a = pair (dyn_index a) static_in
  let out_index a = pair (dyn_index a) static_out

  (* inner shredding function *)
  let rec shinner a =
    let open QL in
    function
      | Project (v,l) -> Project (v,l)
      | Apply (Primitive "Empty", [e]) -> Apply (Primitive "Empty", [shred_outer e []])
      | Apply (Primitive "length", [e]) -> Apply (Primitive "length", [shred_outer e []])
      | Apply (f, vs) -> Apply (f, List.map (shinner a) vs)
      | Record fields ->
        Record (StringMap.map (shinner a) fields)
      | e when QL.is_list e ->
        in_index a
      | e -> e

  (* outer shredding function *)
  and shouter a p : QL.t -> QL.t list =
    let open QL in
    function
      | Concat cs ->
        concat_map (shouter a p) cs
      | Record fields ->
        begin
          match p with
            | (`Record l :: p) ->
              shouter a p (StringMap.find l fields)
            | _ -> assert false
        end
      | For (Some b, gs, os, body) ->
        let f, body = split_conditions (fun x -> x) body in
          begin
            match p with
              | [] ->
                [For (Some b, gs, os, f (Singleton (pair (out_index a) (shinner b body))))]
              | (`List :: p) ->
                List.map
                  (fun c -> For (Some b, gs, os, f c))
                  (shouter b p body)
              | _ -> assert false
          end
      | e ->
         Debug.print ("Can't apply shouter to: " ^ QL.show e);
         assert false

  and shred_outer q p = QL.Concat (shouter top p q)

  let shred_query : QL.t -> nested_type -> QL.t package =
    fun q t -> package (shred_outer q) t


  let rec shred_inner_type : nested_type -> shredded_type =
    function
      | `Primitive p   -> `Primitive p
      | `Record fields -> `Record (StringMap.map shred_inner_type fields)
      | `List _        ->
        `Record
          (StringMap.add "1" (`Primitive Primitive.Int)
            (StringMap.add "2" (`Primitive Primitive.Int) StringMap.empty))

  let rec shred_outer_type : nested_type -> path -> shredded_type =
    fun t p ->
      match t, p with
        | `List t, [] ->
          `Record
            (StringMap.add "1"
               (`Record
                   (StringMap.add "1" (`Primitive Primitive.Int)
                      (StringMap.add "2" (`Primitive Primitive.Int)
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
  type gen = Var.var * QL.t

  let rec query : gen list -> QL.t list -> QL.t -> QL.t -> QL.t list =
    fun gs os cond ->
      let open QL in
      function
        | Singleton r ->
          [For (None, gs, os, Q.reduce_where_then (cond, Singleton (inner r)))]
        | Concat vs ->
          concat_map (query gs os cond) vs
        | If (cond', v, Concat []) ->
          query gs os (Q.reduce_and (cond, cond')) v
        | For (_, gs', os', body) ->
          query (gs @ gs') (os @ os') cond body
        | _ -> assert false

  and inner =
    let open QL in
    function
      | If (c, t, e) ->
        If (inner c, inner t, inner e)
      | Record fields -> Record (StringMap.map inner fields)
      | Project (e, l) -> Project (inner e, l)
      | Apply (f, es) -> Apply (f, List.map inner es)
      | Primitive p -> Primitive p
      | Var (x, t) -> Var (x, t)
      | Constant c -> Constant c
      | e when QL.is_list e ->
        Concat (query [] [] (Constant (Constant.Bool true)) e)
      | _ -> assert false

  let query : QL.t -> QL.t list =
    query [] [] (QL.Constant (Constant.Bool true))
end


module LetInsertion =
struct
  type let_clause = Var.var * QL.t * Var.var * QL.t
      [@@deriving show]
  type query = let_clause list
      [@@deriving show]

  type cond = QL.t option
  type gen = Var.var * QL.t

  let where c e =
    match c with
      | None -> e
      | Some c ->
        QL.If (c, e, QL.Concat [])

  let index = QL.Primitive "index"

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

  let rec gens : QL.t -> (gen list) list =
    function
      | QL.Singleton _           -> []
      | QL.If (_, t, QL.Concat []) -> gens t
      | QL.For (_, gs, _, e)     -> gs :: gens e
      | _                      -> assert false

  let rec orders : QL.t -> (QL.t list) list =
    function
      | QL.Singleton _           -> []
      | QL.If (_, t, QL.Concat []) -> orders t
      | QL.For (_, _, os, e)     -> os :: orders e
      | _                      -> assert false

  let rec conds : QL.t -> cond list =
    function
      | QL.Singleton _                           -> []
      | QL.For (_, _, _, QL.If (c, t, QL.Concat [])) -> Some c :: conds t
      | QL.For (_, _, _, e)                      -> None :: conds e
      | _                                      -> assert false

  let rec body : QL.t -> QL.t =
    function
      | QL.Singleton e           -> e
      | QL.If (_, t, QL.Concat []) -> body t
      | QL.For (_, _, _, e)      -> body e
      | _                      -> assert false


  let fields_of_list : string list -> StringSet.t =
    List.fold_left
      (fun fields l ->
        StringSet.add l fields)
      StringSet.empty

  (* dynamic index type *)
  let index_type = Types.int_type

  let rec lins_inner (z, z_fields) ys : QL.t -> QL.t =
    let open QL in
    let li x = lins_inner (z, z_fields) ys x in
    let liq = lins_inner_query (z, z_fields) ys in
    function
      (* Need to make sure this happens on the regex, too. *)
      | Project (Var (x, fields), l) ->
        begin
          match position_of x ys with
            | None -> Project (Var (x, fields), l)
            | Some i ->
              (* z.1.i.l *)
              Project
                (Project
                    (Project (Var (z, z_fields), "1"), string_of_int i), l)
        end
      | Apply (Primitive "Empty", [e]) -> Apply (Primitive "Empty", [liq e])
      | Apply (Primitive "length", [e]) -> Apply (Primitive "length", [liq e])
      | Apply (Primitive "tilde", [s; r]) as e ->
          Debug.print ("Applying lins_inner to tilde expression: " ^ QL.show e);
          Apply (Primitive "tilde", [li s; li r])
      | Apply (Primitive f, es) ->
        Apply (Primitive f, List.map li es)
      | Record fields ->
        Record (StringMap.map li fields)
      | Primitive "out" ->
        (* z.2 *)
        Project (Var (z, z_fields), "2")
      | Primitive "in"  -> Primitive "index"
      | Constant c      -> Constant c
      (* Regex variants. *)
      | Variant ("Simply", x) ->
          Variant ("Simply", li x)
      | Variant ("Seq", Singleton r) ->
          Variant ("Seq", Singleton (li r))
      | Variant ("Seq", Concat rs) ->
          Variant ("Seq",
            Concat (List.map (
              function | Singleton x -> Singleton (li x) | _ -> assert false) rs))
      | Variant ("Quote", Variant ("Simply", v)) ->
          Variant ("Quote", Variant ("Simply", li v))
      (* Other regex variants which don't need to be traversed *)
      | Variant (s, x) when s = "Repeat" || s = "StartAnchor" || s = "EndAnchor" ->
          Variant (s, x)
      | e ->
        Debug.print ("Can't apply lins_inner to: " ^ QL.show e);
        assert false

  and lins_inner_query (z, z_fields) ys : QL.t -> QL.t =
    fun e ->
      let li = lins_inner (z, z_fields) ys in
      let liq = lins_inner_query (z, z_fields) ys in
        let open QL in
        match e with
          | Concat es -> Concat (List.map liq es)
          | For (tag, gs, os, body) ->
            For (tag, gs, List.map li os, liq body)
          | If (c, t, Concat []) -> If (li c, liq t, Concat [])
          (* OPTIMISATION:

             For Empty and length we don't care about what the body
             returns.
          *)
          | Singleton _ -> Singleton (Record StringMap.empty)
          | e ->
            Debug.print ("Can't apply lins_inner_query to: " ^ QL.show e);
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
            | Some c, Some c' -> Some (Q.reduce_and (c, c')))
        (init (conds c))
        None in

    let r_out =
      tuple (List.map
               (fun (x, source) ->
                 match source with
                   | QL.Table t ->
                     let tyx = Types.make_record_type (QL.table_field_types t) in
                     QL.eta_expand_var (x, tyx)
                   | _ -> assert false)
               gs_out) in
    let r_out_type =
      Types.make_tuple_type
        (List.map
           (fun (_, source) ->
             match source with
               | QL.Table Value.Table.{ row; _ } ->
                 Types.Record (Types.Row row)
               | _ -> assert false)
           gs_out) in

    let gs_in = last (gens c) in
    let x_in = last (conds c) in

    let os = List.concat (orders c) in
    let q = Var.fresh_raw_var () in
    let z = Var.fresh_raw_var () in
    let tyz =
      QL.recdty_field_types
        (Types.make_tuple_type
           [r_out_type; index_type])
      |> Types.make_record_type
    in
      (q, QL.For (None, gs_out, [], where x_out (QL.Singleton (pair r_out index))),
       z, QL.For (None, gs_in, os,
                where
                  (opt_map (lins_inner (z, tyz) ys) x_in)
                  (QL.Singleton (lins_inner (z, tyz) ys (body c)))))

  and lins_query : QL.t -> query =
    function
      | QL.Concat cs -> List.map lins cs
      | _          -> assert false

end


module FlattenRecords =
struct
  open Shred

  type let_clause = LetInsertion.let_clause
  type query = LetInsertion.query


  let rec flatten_inner : QL.t -> QL.t =
    let open QL in
    function
      | Constant c    -> Constant c
      | Primitive p   -> Primitive p
      | Apply (Primitive "Empty", [e]) -> Apply (Primitive "Empty", [flatten_inner_query e])
      | Apply (Primitive "length", [e]) -> Apply (Primitive "length", [flatten_inner_query e])
      | Apply (Primitive "tilde", [s; r]) as e ->
          Debug.print ("Applying flatten_inner to tilde expression: " ^ QL.show e);
          Apply (Primitive "tilde", [flatten_inner s; flatten_inner r])
      | Apply (Primitive f, es) -> Apply (Primitive f, List.map flatten_inner es)
      | If (c, t, e)  ->
        If (flatten_inner c, flatten_inner t, flatten_inner e)
      | Project (Var (x, t), l) -> Project (Var (x, t), l)
      | Project (Project (Project (Var (x, t), "1"), i), l) ->
        (* HACK: this keeps z annotated with its original unflattened type *)
        Project (Var (x, t), "1"^"@"^i^"@"^l)
      | Record fields ->
        (* concatenate labels of nested records *)
        Record
          (StringMap.fold
             (fun name body fields ->
               match flatten_inner body with
                 | Record inner_fields ->
                   StringMap.fold
                     (fun name' body fields ->
                       StringMap.add (name ^ "@" ^ name') body fields)
                     inner_fields
                     fields
                 | body ->
                   StringMap.add name body fields)
             fields
             StringMap.empty)
      | Variant ("Simply", x) ->
          Variant ("Simply", flatten_inner x)
      | Variant ("Seq", Singleton r) ->
          Variant ("Seq", Singleton (flatten_inner r))
      | Variant ("Seq", Concat rs) ->
          Variant ("Seq",
            Concat (List.map (
              function | Singleton x -> Singleton (flatten_inner x) | _ -> assert false) rs))
      | Variant ("Quote", Variant ("Simply", v)) ->
          Variant ("Quote", Variant ("Simply", flatten_inner v))
      (* Other regex variants which don't need to be traversed *)
      | Variant (s, x) when s = "Repeat" || s = "StartAnchor" || s = "EndAnchor" ->
          Variant (s, x)
      | e ->
        Debug.print ("Can't apply flatten_inner to: " ^ QL.show e);
        assert false

  and flatten_inner_query : QL.t -> QL.t = fun e -> flatten_comprehension e

  and flatten_comprehension : QL.t -> QL.t =
    let open QL in
    function
      | For (tag, gs, os, body) ->
        For (tag, gs, os, flatten_comprehension body)
      | If (c, e, Concat []) ->
        If (flatten_inner c, flatten_comprehension e, Concat [])
      | Singleton e ->
        let e' =
          (* lift base expressions to records *)
          match flatten_inner e with
            | Record fields -> Record fields
            | p -> Record (StringMap.add "@" p StringMap.empty)
        in
          Singleton e'
      (* HACK: not sure if Concat is supposed to appear here...
         but it can do inside "Empty" or "Length". *)
      | Concat es ->
        Concat (List.map flatten_comprehension es)
      | e ->
        Debug.print ("Can't apply flatten_comprehension to: " ^ QL.show e);
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
      |    `Record rcd -> `Record (List.map (fun (nm,t') ->
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
        | `Record rcd -> `Record (List.map (fun (n,t') -> (n,build t')) rcd)
        | `Primitive (ty,idx) ->
            Database.value_of_db_string (Array.get array idx) ty

    in build template

  let build_stitch_map (((vs:Value.dbvalue),rs),t) =
    let st = FlattenRecords.unflatten_type t in
    let tmpl = FlattenRecords.make_template st in
    let sql_map x = match List.assoc_opt x rs with
      | Some y -> y
      (* HACK HACK
         dummy empty queries don't have indices, so we can't find "1@1" or "1@2"
         in rs: we output a dummy column position (-1) instead *)
      | _ when x = "1@1" || x = "1@2" -> (Types.Primitive Primitive.Int, -1)
      | _ -> assert false
    in
    let idx_and_val = function
      | `Record fl ->
           let w_tmpl = try List.assoc "2" fl with NotFound _ -> `Record [] in
           (match List.assoc "1" fl with
            | `Record fl' -> (match List.assoc "1" fl', List.assoc "2" fl' with
              | `Primitive (_,a_idx), `Primitive (_,d_idx) -> (a_idx,d_idx,w_tmpl)
              | _ -> assert false)
            | _ -> assert false)
      | _ -> assert false
    in
    let tmpl' = FlattenRecords.template_map sql_map tmpl in
    let (a_idx,d_idx,w_tmpl) = idx_and_val tmpl' in
    let add_row_to_map row m =
      let w = build_unflattened_record_from_array w_tmpl row in
      let a = int_of_string(Array.get row a_idx) in
      let d = int_of_string(Array.get row d_idx) in
      insert (a,d) w m
    in vs#fold_array add_row_to_map IntPairMap.empty

  let stitch_mapped_query : Value.t list IntPairMap.t Shred.package -> Value.t =
      stitch (`Record [("1", `Int Shred.top); ("2", `Int 1)])


end


let unordered_query_package t v =
  let t = Shred.nested_type_of_type t in
  (* Debug.print ("v: "^string_of_t v); *)
  Sql.reset_dummy_counter ();
  let w = QL.Concat (Split.query v) in
    (* Debug.print ("w: "^string_of_t w); *)
  let tagged_w = tag_query w in
  let shredded_w = Shred.shred_query tagged_w t in
  let lins_w = Shred.pmap (LetInsertion.lins_query) shredded_w in
  let flat_w = Shred.pmap (FlattenRecords.flatten_query) lins_w in
  let query_package =
    Shred.pmap QL.sql_of_let_query flat_w in
  let shredded_t = Shred.shred_query_type t in
  let query_type_package =
    Shred.pmap (FlattenRecords.flatten_query_type) shredded_t in
  let typed_query_package =
    Shred.pzip query_package query_type_package in
  typed_query_package

let compile_shredded : Value.env -> Ir.computation
                       -> (Value.database * (Sql.query * Shred.flat_type) Shred.package) option =
  fun env e ->
    let v = Q.Eval.eval QueryPolicy.Nested env e in
      match QL.used_database v with
        | None    -> None
        | Some db ->
          let t = QL.type_of_expression v in
          let p = unordered_query_package t v in
            Some (db, p)

  let compile_temporal_join :
      Temporality.t ->
      Value.env ->
      Ir.computation ->
        (Value.database * (Sql.query * Shred.flat_type) Shred.package) =
  fun tmp env e ->
    let v =
      Q.Eval.eval QueryPolicy.Nested env e
        |> TemporalQuery.TemporalJoin.rewrite_temporal_join tmp in
      match QL.used_database v with
        | None    ->
            raise (Errors.runtime_error "Unable to compile temporal join (No DB found).")
        | Some db ->
          Debug.print ("Temporal Join Query: " ^ QL.show v ^ "\n");
          let t = QL.type_of_expression v in
          let p = unordered_query_package t v in
            (db, p)
