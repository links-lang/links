(*****************************************************************************
 ** evalMixingQuery.ml - compilation of DB queries mixing set and bag       **
 **                      semantics to SQL                                   **
 **                                                                         **
 ** author: Wilmer Ricciotti                                                **
 *****************************************************************************)

open Utility
open CommonTypes

module QL = QueryLang
module E = MixingQuery.Eval
module C = Constant
module S = Sql


exception EvalMixingUnimplemented of string

let error msg : 'a = raise (EvalMixingUnimplemented msg)
let eval_error fmt : 'r =
    Printf.ksprintf error fmt

let mapstrcat sep f l = l |> List.map f |> String.concat sep

let dummy_sql_empty_query =
  (S.All,S.Fields [(S.Constant (Constant.Int 42), "@unit@")], [], S.Constant (Constant.Bool false), [], [])

let dependency_of_contains_free = function true -> S.Lateral | _ -> S.Standard

(* convert an NRC-style query into an SQL-style query *)
let rec sql_of_query is_set = function
| QL.Concat ds -> S.Union (is_set, List.map (disjunct is_set) ds, 0)
| q -> disjunct is_set q

and disjunct is_set = function
| QL.Prom p -> sql_of_query S.Distinct p
| QL.Singleton _ as j -> S.Select (body is_set [] [] j)
| QL.For (_, gs, os, j) -> S.Select (body is_set gs os j)
| QL.AggBy (ar, q) -> aggregator ar q
| _arg ->
   Debug.print ("error in EvalMixingQuery.disjunct: unexpected arg = " ^ QL.show _arg);
   failwith "disjunct"

and aggregator ar q =
  let aggr = function
  | QL.Primitive "Sum" -> "sum"
  | QL.Primitive "SumF" -> "sum"
  | QL.Primitive "Avg" -> "avg"
  | QL.Primitive "AvgF" -> "avg"
  | QL.Primitive "Min" -> "min"
  | QL.Primitive "Max" -> "max"
  | QL.Primitive "MinF" -> "min"
  | QL.Primitive "MaxF" -> "max"
  | QL.Primitive "length" -> "count"
  | _ -> assert false
  in
  let z = Var.fresh_raw_var () in
  let tyk, _tyv = q |> QL.type_of_expression |> Types.unwrap_map_type in
  let fsk, _, _ = tyk |> Types.extract_row |> Types.extract_row_parts in
  let fields_k = fsk |> StringMap.to_alist |> List.map (fun (f,_) -> S.Project (z, "1@" ^ f), "1@" ^ f) in
  let fields_v = ar |> StringMap.to_alist |> List.map (fun (f_out, (aggfun, f_in)) ->
    S.Apply (aggr aggfun, [S.Project (z, "2@" ^ f_in)]), "2@" ^ f_out)
  in
  let fields = fields_k @ fields_v in
  let gbys = List.map (fun (_,f) -> S.Project (z, f)) fields_k in
  S.Select (S.All, S.Fields fields, [S.Subquery (S.Standard, sql_of_query S.All q, z)], S.Constant (Constant.Bool true), gbys, [])

and generator locvars = function
| (QL.Entries, v, QL.Prom p) -> (S.Subquery (dependency_of_contains_free (E.contains_free locvars p), sql_of_query S.Distinct p, v))
| (QL.Entries, v, QL.Table Value.Table.{ name; _}) -> (S.TableRef (name, v))
| (QL.Entries, v, QL.Dedup (QL.Table Value.Table.{ name; _ })) ->
    S.Subquery (S.Standard, S.Select (S.Distinct, S.Star, [S.TableRef (name, v)], S.Constant (Constant.Bool true), [], []), v)
| (QL.Keys, v, QL.GroupBy ((x, QL.Record gc), QL.Table Value.Table.{ name; _}))
| (QL.Keys, v, QL.GroupBy ((x, QL.Record gc), QL.Dedup (QL.Table Value.Table.{ name; _}))) ->
    let fields = List.map (fun (f,e) -> (base_exp e, f)) (StringMap.to_alist gc) in
    S.Subquery (dependency_of_contains_free (E.contains_free locvars (QL.Record gc)),
      S.Select (S.Distinct, S.Fields fields, [S.TableRef (name, x)], S.Constant (Constant.Bool true), [], []), v)
| (QL.Keys, v, q) ->
    let z = Var.fresh_raw_var () in
    let tyk, _ = q |> QL.type_of_expression |> Types.unwrap_map_type in
    let fsk, _, _ = tyk |> Types.extract_row |> Types.extract_row_parts in
    let fields =
      fsk
      |> StringMap.to_alist
      |> List.map (fun (f,_) -> S.Project (z, "1@" ^ f), f)
    in
    S.Subquery (dependency_of_contains_free (E.contains_free locvars q),
      S.Select (S.Distinct,
        S.Fields fields,
        [S.Subquery (S.Standard, sql_of_query S.All q, z)],
        S.Constant (Constant.Bool true),
        [], []), v)
| (_genkind, _, _arg) -> Debug.print ("error in EvalMixingQuery.disjunct: unexpected arg = " ^ QL.show _arg); failwith "generator"

and body is_set gs os j =
    let selquery body where =
        let froms =
            gs
            |> List.fold_left (fun (locvars,acc) (_genkind, v,_q as g) -> (v::locvars, generator locvars g::acc)) ([],[])
            |> snd
            |> List.rev
        in
        let os = List.map base_exp os in
        (is_set, S.Fields body, froms, where, [], os)
    in
    match j with
    | QL.Concat [] -> dummy_sql_empty_query
    | QL.Singleton (QL.Record fields) ->
        selquery
        <| List.map (fun (f,x) -> (base_exp x, f)) (StringMap.to_alist fields)
        <| Sql.Constant (Constant.Bool true)
    | QL.Singleton (QL.MapEntry (QL.Record keys, QL.Record values)) ->
        selquery
        <| List.map (fun (f,x) -> (base_exp x, "1@" ^ f)) (StringMap.to_alist keys)
           @ List.map (fun (f,x) -> (base_exp x, "2@" ^ f)) (StringMap.to_alist values)
        <| Sql.Constant (Constant.Bool true)
    | QL.If (c, QL.Singleton (QL.Record fields), QL.Concat []) ->
        selquery
        <| List.map (fun (f,x) -> (base_exp x, f)) (StringMap.to_alist fields)
        <| base_exp c
    | QL.If (c, QL.Singleton (QL.MapEntry (QL.Record keys, QL.Record values)), QL.Concat []) ->
        selquery
        <| List.map (fun (f,x) -> (base_exp x, "1@" ^ f)) (StringMap.to_alist keys)
           @ List.map (fun (f,x) -> (base_exp x, "2@" ^ f)) (StringMap.to_alist values)
        <| base_exp c
    | _ -> Debug.print ("error in EvalMixingQuery.body: unexpected j = " ^ QL.show j); failwith "body"

and base_exp = function
(* XXX: Project expects a (numbered) var, but we have a table name
   so I'll make an act of faith and believe that we never project from tables, but only from variables *)
| QL.Project (QL.Table _, _) as q ->
    Debug.print ("error in EvalMixingQuery.base_exp: unexpected Project on Table: " ^ QL.show q); failwith "base_exp"
| QL.Project (QL.Var (n,_), l) -> S.Project (n,l)
| QL.If (c, t, e) -> S.Case (base_exp c, base_exp t, base_exp e)
| QL.Apply (QL.Primitive "tilde", [s; r]) ->
    begin
    match QueryLang.likeify r with
        | Some r ->
        S.Apply ("LIKE", [base_exp s; base_exp r])
        | None ->
        let r =
                (* HACK:

                    this only works if the regexp doesn't include any variables bound by the query
                *)
                S.Constant (Constant.String (Regex.string_of_regex (Linksregex.Regex.ofLinks (QL.value_of_expression r))))
            in
                Sql.Apply ("RLIKE", [base_exp s; r])
    end
| QL.Apply (QL.Primitive "Empty", [v]) -> S.Empty (sql_of_query S.All v)
(* length takes as input a collection of records so it cannot be converted to S.Aggr *)
| QL.Apply (QL.Primitive "length", [v]) -> S.Length (sql_of_query S.All v)
| QL.Apply (QL.Primitive "Sum", [v]) -> S.Aggr ("sum", sql_of_query S.All v)
| QL.Apply (QL.Primitive "SumF", [v]) -> S.Aggr ("sum", sql_of_query S.All v)
| QL.Apply (QL.Primitive "Avg", [v]) -> S.Aggr ("avg", sql_of_query S.All v)
| QL.Apply (QL.Primitive "AvgF", [v]) -> S.Aggr ("avg", sql_of_query S.All v)
| QL.Apply (QL.Primitive "Min", [v]) -> S.Aggr ("min", sql_of_query S.All v)
| QL.Apply (QL.Primitive "MinF", [v]) -> S.Aggr ("min", sql_of_query S.All v)
| QL.Apply (QL.Primitive "Max", [v]) -> S.Aggr ("max", sql_of_query S.All v)
| QL.Apply (QL.Primitive "MaxF", [v]) -> S.Aggr ("max", sql_of_query S.All v)
| QL.Apply (QL.Primitive f, vs) -> S.Apply (f, List.map base_exp vs)
| QL.Constant c -> S.Constant c
| e ->
    Debug.print ("Not a base expression: " ^ (QL.show e) ^ "\n");
    failwith "base_exp"

(* external call will start with a bag query *)
let sql_of_query = sql_of_query S.All

let compile_mixing : delateralize:QueryPolicy.t -> Value.env -> (int * int) option * Ir.computation -> (Value.database * Sql.query * Types.datatype * (Value.t -> Value.t)) option =
  fun ~delateralize env (range, e) ->
    (* Debug.print ("env: "^Value.show_env env);
    Debug.print ("e: "^Ir.show_computation e); *)
    if range != None then eval_error "Range is not (yet) supported by the new mixing normaliser";
    let evaluator =
        if delateralize = QueryPolicy.Delat
            then Delateralize.eval QueryPolicy.Mixing
            else MixingQuery.Eval.eval QueryPolicy.Mixing
    in
    let v = evaluator env e in
      (* Debug.print ("v: "^ QL.show v); *)
      match QL.used_database v with
        | None -> None
        | Some db ->
    let strip_presence = function Types.Present t -> t | _ -> assert false in
    let v_flat = QL.FlattenRecords.flatten_query v in
    (*
    Debug.print ("Generated NRC query: " ^ QL.show v);
    Debug.print ("Flattened NRC query: " ^ QL.show v_flat);
    *)
    let readback = QL.FlattenRecords.unflatten_query (QL.type_of_expression v) in
    (* the calling code expects the item type, not the list type *)
    let t_flat = Types.unwrap_list_type (QL.type_of_expression v_flat) in
    let t_flat =
      try
        let tyk, tyv = Types.unwrap_mapentry_type t_flat in
        let rowk, _, _ = tyk |> Types.extract_row |> Types.extract_row_parts in
        let rowv, _, _ = tyv |> Types.extract_row |> Types.extract_row_parts in
        let row = StringMap.fold
          <| (fun k v acc -> StringMap.add ("1@" ^ k) (strip_presence v) acc)
          <| rowk
          <| StringMap.empty
        in
        let row = StringMap.fold
          <| (fun k v acc -> StringMap.add ("2@" ^ k) (strip_presence v) acc)
          <| rowv
          <| row
        in
        Types.make_record_type row
      with _ -> t_flat
    in
    let q = sql_of_query v_flat in
    let _range = None in
      Debug.print ("Generated SQL query: "^(db#string_of_query ~range:_range q));
      Some (db, q, t_flat, readback)
