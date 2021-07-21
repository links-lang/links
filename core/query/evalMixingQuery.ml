(*****************************************************************************
 ** simpleSqlGen.ml - Simple Sql syntax generator                           **
 **                                                                         **
 ** author: Wilmer Ricciotti                                                **
 *****************************************************************************)

open Utility
open CommonTypes

module Q = MixingQuery.Lang
module E = MixingQuery.Eval
module C = Constant
module S = Sql


exception EvalMixingUnimplemented of string

let error msg : 'a = raise (EvalMixingUnimplemented msg)
let eval_error fmt : 'r =
    Printf.ksprintf error fmt

let mapstrcat sep f l = l |> List.map f |> String.concat sep

let dummy_sql_empty_query =
    (S.All,S.Fields [(S.Constant (Constant.Int 42), "@unit@")], [], S.Constant (Constant.Bool false), [])

(* convert an NRC-style query into an SQL-style query *)
let rec sql_of_query is_set = function
| Q.Concat ds -> S.Union (is_set, List.map (disjunct is_set) ds, 0)
| q -> disjunct is_set q

and disjunct is_set = function
| Q.Prom p -> sql_of_query S.Distinct p
| Q.Singleton _ as j -> S.Select (body is_set [] [] j)
| Q.For (_, gs, os, j) -> S.Select (body is_set gs os j)
| _arg -> Debug.print ("error in SimpleSqlGen.disjunct: unexpected arg = " ^ Q.show _arg); failwith "disjunct"

and generator locvars = function
| (v, Q.Prom p) -> (S.Subquery (E.contains_free locvars p, sql_of_query S.Distinct p, v))
| (v, Q.Table (_, tname, _, _)) -> (S.TableRef (tname, v))
| (v, Q.Dedup (Q.Table (_, tname, _, _))) ->
    S.Subquery (false, S.Select (S.Distinct, S.Star, [S.TableRef (tname, v)], S.Constant (Constant.Bool true), []), v)
| (_, _arg) -> Debug.print ("error in SimpleSqlGen.disjunct: unexpected arg = " ^ Q.show _arg); failwith "generator"

and body is_set gs os j =
    let selquery body where =
        let froms =
            gs
            |> List.fold_left (fun (locvars,acc) (v,_q as g) -> (v::locvars, generator locvars g::acc)) ([],[])
            |> snd
            |> List.rev
        in
        let os = List.map base_exp os in
        (is_set, S.Fields body, froms, where, os)
    in
    match j with
    | Q.Concat [] -> dummy_sql_empty_query
    | Q.Singleton (Q.Record fields) ->
        selquery
        <| List.map (fun (f,x) -> (base_exp x, f)) (StringMap.to_alist fields)
        <| Sql.Constant (Constant.Bool true)
    | Q.If (c, Q.Singleton (Q.Record fields), Q.Concat []) ->
        selquery
        <| List.map (fun (f,x) -> (base_exp x, f)) (StringMap.to_alist fields)
        <| base_exp c
    | _ -> Debug.print ("error in SimpleSqlGen.body: unexpected j = " ^ Q.show j); failwith "body"

and base_exp = function
(* XXX: Project expects a (numbered) var, but we have a table name
   so I'll make an act of faith and believe that we never project from tables, but only from variables *)
(* | Q.Project (Q.Table (_, n, _, _), l) -> S.Project (n,l) *)
| Q.Project (Q.Var (n,_), l) -> S.Project (n,l)
| Q.If (c, t, e) -> S.Case (base_exp c, base_exp t, base_exp e)
| Q.Apply (Q.Primitive "tilde", [s; r]) ->
    begin
    match MixingQuery.likeify r with
        | Some r ->
        S.Apply ("LIKE", [base_exp s; base_exp r])
        | None ->
        let r =
                (* HACK:

                    this only works if the regexp doesn't include any variables bound by the query
                *)
                S.Constant (Constant.String (Regex.string_of_regex (Linksregex.Regex.ofLinks (Q.value_of_expression r))))
            in
                Sql.Apply ("RLIKE", [base_exp s; r])
    end
| Q.Apply (Q.Primitive "Empty", [v]) -> S.Empty (sql_of_query S.All v)
| Q.Apply (Q.Primitive "length", [v]) -> S.Length (sql_of_query S.All v)
| Q.Apply (Q.Primitive f, vs) -> S.Apply (f, List.map base_exp vs)
| Q.Constant c -> S.Constant c
(* WR: we don't support indices in this simple Sql generator *)
(* | Q.Primitive "index" -> ??? *)
| e ->
    Debug.print ("Not a base expression: " ^ (Q.show e) ^ "\n");
    failwith "base_exp"

(* external call will start with a bag query *)
let sql_of_query = sql_of_query S.All

let compile_mixing : delateralize:QueryPolicy.t -> Value.env -> (int * int) option * Ir.computation -> (Value.database * Sql.query * Types.datatype) option =
  fun ~delateralize env (range, e) ->
    (* Debug.print ("env: "^Value.show_env env);
    Debug.print ("e: "^Ir.show_computation e); *)
    (* XXX: I don't see how the evaluation here is different depending on the policy *)
    if range != None then eval_error "Range is not (yet) supported by the new mixing normaliser";
    let evaluator =
        (* FIXME *)
        if delateralize = QueryPolicy.Delat
            then Delateralize.eval QueryPolicy.Flat
            else MixingQuery.Eval.eval QueryPolicy.Flat
    in
    let v = evaluator env e in
      (* Debug.print ("v: "^ Q.show v); *)
      match MixingQuery.used_database v with
        | None -> None
        | Some db ->
            let t = Types.unwrap_list_type (MixingQuery.type_of_expression v) in
            (* Debug.print ("Generated NRC query: " ^ Q.show v ); *)
            let q = sql_of_query v in
            let _range = None in
              (* Debug.print ("Generated SQL query: "^(Sql.string_of_query db _range q)); *)
              Some (db, q, t)
