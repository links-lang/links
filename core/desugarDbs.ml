(*
  Desugaring database stuff
  -------------------------

***REMOVED***
    delete (p <-- e) where b
  -->
    let
      t = e
      rows = query {for (p as r <-- t) where b [r]}
    in
      DeleteRows (t, rows)

    update (p <-- e) where b set (l1=e1, ..., lk=ek)
  -->
    let
      t = e
      rows = query {for (p as r <-- t) where b [r]}
      row_pairs = for (p as r <-- rows) [(r, (l1=e1, ..., lk=ek))]
    in
      UpdateRows (t, row_pairs)
*************

TODO:

move insert into the IR

    insert table values rows
  -->
    InsertRows (table, rows)

    insert table values rows returning e
  -->
    InsertReturning (table, rows, field)

*)

module QualifiedName = Sugartypes.QualifiedName
let dp = Sugartypes.dummy_position

class desugar_dbs env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method! phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
(*     | `DBDelete (pattern, table, condition) -> *)
(*         let eff = o#lookup_effects in *)
(*         let o, table, table_type = o#phrase table in *)
(*         let read_type = TypeUtils.table_read_type table_type in *)
(*         let write_type = TypeUtils.table_write_type table_type in *)
(*         let needed_type = TypeUtils.table_needed_type table_type in *)
(*         let (tb, t) = *)
(*           let var = Utility.gensym ~prefix:"_db_" () in *)
(*             (var, Some table_type, dp), ((`Var var), dp) in *)
(*         let (rb, r) = *)
(*           let var = Utility.gensym ~prefix:"_db_" () in *)
(*             (var, Some read_type, dp), ((`Var var), dp) in *)

(*         let o, pattern = o#pattern pattern in *)
(*         let o, condition, _ = TransformSugar.option o (fun o -> o#phrase) condition in *)

(*         let rows : Sugartypes.phrase = *)
(*           let rows_type = Types.make_list_type read_type in *)
(*             `Query *)
(*               (None, *)
(*                (`Iteration ([`Table ((`As (rb, pattern), dp), t)], *)
(*                             (`ListLit ([r], Some read_type), dp), condition, None), dp), *)
(*                Some rows_type), dp in *)
(*         let e : Sugartypes.phrasenode = *)
(*           `Block *)
(*             ([`Val ([], ((`Variable tb), dp), table, `Unknown, None), dp], *)
(*              (`FnAppl *)
(*                 ((`TAppl ((`Var "DeleteRows", dp), [`Type read_type; `Type write_type; `Type needed_type; `Row eff]), dp), *)
(*                  [t; rows]), dp)) *)
(*         in *)
(*           o, e, Types.unit_type *)
(*     | `DBUpdate (pattern, table, condition, fields) -> *)
(*         let eff = o#lookup_effects in *)
(*         let o, table, table_type = o#phrase table in *)
(*         let read_type = TypeUtils.table_read_type table_type in *)
(*         let write_type = TypeUtils.table_write_type table_type in *)
(*         let needed_type = TypeUtils.table_needed_type table_type in *)
(*         let (tb, t) = *)
(*           let var = Utility.gensym ~prefix:"_db_" () in *)
(*             (var, Some table_type, dp), ((`Var var), dp) in *)
(*         let (rb, r) = *)
(*           let var = Utility.gensym ~prefix:"_db_" () in *)
(*             (var, Some read_type, dp), ((`Var var), dp) in *)

(*         let o, pattern = o#pattern pattern in *)
(*         let o, condition, _ = TransformSugar.option o (fun o -> o#phrase) condition in *)

(*         let o, fields, ts = *)
(*           TransformSugar.list o (fun o (name, e) -> *)
(*                                    let o, e, et = o#phrase e in *)
(*                                      o, (name, e), et) fields in *)
(*         let update_type = *)
(*           Types.make_record_type *)
(*             (List.fold_right2 *)
(*                (fun (name, _) t fields -> *)
(*                   StringMap.add name t fields) *)
(*                fields *)
(*                ts *)
(*                StringMap.empty) in *)

(*         let rows = *)
(*           `Query (None, *)
(*                   (`Iteration ([`Table ((`As (rb, pattern), dp), t)], *)
(*                                (`ListLit ([r], Some read_type), dp), condition, None), dp), *)
(*                   Some (Types.make_list_type (read_type))), dp in *)

(*         let pair_type = Types.make_tuple_type [read_type; update_type] in *)

(*         let body =  *)
(*           (`ListLit *)
(*              ([(`TupleLit *)
(*                   [r; *)
(*                    (`RecordLit (fields, None), dp)]), dp], Some pair_type)), dp in *)

(*         let row_pairs = *)
(*           `Iteration ([`List ((`As (rb, pattern), dp), rows)], body, None, None), dp in *)

(*         let e = *)
(*           `Block *)
(*             ([`Val ([], ((`Variable tb), dp), table, `Unknown, None), dp], *)
(*              (`FnAppl *)
(*                 ((`TAppl ((`Var "UpdateRows", dp), [`Type read_type; `Type write_type; `Type needed_type; `Row eff]), dp), *)
(*                  [t; row_pairs]), dp)) *)
(*         in *)
(*           o, e, Types.unit_type           *)
    | `DBInsert (table, _labels, rows, returning) ->
      (* TODO: work out how to type this properly *)
        let eff = o#lookup_effects in
        let o, table, table_type = o#phrase table in
        let read_type = TypeUtils.table_read_type table_type in
        let write_type = TypeUtils.table_write_type table_type in
        let needed_type = TypeUtils.table_needed_type table_type in

        (* HACK

           We need value_type as our type system is not expressive
           enough to give InsertRows a more accurate type. This isn't
           too much of a problem as InsertRows can only be generated
           from well-typed insert expressions. An alternative approach
           would be to maintain some kind of insert expression in the
           IR. *)
        let value_type = `Record (Types.make_empty_open_row (`Any, `Any)) in
        let o, rows, _ = o#phrase rows in
        let o, (e : Sugartypes.phrasenode) =
          match returning with
          | None ->
             let q = QualifiedName.of_name "InsertRows" in
             (o,
              `FnAppl
                ((`TAppl ((`Var q, dp), [`Type read_type; `Type write_type; `Type needed_type; `Type value_type; `Row eff]), dp),
                 [table; rows]))
          | Some field ->
             let q = QualifiedName.of_name "InsertReturning" in
             let o, field, _ = o#phrase field in
             (o,
              `FnAppl
                ((`TAppl ((`Var q, dp), [`Type read_type; `Type write_type; `Type needed_type; `Type value_type; `Row eff]), dp),
                 [table; rows; field]))
        in
          o, e, Types.unit_type
    | e -> super#phrasenode e
end

let desugar_dbs env = ((new desugar_dbs env) : desugar_dbs :> TransformSugar.transform)

let has_no_dbs =
object
  inherit SugarTraversals.predicate as super

  val has_no_dbs = true
  method satisfied = has_no_dbs

  method! phrasenode = function
(*     | `DBDelete _ -> {< has_no_dbs = false >} *)
(*     | `DBUpdate _ -> {< has_no_dbs = false >} *)
    | `DBInsert _ -> {< has_no_dbs = false >}
    | e -> super#phrasenode e
end
