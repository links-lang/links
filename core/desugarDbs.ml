open SugarConstructors.Make

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


let insert_rows      = "InsertRows"
let insert_returning = "InsertReturning"

class desugar_dbs env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method! phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | `DBInsert (table, _labels, rows, returning) ->
      (* TODO: work out how to type this properly *)
        let eff = o#lookup_effects in
        let o, table, table_type = o#phrase table in
        let read_type   = TypeUtils.table_read_type   table_type in
        let write_type  = TypeUtils.table_write_type  table_type in
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
        let tyvars = [`Type read_type; `Type write_type; `Type needed_type;
                      `Type value_type; `Row eff] in
        let o, (e : Sugartypes.phrasenode) =
          match returning with
            | None ->
                (o, fn_appl_node insert_rows tyvars [table; rows])
            | Some field ->
                let o, field, _ = o#phrase field in
                (o, fn_appl_node insert_returning tyvars [table; rows; field])
        in (o, e, Types.unit_type)
    | e -> super#phrasenode e
end

let desugar_dbs env = ((new desugar_dbs env) : desugar_dbs :> TransformSugar.transform)

let has_no_dbs =
object
  inherit SugarTraversals.predicate as super

  val has_no_dbs = true
  method satisfied = has_no_dbs

  method! phrasenode = function
    | `DBInsert _ -> {< has_no_dbs = false >}
    | e -> super#phrasenode e
end
