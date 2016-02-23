open Utility

class where_prov_rewriting env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  val dp = Sugartypes.dummy_position

  method! iterpatt : Sugartypes.iterpatt -> ('self_type * Sugartypes.iterpatt) = function
    | `Table (pattern, phrase) as _tbl ->
       let (o, pattern) = o#pattern pattern in
       let (o, phrase, _t) = o#phrase phrase in
       let phrase' : Sugartypes.phrase = (`Projection (phrase, "2"), dp) in
       let phrase' = `FnAppl (phrase', []), dp in
       let res = `List (pattern, phrase') in
       (* Debug.print ("Before: "^Sugartypes.Show_iterpatt.show _tbl^"\n after: "^Sugartypes.Show_iterpatt.show res); *)
       (* We don't actually *change* the type (apparently, everywhere, something...), so don't try to extract *)
       (* let t = TypeUtils.project_type "1" t in *)
       (o, res)
    | `List _ as i -> super#iterpatt i

  method! phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | `Data e as _dbg ->
       let a, e, c = o#phrase e in
       let e : Sugartypes.phrasenode = `Projection (e, "!data") in
       (o, e, c)
    | `Prov e as _dbg ->
       let a, e, c = o#phrase e in
       let e : Sugartypes.phrasenode = `Projection (e, "!prov") in
       (o, e, c)
    | `TableLit (tname, (dtype, Some (read_row, write_row, needed_row)), constraints, keys, db) as _dbg ->
       (* Debug.print ("TableLit: "^Sugartypes.Show_phrasenode.show _dbg); *)
       let (o, tname, _) = o#phrase tname in
       let (o, keys, _) = o#phrase keys in
       let (o, db, _) = o#phrase db in
       let (o, dtype) = o#sugar_datatype dtype in
       let (o, read_row) = o#datatype read_row in
       let (o, write_row) = o#datatype write_row in
       let (o, needed_row) = o#datatype needed_row in
       (* TODO only remove prov constraints from constraints, not all constraints *)
       let tablelit : Sugartypes.phrasenode =
         `TableLit (tname, (dtype, Some (read_row, write_row, needed_row)), [], keys, db) in
       let tablelit_type = `Table (read_row, write_row, needed_row) in

       (* FIXME Ask Sam how to introduce a new variable with a fresh name. *)
       let pattern : Sugartypes.pattern = (`Variable ("t", Some read_row, dp), dp) in

       (* Move this mess somewhere... *)
       let prov_rows : (Sugartypes.name * Sugartypes.fieldconstraint list) list -> (Sugartypes.phrase option) StringMap.t =
         fun l ->
         let res = ref StringMap.empty in
         List.iter (fun (name, constraints) ->
                    List.iter (function
                                | `Prov e -> if StringMap.mem name !res
                                             then failwith "Duplicate provenance declaration"
                                             else res := StringMap.add name e !res
                                | _ -> ())
                              constraints) l;
         !res in
       let prov_rows = prov_rows constraints in

       let prov_calc_record : Sugartypes.phrasenode = begin
           let (fsm, _, _) = TypeUtils.extract_row read_row in
           let fields : string list = StringMap.fold (fun k v a -> k :: a) fsm [] in
           let non_prov_e : string -> Sugartypes.phrase = fun name ->
             `Projection ((`Var "t", dp), name), dp in
           let prov_e : string -> Sugartypes.phrase -> Sugartypes.phrase = fun name e ->
             `RecordLit ([("!data", non_prov_e name);
                          (* TODO What if the prov function is polymorphic? Insert appropriate `TAppl? *)
                          ("!prov", (`FnAppl (e, [(`Var "t", dp)]), dp))], None), dp in
           let default_prov : string -> string -> Sugartypes.phrase = fun table column ->
             `RecordLit ([("!data", non_prov_e column);
                          ("!prov", (`TupleLit [`Constant (`String table), dp;
                                                `Constant (`String column), dp;
                                                `Projection ((`Var "t", dp), "oid"), dp], dp))], None), dp in
           let table_name = match tname with
             | `Constant (`String n), _ -> n | _ -> assert false in
           let record : (string * Sugartypes.phrase) list =
             List.map
               (fun name -> match StringMap.lookup name prov_rows with
                            | Some None -> name, default_prov table_name name
                            | Some (Some e) -> name, prov_e name e
                            | None -> name, non_prov_e name)
               fields in
           `RecordLit (record, None)
         end in

       (* TODO The second argument to `ListLit is the type of the list elements.
               I'm not sure whether this needs to be the one with Provs or the desugared type. *)
       let prov_calc_expr : Sugartypes.phrasenode = `ListLit ([prov_calc_record, dp], Some read_row) in
       let iter : Sugartypes.phrasenode = `Iteration ([`Table (pattern, (tablelit, dp))], (prov_calc_expr, dp), None, None) in

       let prov_row = TypeUtils.map_record_type (fun t n -> if StringMap.mem n prov_rows
                                                            then Types.make_prov_type t
                                                            else t)
                                                read_row in
       let prov_type = Types.make_list_type prov_row in

       let delayed_type = Types.make_pure_function_type Types.unit_type prov_type in
       (* Debug.print ("Delayed prov type: "^(Types.Show_datatype.show delayed_type)); *)

       (* TODO this is probably wrong. Should be, roughly
          fun () server {
            for (t <- tablelit) provCalcHere
          } : () -> [(table_row: table_row_type,
                      table_row_2: (data: table_row_2_type,
                                    prov: (String, String, Int)))]
        *)
       let delayed_prov : Sugartypes.phrasenode =
         `FunLit (Some [(Types.unit_type, Types.make_empty_closed_row ())],
                  `Unl,
                  ([[]], (`Block ([], (iter, dp)), dp)),
                  `Server) in
       (* Debug.print ("Delayed prov fun: "^(Sugartypes.Show_phrasenode.show delayed_prov)); *)
       
       let pair : Sugartypes.phrasenode = `TupleLit [(tablelit, dp); (delayed_prov, dp)] in
       let pair_type = Types.make_tuple_type [tablelit_type; delayed_type] in
       (* Debug.print ("TableLit desugared:\n"^Sugartypes.Show_phrasenode.show pair); *)
       (o, pair, pair_type)

    | `Iteration (gens, body, cond, orderby) as _iteration ->
       let (o, gens) = TransformSugar.listu o (fun o -> o#iterpatt) gens in
       let (o, body, t) = o#phrase body in
       let (o, cond, _) = TransformSugar.option o (fun o -> o#phrase) cond in
       let (o, orderby, _) = TransformSugar.option o (fun o -> o#phrase) orderby in
       let res = `Iteration (gens, body, cond, orderby) in
       (* Debug.print ("Before: "^Sugartypes.Show_phrasenode.show _iteration^"\n after: "^Sugartypes.Show_phrasenode.show res); *)
       (o, res, t)

    (* TODO What to do with where clauses? *)
    | `DBDelete (p, from, where) ->
       let (o, from, _) = o#phrase from in
       let (o, p) = o#pattern p in
       let (o, where, _) = TransformSugar.option o (fun o -> o#phrase) where in
       (o, `DBDelete (p, (`Projection (from, "1"), dp), where), Types.unit_type)

    | `DBInsert (into, labels, values, id) ->
       let (o, into, _) = o#phrase into in
       let (o, values, _) = o#phrase values in
       let (o, id, _) = TransformSugar.option o (fun o -> o#phrase) id in
       (o, `DBInsert ((`Projection (into, "1"), dp), labels, values, id), Types.unit_type)

    | `DBUpdate (p, from, where, set) ->
       let (o, from, _) = o#phrase from in
       let (o, p) = o#pattern p in
       let (o, where, _) = TransformSugar.option o (fun o -> o#phrase) where in
       let (o, set) =
         TransformSugar.listu o
                              (fun o (name, value) ->
                               let (o, value, _) = o#phrase value in (o, (name, value)))
                              set
       in
       (o, `DBUpdate (p, (`Projection (from, "1"), dp), where, set), Types.unit_type)

    | e -> super#phrasenode e
end

let where_prov_rewriting env = ((new where_prov_rewriting env) : where_prov_rewriting :> TransformSugar.transform)
