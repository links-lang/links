open Utility

class where_prov_rewriting env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  val dp = Sugartypes.dummy_position

  method! iterpatt : Sugartypes.iterpatt -> ('self_type * Sugartypes.iterpatt) = function
    | `Table (pattern, phrase) ->
       let (o, pattern) = o#pattern pattern in
       let (o, phrase, _t) = o#phrase phrase in
       let phrase' : Sugartypes.phrase = (`Projection (phrase, "2"), dp) in
       let phrase' = `FnAppl (phrase', []), dp in
       Debug.print ("Before: "^Sugartypes.Show_phrase.show phrase^"\n after: "^Sugartypes.Show_phrase.show phrase'^"\n type: "^Types.Show_datatype.show _t);
       (* We don't actually *change* the type (apparently, everywhere, something...), so don't try to extract *)
       (* let t = TypeUtils.project_type "1" t in *)
       (o, `List (pattern, phrase'))
    | `List _ as i -> super#iterpatt i

  method! phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | `FunLit stuff ->
       Debug.print ("Functions look like this:"^(Sugartypes.Show_phrasenode.show (`FunLit stuff)));
       super#phrasenode (`FunLit stuff)
    | `TableLit (name, (dtype, Some (read_row, write_row, needed_row)), constraints, keys, db) as _dbg ->
       Debug.print ("TableLit: "^Sugartypes.Show_phrasenode.show _dbg);
       let (o, name, _) = o#phrase name in
       let (o, keys, _) = o#phrase keys in
       let (o, db, _) = o#phrase db in
       let (o, dtype) = o#sugar_datatype dtype in
       let (o, read_row) = o#datatype read_row in
       let (o, write_row) = o#datatype write_row in
       let (o, needed_row) = o#datatype needed_row in
       let tablelit : Sugartypes.phrasenode =
         `TableLit (name, (dtype, Some (read_row, write_row, needed_row)), constraints, keys, db) in
       let tablelit_type = `Table (read_row, write_row, needed_row) in

       let pattern : Sugartypes.pattern = (`Variable ("p", Some read_row, dp), dp) in
       (* Not sure what the `datatype option` in a ListLit is.
          - None fails pattern matching in transformSugar
          - Could be the element type. Could be the list type. *)
       let prov_calc_expr : Sugartypes.phrasenode = `ListLit ([`Var "p", dp], Some read_row) in
       let iter : Sugartypes.phrasenode = `Iteration ([`Table (pattern, (tablelit, dp))], (prov_calc_expr, dp), None, None) in

       (* Move this mess somewhere... *)
       let prov_rows : (Sugartypes.name * Sugartypes.fieldconstraint list) list -> Sugartypes.phrase StringMap.t =
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
       let prov_row = TypeUtils.map_record_type (fun t n -> if StringMap.mem n prov_rows
                                                            then Types.make_prov_type t (* Types.make_record_type (StringMap.from_alist [("data", t); ("prov", Types.prov_triple_type)]) *)
                                                            else t)
                                                read_row in
       let prov_type = Types.make_list_type prov_row in

       let delayed_type = Types.make_pure_function_type Types.unit_type prov_type in
       Debug.print ("Delayed prov type: "^(Types.Show_datatype.show delayed_type));

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
       Debug.print ("Delayed prov fun: "^(Sugartypes.Show_phrasenode.show delayed_prov));
       
       let pair : Sugartypes.phrasenode = `TupleLit [(tablelit, dp); (delayed_prov, dp)] in
       let pair_type = Types.make_tuple_type [tablelit_type; delayed_type] in
       Debug.print ("TableLit desugared:\n"^Sugartypes.Show_phrasenode.show pair);
       (o, pair, pair_type)
       
    | `Iteration (gens, body, cond, orderby) as _dbg ->
       Debug.print ("Iteration: \n" ^ Sugartypes.Show_phrasenode.show _dbg);
       let (o, e, t) = super#phrasenode _dbg in
       Debug.print ("Desugared iteration:\n" ^ Sugartypes.Show_phrasenode.show e);
       (o, e, t)

    | e -> super#phrasenode e
end

let where_prov_rewriting env = ((new where_prov_rewriting env) : where_prov_rewriting :> TransformSugar.transform)
