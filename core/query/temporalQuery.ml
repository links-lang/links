open Utility
open CommonTypes
open Types

(* Query compilation for temporal queries. *)

module Q = QueryLang
let base = Q.base

module OpHelpers = struct
  let binop op x y = Sql.Apply (op, [x; y])
  let lt = binop "<"
  let lte = binop "<="
  let gt = binop ">"
  let gte = binop ">="
  let _eq = binop "=="
  let op_and = binop "&&"
  let _op_or = binop "||"
end

module TransactionTime = struct

  let update :
    Types.datatype StringMap.t ->
    ((Ir.var * string) * Q.t option * Q.t) ->
    string ->
    string ->
    Sql.query =
  fun table_types ((tbl_var, table), where, body) tt_from tt_to ->

    let now_const = Constant.DateTime.now () in
    let sql_now = Sql.Constant now_const in
    let forever_const = Constant.DateTime.forever in
    let sql_forever = Sql.Constant forever_const in

    let open Sql in
    let is_current = Apply ("==", [Project (tbl_var, tt_to); sql_forever]) in

    (* Begin by constructing a select query, which gets our affected rows. *)

    (* We need to augment table_types with the period-stamping columns. *)
    let table_types =
      table_types
        |> StringMap.add tt_from (Primitive Primitive.DateTime)
        |> StringMap.add tt_to (Primitive Primitive.DateTime) in
    let field_names =
      StringMap.to_alist table_types |> List.map fst in

    (* The select query should either select the updated field if specified,
     * otherwise it should select a the field projection. *)

    (* Begin by getting the update fields, augmented with the period stamp [now, forever) *)
    let record_fields =
      match body with
        | Q.Record fields ->
            fields
            |> StringMap.add tt_from (Q.Constant now_const)
            |> StringMap.add tt_to (Q.Constant forever_const)
        | _ -> assert false in
    let record_fields_list = StringMap.to_alist record_fields in

    (* Select either the field name if unspecified, or the updated value
     * if it is. *)
    let select_fields =
      StringMap.mapi (fun k _ ->
        OptionUtils.opt_map (base []) (StringMap.lookup k record_fields)
        |> OptionUtils.from_option (Project (tbl_var, k))) table_types
      |> StringMap.to_alist
      (* Need to swap (col, val) pairs to (val, col) to fit select_clause AST,
       * which mirrors "SELECT V as K" form in SQL *)
      |> List.map (fun (k, v) -> (v, k)) in
    let select_fields = Sql.Fields select_fields in

    (* We need to add an "is_current" clause to the selection predicate. *)
    let sel_where =
      match where with
        | Some where -> Apply ("&&", [base [] where; is_current])
        | None -> is_current in

    (* And here's the selection query: *)
    let sel_query =
      Sql.Select (Sql.All, select_fields, [TableRef (table, tbl_var)], sel_where, []) in

    (* Generate fresh variable for selection result *)
    let sel_var = Var.fresh_raw_var () in

    (* Next, we need to insert the results *)
    let ins_query =
      Sql.Insert { ins_table = table; ins_fields = field_names;
        ins_records = Query sel_var } in

    (* Next, we need an update query which closes off the previous rows. *)
    (* The update predicate is the records which satisfy the selection predicate,
     * but do *not* have the updated fields *)
    let const_true = Sql.Constant (Constant.Bool true) in
    let fields_neq =
      let fields_eq =
        List.fold_left (fun acc (label, v) ->
          let proj = Project (tbl_var, label) in
          let eq = Apply ("==", [proj; base [] v]) in
          Apply ("&&", [eq; acc])) const_true record_fields_list in
      Apply ("not", [fields_eq]) in

    let update_predicate =
      let basic_clauses = [is_current; fields_neq] in
      let clauses =
        match where with
          | None -> basic_clauses
          | Some pred -> (base [] pred) :: basic_clauses in
      List.fold_left (fun acc x -> Apply ("&&", [x; acc])) const_true clauses in

    let upd_query =
      Sql.Update {
        upd_table = table;
        upd_fields = [(tt_to, sql_now)];
        upd_where = Some update_predicate
      } in

    (* Finally, construct the transaction. *)
    Sql.Transaction ([
      (* First variable of "with" is unused? *)
      (* Note: Modified this to treat child queries as a transaction, for
         consistency. *)
      Sql.With (Sql.string_of_subquery_var sel_var,
        sel_query, Sql.Transaction [ins_query; upd_query])
    ])


  let delete :
    ((Ir.var * string) * Q.t option) ->
    string -> (* tt_to field *)
    Sql.query =
    fun ((tbl_var, table), where) tt_to ->
      let now = Sql.Constant (Constant.DateTime.now ()) in
      let forever = Sql.Constant (Constant.DateTime.forever) in
      let open Sql in
      let is_current =  Apply ("==", [Project (tbl_var, tt_to); forever]) in

      (* where x --> where (x && is_current) *)
      let upd_where =
        Some (OptionUtils.opt_app
          (fun q -> Apply ("&&", [base [] q; is_current]))
          is_current
          where) in

      let upd_fields = [(tt_to, now)] in
      Update { upd_table = table; upd_fields; upd_where }


  let compile_update :
    Value.env ->
    ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option * Ir.computation) ->
    string -> (* transaction time from field *)
    string -> (* transaction time to field *)
    Sql.query =
    fun env ((x, table, field_types), where, body) tt_from tt_to ->
      (* SJF: Note that I've changed this from QueryPolicy.Default to
         QueryPolicy.Flat. *)
      let env =
        Q.bind
          (Q.env_of_value_env QueryPolicy.Flat env)
          (x, Q.Var (x, Types.make_record_type field_types))
      in
      let where = opt_map (Query.Eval.norm_comp env) where in
      let body = Query.Eval.norm_comp env body in
      update field_types ((x, table), where, body) tt_from tt_to

  let compile_delete :
   Value.database ->
    Value.env ->
   ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option) ->
    string (* Transaction time 'to' field *) ->
    Sql.query =
      fun db env ((x, table, field_types), where) to_field ->
    let env =
      Q.bind
        (Q.env_of_value_env QueryPolicy.Flat env)
        (x, Q.Var (x, Types.make_record_type field_types)) in
    let where = opt_map (Query.Eval.norm_comp env) where in
    let q = delete ((x, table), where) to_field in
    Debug.print ("Generated delete query: " ^ (db#string_of_query q));
    q
end

module ValidTime = struct

  (* Wraps the results of a selection in a record corresponding
   * to valid-time metadata. *)
  let metadata x field_types from_field to_field =
    let extended_field_types =
        field_types
          |> StringMap.add from_field Types.datetime_type
          |> StringMap.add to_field Types.datetime_type in
    let table_var = Q.Var (x, Types.make_record_type extended_field_types) in
    let metadata_record =
      StringMap.from_alist [
        (TemporalOperation.data_field,
          Q.eta_expand_var (x, Types.make_record_type field_types));
        (TemporalOperation.from_field,
          Q.Project (table_var, from_field) );
        (TemporalOperation.to_field,
          Q.Project (table_var, to_field))
      ] in
    Q.Record metadata_record

  module Update = struct
    let current :
      Types.datatype StringMap.t ->
      ((Ir.var * string) * Q.t option * Q.t) ->
      string ->
      string ->
      Sql.query =
      fun table_types ((tbl_var, table), where, body) from_field to_field ->
        (* Valid time current updates are similar to transaction-time updates
         * in structure: fetch the current rows, but with updated fields;
         * insert the updated rows; and close off the previous ones.
         * There's an extra step, though: ensure any future rows are also
         * updated. *)
        let open Sql in
        let now_const = Constant.DateTime.now () in
        let sql_now = Constant now_const in
        let sql_proj field = Project (tbl_var, field) in
        let sql_binop op x1 x2 = Apply (op, [x1; x2]) in
        let current_at =
          sql_binop "&&"
            (sql_binop "<=" (sql_proj from_field) sql_now)
            (sql_binop ">" (sql_proj to_field) sql_now) in

        (* Construct select query. Currently a lot is C&P; would be better to
         * abstract it (in some nice way) *)
        let table_types =
          table_types
            |> StringMap.add from_field (Primitive Primitive.DateTime)
            |> StringMap.add to_field (Primitive Primitive.DateTime) in
        let field_names =
          StringMap.to_alist table_types |> List.map fst in
        let record_fields =
          match body with
            | Q.Record fields -> fields
            | _ -> assert false in
        let fields_with_time =
            StringMap.add from_field (Q.Constant now_const) record_fields in

        (* Select either the field name if unspecified, or the updated value
         * if it is. *)
        let select_fields =
          StringMap.mapi (fun k _ ->
            OptionUtils.opt_map (base []) (StringMap.lookup k fields_with_time)
            |> OptionUtils.from_option (Project (tbl_var, k))) table_types
          |> StringMap.to_alist
          (* Need to swap (col, val) pairs to (val, col) to fit select_clause AST,
           * which mirrors "SELECT V as K" form in SQL *)
          |> List.map (fun (k, v) -> (v, k)) in
        (* Add "current at" clause to predicate *)
        let sel_where =
          match where with
            | Some where -> Sql.Apply ("&&", [base [] where; current_at])
            | None -> current_at in

        let select =
          Sql.Select (All, Fields select_fields, [TableRef (table, tbl_var)], sel_where, []) in

        (* Generate fresh variable for selection result *)
        let sel_var = Var.fresh_raw_var () in

        (* Next, we need to insert the results *)
        let insert =
          Insert { ins_table = table; ins_fields = field_names;
            ins_records = Query sel_var } in

        (* Next: close off the old rows *)
        let upd_current =
          let pred =
            let starts_before_now =
              sql_binop "<" (sql_proj from_field) sql_now in
            match where with
              | Some where -> sql_binop "&&" (base [] where) starts_before_now
              | _ -> starts_before_now in
          Update {
            upd_table = table;
            upd_fields = [(to_field, sql_now)];
            upd_where = Some pred
          } in

        (* Update the future rows.  *)
        let upd_future =
          let pred =
            let in_future =
              sql_binop ">=" (sql_proj from_field) sql_now in
            match where with
              | Some where -> sql_binop "&&" (base [] where) in_future
              | _ -> in_future in

          Update {
            upd_table = table;
            upd_fields =
              StringMap.to_alist record_fields
              |> List.map (fun (x, y) -> (x, base [] y));
            upd_where = Some pred
          } in

        (* Finally, construct the transaction. *)
        Transaction ([
          Sql.With (Sql.string_of_subquery_var sel_var,
            select, Sql.Transaction [insert; upd_current; upd_future])
        ])

    let nonsequenced :
      ((Ir.var * string) * Q.t option * Q.t * Q.t option * Q.t option) ->
      string (* valid from field *) ->
      string (* valid to field *) ->
      Sql.query =
      fun ((_, table), where, body, valid_from, valid_to) from_field to_field ->
        let open Sql in
        let upd_where = OptionUtils.opt_map (base []) where in
        let opt_as_tuple_list field x =
          OptionUtils.opt_as_list x
          |> List.map (fun x -> (field, base [] x)) in

        let upd_from = opt_as_tuple_list from_field valid_from in
        let upd_to = opt_as_tuple_list to_field valid_to in

        let upd_fields =
          Q.unbox_record body
          |> StringMap.map (base [])
          |> StringMap.to_alist in
        let upd_fields = upd_fields @ upd_from @ upd_to in
        Update { upd_table = table; upd_fields; upd_where }

    let sequenced :
      Types.datatype StringMap.t ->
      ((Ir.var * string) * Q.t option * Q.t * Q.t * Q.t) ->
      string (* valid from field *) ->
      string (* valid to field *) ->
      Sql.query =
        fun table_types ((tbl_var, table), where, app_from, app_to, set) from_field to_field ->
          let open Sql in
          let app_from = base [] app_from in
          let app_to = base [] app_to in

          (* Preamble: Unbox, get things set up *)

          (* - Add the period-stamping fields to the table types *)
          let table_types =
            table_types
              |> StringMap.add from_field (Primitive Primitive.DateTime)
              |> StringMap.add to_field (Primitive Primitive.DateTime) in

          let field_names =
            StringMap.to_alist table_types |> List.map fst in

          let and_where pred =
            let open OpHelpers in
            match where with
              | Some where -> op_and (base [] where) pred
              | None -> pred in

          let proj field = Project (tbl_var, field) in

          (* 2x Selections *)
          (*  - Select either the field name if unspecified, or the updated value
           *    if it is. *)
          let make_select values where =
            let values = StringMap.from_alist values in
            let fields =
              StringMap.mapi (fun k _ ->
                StringMap.lookup k values
                |> OptionUtils.from_option (Project (tbl_var, k))) table_types
              |> StringMap.to_alist
              (* Need to swap (col, val) pairs to (val, col) to fit select_clause AST,
               * which mirrors "SELECT V as K" form in SQL *)
              |> List.map (fun (k, v) -> (v, k)) in
            Sql.Select (All, Fields fields, [TableRef (table, tbl_var)], where, []) in

          let insert_select sel =
            let var = Var.fresh_raw_var () in
            let ins =
              Sql.Insert {
                ins_table = table;
                ins_fields = field_names;
                ins_records = Query var
              } in
            Sql.With (Sql.string_of_subquery_var var, sel, ins) in

          (*  - Selection / insert #1: Old values at beginning of PA *)
          let sel1 =
            let where =
              let open OpHelpers in
              op_and
                (lt (proj from_field) app_from)
                (gt (proj to_field) app_from)
              |> and_where in
            make_select [(to_field, app_from)] where |> insert_select in

          (* Selection / insert #2: Old values at end of PA *)
          let sel2 =
            let where =
              let open OpHelpers in
              op_and
                (lt (proj from_field) app_to)
                (gt (proj to_field) app_to)
              |> and_where in
            make_select [(from_field, app_to)] where |> insert_select in

          (* Update #1: Update rows overlapping PA. *)
          let upd1 =
            let upd_fields =
              Q.unbox_record set
              |> StringMap.to_alist
              |> List.map (fun (k, v) -> (k, base [] v)) in

            let where =
              let open OpHelpers in
              op_and
                (lt (proj from_field) app_to)
                (gt (proj to_field) app_from)
              |> and_where in

            (* - Unpack fields to be updated *)
            Sql.Update {
              upd_table = table;
              upd_fields;
              upd_where = Some where
            } in
          (* Update #2: Set start time of updated rows to be start of PA. *)
          let upd2 =
            let where =
              let open OpHelpers in
              op_and
                (lt (proj from_field) app_from)
                (gt (proj to_field) app_from)
              |> and_where in

            (* - Unpack fields to be updated *)
            Sql.Update {
              upd_table = table;
              upd_fields = [(from_field, app_from)];
              upd_where = Some where
            } in

          (* Update #3: Set end time of updated rows to be end of PA. *)
          let upd3 =
            let where =
              let open OpHelpers in
              op_and
                (lt (proj from_field) app_to)
                (gt (proj to_field) app_to)
              |> and_where in

            (* - Unpack fields to be updated *)
            Sql.Update {
              upd_table = table;
              upd_fields = [(to_field, app_to)];
              upd_where = Some where
            } in

          (* Batch up *)
          Transaction [
            sel1; sel2; upd1; upd2; upd3
          ]
  end

  module Delete = struct
    let current :
      ((Ir.var * string) * Q.t option) ->
      string -> (* valid from field *)
      string -> (* valid to field *)
      Sql.query =
      fun ((tbl_var, table), where) from_field to_field ->
        let open Sql in
        let now_const = Constant.DateTime.now () in
        let sql_now = Constant now_const in
        let sql_binop op x1 x2 = Apply (op, [x1; x2]) in
        let sql_proj field = Project (tbl_var, field) in

        let current_at =
          sql_binop "&&"
            (sql_binop "<=" (sql_proj from_field) sql_now)
            (sql_binop ">" (sql_proj to_field) sql_now) in

        let upd_where =
          Some (OptionUtils.opt_app
            (fun q -> sql_binop "&&" (base [] q) current_at)
            current_at
            where) in

        let del_where =
          let valid_in_future =
            sql_binop ">=" (sql_proj from_field) sql_now in
          Some (OptionUtils.opt_app
            (fun q -> sql_binop "&&" (base [] q) valid_in_future)
            valid_in_future
            where) in

        let upd =
          Update {
            upd_table = table;
            upd_fields = [(to_field, sql_now)];
            upd_where
          } in

        let del = Delete { del_table = table; del_where } in
        Transaction [ upd; del ]

    (* Note: No need for `nonsequenced` since it's the same logic
     * as a 'regular' deletion. *)


    let sequenced :
      Types.datatype StringMap.t ->
      ((Ir.var * string) * Q.t option * Q.t * Q.t) ->
      string (* valid from field *) ->
      string (* valid to field *) ->
      Sql.query =
        fun table_types ((tbl_var, table), where, app_from, app_to) from_field to_field ->
          let open OpHelpers in
          let open Sql in
          let app_from = base [] app_from in
          let app_to = base [] app_to in
          let proj k = Sql.Project (tbl_var, k) in
          let and_where pred =
            match where with
              | Some where -> op_and (base [] where) pred
              | None -> pred in

          (* Add the period-stamping fields to the table types *)
          let table_types =
            table_types
              |> StringMap.add from_field (Primitive Primitive.DateTime)
              |> StringMap.add to_field (Primitive Primitive.DateTime) in

          (* Select all fields, 'start' date is end of PA *)
          let select_fields =
            StringMap.mapi (fun k _ ->
              if k = from_field then app_to else proj k) table_types
            |> StringMap.to_alist
            (* Need to swap (col, val) pairs to (val, col) to fit select_clause AST,
             * which mirrors "SELECT V as K" form in SQL *)
            |> List.map (fun (k, v) -> (v, k)) in

          let field_names = List.map snd select_fields in

          let sel_where  =
            op_and
              (lt (proj from_field) app_from)
              (gt (proj to_field) app_to)
            |> and_where in

          let sel_query =
            Sql.Select (All, Fields select_fields, [TableRef (table, tbl_var)], sel_where, []) in

          (* Generate fresh variable for selection result *)
          let sel_var = Var.fresh_raw_var () in

          (* Next, we need to insert the results *)
          let ins_query =
            Sql.Insert { ins_table = table; ins_fields = field_names;
              ins_records = Sql.Query sel_var } in

          (* Truncate 'end' date for records with PV starting before PA and ending in PA *)
          let update upd_table upd_fields where =
            Sql.Update { upd_table; upd_fields; upd_where = Some where } in

          let upd1_pred =
            op_and
              (lt (proj from_field) app_from)
              (gte (proj to_field) app_from) |> and_where in
          let upd1 = update table [(to_field, app_from)] upd1_pred in

          (* Truncate 'start' date for records with PV starting in PA and ending after PA *)
          let upd2_pred =
            op_and
              (lt (proj from_field) app_to)
              (gte (proj to_field) app_to) |> and_where in
          let upd2 = update table [(from_field, app_to)] upd2_pred in

          (* Delete records completely overlapped by PV *)
          let del_pred =
            op_and
              (gte (proj from_field) app_from)
              (lte (proj to_field) app_to) |> and_where in

          let del =
            Sql.Delete {
              del_table = table;
              del_where = Some del_pred
            } in

          (* Package everything up in a transaction *)
          Sql.With (Sql.string_of_subquery_var sel_var, sel_query,
            Sql.Transaction [ins_query; upd1; upd2; del])
  end

  let compile_update :
      Ir.valid_time_update ->
      Value.database ->
      Value.env ->
      ((Ir.var * string * Types.datatype StringMap.t) *
        Ir.computation option * Ir.computation) ->
      string (* valid from field *) ->
      string (* valid to field *) ->
      Sql.query =
    fun upd db env ((x, table, field_types), where, body)
      from_field to_field ->

      let to_bind =
        match upd with
          | Ir.NonsequencedUpdate _ ->
              metadata x field_types from_field to_field
          | _ -> Q.Var (x, Types.make_record_type field_types) in

      let env =
        Q.bind
          (Q.env_of_value_env QueryPolicy.Flat env)
          (x, to_bind) in

      let where = opt_map (Query.Eval.norm_comp env) where in
      let body = Query.Eval.norm_comp env body in

      let q =
        let open Ir in
        match upd with
          | CurrentUpdate ->
              Update.current
                field_types
                ((x, table), where, body) from_field to_field
          | SequencedUpdate { validity_from; validity_to } ->
              let validity_from = Query.Eval.xlate env validity_from in
              let validity_to = Query.Eval.xlate env validity_to in
              Update.sequenced
                field_types
                ((x, table), where, validity_from, validity_to, body) from_field to_field
          | NonsequencedUpdate { from_time; to_time } ->
              let from_time = opt_map (Query.Eval.norm_comp env) from_time in
              let to_time = opt_map (Query.Eval.norm_comp env) to_time in
              Update.nonsequenced
                ((x, table), where, body, from_time, to_time)
                from_field to_field
      in
      Debug.print ("Generated valid-time update query: " ^ (db#string_of_query q));
      q

  let compile_delete :
    Ir.valid_time_deletion ->
    Value.database ->
    Value.env ->
    ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option) ->
    string (* from field *) ->
    string (* to field *) ->
    Sql.query =
      fun del db env ((x, table, field_types), where) from_field to_field ->
        let env to_bind =
          Q.bind
            (Q.env_of_value_env QueryPolicy.Flat env)
            (x, to_bind) in
        let open Ir in
        let q =
          begin
            match del with
              | CurrentDeletion ->
                  let env = env (Q.Var (x, Types.make_record_type field_types)) in
                  let where = opt_map (Query.Eval.norm_comp env) where in
                  Delete.current
                    ((x, table), where) from_field to_field
              | NonsequencedDeletion ->
                  (* Same logic as deletion -- just that the metadata
                   * we've bound will be different *)
                  let md =
                    metadata x field_types from_field to_field in
                  let where = opt_map (Query.Eval.norm_comp (env md)) where in
                  QueryLang.delete ((x, table), where)
              | SequencedDeletion { validity_from; validity_to } ->
                  let env = env (Q.Var (x, Types.make_record_type field_types)) in
                  let where = opt_map (Query.Eval.norm_comp env) where in
                  Delete.sequenced field_types
                    ((x, table), where,
                      Query.Eval.xlate env validity_from,
                      Query.Eval.xlate env validity_to)
                    from_field to_field
          end
        in
        Debug.print ("Generated valid time deletion query: " ^ (db#string_of_query q));
        q
end
