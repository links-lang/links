open Utility
open CommonTypes
open Types

(*
 * This module contains the query rewrite logic for temporal updates.
 * The translations mainly involve massaging the SQL DSL, following
 * the translations in Snodgrass' book,
 * "Developing Time-Oriented Database Applications in SQL".
 *
 * As an example, instead of creating an SQL Delete, a TransactionTime
 * deletion is, roughly speaking:
 *
 *   [| delete (x <-t- tbl) where M |] =
 *     update tbl set x.to = now() where [| M |]
 *
 * These functions are generally called directly from `evalir.ml` or entry
 * functions in `query.ml`. This module will not be touched for current-time
 * tables.
 *)
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

(* Some shared things *)
let forever_const = Constant.DateTime.forever
let sql_forever = Sql.Constant forever_const

let current_at from_field to_field timestamp =
  let open OpHelpers in
  op_and
    (lte from_field timestamp)
    (gt to_field timestamp)

(* Shared between valid and transaction time *)
let current_insertion table_name field_names from_field to_field rows =
  let compile_rows =
      List.map (List.map (Q.expression_of_base_value ->- base [])) in

  let rows = compile_rows rows in
  let field_names = field_names @ [from_field; to_field] in
  let now = Sql.Constant (Constant.DateTime.now ()) in
  let rows = List.map (fun vs -> vs @ [now; sql_forever]) rows in
  Sql.(Insert {
    ins_table = table_name;
    ins_fields = field_names;
    ins_records = Values rows })

module TransactionTime = struct

  let insert = current_insertion

  let update :
    Types.datatype StringMap.t ->
    ((Ir.var * string) * Q.t option * Q.t) ->
    string ->
    string ->
    Sql.query =
  fun table_types ((tbl_var, table), where, body) tt_from tt_to ->
    let open Sql in
    let now_const = Constant.DateTime.now () in
    let sql_now = Constant now_const in
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
        ins_records = TableQuery sel_var } in

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
      (* Note: Modified this to treat child queries as a transaction, for
         consistency. *)
      Sql.With (Sql.string_of_table_var sel_var,
        sel_query, [ins_query; upd_query])
    ])


  let delete :
    ((Ir.var * string) * Q.t option) ->
    string -> (* tt_to field *)
    Sql.query =
    fun ((tbl_var, table), where) tt_to ->
      let now = Sql.Constant (Constant.DateTime.now ()) in
      let open Sql in
      let is_current =  Apply ("==", [Project (tbl_var, tt_to); sql_forever]) in

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
      let env =
        Q.bind
          (Q.env_of_value_env QueryPolicy.Nested env)
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
        (Q.env_of_value_env QueryPolicy.Nested env)
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
        (TemporalField.data_field,
          Q.eta_expand_var (x, Types.make_record_type field_types));
        (TemporalField.from_field,
          Q.Project (table_var, from_field) );
        (TemporalField.to_field,
          Q.Project (table_var, to_field))
      ] in
    Q.Record metadata_record

  module Insert = struct
    let current = current_insertion

    (* Massages VT metadata into records to be inserted *)
    let valid_time_row_columns_values from_field to_field v =
      (* v should be a non-empty list of valid-time metadata. *)
      let records = Value.unbox_list v in
      assert (records <> []);
      let md_from = TemporalField.from_field in
      let md_to = TemporalField.to_field in
      let md_data = TemporalField.data_field in

      let fields =
        List.hd records
        |> Value.unbox_record
        |> List.assoc md_data
        |> Value.unbox_record
        |> List.map fst in
      let fields = fields @ [from_field; to_field] in

      let vss =
        List.map (fun md ->
          let md = Value.unbox_record md in
          let from_val = List.assoc md_from md in
          let to_val = List.assoc md_to md in

          let data =
            List.assoc md_data md
              |> Value.unbox_record
              |> List.map snd in
          let data = data @ [from_val; to_val] in
          List.map (Q.expression_of_base_value ->- base []) data
        ) records in

      (fields, vss)

    let sequenced table_name from_field to_field raw_rows =
      let (field_names, rows) =
        valid_time_row_columns_values from_field to_field raw_rows
      in
      Sql.(Insert {
        ins_table = table_name;
        ins_fields = field_names;
        ins_records = Values rows })
  end

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
        let open OpHelpers in
        let now_const = Constant.DateTime.now () in
        let sql_now = Constant now_const in
        let sql_proj field = Project (tbl_var, field) in
        let current_now =
          current_at (sql_proj from_field) (sql_proj to_field) sql_now in

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
            | Some where -> OpHelpers.op_and (base [] where) current_now
            | None -> current_now
        in

        let select =
          Sql.Select (All, Fields select_fields, [TableRef (table, tbl_var)], sel_where, []) in

        (* Generate fresh variable for selection result *)
        let sel_var = Var.fresh_raw_var () in

        (* Next, we need to insert the results *)
        let insert =
          Insert { ins_table = table; ins_fields = field_names;
            ins_records = TableQuery sel_var } in

        (* Next: close off the old rows *)
        let upd_current =
          let pred =
            let starts_before_now =
              lt (sql_proj from_field) sql_now in
            match where with
              | Some where -> op_and (base [] where) starts_before_now
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
              gte (sql_proj from_field) sql_now in
            match where with
              | Some where -> op_and (base [] where) in_future
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
          Sql.With (Sql.string_of_table_var sel_var,
            select, [insert; upd_current; upd_future])
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
                ins_records = TableQuery var
              } in
            Sql.With (Sql.string_of_table_var var, sel, [ins]) in

          (*  - Selection / insert #1: Old values at beginning of period of
                applicability *)
          let sel1 =
            let where =
              let open OpHelpers in
              op_and
                (lt (proj from_field) app_from)
                (gt (proj to_field) app_from)
              |> and_where in
            make_select [(to_field, app_from)] where |> insert_select in

          (* Selection / insert #2: Old values at end of period of applicability *)
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
        let open OpHelpers in
        let now_const = Constant.DateTime.now () in
        let sql_now = Constant now_const in
        let sql_proj field = Project (tbl_var, field) in
        let current_now =
          current_at (sql_proj from_field) (sql_proj to_field) sql_now
        in

        let upd_where =
          Some (OptionUtils.opt_app
            (fun q -> op_and (base [] q) current_now)
            current_now
            where) in

        let del_where =
          let valid_in_future =
            gte (sql_proj from_field) sql_now in
          Some (OptionUtils.opt_app
            (fun q -> op_and (base [] q) valid_in_future)
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
              ins_records = Sql.TableQuery sel_var } in

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
          Sql.Transaction [
              Sql.With (Sql.string_of_table_var sel_var, sel_query,
                [ins_query; upd1; upd2; del])
          ]
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
          (Q.env_of_value_env QueryPolicy.Nested env)
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
            (Q.env_of_value_env QueryPolicy.Nested env)
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

module TemporalJoin = struct

  (* By virtue of normal forms, we can do a straightforward transformation
   * to rewrite into a temporally-joined normal form (i.e., a join where the
   * period of validity of the returned value is the intersection of the periods
   * of validity of all joined records).
   *
   * There is one restriction: temporally-joined tables must be of the same time
   * dimension, meaning that we can't have both transaction-time and valid-time
   * tables in the same query. This will be checked dynamically at present,
   * however it might be possible to do so statically in future. *)
  class visitor temporality =
    object(o)
      inherit QueryLang.Transform.visitor as super
      (* The `for` comprehension of a normalised queries will
       * contain a list of all generators used within the query.
       * The traversal object needs to record a list of pairs
       * [(table name, start column, end column)] in order to generate
       * the correct projections for the predicate. *)
      val tables = []

      method private set_tables tbls = {< tables = tbls >}

      method private project tbl field =
          match tbl with
            | Q.Record x -> StringMap.find field x
            | _ -> Q.Project (tbl, field)

      (* Start time: maximum of all start times *)
      method start_time =
        let open Q in
        List.fold_right (fun (tbl_var, start_time, _) expr ->
          Apply (Primitive "greatest", [o#project tbl_var start_time; expr])
        ) tables (Constant Constant.DateTime.beginning_of_time)

      (* End time: minimum of all end times *)
      method end_time =
        let open Q in
        List.fold_right (fun (tbl_var, _, end_time) expr ->
          Apply (Primitive "least", [o#project tbl_var end_time; expr])
        ) tables (Q.Constant forever_const)

      method! query =
        let open Q in
        let open Value.Table in
        let app prim args = Apply (Primitive prim, args) in
        function
          | For (tag, gens, os, body) ->
              let tables =
                (* Restrict attention to ValidTime or TransactionTime tables *)
                List.filter_map (function
                  | (v, Q.Table ({ temporality; _ } as t))
                      when temporality = Temporality.Valid ||
                           temporality = Temporality.Transaction ->
                      Some (v, t)
                  | _ -> None) gens
              in

              (* Ensure that all tables correspond to the given temporality *)
              let matches_mode x = x.temporality = temporality in
              let () = List.iter (fun x ->
                if matches_mode (snd x) then () else
                  raise
                    (Errors.runtime_error
                      ("All tables in a temporal join must match the " ^
                      "mode of the join."))) tables
              in

              (* Create a Var for each variable -- requires creating a type
                 from the row and field names. *)
              let tables =
                List.map (fun (v, x) ->
                  (* Always defined for Valid / Transaction time *)
                  (* Might want a better representation -- this screams bad design. *)
                  let (from_field, to_field) =
                    OptionUtils.val_of x.temporal_fields
                  in
                  let ty =
                    List.fold_left
                      (fun acc (k, x) ->
                        match x with
                          | Present t -> StringMap.add k t acc
                          | _ -> assert false)
                      (StringMap.empty)
                      (fst3 x.row |> StringMap.to_alist) in
                  (Q.Var (v, Types.make_record_type ty), from_field, to_field)
                ) tables
              in

              let o = o#set_tables tables in
              let (o, body) = o#query body in
              (o, For (tag, gens, os, body))
          | If (i, t, e) ->
              let (o, i) = o#query i in
              let (o, t) = o#query t in
              let (o, e) = o#query e in
              let i =
                app "&&" [
                  i; app "<" [o#start_time; o#end_time] ] in
              (o, If (i, t, e))
          | Singleton data ->
              let record_fields =
                [(TemporalField.data_field, data);
                 (TemporalField.from_field, o#start_time);
                 (TemporalField.to_field, o#end_time)]
              in
              (o, Singleton (Record (StringMap.from_alist record_fields)))
          | q -> super#query q
    end

    (* External function *)
    let rewrite_temporal_join mode q =
      snd ((new visitor mode)#query q)
end
