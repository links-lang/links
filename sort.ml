(* What used to be the sort optimisation.  Currently out of action *)

(** Pushes the sort operator into a query.
    @param expr An expression to optimise.
    @return The expression modified as mentioned above.
    @version 1.0 *)
let rec sql_sort : optimiser = fun expr ->
  let rec expr_fields bindings = 
    let rec skip_lets bindings : expression -> ((string * [`Field of string | `Row]) list * expression) = function
        | Let (_, Variable (name, _), body, _) when mem_assoc name bindings -> skip_lets ((name, assoc name bindings) :: bindings) body
        | Let (_, Variable (name, _), body, _)                              -> skip_lets bindings body
        | Record_selection (label, _, _, Variable (name, _), body, _) when mem_assoc name bindings -> (match assoc name bindings with
                                                                                                         | `Row -> skip_lets ((name, `Field label) :: bindings) body
                                                                                                         | _ -> failwith "OP920")
        | Record_selection (label, _, _, Variable (name, _), body, _)                              -> skip_lets bindings body
        | expr -> (bindings, expr) in
    let rec fields_from_list bindings : expression -> [`Not_found | `Found_all | `Found of string list] = function
        | Variable (name, _) when mem_assoc name bindings ->
            (match assoc name bindings with
               | `Row -> `Found_all
               | `Field field -> `Found [field])
        | Let (variable, Variable (name, _), body, _) when mem_assoc name bindings -> fields_from_list ((variable, assoc name bindings) :: bindings) body
        | Let (variable, Variable (name, _), body, _)                              -> fields_from_list bindings body
        | Record_extension (_, value, unit_expression, _) ->
            let val_bindings, value = skip_lets bindings value in
              (match value with
                 | Variable (name, _) when mem_assoc name val_bindings ->
                     `Found [
                       match assoc name val_bindings with
                         | `Field field -> field
                         | _ -> failwith "OP915"
                     ]
                 | _ -> `Not_found)
        | Record_extension (_, value, record, _) ->
            let val_bindings, value = skip_lets bindings value in
              (match value with
                 | Variable (name, _) when mem_assoc name val_bindings ->
                     (match fields_from_list bindings record with
                        | `Found fields -> `Found ((
                                                     match assoc name val_bindings with
                                                       | `Field field -> field
                                                       | _ -> failwith "OP915"
                                                   ) :: fields)
                        | `Not_found -> `Not_found
                        | `Found_all -> failwith "OP933")
                 | _ -> `Not_found)
        | Record_selection (label, label_variable, variable, Variable (name, _), body, _) when mem_assoc name bindings ->
            (match assoc name bindings with
               | `Row -> expr_fields ((label_variable, `Field label) :: (variable, `Row) :: bindings) body
               | _ -> failwith "OP920")
        | Record_selection (_, _, _, Variable _, body, _) -> expr_fields bindings body
        | _ -> `Not_found in
      function
        | Let (variable, Variable (name, _), body, _) when mem_assoc name bindings -> expr_fields ((variable, assoc name bindings) :: bindings) body
        | Record_selection (label, label_variable, variable, Variable (name, _), body, _) when mem_assoc name bindings ->
            (match assoc name bindings with
               | `Row -> expr_fields ((label_variable, `Field label) :: (variable, `Row) :: bindings) body
               | _ -> failwith "OP920")
        | List_of (elem, _, _) -> fields_from_list bindings elem
        | Sort (_, e', _)              
        | Record_selection_empty (_, e', _)
        | Condition (_, Nil _, e', _)
        | Condition (_, e', Nil _, _)
        | Let (variable, Variable _, e', _)
        | Record_selection (_, _, _, Variable _, e', _)  -> expr_fields bindings e'
        | Let _
        | Rec _ 
        | Apply _
        | Table _
        | Variable _
        | Condition _ 
        | Concat _ 
        | Nil _
        | Record_selection _
        | For _  -> `Not_found
        | _ -> failwith "OP865" in
  let rec listify = function
    | Condition (i, e, t, d)                            -> Condition (i, listify t, listify e, d)
    | Let (var, value, body, d)                         -> Let (var, value, listify body, d)
    | Record_selection (lab, lvar, var, value, body, d) -> Record_selection (lab, lvar, var, value, listify body, d)
    | Record_selection_empty (v, body, d)               -> Record_selection_empty (v, listify body, d)
    | List_of (e, _, d)                       -> List_of (e, `List, d)
    | Nil v                                -> Nil v
    | _ -> failwith "OP865" in
  let rec push_sort up (expr:expression) : [`No_push | `Push of expression] = 
    (* Some s  == `Push s
       None    == No_push *)
    match expr with
      | Variable _
      | Nil _
      | List_of _
      | Concat _
      | Apply _  (* IMPROVABLE *) -> None
      | Condition (condition, t, e, data) ->
          (match push_sort up t, push_sort up e with
             | (None, None) -> None
             | (None, Some e) -> Some (Condition (condition, Sort (up, t, (Sugar._DUMMY_POS, `Not_typed, None)), e, data))
             | (Some t, None) -> Some (Condition (condition, t, Sort (up, e, (Sugar._DUMMY_POS, `Not_typed, None)), data))
             | (Some t, Some e) -> Some (Condition (condition, t, e, data)))
      | Let (variable, value, body, data) ->
          (match push_sort up body with
             | None -> None
             | Some body -> Some (Let (variable, value, body, data)))
      | Rec (variables, body, data) ->
          (match push_sort up body with
             | None -> None
             | Some body -> Some (Rec (variables, body, data)))
      | Record_selection (label, labvar, variable, value, body, data) ->
          (match push_sort up body with
             | None -> None
             | Some body -> Some (Record_selection (label, labvar, variable, value, body, data)))
      | Record_selection_empty (value, body, data) ->
          (match push_sort up body with
             | None -> None
             | Some body -> Some (Record_selection_empty (value, body, data)))
      | Variant_selection (value, case_label, case_variable, case_body, variable, body, data) ->
          (match push_sort up case_body, push_sort up body with
             | (None, None) -> None
             | (None, Some body) -> Some (Variant_selection (value, case_label, case_variable, Sort (up, case_body, (Sugar._DUMMY_POS, `Not_typed, None)), variable, body, data))
             | (Some case_body, None) -> Some (Variant_selection (value, case_label, case_variable, case_body, variable, Sort (up, body, (Sugar._DUMMY_POS, `Not_typed, None)), data))
             | (Some case_body, Some body) -> Some (Variant_selection (value, case_label, case_variable, case_body, variable, body, data)))
      | Variant_selection_empty (value, case_label, case_variable, case_body, data) ->
          (match push_sort up case_body with
             | None -> None
             | Some case_body -> Some (Variant_selection_empty (value, case_label, case_variable, case_body, data)))
      | For (expr, variable, Table (db, s, query, _), data) ->
          (match expr_fields [(variable, `Row)] expr with
             | `Not_found -> None
             | `Found_all ->
                 let new_sortings = 
		   let f = 
		     if up then (fun col -> `Asc  (col.Query.table_renamed, col.Query.renamed))
                     else       (fun col -> `Desc (col.Query.table_renamed, col.Query.renamed))
		   in map f query.Query.result_cols 
		 in Some (For
			     (listify expr, variable, 
			      Table (db, s,
				     {query with 
					Query.sortings = (new_sortings @ query.Query.sortings)},
				     (Sugar._DUMMY_POS, `Not_typed, None)), 
                              data))
             | `Found raw_order ->
                 let new_sortings = (map (
                                      fun field ->
                                        let table = (find (fun col -> col.Query.renamed = field) query.Query.result_cols).Query.table_renamed in
                                          if up then (`Asc (table, field)) else (`Desc (table, field))
                                    ) raw_order) in 
                 let query = {query with Query.sortings = (new_sortings @ query.Query.sortings)} in
                   Some (For (listify expr, variable, 
                                                Table (db, s, query, (Sugar._DUMMY_POS, `Not_typed, None)),
                                                data)))
      | For _ -> None (* IMPROVABLE? *)
      | Sort (inner_up, list, _) when inner_up = up -> Some list  (* IMPROVABLE this sort really does not bring anyting *)
      | Sort _ -> None  (* IMPROVABLE this sort really does not bring anyting *)
      | Table (db, s, query, data) ->
          let query_order = map (fun col ->
                                   if up then (`Asc (col.Query.table_renamed, col.Query.renamed))
                                   else (`Desc (col.Query.table_renamed, col.Query.renamed))) query.Query.result_cols
          in
            Some (Table (db, s, {query with Query.sortings = query_order}, data))
      | _ -> failwith "OP943" in
    
    match expr with
      | Sort (up, list, _) ->
	  (match push_sort up list with
             | Some list -> list
             | None -> expr)
