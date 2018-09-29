open Types
open Sugartypes
open Operators
open Utility
open LensUtility
open Value
open LensRecordHelpers

exception Runtime_error = Errors.Runtime_error

let is_null name = (name = "null")

type select_query = {
    tables : (string * string) list;
    cols : lens_col list;
    pred: lens_phrase option;
    db : database;
}

let value_of_constant : Constant.constant -> Value.t  = function
    | `Int a -> `Int a
    | `Float f -> `Float f
    | `String s -> `String s
    | `Bool b -> `Bool b
    | _ -> failwith "Unsupported constant type."

let constant_of_value : Value.t -> Constant.constant  = function
    | `Int a -> `Int a
    | `Float f -> `Float f
    | `String s -> `String s
    | `Bool b -> `Bool b
    | _ -> failwith "Unsupported value type."

let translate_op_to_sql = function
    | "==" -> "="
    | "&&" -> "AND"
    | "||" -> "OR"
    | a -> a

let name_of_var (expr: phrase) =
    let expr,_ = expr in
    match expr with
    | `Var n -> n
    | _ -> failwith "Expected var."

let rec lens_phrase_of_phrase : phrase -> lens_phrase = function
    | `Constant c, _ -> `Constant c
    | `Var v, _ -> `Var v
    | `UnaryAppl ((_, op), phrase),_ -> `UnaryAppl (op, lens_phrase_of_phrase phrase)
    | `InfixAppl ((_, op), phrase1, phrase2),_ -> `InfixAppl (op, lens_phrase_of_phrase phrase1, lens_phrase_of_phrase phrase2)
    | `TupleLit l, _ -> `TupleLit (List.map lens_phrase_of_phrase l)
    | `FnAppl (fn, arg), _ ->
        begin
            match name_of_var fn with
            | "not" -> `UnaryAppl ((`Name "!"), lens_phrase_of_phrase (List.hd arg))
            | _ -> failwith "Unsupported function"
        end
    | _ -> failwith "Unknown phrasenode for lens_phrase to phrase."

let rec calculate_predicate (expr : lens_phrase) (get_val : string -> Value.t) =
    match expr with
    | `Constant c -> value_of_constant c
    | `Var v ->
        begin
            try
                get_val v
            with NotFound _ -> failwith ("Could not find column " ^ v ^ ".")
        end
    | `InfixAppl (op, a1, a2) ->
        let a1 = calculate_predicate a1 get_val in
        let a2 = calculate_predicate a2 get_val in
        begin
            match (string_of_binop op) with
            | "==" -> box_bool ( match a1 with `Bool b -> b == unbox_bool a2 | `Int i -> i == unbox_int a2 | `String s -> s = unbox_string a2 | _ -> failwith "Unsupported equality constant.")
            | "&&" -> box_bool (unbox_bool a1 && unbox_bool a2)
            | "+" -> box_int ((unbox_int a1) + (unbox_int a2))
            | "*" -> box_int (unbox_int a1 * unbox_int a2)
            | "-" -> box_int (unbox_int a1 - unbox_int a2)
            | ">" -> box_bool (unbox_int a1 > unbox_int a2)
            | "<" -> box_bool (unbox_int a1 < unbox_int a2)
            | _ -> failwith ("Unknown binary operation " ^ string_of_binop op ^ ".")
        end
    | `TupleLit l -> calculate_predicate (List.hd l) get_val
    | `UnaryAppl (op, arg) ->
        begin
            match string_of_unary_op op with
            | "!" ->
                let res = calculate_predicate arg get_val in
                let res = not (unbox_bool res) in
                box_bool res
            | ".-" ->
                let res = calculate_predicate arg get_val in
                let res = -. (unbox_float res) in
                box_float res
            | "-" ->
                let res = calculate_predicate arg get_val in
                let res = - (unbox_int res) in
                box_int res
            | op -> failwith ("Unsupported unary operation " ^ op)
        end
    | `In (names, vals) ->
            let find = List.map get_val names in
            let vals = List.map (List.map value_of_constant) vals in
            let res = List.mem find vals in
            box_bool res
    | `Case (inp, cases, otherwise) ->
            let inp = match inp with
            | None -> `Bool true
            | Some inp -> calculate_predicate inp get_val in
            try
                let (_k,v) = List.find (fun (k,_v) -> calculate_predicate k get_val = inp) cases in
                calculate_predicate v get_val
            with
                NotFound _ -> calculate_predicate otherwise get_val

    (* | `FnAppl (fn, arg) ->
        begin
            match name_of_var fn with
            | "not" ->
                let res = calculate_predicate (List.hd arg) get_val in
                let res = not (unbox_bool res) in
                box_bool res
            | _ -> failwith "Unknown function."
        end *)
    | _ -> failwith "Unknown phrasenode for calculate_predicate."


class dummy_database = object(_self)
  inherit Value.database
  method driver_name () = "dummy"
  method exec _query : Value.dbvalue =
    failwith "Dummy database exec not supported."
  method escape_string str =
    "'" ^ Str.global_replace (Str.regexp "'") "''" str ^ "'"
  method quote_field f =
    "`" ^ Str.global_replace (Str.regexp "`") "``" f ^ "`"
  method! make_insert_returning_query : (string * string list * string list list * string) -> string list =
    failwith "Dummy database make_insert_returning_query not supported"
end

let rec construct_query_db (expr : lens_phrase) (db : Value.database) (mapCol : string -> string) =
    let construct_query expr = construct_query_db expr db mapCol in
    match expr with
    | `Constant c -> Constant.string_of_constant c
    | `Var v -> mapCol v
    | `InfixAppl (op, a1, a2) -> construct_query a1 ^ " " ^ translate_op_to_sql (string_of_binop op) ^ " " ^ construct_query a2
    | `TupleLit l -> "(" ^ List.fold_left (fun a b -> a ^ ", " ^ (construct_query b)) (construct_query (List.hd l)) (List.tl l)  ^ ")"
    | `UnaryAppl (op, a1) ->
        begin
            match string_of_unary_op op with
            | "!" -> "NOT (" ^ construct_query a1 ^ ")"
            | a -> a ^ " (" ^ construct_query a1 ^ ")"
        end
    | `In (names, vals) ->
        let b = Buffer.create 255 in
        let cc = Buffer.add_string b in
        cc "("; List.iteri (fun i v -> if i > 0 then cc ", "; cc (mapCol v)) names; cc ") IN (";
        List.iteri (fun i v -> if i > 0 then cc ", "; cc "(";
            List.iteri (fun i2 v2 -> if i2 > 0 then cc ", "; cc (Constant.string_of_constant v2)) v; cc ")") vals;
            cc ")";
        Buffer.contents b
    | `Case (inp, cases, otherwise) ->
            let otherwise = construct_query otherwise in
            if cases = [] then
                (* special case: if there are no cases then just return query for otherwise *)
                otherwise
            else
                let cases = List.fold_left (fun a (k,v) -> a ^ " WHEN " ^ construct_query k ^ " THEN " ^ construct_query v) "" cases in
                (match inp with None -> "CASE" | Some inp -> "CASE (" ^ construct_query inp ^ ")") ^
                    cases ^ " ELSE " ^ otherwise ^ " END"

let construct_query (expr : lens_phrase) =
  let db = (new dummy_database) in
  let mapCol = fun a -> db#quote_field a in
  construct_query_db expr db mapCol

let construct_select_query (query : select_query) =
  let db = query.db in
  let colFn col = db#quote_field col.table ^ "." ^ db#quote_field col.name ^ " AS " ^ db#quote_field col.alias in
  let tableFn (table, alias) = db#quote_field table ^ " AS " ^ db#quote_field alias in
  let cols = List.filter (fun c -> c.present) query.cols in
  let cols = List.fold_left (fun a b -> a ^ ", " ^ colFn b) (colFn (List.hd cols)) (List.tl cols) in
  let tables = query.tables in
  let tables = List.fold_left (fun a b -> a ^ ", " ^ tableFn b) (tableFn (List.hd tables)) (List.tl tables) in
  let sql = "SELECT " ^ cols ^ " FROM " ^ tables in
  let mapCol = fun a ->
    let col = List.find (fun c -> c.alias = a) query.cols in
    db#quote_field col.table ^ "." ^ db#quote_field col.name in
   match query.pred with
  | Some qphrase -> sql ^ " WHERE " ^ construct_query_db qphrase db mapCol
  | None -> sql

let construct_select_query_sort db (sort : lens_sort) =
  let colFn col = db#quote_field col.table ^ "." ^ db#quote_field col.name ^ " AS " ^ db#quote_field col.alias in
  let tableFn (table, alias) = db#quote_field table ^ " AS " ^ db#quote_field alias in
  let cols = LensSort.cols sort in
  let colsF = List.filter (fun c -> c.present) cols in
  let tables = List.map (fun c -> c.table) cols in
  let tables = List.sort_uniq compare tables in
  let tables = List.map (fun c -> (c,c)) tables in
  let colsQ = List.fold_left (fun a b -> a ^ ", " ^ colFn b) (colFn (List.hd colsF)) (List.tl colsF) in
  let tablesQ = List.fold_left (fun a b -> a ^ ", " ^ tableFn b) (tableFn (List.hd tables)) (List.tl tables) in
  let sql = "SELECT " ^ colsQ ^ " FROM " ^ tablesQ in
  let mapCol = fun a ->
    let col = List.find (fun c -> c.alias = a) cols in
    db#quote_field col.table ^ "." ^ db#quote_field col.name in
   match get_lens_sort_pred sort with
  | Some qphrase -> sql ^ " WHERE " ^ construct_query_db qphrase db mapCol
  | None -> sql



let create_phrase_and (left : lens_phrase) (right : lens_phrase) =
    `InfixAppl (`And, left, right)

let create_phrase_or (left : lens_phrase) (right : lens_phrase) =
    `InfixAppl (`Or, left, right)

let create_phrase_equal (left : lens_phrase) (right : lens_phrase) =
    `InfixAppl (`Name "==", left, right)

let create_phrase_var (name : string) =
    `Var name

let create_phrase_constant_of_record_col (r : Value.t) (key : string) =
    `Constant (constant_of_value (get_record_val key r))

let create_phrase_not (arg : lens_phrase) =
    `UnaryAppl (`Name "!", arg)

let create_phrase_tuple (arg : lens_phrase) =
    `TupleLit [arg]

module Phrase = struct
    let combine_and phrase1 phrase2 =
        let tup_or x = match x with
            | `InfixAppl (`Or, _, _) -> create_phrase_tuple x
            | _ -> x in
        match phrase1 with
        | Some x ->
            begin
                match phrase2 with
                | Some x' -> Some (create_phrase_and (tup_or x) (tup_or x'))
                | None -> phrase1
            end
        | None -> phrase2

    let combine_or phrase1 phrase2 =
        match phrase1 with
        | Some x ->
            begin
                match phrase2 with
                | Some x' -> Some (create_phrase_or x x')
                | None -> phrase1
            end
        | None -> phrase2

    let combine_and_l phrasel =
        List.fold_left (fun phrase term ->
            combine_and phrase (Some term)
        ) None phrasel

    let combine_and_l_opt phrasel =
        List.fold_left (fun phrase term ->
            combine_and phrase term
        ) None phrasel

    let var = create_phrase_var

    let constant c = `Constant c
    let constant_bool b = `Constant (`Bool b)
    let constant_val v = `Constant (constant_of_value v)
    let constant_int i = `Constant (`Int i)

    let fold_or phrases =
        let ored = List.fold_left combine_or None phrases in
        match ored with
        | None -> Some (constant_bool false)
        | _ -> ored

    let equal = create_phrase_equal
    let greater_than left right =
        `InfixAppl (`Name ">", left, right)

    let constant_from_col = create_phrase_constant_of_record_col

    let in_expr names vals =
        if names = [] then
            None
        else if vals = [] then
            Some (`Constant (`Bool false))
        else
            let val_of_rec r = List.map constant_of_value r in
            let vals = List.map val_of_rec vals in
            Some (`In (names, vals))

    let matching_cols_simp (on : string list) (row : Value.t list) =
        let phrase = List.fold_left (fun phrase (on,v) ->
            let term = Some (equal (var on) (constant (constant_of_value v))) in
            combine_and phrase term
        ) None (List.combine on row) in
        phrase

    let matching_cols (on : ColSet.t) row =
        let phrase = List.fold_left (fun phrase on ->
            let term = Some (equal (var on) (constant_from_col row on)) in
            combine_and phrase term
        ) None (ColSet.elements on) in
        phrase

    let negate = create_phrase_not

    let tuple = create_phrase_tuple

    let case var = `Case var


    (* a generic function for traversing phrase expressions,
     * the function dosth is called on each node, and the
     * result is replaced with it *)
    let rec traverse (expr : lens_phrase) dosth =
        let fn expr' = traverse expr' dosth in
        let expr = match expr with
        | `Constant _ -> expr
        | `Var _ -> expr
        | `UnaryAppl (a, arg) ->
                let arg = fn arg in
                `UnaryAppl (a, arg)
        | `InfixAppl (a, a1, a2) ->
                let a1 = fn a1 in
                let a2 = fn a2 in
                `InfixAppl (a, a1, a2)
        | `TupleLit (x :: []) ->
                let x = fn x in
                `TupleLit ([x])
        | `Case (phr, cases, otherwise) ->
                let phr = OptionUtils.opt_map fn phr in
                let cases = List.map (fun (inp, lst) -> fn inp, fn lst) cases in
                let otherwise = fn otherwise in
                `Case (phr, cases, otherwise)
        | _ -> failwith "Unknown operation" in
        dosth expr

    let get_vars expr =
        let cols = ref ColSet.empty in
        let _ = traverse expr (fun expr ->
            match expr with
            | `Var n -> cols := ColSet.add n (!cols); expr
            | _ -> expr
        ) in
        !cols

    let rename_var (expr : lens_phrase) (repl : (string * string) list) : lens_phrase =
        traverse expr (fun expr ->
            match expr with
            | `Var n ->
                let v = List.find_opt (fun (k,_v) -> k = n) repl in
                let v = OptionUtils.opt_map (fun (_k,v) -> `Var v) v in
                OptionUtils.from_option expr v
            | _ -> expr
        )

    let replace_var (expr : lens_phrase) (repl : (string * Value.t) list) : lens_phrase =
        traverse expr (fun expr ->
            match expr with
            | `Var n ->
                let v = List.find_opt (fun (k,_v) -> k = n) repl in
                let v = OptionUtils.opt_map (fun (_,v) -> `Constant (constant_of_value v)) v in
                OptionUtils.from_option expr v
            | _ -> expr
        )


end

(*let calculate_predicate (expr : phrase) pred =
    (* let _ = Debug.print (construct_query expr) in
    let _ = Debug.print (string_of_value (calculate_predicate expr (fun a -> match a with "itemID" -> `Int 6 | "name" -> `String "john"))) in
    let _ = Debug.print (construct_query (replace_var expr ["itemID", `Int 5;"test", `Int 3])) in
    let expr, pos = expr in
    let expr2 = (expr,pos) in
    let applied = replace_var expr2 ["itemID", `Int 5] in
    let testExpr =  `InfixAppl (([], `And), (`TupleLit [expr2],pos), (`TupleLit [applied],pos)),pos in
    let _ = Debug.print (construct_query testExpr) in *)
    `Bool true *)

let calculate_predicate_rec (expr : lens_phrase) (r : Value.t) =
    let get_val = fun x -> get_record_val x r in
    calculate_predicate expr get_val




(* execution, as taken from database.ml (this cannot be reused though, as
      database.ml is lower down in the compile chain) *)

let value_of_db_string (value:string) t =
  match TypeUtils.concrete_type t with
    | `Primitive `Bool ->
        (* HACK:

           This should probably be part of the database driver as
           different databases have different representations of
           booleans.

           mysql appears to use 0/1 and postgres f/t
        *)
        Value.box_bool (value = "1" || value = "t" || value = "true")
    | `Primitive `Char -> Value.box_char (String.get value 0)
    | `Primitive `String -> Value.box_string value
    | `Primitive `Int  -> Value.box_int (int_of_string value)
    | `Primitive `Float -> (if value = "" then Value.box_float 0.00      (* HACK HACK *)
                            else Value.box_float (float_of_string value))
    | t -> failwith ("value_of_db_string: unsupported datatype: '" ^
                        Types.string_of_datatype t(*string_of_datatype t*)^"'")

let result_signature field_types result =
    let n = result#nfields in
    let rec rs i =
      if i >= n then
        [],true
      else
        let name = result#fname i in
          if start_of ~is:"order_" name then
            (* ignore ordering fields *)
            rs (i+1)
          else if List.mem_assoc name field_types then
            let fields,null_query = rs (i+1) in
	    (name, (List.assoc name field_types, i)) :: fields,
	    null_query && is_null(name)
          else
            failwith("Column " ^ name ^
                        " had no type info in query's type spec: " ^
                        mapstrcat ", " (fun (name, t) -> name ^ ":" ^
                          Types.string_of_datatype t)
                        field_types)
    in let rs, null_query = rs 0
    in if null_query then [] else rs

(* builds record given a row field accessor function *)
 let build_record (rs: (string * (Types.datatype * int)) list) (row:int -> string) =
    let rec build rs =
      match rs with
      | [] -> []
      | (name,(t,i))::rs' ->
	  (name,value_of_db_string (row i) t)::build rs'
    in build rs

let execute_select_result
    (field_types:(string * Types.datatype) list) (query:string) (db: database)  =
  let result = (db#exec query) in
    (match result#status with
       | `QueryOk -> result, result_signature field_types result
       | `QueryError msg -> raise (Runtime_error ("An error occurred executing the query " ^ query ^ ": " ^ msg)))


let build_result ((result:Value.dbvalue),rs) =
  `List (result#map (fun row ->
                     `Record (build_record rs row))
	   )

let execute_select
    (field_types:(string * Types.datatype) list) (query:string) (db : database)
    : Value.t =
  let result,rs = execute_select_result field_types query db in
  build_result (result,rs)
