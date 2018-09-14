open OUnit2
open Pg_database
open Types
open Value
open Utility
open LensUtility
open LensHelpers
open LensSetOperations

(* ensure links configuration is loaded *)
let _ = OptionUtils.opt_iter (Settings.load_file false) Basicsettings.config_file_path

let display_table_query_opt = Conf.make_bool "display_table_query" false "Show queries to take and manipulate tables."
let leave_tables_opt = Conf.make_bool "leave_tables" false "Do not delete tables after run." 
let database_args_opt = Conf.make_string "database_args" ("links:" ^ Settings.get_value Basicsettings.database_args) "Database connection args." 
let verbose_opt = Conf.make_bool "v" false "Print verbose information."
let classic_opt = Conf.make_bool "classic_lenses" false "Use non incremental relational lenses."
let benchmark_opt = Conf.make_bool "benchmark" false "Benchmark operations."
let set_n_opt = Conf.make_int "set_n" 0 "Override n." 
let set_upto_opt = Conf.make_int "set_upto" 10 "Override upto."

module LensTestHelpers = struct

    let get_db test_ctx = 
        (* host port dbname user pw *) 
        let (conn,_) = get_pg_database_by_string (database_args_opt test_ctx) in
        conn

    (** Only print when **)
    let fmt_std_v test_ctx (fn : Format.formatter -> unit) =
        if verbose_opt test_ctx then
            fn Format.std_formatter
        else
            ()

    let fmt_err_v test_ctx (fn : Format.formatter -> unit) =
        if verbose_opt test_ctx then
            fn Format.err_formatter
        else
            ()

    let print_verbose test_ctx message = 
        if verbose_opt test_ctx then
            print_endline message
        else
            ()

    let colslist_of_string str =
        let cols = String.split_on_char ' ' str in
        let cols = List.filter (fun s -> String.length s <> 0) cols in
        cols

    let rec_constr_int (cols : string) (vals : int list) =
        let cols = colslist_of_string cols in
        Value.box_record (List.map2 (fun c v -> (c, box_int v)) cols vals)

    let delt_constr_int (cols : string) (vals, m : int list * int) =
    rec_constr_int cols vals, m

    let colset_of_string str = 
        let cols = colslist_of_string str in
        ColSet.of_list cols

    let fundep_of_string str =
        let split = Str.split (Str.regexp "->") str in
        let _ = assert_equal (List.length split) 2 in
        let colsets = List.map colset_of_string split in
        FunDep.make (List.nth colsets 0) (List.nth colsets 1)

    let fundepset_of_string str =
        let split = Str.split (Str.regexp ";") str in
        let fds = List.map fundep_of_string split in
        FunDepSet.of_list fds

    let mem_lens (fd_set : fundepset) (name : string) data =
        let cols = FunDepSet.fold (fun fd fld -> ColSet.union_all [FunDep.left fd; FunDep.right fd; fld]) fd_set ColSet.empty in
        let cols = ColSet.elements cols in
        let colFn tbl name = {
            alias = name; name = name; table = tbl; typ = `Primitive `Int; present = true
        } in
        let l1 = `LensMem ((`List data), (fd_set, None, List.map (colFn name) cols)) in
        l1

    let mem_lens_str fds name data =
        mem_lens (fundepset_of_string fds) name data

    let join_lens_dl l1 l2 on =
        let sort, on = join_lens_sort (Lens.sort l1) (Lens.sort l2) on in
        `LensJoin (l1, l2, on, `Constant (`Bool true), `Constant (`Bool false), sort)

    let join_lens_dr l1 l2 on =
        let sort, on = join_lens_sort (Lens.sort l1) (Lens.sort l2) on in
        `LensJoin (l1, l2, on, `Constant (`Bool false), `Constant (`Bool true), sort)

    let select_lens l phrase =
        let sort = Lens.sort l in
        let sort = LensTypes.select_lens_sort sort phrase in
        `LensSelect (l, phrase, sort)

    let drop_lens l drop key default = 
        let sort = Lens.sort l in
        let (fds, cond, r) = sort in
        let fds = FunDepSet.remove_defines fds (ColSet.singleton drop) in
        let r = LensRecordHelpers.remove_record_type_column drop r in
        `LensDrop (l, drop, key, default, (fds, cond, r))

    let select_query l predicate =
        lens_get_select l predicate

    let create_table test_ctx db tablename (primary_key : string list) (fields : string list) =
        let colfn col = col ^ " INTEGER NOT NULL" in
        let query = "CREATE TABLE " ^ tablename ^ " ( " 
            ^ List.fold_left (fun a b -> a ^ colfn b ^ ", ") "" fields
            ^ "CONSTRAINT \"PK_" ^ tablename ^ "\" PRIMARY KEY ("
                ^ List.fold_left (fun a b -> a ^ ", " ^ b) (List.hd primary_key) (List.tl primary_key)
            ^ "));" in
        let _ = if display_table_query_opt test_ctx then Debug.print query else () in
        db#exec query 

      
    let create_table_easy test_ctx db tablename str =
        let fd = fundep_of_string str in
        let left = FunDep.left fd in
        let right = FunDep.right fd in
        let cols = ColSet.union left right in
        create_table test_ctx db tablename (ColSet.elements left) (ColSet.elements cols)

    let value_as_string db =
        function
        | `String s -> "\'" ^ db#escape_string s ^ "\'"
        | v -> string_of_value v

    let row_columns data =
        let data = unbox_list data in
        let r = List.hd data in
        List.map fst (unbox_record r)  

    let row_values db data = 
        let data = unbox_list data in
        let fn = List.map (value_as_string db -<- snd) in
        let data = List.map (fn -<- unbox_record) data in
        data

    let box_int_record_list cols data = 
        let data = List.map (List.map box_int) data in
        let data = List.map (List.combine cols) data in
        let data = List.map box_record data in
        let data = box_list data in
        data

    let insert_rows db table data =
        (* See lib.ml "InsertRows" *)
        let cols = row_columns data in
        let rows = row_values db data in
        Database.execute_insert (table, cols, rows) db 

    let drop_if_exists test_ctx (db : database) table =
        let query = "DROP TABLE IF EXISTS " ^ table in
        let _ = if display_table_query_opt test_ctx then Debug.print query else () in
        db#exec query

    let drop_if_cleanup test_ctx (db : database) table =
        if leave_tables_opt test_ctx then 
            ()
        else 
            let _ = drop_if_exists test_ctx db table in
            ()

    type col_gen_type = [
        | `Seq
        | `Constant of int
        | `RandTo of int
        | `Rand
    ]

    (* is there a more standardized function for this? *)
    let rec range a b =
        if a > b then []
        else a :: range (a+1) b;; 

    let gen_data (cols : col_gen_type list) cnt =
        let _ = Random.self_init () in
        let data = List.map (fun i ->
            List.map (function
                | `Seq -> i
                | `Constant n -> n
                | `RandTo n -> 1 + Random.int (if n < 5 then 5 else n) 
                | `Rand -> Random.bits ()
            ) cols 
        ) (range 1 cnt) in
        data

    let create_record_type (cols : (string * typ) list) =
        let cols = List.map (fun (a,b) -> a, `Present b) cols in
        `Record (Utility.StringMap.from_alist cols, Unionfind.fresh `Closed, false)

    let create_lens_db db tablename fd (key : string list) (cols : string list) = 
        let colFn tbl name = {
            alias = name; name = name; table = tbl; typ = `Primitive `Int; present = true
        } in
        (* table is `Table of (database * db : str) * tablename : str * keys : string list list * row type *)
        let colst = List.map (fun a -> a, `Primitive `Int) cols in
        let `Record recordType = create_record_type colst in
        let table = ((db, ""), tablename, [key], recordType) in
        let l1 = `Lens (table, (FunDepSet.singleton fd, None, List.map (colFn tablename) cols)) in
        l1

    let drop_create_populate_table test_ctx (db : database) table str str2 colGen cnt =
        let fd = fundep_of_string str in
        let left = FunDep.left fd in
        let _right = FunDep.right fd in
        let cols = colslist_of_string str2 in
        let _ = drop_if_exists test_ctx db table in
        let _ = create_table test_ctx db table (ColSet.elements left) cols in
        let data = gen_data colGen cnt in
        if List.length data > 0 then
            let data = box_int_record_list cols data in
            let _ = insert_rows db table data in
            ()
        else
            ();
        let lens = create_lens_db db table fd (ColSet.elements left) cols in
        lens

    let reset_query_timer () = query_timer := 0; query_count := 0

    let get_query_time () = !query_timer

    let print_query_time () = 
        print_endline ("Query Time: " ^ string_of_int (get_query_time ()) ^ " / " ^ string_of_int !query_count ^ " queries")

    let time_query _in_mem fn =
        reset_query_timer ();
        let res = Debug.debug_time_out fn (fun time -> print_endline ("Total Time: " ^ string_of_int time)) in
        print_query_time ();
        res

    let time_op fn =
        reset_query_timer ();
        let ttime = ref 0 in
        let _ = Debug.debug_time_out fn (fun time -> ttime := time) in
        (get_query_time (), !ttime)

    let time_query_both fn =
        let res = time_query false fn in
        let _ = time_query true fn in
        res

    let col_list_to_string (cols : string list) (sep : string) =
        List.fold_left (fun a b -> a ^ sep ^ b) (List.hd cols) (List.tl cols)

    let assert_rec_list_eq (actual : Value.t) (expected : Value.t) =
        if actual = box_list [] || expected = box_list [] then
            (* cannot construct sorted records without columns, but if one is empty so should the other *)
            assert (actual = expected)
        else
            let actual = SortedRecords.construct actual in
            let expected = SortedRecords.construct expected in
            assert (actual = expected)
end

let test_fundep_of_string _test_ctx = 
    let _fds = LensTestHelpers.fundepset_of_string "A B -> C; C -> D; D -> E F" in
    let _ = assert_equal "{{\"A\"; \"B\"; } -> {\"C\"; }; {\"C\"; } -> {\"D\"; }; {\"D\"; } -> {\"E\"; \"F\"; }; }" in
    ()


let suite =
    "lens_common_helpers">:::
    [
        "fundep_of_string">:: test_fundep_of_string;
    ];;
