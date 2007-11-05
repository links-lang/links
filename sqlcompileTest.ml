open List
open Utility
open Syntax
open Inference
open Sqlcompile

let d = `U dummy_position

let database_expr = 
  Database(record_expr [("args", Syntax.Constant(Syntax.String "fang", d));
                        ("driver", Syntax.Constant(Syntax.String "foop", d));
                        ("name", Syntax.Constant(Syntax.String "it", d))]
             d, d)

(** A simple test: converting trivial comprehension into trivial query. *)
let query_expr =
  (For (List_of(Variable("x", d), d),
        "x",
        Apply(Variable("asList", d), 
              [TableHandle(database_expr, 
                           Syntax.Constant(Syntax.String "foo", d),
                           (Types.make_record_type [("a", Types.int_type)], 
                            Types.unit_type), d)],
              d),
        d))

(** Tests compilation despite renamings *)
let query2_expr =
  (For (Let("y", Variable("x", d), List_of(Variable("y", d), d), d),
        "x",
        Apply(Variable("asList", d), 
              [TableHandle(database_expr, 
                           Syntax.Constant(Syntax.String "foo", d),
                           (Types.make_record_type [("a", Types.int_type)], 
                            Types.unit_type), d)],
              d),
        d))

(** Tests project/erase normalization *)
let query3_expr =
  (For (Let("y", Erase (Variable("x", d), "a", d),
            List_of(record_expr 
                      [("c", Project(Variable("y", d), "b", d))] d,
                    d), d),
        "x",
        Apply(Variable("asList", d), 
              [TableHandle(database_expr, 
                           Syntax.Constant(Syntax.String "foo", d),
                           (Types.make_record_type [("a", Types.int_type);
                                                    ("b", Types.int_type)], 
                            Types.unit_type), d)],
              d),
        d))

(** Tests sort clauses. *)
let sorted_query_expr =
  (For(List_of(Variable("x", d), d),
       "x",
       SortBy(Apply(Variable("asList", d), 
                    [TableHandle(database_expr, 
                                 Syntax.Constant(Syntax.String "foo", d),
                                 (Types.make_record_type[("a",Types.int_type)],
                                  Types.unit_type), d)],
                    d), 
              Abstr(["x"], Project(Variable("x", d), "a", d), d),
              d),
       d))

let at_Some f = function
    Some x -> f x
  | None -> false
    
let rec is_TableQuery = function
  | Let(_x, _b, e, _d) -> is_TableQuery e
  | TableQuery _ -> true
  | _ -> false

let typecheck_sql_compile expr = 
  let (_env, expr) = Inference.type_expression Library.typing_env expr in
    sql_compile expr

let show_expr_option = function
    Some e -> print_endline "Compiled to: ";
      print_endline(string_of_expression e)
  | None -> print_endline "compiled to nothing"

let show_successes = false

let ez_test x f test diag i = 
  let y = f x in
  if test y then (if show_successes then diag y; true) else
    (prerr_endline("Test " ^ string_of_int i ^ " failed:");
     diag y; false)

let sum xs = fold_right (+) xs 0

let run_tests tests = 
  let num_tests = length tests in
  let num_successes = 
    sum (List.map2 (fun (x,f,test,diag) i -> 
                           if ez_test x f test diag i then
                             1 else 0
                   ) tests (fromTo 0 num_tests))
  in
    prerr_endline(Printf.sprintf "%d tests executed. %d succeeded. %d failed." 
                    num_tests num_successes (num_tests - num_successes))

let compiles_to_sql expr = 
  (expr, typecheck_sql_compile, at_Some is_TableQuery, show_expr_option)

let sqlcompile_tests =
  [compiles_to_sql query_expr;
   compiles_to_sql query2_expr;
   compiles_to_sql query3_expr;
   compiles_to_sql sorted_query_expr]

let test() = run_tests sqlcompile_tests
