open List
open Utility
open Syntax
open Inference
open Sqlcompile

let d = `U dummy_position

let strlit (str : string) : untyped_expression =
  let chars = explode str in
    fold_right (fun l r -> Concat(l, r, d) )
      ((map
          (fun ch -> List_of(Constant((Char ch), d), d)) chars)
         : untyped_expression list)
      ((Nil d : untyped_expression))
      
let sample_database_expr = 
  Database(record_expr [("args", strlit "fang");
                        ("driver", strlit "foop");
                        ("name", strlit "it")]
             d, d)

let sample_query_expr =
  (For (List_of(Variable("x", d), d),
        "x",
        Apply(Variable("asList", d), 
              [TableHandle(sample_database_expr, 
                           strlit "foo",
                           (Types.make_record_type [("a", Types.int_type)], 
                            Types.unit_type), d)],
              d),
        d))

let sample_sorted_query_expr =
  (For(List_of(Variable("x", d), d),
       "x",
       SortBy(Apply(Variable("asList", d), 
                    [TableHandle(sample_database_expr, 
                                 strlit "foo",
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

(* let compiles_to_sql expr =  *)
(*   let (_env, expr) = Inference.type_expression Library.typing_env expr in *)
(*   let compiled_expr = sql_compile expr in *)
(*     (match compiled_expr with *)
(*         Some e -> print_endline "Compiled to: "; *)
(*           print_endline(string_of_expression e) *)
(*       | None -> print_endline "compiled to nothing"); *)
(*     match compiled_expr with *)
(*         Some e -> is_TableQuery e *)
(*       | _ -> false *)

let ez_test x f test diag i = 
  let y = f x in
  if test y then true else
    (prerr_endline("test " ^ string_of_int i ^ " failed:");
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
  [compiles_to_sql sample_query_expr;
   compiles_to_sql sample_sorted_query_expr]

let test() = run_tests sqlcompile_tests
