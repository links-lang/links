open Schinks
open Driver


(* Param bound in function body *)
let prog_param_bound_in_function_body =
  fun_ "f" ([int] |--> int)
    [("x", int)]
    (tc_to_comp (return (var "x")))


(* Unbound var in function *)
let prog_unbound_var_in_function =
  fun_ "f" ([] |--> int)
    []
    (tc_to_comp (return (wi_var 0)))
let error_unbound_var_in_function = "Variable 0 is unbound"

(* Use variable after it went out of scope *)
let prog_var_went_out_of_scope =
  [
    fun_ "f" ([] |--> int)
      [("x", int)]
      (tc_to_comp (return (wi_var 0)));

    let_ "y" int
      (return (var "x"))
  ]
let error_prog_var_went_out_of_scope = "Variable [0-9] is unbound"


(* Fun is not recursive *)
let prog_fun_not_recursive =
  wi_fun_ (wi_binder 0 ([int] |--> int))
    [binder "x" int]
    (tc_to_comp (apply (wi_var 0) [var "x"]))
let error_fun_not_recursive = "Variable 0 is unbound"


(* Rec is indeed recursive *)
let prog_rec_is_recursive =
  rec_
    [
      def "f" ([int] |~~> int)
        [("x", int)]
        (computation
           [let_ "z" string (apply (var "g") [s "bla"])]
           ((return (i 3))));

      def "g" ([string] |~~> string)
        [("y", string)]
        (computation
           [let_ "z" int (apply (var "f") [i 3])]
           ((return (s "bla"))));
    ]



let suite =
  mk_suite
    ~name:"binding_tests"
    [
      expect_no_errors ~name:"param_bound_in_function_body"
        (binding_to_comp prog_param_bound_in_function_body);

      expect_error ~name:"unbound_var_in_function"
        ~error_regex:error_unbound_var_in_function
        (binding_to_comp prog_unbound_var_in_function);

      expect_error ~name:"var_went_out_of_scope"
        ~error_regex:error_prog_var_went_out_of_scope
        (bindings_to_comp prog_var_went_out_of_scope);

      expect_error ~name:"fun_not_recursive"
        ~error_regex:error_fun_not_recursive
        (binding_to_comp prog_fun_not_recursive);

      expect_no_errors ~name:"rec_is_recursive"
        (binding_to_comp prog_rec_is_recursive);
    ]
