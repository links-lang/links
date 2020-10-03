open Schinks
open Driver


(* No parameters *)
let prog_no_parameters =
    [
      fun_ "f" ([] |~~> int)
        []
        (tc_to_comp (return (i 3)));

      let_ "x" int
        (apply (var "f") [])
    ]


(* Divergence between overall type and parameter types *)
let prog_type_annot_vs_param_type =
  fun_ "f" ([int] |--> int)
    [("x", string)]
    (tc_to_comp (return (var "x")))
let error_type_annot_vs_param_type = "Type mismatch"


(* There is a difference between a function of type
  [unit] -> int    and
  [] -> int
*)
let prog_unit_vs_no_params =
  fun_ "f" ([] |~~> int)
    [("x", unit_t)]
    (tc_to_comp (return (i 3)))
let error_unit_vs_no_params = "Type mismatch"


let suite =
  mk_suite
    ~name:"functions"
    [
      expect_no_errors ~name:"No parameters"
        (bindings_to_comp prog_no_parameters);

      expect_error ~name:"Divergence between overall type and parameter types"
        ~error_regex:error_type_annot_vs_param_type
        (binding_to_comp prog_type_annot_vs_param_type);

      expect_error ~name:"unit_vs_no_params"
        ~error_regex:error_unit_vs_no_params
        (binding_to_comp prog_unit_vs_no_params);
    ]
