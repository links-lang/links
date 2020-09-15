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




let suite =
  mk_suite
    ~name:"functions"
    [
      expect_no_errors ~name:"No parameters"
        (bindings_to_comp prog_no_parameters);

      expect_error ~name:"Divergence between overall type and parameter types"
        ~error_regex:error_type_annot_vs_param_type
        (binding_to_comp prog_type_annot_vs_param_type);
    ]
