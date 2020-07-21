open Schinks_st
open Driver



let prog_quantifier_mismatch_1 =
  [
    fun_ "f"  (forall [q "T1"; q "T2"] ([tvar "T1"; tvar "T2"] |--> int))
      ~tparams:[q "Q1"; q "Q2"]
      [("x", tvar "T1");( "y", tvar "T2")] (* type error, must use Q* here *)
      (tc_to_comp (return (i 3)));
  ]
let error_quantifier_mismatch_1 = "Type mismatch:"


let prog_kind_mismatch_1 =
  [
    fun_ "f"  (forall [q "T1"] ([tvar "T1"] |--> int))
      ~tparams:[q"Q1"]
      [("x", tvar_row "Q1")]  (* Usage of Q1 does not match its declared kind *)
      (tc_to_comp (return (i 3)));
  ]
let error_kind_mismatch_1 = "Type mismatch:"


let suite =
  mk_suite
    ~name:"quantifiers"
    [
      expect_error ~name:"error_quantifier_mismatch_1"
        ~error_regex:error_quantifier_mismatch_1
        (bindings_to_comp prog_quantifier_mismatch_1);

     expect_error ~name:"error_kind_mismatch_1"
        ~error_regex:error_kind_mismatch_1
        (bindings_to_comp prog_kind_mismatch_1);
    ]
