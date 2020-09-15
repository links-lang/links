open Schinks
open Driver
open Links_core.CommonTypes


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
      ~tparams:[q "Q1"]
      [("x", tvar_row "Q1")]  (* Usage of Q1 does not match its declared kind *)
      (tc_to_comp (return (i 3)));
  ]
let error_kind_mismatch_1 = "Type mismatch:"


let prog_quantifier_out_of_scope1 =
  [
    fun_ "f"  (forall [wi_q 0] ([] |--> int))
      ~tparams:[q "Q1"]
      []
      (tc_to_comp (return (i 3)));

    fun_ "g"   ([] |--> int)
      []
      (computation
        [
          fun_ "nested" ([wi_tvar 0] |--> int) (* Error: type variable 0 isn't in scope anymore *)
            ["x", wi_tvar 0]
            (tc_to_comp (return (i 4)));
        ]
        (return (i 3)))

  ]
let error_quantifier_out_of_scope1 = "Type variable 0 is unbound"


let prog_quantifier_out_of_scope2 =
  [
    fun_ "f"  (forall [wi_q 0] ([] |--> int))
      ~tparams:[wi_q 1]
      []
      (tc_to_comp (return (i 3)));

    fun_ "g"   ([] |--> int)
      []
      (computation
        [
          fun_ "nested" ([wi_tvar 1] |--> int) (* Error: type variable 1 isn't in scope anymore *)
            ["x", wi_tvar 1]
            (tc_to_comp (return (i 4)));
        ]
        (return (i 3)))

  ]
let error_quantifier_out_of_scope2 = "Type variable 1 is unbound"


let prog_quantifier_shadowing =
  fun_ "f" (forall [q "T1"] ([tvar "T1"] |--> unit_t))
      ~tparams:[q "Q1"]
      ["x", tvar "Q1"]
      (bindings_to_comp
        [
          fun_ "nested1" (forall  [q ~pk:PrimaryKind.Row "Q1"] ([] |--> int)) (* Shadowing Q1 to be a row now *)
            ~tparams:[q ~pk:PrimaryKind.Row "V1"]
            []
            (tc_to_comp (return (i 3)));

          fun_ "nested2" ([tvar "Q1"] |--> int) (* Q1 is back to having primary kind type now *)
            ["x", tvar "Q1"]
            (tc_to_comp (return (i 4)));
        ])


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

      expect_error ~name:"quantifier_out_of_scope1"
        ~error_regex:error_quantifier_out_of_scope1
        (bindings_to_comp prog_quantifier_out_of_scope1);

      expect_error ~name:"quantifier_out_of_scope2"
        ~error_regex:error_quantifier_out_of_scope2
        (bindings_to_comp prog_quantifier_out_of_scope2);

      expect_no_errors ~name:"quantifier_shadowing"
        (binding_to_comp prog_quantifier_shadowing);


    ]
