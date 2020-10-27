open Schinks
open Driver

open Links_core
(* open Utility *)
open CommonTypes


(* Tests for closure conversion.
  Note that for a function taking a closure, there are three places where
  quantifiers matter:
  1. The toplevel quantifiers of the type of the function
  2. The quantifiers in the type abstraction built into a functions
     (e.g., the "tyars")
  3. The toplevel quantifiers of the type annotation of the closure variable.

  The quantifiers in 1. and 2. must be
  identical, modulo alpha-renaming
  The quantifiers in 3. are a prefix of
  those in 1 (and therefore also 2), modulo alpha-renaming.
*)


(* This tests that closures don't necessarily have to be records in terms,
  even though closure conversion always yields records *)
let prog_non_record_closure =
  computation
    [ fun_ "f" ([int] |--> int)
        [("x", int)]
        ~closure_var:("c",  int)
        (tc_to_comp (return (var "c")))
    ]
    (return (closure "f" [] (i 3)))


(* Tests the three different points where quantifiers are given (as described
  above).
  This function has two quantifiers for the closure itself, and two for the
  function. (where the ones for the closure are repeated, yielding 4 in total)
  *)
let prog_closure_quantifiers =
  fun_ "f" (forall [q "T1"; q "T2"; q "T3"; q "T4"] ([] |--> (tvar "T2")))
    ~tparams:[q "Q1"; q "Q2"; q "Q3"; q "Q4"]
    []
    ~closure_var:("c", forall [q "U1"; q "U2"] (tvar "U2"))
    (tc_to_comp (return (var "c")))


let prog_closure_supply_good =
  [
    prog_closure_quantifiers; (* Reuse function above *)

    let_ "x" (forall [q "Qx"; q "Qy" ] ([] |--> int))
      (return (closure "f" [targ unit_t; targ int] (i 3))) (* okay *)
  ]

let prog_closure_supply_tyargs_wrong =
  [
    prog_closure_quantifiers; (* Reuse function above *)

    let_ "x" (forall [q "Qx"; q "Qy" ] ([] |--> int))
      (return (closure "f" [targ unit_t] (i 3))) (* insufficient type args *)
  ]
let error_closure_supply_tyargs_wrong = "Providing wrong number of closure \
                                         type arguments"

let prog_closure_supply_type_wrong =
  [
    prog_closure_quantifiers; (* Reuse function above *)

    let_ "x" (forall [q "Qx"; q "Qy" ] ([] |--> int))
      (* wrong type of supplied closure env: *)
      (return (closure "f" [targ unit_t; targ string] (i 3)))
  ]
let error_closure_supply_type_wrong = "Type mismatch.+Expected.+String.+Actual.+Int"



let prog_closure_supply_kind_wrong =
  let some_row = lift_type (Links_core.Types.make_empty_closed_row ()) in
  [
    prog_closure_quantifiers; (* Reuse function above *)

    let_ "x" (forall [q "Qx"; q "Qy" ] ([] |--> int))
      (* wrong kind of first supplied targ: *)
      (return (closure "f" [targ ~pk:PrimaryKind.Row some_row; targ int] (i 3)))
  ]
let error_closure_supply_kind_wrong = "Kind mismatch in type application"


let prog_closure_quantifier_mismatch =
  let base_sk = (fst Sugartypes.default_subkind, Restriction.Base) in

  fun_ "f" (forall [q "T1"; q "T2"; q "T3"; q "T4"] ([] |--> (tvar "T2")))
    ~tparams:[q "Q1"; q "Q2"; q "Q3"; q "Q4"]
    []
    (* Reject this due to the mismatch in U2, we don't allow any subkinding here: *)
    ~closure_var:("c", forall [q "U1"; q ~sk:base_sk "U2"] (tvar "U2"))
    (tc_to_comp (return (var "c")))
let error_closure_quantifier_mismatch = "Mismatch in quantifier kinds in tyvar \
                                         list vs closure variable quantifiers"


let prog_no_closure_expected =
  computation
    [
      (fun_ "f" ([] |--> unit_t)
         []
         (tc_to_comp (return unit)))
    ]
    (return (closure "f" [] unit))
let error_no_closure_expected = "Providing closure to a function that does not need one"


let prog_untracked =
  (return (closure "f" [] unit))
let error_untracked = "Variable . is unbound"





let suite =
  mk_suite
    ~name:"closures"
    [
      expect_no_errors ~name:"non_record_closure"
        prog_non_record_closure;

      expect_no_errors ~name:"closure_quantifiers"
        (binding_to_comp prog_closure_quantifiers);

      expect_no_errors ~name:"closure_supply_good"
       (bindings_to_comp prog_closure_supply_good);

      expect_error ~name:"closure_supply_tyargs_wrong"
       ~error_regex:error_closure_supply_tyargs_wrong
       (bindings_to_comp prog_closure_supply_tyargs_wrong);

      expect_error ~name:"closure_supply_type_wrong"
       ~error_regex:error_closure_supply_type_wrong
       (bindings_to_comp prog_closure_supply_type_wrong);

      expect_error ~name:"closure_supply_kind_wrong"
       ~error_regex:error_closure_supply_kind_wrong
       (bindings_to_comp prog_closure_supply_kind_wrong);

      expect_error ~name:"closure_quantifier_mismatch"
       ~error_regex:error_closure_quantifier_mismatch
       (binding_to_comp prog_closure_quantifier_mismatch);

      expect_error ~name:"no_closure_expected"
        ~error_regex:error_no_closure_expected
        prog_no_closure_expected;

      expect_error ~name:"untracked"
        ~error_regex:error_untracked
        (tc_to_comp prog_untracked)
    ]
