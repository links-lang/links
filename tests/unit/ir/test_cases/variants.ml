open Schinks
open Driver
open Links_core.CommonTypes

module PK = PrimaryKind


let mk_superfluous_case_test
      name
      error_regex
      _VARIANT_TYPE
      _DEFAULT_OPT =
  expect_error
    ~name
    ~error_regex
    (binding_to_comp
      (fun_ "f" (forall [q_row "R"] ([_VARIANT_TYPE] |--> int))
        ~tparams:[q_row "R"]
        ["v", _VARIANT_TYPE]
        (tc_to_comp
           (case (var "v")
              ?default:_DEFAULT_OPT
              [
                ("c1", binder "_x" unit_t, (tc_to_comp (return (i 10))));
                (* Case c2 is supposed to be superfluous *)
                ("c2", binder "_y" unit_t, (tc_to_comp (return (i 11))))
              ]))))

let good =
[
  expect_no_errors ~name:"empty_default"
    (tc_to_comp
      (case (inject "constr" unit (variant ["constr", present unit_t]))
       ~default:(binder "_x" (variant []), (tc_to_comp (return (i 3))))
       ["constr", binder "_y" unit_t, tc_to_comp (return (i 2))]));

   (* Presence polymorphism is allowed for labels that have no corresponding match case,
      but are given to the default case instead *)
   expect_no_errors
    ~name:"presence_polymorphism_good"
    (binding_to_comp
      (fun_ "f" (forall [q ~pk:PK.Presence "P"]
                   ([(variant [("pres", present unit_t); ("poly", presence_var "P")])] |--> int))
        ~tparams:[q ~pk:PK.Presence "P"]
        ["v", (variant [("pres", present unit_t); ("poly", presence_var "P")])]
        (tc_to_comp
           (case (var "v")
              ~default:(
                binder "_x" (variant ["poly", presence_var "P"]),
                (tc_to_comp (return (i 1))))
              [
                ("pres", binder "_x" unit_t, (tc_to_comp (return (i 10))));
              ]))))
]




let bad =
[

  (* Variant-only tests *)

  expect_error
     ~name:"bad_variant1"
     ~error_regex:"trying to inject into non-variant type"
     (tc_to_comp
       (return (inject "bla" unit int)));

  expect_error
     ~name:"bad_variant2"
     ~error_regex:"Type mismatch:.*Expected:.*Int"
     (tc_to_comp
        (* Type in constructor and in variant type dont match *)
       (return (inject "bla" unit (variant ["bla", present int]))));

  expect_error
     ~name:"bad_variant3"
     ~error_regex:"Attempt to split row otherfield:Int on absent field bla"
     (tc_to_comp
        (* Label doesn't exist in variant type *)
       (return (inject "bla" unit (variant ["otherfield", present int]))));


  (* Tests for case forms *)

  expect_error
    ~name:"non_variant_type"
    ~error_regex:"Case over non-variant value"
    (tc_to_comp
       (case (record [("f1", (i 3))])
          ~default:(binder "_x" (record_t []), (tc_to_comp (return (i 3))))
          []));

  mk_superfluous_case_test
    "superfluous_case_without_default_closed_row"
    "cases not identical to present fields in closed row, no default case" (* WEIRD ERROR *)
    (variant ["c1", present unit_t])
    None;

  mk_superfluous_case_test
    "superfluous_case_with_default_closed_row"
    "superfluous case"
    (variant ["c1", present unit_t])
    (Some (binder "_d" (variant []), tc_to_comp (return (i 1))));

  mk_superfluous_case_test
    "superfluous_case_without_default_open_row"
    "case without default over open row" (* WEIRD ERROR *)
    (variant ~row_var:(row_var "R") ["c1", present unit_t])
    None;

  mk_superfluous_case_test
    "superfluous_case_without_default_open_row"
    "superfluous case"
    (variant ~row_var:(row_var "R") ["c1", present unit_t])
    (Some (binder "_d" (variant []), tc_to_comp (return (i 1))));


  expect_error
    ~name:"missing_case_closed_row"
    ~error_regex:"cases not identical to present fields in closed row, no default case" (* WEIRD ERROR *)
    (binding_to_comp
      (fun_ "f" ([(variant ["c1", present int; "c2", present int])] |--> int)
        ["v", variant ["c1", present int; "c2", present int]]
        (tc_to_comp
           (case (var "v")
              [
                ("c1", binder "_x" unit_t, (tc_to_comp (return (i 10))));
              ]))));


  (* Like test above, but with open row. The error is more general, as the lack of the case c2 doesn't matter *)
  expect_error
    ~name:"missing_case_open_row"
    ~error_regex:"case without default over open row"
    (binding_to_comp
      (fun_ "f" (forall [q_row "R"] ([(variant ~row_var:(row_var "R") ["c1", present int; "c2", present int])] |--> int))
        ~tparams:[q_row "R"]
        ["v", variant ~row_var:(row_var "R") ["c1", present int; "c2", present int]]
        (tc_to_comp
           (case (var "v")
              [
                ("c1", binder "_x" unit_t, (tc_to_comp (return (i 10))));
              ]))));


  expect_error
    ~name:"default_binder_wrong_type"
    ~error_regex:"Type mismatch.*Expected:.*c1-.*Actual:"
    (binding_to_comp
      (fun_ "f" (forall [q_row "R"] ([(variant ~row_var:(row_var "R") ["c1", present int; "c2", present int])] |--> int))
        ~tparams:[q_row "R"]
        ["v", variant ~row_var:(row_var "R") ["c1", present int; "c2", present int]]
        (tc_to_comp
           (case (var "v")
              (* We just omit c1 rather than setting it absent. Type Mismatch! *)
              ~default:(binder "_" (variant ~row_var:(row_var "R") ["c2", present int]),
                        tc_to_comp (return (i 1)))
              [
                ("c1", binder "_" int, (tc_to_comp (return (i 10))));
              ]))));

     (* Presence polymorphism is not allowed for labels that have a corresponding match case *)
     expect_error
      ~name:"presence_polymorphism_bad_1"
      ~error_regex:"row contains presence-polymorphic labels with corresponding match clauses. These can only be handled by a default case."
      (binding_to_comp
        (fun_ "f" (forall [q ~pk:PK.Presence "P"]
                     ([(variant [("pres", present unit_t); ("poly", presence_var "P")])] |--> int))
          ~tparams:[q ~pk:PK.Presence "P"]
          ["v", (variant [("pres", present unit_t); ("poly", presence_var "P")])]
          (tc_to_comp
             (case (var "v")
                [
                  ("pres", binder "_x" unit_t, (tc_to_comp (return (i 10))));
                  ("poly", binder "_x" unit_t, (tc_to_comp (return (i 11))))
                ]))));

     (* Presence polymorphism is not allowed for labels that have a corresponding match case *)
     expect_error
      ~name:"presence_polymorphism_bad_2"
      ~error_regex:"row contains presence-polymorphic labels with corresponding match clauses. These can only be handled by a default case."
      (binding_to_comp
        (fun_ "f" (forall [q ~pk:PK.Presence "P"]
                     ([(variant [("pres", present unit_t); ("poly", presence_var "P")])] |--> int))
          ~tparams:[q ~pk:PK.Presence "P"]
          ["v", (variant [("pres", present unit_t); ("poly", presence_var "P")])]
          (tc_to_comp
             (case (var "v")
                ~default:(
                  binder "_x"  (variant [("pres", present unit_t)]),
                  (tc_to_comp (return (i 1))))
                [
                  ("poly", binder "_x" unit_t, (tc_to_comp (return (i 11))))
                ]))))

]



let suite = mk_suite ~name:"effects" (bad @ good)
