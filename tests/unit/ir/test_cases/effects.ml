open Schinks
open Driver
open Links_core.CommonTypes

module PK = PrimaryKind


(* Links insists that the toplevel is wild *)
let prog_toplevel_wild =
  [
    fun_ "tamefun" ([] |--> int)
      []
      (tc_to_comp (return (i 3)));

    let_ "x" int
      (apply (var "tamefun") [])
  ]
let error_toplevel_wild = "Incompatible effects:"

(* Used in tests below *)
let shared_poly_effect =
  fun_ "f" (forall [q_row "e"] ( [] .-->{row [] (row_var "e")} unit_t))
    ~tparams:[q_row "e"]
    []
    (tc_to_comp (return unit))


(* Instantiate f to be tame *)
let prog_instantiation1 =
  [
    shared_poly_effect;

    fun_ "tame" ([] |--> unit_t)
      []
      (tc_to_comp
         (apply
            (tapp (var "f") [PK.Row, (row [] closed)])
            []))
  ]

(* Instantiate f to have some effect *)
let prog_instantiation2 =
  [
    shared_poly_effect;

    fun_ "tame" ([] |--> unit_t)
      []
      (tc_to_comp
         (apply
            (* Instantiating f to be non-tame*)
            (tapp (var "f") [PK.Row, (row ["bla", present ([] |--> int)] closed)])
            []))
  ]
let error_instantiation2 = "Incompatible effects:"


(* Verify that Absent in closed rows is irrelevant *)
let prog_instantiation3 =
  [
    shared_poly_effect;

    fun_ "tame" ([] |--> unit_t)
      []
      (tc_to_comp
         (apply
            (* Absent effect is irrelevant as the row is closed *)
            (tapp (var "f") [PK.Row, (row ["bla", absent] closed)])
            []))
  ]


(* Instantiate f such that it's effect contain a row variable *)
let prog_instantiation4 =
  [
    shared_poly_effect;

    fun_ "tame" (forall [q_row "Fo" ] ([] |--> unit_t))
      ~tparams:[q_row "Fi"]
      []
      (tc_to_comp
         (apply
            (* The effect variable breaks things here *)
            (tapp (var "f") [PK.Row, (row [] (row_var "Fi"))])
            []))
  ]
let error_instantiation4 = "Incompatible effects:"


(* a nested function's effects do not affect the ambient effect *)
let prog_fun_binding_no_effects =
  [
    shared_poly_effect;

    fun_ "tame" ([] |--> unit_t)
      []
      (computation
         [
           fun_ "wild_fun" ([] |~~> unit_t)
             []
             (tc_to_comp
                (apply
                   (tapp (var "f") [PK.Row, (row ["wild", present unit_t] closed)])
                   []))
         ]
         (return unit))
  ]

(* a let binding has effects! *)
let prog_let_binding_effects =
  [
    shared_poly_effect;

    fun_ "tame" ([] |--> unit_t)
      []
      (computation
         [
           let_ "wild_let" unit_t
             (apply
                (tapp (var "f") [PK.Row, (row ["wild", present unit_t] closed)])
                [])
         ]
         (return unit))
  ]
let error_let_binding_effects = "Incompatible effects:.+Actual:.+wild"



let suite =
  mk_suite
    ~name:"effects"
    [
      expect_error ~name:"Toplevel wild"
        ~error_regex:error_toplevel_wild
        (bindings_to_comp prog_toplevel_wild);

      expect_no_errors ~name:"instantiation1"
        (bindings_to_comp prog_instantiation1);

      expect_error ~name:"instantiation2"
        ~error_regex:error_instantiation2
        (bindings_to_comp prog_instantiation2);

      expect_no_errors ~name:"instantiation3"
        (bindings_to_comp prog_instantiation3);

      expect_error ~name:"instantiation4"
        ~error_regex:error_instantiation4
        (bindings_to_comp prog_instantiation4);

      expect_no_errors ~name:"fun_binding_no_effects"
        (bindings_to_comp prog_fun_binding_no_effects);

      expect_error ~name:"let_binding_effects"
        ~error_regex:error_let_binding_effects
        (bindings_to_comp prog_let_binding_effects);
    ]
