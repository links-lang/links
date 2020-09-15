open Schinks
open Driver

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



let suite =
  mk_suite
    ~name:"effects"
    [
      expect_error ~name:"Toplevel wild"
        ~error_regex:error_toplevel_wild
        (bindings_to_comp prog_toplevel_wild);

    ]
