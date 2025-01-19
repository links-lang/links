open OUnit2
open Links_lens
open Lens.Utility
module H = UnitTestsLensCommon.LensTestHelpers

module Set = struct
  module Set = struct
    let of_list l = List.map ~f:H.colset_of_string l |> Alias.Set.Set.of_list

    let assert_equal_drop_error ~ctxt v1 v2 =
      assert_equal ~ctxt ~cmp:Lens.Sort.Drop_sort_error.equal
        ~printer:Lens.Sort.Drop_sort_error.show v1 v2

    let test_disjoint_1 _test_ctx =
      of_list [ "A B C"; "D"; "E F"; "H" ]
      |> Alias.Set.Set.is_disjoint
      |> Result.is_ok
      |> assert_bool "is not disjoint"

    let test_disjoint_2 _test_ctx =
      of_list [ "A B C"; "C D"; "E F"; "H" ]
      |> Alias.Set.Set.is_disjoint
      |> Result.is_error
      |> assert_bool "is not disjoint"

    let test_disjoint_3 _test_ctx =
      of_list [ "A B"; "C D"; "E F A"; "H" ]
      |> Alias.Set.Set.is_disjoint
      |> Result.is_error
      |> assert_bool "is not disjoint"

    let suite =
      [
        "disjoint_1" >:: test_disjoint_1;
        "disjoint_2" >:: test_disjoint_2;
        "disjoint_3" >:: test_disjoint_3;
      ]
  end

  let suite = [ "set" >::: Set.suite ]
end

let suite = "alias" >::: [ "set" >::: Set.suite ]
