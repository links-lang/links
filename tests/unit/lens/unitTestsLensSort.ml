open UnitTestsLensCommon
open OUnit2
open Lens.Sort
open Lens.Utility
module PT = Lens.Phrase.Type
module H = LensTestHelpers

let fds s = H.fundepset_of_string s

let lcol c =
  Lens.Column.make ~table:"t1" ~alias:c ~name:c ~typ:PT.Int ~present:true

let lcols c = H.colslist_of_string c |> List.map ~f:lcol

let cols = H.colset_of_string

module Drop = struct
  let assert_equal_drop_error ~ctxt v1 v2 =
    assert_equal ~ctxt ~cmp:Lens.Sort.Drop_sort_error.equal
      ~printer:Lens.Sort.Drop_sort_error.show v1 v2

  let simple _ctxt =
    let sort =
      let fds = fds "A -> B; B -> C" in
      let cols = lcols "A B C" in
      make ~fds cols
    in
    let _sort' =
      let drop = [ "C" ] in
      let key = cols "B" in
      let default = [ Lens.Phrase.Value.Int 0 ] in
      drop_lens_sort sort ~drop ~key ~default |> Result.ok_exn
    in
    ()

  let unbound_column ctxt =
    let sort =
      let fds = fds "A -> B; B -> C" in
      let cols = lcols "A B C" in
      make ~fds cols
    in
    let error =
      let drop = [ "D" ] in
      let key = cols "E" in
      let default = [ Lens.Phrase.Value.Int 0 ] in
      drop_lens_sort sort ~drop ~key ~default |> Result.unpack_error_exn
    in
    assert_equal_drop_error ~ctxt
      (Lens.Sort.Drop_sort_error.UnboundColumns (cols "D E"))
      error

  let drop_not_defined_by_key ctxt =
    let sort =
      let fds = fds "A -> B; B -> C" in
      let cols = lcols "A B C D" in
      make ~fds cols
    in
    let error =
      let drop = [ "D" ] in
      let key = cols "B" in
      let default = [ Lens.Phrase.Value.Int 0 ] in
      drop_lens_sort sort ~drop ~key ~default |> Result.unpack_error_exn
    in
    assert_equal_drop_error ~ctxt Lens.Sort.Drop_sort_error.DropNotDefinedByKey
      error

  let column_type_error ctxt =
    let sort =
      let fds = fds "A -> B; B -> C" in
      let cols = lcols "A B C D" in
      make ~fds cols
    in
    let error =
      let drop = [ "C" ] in
      let key = cols "B" in
      let default = [ Lens.Phrase.Value.String "test" ] in
      drop_lens_sort sort ~drop ~key ~default |> Result.unpack_error_exn
    in
    assert_equal_drop_error ~ctxt
      (Lens.Sort.Drop_sort_error.DropTypeError
         { column = "C"; default_type = PT.String; column_type = PT.Int })
      error

  let suite =
    [
      "simple" >:: simple;
      "unbound_column" >:: unbound_column;
      "defining_fd_not_found" >:: drop_not_defined_by_key;
      "column_type_error" >:: column_type_error;
    ]
end

let suite = "lens_sort" >::: [ "drop" >::: Drop.suite ]
