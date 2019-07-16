open UnitTestsLensCommon
open OUnit2
open Links_core
open Links_lens
open Utility
open Phrase.Value
module Fun_dep = Lens.Fun_dep
module H = LensTestHelpers

let dat_fd_set = H.fundepset_of_string "A B -> C D; C D -> E; E -> F G"

let dat_cols = H.colset_of_string "C D"

let dat_closure = H.colset_of_string "C D E F G"

let dat_fd_set_2 = H.fundepset_of_string "A -> B; B -> C"

let cols s = H.colset_of_string s

let fds s = H.fundepset_of_string s

let rec_constr (cols : string list) (vals : int list) =
  box_record (List.map2 (fun c v -> (c, box_int v)) cols vals)

let delt_constr (cols : string list) ((vals, m) : int list * int) =
  (rec_constr cols vals, m)

(* Tests *)

let test_show_fd_set test_ctx =
  let show = Fun_dep.Set.show dat_fd_set in
  H.print_verbose test_ctx show ;
  let cmp = "{({A; B; }, {C; D; }); ({C; D; }, {E; }); ({E; }, {F; G; }); }" in
  assert_equal show cmp

let test_transitive_closure _test_ctx =
  let outp = Fun_dep.Set.transitive_closure ~cols:dat_cols dat_fd_set in
  assert_equal true (Lens.Alias.Set.equal outp dat_closure)

let construct_join_lens fd_set name data =
  let cols =
    Fun_dep.Set.fold
      (fun fd fld ->
        Lens.Alias.Set.union_all [Fun_dep.left fd; Fun_dep.right fd; fld])
      fd_set Lens.Alias.Set.empty
  in
  let cols = Lens.Alias.Set.elements cols in
  let colFn table name =
    Lens.Column.make ~alias:name ~name ~table ~typ:Lens.Phrase.Type.Int
      ~present:true
  in
  let l1 =
    `LensMem (`List data, (fd_set, None, List.map ~f:(colFn name) cols))
  in
  l1

let construct_join_lens_2 l1 l2 on =
  let sort, on =
    Lens.Sort.join_lens_sort (Lens.Value.sort l1) (Lens.Value.sort l2) ~on
    |> Result.ok_exn
  in
  `LensJoin (l1, l2, on, `Constant (`Bool true), `Constant (`Bool false), sort)

let cat_tex cols name delta =
  let cs = List.fold_right (fun _a b -> b ^ "c") cols "" in
  let _ = Debug.print ("\\begin{array}{c|" ^ cs ^ "}") in
  let _ =
    Debug.print
      ("\t" ^ name ^ List.fold_left (fun a b -> a ^ " & " ^ b) "" cols ^ "\\\\")
  in
  let _ = Debug.print "\t\\hline" in
  let _ =
    if List.length delta = 0 then Debug.print "\\\\"
    else
      let _ =
        List.map
          ~f:(fun (row, m) ->
            Debug.print
              ( List.fold_left
                  (fun a (_, b) ->
                    a ^ "& " ^ string_of_int (unbox_int b) ^ " ")
                  ("\t" ^ string_of_int m)
                  (unbox_record row)
              ^ "\\\\" ))
          delta
      in
      ()
  in
  let _ = Debug.print "\\end{array}" in
  ()

let test_calculate_fd_changelist test_ctx =
  let data = UnitTestsLensSetOperations.test_data_3 in
  let fds = dat_fd_set_2 in
  let changeset =
    Lens.Sorted_records.calculate_fd_changelist ~fun_deps:fds data
  in
  let _ =
    List.map
      ~f:(fun ((cols_l, cols_r), changes) ->
        let _ =
          H.print_verbose test_ctx
            ( H.col_list_to_string cols_l " "
            ^ " -> "
            ^ H.col_list_to_string cols_r " " )
        in
        let strfn dat =
          if dat = [] then ""
          else
            List.fold_left
              (fun a b -> a ^ ", " ^ show b)
              (show (List.hd dat))
              (List.tl dat)
        in
        let _ =
          List.map
            ~f:(fun (chl, chr) ->
              H.print_verbose test_ctx ("  " ^ strfn chl ^ " -> " ^ strfn chr))
            changes
        in
        ())
      changeset
  in
  (* let phrase = Lens.Helpers.Incremental.matches_change changeset in
  let str = match phrase with None -> "None" | Some phrase -> Format.asprintf "%a" Lens.Database.fmt_phrase_dummy phrase in
     H.print_verbose test_ctx str; *)
  ()

let assert_equal_cols ~ctxt v1 v2 =
  assert_equal ~ctxt ~cmp:Alias.Set.equal ~printer:Alias.Set.show v1 v2

let assert_equal_fds ~ctxt v1 v2 =
  assert_equal ~ctxt ~cmp:Fun_dep.Set.equal ~printer:Fun_dep.Set.show_pretty v1
    v2

let assert_equal_tree_error ~ctxt v1 v2 =
  assert_equal ~ctxt ~cmp:( = ) ~printer:Fun_dep.Check_error.show v1 v2

let assert_equal_fun_dep_tree ~ctxt v1 v2 =
  assert_equal ~ctxt ~cmp:( = ) ~printer:Fun_dep.Tree.show_pretty v1 v2

let assert_equal_tree_form_error ~ctxt v1 v2 =
  assert_equal ~ctxt ~cmp:Fun_dep.Tree.Tree_form_error.equal
    ~printer:Fun_dep.Tree.Tree_form_error.show v1 v2

let node c subnodes =
  let open Fun_dep.Tree in
  FDNode (cols c, subnodes)

module Tree_form = struct
  let simple ctxt =
    let open Fun_dep.Tree in
    let fds = fds "A -> B C; A -> D; B C -> E F; D -> G" in
    let in_tree_form = Fun_dep.Tree.in_tree_form fds |> H.assert_ok in
    assert_equal_fds ~ctxt fds in_tree_form ;
    let tree = of_fds fds ~columns:(cols "A B C D E F G") |> Result.ok_exn in
    let cmp_bc = node "B C" [node "E F" []] in
    let cmp_d = node "D" [node "G" []] in
    assert_equal_fun_dep_tree ~ctxt [node "A" [cmp_bc; cmp_d]] tree

  let key_extra ctxt =
    let open Fun_dep.Tree in
    let fds = fds "A -> B C" in
    let in_tree_form = Fun_dep.Tree.in_tree_form fds |> H.assert_ok in
    assert_equal_fds ~ctxt fds in_tree_form ;
    let tree = of_fds fds ~columns:(cols "A B C D") |> Result.ok_exn in
    let cmp_tree = [node "D" []; node "A" [node "B C" []]] in
    assert_equal_fun_dep_tree ~ctxt cmp_tree tree

  let key_overlap ctxt =
    let open Fun_dep.Tree in
    let fds = fds "A B -> D E; B C -> F G" in
    let err = Fun_dep.Tree.in_tree_form fds |> H.assert_error in
    assert_equal_tree_form_error ~ctxt err
      (Fun_dep.Tree.Tree_form_error.NotDisjoint (cols "B")) ;
    let tree = of_fds fds ~columns:(cols "A B C D E F G") |> H.assert_error in
    assert_equal_tree_error ~ctxt
      (Fun_dep.Check_error.FunDepNotTreeForm (cols ""))
      tree

  let key_split ctxt =
    let open Fun_dep.Tree in
    let fds' = fds "A -> B C D E; B C -> F; D E -> G" in
    let in_tree_form = Fun_dep.Tree.in_tree_form fds' |> H.assert_ok in
    assert_equal_fds ~ctxt
      (fds "A -> B C; A -> D E; B C -> F; D E -> G")
      in_tree_form ;
    let columns = cols "A B C D E F G" in
    let tree = of_fds fds' ~columns |> Result.ok_exn in
    let cmp_tree =
      [node "A" [node "B C" [node "F" []]; node "D E" [node "G" []]]]
    in
    assert_equal_fun_dep_tree ~ctxt cmp_tree tree

  let key_split_2 ctxt =
    let fds' = fds "A -> B C D E; B C -> F" in
    let in_tree_form = Fun_dep.Tree.in_tree_form fds' |> H.assert_ok in
    assert_equal_fds ~ctxt (fds "A -> B C; A -> D E; B C -> F") in_tree_form

  let recursive ctxt =
    let open Fun_dep.Tree in
    let fds' = fds "A -> B; B -> A" in
    let err = Fun_dep.Tree.in_tree_form fds' |> H.assert_error in
    assert_equal_tree_form_error ~ctxt err
      (Fun_dep.Tree.Tree_form_error.ContainsCycle [cols "A"; cols "B"; cols "A"]) ;
    let columns = cols "A B C" in
    let error = of_fds fds' ~columns |> Result.unpack_error_exn in
    assert_equal_tree_error ~ctxt
      (Fun_dep.Check_error.ProbablyCycle (cols "A B"))
      error

  let suite =
    [ "simple" >:: simple
    ; "key_extra" >:: key_extra
    ; "key_overlap" >:: key_overlap
    ; "key_split" >:: key_split
    ; "key_split_2" >:: key_split_2
    ; "recursive" >:: recursive ]
end

module Remove_fd = struct
  let simple ctxt =
    let open Fun_dep.Set in
    let fds' =
      fds "A -> B; B -> C" |> remove_defines ~cols:(cols "C") |> Result.ok_exn
    in
    let cmp = fds "A -> B" in
    assert_equal_fds ~ctxt fds' cmp

  let remove_intermediary ctxt =
    let open Fun_dep.Set in
    let fds' =
      fds "A -> B; B -> C; C -> D"
      |> remove_defines ~cols:(cols "C")
      |> Result.ok_exn
    in
    let cmp = fds "A -> B; B -> D" in
    assert_equal_fds ~ctxt fds' cmp

  let split_multikey ctxt =
    let open Fun_dep.Set in
    let fds' =
      fds "A -> B C D; B -> E F"
      |> remove_defines ~cols:(cols "E")
      |> Result.ok_exn
    in
    let cmp = fds "A -> B C D; B -> F" in
    assert_equal_fds ~ctxt fds' cmp

  let suite =
    [ "simple" >:: simple
    ; "remove_intermediary" >:: remove_intermediary
    ; "split_multikey" >:: split_multikey ]
end

module Outputs = struct
  let simple ctxt =
    let open Fun_dep.Set in
    let fds' = fds "A -> B; B -> C; B -> D" in
    let os = outputs fds' in
    assert_equal_cols ~ctxt (cols "B C D") os

  let suite = ["simple" >:: simple]
end

let suite =
  "lens_fd_helpers"
  >::: [ "show_fd_set" >:: test_show_fd_set
       ; "transitive_closure" >:: test_transitive_closure
       ; "changesets"
         >::: ["calculate_fd_changelist" >:: test_calculate_fd_changelist]
       ; "phrase_gen" >::: []
       ; "tree_form" >::: Tree_form.suite
       ; "remove_fd" >::: Remove_fd.suite
       ; "outputs" >::: Outputs.suite ]
