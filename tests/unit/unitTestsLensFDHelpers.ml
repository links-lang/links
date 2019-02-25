(*pp deriving *)

open UnitTestsLensCommon
open OUnit2

open Links_core
open Types
open Value
open Utility
open CommonTypes

module Fun_dep = Lens.Fun_dep

(* let _ = Settings.set_value Debug.debugging_enabled true *)

(* data *)

let dat_fd_set = LensTestHelpers.fundepset_of_string "A B -> C D; C D -> E; E -> F G"

let dat_cols = LensTestHelpers.colset_of_string "C D"

let dat_closure = LensTestHelpers.colset_of_string "C D E F G"

let dat_fd_set_2 = LensTestHelpers.fundepset_of_string "A -> B; B -> C"

let rec_constr (cols : string list) (vals : int list) = Value.box_record (List.map2 (fun c v -> (c, box_int v)) cols vals)
let delt_constr (cols : string list) (vals, m : int list * int) = rec_constr cols vals, m

(* Tests *)

let test_show_fd_set test_ctx =
  let show = Fun_dep.Set.show dat_fd_set in
  LensTestHelpers.print_verbose test_ctx show;
  let cmp = "{({A; B; }, {C; D; }); ({C; D; }, {E; }); ({E; }, {F; G; }); }" in
  assert_equal show cmp

let test_show_fd_tree test_ctx =
  let tree = Fun_dep.Tree.of_fds dat_fd_set |> OptionUtils.val_of in
  LensTestHelpers.fmt_std_v test_ctx (fun fmt -> Fun_dep.Tree.pp_pretty fmt tree)

let test_transitive_closure _test_ctx =
  let outp = Fun_dep.Set.transitive_closure ~cols:dat_cols dat_fd_set in
  assert_equal true (Lens.Alias.Set.equal outp dat_closure)

let construct_join_lens fd_set name data =
  let cols = Fun_dep.Set.fold (fun fd fld -> Lens.Alias.Set.union_all [Fun_dep.left fd; Fun_dep.right fd; fld]) fd_set Lens.Alias.Set.empty in
  let cols = Lens.Alias.Set.elements cols in
  let colFn tbl name = {
    alias = name;  name = name; table = tbl; typ = `Primitive Primitive.Int; present = true
  } in
  let l1 = `LensMem ((`List data), (fd_set, None, List.map (colFn name) cols)) in
  l1

let construct_join_lens_2 l1 l2 on =
  let sort, on = Lens.Sort.join_lens_sort (Lens.Value.sort l1) (Lens.Value.sort l2) ~on in
  `LensJoin (l1, l2, on, `Constant (`Bool true), `Constant (`Bool false), sort)

let cat_tex cols name delta =
  let cs = List.fold_right (fun _a b -> b ^ "c") cols "" in
  let _ = Debug.print ("\\begin{array}{c|" ^ cs ^ "}") in
  let _ = Debug.print ("\t" ^ name ^
                       (List.fold_left (fun a b -> a ^ " & " ^ b) "" cols )
                       ^ "\\\\") in
  let _ = Debug.print "\t\\hline" in
  let _ = if List.length delta = 0 then
      Debug.print "\\\\"
    else
      let _ = List.map (fun (row, m) -> Debug.print (
          (List.fold_left (fun a (_,b) -> a ^ "& " ^ string_of_int (unbox_int b) ^ " ") ("\t" ^ string_of_int m) (unbox_record row))
          ^ "\\\\")) delta in
      () in
  let _ = Debug.print "\\end{array}" in
  ()

let test_calculate_fd_changelist test_ctx =
  let data = UnitTestsLensSetOperations.test_data_3 in
  let fds = dat_fd_set_2 in
  let changeset = Lens.Sorted_records.calculate_fd_changelist ~fun_deps:fds data in
  let _ = List.map (fun ((cols_l, cols_r),changes) ->
      let _ = LensTestHelpers.print_verbose test_ctx (LensTestHelpers.col_list_to_string cols_l " " ^ " -> " ^ LensTestHelpers.col_list_to_string cols_r " ") in
      let strfn dat = if dat = [] then "" else  List.fold_left (fun a b -> a ^ ", " ^ string_of_value b) (string_of_value (List.hd dat)) (List.tl dat) in
      let _ = List.map (fun (chl, chr) ->
          LensTestHelpers.print_verbose test_ctx ("  " ^ strfn chl ^ " -> " ^ strfn chr)
        ) changes in
      ()
    ) changeset in
  let phrase = LensHelpersIncremental.matches_change changeset in
  let str = match phrase with None -> "None" | Some phrase -> Format.asprintf "%a" Lens.Database.fmt_phrase_dummy phrase in
  LensTestHelpers.print_verbose test_ctx str;
  ()

let suite =
  "lens_fd_helpers">:::
  [
    "show_fd_set">:: test_show_fd_set;
    "show_fd_tree" >:: test_show_fd_tree;
    "transitive_closure">:: test_transitive_closure;
    "changesets">::: [
      "calculate_fd_changelist">::test_calculate_fd_changelist;
    ];
    "phrase_gen">::: [
    ];
  ];;
