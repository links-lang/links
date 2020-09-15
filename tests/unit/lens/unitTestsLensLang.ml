open OUnit2
module Phrase = Lens.Phrase
module GV = Phrase.Grouped_variables

let assert_gtv_equal l1 l2 =
  let cmp_to = GV.of_lists l2 in
  assert_equal ~cmp:Phrase.Grouped_variables.equal l1 cmp_to

let assert_partial_overlaps l1 ~cols v =
  let cols = Lens.Alias.Set.of_list cols in
  let res = GV.has_partial_overlaps l1 ~cols in
  assert_equal res v

let test_gtv_01 _test_ctx =
  let phrase =
    let open Phrase.O in
    v "A" < i 20 && v "B" > i 5
  in
  let grouped = GV.gtv phrase in
  assert_gtv_equal grouped [ [ "A" ]; [ "B" ] ]

let test_gtv_02 _test_ctx =
  let phrase =
    let open Phrase.O in
    v "A" < i 20 || v "B" > i 5
  in
  let grouped = GV.gtv phrase in
  assert_gtv_equal grouped [ [ "A"; "B" ] ]

let test_gtv_03 _test_ctx =
  let phrase =
    let open Phrase.O in
    v "A" < i 20 || i 30 > i 5
  in
  let grouped = GV.gtv phrase in
  assert_gtv_equal grouped [ [ "A" ] ]

let test_partial_overlaps _test_ctx =
  let gtvs = GV.of_lists [ [ "A" ]; [ "A"; "B" ]; [ "C"; "D" ] ] in
  assert_partial_overlaps gtvs ~cols:[ "A" ] true;
  assert_partial_overlaps gtvs ~cols:[ "A"; "B" ] false;
  assert_partial_overlaps gtvs ~cols:[ "B"; "C" ] true;
  assert_partial_overlaps gtvs ~cols:[ "A"; "B"; "C"; "D" ] false

let suite =
  "lang"
  >::: [
         "gtv_01" >:: test_gtv_01;
         "gtv_02" >:: test_gtv_02;
         "gtv_03" >:: test_gtv_03;
         "partial_overlaps_01" >:: test_partial_overlaps;
       ]
