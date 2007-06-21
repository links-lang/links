(*pp deriving *)

open Defs

module Test (D : Dump.Dump) =
struct
  let test v = D.from_string (D.to_string v) = v
end

let sum = begin
  let module T = Test (Dump_sum) in
    assert (T.test S0);
    assert (T.test (S1 max_int));
    assert (T.test (S2 (min_int, 1243.2)));
    assert (T.test (S2 (min_int, 1243.2)));
    assert (T.test (S3 (12, 0.0, true)));
    assert (T.test (Sunit ()));
    assert (T.test (Stup (1001, 10.01)));
end

let r1 = begin
  let module T = Test (Dump_r1) in
    assert (T.test {r1_l1 = max_int - 10; r1_l2 = min_int + 10});
end

let intseq = begin
  let module T = Test (Dump_intseq) in
    assert (T.test INil);
    assert (T.test (ICons (10, ICons (20, ICons (30, INil)))));
end

let seq = begin
  let module T = Test (Dump_seq (Dump.Dump_bool)) in
    assert (T.test Nil);
    assert (T.test (Cons (true, Cons (false, Cons (true, Nil)))));
end

let uses_seqs = begin
  let module T = Test (Dump_uses_seqs) in
    assert (T.test (INil, Nil));
    assert (T.test (INil, Cons (0.0, Cons(10.0, Nil))));
    assert (T.test (ICons (10, ICons(20, INil)), Nil));
    assert (T.test (ICons (10, ICons(20, INil)), 
                    Cons (0.0, Cons(10.0, Nil))));
end

let poly1 = begin
  let module T = Test (Dump_poly1) in
    assert (T.test `T0);
    assert (T.test (`T1 (-1231)));
end

let poly2 = begin
  let module T = Test (Dump_poly2) in
    assert (T.test (P (10, `T1 11, 12.0)));
end

let poly3 = begin
  let module T = Test (Dump_poly3) in
    assert (T.test `Nil);
    assert (T.test (`Cons (1, `Cons (2, `Cons (3, `Nil)))));
end

let poly3b = begin
  let module T = Test (Dump_poly3b) in
    assert (T.test (10, `Nil, `F));
    assert (T.test (0, `Cons (10, `Cons (11, `Cons (12, `Nil))), `F));
end

let poly7 = begin
    let module T  = Test(Dump_poly7(Dump.Dump_bool)) in
    let module T' = Test(Dump_poly8(Dump.Dump_int))  in
      assert (T.test (Foo (`F true)));
      assert (T.test (Foo (`F false)));
      assert (T'.test {x = `G (`H (`I (Foo (`F (max_int - 1)))))});
      assert (T'.test {x = `G (`H (`I (Foo (`F (min_int + 1)))))});
end

let poly10 = begin
  let module T = Test (Dump_poly10) in
    assert (T.test `F);
    assert (T.test `Nil);
    assert (T.test (`Cons (12, `Cons (14, `Nil))));
end

let mutrec = begin
    let module A = Test (Dump_mutrec_a) in
    let module B = Test (Dump_mutrec_b) in
    let module C = Test (Dump_mutrec_c) in
    let module D = Test (Dump_mutrec_d) in
    let a = N in
    let b = { l1 = S (3, a); l2 = a } in
    let c = S (3, S (4, S (5, N))) in
    let d = `T b in
      assert (A.test a);
      assert (B.test b);
      assert (C.test c);
      assert (D.test d);
end

let pmutrec = begin
  (*
    type ('a,'b) pmutrec_a = ('a,'b) pmutrec_c
    and ('a,'b) pmutrec_b = { pl1 : ('a,'b) pmutrec_c ; pl2 : ('a,'b) pmutrec_a }
    and ('a,'b) pmutrec_c = SS of 'a * ('a,'b) pmutrec_a * 'b
    and ('a,'b) pmutrec_d = [`T of ('a,'b) pmutrec_b]
  *)
end

let ff1 = begin
  let module T = Test(Dump_ff1(Dump.Dump_bool)) in    
    assert (T.test (F (true,false)));
    assert (T.test (G 435));
end

let ff2 = begin
  let module T = Test(Dump_ff2(Dump.Dump_bool)(Dump.Dump_int)) in
    assert (T.test (F1 (F2 (Nil, 10, None))));
    assert (T.test (F1 (F2 (Cons (true, Cons (false, Nil)), 10, Some 14))));
end

let tup0 = begin
  let module T = Test (Dump_tup0) in
    assert (T.test ());
end

let tup2 = begin
  let module T = Test (Dump_tup2) in
    assert (T.test (10, 10.0));
    assert (T.test (max_int, -10.0));
end

let tup3 = begin
  let module T = Test (Dump_tup3) in
    assert (T.test (0,12.3,true));
    assert (T.test (min_int,-12.3,false));
end

let tup4 = begin
  let module T = Test (Dump_tup4) in
    assert (T.test (0,0,true,()));
    assert (T.test (min_int,max_int,false,()));
end

let t = begin
  let module T = Test (Dump_t) in
    assert (T.test min_int);
    assert (T.test max_int);
    assert (T.test 10);
end
