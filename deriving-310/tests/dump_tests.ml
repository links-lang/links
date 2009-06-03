(*pp deriving *)

open Defs

let tester (d : 'a Dump.dump) (v : 'a) = Dump.from_string d (Dump.to_string d v) = v

let sum = begin
  let t = tester (dump_sum) in
    assert (t S0);
    assert (t (S1 max_int));
    assert (t (S2 (min_int, 1243.2)));
    assert (t (S2 (min_int, 1243.2)));
    assert (t (S3 (12, 0.0, true)));
    assert (t (Sunit ()));
    assert (t (Stup (1001, 10.01)));
end

let r1 = begin
  let t = tester (dump_r1) in
    assert (t {r1_l1 = max_int - 10; r1_l2 = min_int + 10});
end

let intseq = begin
  let t = tester (dump_intseq) in
    assert (t INil);
    assert (t (ICons (10, ICons (20, ICons (30, INil)))));
end

let seq = begin
  let t = tester (dump_seq (Dump.dump_bool)) in
    assert (t Nil);
    assert (t (Cons (true, Cons (false, Cons (true, Nil)))));
end

let uses_seqs = begin
  let t = tester (dump_uses_seqs) in
    assert (t (INil, Nil));
    assert (t (INil, Cons (0.0, Cons(10.0, Nil))));
    assert (t (ICons (10, ICons(20, INil)), Nil));
    assert (t (ICons (10, ICons(20, INil)), 
                    Cons (0.0, Cons(10.0, Nil))));
end

let poly1 = begin
  let t = tester (dump_poly1) in
    assert (t `T0);
    assert (t (`T1 (-1231)));
end

let poly2 = begin
  let t = tester (dump_poly2) in
    assert (t (P (10, `T1 11, 12.0)));
end

let poly3 = begin
  let t = tester (dump_poly3) in
    assert (t `Nil);
    assert (t (`Cons (1, `Cons (2, `Cons (3, `Nil)))));
end

let poly3b = begin
  let t = tester (dump_poly3b) in
    assert (t (10, `Nil, `F));
    assert (t (0, `Cons (10, `Cons (11, `Cons (12, `Nil))), `F));
end

let poly7 = begin
    let t  = tester (dump_poly7 (Dump.dump_bool)) in
    let t' = tester (dump_poly8 (Dump.dump_int)) in
      assert (t (Foo (`F true)));
      assert (t (Foo (`F false)));
      assert (t' {x = `G (`H (`I (Foo (`F (max_int - 1)))))});
      assert (t' {x = `G (`H (`I (Foo (`F (min_int + 1)))))});
end

let poly10 = begin
  let t = tester (dump_poly10) in
    assert (t `F);
    assert (t `Nil);
    assert (t (`Cons (12, `Cons (14, `Nil))));
end

let mutrec = begin
  let test_a = tester dump_mutrec_a in
  let test_b = tester (dump_mutrec_b) in
  let test_c = tester (dump_mutrec_c) in
  let test_d = tester (dump_mutrec_d) in
  let a = N in
  let b = { l1 = S (3, a); l2 = a } in
  let c = S (3, S (4, S (5, N))) in
  let d = `T b in
    assert (test_a a);
    assert (test_b b);
    assert (test_c c);
    assert (test_d d);
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
  let t = tester (dump_ff1 (Dump.dump_bool)) in    
    assert (t (F (true,false)));
    assert (t (G 435));
end

let ff2 = begin
  let t = tester (dump_ff2 (Dump.dump_bool) (Dump.dump_int)) in
    assert (t (F1 (F2 (Nil, 10, None))));
    assert (t (F1 (F2 (Cons (true, Cons (false, Nil)), 10, Some 14))));
end

let tup0 = begin
  let t = tester (dump_tup0) in
    assert (t ());
end

let tup2 = begin
  let t = tester (dump_tup2) in
    assert (t (10, 10.0));
    assert (t (max_int, -10.0));
end

let tup3 = begin
  let t = tester (dump_tup3) in
    assert (t (0,12.3,true));
    assert (t (min_int,-12.3,false));
end

let tup4 = begin
  let t = tester (dump_tup4) in
    assert (t (0,0,true,()));
    assert (t (min_int,max_int,false,()));
end

let t = begin
  let t = tester (dump_t) in
    assert (t min_int);
    assert (t max_int);
    assert (t 10);
end
