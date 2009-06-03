open Defs

let sum =
  begin
    assert (eq_sum.Eq.eq S0 S0);
    assert (not (eq_sum.Eq.eq S0 (S1 0)));
    assert (eq_sum.Eq.eq (S1 0) (S1 0));
    assert (eq_sum.Eq.eq (Stup (3,0.0)) (Stup (3,0.0)));
    assert (not (eq_sum.Eq.eq (Stup (0,0.0)) (Stup (1,0.0))));
  end

let nullsum =
  begin
    assert (eq_nullsum.Eq.eq N2 N2)
  end

let r1 = 
  begin
    assert (eq_r1.Eq.eq
              { r1_l1 = 10; r1_l2 = 20 }
              { r1_l1 = 10; r1_l2 = 20 });
    assert (not (eq_r1.Eq.eq
                   { r1_l1 = 20; r1_l2 = 10 }
                   { r1_l1 = 10; r1_l2 = 20 }));
  end

let r2 =
  begin
    let l, r = ({ r2_l1 = 10; r2_l2 = 20},
                { r2_l1 = 10; r2_l2 = 20}) in
    assert (eq_r2.Eq.eq l l);
    assert (not (eq_r2.Eq.eq l r));
    assert (not (eq_r2.Eq.eq r l));
  end

let r3 =
  begin
    let l, r = ({ r3_l1 = 10; r3_l2 = 20},
                { r3_l1 = 10; r3_l2 = 20}) in
    assert (eq_r3.Eq.eq l l);
    assert (not (eq_r3.Eq.eq l r));
    assert (not (eq_r3.Eq.eq r l));
  end

let intseq = 
  begin
    assert (eq_intseq.Eq.eq INil INil); 
    assert (eq_intseq.Eq.eq
              (ICons (1,INil))
              (ICons (1,INil)));
    assert (not (eq_intseq.Eq.eq
                   (ICons (1,INil))
                   INil));
    assert (not (eq_intseq.Eq.eq
                   INil
                   (ICons (1,INil))));
    assert (not (eq_intseq.Eq.eq
                   INil
                 (let rec i = ICons(1,i) in i)));
  end

let uses_seqs = 
  begin
    let eq = eq_uses_seqs.Eq.eq in
      assert (eq (INil,Cons(1.0,Nil)) (INil,Cons(1.0,Nil)));
      assert (not (eq (INil,Cons(1.0,Nil)) (INil,Cons(2.0,Nil))));
      assert (not (eq (ICons (1,INil),Nil) (INil,Nil)));
  end

let poly0 =
  begin
    let eq = eq_poly0.Eq.eq in
      assert (eq `T0 `T0);
      assert (not (eq `T1 `T3));
  end

let poly1 = 
  begin
    let eq = eq_poly1.Eq.eq in
      assert (eq `T0 `T0);
      assert (eq (`T1 10) (`T1 10));
      assert (not (eq (`T1 20) (`T1 10)));
      assert (not (eq (`T1 20) `T0));
  end

let poly2 = 
  begin
    let eq = eq_poly2.Eq.eq in
      assert (eq (P (3, `T0, 0.0)) (P (3, `T0, 0.0)));
      assert (eq (P (4, `T1 10, 2.0)) (P (4, `T1 10, 2.0)));
      assert (not (eq (P (5, `T1 10, 2.0)) (P (5, `T0, 2.0))));
      assert (not (eq (P (6, `T0, 2.0)) (P (6, `T0, 10.0))));
      assert (not (eq (P (0, `T0, 2.0)) (P (7, `T0, 2.0))));
  end

let poly3 =
  begin
    let eq = eq_poly3.Eq.eq in
      assert (eq `Nil `Nil);
      assert (eq (`Cons (3,`Nil)) (`Cons (3,`Nil)));
      assert (eq (`Cons (3,`Cons (4,`Nil))) (`Cons (3,`Cons (4,`Nil))));
      assert (not (eq (`Cons (3,`Cons (4,`Nil))) (`Cons (3,`Nil))));
  end

let poly3b = 
  begin
    let eq = eq_poly3b.Eq.eq in
      assert (eq (0,`Nil,`F)  (0,`Nil,`F));
      assert (not (eq (0,`Cons (1,`Nil),`F)  (0,`Nil,`F)));
      assert (not (eq (1,`Nil,`F)  (0,`Nil,`F)));
  end

let poly7_8 = 
  begin
    let m7 = eq_poly7 Eq.eq_int in
    let m8 = eq_poly8 Eq.eq_int in
      assert (m7.Eq.eq (Foo (`F 0)) (Foo (`F 0)));
      assert (not (m7.Eq.eq (Foo (`F 0)) (Foo (`F 1))));
      assert (m8.Eq.eq
                {x = `G (`H (`I (Foo (`F 0))))}
                {x = `G (`H (`I (Foo (`F 0))))});
      assert (not
                (m8.Eq.eq
                   {x = `G (`H (`I (Foo (`F 0))))}
                   {x = `G (`H (`I (Foo (`F 1))))}));
  end

let poly10 = 
  begin
    let eq = eq_poly10.Eq.eq in
      assert (eq `F `F);
      assert (eq `Nil `Nil);
      assert (not (eq `Nil `F));
  end

let mutrec = 
  begin
    let rec cyclic_1 = S (0, cyclic_2)
    and     cyclic_2 = S (1, cyclic_1) in
      assert (not (eq_mutrec_a.Eq.eq cyclic_1 cyclic_2));
      assert (not 
                (eq_mutrec_d.Eq.eq 
                   (`T {l1 = cyclic_1; l2 = cyclic_2})
                   (`T {l1 = cyclic_2; l2 = cyclic_1})));
  end

let pmutrec = 
  begin
    let m_a = eq_pmutrec_a (Eq.eq_int) (Eq.eq_bool) in
    let m_b = eq_pmutrec_b (Eq.eq_int) (Eq.eq_bool) in
    let m_c = eq_pmutrec_c (Eq.eq_int) (Eq.eq_bool) in
    let m_d = eq_pmutrec_d (Eq.eq_int) (Eq.eq_bool) in
    let rec cyclic_1 = SS (0, cyclic_2, true)
    and     cyclic_2 = SS (1, cyclic_1, true) in
      assert (not (m_a.Eq.eq cyclic_1 cyclic_2));
      assert (not 
                (m_d.Eq.eq 
                   (`T {pl1 = cyclic_1; pl2 = cyclic_2})
                   (`T {pl1 = cyclic_2; pl2 = cyclic_1})));
  end


let ff1 =
  begin
    let m = eq_ff1 (Eq.eq_bool) in
      assert (m.Eq.eq (F (true,false)) (F (true,false)));
      assert (m.Eq.eq (G (-1)) (G (-1)));
      assert (not (m.Eq.eq (F (false,true)) (F (true,false))));
      assert (not (m.Eq.eq (G (-1)) (G 0)));
      assert (not (m.Eq.eq (G (-1)) (F (true, true))));
  end

let ff2 = 
  begin
    let m = eq_ff2 (Eq.eq_bool) (Eq.eq_bool) in
      assert (m.Eq.eq
                (F1 (F2 (Cons (true,Nil), 0, None)))
                (F1 (F2 (Cons (true,Nil), 0, None))));

      assert (not (m.Eq.eq
                     (F2 (Nil, 0, None))
                     (F2 (Cons (true,Nil), 0, None))));

      assert (not (m.Eq.eq
                     (F2 (Cons (true,Nil), 0, Some true))
                     (F2 (Cons (true,Nil), 0, Some false))));

      assert (not (m.Eq.eq
                     (F2 (Cons (true,Nil), 0, None))
                     (F2 (Cons (true,Nil), 0, Some false))));
  end

let tup0 =
  begin
    assert (eq_tup0.Eq.eq () ());
  end

let tup2 = 
  begin
    assert (eq_tup2.Eq.eq (10,5.0) (10,5.0));
    assert (not (eq_tup2.Eq.eq (10,5.0) (11,5.0)));
    assert (not (eq_tup2.Eq.eq (10,5.0) (10,4.0)));
  end

let tup3 =
  begin
    assert (eq_tup3.Eq.eq (10,2.5,true) (10,2.5,true));
    assert (not (eq_tup3.Eq.eq (10,2.5,true) (11,2.5,true)));
    assert (not (eq_tup3.Eq.eq (10,2.5,true) (10,2.4,true)));
    assert (not (eq_tup3.Eq.eq (10,2.5,true) (10,2.5,false)));
  end

let tup4 =
  begin
    assert (eq_tup4.Eq.eq (1,2,true,()) (1,2,true,()));
    assert (not (eq_tup4.Eq.eq (1,2,true,()) (0,2,true,())));
    assert (not (eq_tup4.Eq.eq (1,2,true,()) (1,3,true,())));
    assert (not (eq_tup4.Eq.eq (1,2,true,()) (1,2,false,())));
  end

let withref =
  begin
    let x = ref 0 in
      assert (eq_withref.Eq.eq (WR (0,x)) (WR (0,x)));
      assert (not (eq_withref.Eq.eq (WR (0,x)) (WR (0,ref 0))));
  end

let t =
  begin
    assert (eq_t.Eq.eq 0 0);
    assert (eq_t.Eq.eq (-10) (-10));
    assert (eq_t.Eq.eq 14 14);
    assert (not (eq_t.Eq.eq 14 0));
    assert (not (eq_t.Eq.eq 0 14));
    assert (not (eq_t.Eq.eq (-1) 0));
  end
