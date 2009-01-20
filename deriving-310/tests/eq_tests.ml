open Defs

let sum =
  begin
    assert (Eq_sum.eq S0 S0);
    assert (not (Eq_sum.eq S0 (S1 0)));
    assert (Eq_sum.eq (S1 0) (S1 0));
    assert (Eq_sum.eq (Stup (3,0.0)) (Stup (3,0.0)));
    assert (not (Eq_sum.eq (Stup (0,0.0)) (Stup (1,0.0))));
  end

let nullsum =
  begin
    assert (Eq_nullsum.eq N2 N2)
  end

let r1 = 
  begin
    assert (Eq_r1.eq
              { r1_l1 = 10; r1_l2 = 20 }
              { r1_l1 = 10; r1_l2 = 20 });
    assert (not (Eq_r1.eq
                   { r1_l1 = 20; r1_l2 = 10 }
                   { r1_l1 = 10; r1_l2 = 20 }));
  end

let r2 =
  begin
    let l, r = ({ r2_l1 = 10; r2_l2 = 20},
                { r2_l1 = 10; r2_l2 = 20}) in
    assert (Eq_r2.eq l l);
    assert (not (Eq_r2.eq l r));
    assert (not (Eq_r2.eq r l));
  end

let r3 =
  begin
    let l, r = ({ r3_l1 = 10; r3_l2 = 20},
                { r3_l1 = 10; r3_l2 = 20}) in
    assert (Eq_r3.eq l l);
    assert (not (Eq_r3.eq l r));
    assert (not (Eq_r3.eq r l));
  end

let intseq = 
  begin
    assert (Eq_intseq.eq INil INil); 
    assert (Eq_intseq.eq
              (ICons (1,INil))
              (ICons (1,INil)));
    assert (not (Eq_intseq.eq
                   (ICons (1,INil))
                   INil));
    assert (not (Eq_intseq.eq
                   INil
                   (ICons (1,INil))));
    assert (not (Eq_intseq.eq
                   INil
                 (let rec i = ICons(1,i) in i)));
  end

let uses_seqs = 
  begin
    let eq = Eq_uses_seqs.eq in
      assert (eq (INil,Cons(1.0,Nil)) (INil,Cons(1.0,Nil)));
      assert (not (eq (INil,Cons(1.0,Nil)) (INil,Cons(2.0,Nil))));
      assert (not (eq (ICons (1,INil),Nil) (INil,Nil)));
  end

let poly0 =
  begin
    let eq = Eq_poly0.eq in
      assert (eq `T0 `T0);
      assert (not (eq `T1 `T3));
  end

let poly1 = 
  begin
    let eq = Eq_poly1.eq in
      assert (eq `T0 `T0);
      assert (eq (`T1 10) (`T1 10));
      assert (not (eq (`T1 20) (`T1 10)));
      assert (not (eq (`T1 20) `T0));
  end

let poly2 = 
  begin
    let eq = Eq_poly2.eq in
      assert (eq (P (3, `T0, 0.0)) (P (3, `T0, 0.0)));
      assert (eq (P (4, `T1 10, 2.0)) (P (4, `T1 10, 2.0)));
      assert (not (eq (P (5, `T1 10, 2.0)) (P (5, `T0, 2.0))));
      assert (not (eq (P (6, `T0, 2.0)) (P (6, `T0, 10.0))));
      assert (not (eq (P (0, `T0, 2.0)) (P (7, `T0, 2.0))));
  end


let poly3 =
  begin
    let eq = Eq_poly3.eq in
      assert (eq `Nil `Nil);
      assert (eq (`Cons (3,`Nil)) (`Cons (3,`Nil)));
      assert (eq (`Cons (3,`Cons (4,`Nil))) (`Cons (3,`Cons (4,`Nil))));
      assert (not (eq (`Cons (3,`Cons (4,`Nil))) (`Cons (3,`Nil))));
  end

let poly3b = 
  begin
    let eq = Eq_poly3b.eq in
      assert (eq (0,`Nil,`F)  (0,`Nil,`F));
      assert (not (eq (0,`Cons (1,`Nil),`F)  (0,`Nil,`F)));
      assert (not (eq (1,`Nil,`F)  (0,`Nil,`F)));
  end


let poly7_8 = 
  begin
    let module M7 = Eq_poly7(Eq.Eq_int) in
    let module M8 = Eq_poly8(Eq.Eq_int) in
      assert (M7.eq (Foo (`F 0)) (Foo (`F 0)));
      assert (not (M7.eq (Foo (`F 0)) (Foo (`F 1))));
      assert (M8.eq
                {x = `G (`H (`I (Foo (`F 0))))}
                {x = `G (`H (`I (Foo (`F 0))))});
      assert (not
                (M8.eq
                   {x = `G (`H (`I (Foo (`F 0))))}
                   {x = `G (`H (`I (Foo (`F 1))))}));
  end

let poly10 = 
  begin
    let eq = Eq_poly10.eq in
      assert (eq `F `F);
      assert (eq `Nil `Nil);
      assert (not (eq `Nil `F));
  end

let mutrec = 
  begin
    let rec cyclic_1 = S (0, cyclic_2)
    and     cyclic_2 = S (1, cyclic_1) in
      assert (not (Eq_mutrec_a.eq cyclic_1 cyclic_2));
      assert (not 
                (Eq_mutrec_d.eq 
                   (`T {l1 = cyclic_1; l2 = cyclic_2})
                   (`T {l1 = cyclic_2; l2 = cyclic_1})));
  end

let pmutrec = 
  begin
    let module M_a = Eq_pmutrec_a(Eq.Eq_int)(Eq.Eq_bool) in
    let module M_b = Eq_pmutrec_b(Eq.Eq_int)(Eq.Eq_bool) in
    let module M_c = Eq_pmutrec_c(Eq.Eq_int)(Eq.Eq_bool) in
    let module M_d = Eq_pmutrec_d(Eq.Eq_int)(Eq.Eq_bool) in
    
    let rec cyclic_1 = SS (0, cyclic_2, true)
    and     cyclic_2 = SS (1, cyclic_1, true) in
      assert (not (M_a.eq cyclic_1 cyclic_2));
      assert (not 
                (M_d.eq 
                   (`T {pl1 = cyclic_1; pl2 = cyclic_2})
                   (`T {pl1 = cyclic_2; pl2 = cyclic_1})));
  end


let ff1 =
  begin
    let module M = Eq_ff1(Eq.Eq_bool) in
      assert (M.eq (F (true,false)) (F (true,false)));
      assert (M.eq (G (-1)) (G (-1)));
      assert (not (M.eq (F (false,true)) (F (true,false))));
      assert (not (M.eq (G (-1)) (G 0)));
      assert (not (M.eq (G (-1)) (F (true, true))));
  end

let ff2 = 
  begin
    let module M = Eq_ff2(Eq.Eq_bool)(Eq.Eq_bool) in
      assert (M.eq
                (F1 (F2 (Cons (true,Nil), 0, None)))
                (F1 (F2 (Cons (true,Nil), 0, None))));

      assert (not (M.eq
                     (F2 (Nil, 0, None))
                     (F2 (Cons (true,Nil), 0, None))));

      assert (not (M.eq
                     (F2 (Cons (true,Nil), 0, Some true))
                     (F2 (Cons (true,Nil), 0, Some false))));

      assert (not (M.eq
                     (F2 (Cons (true,Nil), 0, None))
                     (F2 (Cons (true,Nil), 0, Some false))));
  end

let tup0 =
  begin
    assert (Eq_tup0.eq () ());
  end

let tup2 = 
  begin
    assert (Eq_tup2.eq (10,5.0) (10,5.0));
    assert (not (Eq_tup2.eq (10,5.0) (11,5.0)));
    assert (not (Eq_tup2.eq (10,5.0) (10,4.0)));
  end

let tup3 =
  begin
    assert (Eq_tup3.eq (10,2.5,true) (10,2.5,true));
    assert (not (Eq_tup3.eq (10,2.5,true) (11,2.5,true)));
    assert (not (Eq_tup3.eq (10,2.5,true) (10,2.4,true)));
    assert (not (Eq_tup3.eq (10,2.5,true) (10,2.5,false)));
  end

let tup4 =
  begin
    assert (Eq_tup4.eq (1,2,true,()) (1,2,true,()));
    assert (not (Eq_tup4.eq (1,2,true,()) (0,2,true,())));
    assert (not (Eq_tup4.eq (1,2,true,()) (1,3,true,())));
    assert (not (Eq_tup4.eq (1,2,true,()) (1,2,false,())));
  end

let withref =
  begin
    let x = ref 0 in
      assert (Eq_withref.eq (WR (0,x)) (WR (0,x)));
      assert (not (Eq_withref.eq (WR (0,x)) (WR (0,ref 0))));
  end

let t =
  begin
    assert (Eq_t.eq 0 0);
    assert (Eq_t.eq (-10) (-10));
    assert (Eq_t.eq 14 14);
    assert (not (Eq_t.eq 14 0));
    assert (not (Eq_t.eq 0 14));
    assert (not (Eq_t.eq (-1) 0));
  end
