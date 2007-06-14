(*pp deriving *)

open Defs

module Test (S : Shelve.Shelve) =
struct
  let test v = S.E.eq (S.unshelveS (S.shelveS v)) v
end

let sum =
  begin
    let test = let module T = Test(Shelve_sum) in T.test in
      assert (test S0);
      assert (test (S1 3));
      assert (test (S2 (10,2.0)));
      assert (test (Sunit ()));
      assert (test (Stup (10,2.0)));
      assert (test (Stup1 3));
  end

let nullsum = 
  begin
    let test = let module T = Test(Shelve_nullsum) in T.test in
      assert (test N0);
      assert (test N1);
      assert (test N2);
      assert (test N3);
  end

let r1 =
  begin
    let test = let module T = Test(Shelve_r1) in T.test in
      assert (test {r1_l1 = 10; r1_l2 = 20});
      assert (test {r1_l1 = min_int; r1_l2 = max_int});
      assert (test {r1_l1 = max_int; r1_l2 = min_int});
  end

let r2 =
  begin
    let v = { r2_l1 = 10; 
              r2_l2 = 14 } in 
      assert (not (Eq_r2.eq
                     (Shelve_r2.unshelveS
                        (Shelve_r2.shelveS v)) v));
      assert (Shelve_r2.unshelveS
                (Shelve_r2.shelveS v) = v);
  end

let r3 =
  begin
    let v = { r3_l1 = 10; 
              r3_l2 = 14 } in
      assert (not (Eq_r3.eq
                     (Shelve_r3.unshelveS
                        (Shelve_r3.shelveS v)) v));
      assert (Shelve_r3.unshelveS
                (Shelve_r3.shelveS v) = v);
  end

let intseq =
  begin
    let test = let module T = Test(Shelve_intseq) in T.test in
      assert (test INil);
      assert (test (ICons (10, ICons (20, ICons (30, ICons (40, INil))))));
      assert (test (ICons (max_int, ICons (min_int, INil))));
  end

let seq =
  begin
    let test = let module T = Test(Shelve_seq(Shelve.Shelve_bool)) in T.test in
    let test' = let module T = Test(Shelve_seq(Shelve_seq(Shelve.Shelve_bool))) in T.test in

      assert (test Nil);
      assert (test (Cons (false, Cons (true, Cons (false, Nil)))));
      assert (test' Nil);
      assert (test' (Cons (Cons (false, Cons (true, Nil)), 
                           Cons (Cons (true, Cons (false, Nil)),
                                 Nil))));
  end

let uses_seqs =
  begin
    let test = let module T = Test(Shelve_uses_seqs) in T.test in
      assert (test (INil, Nil));
      assert (test (INil, Cons (0.0, Cons(10.0, Nil))));
      assert (test (ICons (10, ICons(20, INil)), Nil));
      assert (test (ICons (10, ICons(20, INil)), 
                    Cons (0.0, Cons(10.0, Nil))));
  end

type permute0 = [`T3 | `T1 | `T2 | `T0] deriving (Typeable, Eq, Shelve)
let poly0 =
  begin
    let test v = Eq_permute0.eq (Shelve_permute0.unshelveS (Shelve_poly0.shelveS v)) v in
      assert (test `T0);
      assert (test `T1);
      assert (test `T2);
      assert (test `T3);
  end

type permute3 = [`Nil | `Cons of int * permute3] deriving (Typeable, Eq, Shelve)
let _ =
  begin
    let test v = Eq_permute3.eq (Shelve_permute3.unshelveS (Shelve_poly3.shelveS v)) v in
      assert (test `Nil);
      assert (test (`Cons (0, `Cons (1, `Cons (2, `Nil)))));
  end

let poly3b =
  begin
    let test = let module T = Test(Shelve_poly3b) in T.test in
      assert (test (10, `Nil, `F));
      assert (test (10, `Cons (10, `Cons (-20, `Nil)), `F));
  end

let _ =
  begin
    let test = let module T = Test(Shelve_poly7(Shelve.Shelve_bool)) in T.test
    and test' = let module T = Test(Shelve_poly8(Shelve.Shelve_int)) in T.test in
      assert (test (Foo (`F true)));
      assert (test (Foo (`F false)));
      assert (test' {x = `G (`H (`I (Foo (`F (max_int - 1)))))});
      assert (test' {x = `G (`H (`I (Foo (`F (min_int + 1)))))});
  end

let _ =
  begin
    let test = let module T = Test(Shelve_poly10) in T.test in    
      assert (test `F);
      assert (test `Nil);
      assert (test (`Cons (12, `Cons (14, `Nil))));
  end

let mutrec =
  begin
    let module A = Test(Shelve_mutrec_a) in
    let module B = Test(Shelve_mutrec_b) in
    let module C = Test(Shelve_mutrec_c) in
    let module D = Test(Shelve_mutrec_d) in
    let a = N in
    let b = { l1 = S (3, a); l2 = a } in
    let c = S (3, S (4, S (5, N))) in
    let d = `T b in
      assert (A.test a);
      assert (B.test b);
      assert (C.test c);
      assert (D.test d);
  end

let pmutrec =
  begin
    (*
      type ('a,'b) pmutrec_a = ('a,'b) pmutrec_c
      and ('a,'b) pmutrec_b = { pl1 : ('a,'b) pmutrec_c ; pl2 : ('a,'b) pmutrec_a }
      and ('a,'b) pmutrec_c = SS of 'a * ('a,'b) pmutrec_a * 'b
      and ('a,'b) pmutrec_d = [`T of ('a,'b) pmutrec_b]
    *)
  end

let ff1 =
  begin
    let test = let module T = Test(Shelve_ff1(Shelve.Shelve_bool)) in T.test in    
      assert (test (F (true,false)));
      assert (test (G 435));
  end

let ff2 =
  begin
    let test = let module T = Test(Shelve_ff2(Shelve.Shelve_bool)(Shelve.Shelve_int)) in T.test in
      assert (test (F1 (F2 (Nil, 10, None))));
      assert (test (F1 (F2 (Cons (true, Cons (false, Nil)), 10, Some 14))));
  end

let unit =
  begin
    let test = let module T = Test(Shelve_unit) in T.test in    
      assert (test ());
  end

let tup2 =
  begin
    let test = let module T = Test(Shelve_tup2) in T.test in    
      assert (test (-10,12e4));
      assert (test (max_int,12e4));
  end

let tup3 =
  begin
    let test = let module T = Test(Shelve_tup3) in T.test in    
      assert (test (0,12.3,true));
      assert (test (min_int,-12.3,false));
  end

let tup4 =
  begin
    let test = let module T = Test(Shelve_tup4) in T.test in    
      assert (test (0,0,true,()));
      assert (test (min_int,max_int,false,()));
  end

let withref =
  begin
    let v = WR (10, ref 20) in
      assert (not 
                (Eq_withref.eq (Shelve_withref.unshelveS 
                                  (Shelve_withref.shelveS v)) v));
      assert (Shelve_withref.unshelveS 
                (Shelve_withref.shelveS v) = v);
  end

let t =
  begin
    let test v = Eq_int.eq (Shelve_int.unshelveS (Shelve_t.shelveS v)) v in
      assert (test min_int);
      assert (test max_int);
      assert (test 10);
 end

type refobj = A | B of refobj ref
  deriving (Eq, Typeable, Shelve)

let circular = 
  let s = ref A in
  let r = B s in
     s := r;
    r

let _ = 
  let v = Shelve_refobj.unshelveS (Shelve_refobj.shelveS circular) in
  let (B {contents = 
           B {contents = 
               B {contents = 
                   B {contents = 
                       B {contents = 
                           B {contents = 
                               B {contents = _ }}}}}}}) = v in
    ()
    

type mut = {
  mutable x : mut option;
  mutable y : mut option;
  z : int;
} deriving (Eq, Typeable, Shelve)

let circularm =
  let i = {z = 1; x = None; y = None} in
  let j = {z = 2; x = None; y = Some i} in 
    i.x <- Some j;
    i.y <- Some i;
    j.x <- Some j;
    i
  
let _ =
  let v = Shelve_mut.unshelveS (Shelve_mut.shelveS circularm) in
  let {z = 1; 
       x = Some {z = 2; x = Some {z = 2; 
                                  x = Some _; 
                                  y = Some _}; 
                 y = Some _};
       y = Some {z = 1; 
                 x = Some {z = 2; x = Some {z = 2; 
                                            x = Some {z = 2; 
                                                      x = Some _; 
                                                      y = Some _}; 
                                            y = Some _}; 
                       y = Some _};
                 y = Some _}} = v in 
    ()

type t1 = { mutable x : t2 option }
and  t2 = { y : t1 option }
    deriving (Eq, Typeable, Shelve)

let circular_a = 
  let a = { x = None } in
  let b = { y = Some a } in
    a.x <- Some b;
    a

let _ =
  let {x = Some {y = Some
           {x = Some {y = Some
                {x = Some {y = Some
                     {x = Some {y = Some _}}}}}}}} = 
    Shelve_t1.unshelveS (Shelve_t1.shelveS circular_a) in
    ()
