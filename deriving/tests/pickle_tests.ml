(*pp deriving *)

open Defs

module Test (S : Pickle.Pickle) =
struct
  let test v = S.E.eq (S.from_string (S.to_string v)) v
end

let sum =
  begin
    let test = let module T = Test(Pickle_sum) in T.test in
      assert (test S0);
      assert (test (S1 3));
      assert (test (S2 (10,2.0)));
      assert (test (Sunit ()));
      assert (test (Stup (10,2.0)));
      assert (test (Stup1 3));
  end

let nullsum = 
  begin
    let test = let module T = Test(Pickle_nullsum) in T.test in
      assert (test N0);
      assert (test N1);
      assert (test N2);
      assert (test N3);
  end

let r1 =
  begin
    let test = let module T = Test(Pickle_r1) in T.test in
      assert (test {r1_l1 = 10; r1_l2 = 20});
      assert (test {r1_l1 = min_int; r1_l2 = max_int});
      assert (test {r1_l1 = max_int; r1_l2 = min_int});
  end

let r2 =
  begin
    let v = { r2_l1 = 10; 
              r2_l2 = 14 } in 
      assert (not (Eq_r2.eq
                     (Pickle_r2.from_string
                        (Pickle_r2.to_string v)) v));
      assert (Pickle_r2.from_string
                (Pickle_r2.to_string v) = v);
  end

let r3 =
  begin
    let v = { r3_l1 = 10; 
              r3_l2 = 14 } in
      assert (not (Eq_r3.eq
                     (Pickle_r3.from_string
                        (Pickle_r3.to_string v)) v));
      assert (Pickle_r3.from_string
                (Pickle_r3.to_string v) = v);
  end

let intseq =
  begin
    let test = let module T = Test(Pickle_intseq) in T.test in
      assert (test INil);
      assert (test (ICons (10, ICons (20, ICons (30, ICons (40, INil))))));
      assert (test (ICons (max_int, ICons (min_int, INil))));
  end

let seq =
  begin
    let test = let module T = Test(Pickle_seq(Pickle.Pickle_bool)) in T.test in
    let test' = let module T = Test(Pickle_seq(Pickle_seq(Pickle.Pickle_bool))) in T.test in

      assert (test Nil);
      assert (test (Cons (false, Cons (true, Cons (false, Nil)))));
      assert (test' Nil);
      assert (test' (Cons (Cons (false, Cons (true, Nil)), 
                           Cons (Cons (true, Cons (false, Nil)),
                                 Nil))));
  end

let uses_seqs =
  begin
    let test = let module T = Test(Pickle_uses_seqs) in T.test in
      assert (test (INil, Nil));
      assert (test (INil, Cons (0.0, Cons(10.0, Nil))));
      assert (test (ICons (10, ICons(20, INil)), Nil));
      assert (test (ICons (10, ICons(20, INil)), 
                    Cons (0.0, Cons(10.0, Nil))));
  end

type permute0 = [`T3 | `T1 | `T2 | `T0] deriving (Typeable, Eq, Pickle)
let poly0 =
  begin
    let test v = Eq_permute0.eq (Pickle_permute0.from_string (Pickle_poly0.to_string v)) v in
      assert (test `T0);
      assert (test `T1);
      assert (test `T2);
      assert (test `T3);
  end

type permute3 = [`Nil | `Cons of int * permute3] deriving (Typeable, Eq, Pickle)
let _ =
  begin
    let test v = Eq_permute3.eq (Pickle_permute3.from_string (Pickle_poly3.to_string v)) v in
      assert (test `Nil);
      assert (test (`Cons (0, `Cons (1, `Cons (2, `Nil)))));
  end

let poly3b =
  begin
    let test = let module T = Test(Pickle_poly3b) in T.test in
      assert (test (10, `Nil, `F));
      assert (test (10, `Cons (10, `Cons (-20, `Nil)), `F));
  end

let _ =
  begin
    let test = let module T = Test(Pickle_poly7(Pickle.Pickle_bool)) in T.test
    and test' = let module T = Test(Pickle_poly8(Pickle.Pickle_int)) in T.test in
      assert (test (Foo (`F true)));
      assert (test (Foo (`F false)));
      assert (test' {x = `G (`H (`I (Foo (`F (max_int - 1)))))});
      assert (test' {x = `G (`H (`I (Foo (`F (min_int + 1)))))});
  end

let _ =
  begin
    let test = let module T = Test(Pickle_poly10) in T.test in    
      assert (test `F);
      assert (test `Nil);
      assert (test (`Cons (12, `Cons (14, `Nil))));
  end

let mutrec =
  begin
    let module A = Test(Pickle_mutrec_a) in
    let module B = Test(Pickle_mutrec_b) in
    let module C = Test(Pickle_mutrec_c) in
    let module D = Test(Pickle_mutrec_d) in
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
    let test = let module T = Test(Pickle_ff1(Pickle.Pickle_bool)) in T.test in    
      assert (test (F (true,false)));
      assert (test (G 435));
  end

let ff2 =
  begin
    let test = let module T = Test(Pickle_ff2(Pickle.Pickle_bool)(Pickle.Pickle_int)) in T.test in
      assert (test (F1 (F2 (Nil, 10, None))));
      assert (test (F1 (F2 (Cons (true, Cons (false, Nil)), 10, Some 14))));
  end

let unit =
  begin
    let test = let module T = Test(Pickle_unit) in T.test in    
      assert (test ());
  end

let tup2 =
  begin
    let test = let module T = Test(Pickle_tup2) in T.test in    
      assert (test (-10,12e4));
      assert (test (max_int,12e4));
  end

let tup3 =
  begin
    let test = let module T = Test(Pickle_tup3) in T.test in    
      assert (test (0,12.3,true));
      assert (test (min_int,-12.3,false));
  end

let tup4 =
  begin
    let test = let module T = Test(Pickle_tup4) in T.test in    
      assert (test (0,0,true,()));
      assert (test (min_int,max_int,false,()));
  end

let withref =
  begin
    let v = WR (10, ref 20) in
      assert (not 
                (Eq_withref.eq (Pickle_withref.from_string 
                                  (Pickle_withref.to_string v)) v));
      assert (Pickle_withref.from_string 
                (Pickle_withref.to_string v) = v);
  end

let t =
  begin
    let test v = Eq_int.eq (Pickle_int.from_string (Pickle_t.to_string v)) v in
      assert (test min_int);
      assert (test max_int);
      assert (test 10);
 end

type refobj = A | B of refobj ref
  deriving (Eq, Typeable, Pickle)

let circular = 
  let s = ref A in
  let r = B s in
     s := r;
    r

let _ = 
  let v = Pickle_refobj.from_string (Pickle_refobj.to_string circular) in
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
} deriving (Eq, Typeable, Pickle)

let circularm =
  let i = {z = 1; x = None; y = None} in
  let j = {z = 2; x = None; y = Some i} in 
    i.x <- Some j;
    i.y <- Some i;
    j.x <- Some j;
    i
  
let _ =
  let v = Pickle_mut.from_string (Pickle_mut.to_string circularm) in
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
    deriving (Eq, Typeable, Pickle)

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
    Pickle_t1.from_string (Pickle_t1.to_string circular_a) in
    ()
