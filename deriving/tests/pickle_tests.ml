(*pp deriving *)

open Defs

let test (s : 'a Pickle.pickle) =
  fun v -> s.Pickle._Hash.Hash._Eq.Eq.eq (Pickle.from_string s (Pickle.to_string s v)) v


let sum =
  begin
    let test = test pickle_sum in
      assert (test S0);
      assert (test (S1 3));
      assert (test (S2 (10,2.0)));
      assert (test (Sunit ()));
      assert (test (Stup (10,2.0)));
      assert (test (Stup1 3));
  end

let nullsum = 
  begin
    let test = test pickle_nullsum in
      assert (test N0);
      assert (test N1);
      assert (test N2);
      assert (test N3);
  end

let r1 =
  begin
    let test = test pickle_r1 in
      assert (test {r1_l1 = 10; r1_l2 = 20});
      assert (test {r1_l1 = min_int; r1_l2 = max_int});
      assert (test {r1_l1 = max_int; r1_l2 = min_int});
  end

let r2 =
  begin
    let v = { r2_l1 = 10; 
              r2_l2 = 14 } in 
      assert (not (eq_r2.Eq.eq
                     (Pickle.from_string pickle_r2
                        (Pickle.to_string pickle_r2 v)) v));
      assert (Pickle.from_string pickle_r2
                (Pickle.to_string pickle_r2 v) = v);
  end

let r3 =
  begin
    let v = { r3_l1 = 10; 
              r3_l2 = 14 } in
      assert (not (eq_r3.Eq.eq
                     (Pickle.from_string pickle_r3
                        (Pickle.to_string pickle_r3 v)) v));
      assert (Pickle.from_string pickle_r3
                (Pickle.to_string pickle_r3 v) = v);
  end

let intseq =
  begin
    let test = test pickle_intseq in
      assert (test INil);
      assert (test (ICons (10, ICons (20, ICons (30, ICons (40, INil))))));
      assert (test (ICons (max_int, ICons (min_int, INil))));
  end

let seq =
  begin
    let test1 = test (pickle_seq Pickle.pickle_bool) in
    let test2 = test (pickle_seq (pickle_seq Pickle.pickle_bool)) in

      assert (test1 Nil);
      assert (test1 (Cons (false, Cons (true, Cons (false, Nil)))));
      assert (test2 Nil);
      assert (test2 (Cons (Cons (false, Cons (true, Nil)), 
                           Cons (Cons (true, Cons (false, Nil)),
                                 Nil))));
  end

let uses_seqs =
  begin
    let test = test pickle_uses_seqs in
      assert (test (INil, Nil));
      assert (test (INil, Cons (0.0, Cons(10.0, Nil))));
      assert (test (ICons (10, ICons(20, INil)), Nil));
      assert (test (ICons (10, ICons(20, INil)), 
                    Cons (0.0, Cons(10.0, Nil))));
  end

type permute0 = [`T3 | `T1 | `T2 | `T0] deriving (Typeable, Eq, Hash, Pickle)
let poly0 =
  begin
    let test v = eq_permute0.Eq.eq (Pickle.from_string pickle_permute0 (Pickle.to_string pickle_poly0 v)) v in
      assert (test `T0);
      assert (test `T1);
      assert (test `T2);
      assert (test `T3);
  end

type permute3 = [`Nil | `Cons of int * permute3] deriving (Typeable, Eq, Hash, Pickle)
let _ =
  begin
    let test v = eq_permute3.Eq.eq (Pickle.from_string pickle_permute3 (Pickle.to_string pickle_poly3 v)) v in
      assert (test `Nil);
      assert (test (`Cons (0, `Cons (1, `Cons (2, `Nil)))));
  end

let poly3b =
  begin
    let test = test pickle_poly3b in
      assert (test (10, `Nil, `F));
      assert (test (10, `Cons (10, `Cons (-20, `Nil)), `F));
  end

let _ =
  begin
    let test = test (pickle_poly7 Pickle.pickle_bool)
    and test' = test (pickle_poly8 Pickle.pickle_int) in
      assert (test (Foo (`F true)));
      assert (test (Foo (`F false)));
      assert (test' {x = `G (`H (`I (Foo (`F (max_int - 1)))))});
      assert (test' {x = `G (`H (`I (Foo (`F (min_int + 1)))))});
  end

let _ =
  begin
    let test = test pickle_poly10 in
      assert (test `F);
      assert (test `Nil);
      assert (test (`Cons (12, `Cons (14, `Nil))));
  end

let mutrec =
  begin
    let a = test(pickle_mutrec_a) in
    let b = test(pickle_mutrec_b) in
    let c = test(pickle_mutrec_c) in
    let d = test(pickle_mutrec_d) in
    let a' = N in
    let b' = { l1 = S (3, a'); l2 = a' } in
    let c' = S (3, S (4, S (5, N))) in
    let d' = `T b' in
      assert (a a');
      assert (b b');
      assert (c c');
      assert (d d');
  end

let pmutrec =
  begin
 (*     type ('a,'b) pmutrec_a = ('a,'b) pmutrec_c
      and ('a,'b) pmutrec_b = { pl1 : ('a,'b) pmutrec_c ; pl2 : ('a,'b) pmutrec_a }
      and ('a,'b) pmutrec_c = SS of 'a * ('a,'b) pmutrec_a * 'b
      and ('a,'b) pmutrec_d = [`T of ('a,'b) pmutrec_b]*)
  end

let ff1 =
  begin
    let test =  test (pickle_ff1 Pickle.pickle_bool) in
      assert (test (F (true,false)));
      assert (test (G 435));
  end

let ff2 =
  begin
    let test = test (pickle_ff2 Pickle.pickle_bool Pickle.pickle_int) in
      assert (test (F1 (F2 (Nil, 10, None))));
      assert (test (F1 (F2 (Cons (true, Cons (false, Nil)), 10, Some 14))));
  end

let unit =
  begin
    let test = test pickle_unit in    
      assert (test ());
  end

let tup2 =
  begin
    let test = test pickle_tup2 in    
      assert (test (-10,12e4));
      assert (test (max_int,12e4));
  end

let tup3 =
  begin
    let test = test pickle_tup3 in    
      assert (test (0,12.3,true));
      assert (test (min_int,-12.3,false));
  end

let tup4 =
  begin
    let test = test pickle_tup4 in    
      assert (test (0,0,true,()));
      assert (test (min_int,max_int,false,()));
  end

let withref =
  begin
    let v = WR (10, ref 20) in
      assert (not 
                (eq_withref.Eq.eq (Pickle.from_string pickle_withref
                                  (Pickle.to_string pickle_withref v)) v));
      assert (Pickle.from_string pickle_withref
                (Pickle.to_string pickle_withref v) = v);
  end

let t =
  begin
    let test v = eq_int.Eq.eq (Pickle.from_string pickle_int (Pickle.to_string pickle_t v)) v in
      assert (test min_int);
      assert (test max_int);
      assert (test 10);
 end

type refobj = A | B of refobj ref
  deriving (Eq, Hash, Typeable, Pickle)

let circular = 
  let s = ref A in
  let r = B s in
  let () = s := r in
  let v = Pickle.from_string pickle_refobj (Pickle.to_string pickle_refobj r) in
  let (B {contents = 
           B {contents = 
               B {contents = 
                   B {contents = 
                       B {contents = 
                           B {contents = 
                               B {contents = _ }}}}}}}) = v in
    test pickle_refobj r

type mut = {
  mutable x : mut option;
  mutable y : mut option;
  z : int;
} deriving (Eq, Hash, Typeable, Pickle)

let circularm =
  let i = {z = 1; x = None; y = None} in
  let j = {z = 2; x = None; y = Some i} in 
    i.x <- Some j;
    i.y <- Some i;
    j.x <- Some j;
    i
  
let _ =
  let v = Pickle.from_string pickle_mut (Pickle.to_string pickle_mut circularm) in
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
    deriving (Eq, Hash, Typeable, Pickle)

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
    Pickle.from_string pickle_t1 (Pickle.to_string pickle_t1 circular_a) in
    ()
