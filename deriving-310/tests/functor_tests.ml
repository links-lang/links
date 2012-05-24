open Defs

let r1 =
  begin
    let map : r1 -> r1 = Functor_r1.map in
    let x = {r1_l1 = 2; r1_l2 = 12} in

    assert (map x = x);
  end

let intseq = 
  begin
    let map : intseq -> intseq = Functor_intseq.map in
    let i = ICons (0, ICons (1, ICons (2, INil))) in
      assert (map i = i);
  end

let seq =
  begin
    let map =
      let module M : sig val map : ('a -> 'b) -> 'a seq -> 'b seq end
        = struct let map = Functor_seq.map end in M.map in
      assert (map ((+)1) (Cons (1, Cons (2, Cons (3, Cons (4, Nil)))))
              = Cons (2, Cons (3, Cons (4, Cons (5, Nil)))));
  end

let poly7 =
  begin
    let map = 
      let module M : sig val map : ('a -> 'b) -> 'a poly7 -> 'b poly7 end
        = struct let map = Functor_poly7.map end in M.map in
      assert (map ((+)1) (Foo (`F 0)) = Foo (`F 1));
  end

let poly8 = 
  begin
    let map = 
      let module M : sig val map : ('a -> 'b) -> 'a poly8 -> 'b poly8 end
        = struct let map = Functor_poly8.map end in M.map in
      assert (map ((+)1)
                { x = `G (`H (`I (Foo (`F 0))))}
              = { x = `G (`H (`I (Foo (`F 1))))});
  end

let poly10 =
  begin
    let map : poly10 -> poly10 = Functor_poly10.map in
      assert (map `F = `F);
      assert (map (`Cons (1,`Cons (2, `Nil))) = (`Cons (1,`Cons (2, `Nil))));
  end

let pmutrec =
  begin
    let _ =
      let module M : sig val map : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) pmutrec_a -> ('b,'d) pmutrec_a end 
        = struct let map = Functor_pmutrec_a.map end in M.map in
    let _ =
      let module M : sig val map : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) pmutrec_b -> ('b,'d) pmutrec_b end 
        = struct let map = Functor_pmutrec_b.map end in M.map in
    let _ =
      let module M : sig val map : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) pmutrec_c -> ('b,'d) pmutrec_c end 
        = struct let map = Functor_pmutrec_c.map end in M.map in
    let _ =
      let module M : sig val map : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) pmutrec_d -> ('b,'d) pmutrec_d end 
        = struct let map = Functor_pmutrec_d.map end in M.map in
      ()
  end

let ff1 =
  begin
    let map = 
      let module M : sig val map : ('a -> 'b) -> 'a ff1 -> 'b ff1 end
        = struct let map = Functor_ff1.map end in M.map in
      assert (map ((+)1) (F (1,2)) = F (2,3));
      assert (map ((+)1) (G 3) = G 3);
  end

let ff2 = 
  begin
    let map f = 
      let module M : sig val map : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) ff2 -> ('b,'d) ff2 end
        = struct let map = Functor_ff2.map end in M.map f in
      assert (map ((+)1) not (F1 (F2 (Cons (1,Cons (2, Nil)), 3, Some true)))
                = (F1 (F2 (Cons (2,Cons (3, Nil)), 3, Some false))));

      assert (map not ((+)1) (F1 (F2 (Cons (true,Nil), 3, Some 0)))
              =  (F1 (F2 (Cons (false,Nil), 3, Some 1))));
  end
      
(*
type 'a constrained = [`F of 'a] constraint 'a = int
*)

let t =
  begin
    let map : int -> int = Functor_t.map in
      assert (map 12 = 12);
  end
