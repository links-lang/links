(* Complete OCaml implementation of QuickCheck 
   (See

      @InProceedings{quickcheck:00,
        author =       {Koen Claessen and John Hughes},
        title =        {QuickCheck: A Lightweight Tool for Random Testing of Haskell Programs},
        booktitle =    {ICFP},
        year =         2000,
        address =      {Montreal, Canada},
        organization = {ACM}
      }
   
   or http://www.cs.chalmers.se/~rjmh/QuickCheck/)
*)

open Monad
open Functor

(* Mostly, this is not polymorphic enough. 
   Everywhere that `prop' appears in the 
   quickcheck module interface we want (Testable a)
*)

module type RandomGen = 
sig
  type g 

  (* The split operation allows one to obtain two independent random
     number generators. This is very useful in functional programs (for
     example, when passing a random number generator down to recursive
     calls), but very little work has been done on statistically robust
     implementations of split ([1,4] are the only examples we know of).
     
     The result of repeatedly using next should be at least as
     statistically robust as the "Minimal Standard Random Number
     Generator" described by [2,3]. Until more is known about
     implementations of split, all we require is that split deliver
     generators that are (a) not identical and (b) independently
     robust in the sense just given.  *)
  val split : g -> (g * g)

  (* The next operation returns an Int that is uniformly distributed
     in the range returned by genRange (including both end points),
     and a new generator. *)
  val next : g -> (int * g)
end

module type Random = 
sig 
  type a 
  module RandomR  (RandomGen : RandomGen) :
  sig
    (* randomR takes a range (lo,hi) and a random number generator g,
       and returns a random value uniformly distributed in the closed
       interval [lo,hi], together with a new generator. It is unspecified
       what happens if lo>hi. For continuous types there is no
       requirement that the values lo and hi are ever produced, but they
       may be, depending on the implementation and the interval. *)
    val randomR : (a * a) -> RandomGen.g -> (a * RandomGen.g)
  end
end

module Random_int : Random with type a = int  =
struct
  type a = int 
  let max_int = Primitives.Bounded_int.maxBound
  let min_int = Primitives.Bounded_int.minBound
  module RandomR (RandomGen : RandomGen) =
  struct
    open Big_int
    let randomR (lo,hi) g = 
      if lo = hi then 
        lo, snd (RandomGen.next g)
      else 
      let v, g = RandomGen.next g in
        (* Do all arithmetic in Big_int to avoid overflow *)
      let loBig = big_int_of_int lo
      and hiBig = big_int_of_int hi
      and vBig  = big_int_of_int v in


      let v' = int_of_big_int 
        (try

(sub_big_int (mod_big_int vBig (sub_big_int hiBig loBig)) loBig)
      with Division_by_zero -> 
        failwith "division by zero doing randomR")

      in
        (v',g)
    end
end

type stdgen = Random.State.t

module RandomGen_stdgen : RandomGen with type g = stdgen =
struct
  type g = stdgen
  let max_int = Primitives.Bounded_int.maxBound
  let top = Int64.shift_left (Int64.of_int max_int) 1
  let next g = 
    let g' = Random.State.copy g in
    let v = Int64.sub (Random.State.int64 g' top) (Int64.of_int max_int) in
      (Int64.to_int v, g')
  let split g = 
    let g' = Random.State.copy g in
    let bits = Random.State.bits g' in
        let l = Random.State.make [|bits|] in
        let r = Random.State.make [|lnot bits|] in
          (l, r)
end

let unlines = function
  | [] -> ""
  | l -> (String.concat "\n" l) ^ "\n" (* should add trailing "\n" *)
let sum = List.fold_left (+) 0

let span (p : 'a -> bool) : 'a list -> ('a list * 'a list) =
  let rec span = function
    | x::xs' when p x -> let ys, zs = span xs' in x::ys, zs
    | xs              -> [], xs in
    span
      
let groupBy eq : 'a list -> 'a list list = 
  let rec group = function
    | [] -> []
    | x::xs -> (let ys, zs = span (eq x) xs in 
                  (x::ys)::group zs) 
  in group

let group = groupBy (=)

 
(* (This isn't right: newStdGen should return a new, differently-seeded
   generator each time it's called):

   newStdGen applies split to the current global random generator,
   updates it with one of the results, and returns the other. *)
let newStdGen : stdgen IO.m = IO.mkIO (fun () -> Random.State.make [|0|])

module QuickCheck :
sig
  (* Types *)
  type config
  type property
  type result
  type 'a gen (* Functor, Monad *)
      
  (* instance decls *)
  module Monad_gen         : Monad with type 'a m = 'a gen
  module Functor_gen       : Functor with type 'a f = 'a gen

  (* Classes *)
  module type Arbitrary = 
  sig 
    type a 
    val arbitrary          : a gen
    val coarbitrary        : a -> 'b gen -> 'b gen
  end
  module type Testable = 
  sig
    type a
    val property           : a -> property
  end

  module Testable_unit : Testable with type a = unit
  module Testable_bool : Testable with type a = bool
  module Testable_result : Testable with type a = result
  module Testable_property : Testable with type a = property
  module Testable_function (Arbitrary : Arbitrary)(Show : Show.Show with type a = Arbitrary.a)(Testable : Testable) : Testable
    with type a = Arbitrary.a -> Testable.a


  module Property_utils (Testable : Testable) : sig
  val quickCheck           : Testable.a -> unit IO.m
  val verboseCheck         : Testable.a -> unit IO.m
  val test                 : Testable.a -> unit IO.m  (* = quickCheck *)
  val check                : config -> Testable.a -> unit IO.m
 
  (* property combinators *)
(*
  val forAll               : 'a gen -> ('a -> Testable.a) -> Testable.a
*)
  module ForAll (Show : Show.Show) : sig val forAll : Show.a gen -> (Show.a -> Testable.a) -> property end


  val (==>)                : bool -> Testable.a -> property
  
  (* gathering test-case information *)
  val label                : string         -> Testable.a -> property
  module Collect (Show     : Show.Show) :
    sig
      val collect          : Show.a -> Testable.a -> property
    end
  val classify             : bool -> string -> Testable.a -> property
  val trivial              : bool           -> Testable.a -> property
  end

  (* generator combinators *)
  val elements             : 'a list -> 'a gen
  val two                  : 'a gen -> ('a * 'a) gen
  val three                : 'a gen -> ('a * 'a * 'a) gen
  val four                 : 'a gen -> ('a * 'a * 'a * 'a) gen
  
  val sized                : (int -> 'a gen) -> 'a gen
  val resize               : int -> 'a gen -> 'a gen
  module Choose (Random    : Random) :
  sig 
    val choose             : (Random.a * Random.a) -> Random.a gen
  end
  val oneof                : 'a gen list -> 'a gen
  val frequency            : (int * 'a gen) list -> 'a gen
  module Vector (Arbitrary : Arbitrary) :
  sig
    val vector             : int -> Arbitrary.a list gen
  end

  (* default generators *)
  val rand                 : stdgen gen
  val promote              : ('a -> 'b gen) -> ('a -> 'b) gen
  val variant              : int -> 'a gen -> 'a gen
end
 = 
struct
  type config = { maxTest : int;
                  maxFail : int;
                  size    : int -> int;
                  every   : int -> string list -> string }
  type property = Prop of (result gen)
  and 'a gen = Gen of (int -> stdgen -> 'a)
  and result = { ok : bool option;
                 stamp : string list;
                 arguments : string list}

  module Monad_gen : Monad.Monad with type 'a m = 'a gen = 
    Monad.MonadDefault(
      struct
        type 'a m = 'a gen
        let return a = Gen (fun _ _ -> a)
        let (>>=) (Gen m) k = 
          Gen (fun n r0 -> let (r1, r2) = RandomGen_stdgen.split r0 in
                           let Gen m' = k (m n r1)
                           in m' n r2)
        let fail = failwith
      end)
  module Monad_gen_utils = MonadUtils(Monad_gen)
  module Functor_gen = MonadFunctor(Monad_gen)

  let quick : config = 
  { maxTest = 100;
    maxFail = 1000;
    size    = (fun s -> (s / 2) + 3);
    every   = fun n _(*args*) -> let s = string_of_int n in s ^ String.make (String.length s) '\b'
  }
  let verbose : config = 
    { quick with every = fun n args -> string_of_int n ^ ":\n" ^ unlines args }


  (* Classes *)
  module type Arbitrary = 
  sig 
    type a 
    val arbitrary : a gen
    val coarbitrary : a -> 'b gen -> 'b gen
  end
  module type Testable = 
  sig
    type a
    val property : a -> property
  end

  let result : result -> property =
    fun res -> Prop (Monad_gen.return res)
  let nothing = { ok = None; stamp = []; arguments = [] }

  module Testable_utils (Testable : Testable) =
  struct
    let evaluate a = let Prop gen = Testable.property a in gen

    let label s a = 
      let add res = {res with stamp = s :: res.stamp}
      in 
        Prop (Functor_gen.map add (evaluate a))
    
    let classify l name = 
      match l with
        | true -> label name
        | false -> Testable.property
    let trivial x = classify x "trivial"
  end

    (*forAll :: (Show a, Testable b) => Gen a -> (a -> b) -> Property*)
  module ForAll' (Show : Show.Show) (Testable : Testable) =
  struct
    open Monad_gen
    module Evaluate = Testable_utils(Testable)
    let forAll : Show.a gen -> (Show.a -> Testable.a) -> property
        = fun gen body -> Prop (
        let argument a res = { res with arguments = Show.show a :: res.arguments } in
          gen >>= fun a ->
          Evaluate.evaluate (body a) >>= fun res ->
          return (argument a res))
  end

  module Testable_unit : Testable with type a = unit =
  struct
    type a = unit
    let property _ = (result nothing : property)
  end
  module Testable_bool : Testable with type a = bool =
  struct
    type a = bool
    let property b = result {nothing with ok = Some b}
  end
  module Testable_result : Testable with type a = result =
  struct
    type a = result
    let property rss = result rss
  end
  module Testable_property : Testable with type a = property =
  struct
    type a = property
    let property prop = prop
  end
  module Testable_function 
    (Arbitrary : Arbitrary)
    (Show : Show.Show with type a = Arbitrary.a)
    (Testable : Testable) : Testable
    with type a = Arbitrary.a -> Testable.a =
  struct
    module F = ForAll' (Show) (Testable)

    type a = Arbitrary.a -> Testable.a
    let property : (Arbitrary.a -> Testable.a) -> property
        = fun f -> F.forAll Arbitrary.arbitrary f
  end

  (*   default generators *)
  let promote f = Gen (fun n r -> fun a -> let Gen m = f a in m n r)
  let variant v (Gen m) = 
(*    let rec rands r0 = 
      (* BUG: this must be a stream, or process. Eager evaluation just loops here. *)
      let (r1, r2) = RandomGen_stdgen.split r0 in
        r1 :: rands r2
*)
    let rec rands r0 n = 
      if n = 0 then (let (r1, _) = RandomGen_stdgen.split r0 in
                       r1)
      else 
        let (_, r2) = RandomGen_stdgen.split r0 in
          rands r2 (n - 1)
    in
(*      Gen (fun n r -> m n (List.nth (rands r) (v+1)))*)
      Gen (fun n r -> m n (rands r (v+1)))

  module Check (Testable : Testable) =
  struct
    open IO
    open Monad_IO
    module M = Testable_utils(Testable)
    open M

    let done_ : string -> int -> string list list -> unit IO.m 
      = fun mesg ntest stamps ->
        let (-<-) f g x = f (g x) in
        let rec display = function
          | []  -> ".\n"
          | [x] -> " (" ^ x ^ ").\n"
          | xs  -> ".\n" ^ unlines (List.map (fun s -> s ^ ".") xs)
        and pairLength = function
          | ((xs::_) as xss) -> (List.length xss, xs) 
          | _ -> assert false
        and entry (n, xs)         = percentage n ntest ^ " " ^ (String.concat ", " xs)
        and percentage n m        = string_of_int ((100 * n) / m) ^ "%" in
        let table = (display
                      -<- List.map entry
                      -<- List.rev
                      -<- List.sort compare
                      -<- List.map pairLength
                      -<- group
                      -<- List.sort compare
                      -<- List.filter (not -<- ((=)[]))) stamps
          in
        putStr (mesg ^ " " ^ string_of_int ntest ^ " tests" ^ table)

    let generate : int -> stdgen -> 'a gen -> 'a
      = fun n rnd (Gen m) -> 
        let module R = Random_int.RandomR(RandomGen_stdgen) in
        let (size, rnd') = R.randomR (0, n) rnd in
          m size rnd'
              
    let rec tests : config -> result gen -> stdgen -> int -> int -> string list list -> unit IO.m
      = fun config gen rnd0 ntest nfail stamps ->
      if ntest == config.maxTest then done_ "OK, passed" ntest stamps
      else if nfail == config.maxFail then done_ "Arguments exhausted after" ntest stamps
      else 
        let (rnd1,rnd2) = RandomGen_stdgen.split rnd0 in
        let result      = generate (config.size ntest) rnd2 gen in
          putStr (config.every ntest result.arguments) >>
            match result.ok with
              | None -> tests config gen rnd1 ntest (nfail+1) stamps
              | Some true -> tests config gen rnd1 (ntest+1) nfail (result.stamp::stamps)
              | Some false ->
                  putStr ("Falsifiable, after "
                           ^ string_of_int ntest
                           ^ " tests:\n"
                           ^ unlines result.arguments)

    let check : config -> Testable.a -> unit IO.m
      = fun config a ->
        newStdGen >>= fun rnd ->
        tests config (evaluate a) rnd 0 0 []
  end

  module Property_utils (Testable : Testable) = struct

    let rec check = let module C = Check(Testable) in C.check
    and quickCheck p = check quick p
    and verboseCheck p = check verbose p
    and test p = check quick p
    module ForAll (Show : Show.Show) = ForAll' (Show) (Testable)

    let (==>) b a = 
      if b then Testable.property a
      else Testable_unit.property ()

  (* gathering test-case information *)
  include Testable_utils(Testable)

  module Collect' (Show : Show.Show) (Testable : Testable) = 
  struct
    let collect v = 
      let module T = Testable_utils(Testable) in
        T.label (Show.show v)
  end
  
  module Collect (Show : Show.Show) = Collect'(Show)(Testable)

  end



  (* generator combinators *)
  let rand = Gen (fun _ r -> r)

  module Choose (Random : Random) =
  struct
    let choose : (Random.a * Random.a) -> Random.a gen
      = let module RandomR = Random.RandomR(RandomGen_stdgen) in
          fun bounds -> 
            Functor_gen.map
              (fun x -> (fst (RandomR.randomR bounds x))) rand
  end
  let elements xs = 
    let module ChooseInt = Choose (Random_int) in
      Functor_gen.map (List.nth xs) (ChooseInt.choose (0, List.length xs - 1))
  let two m = Monad_gen_utils.liftM2 (fun x y -> (x,y)) m m
  let three m = Monad_gen_utils.liftM3 (fun x y z -> (x,y,z)) m m m
  let four m =  Monad_gen_utils.liftM4 (fun w x y z -> (w,x,y,z)) m m m m
  
  let sized fgen = Gen (fun n r -> let Gen m = fgen n in m n r)
  let resize n (Gen m) = Gen (fun _ r -> m n r)
  let oneof gens = Monad_gen.(>>=) (elements gens) (fun x -> x)
  let frequency xs = 
    let rec pick n = function
      | ((k,x)::xs) ->
          if n <= k then x
          else pick (n-k) xs 
      | _ -> assert false in
    let tot = sum (List.map fst xs)
    in
    let module ChooseInt = Choose (Random_int) in
      Monad_gen.(>>=) (ChooseInt.choose (1, tot)) (fun x -> pick x xs)

  module Vector (Arbitrary : Arbitrary) =
  struct
    let vector : int -> Arbitrary.a list gen
      = fun n -> Monad_gen_utils.sequence (List.map (fun _ -> Arbitrary.arbitrary)
                                              (Primitives.Enum_int.enumFromTo 1 n))
  end
end

open QuickCheck
module Monad_gen_utils = Monad.MonadUtils(Monad_gen)

module Arbitrary_unit : Arbitrary with type a = unit = 
struct
  type a = unit
  let arbitrary = Monad_gen.return ()
  let coarbitrary _ = variant 0
end

module Arbitrary_bool : Arbitrary with type a = bool =
struct
  type a = bool
  let arbitrary = elements [true; false]
  let coarbitrary b = if b then variant 0 else variant 1
end


module Arbitrary_char : Arbitrary with type a = char =
struct
  open Monad_gen_utils
  type a = char
  let arbitrary 
      = let module ChooseInt = Choose (Random_int) in
          ChooseInt.choose (32,255) >>= fun n -> return (Char.chr n)
  let coarbitrary n = variant (Char.code n)
end

module Arbitrary_int : Arbitrary with type a = int =
struct
  type a = int
  let arbitrary = 
    let module ChooseInt = Choose (Random_int) in
      sized (fun n -> ChooseInt.choose (-n,n))
  let coarbitrary n = variant (if n >= 0 then 2*n else 2*(-n) + 1)
end

module Arbitrary_2 (A1 : Arbitrary) (A2 : Arbitrary)
  : Arbitrary with type a = (A1.a * A2.a) =
struct
  open Monad_gen_utils
  type a = (A1.a * A2.a) 
  let arbitrary = liftM2 (fun x y -> x,y) A1.arbitrary A2.arbitrary
  let coarbitrary (a,b) x = A1.coarbitrary a (A2.coarbitrary b x)
end

module Arbitrary_3 (A1 : Arbitrary) (A2 : Arbitrary) (A3 : Arbitrary)
  : Arbitrary with type a = (A1.a * A2.a * A3.a) =
struct
  open Monad_gen_utils
  type a = (A1.a * A2.a * A3.a) 
  let arbitrary = liftM3 (fun x y z -> x,y,z) A1.arbitrary A2.arbitrary A3.arbitrary
  let coarbitrary (a,b,c) x = A1.coarbitrary a (A2.coarbitrary b (A3.coarbitrary c x))
end

module Arbitrary_4 (A1 : Arbitrary) (A2 : Arbitrary) (A3 : Arbitrary) (A4 : Arbitrary)
  : Arbitrary with type a = (A1.a * A2.a * A3.a * A4.a) =
struct
  open Monad_gen_utils
  type a = (A1.a * A2.a * A3.a * A4.a) 
  let arbitrary 
      = (liftM4
            (fun w x y z -> w,x,y,z)
            A1.arbitrary A2.arbitrary A3.arbitrary A4.arbitrary)
  let coarbitrary (a,b,c,d) x
      = A1.coarbitrary a (A2.coarbitrary b (A3.coarbitrary c (A4.coarbitrary d x)))
end

module Arbitrary_float : Arbitrary with type a = float =
struct
  open Monad_gen_utils
  type a = float
  let fraction a b c
      = float_of_int a +. (float_of_int b /. (abs_float (float_of_int c) +. 1.)) 
  let decodeFloat f
      = let m,e = frexp f in
          (int_of_float (m *. float_of_int Primitives.Bounded_int.maxBound), e)
  let arbitrary
      = (liftM3
            fraction
            Arbitrary_int.arbitrary Arbitrary_int.arbitrary Arbitrary_int.arbitrary)
  let coarbitrary x = let module M = Arbitrary_2(Arbitrary_int)(Arbitrary_int) in
                        M.coarbitrary (decodeFloat x)
end

module Arbitrary_list (A : Arbitrary) : Arbitrary with type a = A.a list =
struct
  module rec This : sig  type a = A.a list val coarbitrary : a -> 'b gen -> 'b gen end  =
  struct
    open Monad_gen_utils
    type a = A.a list
    let coarbitrary = 
      function
        | [] -> variant 0
        | x::xs -> fun z -> A.coarbitrary x (variant 1 (This.coarbitrary xs z))
  end
  include This
  open Monad_gen_utils
  let arbitrary = 
    let module V = Vector(A) in
    let module ChooseInt = Choose (Random_int) in 
      sized (fun n -> ChooseInt.choose (0,n) >>= V.vector) 
end

module Arbitrary_function (A : Arbitrary) (B : Arbitrary) 
  : Arbitrary with type a = A.a -> B.a =
struct
  open Monad_gen_utils
  type a = A.a -> B.a
  let arbitrary         = promote (fun s -> A.coarbitrary s B.arbitrary)
  let coarbitrary f gen = A.arbitrary >>= (fun s -> (B.coarbitrary (f s) gen))
end


open Primitives
open Show
open QuickCheck

module Arbitrary_intlist = Arbitrary_list(Arbitrary_int)
module Show_intlist = Show_list(Show_int)

(* 2.1 A simple example *)
let prop_RevUnit x = 
  List.rev [x] = [x]

let prop_RevApp xs ys = 
  List.rev (xs @ ys) = List.rev ys @ List.rev xs

let prop_RevRev xs = 
  List.rev (List.rev xs) = xs

let test_prop_RevApp = 
  let module M = Property_utils(Testable_function(Arbitrary_intlist)(Show_intlist)
                                   (Testable_function(Arbitrary_intlist)(Show_intlist)
                                       (Testable_bool))) in
    M.quickCheck prop_RevApp

let test_prop_RevUnit =
  let module M = Property_utils(Testable_function(Arbitrary_int)(Show_int)(Testable_bool)) in
    M.quickCheck prop_RevUnit

let test_prop_RevRev =
  let module M = Property_utils(Testable_function(Arbitrary_intlist)(Show_intlist)(Testable_bool)) in
    M.quickCheck prop_RevUnit

let test_prop_RevApp' = 
  (* Test false property *)
  let module M = Property_utils(Testable_function(Arbitrary_intlist)(Show_intlist)
                                   (Testable_function(Arbitrary_intlist)(Show_intlist)
                                       (Testable_bool))) in
    M.quickCheck 
      (fun xs ys -> List.rev (xs @ ys) = List.rev xs @ List.rev ys)


(* 2.2 Functions *)
let (===) f g x = f x = g x

let (-<-) f g x = f (g x)

let prop_CompAssoc : (int -> int) -> (int -> int) -> (int -> int) -> int -> bool
  = fun f g h -> f -<- (g -<- h) === (f -<- g) -<- h

module Arbitrary_int_to_int = Arbitrary_function(Arbitrary_int)(Arbitrary_int)
module Show_int_to_int = Show_unprintable(struct type a = int -> int end)


let test_prop_CompAssoc =
  let module M = Property_utils(Testable_function(Arbitrary_int_to_int)(Show_int_to_int)
                                   (Testable_function(Arbitrary_int_to_int)(Show_int_to_int)
                                       (Testable_function(Arbitrary_int_to_int)(Show_int_to_int)
                                           (Testable_function(Arbitrary_int)(Show_int)
                                               (Testable_bool))))) in
    M.quickCheck prop_CompAssoc

(* 2.3 Conditional laws *)
let prop_MaxLe : int -> int -> property
  = 
  let module M = Property_utils(Testable_bool) in let (==>) = M.(==>) in
    fun x y -> x <= y ==> (max x y = y)

let test_prop_MaxLe =
  let module M = Property_utils(Testable_function(Arbitrary_int)(Show_int)
                                   (Testable_function(Arbitrary_int)(Show_int)
                                       (Testable_property))) in
    M.quickCheck prop_MaxLe

let rec ordered = function
  | [] -> true
  | [_] -> true
  | x::(y::_ as rest) -> x <= y && ordered rest

let rec insert item = function
  | [] -> [item]
  | x::_ as list when x >= item -> item :: list
  | x::rest -> x ::insert item rest      

let prop_Insert : int -> int list -> property
  = fun x xs -> 
    let module M = Property_utils(Testable_bool) in let (==>) = M.(==>) in
      ordered xs ==> ordered (insert x xs)
                                                      
let test_prop_Insert =
  let module M = Property_utils(Testable_function(Arbitrary_int)(Show_int)
                                   (Testable_function(Arbitrary_intlist)(Show_intlist)
                                       (Testable_property)))
  in M.quickCheck prop_Insert

(* 2.4 Monitoring Test Data *)
let prop_Insert' : int -> int list -> 'a
  = let module M1 = Property_utils(Testable_bool) in
  let module M2 = Property_utils(Testable_property) in
    let (==>) = M2.(==>) in
      fun x xs ->
        (ordered xs ==>
            ((M1.classify (xs = []) "trivial" (ordered (insert x xs)))))

let test_prop_Insert' =
  let module M = Property_utils(Testable_function(Arbitrary_int)(Show_int)
                                   (Testable_function(Arbitrary_intlist)(Show_intlist)
                                       (Testable_property)))
  in M.quickCheck prop_Insert'

(* Define orderedList ?*)

(* 2.5 Infinite structures *)
(* Can't do prop_DoubleCycle because of lack of lazy append *)

(* 3.2 Generators for User-Defined Types *)
type colour = Red | Blue | Green

module Arbitrary_colour
  : Arbitrary with type a = colour =
struct
  open Monad_gen
  type a = colour
  let arbitrary = oneof [return Red; return Blue; return Green]
  let coarbitrary = function
    | Red -> variant 0
    | Blue -> variant 1
    | Green -> variant 2
end

type 'a tree = Leaf of 'a | Branch of ('a tree * 'a tree)


module Arbitrary(Arbitrary : Arbitrary) 
  : Arbitrary with type a = Arbitrary.a tree =
struct
  open Monad_gen_utils
  type a = Arbitrary.a tree

  let rec arbTree = function
    | 0 -> liftM (fun s -> Leaf s) Arbitrary.arbitrary
    | n -> (frequency
               [(1, liftM (fun s -> Leaf s) Arbitrary.arbitrary);
                (4, liftM2 (fun a b -> Branch (a,b)) (arbTree (n / 2)) (arbTree (n / 2)))])
                  
  let arbitrary = sized arbTree

  let coarbitrary _ = failwith "NYI"
end


(* 5.1 Case study: unification *)
type var = Variable of int
and term = Var of var | Constr of (name * term list)
and name = Name of string

module Arbitrary_name
  : Arbitrary with type a = name =
struct
  open Monad_gen_utils
  type a = name
  let arbitrary = 
    let ilog n = int_of_float (log (float_of_int n)) in
      sized
        (fun n -> oneof
          (List.map (fun i -> return (Name ("v" ^ string_of_int i))) 
              (Primitives.Enum_int.enumFromTo 1 (ilog (n+1)))))
  let coarbitrary _ = failwith "NYI"
    
end 
