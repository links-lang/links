(*pp deriving *)

type t1 = F deriving (Typeable)
type t2 = F deriving (Typeable)

let eq_types = Typeable.TypeRep.eq

let _ =
  begin 
    assert (eq_types
                   (Typeable_t1.type_rep ())
                   (Typeable_t1.type_rep ()));
    assert (eq_types
              (Typeable_t2.type_rep ())
              (Typeable_t2.type_rep ()));
    assert (not (eq_types
                   (Typeable_t1.type_rep ())
                   (Typeable_t2.type_rep ())));
    assert (not (eq_types
                   (Typeable_t2.type_rep ())
                   (Typeable_t1.type_rep ())));
  end

type t3 = int deriving (Typeable)

let _ =
  begin 
    assert (eq_types
              (Typeable.Typeable_int.type_rep ())
              (Typeable_t3.type_rep ()));
  end


type t4 = [`T of int] deriving (Typeable)
type t5 = [`T of t3] deriving (Typeable)

let _ =
  begin
    assert (eq_types
              (Typeable_t4.type_rep ())
              (Typeable_t5.type_rep ()));
  end

type t6 = [`T of t5]
    deriving (Typeable)

let _ =
  begin
    assert (not (eq_types
                   (Typeable_t5.type_rep ())
                   (Typeable_t6.type_rep ())));

  end

type t7 = [`T of t6]
    deriving (Typeable)

let _ =
  begin
    assert (not (eq_types
                   (Typeable_t6.type_rep ())
                   (Typeable_t7.type_rep ())));
  end


type t8 = [`A | `B] deriving (Typeable)
type t9 = [`B | `A] deriving (Typeable)

let _ =
  begin
    assert (eq_types
              (Typeable_t8.type_rep ())
              (Typeable_t9.type_rep ()));
  end


type ('a,'r) openr = [`Nil | `Cons of 'a * 'r]
 deriving (Typeable)
type 'a closedr = [`Nil | `Cons of 'a * 'a closedr]
 deriving (Typeable)
type l1 = (int, l1) openr
and l2 = int closedr deriving (Typeable)

let _ =
  begin
    assert (eq_types 
              (Typeable_l1.type_rep ())
              (Typeable_l1.type_rep ()));
  end

type nil = [`Nil] deriving (Typeable)
type t10 = ([nil| `Cons of int * 'a ] as 'a) list
    deriving (Typeable)
type t11 = l2 list deriving (Typeable)

let _ = 
  begin
    assert 
      (eq_types
         (Typeable_t10.type_rep ())
         (Typeable_t11.type_rep ()));
  end
