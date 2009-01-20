(*pp deriving *)

let _ = 
  Eq.eq<bool> true false


let _ = 
  Show.show<(bool * string) list option> 
    (Some ([true, "yes";
            false, "no"]))


let _ =
  [Typeable.mk<int> 3;
   Typeable.mk<float> 3.0;
   Typeable.mk<int list> [1;2;3]]

type 'a seq = [`Nil | `Cons of 'a * 'a seq]
    deriving (Typeable)
    
type nil = [`Nil] 
    deriving (Typeable)
type intlist = ([nil| `Cons of int * 'a ] as 'a)
    deriving (Typeable)
    
let _ = 
  Typeable.throwing_cast<intlist> 
    (Typeable.mk<int seq> (`Cons (1, `Cons (2, `Cons (3, `Nil)))))

let _ =
    Eq.eq<bool> true (Eq.eq<int> 3 4)

let _ =
  print_endline "Tests succeeded!"
