(*pp deriving *)
type 'a seq = Nil | Cons of 'a * 'a seq
  deriving (Eq, Typeable, Shelve)

type intseq = int seq
  deriving (Eq, Typeable, Shelve)

type refobj = A | B of refobj ref
  deriving (Eq, Typeable, Shelve)

let circular = 
  let s = ref A in
  let r = B s in
    s := r;
    r

let shelved = Shelve_refobj.shelveS circular
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
  
let shelvedm = Shelve_mut.shelveS circularm
