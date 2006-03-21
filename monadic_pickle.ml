(* A couple of conventions should make the serialization representation much more compact:

   If we avoid using numeric chars as tags then object ids can be
   simple numbers, and we don't need to use a delimiter (+) for length
   fields.

   If we wanted to be really compact, we could use a binaryer
   representation (representing object ids in integers with each byte
   tagged, for example)

   A simpler API for non-recursive objects would help as well.

   This would probably also be more efficient if it used Buffer
   instead of strings.
*)






(* Memoizing pickle that handles structure-sharing properly.  We'll
   memoize on pointer equality.  Perhaps we should let the user
   choose, instead. *)

(*
  Perhaps this should use a list instead of a hashtable.  Hashing is
  unlikely to be significantly faster (and may even be slower) for
  the small datasets involved.  (Perhaps it should be parameterized 
  over the datastructure.)  Is that premature optimization I smell? 
*)


(** 
    Hashtable for memoization.  Since we need to memoize objects of
    lots of different types, we'll "upcast" to Obj.t.  Since Obj.repr
    is just the identity function we can still compare for structural
    equality. This seems to be an instance where "dynamic typing" wins
    for now.
**)
module GenericHashtbl = Hashtbl.Make 
  (struct
     type t = Obj.t
     let equal = (==)
     let hash = Hashtbl.hash
   end)



type 'a hashtable = 'a GenericHashtbl.t

(* Estimated initial size of the table. (Who knows?  Let's just choose
   an arbitrary prime.) *)
let initial_size = 4093

(* Create a new hash table *)
let ht_make : unit -> 'a hashtable 
  = fun () -> GenericHashtbl.create initial_size

(* Insert a key and value into a table. Destructive. *)
let ht_insert : 'b hashtable -> 'a -> 'b -> unit
  = fun ht k v -> GenericHashtbl.add ht (Obj.repr k) v

(* Retrieve a value from the table by key. *)
let ht_find : 'b hashtable -> 'a -> 'b option
  = fun ht k -> (try Some (GenericHashtbl.find ht (Obj.repr k) )
                 with Not_found -> None)



(**
   State monad.
**)
type ('state, 'b) state = State of ('state -> ('b * 'state))

let runState (State s) = s

let return a = State (fun state -> (a, state))
let (>>=) (State x) f = State (fun s -> (let v, s' = x s in
                                           runState (f v) s'))
(* Does this work right without laziness? *)
let (>>) s f = s >>= fun _ -> f
  
(** 
    Instantiation of the state monad for memoization.
**)

(* We allocate a reference number to each object.  Here's a type alias
   for reference numbers *)
type id = string

(* The state we keep around during serialization. *)
type pickle_state = (
  (* next id to allocate.  This is one greater than the maximum id in
     the hashtable, but easier to retrieve *)
  int                  
  (* A mapping of objects to reference numbers. We add a new entry
   * here every time we start serializing an object. *)
  * id hashtable
  (* A mapping of reference numbers to strings.  We add a new entry
     here every time we finish serializing an object. *)
  * (id * string) list)
    
(* Instantiate the monad type with our pickle state *)
type 'b pickling_monad = (pickle_state, 'b) state

type 'a age = [`Fresh of 'a | `Stale of 'a]
let unage : 'a age -> 'a = function
  | `Fresh a -> a
  | `Stale a -> a

(* 
   Retrieve an serialization id for an object.  If the object has been
   seen already, return the id already assigned.  Otherwise, allocate
   a new id and return that, after inserting it into the table.
*)
let number obj : id age pickling_monad =
  State (fun (next, table, ids) -> 
           match ht_find table obj with
             | Some id -> (`Stale id, (next, table, ids))
             | None    -> (let id = string_of_int next in
                             ht_insert table obj id;
                             (`Fresh id, (next + 1, table, ids))))

(*
  Record the serialized representation of an object.
*)
let serialize id string : unit pickling_monad = 
  State (fun (next, table, ids) -> 
           ((), (next, table, (id, string) :: ids)))


type 'a pickler = 'a -> id pickling_monad

let add_header tag obj = 
  (String.make 1 tag) ^ string_of_int (String.length obj) ^ "+" ^ obj

(* Primitives are serialized as immediates *)

let primitve_pickler (tag : char) (stringize : 'a -> string) (obj : 'a) =
  return (add_header tag (stringize obj))

let pickle_string : string pickler  = primitve_pickler 'h' Sl_utility.identity  
and pickle_bool   : bool pickler    = primitve_pickler 'b' string_of_bool
and pickle_int    : Num.num pickler = primitve_pickler 'i' Num.string_of_num
and pickle_oint   : int pickler     = primitve_pickler 's' string_of_int
and pickle_float  : float pickler   = primitve_pickler 'f' string_of_float
  (* Char is fixed-length, so we needn't save the length.  We could do
     the same for bool if necessary *)
and pickle_char   : char pickler   = fun c -> return ("C" ^ (String.make 1 c))

and null_pickler _ = return ""

(* What are the right abstractions ?
   When we serialize an object, we need to
   1. Find (and record) its id.
   2. Find ids for all the subobjects.
   3. Record the serialization, which uses all the ids.
   4. Return the id.

   Note that we need to record the id of the object before processing
   subobjects, since otherwise a circular reference would cause
   infinite regress.
*)
                 
(* How can we deserialize in a type safe way? 
   Are there any new problems?
*)


(** Overall plan:

    1. An environment has type [(string, number)].
    2. Assign a number to every distinct object (using the hashtable).
    3. Serialize objects to strings, serializing numbers in place of subobjects.
    For instance, `Function (var, body, data) may be serialized as
    something like "f" ^ "#23" ^ "#71" ^ "#22".  It'll also be given a number.
    4. Serialize the mapping from numbers to objects. Actually write this one out.
    5. Serialize the number of the `root' object.  Actually write this one out.
    Deserialize as follows:
    1. Deserialize the number of the root object.
    2. Deserialize the number/object mapping.
    3. Starting at the root object, reconstruct everything.  

    Constructing circular references requires care.  Probably the best
    way to do it is to maintain a list of objects under construction,
    and their ids, and to check the list before attempting to
    construct new objects.  This might be a bit tricky to do without
    `ref', though, depending on exactly what is allowed on the rhs of
    `letrec'.
  **)


(* Some abstraction scratchings *)
let picklewith obj pickler = 
  number obj >>= 
    function
      | `Stale id -> return id
      | `Fresh id -> (pickler obj) id >> return id
          
(* A run-monad function.

     do_serialize (pickle_foo foo)
*)
let do_pickle pickled = 
  let topid, (_, _, idmap) = runState pickled (0, ht_make (), []) in
    topid, String.concat "" (List.map (fun (l, r) -> l ^ r) idmap)


(* A sort of fold over monadic values.  The generic version probably
   has a name. *)
let combine s tag id =
  let rec combine' s tag id = function
    | [] -> serialize id (add_header tag s)
    | car :: cdr -> car >>= fun s2 -> combine' (s ^ s2) tag id cdr
  in combine' "" s tag id
       
let pickle0 tag ()                           ()                            obj = combine tag obj []
let pickle1 tag (p1)                         (v1)                          obj = combine tag obj [p1 v1]
let pickle2 tag (p1,p2)                      (v1,v2)                       obj = combine tag obj [p1 v1; p2 v2]
let pickle3 tag (p1,p2,p3)                   (v1,v2,v3)                    obj = combine tag obj [p1 v1; p2 v2; p3 v3]
let pickle4 tag (p1,p2,p3,p4)                (v1,v2,v3,v4)                 obj = combine tag obj [p1 v1; p2 v2; p3 v3; p4 v4]
let pickle5 tag (p1,p2,p3,p4,p5)             (v1,v2,v3,v4,v5)              obj = combine tag obj [p1 v1; p2 v2; p3 v3; p4 v4; p5 v5]
let pickle6 tag (p1,p2,p3,p4,p5,p6)          (v1,v2,v3,v4,v5,v6)           obj = combine tag obj [p1 v1; p2 v2; p3 v3; p4 v4; p5 v5; p6 v6]
let pickle7 tag (p1,p2,p3,p4,p5,p6,p7)       (v1,v2,v3,v4,v5,v6,v7)        obj = combine tag obj [p1 v1; p2 v2; p3 v3; p4 v4; p5 v5; p6 v6; p7 v7]
let pickle8 tag (p1,p2,p3,p4,p5,p6,p7,p8)    (v1,v2,v3,v4,v5,v6,v7,v8)     obj = combine tag obj [p1 v1; p2 v2; p3 v3; p4 v4; p5 v5; p6 v6; p7 v7; p8 v8]
let pickle9 tag (p1,p2,p3,p4,p5,p6,p7,p8,p9) (v1,v2,v3,v4,v5,v6,v7,v8,v9)  obj = combine tag obj [p1 v1; p2 v2; p3 v3; p4 v4; p5 v5; p6 v6; p7 v7; p8 v8; p9 v9]

(* A test *)
type foo = One of (char * string) 
         | Two
         | Three of (foo * foo)


let rec pickle_foo : foo pickler = fun foo ->
  picklewith foo 
    (function
       | One v        -> pickle2 'a' (pickle_char, pickle_string) v
       | Two          -> pickle0 'b' () ()
       | Three v      -> pickle2 'c' (pickle_foo, pickle_foo) v)

let t = do_pickle (pickle_foo (let rec v = Three (One ('3', "three"), Three (One ('4', "four"), v)) in v))


(*
let rec v = Three (One ('3', "three"), Three (One ('4', "four"), v));;

do_pickle (pickle_foo v)
=> ("0", "0c2+122c2+303a9+C4h4+four1a10+C3h5+three")
*)




(* Deserializing a circular value *)

let push item list = list := item :: !list

let unSome = function
  | Some s -> s
  | None -> failwith "Unexpected None!"


let make_it values (id:int) sid1 sid2 =
  let v = ref None in
  let values = (id, v) :: values in
  let sub1 = 
    try List.assoc sid1 values
    with Not_found -> ref (Some Two)
  and sub2 = 
    try List.assoc sid2 values
    with Not_found -> ref (Some Two)
  in let don = ref false in
  let rec x () = 
    if not !don then
      begin
        don := true;
        v := Some (Three ((v := x (); unSome !sub1), (v := x (); unSome !sub2))) ;
        !v
      end
    else 
      !v

  in x ()


