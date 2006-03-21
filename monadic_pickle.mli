(* The type of a `reference number' for pickled objects. This allows
   us to refer to objects by reference in the serialized representation
   and thus handle circularity. *)
type id

(* The type of a computation that performs a pickling, and returns an 'a.  *)
type 'a pickling_monad

(* The abstract type of a pickling function *)
type 'a pickler = 'a -> id pickling_monad

(* Picklers for primitive types *)
val pickle_string : string pickler
val pickle_bool   : bool pickler
val pickle_int    : Num.num pickler
val pickle_oint   : int pickler
val pickle_float  : float pickler
val pickle_char   : char pickler
val null_pickler  : 'a pickler

(* Picklers for tuples.  Use these to create picklers for ADTs *)
val pickle0 : char -> unit -> unit -> id -> unit pickling_monad
val pickle1 : char -> ('a pickler) -> ('a) -> id -> unit pickling_monad
val pickle2 : char -> ('a pickler * 'b pickler) -> ('a * 'b) -> id -> unit pickling_monad
val pickle3 : char -> ('a pickler * 'b pickler * 'c pickler) -> ('a * 'b * 'c) -> id -> unit pickling_monad
val pickle4 : char -> ('a pickler * 'b pickler * 'c pickler * 'd pickler) -> ('a * 'b * 'c * 'd) -> id -> unit pickling_monad
val pickle5 : char -> ('a pickler * 'b pickler * 'c pickler * 'd pickler * 'e pickler) -> ('a * 'b * 'c * 'd * 'e) -> id -> unit pickling_monad
val pickle6 : char -> ('a pickler * 'b pickler * 'c pickler * 'd pickler * 'e pickler * 'f pickler) -> ('a * 'b * 'c * 'd * 'e * 'f) -> id -> unit pickling_monad
val pickle7 : char -> ('a pickler * 'b pickler * 'c pickler * 'd pickler * 'e pickler * 'f pickler * 'g pickler) -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) -> id -> unit pickling_monad
val pickle8 : char -> ('a pickler * 'b pickler * 'c pickler * 'd pickler * 'e pickler * 'f pickler * 'g pickler * 'h pickler) -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) -> id -> unit pickling_monad
val pickle9 : char -> ('a pickler * 'b pickler * 'c pickler * 'd pickler * 'e pickler * 'f pickler * 'g pickler * 'h pickler * 'i pickler) -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) -> id -> unit pickling_monad

(* Actually perform the serialization, returning the serialized object
   and its id. *)
val do_pickle : id pickling_monad -> id * string

(* Given an object and a function f that takes an object (and its id)
   and pickles it, return a pickler. *)
val picklewith : 'a -> ('a -> id -> unit pickling_monad) -> id pickling_monad
