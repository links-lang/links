exception Not_implemented of string

val not_implemented : string -> 'a

module type Hashed_type = sig
  type t

  val equal : t -> t -> bool

  val hash : t -> int
end

module Int : sig
  type t = int

  val equal : t -> t -> bool

  val hash : t -> int
end

module Hash_set (X : Hashed_type) : sig
  type t

  val create : int -> t

  val clear : t -> unit

  val copy : t -> t

  val add : t -> X.t -> unit

  val remove : t -> X.t -> unit

  val mem : t -> X.t -> bool

  val iter : (X.t -> unit) -> t -> unit

  val fold : (X.t -> 'a -> 'a) -> t -> 'a -> 'a

  val length : t -> int
end

module Dyn_array : sig
  type 'a t

  val create : int -> 'a t

  val to_array : 'a t -> 'a array

  val contents : 'a t -> 'a array

  val sub : 'a t -> int -> int -> 'a array

  val nth : 'a t -> int -> 'a

  val set : 'a t -> int -> 'a -> unit

  val length : 'a t -> int

  val clear : 'a t -> unit

  val reset : 'a t -> unit

  val grow : 'a t -> int -> unit

  val add : 'a t -> 'a -> int

  val append : 'a t -> 'a array -> int -> int -> unit

  val append_whole : 'a t -> 'a array -> unit
end

module Int_set : sig
  type t

  val empty : t

  val singleton : int -> t

  val segment : int -> int -> t

  val mem : int -> t -> bool

  val cup : t -> t -> t

  val cap : t -> t -> t

  val to_buffer : (int -> unit) -> Buffer.t -> t -> unit

  val to_string : (int -> unit) -> t -> string
end

module String_map : Map.S with type key = string

type 'a attribute =
    { key : string;
      value : 'a;
      mandatory : bool }

type 'a attributes =
    { map :  'a attribute String_map.t;
      closed : bool }

type 'a element =
    { ns : 'a;
      label : 'a;
      attributes : 'a attributes;
      contents : 'a }

module Type : sig
  type t

  type contents =
      Not_defined
    | Empty
    | Any
    | Epsilon
    | Char_set of Int_set.t
    | Element of t element
    | Sequence of t list
    | Union of t list

  module Show_t : sig
    type a = t

    val show : a -> string

    val showList : a list -> string

    val showBuf : a -> Buffer.t -> unit

    val showBufList : a list -> Buffer.t -> unit
  end

  module Pickle_t : sig
    type a = t

    val pickle : Buffer.t -> a -> unit

    val unpickle : char Stream.t -> a

    val pickleS : a -> string

    val unpickleS : string -> a
  end

  val create : string option -> contents -> t

  val set : t -> contents -> unit

  val to_buffer : Buffer.t -> t -> unit

  val to_string : t -> string

  val empty : t

  val any : t

  val epsilon : t

  val element : t element -> t

  val union : t -> t -> t

  val concat : t -> t -> t

  val star : t -> t

  val one_or_more : t -> t

  val optional : t -> t

  val latin1_char : t

  val digit : t

  val latin1_string : t

  val digit_string : t 

  val from_char : char -> t

  val from_string : string -> t
end

module Inference : sig
  type t

  module Show_t : sig
    type a = t

    val show : a -> string

    val showList : a list -> string

    val showBuf : a -> Buffer.t -> unit

    val showBufList : a list -> Buffer.t -> unit
  end

  module Pickle_t : sig
    type a = t

    val pickle : Buffer.t -> a -> unit

    val unpickle : char Stream.t -> a

    val pickleS : a -> string

    val unpickleS : string -> a
  end

  exception Cyclic of t

  val fresh : unit -> t

  val from_type : Type.t -> t

  val retrieve_type : t -> Type.t

  val less_than : t -> Type.t -> unit

  val equal_to : t -> Type.t -> unit

  val greater_than : t -> Type.t -> unit

  val extract_inferred_type : t -> Type.t

  val empty : t

  val any : t

  val epsilon : t

  val latin1_char : t

  val digit : t

  val latin1_string : t

  val digit_string : t

  val concat : t -> t -> t

  val element : t element -> t

  val to_string : t -> string

  val unify : t -> t -> unit

  val same : t -> t -> bool
end
