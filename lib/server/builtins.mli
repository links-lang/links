(** Links builtin functions **)

(** Types **)
type 'a maybe = [ `Just of 'a
                | `Nothing ]

type 'a option = [ `Some of 'a
                 | `None ]

(** **)
(*val pow  : int -> int -> int
  val (^.) : float -> float -> float*)

val fabs : float -> float
  
(** Conversions **)
val intToString   : int -> string
val stringToInt   : string -> int
val intToFloat    : int -> float
val floatToInt    : float -> int
val floatToString : float -> string
val stringToFloat : string -> float  
   
(** List operations **)
val concat : 'a list list -> 'a list
val hd     : 'a list -> 'a
val tl     : 'a list -> 'a list
val length : 'a list -> int
  
val drop : int -> 'a list -> 'a list    
val take : int -> 'a list -> 'a list
  
val max : 'a list -> 'a option
val min : 'a list -> 'a option

(** Char functions **)
val isAlpha  : char -> bool
val isAlnum  : char -> bool
val isWord   : char -> bool
val isLower  : char -> bool
val isUpper  : char -> bool
val isDigit  : char -> bool
val isXDigit : char -> bool
val isBlank  : char -> bool

val toUpper  : char -> char
val toLower  : char -> char

val ord      : char -> int
val chr      : int -> char

(** Floating point operations **)
val floor   : float -> float
val ceiling : float -> float
val cos     : float -> float
val cosh    : float -> float
val sin     : float -> float
val sinh    : float -> float
val tan     : float -> float
val tanh    : float -> float
val log     : float -> float
val sqrt    : float -> float

(** String operations **)
val charAt : string -> int -> char

val strsub : string -> int -> int -> string
val strlen : string -> int

val implode : char list -> string
val explode : string -> char list

val (^^) : string -> string -> string


(** Randomness **)
val random : unit -> float

(** Unix socket programming **)
type socket
  
val connectSocket : string -> int -> socket maybe

val writeToSocket : string -> socket -> unit
  
val readFromSocket : socket -> string maybe

val closeSocket : socket -> unit
  
(** Date **)
val serverTime : unit -> int

(** Misc **)     
val print : string -> unit
val error : string -> 'a
val project : int -> (int * 'a) list -> 'a
