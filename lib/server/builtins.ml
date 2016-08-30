(** Links builtin functions **)

(** Types **)
type 'a maybe = [ `Just of 'a
                | `Nothing ]

type 'a option = [ `Some of 'a
                 | `None ]
  
let identity x = x

(** Arithmetics **)
let pow base exponent =  
  let rec pow_aux acc base = function
    | 0 -> acc
    | 1 -> base * acc
    | n -> pow_aux (base * acc) base (n-1)
  in
  pow_aux 1 base exponent
(*  let is_even n = n mod 2 = 0 in
  let exponent = abs exponent in
    let rec aux accumulator base = function
      | 0 -> accumulator
      | 1 -> base * accumulator
      | e when is_even e -> aux accumulator (base * base) (e / 2)
      | e -> aux (base * accumulator) (base * base) ((e - 1) / 2) in
    aux 1 base exponent*)

let (^.) base exponent = base ** exponent

let fabs = abs_float
  
(** Conversions **)
let intToString   = string_of_int
let stringToInt   = int_of_string
let intToFloat    = float_of_int

let floatToInt    = int_of_float
let floatToString = string_of_float
let stringToFloat = float_of_string  
  
(** comparison **)
let less x y = compare x y = -1
  
(** List operations **)
let concat = List.concat
let hd     = List.hd
let tl     = List.tl
let length = List.length
(* Copied from utility.ml *)
let rec drop n = if n = 0 then identity else function
  | []     -> []
  | _ :: t -> drop (n - 1) t
     
let rec take n list = match n, list with
  | 0, _ -> []
  | _, [] -> []
  | _, h :: t -> h :: take (n - 1) t

let reduce op x = List.fold_left op x
     
let max xs =
  let max2 x y = if less x y then y else x in
  match xs with
  | []       -> `None
  | x :: xs  -> `Some (reduce max2 x xs)

let min xs =
  let min2 x y = if less x y then x else y in
  match xs with
  | []      -> `None
  | x :: xs -> `Some (reduce min2 x xs)

(** Char functions **)
let isAlpha  = function 'a'..'z' | 'A'..'Z' -> true | _ -> false
let isAlnum  = function 'a'..'z' | 'A'..'Z' | '0'..'9' -> true | _ -> false
let isWord   = function 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false
let isLower  = function 'a'..'z' -> true | _ -> false
let isUpper  = function 'A'..'Z' -> true | _ -> false
let isDigit  = function '0'..'9' -> true | _ -> false
let isXDigit = function '0'..'9'|'a'..'f'|'A'..'F' -> true | _ -> false
let isBlank  = function ' '|'\t' -> true | _ -> false

let toUpper  = Char.uppercase
let toLower  = Char.lowercase

let ord      = Char.code
let chr      = Char.chr

(** Floating point operations **)
let floor   = floor
let ceiling = ceil
let cos     = cos
let cosh    = cosh
let sin     = sin
let sinh    = sinh
let tan     = tan
let tanh    = tanh
let log     = log
let sqrt    = sqrt

(** String operations **)
let charAt s i =
  try s.[i] with
  | Invalid_argument _ -> failwith "charAt: invalid index"

let strsub s start len =
  try String.sub s start len with
  | Invalid_argument _ -> failwith "strsub: invalid arguments"

let strlen = String.length

let implode cs =
  let s = Bytes.create (length cs) in
  let rec copy i cs =
      match cs with
      | [] -> ()
      | c :: cs -> Bytes.set s i c; copy (i + 1) cs
  in
  let _ = copy 0 cs in
  Bytes.unsafe_to_string s

let explode s =
  let rec copy i cs =
    if i < 0
    then cs
    else copy (i - 1) (s.[i] :: cs)
  in
  copy (strlen s - 1) []

let (^^) = (^)
    
(** Randomness **)
let _ = Random.self_init ()
let random () = Random.float 1.0

(** Unix socket programming **)
type socket = {inc : in_channel ; outc : out_channel }

let make_socket inchan outchan = {inc = inchan ; outc = outchan }
  
let inchan socket  = socket.inc
let outchan socket = socket.outc
  
let connectSocket server port =
  try
    let server_addr =
      try Unix.inet_addr_of_string server
      with Failure "inet_addr_of_string" -> (Unix.gethostbyname server).Unix.h_addr_list.(0)
    in
    let sockaddr = Unix.ADDR_INET (server_addr, port) in
    let domain   = Unix.domain_of_sockaddr sockaddr in
    let sock     = Unix.socket domain Unix.SOCK_STREAM 0 in
    Unix.connect sock sockaddr;
    Unix.set_nonblock sock;
    `Just ( make_socket (Unix.in_channel_of_descr sock) (Unix.out_channel_of_descr sock) )
  with
  | exn -> `Nothing

let writeToSocket message socket =
  let outc = outchan socket in
  output_string outc message;
  flush outc

let readFromSocket socket =
  let inc = inchan socket in
  try `Just (input_line inc) with
  | Sys_blocked_io 
  | End_of_file    -> `Nothing

let closeSocket socket =
  let inc = inchan socket in
  Unix.shutdown (Unix.descr_of_in_channel inc) Unix.SHUTDOWN_SEND
  
(** Date **)
  let serverTime () = int_of_float (Unix.time ())

(** Misc **)     
let print = print_endline
let error = failwith
let rec project (l : int) = function
  | (k,v) :: xs when k = l -> v
  | _ :: xs                -> project l xs
  | []                     -> failwith "Fatal error: Projection failed."
