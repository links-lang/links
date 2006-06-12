(**** Various utility functions ****)

(*** string environments ***)
module OrderedString =
struct
  type t = string
  let compare : string -> string -> int = String.compare
end
module StringMap = Map.Make(OrderedString)
module StringSet = Set.Make(OrderedString)

let assoc_list_of_string_map env =
  StringMap.fold (fun x y l -> (x, y) :: l) env []

let string_map_of_assoc_list l =
  List.fold_right (fun (x, y) env -> StringMap.add x y env) l StringMap.empty 


(*** int environments ***)
module OrderedInt =
struct
  type t = int
  let compare : int -> int -> int = compare
end
module IntMap = Map.Make(OrderedInt)
module IntSet = Set.Make(OrderedInt)

let intset_of_list l = 
  List.fold_right (fun itm set -> IntSet.add itm set) l IntSet.empty



(*** Functional programming ***)

(* compose operators (arrow indicates direction of composition) *)
let (-<-) = fun f g x -> f (g x)
let (->-) f g x = g (f x)


let curry f a b = f (a, b)
let uncurry f (a, b) = f a b
let identity x = x
let notany f = List.for_all (not -<- f)
let flip f x y = f y x


(*** Lists ***)

let rec fromTo f t = 
  if f = t then []
  else f :: fromTo (f+1) t


let span (p : 'a -> bool) : 'a list -> ('a list * 'a list) =
  let rec span = function
    | [] -> [], []
    | x::xs' when p x -> let ys, zs = span xs' in x::ys, zs
    | xs              -> [], xs in
    span
        
let groupBy eq = 
  let rec group = function
    | [] -> []
    | x::xs -> (let ys, zs = span (eq x) xs in 
                  (x::ys)::group zs) 
  in group

let rec unsnoc = 
  function
    |  [x] -> [], x
    | (x::xs) -> let ys, y = unsnoc xs in x :: ys, y
    | []   -> raise (Invalid_argument "unsnoc")

let last l = snd (unsnoc l)

let butlast l = fst (unsnoc l)

(** Comparison function from a less-than function *)
let less_to_cmp less l r = 
  if less r l then 1
  else if less l r then -1
  else 0

(** Remove duplicates from a list *)
let rec unduplicate equal = function
  | [] -> []
  | elem :: elems -> (let _, others = List.partition (equal elem) elems in
                        elem :: unduplicate equal others)

let rec ordered_consecutive = function
  | [] -> true
  | [_] -> true
  | one :: (two :: _ as rest) -> one + 1 = two && ordered_consecutive rest

let rec drop n = if n = 0 then identity else function
  | []     -> []
  | _ :: t -> drop (n - 1) t

let rec take n list = match n, list with 
  | 0, _ -> []
  | _, [] -> []
  | _, h :: t -> h :: take (n - 1) t
  
let rec rassoc_eq eq : 'b -> ('a * 'b) list -> 'a = fun value ->
    function
      | (k, v) :: _ when eq v value -> k
      | _ :: rest -> rassoc_eq eq value rest
      | [] -> raise Not_found

let rassoc i l = rassoc_eq (=) i l
and rassq i l = rassoc_eq (==) i l

let rec rremove_assoc_eq eq : 'b -> ('a * 'b) list -> ('a * 'b) list = fun value ->
  function
    | (_, v) :: rest when eq v value -> rest
    | other :: rest -> other :: rremove_assoc_eq eq value rest
    | [] -> []

let rremove_assoc i l = rremove_assoc_eq (=) i l
and rremove_assq i l = rremove_assoc_eq (==) i l

let concat_map f l = 
  let rec aux = function
    | _, [] -> []
    | f, x :: xs -> f x @ aux (f, xs)
  in aux (f,l)

exception NONE

(* association list utilities*)
let cross f g = fun (x, y) -> f x, g y

let alistmap f = List.map (cross identity f)

(*** Strings ***)

let string_of_char = String.make 1

let string_of_alist = String.concat ", " -<- List.map (fun (x,y) -> x ^ " => " ^ y)

let rec split_string source delim =
  if String.contains source delim then
    let delim_index = String.index source delim in
      (String.sub source 0 delim_index) :: (split_string (String.sub source (delim_index+1) ((String.length source) - delim_index - 1)) delim)
  else source :: []

let rec substitute predicate replacement
  = function
    | [] -> []
    | (first::rest) -> 
	if predicate first then replacement :: rest
	else first::(substitute predicate replacement rest)

let explode : string -> char list = 
let rec explode' list n string = 
  if n = String.length string then list
  else explode' (string.[n] :: list) (n + 1) string
in  List.rev -<- (explode' [] 0)
      
let implode : char list -> string = 
  (String.concat "") -<- (List.map (String.make 1))

(* Find all occurrences of a character within a string *)
let find_char (s : string) (c : char) : int list =
  let rec aux offset occurrences = 
    try let index = String.index_from s offset c in
      aux (index + 1) (index :: occurrences)
    with Not_found -> occurrences
  in List.rev (aux 0 [])

let mapstrcat glue f list = String.concat glue (List.map f list)

let numberp s = try ignore (int_of_string s); true with _ -> false

let index pred list = 
  let rec idx pos  = function
    | x :: _  when pred x -> pos
    | _ :: xs             -> idx (pos + 1) xs
    | []                  -> -1
  in idx 0 list

let dict_map f = List.map (fun (k,v) -> k, f v) 
  
let lines (channel : in_channel) : string list = 
  let rec next_line lines =
    try
      next_line (input_line channel :: lines)
    with End_of_file -> lines
  in List.rev (next_line [])

let process_output : string -> string
  = String.concat "\n" -<- lines -<- Unix.open_process_in

(* lookup is like assoc but uses option types instead of
   exceptions to signal absence *)
let lookup k alist = try Some (List.assoc k alist) with Not_found -> None

(*** option types ***)
let opt_map f = function
    None -> None
  | Some x -> Some (f x)
            
type ('a, 'b) either = Left of 'a | Right of 'b

let option2either = function
  | Some a, _ -> Left a
  | _, Some b -> Right b
  | _, _      -> raise Not_found

let option_or = function
  | Some x, _ -> Some x
  | _, Some y -> Some y
  | _, _      -> None

let rec either_assoc k1 k2 = function
  | [] -> raise Not_found
  | (k,v)::_ when k = k1 -> Left v
  | (k,v)::_ when k = k2 -> Right v
  | _::r -> either_assoc k1 k2 r 

let either_partition (f : 'a -> ('b, 'c) either) (l : 'a list)
    : 'b list * 'c list =
  let rec aux (lefts, rights) = function
    | [] -> (List.rev lefts, List.rev rights)
    | x::xs ->
        match f x with 
          | Left l  -> aux (l :: lefts, rights) xs
          | Right r -> aux (lefts, r :: rights) xs
  in aux ([], []) l
    

let option_assoc2 lbl1 lbl2 alist =
  option_or (lookup lbl1 alist,
	     lookup lbl2 alist)
    
let getVal(optn, dflt) = 
  match optn with
      None -> dflt
    | Some result -> result
        
 (* this is an ugly SML name *)

exception EmptyOption
let valOf = function
  | Some x -> x
  | None -> raise EmptyOption

let isSome = function
  | None -> false
  | Some _ -> true

let fromOption default = function
  | Some value -> value
  | None       -> default 

let perhaps_apply f p = fromOption p (f p)

let opt_sequence e = 
  let rec aux accum = function
  | []             -> Some (List.rev accum)
  | Some x :: rest -> aux (x::accum) rest
  | None :: _      -> None
  in aux [] e

let opt_sum e = 
  let rec aux accum = function
  | []             -> None
  | Some x :: rest -> aux (x::accum) rest
  | None :: _      -> None
  in opt_map List.rev (aux [] e)

(*** character encoding ***)

(* Read a three-digit octal escape sequence and return the
   corresponding char *)
let read_octal c =
  let octal_char = function
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3
    | '4' -> 4 | '5' -> 5 | '6' -> 6 | '7' -> 7 | _ -> invalid_arg "read_octal"
  in Char.chr ((octal_char c.[0]) * 64 + (octal_char c.[1]) * 8 + (octal_char c.[2]))

let read_hex c =
  let hex_char = function
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4
    | '5' -> 5 | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9
    | 'a' | 'A' -> 10
    | 'b' | 'B' -> 11
    | 'c' | 'C' -> 12
    | 'd' | 'D' -> 13
    | 'e' | 'E' -> 14
    | 'f' | 'F' -> 15 | _ -> invalid_arg "read_hex"
  in Char.chr ((hex_char c.[0]) * 16 + (hex_char c.[1]))

(* Handle escape sequences in string literals.

   I would describe them here but the O'Caml lexer gets too confused,
   even though they're in a comment.

   This is here rather than in sl_lexer.mll because the ocamllex gets
   confused by all the backslashes and quotes and refuses to translate
   the file.
*)
let escape_regexp = Str.regexp "\\\\\"\\|\\\\\\\\\\|\\\\[0-3][0-7][0-7]\\|\\\\[xX][0-9a-fA-F][0-9a-fA-F]" 
let decode_escapes s =
  let unquoter s = 
    (* Yes, the Str interface is stateful.  A pretty poor show.  PCRE
       is better, but we'd rather avoid the dependency and stick with
       the standard library. *)
    let s = Str.matched_string s in
      match s with
        | "\\\"" -> "\""
        | "\\\\" -> "\\"
        | other when other.[1] = 'x' || other.[1] = 'X' -> String.make 1 (read_hex (String.sub other 2 2)) 
        | other -> String.make 1 (read_octal (String.sub other 1 3)) in
    Str.global_substitute escape_regexp unquoter s

(** xml_escape
    xml_unescape
    Escape/unescape for XML escape sequences (e.g. &amp;)
*)
let xml_escape s = 
  Str.global_replace (Str.regexp "<") "&lt;" 
    (Str.global_replace (Str.regexp "&") "&amp;" s)

let xml_unescape s =
  Str.global_replace (Str.regexp "&amp;") "&"
    (Str.global_replace (Str.regexp "&lt;") "<" s)

(* base64 *)
let base64decode s = 
  try Netencoding.Base64.decode (Str.global_replace (Str.regexp " ") "+" s)
  with Invalid_argument "Netencoding.Base64.decode" 
      -> raise (Invalid_argument ("base64 decode gave error: " ^ s))

and base64encode s = Netencoding.Base64.encode s

(*** ocaml versions ***)
let ocaml_version_number = (List.map int_of_string
                              (split_string Sys.ocaml_version '.'))

(* Ocaml team says string comparison would work here. Do we believe them? *)
let rec version_atleast a b =
  match a, b with
      _, [] -> true
    | [], _ -> false
    | (ah::at), (bh::bt) -> ah > bh or (ah = bh && version_atleast at bt)
let ocaml_version_atleast min_vsn = version_atleast ocaml_version_number min_vsn


