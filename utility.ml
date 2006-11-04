(**** Various utility functions ****)

let fst3(x, _, _) = x
let snd3(_, y, _) = y
let thd3(_, _, z) = z

(*** string environments ***)
module OrderedString =
struct
  type t = string
  let compare : string -> string -> int = String.compare
end
module StringMap = Map.Make(OrderedString)

type 'a stringmap = 'a StringMap.t

module StringSet = Set.Make(OrderedString)

(*** int environments ***)
module OrderedInt =
struct
  type t = int
  let compare : int -> int -> int = compare
end
module IntMap = Map.Make(OrderedInt)
module IntSet = Set.Make(OrderedInt)

let intset_of_list l = List.fold_right IntSet.add l IntSet.empty

(** {1 Functional combinators} *)
module Functional =
struct
  
  (** "compose" operators (arrow indicates direction of composition) *)
  let (-<-) = fun f g x -> f (g x)
  let (->-) f g x = g (f x)
    

  let curry f a b = f (a, b)
  let uncurry f (a, b) = f a b
  let identity x = x
  let flip f x y = f y x
    
  let cross f g = fun (x, y) -> f x, g y
end    
include Functional
    
(** {1 Lists} *)
module ListUtils = 
struct
  let rec fromTo f t = 
    if f = t then []
    else f :: fromTo (f+1) t

  (** [all_equiv rel list]: given an equiv. rel'n [rel], determine
      whether all elements of [list] are equivalent. *)
  let all_equiv (cmp : 'a -> 'a -> bool) : 'a list -> bool = function
      [] -> true
    | (one::others) ->
        List.for_all (fun x -> cmp one x) others
          
  (** [span pred list]: partition [list] into an initial sublist
      satisfying [pred] and the remainder.  *)
  let span (p : 'a -> bool) : 'a list -> ('a list * 'a list) =
    let rec span = function
      | x::xs' when p x -> let ys, zs = span xs' in x::ys, zs
      | xs              -> [], xs in
      span
        
  (** [groupBy rel list]: given a binary rel'n [rel], partition [list]
      into chunks s.t. successive elements [x], [y] in a chunk give the
      same value under [rel]. *)
  let groupBy eq : 'a list -> 'a list list = 
    let rec group = function
      | [] -> []
      | x::xs -> (let ys, zs = span (eq x) xs in 
                    (x::ys)::group zs) 
    in group

  (** [groupByPred pred] partitions [list] into chunks where all
      elements in the chunk give the same value under [pred]. *)
  let groupByPred pred list = 
  let rec group state result = function
    | [] -> List.rev (List.map List.rev result)
    | x::etc -> let predx = pred x in
      let new_result = (if (predx = state) then
                            (x::List.hd result) :: List.tl result
                        else
                            [x] :: result)
      in
        group predx new_result etc
  in if (list == []) then [] else  group (pred (List.hd list)) [] list
      
  (** [groupByPred']: Alternate implementation of groupByPred. *)
  let groupByPred' pred : 'a list -> 'a list list = 
    let rec group = function
      | [] -> []
      | x::xs -> 
          let predx = pred x in
            (let ys, zs = span (fun y -> pred y = predx) xs in 
               (x::ys)::group zs) 
    in group
      
  (** [unsnoc list]: Partition [list] into its last element and all the
      others. @return (others, lastElem) *)
  let rec unsnoc = 
    function
      |  [x] -> [], x
      | (x::xs) -> let ys, y = unsnoc xs in x :: ys, y
      | []   -> raise (Invalid_argument "unsnoc")
          
  (** [last list]: Return the last element of a list *)
  let last l = snd (unsnoc l)
    
  (** [butlast list]: Return a copy of the list with the last element removed. *)
  let butlast l = fst (unsnoc l)
    
  (** Convert a (bivalent) less-than function into a (three-valued)
      comparison function. *)
  let less_to_cmp less l r = 
    if less r l then 1
    else if less l r then -1
    else 0
      
  (** Remove duplicates from a list, using the given relation to
      determine `duplicates' *)
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
        
  let concat_map f l = 
    let rec aux = function
      | _, [] -> []
      | f, x :: xs -> f x @ aux (f, xs)
    in aux (f,l)
      
  let concat_map_uniq f l = unduplicate (=) (concat_map f l)
    
  let concat_map_undup cmp f l = unduplicate cmp (concat_map f l)
    
  let rec substitute predicate replacement
      = function
        | [] -> []
      | (first::rest) -> 
	  if predicate first then replacement :: rest
	  else first::(substitute predicate replacement rest)

  let iter_over l f = List.iter f l
end
include ListUtils
  
(** {1 Association-list utilities} *)
module AList = 
struct
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
    
  (** alistmap maps f on the contents-parts of the entries, producing a
      new alist *)
  let alistmap f = List.map (cross identity f)

  let alistmapstrcat glue f = 
    (List.map (fun (k, v) -> k ^ " => " ^ f v))
    ->- (String.concat glue) 
    
  (** alistmap' produces an alist by applying f to each element of the
      alist--f should produce a new contents-part for the entry. *)
  let alistmap' f = List.map (fun (x, y) -> (x, f(x, y)))
    
  (** [[map2alist f list]]
      makes an alist that maps [[x]] to [[f x]] for each item in [[list]].
      In category theory this is called the `graph' of f (restricted by [list]).
  *)
  let map2alist f list = List.map (fun x -> (x, f x)) list
  let graph_func f list = map2alist f list

end
include AList

let assoc_list_of_string_map env =
  List.rev (StringMap.fold (fun x y l -> (x, y) :: l) env [])

let zip_string_map_with f env =
  List.rev (StringMap.fold (fun x y l -> (f x y) :: l) env [])

let string_map_of_assoc_list l =
  List.fold_right (fun (x, y) env -> StringMap.add x y env) l StringMap.empty 

(** {1 Strings} *)
module StringUtils = 
struct
  let string_of_char = String.make 1
    
  let string_of_alist = String.concat ", " -<- List.map (fun (x,y) -> x ^ " => " ^ y)
    
  let rec split_string source delim =
    if String.contains source delim then
      let delim_index = String.index source delim in
        (String.sub source 0 delim_index) :: 
          (split_string (String.sub source (delim_index+1) 
                           ((String.length source) - delim_index - 1)) delim)
    else source :: []
      
  let explode : string -> char list = 
    let rec explode' list n string = 
      if n = String.length string then list
      else explode' (string.[n] :: list) (n + 1) string
    in List.rev -<- (explode' [] 0)
      
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

  let start_of ~is s =
    Str.string_match (Str.regexp_string is) s 0 

  let end_of ~is s = 
    let ilen = String.length is
    and slen = String.length s in
      if ilen > slen then false 
      else
        try ignore (Str.search_forward (Str.regexp_string is) s (slen - ilen));
          true
        with Not_found -> 
          false
end
include StringUtils

(** Given a list-of-lists, [groupingsToString] concatenates them using
    [", "] as the delimiter between elements and ["; "] as the delimiter
    between lists. *)
let groupingsToString : ('a -> string) -> 'a list list -> string =
  fun f -> 
    mapstrcat "; " (mapstrcat ", " f)

let numberp s = try ignore (int_of_string s); true with _ -> false

let lines (channel : in_channel) : string list = 
  let input () = 
    try Some (input_line channel)
    with End_of_file -> None
  in
  let rec next_line lines =
    match input () with 
      | Some s -> next_line (s :: lines)
      | None -> lines
  in List.rev (next_line [])

let call_with_open_infile,
  call_with_open_outfile = 
  let call opener closer filename f = 
    let fd = opener filename in
      try
        let rv = f fd in
          closer fd;
          rv
      with x -> 
        closer fd;
        raise x
  in ((fun x -> call open_in close_in x),
      (fun x -> call open_out close_out x))

let process_output : string -> string
  = String.concat "\n" -<- lines -<- Unix.open_process_in

(** [lookup_in alist] is a function that looks up its argument in [alist] *)
let lookup_in alist x = List.assoc x alist

(** lookup is like assoc but uses option types instead of
   exceptions to signal absence *)
let lookup k alist = try Some (List.assoc k alist) with Not_found -> None

let mem_assoc3 key alist : bool = 
  List.exists (fun x -> x = key)
    (List.map fst3 alist)

(*** option types ***)
let opt_map f = function
    None -> None
  | Some x -> Some (f x)
            
type ('a, 'b) either = Left of 'a | Right of 'b

let option_or = function (* !! mplus *)
  | Some x, _ -> Some x
  | _, Some y -> Some y
  | _, _      -> None

let either_partition (f : 'a -> ('b, 'c) either) (l : 'a list)
    : 'b list * 'c list =
  let rec aux (lefts, rights) = function
    | [] -> (List.rev lefts, List.rev rights)
    | x::xs ->
        match f x with 
          | Left l  -> aux (l :: lefts, rights) xs
          | Right r -> aux (lefts, r :: rights) xs
  in aux ([], []) l


module EitherMonad = Monad.MonadPlusUtils(
  struct
    type 'a m = (string, 'a) either
    let return v = Right v
    let (>>=) m k = match m with
      | Left _ as l -> l
      | Right r     -> k r
    let fail msg = Left msg
    let (>>) x y = x >>= fun _ -> y
    let mzero = Left "no error"
    let mplus l r = match l with 
      | Left _ -> r
      | m      -> m
  end)
    
module OptionUtils = 
struct
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
    
  let opt_sequence e =  (* sequence *)
    let rec aux accum = function
      | []             -> Some (List.rev accum)
      | Some x :: rest -> aux (x::accum) rest
      | None :: _      -> None
    in aux [] e
end
include OptionUtils

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



let split3 (s :  ('a * 'b * 'c) list):  'a list * 'b list * 'c list
  =  List.fold_right (fun (x,y,z) (xs,ys,zs) -> (x :: xs, y :: ys, z::zs))  s ([],[],[])

let rec combine3 : 'a list * 'b list * 'c list ->  ('a * 'b * 'c) list = function
  | [], [], [] -> []
  | x::xs, y::ys, z::zs -> (x,y,z) :: combine3 (xs,ys,zs)
  | _          -> invalid_arg "combine3"

(* Any two calls to `gensym' return distinct strings.  The optional
   `prefix' argument can be used to supply a prefix for the string.
*)
let gensym = 
  let counter = ref 0 in
    fun ?prefix:(pref="") () ->
      begin
        incr counter;
        pref ^ "_g" ^ string_of_int !counter
      end


let any_true = List.exists (fun x -> x)

let getenv : string -> string option =
  fun name ->
    try Some (Sys.getenv name)
    with Not_found -> None
