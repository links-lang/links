(*pp deriving *)
(**** Various utility functions ****)

open Notfound

let fst3(x, _, _) = x
let snd3(_, y, _) = y
let thd3(_, _, z) = z

(** {1 Functional combinators} *)
module Functional =
struct

  (** "compose" operators (arrow indicates direction of composition) *)
  let (-<-) f g x = f (g x)
  let (->-) f g x = g (f x)

  let curry f a b = f (a, b)
  let uncurry f (a, b) = f a b
  let identity x = x
  let flip f x y = f y x
  let const x _ = x

  let cross f g = fun (x, y) -> f x, g y
end
include Functional

(** {0 Simulating infix function words (a la Haskell backticks)} *)

(** left-associative *)
let ( <| ) arg f = f arg
let ( |> ) f arg = f arg

(** {0 Maps and sets} *)
module type OrderedShow = sig
  type t
  val compare : t -> t -> int
  module Show_t : Deriving_Show.Show with type a = t
end

module type Map =
sig
  include Map.S
  exception Not_disjoint of key * string

  val filterv : ('a -> bool) -> 'a t -> 'a t
  (** filter by value *)

  val size : 'a t -> int
  (** the number of distinct keys in the map *)

  val to_alist : 'a t -> (key * 'a) list
  (** convert the map to an association list *)

  val from_alist : (key * 'a) list -> 'a t
  (** construct a map from an association list *)

  val to_list : (key -> 'a -> 'b) -> 'a t -> 'b list
  (** contruct a list from a map *)

  val megamap : (key * 'a -> key * 'b) -> 'a t -> 'b t

  val pop : key -> 'a t -> ('a * 'a t)
  (** remove the item with the given key from the map and return the remainder. *)
  val lookup : key -> 'a t -> 'a option
  (** as `find', but return an option instead of raising an exception *)

  val union_disjoint : 'a t -> 'a t -> 'a t
  (** disjoint union *)

  val union_all : ('a t) list -> 'a t
  (** disjoint union of a list of maps *)

  val superimpose : 'a t -> 'a t -> 'a t
  (** Extend the second map with the first *)

  val split_paired : ('a * 'b) t -> ('a t * 'b t)
  (** split a pair map into a pair of maps *)

  val partition : (key -> 'a -> bool) -> 'a t -> ('a t * 'a t)
  (** divide the map by a predicate *)

  module Show_t (A : Deriving_Show.Show) : Deriving_Show.Show
    with type a = A.a t
end

module String = struct
  include String
  module Show_t = Deriving_Show.Show_string
end

module Int = struct
  type t = int
  (*let compare = Pervasives.compare*)
  (*This is a bit of a hack, but should be OK as long as the integers are between 0 and 2^30 or so. *)
  let compare i j = i-j 
  module Show_t = Deriving_Show.Show_int
end

module IntPair = struct
  type t = int * int
	deriving (Show)
  (*let compare = Pervasives.compare*)
  (*This is a bit of a hack, but should be OK as long as the integers are between 0 and 2^30 or so. *)
  let compare (i1,i2) (j1,j2) = if i1 = j1 then i2-j2 else i1-j1
end

module Char = 
struct
  include Char
  module Show_t = Deriving_Show.Show_char
  let isAlpha = function 'a'..'z' | 'A'..'Z' -> true | _ -> false
  let isAlnum = function 'a'..'z' | 'A'..'Z' | '0'..'9' -> true | _ -> false
  let isWord = function 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false
  let isLower = function 'a'..'z' -> true | _ -> false
  let isUpper = function 'A'..'Z' -> true | _ -> false
  let isDigit = function '0'..'9' -> true | _ -> false
  let isXDigit = function '0'..'9'|'a'..'f'|'A'..'F' -> true | _ -> false
  let isBlank = function ' '|'\t' -> true | _ -> false
end

module Map :
sig
  module type OrderedType = OrderedShow
  module type S = Map
  module Make (Ord : OrderedType) : S with type key = Ord.t
end =
struct
  module type OrderedType = OrderedShow
  module type S = Map
  module Make (Ord : OrderedType) = struct
    include Map.Make(Ord)

    exception Not_disjoint of key * string
    module S = Deriving_Show.Show_map(Ord)(Ord.Show_t)

    let find elem map = 
      try find elem map 
      with NotFound _ -> raise (NotFound (Ord.Show_t.show elem ^ 
                                  " (in Map.find)"))
    let filterv f map =
      filter (fun _ -> f) map

    let size m =
      fold (fun _ _ n -> n+1) m 0

    let to_alist map =
      List.rev (fold (fun x y l -> (x, y) :: l) map [])

    let from_alist l =
      List.fold_right (uncurry add) l empty

    let to_list f map =
      List.rev (fold (fun x y l -> (f x y) :: l) map [])

    (** Transform each key-value pair in [m] to a new key-value pair
        by calling [f] and return the resulting [Map]. *)
    let megamap f m = fold (fun k v -> uncurry add (f (k, v))) m empty

    let pop item map =
      (find item map, remove item map)

(* Implemented in NotFound to use original Not_found exception *)
(*  let lookup item map =
      try Some (find item map)
      with NotFound _ -> None
*)

    let union_disjoint a b =
      fold
        (fun k v r -> 
           if (mem k r) then raise (Not_disjoint (k, Ord.Show_t.show k)) 
           else
             add k v r) b a

    let union_all ms = List.fold_right union_disjoint ms empty

    let superimpose a b = fold add b a

    let split_paired m =
      fold
        (fun i (v1, v2) (m1, m2) -> (add i v1 m1, add i v2 m2))
        m (empty, empty)

    let partition f m =
      fold
        (fun i v (p, q) ->
           if (f i v) then
             add i v p, q
           else
             p, add i v q)
        m (empty, empty)

    module Show_t (V : Deriving_Show.Show) = 
      Deriving_Show.Show_map(Ord)(Ord.Show_t)(V)
  end
end

module type Set =
sig
  include Set.S

  val union_all : t list -> t
    (** Take the union of a collection of sets *)

  val from_list : elt list -> t
  (** Construct a set from a list *)

  module Show_t : Deriving_Show.Show with type a = t
end

module Set :
sig
  module type OrderedType = OrderedShow
  module type S = Set
  module Make (Ord : OrderedType) : S with type elt = Ord.t
end =
struct
  module type OrderedType = OrderedShow
  module type S = Set
  module Make (Ord : OrderedType) = struct
    include Set.Make(Ord)
    let union_all sets = List.fold_right union sets empty
    let from_list l = List.fold_right add l empty
    module Show_t = Deriving_Show.Show_set(Ord)(Ord.Show_t)
  end
end

module type INTSET = Set with type elt = int
module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

module IntPairMap = Map.Make(IntPair)

module type STRINGMAP = Map with type key = string
module StringSet = Set.Make(String)
module StringMap : STRINGMAP = Map.Make(String)

module type CHARSET = Set with type elt = char
module CharSet : CHARSET = Set.Make(Char)
module CharMap = Map.Make(Char)

type stringset = StringSet.t
    deriving (Show)

module Typeable_stringset : Deriving_Typeable.Typeable
  with type a = stringset = 
  Deriving_Typeable.Primitive_typeable(struct
    type t = stringset
    let magic = "stringset"
  end)

type 'a stringmap = 'a StringMap.t
    deriving (Show)

type intset = IntSet.t
    deriving (Show)
type 'a intmap = 'a IntMap.t
    deriving (Show)

let list_to_set xs =
  List.fold_right (fun x set -> IntSet.add x set) xs IntSet.empty

(** {0 Lists} *)
module ListUtils =
struct
  (** Test whether the argument is the empty list. *)
  let empty = function [] -> true | _ -> false

  (** [fromTo a b] is the list of consecutive integers starting with
      [a] and ending with [b-1]. Throws [Invalid_argument "fromTo"]
      if [b < a]. *)
  let fromTo f t =
    let rec aux f t result =
      if f = t then result
      else aux (f+1) t (f::result)
    in if (t) < f then raise (Invalid_argument "fromTo")
      else List.rev (aux f t [])

  (** map with index *)
  let mapIndex (f : 'a -> int -> 'b) : 'a list -> 'b list =
    let rec mi i =
      function
        | [] -> []
        | (x :: xs) -> f x i :: mi (i+1) xs
    in
      mi 0

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
    in match list with
      | [] -> []
      | hd::_ -> group (pred hd) [] list

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
  let last l =
    try
      snd (unsnoc l)
    with Invalid_argument _ -> invalid_arg "last"

  (** [curtail list]: Return a copy of the list with the last element removed. *)
  let curtail l =
    try
      fst (unsnoc l)
    with Invalid_argument _ -> invalid_arg "curtail"

  let difference list1 list2 =
    List.filter (fun x -> not (List.mem x list2)) list1

  let remove_all list1 list2 = difference list2 list1

  let subset list1 list2 : bool=
    List.for_all (fun x -> List.mem x list2) list1

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

  let remove x = List.filter ((<>)x)

  let concat_map f l =
    let rec aux = function
      | _, [] -> []
      | f, x :: xs -> f x @ aux (f, xs)
    in aux (f,l)

  let concat_map_uniq f l = unduplicate (=) (concat_map f l)

  let concat_map_undup cmp f l = unduplicate cmp (concat_map f l)

  let for_each l f = List.iter f l

  let push_back f list = list := !list @ [f]
  let push_front f list = list := f :: !list

  let split3 xyzs =
    List.fold_right (fun (x, y, z) (xs, ys, zs) -> x::xs,y::ys,z::zs)
      xyzs
      ([],[],[])

  let split4 wxyzs =
    List.fold_right(fun (w, x, y, z)(ws, xs, ys, zs)-> w::ws,x::xs,y::ys,z::zs)
      wxyzs
      ([],[],[],[])

  let rec filter_map pred f = function
    | [] -> []
    | x::xs ->
        if pred x then (f x)::(filter_map pred f xs) else
          (filter_map pred f xs)

  let print_list xs =
    let rec print_list_inner = function
        | [] -> ""
        | e::[] -> e
        | e::xs -> e ^ ", " ^ (print_list_inner xs) in
    "[" ^ print_list_inner xs ^ "]"

end
include ListUtils

(** {1 Association-list utilities} *)
module AList =
struct
  let rassoc_eq eq : 'b -> ('a * 'b) list -> 'a =
    fun value l ->
      try fst (List.find (snd ->- eq value) l)
      with NotFound _ -> not_found "rassoc_eq" value

  let rassoc i l = rassoc_eq (=) i l
  and rassq i l = rassoc_eq (==) i l

  let rremove_assoc_eq eq : 'b -> ('a * 'b) list -> ('a * 'b) list =
    fun value -> List.filter (not -<- eq value -<- snd)

  let rremove_assoc i l = rremove_assoc_eq (=) i l
  and rremove_assq i l = rremove_assoc_eq (==) i l

  let remove_keys alist keys =
    List.filter (fun (x,_) -> not (List.mem x keys)) alist

  (** alistmap maps f on the contents-parts of the entries, producing a
      new alist *)
  let alistmap f = List.map (cross identity f)

  let show_fgraph ?(glue=", ") f =
    (List.map (fun x -> x ^ " => " ^ f x))
    ->- (String.concat glue)

  (** alistmap' produces an alist by applying f to each element of the
      alist--f should produce a new contents-part for the entry. *)
  let alistmap' f = List.map (fun (x, y) -> (x, f(x, y)))

  (** [[map2alist f list]]
      makes an alist that maps [[x]] to [[f x]] for each item in [[list]].
      This is called the `graph' of f on the domain [list].
  *)
  let map2alist f list = List.map (fun x -> (x, f x)) list
  let graph_func = map2alist

  let range alist = List.map snd alist
  let dom alist = List.map fst alist

  (** [lookup_in alist] is a function that looks up its argument in [alist] *)
  let lookup_in alist x = List.assoc x alist

  (** lookup is like assoc but uses option types instead of
      exceptions to signal absence *)
  let lookup k alist = try Some (List.assoc k alist) with NotFound _ -> None

end
include AList

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

  let is_numeric str =
    List.for_all
      (fun ch -> ch <|List.mem|> ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9'])
      (explode str)

  let implode : char list -> string =
    (String.concat "") -<- (List.rev -<- (List.rev_map (String.make 1)))

  let contains p = explode ->- List.exists p

  (* Find all occurrences of a character within a string *)
  let find_char (s : string) (c : char) : int list =
    let rec aux offset occurrences =
      try let index = String.index_from s offset c in
            aux (index + 1) (index :: occurrences)
      with NotFound _ -> occurrences
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
        with NotFound _ -> false

  let count c str =
    let count = ref 0 in
      begin
        String.iter (function
                       | c' when c = c' -> incr count
                       | _              -> ())
          str;
        !count
      end
end
include StringUtils

(** Given a list-of-lists, [groupingsToString] concatenates them using
    [", "] as the delimiter between elements and ["; "] as the delimiter
    between lists. *)
let groupingsToString : ('a -> string) -> 'a list list -> string =
  fun f ->
    mapstrcat "; " (mapstrcat ", " f)

let numberp s = try ignore (int_of_string s); true with _ -> false

(** {0 File I/O utilities} *)

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
  in ((fun x ?(binary=false) -> call (if binary then open_in_bin else open_in) close_in x),
      (fun x ?(binary=false) -> call (if binary then open_out_bin else open_out) close_out x))

let process_output : string -> string
  = fun proc ->
    let fd = Unix.open_process_in proc in
      try
        let lines = lines fd in
        let rv = String.concat "\n" lines in
          close_in_noerr fd;
          rv
      with e ->
        close_in_noerr fd;
        raise e

let filter_through : command:string -> string -> string =
  fun ~command string ->
    let filename, fd = Filename.open_temp_file "pp" ".links" in
    let () = output_string fd string in
    let () = close_out fd in
    let sh = Printf.sprintf "%s < %s" command filename in
    let filtered = process_output sh in
      Sys.remove filename;
      filtered

(** Is f1 strictly newer than f2, in terms of modification time? *)
let newer f1 f2 =
   ((Unix.stat f1).Unix.st_mtime > (Unix.stat f2).Unix.st_mtime)

(** Given a path name, possibly relative to CWD, return an absolute
    path to the same file. *)
let absolute_path filename =
  if Filename.is_relative filename then
    Filename.concat (Sys.getcwd()) filename
  else filename

(** Is the UID of the process is the same as that of the file's owner? *)
let getuid_owns file =
  Unix.getuid() == (Unix.stat file).Unix.st_uid

(** {0 3-way assoc-list} *)

let mem_assoc3 key : ('a * 'b * 'c) list -> bool =
  List.exists (fun (x,_,_) -> x = key)

(** {0 either type} **)
type ('a, 'b) either = Left of 'a | Right of 'b
  deriving (Show)

let inLeft l = Left l
let inRight r = Right r
let fromLeft = function
  | Left l -> l
  | _ -> assert false
let fromRight = function
  | Right  r -> r
  | _ -> assert false

let either_partition (f : 'a -> ('b, 'c) either) (l : 'a list)
    : 'b list * 'c list =
  let rec aux (lefts, rights) = function
    | [] -> (List.rev lefts, List.rev rights)
    | x::xs ->
        match f x with
          | Left l  -> aux (l :: lefts, rights) xs
          | Right r -> aux (lefts, r :: rights) xs
  in aux ([], []) l


module EitherMonad = Deriving_monad.MonadPlusUtils(
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
  let val_of = function
    | Some x -> x
    | None -> raise EmptyOption

  let is_some = function
    | None -> false
    | Some _ -> true

  let opt_app f default = function
    | None -> default
    | Some a -> f a

  let opt_map f = function
    | None -> None
    | Some x -> Some (f x)

  let opt_split = function
    | None -> None, None
    | Some (x, y) -> Some x, Some y

  let opt_iter f = opt_map f ->- ignore

  let from_option default = function
    | None -> default
    | Some x -> x

  let perhaps_apply f p =
    match f p with
      | None -> p
      | Some x -> x

  let opt_as_list = function
    | None -> []
    | Some x -> [x]

(*
  NOTE:

  The following equations hold

            opt_map f = opt_app (fun x -> Some (f x)) None
           from_option = opt_app (fun x -> x)
    perhaps_apply f p = from_option p (f p)
                      = opt_app (fun x -> x) p (f p)

  I've left the explicit definitions because they are more perspicuous
  than the derived versions and hardly verbose.
*)

  let opt_sequence e =  (* sequence *)
    let rec aux accum = function
      | []             -> Some (List.rev accum)
      | Some x :: rest -> aux (x::accum) rest
      | None :: _      -> None
    in aux [] e
end
include OptionUtils

(** {0 Character Encoding} **)

(** Read a three-digit octal escape sequence and return the
    corresponding char *)

let read_octal c =
  let octal_char = function
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3
    | '4' -> 4 | '5' -> 5 | '6' -> 6 | '7' -> 7 | _ -> invalid_arg "read_octal"
  in Char.chr((octal_char c.[0]) * 64 +
              (octal_char c.[1]) * 8 +
              (octal_char c.[2]))

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

(**Handle escape sequences in string literals.*)
(*
   I would describe them here but the O'Caml lexer gets too confused,
   even though they're in a comment.

   This is here rather than in sl_lexer.mll because the ocamllex gets
   confused by all the backslashes and quotes and refuses to translate
   the file.
*)
let escape_regexp = Str.regexp "\\\\n\\|\\\\r\\|\\\\t\\|\\\\\"\\|\\\\\\\\\\|\\\\[0-3][0-7][0-7]\\|\\\\[xX][0-9a-fA-F][0-9a-fA-F]"
let decode_escapes s =
  let unquoter s =
    (* Yes, the Str interface is stateful.  A pretty poor show.  PCRE
       is better, but we'd rather avoid the dependency and stick with
       the standard library. *)
    let s = Str.matched_string s in
      match s with
        | "\\\"" -> "\""
        | "\\\\" -> "\\"
        | "\\n" -> "\n"
        | "\\r" -> "\r"
        | "\\t" -> "\t"
        | other when other.[1] = 'x' || other.[1] = 'X' ->
            String.make 1 (read_hex (String.sub other 2 2))
        | other -> String.make 1 (read_octal (String.sub other 1 3)) in
    Str.global_substitute escape_regexp unquoter s

(** [xml_escape], [xml_unescape]
    Escape/unescape for XML escape sequences (e.g. &amp;)
*)
let xml_escape s =
  Str.global_replace (Str.regexp "<") "&lt;"
    (Str.global_replace (Str.regexp "&") "&amp;" s)

let xml_unescape s =
  Str.global_replace (Str.regexp "&amp;") "&"
    (Str.global_replace (Str.regexp "&lt;") "<" s)

(** (0 base64 Routines) *)
let base64decode s =
  try Netencoding.Base64.decode (Str.global_replace (Str.regexp " ") "+" s)
  with Invalid_argument "Netencoding.Base64.decode"
      -> raise (Invalid_argument ("base64 decode gave error: " ^ s))

let base64encode = Netencoding.Base64.encode

(** (0 Ocaml Version Comparison) ***)
let ocaml_version_number = (List.map int_of_string
                              (split_string Sys.ocaml_version '.'))

(* Ocaml team says string comparison would work here. Do we believe them? *)
let rec version_atleast a b =
  match a, b with
      _, [] -> true
    | [], _ -> false
    | (ah::at), (bh::bt) -> ah > bh || (ah = bh && version_atleast at bt)
let ocaml_version_atleast min_vsn = version_atleast ocaml_version_number min_vsn

let gensym_counter = ref 0

(** Any two calls to [gensym] return distinct strings.  The optional
    [prefix] argument can be used to supply a prefix for the string.
*)
let gensym =
  let counter = gensym_counter in
    fun ?prefix:(pref="") () ->
      begin
        incr counter;
        pref ^ "_g" ^ string_of_int !counter
      end

(** gensym a new symbol for each item in the list and return the pairs
    of each item with its new name, always using the optional [prefix]
    argument as the prefix if given. The "graph" of the gensym function,
    if you will.
*)
let pair_fresh_names ?prefix:pfx list =
  graph_func
    (match pfx with
       | Some pfx -> (fun _ -> gensym ~prefix:pfx ())
       | None     -> (fun _ -> gensym ()))
    list

(** Given a list of names, generate a fresh name for each and pair the
    old name with the new one. *)
let refresh_names =
  graph_func (fun x -> gensym ~prefix:x ())

(** Return [true] if any element of the given list is [true] *)
let any_true = List.exists identity

(** {0 System interaction} *)

(** Get an environment variable, return [Some x] if it is defined as
    x, or [None] if it is not in the environment. *)
let getenv : string -> string option =
  fun name ->
    try Some (Sys.getenv name)
    with NotFound _ -> None

(** Initialise the random number generator *)
let _ = Random.self_init()


(* This is unpleasant, but we can't just say "include Notfound"
   because of name clashes.
*)
module Buffer = Notfound.Buffer
module Hashtbl = Notfound.Hashtbl
module List = Notfound.List
module ListLabels = Notfound.ListLabels
module MoreLabels = Notfound.MoreLabels
module Str = Notfound.Str
module StringLabels = Notfound.StringLabels
module Sys = Notfound.Sys
module Unix = Notfound.Unix
module UnixLabels = Notfound.UnixLabels

exception NotFound = Notfound.NotFound

(** the integer power function *)
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

(** string of float with a trailing 0 *)
let string_of_float' : float -> string =
  fun f ->
    let s = string_of_float f in
    if String.get s ((String.length s)-1) = '.' then
      s ^ "0"
    else
      s

let time_seconds() = int_of_float (Unix.time())
let time_milliseconds() = int_of_float (Unix.gettimeofday() *. 1000.0)
