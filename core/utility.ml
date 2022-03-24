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
let ( <| ) f arg = f arg
let ( |> ) arg f = f arg

(** {0 Maps and sets} *)
module type OrderedShow = sig
  type t [@@deriving show]
  val compare : t -> t -> int
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
  (** construct a list from a map *)

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

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  (** filters using both keys and values *)

  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
  (** filters and applies a function -- None values discarded *)

  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module String = struct
  include String
  let pp = Format.pp_print_string
  let show = fun x -> x

  let rec blits dest pos sep seplen = function
    | [] -> dest
    | [xs] ->
       blit xs 0 dest 0 (length xs); dest
    | xs :: xss ->
       let pos = pos - length xs in
       blit xs 0 dest pos (length xs);
       let pos = pos - seplen in
       blit sep 0 dest pos seplen;
       blits dest pos sep seplen xss

  let rev_concat sep = function
    | [] -> ""
    | xs ->
       let seplen = length sep in
       let buffer_size =
         let rec compute_size acc seplen = function
           | [] -> acc
           | xs :: [] -> length xs + acc
           | xs :: xss -> compute_size (length xs + seplen + acc) seplen xss
         in
         compute_size 0 seplen xs
       in
       let buffer =
         blits (Bytes.create buffer_size) buffer_size sep seplen xs
       in
       Bytes.to_string buffer
end

module Int = struct
  type t = int
  (*This is a bit of a hack, but should be OK as long as the integers are between 0 and 2^30 or so. *)
  let compare i j = i-j
  let pp = Format.pp_print_int
  let show = string_of_int
end

module IntPair = struct
  type t = int * int
    [@@deriving show]
  (*This is a bit of a hack, but should be OK as long as the integers are between 0 and 2^30 or so. *)
  let compare (i1,i2) (j1,j2) = if i1 = j1 then i2-j2 else i1-j1
end

module Char =
struct
  include Char

  let pp = Format.pp_print_char
  let show = fun x  -> Format.asprintf "%a" pp x
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

    let lookup elem map =
      try Some (find elem map)
      with NotFound _ -> None

    let find elem map =
      try find elem map
      with NotFound _ -> raise (NotFound (Ord.show elem ^
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
           if (mem k r) then raise (Not_disjoint (k, Ord.show k))
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

    let filter_map f m =
      fold (fun k v acc ->
        match f k v with
          | Some x -> add k x acc
          | None -> acc) m empty

    let filter f =
      filter_map (fun k v ->
        if f k v then Some v else None)

    let pp af formatter map =
      Format.pp_open_box formatter 0;
      Format.pp_print_string formatter "{";
      iter (fun key value ->
                Format.pp_open_box formatter 0;
                Ord.pp formatter key;
                Format.pp_print_string formatter " => ";
                af formatter value;
                Format.fprintf formatter ";@;";
                Format.pp_close_box formatter ();
             ) map;
      Format.pp_print_string formatter "}";
      Format.pp_close_box formatter ()

    let show = (fun af x  -> Format.asprintf "%a" (pp af) x)
  end
end

module type Set =
sig
  include Set.S

  val union_all : t list -> t
    (** Take the union of a collection of sets *)

  val from_list : elt list -> t
  (** Construct a set from a list *)

  val pp : Format.formatter -> t -> unit
  val show : t -> string
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

    let pp formatter set =
      Format.pp_open_box formatter 0;
      Format.pp_print_string formatter "{";
      iter (fun elt ->
                Format.pp_open_box formatter 0;
                Ord.pp formatter elt;
                Format.fprintf formatter ";@;";
                Format.pp_close_box formatter ();
             ) set;
      Format.pp_print_string formatter "}";
      Format.pp_close_box formatter ()

    let show : t -> string = fun x  -> Format.asprintf "%a" pp x
  end
end

module type INTSET = Set with type elt = int
module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

module IntPairMap = Map.Make(IntPair)

module type STRINGMAP = Map with type key = string
module type INTMAP = Map with type key = int
module StringSet = Set.Make(String)
module StringMap : STRINGMAP = Map.Make(String)

module type CHARSET = Set with type elt = char
module CharSet : CHARSET = Set.Make(Char)
module CharMap = Map.Make(Char)

type stringset = StringSet.t
    [@@deriving show]

(**module Typeable_stringset : Deriving_Typeable.Typeable
  with type a = stringset =
  Deriving_Typeable.Primitive_typeable(struct
    type t = stringset
    let magic = "stringset"
  end)**)

type 'a stringmap = 'a StringMap.t
    [@@deriving show]

type intset = IntSet.t
    [@@deriving show]
type 'a intmap = 'a IntMap.t
    [@@deriving show]

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

  (** [unsnoc_opt list]: Partition [list] into its last element and all the
     others. @return Some (others, lastElem) or None if the list is empty. *)
  let unsnoc_opt = function
    | [] -> None
    | xs -> Some (unsnoc xs)

  (** [last list]: Return the last element of a list *)
  let rec last = function
    | [x] -> x
    | _ :: xs -> last xs
    | [] -> invalid_arg "last"

  (** [last_opt list]: Return the last element of a list, or None if the list is
     empty. *)
  let last_opt = function
    | [] -> None
    | xs -> Some (last xs)

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

  (** Checks whether list contains duplicates *)
  let rec has_duplicates = function
    | []        -> false
    | (x :: xs) -> List.mem x xs || has_duplicates xs

  (** Remove duplicates from a list, using the given relation to
      determine `duplicates' *)
  let rec unduplicate equal = function
    | [] -> []
    | elem :: elems -> (let _, others = List.partition (equal elem) elems in
                          elem :: unduplicate equal others)

  (** Collects only elements which are duplicate in the original list. *)
  let rec collect_duplicates equal = function
    | [] -> []
    | elem :: elems -> (let same, others = List.partition (equal elem) elems in
                        if empty same
                        then collect_duplicates equal others
                        else elem :: collect_duplicates equal others)

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

  let rec split n list = match n, list with
    | 0, xs -> ([], xs)
    | _, [] -> ([], [])
    | _, h :: t ->
       let (x, y) = split (n - 1) t in
       (h :: x, y)

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

  let drop_nth xs n =
    (take n xs) @ (drop (n+1) xs)

  let rec filter_map pred f = function
    | [] -> []
    | x::xs ->
        if pred x then (f x)::(filter_map pred f xs) else
          (filter_map pred f xs)


  exception Lists_length_mismatch

  (** Filter on two lists and map them together.
      Equivalent to map -<- filter -<- zip
      precondition: the two lists must be the same length *)
  let rec filter_map2 pred f =
    fun xs ys ->
    match (xs, ys) with
    | ([], []) -> []
    | (x::xs, y::ys) ->
       if pred (x, y)
       then (f (x, y))::(filter_map2 pred f xs ys)
       else filter_map2 pred f xs ys
    | _ -> raise Lists_length_mismatch

  let rec map_filter f pred = function
    | [] -> []
    | x :: xs ->
       let y = f x in
       if pred y
       then y :: (map_filter f pred xs)
       else (map_filter f pred xs)

  let print_list xs =
    let rec print_list_inner = function
        | [] -> ""
        | e::[] -> e
        | e::xs -> e ^ ", " ^ (print_list_inner xs) in
    "[" ^ print_list_inner xs ^ "]"

  let rec zip xs ys =
    match xs, ys with
    | [], _
    | _, [] -> []
    | x :: xs, y :: ys -> (x, y) :: zip xs ys

  let rec zip_with f xs ys =
    match xs, ys with
    | [], _
    | _, [] -> []
    | x :: xs, y :: ys -> f x y :: zip_with f xs ys

  let split_with : ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list = fun f xs ->
    List.fold_right (fun a (bs, cs) -> let (b, c) = f a in (b::bs, c::cs))
                    xs ([], [])

  let rec zip' xs ys =
    match xs, ys with
    | [], [] -> []
    | x :: xs, y :: ys -> (x, y) :: zip' xs ys
    | _, _ -> raise Lists_length_mismatch

  let rec zip_with' f xs ys =
    match xs, ys with
    | [], [] -> []
    | x :: xs, y :: ys -> f x y :: zip_with' f xs ys
    | _, _ -> raise Lists_length_mismatch

  let rec transpose : 'a list list -> 'a list list = function
    | [] -> []
    | [] :: xss -> transpose xss
    | (x :: xs) :: xss ->
       (x :: (List.map List.hd xss)) :: transpose (xs :: List.map List.tl xss)
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

  (* FIXME: consolidate split_string and split (suspicion:
   split_string doesn't properly deal with failure whereas split
   does) *)

  let rec split_string source delim =
    if String.contains source delim then
      let delim_index = String.index source delim in
        (String.sub source 0 delim_index) ::
          (split_string (String.sub source (delim_index+1)
                           ((String.length source) - delim_index - 1)) delim)
    else source :: []

  (* taken from the internals of cgi *)
  let split separator text =
    let len = String.length text in
    let rec loop pos =
      if pos < len then
        try
          let last = String.index_from text pos separator in
          let str = String.sub text pos (last-pos) in
          str::(loop (succ last))
        with NotFound _ ->
          if pos < len then [String.sub text pos (len-pos)]
          else []
      else []
    in
    loop 0

  let explode : string -> char list =
    let rec explode' list n string =
      if n = String.length string then list
      else explode' (string.[n] :: list) (n + 1) string
    in List.rev -<- (explode' [] 0)

  let is_numeric str =
    List.for_all
      (fun ch -> ch |>List.mem<| ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9'])
      (explode str)

  let implode : char list -> string =
    (String.concat "") -<- (List.rev -<- (List.rev_map (String.make 1)))

  let contains p = explode ->- List.exists p

  (* Find all occurrences of a character within a string *)
  let find_char (s : bytes) (c : char) : int list =
    let rec aux offset occurrences =
      try let index = Bytes.index_from s offset c in
            aux (index + 1) (index :: occurrences)
      with Not_found -> occurrences
    in List.rev (aux 0 [])

  let mapstrcat glue f list = String.concat glue (List.map f list)

  let string_starts_with s pref =
    String.length s >= String.length pref &&
    String.sub s 0 (String.length pref) = pref

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

  let replace pattern replacement =
    Str.global_replace (Str.regexp_string pattern) replacement
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
  [@@deriving show]

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


(** This module isn't used but creates a dependency on deriving,
which we would like to avoid
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
  end)**)

(* Extensions of queue module to handle queue -> list and list -> queue
 * conversions *)
module Queue = struct
  include Queue
  let to_list q =
    List.rev @@ Queue.fold (fun acc x -> x :: acc) [] q

  let of_list xs =
    let q = Queue.create () in
    List.iter (fun x -> Queue.add x q) xs;
    q
end

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

  let opt_bind f = function
    | None -> None
    | Some a -> f a

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

  let some : 'a -> 'a option
    = fun x -> Some x

  let (>>=?) o f = opt_bind f o

  (* option-disjunction *)
  let (||=?) o o' =
    match o with
    | None -> o'
    | _ -> o

  let rec (>>==?) (l : 'a list) (f : 'a -> 'a option) : 'a list option =
    match l with
    | [] -> None
    | a::al ->
        match f a, al >>==? f with
        | None, None -> None
        | fa, fal ->
            Some (from_option a fa::from_option al fal)

  let map_tryPick f m =
    StringMap.fold
      (fun k v acc -> lazy (match f k v with
        | None -> Lazy.force acc
        | y -> y))
      m
      (lazy None)
    |> Lazy.force

  let rec list_tryPick f = function
    | [] -> None
    | x::l -> match f x with
      | None -> list_tryPick f l
      | y -> y

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
let escape_regexp = Str.regexp "\\\\b\\|\\\\n\\|\\\\r\\|\\\\t\\|\\\\\"\\|\\\\\\\\\\|\\\\[0-3][0-7][0-7]\\|\\\\[xX][0-9a-fA-F][0-9a-fA-F]"
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
        | "\\b" -> "\b"
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
  try Base64.decode_exn (Str.global_replace (Str.regexp " ") "+" s)
  with Invalid_argument s as e ->
    if s = "B64.decode" then
      raise (Invalid_argument ("base64 decode gave error: " ^ s))
    else
      raise e

let base64encode s =
  (* We may want to use Base64.uri_safe_alphabet rather than the default alphabet *)
  Base64.encode_exn ~alphabet:Base64.default_alphabet ~pad:true s

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

(** Get an environment variable, return its value if it is defined, or
    raise an exception if it is not in the environment. *)
let safe_getenv s =
  try Sys.getenv s
  with NotFound _ ->
    (* We need to retain this `failwith` since `errors.ml` depends on
     * Utility.ml *)
    failwith ("The environment variable " ^ s ^ " is not set")

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
module Printexc = Notfound.Printexc

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
let time_microseconds() = int_of_float (Unix.gettimeofday() *. 1000000.0)

let strip_leading_slash s =
  if s = "" then s else
  if s = "/" then "" else
  if s.[0] = '/' then String.sub s 1 ((String.length s) - 1) else s

let strip_trailing_slash s =
  if s = "" then s else
  if s = "/" then "" else
  if s.[(String.length s) - 1] = '/' then
    String.sub s 0 ((String.length s) - 1) else
    s

let strip_slashes = (strip_leading_slash -<- strip_trailing_slash)


let format_omission : Format.formatter -> unit = fun fmt -> Format.pp_print_string fmt "..."

module Disk: sig
  exception End_of_stream
  exception AccessError of string
  exception BadLink

  type dir_t
  type file_t
  type link_t

  type item = Directory of dir_t
            | File of file_t
            | Link of link_t
  type inode = { no: int; data: item }

  module Directory: sig
    type t = dir_t
    val basename : t -> string
    val dirname : t -> string
    val to_filename : t -> string
    (* May raise [AccessError]. *)
    val of_path : string -> t
  end

  module File: sig
    type t = file_t
    val basename : t -> string
    val dirname : t -> string
    val relative_name : t -> string
    val to_filename : t -> string
  end

  module Link: sig
    type t = link_t
    (* May raise [BadLink]. *)
    val follow : t -> inode
  end

  module Iterator: sig
    type t

    (* May raise [End_of_stream] or [AccessError]. *)
    val next : t -> inode
    val finalise : t -> unit
    (* May raise [AccessError]. *)
    val of_directory : Directory.t -> t
  end
end = struct

  exception End_of_stream
  exception AccessError of string
  exception BadLink

  type dir_t =
    { root: string;
      rev_suffix: string list }
  type file_t = dir_t
  type link_t = dir_t

  let to_string root rev_suffix =
    Filename.concat root (String.rev_concat Filename.dir_sep rev_suffix)

  let to_filename f =
    to_string f.root f.rev_suffix

  let basename { rev_suffix; _ } =
    List.hd rev_suffix

  let dirname { root; rev_suffix } =
    Filename.concat root (String.rev_concat Filename.dir_sep (List.tl rev_suffix))

  let make root rev_suffix = { root; rev_suffix }

  type item =
    | Directory of dir_t
    | File of file_t
    | Link of link_t
  and inode = { no: int; data: item }

  let make_file no f = { no; data = File f }
  let make_link no l = { no; data = Link l }
  let make_dir no d = { no; data = Directory d }

  open Unix
  module rec File: sig
    type t = file_t
    val basename : t -> string
    val relative_name : t -> string
    val dirname : t -> string
    val make : string -> string list -> t
    val to_filename : t -> string
  end = struct
    type t = file_t
    let basename = basename
    let dirname = dirname
    let relative_name f =
      String.rev_concat Filename.dir_sep f.rev_suffix
    let make = make
    let to_filename = to_filename
  end and Directory: sig
    type t = dir_t
    val basename : t -> string
    val dirname : t -> string
    val to_filename : t -> string
    val make : string -> string list -> t
    val of_path : string -> t
  end = struct
    type t = dir_t
    let basename = basename
    let dirname = dirname
    let to_filename = to_filename
    let make = make

    let of_path path =
      if Sys.file_exists path && Sys.is_directory path
      then make path []
      else raise (AccessError path)

  end and Iterator: sig
    type t
    val next : t -> inode
    val finalise : t -> unit
    val of_directory : Directory.t -> t
  end = struct
    type t =
      { handle: Unix.dir_handle;
        file_obj: file_t }

    let finalise { handle; _ } =
      try closedir handle
      with Unix_error (Unix.EBADF, "closedir", _) -> ()

    let rec next it =
      let exception Next in
      try
        let item = readdir it.handle in
        (if String.equal "." item || String.equal ".." item
         then raise Next);
        let suffix = item :: it.file_obj.rev_suffix in
        let node = lstat (to_string it.file_obj.root suffix) in
        match node.st_kind with
        | S_REG -> make_file node.st_ino (File.make it.file_obj.root suffix)
        | S_DIR -> make_dir node.st_ino (Directory.make it.file_obj.root suffix)
        | S_LNK -> make_link node.st_ino (Link.make it.file_obj.root suffix)
        | _ -> raise Next
      with
      | Next -> next it
      | End_of_file ->
         finalise it; raise End_of_stream

    let of_directory dir =
      let filepath = Directory.to_filename dir in
      if Sys.file_exists filepath && Sys.is_directory filepath
      then
        try { handle = opendir filepath;
              file_obj = dir }
        with _ -> raise (AccessError filepath)
      else raise (AccessError filepath)

  end and Link: sig
    type t = link_t
    val follow : t -> inode
    val make : string -> string list -> t
  end = struct
    type t = link_t

    let follow link =
      let node =
        try Unix.stat (to_filename link)
        with Unix.Unix_error _ -> raise (AccessError (to_filename link))
      in
      match node.st_kind with
      | S_REG -> make_file node.st_ino (File.make link.root link.rev_suffix)
      | S_DIR -> make_dir node.st_ino (Directory.make link.root link.rev_suffix)
      | S_LNK -> make_link node.st_ino (Link.make link.root link.rev_suffix)
      | _ -> raise BadLink

    let make = make
  end
end

module type GLOB_POLICY = sig
  val symlinks : [`AlwaysFollow | `FollowOnce | `FollowOnceQuietly ]
  val scan_depth : [`Bounded of int | `SystemDefault ]
end

module DefaultPolicy : GLOB_POLICY = struct
  let symlinks = `FollowOnceQuietly
  let scan_depth = `SystemDefault
end

module Glob: sig
  module type S = sig
    exception CyclicLinkError
    val files : string -> Str.regexp -> Disk.File.t list
    val files_ext : string -> string list -> Disk.File.t list
  end

  module Make(P : GLOB_POLICY): sig
    include S
  end
end = struct
  module type S = sig
    exception CyclicLinkError
    val files : string -> Str.regexp -> Disk.File.t list
    val files_ext : string -> string list -> Disk.File.t list
  end

  module Make(P : GLOB_POLICY) = struct
    exception CyclicLinkError
    type state =
      { mutable todo: (int * Disk.Directory.t Queue.t) list;
        mutable files: Disk.File.t list;
        visited: (int, unit) Hashtbl.t }

    let empty () = { todo = []; files = []; visited = Hashtbl.create 17 }

    let is_match : Str.regexp -> Disk.File.t -> bool
      = fun pattern file ->
      match Str.search_forward pattern (Disk.File.basename file) 0 with
      | _ -> true
      | exception Notfound.NotFound _ -> false

    let depth { todo; _ } =
      match todo with
      | [] -> assert false
      | (d, _) :: _ -> d

    let max_depth = match P.scan_depth with
      | `Bounded max -> max
      | `SystemDefault -> max_int

    let follow_symlink st inode =
      match P.symlinks with
      | `FollowOnce ->
         if Hashtbl.mem st.visited inode.Disk.no
         then raise CyclicLinkError
         else true
      | `FollowOnceQuietly -> not (Hashtbl.mem st.visited inode.Disk.no)
      | `AlwaysFollow -> true

    let add_todo st dir =
      match st.todo with
      | [] -> assert false
      | (_, q) :: _ ->
         Queue.push dir q

    open Disk
    let files root pattern =
      let rec scan_next st pattern =
        match st.todo with
        | [] -> assert false
        | [(_, q)] when Queue.is_empty q -> st.files
        | (_, q) :: todo when Queue.is_empty q ->
           st.todo <- todo; scan_next st pattern
        | (_, q) :: _ ->
           let dir = Queue.pop q in
             begin match try Some (Iterator.of_directory dir)
                         with AccessError _ -> None
             with
             | None -> scan_next st pattern
             | Some it -> loop st pattern it
             end
      and loop st pattern it =
        match Iterator.next it with
        | inode -> process_inode st pattern it inode
        | exception AccessError _ ->
           loop st pattern it
        | exception End_of_stream ->
           Iterator.finalise it;
           scan_next st pattern
      and process_inode st pattern it inode =
        match inode.data with
        | File file when is_match pattern file ->
           st.files <- file :: st.files;
           loop st pattern it
        | Directory dir when depth st < max_depth ->
           add_todo st dir;
           loop st pattern it
        | Link link when follow_symlink st inode ->
           Hashtbl.add st.visited inode.no ();
           begin match Disk.Link.follow link with
           | inode -> process_inode st pattern it inode
           | exception Disk.BadLink -> loop st pattern it
           end
        | _ -> loop st pattern it
      in
      let st =
        let q = Queue.create () in
        Queue.push (Disk.Directory.of_path root) q;
        { (empty ()) with todo = [(0, q)] }
      in
      scan_next st pattern

    let files_ext root extensions =
      let pattern = Printf.sprintf "\\.\\(%s\\)$" (String.concat "\\|" extensions) in
      files root (Str.regexp pattern)
  end
end

(* Looks for a given file, either in the current directory or in the Links opam path *)
let locate_file filename =
  (* If LINKS_LIB is not defined then we search in current directory *)
  let executable_dir = Filename.dirname Sys.executable_name in
  if Sys.file_exists (Filename.concat executable_dir filename) then
    executable_dir
  else try
      (* If all else failed we search for OPAM installation of Links and
         use a prelude that it provides *)
      let opam_links_lib =
        input_line (Unix.open_process_in "opam config var links:lib 2>/dev/null") in
      if Sys.file_exists (Filename.concat opam_links_lib filename)
      then opam_links_lib
      else (* But if no OPAM installation exists we fall back to current
              directory so that user gets a reasonable error message *)
        executable_dir
    with End_of_file ->
      (* User probably does not have OPAM, so fall back to current directory *)
      executable_dir


module LwtHelpers =
struct
  open Lwt.Infix

  let foldl_lwt : ('a -> 'b -> 'a Lwt.t) -> 'a Lwt.t -> 'b list -> 'a Lwt.t =
    fun f z xs ->
      let rec go acc xs =
        match xs with
          | [] -> acc
          | x :: xs ->
              acc >>= fun acc ->
              go (f acc x) xs in
      go z xs

  let rec foldr_lwt : ('a -> 'b -> 'b Lwt.t) -> 'a list -> 'b Lwt.t -> 'b Lwt.t =
    fun f xs acc ->
        match xs with
          | [] -> acc
          | x :: xs ->
              (foldr_lwt f xs acc) >>= fun acc ->
              f x acc

  (* sequence : [m a] -> m [a] *)
  let rec sequence : ('a Lwt.t) list -> ('a list) Lwt.t  = function
    | [] -> Lwt.return []
    | x :: xs ->
        x >>= fun x ->
        (sequence xs) >>= fun xs ->
        Lwt.return (x :: xs)
end


(* efficient polymorphic buffers *)
(* builds an array of n pages of size m, with some initial dummy value *)
(* allows random access reading/writing and appending at the end *)
module PolyBuffer : sig
  type 'a buf
  val init : int -> int -> 'a -> 'a buf
  val length : 'a buf -> int
  val get : 'a buf -> int -> 'a
  val set : 'a buf -> int -> 'a -> unit
  val append : 'a buf -> 'a -> unit
  val to_list : 'a buf -> 'a list
end =
struct
  type 'a buf = {mutable numpages: int;
                pagesize: int;
                default: 'a;
                mutable currpage: int;
                mutable nextitem: int;
                mutable pages:'a
                array array}

  let init n m x = {numpages = n;
                    pagesize = m;
                    default = x;
                    currpage = 0;
                    nextitem = 0;
                    pages = Array.init n (fun _ -> Array.init m (fun _ -> x)) }

  let length buf = buf.currpage*buf.pagesize + buf.nextitem

  let set buf i x =
  if 0 <= i && i < buf.currpage*buf.pagesize + buf.nextitem
    then Array.set (Array.get buf.pages (i/buf.pagesize)) (i mod buf.pagesize) x
    else raise Not_found

  let get buf i =
    if 0 <= i && i < buf.currpage*buf.pagesize + buf.nextitem
    then Array.get (Array.get buf.pages (i/buf.pagesize)) (i mod buf.pagesize)
    else raise (Invalid_argument "index out of bounds")

  let append buf x =
    (* first, check if there is enough space or allocation is needed *)
    if (buf.nextitem = buf.pagesize)
    then begin
      buf.nextitem <- 0;
      buf.currpage <- buf.currpage+1;
      if (buf.currpage = buf.numpages)
      then begin (* need to allocate a new page and copy over *)
        buf.numpages <- buf.numpages+1;
        let newpages = Array.init buf.numpages (fun i ->
                          if i < Array.length(buf.pages)
                          then Array.get buf.pages i
                          else Array.init buf.pagesize (fun _ -> buf.default)) in
        buf.pages <- newpages
      end
    end;
    Array.set (Array.get buf.pages buf.currpage) buf.nextitem x;
    buf.nextitem <- buf.nextitem + 1

  let to_list buf = List.init (length buf) (get buf)
end

module CalendarShow = struct
  include CalendarLib.Fcalendar.Precise

  let pp ppf x =
    Format.fprintf ppf "%04d-%02d-%02d %02d:%02d:%09.6f"
      (year x) (month x |> CalendarLib.Date.int_of_month)
      (day_of_month x) (hour x) (minute x) (second x)

  let show x =
      Format.asprintf "%a" pp x
end

module UnixTimestamp = struct
  let of_calendar cal =
    let tm = {
      Unix.tm_sec = CalendarShow.second cal |> int_of_float;
      Unix.tm_min = CalendarShow.minute cal;
      Unix.tm_hour = CalendarShow.hour cal;
      Unix.tm_mday = CalendarShow.day_of_month cal;
      Unix.tm_mon = (CalendarShow.month cal |> CalendarLib.Date.int_of_month) - 1;
      Unix.tm_year = (CalendarShow.year cal) - 1900;
      Unix.tm_wday = 0; (* ignored *)
      Unix.tm_yday =  0; (* ignored *)
      Unix.tm_isdst = false (* ignored *)
    } in
    Unix.mktime tm |> fst

  let to_calendar tm =
    (CalendarShow.lmake
      ~year:(tm.Unix.tm_year + 1900)
      ~month:(tm.Unix.tm_mon + 1)
      ~day:tm.Unix.tm_mday
      ~hour:tm.Unix.tm_hour
      ~minute:tm.Unix.tm_min
      ~second:(float_of_int tm.Unix.tm_sec) ())

  let to_local_calendar t =
    Unix.localtime t |> to_calendar

  let to_utc_calendar t =
    Unix.gmtime t |> to_calendar
end

module IO = struct
  module Channel = struct
    let cat : in_channel -> out_channel -> unit
      = fun ic oc ->
      try
        let rec loop () =
          output_string oc (input_line ic ^ "\n"); loop ()
        in
        loop ()
      with End_of_file -> ()
  end
end
