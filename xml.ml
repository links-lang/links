exception Not_implemented of string

let not_implemented message =
  raise (Not_implemented message)

let xml_typing = Settings.add_bool ("xml-typing", false, false)

let to_string_from_to_buffer to_buffer =
  let to_string v =
    let buf = Buffer.create 16 in
    to_buffer buf v;
    Buffer.contents buf in
  to_string

module type Hashed_type = sig
  type t

  val equal : t -> t -> bool

  val hash : t -> int
end

module type Hashed_ordered_type = sig
  type t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val hash : t -> int
end

module Bool = struct
  type t = bool

  let compare = compare

  let equal = (=)

  let hash i = Hashtbl.hash i
end

module Int = struct
  type t = int

  let compare = compare

  let equal = ( = )

  let hash = Hashtbl.hash
end

module String = struct
  include String

  let fold_left f v s =
    let v = ref v in
    for i = 0 to String.length s - 1 do
      v := f !v s.[i]
    done;
    !v

  let fold_right f s v =
    let v = ref v in
    for i = String.length s - 1 downto 0 do
      v := f s.[i] !v
    done;
    !v

  let to_list s = fold_right (fun c l -> c :: l) s []

  let to_list_map f s =  fold_right (fun c l -> f c :: l) s []
end

module Pair (X : Hashed_ordered_type) (Y : Hashed_ordered_type) = struct
  type t = X.t * Y.t

  let compare (x1, y1) (x2, y2) =
    let result = X.compare x1 x2 in
    if result <> 0 then result
    else Y.compare y1 y2

  let equal (x1, y1) (x2, y2) = X.equal x1 x2 && Y.equal y1 y2

  let hash (x, y) = Hashtbl.hash (X.hash x, Y.hash y)
end

module Hash_set (X : Hashed_type) = struct
  module Hash_table = Hashtbl.Make(X)

  type t = unit Hash_table.t

  let create = Hash_table.create

  let clear = Hash_table.clear

  let copy = Hash_table.copy

  let add set item = Hash_table.add set item ()

  let remove = Hash_table.remove

  let mem = Hash_table.mem

  let iter f set = Hash_table.iter (fun k () -> f k) set

  let fold f set v = Hash_table.fold (fun k () v -> f k v) set v

  let length = Hash_table.length
end

module Dyn_array = struct
  type 'a t =
      { mutable contents : 'a array;
        mutable length : int;
        initial_capacity : int }

  let create n =
    { contents = [||];
      length = 0;
      initial_capacity = n }

  let to_array a =
    Array.sub a.contents 0 a.length

  let contents = to_array

  let sub a off len =
    if off + len > a.length then
      invalid_arg "sub";
    Array.sub a.contents off len

  let nth a i =
    if 0 <= i && i < a.length then a.contents.(i)
    else invalid_arg "nth"

  let set a i v =
    if 0 <= i && i < a.length then a.contents.(i) <- v
    else invalid_arg "set"

  let length a = a.length

  let clear a = a.length <- 0

  let reset a =
    a.contents <- [||];
    a.length <- 0

  let rec grow a c =
    let l = Array.length a.contents in
    if l < c then
      begin
        a.contents <-
          Array.init (l * 2) (function i -> a.contents.(min (l - 1) i));
        grow a c
      end

  let add a i =
    if a.contents = [||] then
      begin
        a.contents <- Array.create (max 1 a.initial_capacity) i;
        a.length <- 1;
        0
      end
    else
      let index = a.length in
      let new_length = succ index in
      grow a new_length;
      a.contents.(a.length) <- i;
      a.length <- new_length;
      index

  let append a array off len =
    if array <> [||] then
      begin
        if a.contents = [||] then
          a.contents <- Array.create (max 1 a.initial_capacity) array.(0);
        if off + len > Array.length array then
          invalid_arg "append";
        let new_length = a.length + len in
        grow a new_length;
        flush stderr;
        Array.blit array off a.contents a.length len;
        a.length <- new_length
      end

  let append_whole a array = append a array 0 (Array.length array)
end

module Int_set = struct
  type t = (int * int) list

  let empty = []

  let singleton c = [(c, c + 1)]

  let segment l h = if l < h then [(l, h)] else []

  let rec mem c s =
    match s with
        [] -> false
      | (l, h) :: t ->
          if h <= c then mem c t
          else l <= c

  let rec cup a b =
    match (a, b) with
        ([], _) -> b
      | (_, []) -> a
      | ((a_l, a_h as a_i) :: a_t, (b_l, b_h as b_i) :: b_t) ->
          if a_h < b_l then a_i :: cup a_t b
          else if b_h < a_l then b_i :: cup a b_t
          else if a_h < b_h then cup a_t ((min a_l b_l, b_h) :: b_t)
          else cup ((min a_l b_l, a_h) :: a_t) b_t

  let rec cap a b =
    match (a, b) with
        ([], _) | (_, []) -> []
      | ((a_l, a_h) :: a_t, (b_l, b_h) :: b_t) ->
          if a_h < b_l || b_h < a_l then cap a_t b_t
          else if a_h < b_h then (max a_l b_l, a_h) :: cap a_t b
          else (max a_l b_l, b_h) :: cap a b_t

  let rec diff a b =
    match (a, b) with
        ([], _) -> []
      | (_, []) -> a
      | ((a_l, a_h as a_i) :: a_t, (b_l, b_h) :: b_t) ->
          if a_h < b_l then a_i :: diff a_t b
          else if b_h < a_l then diff a b_t
          else
            let do_tail () =
              if a_h < b_h then diff a_t b
              else if b_h < a_h then diff ((b_h, a_h) :: a_t) b_t
              else diff a_t b_t in
            if a_l < b_l then (a_l, b_l) :: do_tail ()
            else do_tail () 

  let rec leq a b =
    match (a, b) with
        ([], _) -> true
      | (_, []) -> false
      | ((a_l, a_h) :: a_t, (b_l, b_h) :: b_t) ->
          if b_h < a_l then leq a b_t
          else b_l <= a_l && a_h <= b_h && leq a_t b_t

  let to_buffer int_to_buffer buf s =
    let add_segment (l, h) =
      if l = h - 1 then  int_to_buffer l
      else
        begin
          int_to_buffer l;
          Buffer.add_char buf '-';
          int_to_buffer (h - 1);
        end in
    match s with
        [] -> Buffer.add_string buf "(-)"
      | [segment] -> add_segment segment
      | h :: t ->
          Buffer.add_char buf '(';
          add_segment h;
          List.iter (function s -> Buffer.add_char buf '|'; add_segment s) t;
          Buffer.add_char buf ')'

  let to_string int_to_buffer =
    to_string_from_to_buffer (to_buffer int_to_buffer)
end

module String_map = Map.Make (String)

type 'a attribute =
    { key : string;
      value : 'a;
      mandatory : bool }

let attribute_iter f attribute =
  f attribute.value

let attribute_map f attribute =
  { attribute with
      value = f attribute.value }

type 'a attributes =
    { map :  'a attribute String_map.t;
      closed : bool }

let attributes_iter f attributes =
  String_map.iter (fun _ value -> attribute_iter f value) attributes.map

let attributes_map f attributes =
  { attributes with
      map = String_map.map (attribute_map f) attributes.map }

type 'a element =
    { ns : 'a;
      label : 'a;
      attributes : 'a attributes;
      contents : 'a }

let element_iter f element =
  f element.ns;
  f element.label;
  attributes_iter f element.attributes;
  f element.contents

let element_map f element =
  { ns = f element.ns;
    label = f element.label;
    attributes = attributes_map f element.attributes;
    contents = f element.contents }

let list_to_buffer separator item_to_buffer buf list =
  match list with
      [] -> ()
    | head :: tail ->
        item_to_buffer head;
        List.iter
          (function item ->
             Buffer.add_string buf separator;
             item_to_buffer item) tail

module Sequence = struct
  type 'a t = List of 'a | Union of 'a t list

  let empty = List []

  let singleton x = List [x]

  let union sequences =
    match sequences with
        [] -> empty
      | [sequence] -> sequence
      | _ -> Union sequences

  let rec iter f sequence =
    match sequence with
        List list -> List.iter f list
      | Union sequences -> List.iter (iter f) sequences

  let rec fold_left f v sequence =
    match sequence with
        List list -> List.fold_left f v list
      | Union sequences -> List.fold_left (fold_left f) v sequences

  let rec fold_right f sequence v =
    match sequence with
        List list -> List.fold_right f list v
      | Union sequences -> List.fold_right (fold_right f) sequences v

  let add_left item sequence =
    match sequence with
        List list -> List (item :: list)
      | _ -> union [singleton item; sequence]

  let add_right sequence item = union [sequence; singleton item]
end

module type Label_type = sig
  include Hashed_ordered_type

  val leq : t -> t -> bool
end

module Label = struct
  type t =
      Tag
    | Char_set of Int_set.t
    | Top

  let compare = compare

  let hash = Hashtbl.hash

  let equal = ( = )

  let leq a b =
    match (a, b) with
        _, Top
      | Tag, Tag -> true
      | Char_set a, Char_set b -> Int_set.leq a b
      | _ -> false
end

module Make (Label : Label_type) = struct
  module Expr = struct
    type t = {
      mutable hash : int;
      has_leaf : bool;
      mutable labels : label list;
      set : set;
      mutable hashed : bool }
    and label = {
      mutable label_hash : int;
      label : Label.t;
      left : t;
      right : t; }
    and set = {
      mutable contents : t list;
      mutable valid : bool }

    let new_set () = { contents = []; valid = true }

    let create set has_leaf =
      assert (set.valid);
      let expr =
        { hash = 0;
          has_leaf = has_leaf;
          labels = [];
          set = set;
          hashed = false } in
      set.contents <- expr :: set.contents;
      expr

    let add_label expr label left right =
      assert (expr.set.valid && left.set == expr.set && right.set == expr.set);
      expr.labels <-
        { label_hash = 0; label = label; left = left;
          right = right } :: expr.labels

    let hash_deep = 16

    let hash_width = 64

    let rec compute_expr_hash deep width expr =
      compute_unfolded_expr_hash deep width expr.has_leaf expr.labels
    and compute_unfolded_expr_hash deep width has_leaf labels =
      if deep <= 0 || width <= 0 then 0
      else
        let deep = deep - 1 in
        let width = width - 1 in
        let labels_hash =
          List.fold_left
            (fun hash label ->
               (* Odd trick: I use a commutative operator to merge
                  two hash values here to make the hash of the list
                  independent of the order of its elements (the list
                  is not sorted yet, since the sort compare hash
                  values...).*)
               hash +
                 Hashtbl.hash (Label.hash label.label,
                               compute_expr_hash deep width label.left,
                               compute_expr_hash deep width label.right))
            0 labels in
        Hashtbl.hash (has_leaf, labels_hash)

    let immediate_expr set has_leaf labels =
      { hash =
          compute_unfolded_expr_hash hash_deep hash_width has_leaf labels;
        has_leaf = has_leaf;
        labels = labels;
        set = set;
        hashed = true }

    let update_expr_hash expr =
      if not expr.hashed then
        begin
          expr.hash <- compute_expr_hash hash_deep hash_width expr;
          expr.hashed <- true;
        end

    let compare_labels a b = a.label_hash - b.label_hash

    let update_expr expr =
      List.iter
        (function label ->
           update_expr_hash label.left;
           update_expr_hash label.right;
           label.label_hash <-
             Hashtbl.hash (Label.hash label.label, label.left.hash,
                           label.right.hash)) expr.labels;
      update_expr_hash expr;
      expr.labels <- List.fast_sort compare_labels expr.labels

    let close_set set =
      List.iter update_expr set.contents;
      set.valid <- false
  end

  module Type = struct
    module Set_pair_int = Set.Make (Pair (Int) (Int))

    let merge_list_of_lists merger list =
      let rec merge_two_by_two result list =
        match list with
            [] -> result
          | [item] -> item :: result
          | first :: second :: tail ->
              merge_two_by_two (merger first second :: result) tail in
      let rec merge_list_of_lists first list =
        match list with
            [] -> first
          | second :: tail ->
              merge_list_of_lists (merger first second)
                (merge_two_by_two [] tail) in
      match list with
          [] -> invalid_arg "merge_list_of_lists"
        | [result] -> result
        | first :: second :: tail ->
            merge_list_of_lists (merger first second) (merge_two_by_two [] tail)

    open Expr

    let rec inclusion hyp e1 e2 =
      let inclusion_union hyp t u =
        let (has_leaf, labels) =
          merge_list_of_lists
            (fun (has_leaf, labels) (has_leaf', labels') ->
               has_leaf || has_leaf',
               List.merge compare_labels labels labels') u in
        inclusion hyp t (immediate_expr e1.set has_leaf labels) in
      assert (e1.hashed && e2.hashed);
      if Set_pair_int.mem (e1.hash, e2.hash) hyp then Some hyp
      else
        let hyp = Set_pair_int.add (e1.hash, e2.hash) hyp in
        if e1.has_leaf && not e2.has_leaf then None
        else
          let rec inclusion_labels hyp labels1 =
            match labels1 with
                [] -> Some hyp
              | label1 :: tail ->
                  let inclusion_subset hyp s s' =
                    let extract extractor list =
                      List.rev_map
                        (function label ->
                           let expr = extractor label in
                           (expr.has_leaf, expr.labels)) list in
                    let s = extract (function label -> label.left) s in
                    match inclusion_union hyp label1.left s with
                        None -> None
                      | Some new_hyp ->
                          let s' = extract (function label -> label.right) s' in
                          inclusion_union new_hyp label1.right s' in
                  let rec for_all_subsets hyp left right list =
                    match list with
                        [] -> inclusion_subset hyp left right
                      | h :: t ->
                          match for_all_subsets hyp (h :: left) right t with
                              None -> None
                            | Some new_hyp ->
                                for_all_subsets new_hyp left (h :: right) t in
                  let labels2 =
                    List.filter
                      (function label ->
                         Label.leq label1.label label.label) e2.labels in
                  match for_all_subsets hyp [] [] labels2 with
                      None -> None
                    | Some hyp -> inclusion_labels hyp tail in
          inclusion_labels hyp e1.labels
  end
end

module Tree = Make (Label)

module Type = struct
  type t =
      { name : name;
        mutable desc : contents;
        mutable hash : int;
        mutable concrete : bool }
  and contents =
      Not_defined
    | Empty
    | Any
    | Epsilon
    | Char_set of Int_set.t
    | Element of t element
    | Sequence of t list
    | Union of t list
  and name =
      No_name
    | Fixed of string
    | Operator of t * string

  module Show_t = Show.ShowDefaults (struct
    type a = t

    let showBuf _ =
      not_implemented "Xml.Show_t.showBuf"
  end)

  module Pickle_t = Pickle.Pickle_defaults (struct
    type a = t

    let pickle _ =
      not_implemented "Xml.Pickle_t.pickle"

    let unpickle _ =
      not_implemented "Xml.Pickle_t.unpickle"
  end)

  let rec remove_useless useless list =
    match list with
        [] | [_] -> list
      | a :: (b :: _ as tail) ->
          if a.hash = b.hash then remove_useless useless tail
          else if useless a then remove_useless useless tail
          else a :: (remove_useless useless tail)

  let create' name value =
    { name = name;
      desc = value;
      hash = 0;
      concrete = false }

  let create name value =
    let name =
      match name with
          None -> No_name
        | Some name -> Fixed name in
    create' name value

  let rec hash depth width ty =
    if depth <= 0 || width <= 0 then (0, 0)
    else
      let depth = depth - 1 and width = width - 1 in
      let hash_list list =
        List.fold_left
          (fun (value, width) b ->
             let (value', width) = hash depth width b in
             (value + value', width)) (0, width) list in
      match ty.desc with
          Not_defined -> (0, width)
        | Empty -> (1, width)
        | Any -> (2, width)
        | Epsilon -> (3, width)
        | Char_set v -> (Hashtbl.hash (4, Hashtbl.hash v), width)
        | Element element ->
            let (result, width) = hash_element depth width element in
            (Hashtbl.hash (5, result), width)
        | Sequence list ->
            let (result, width) = hash_list list in
            (Hashtbl.hash (6, result), width)
        | Union list ->
            let (result, width) = hash_list list in
            (Hashtbl.hash (7, result), width)
  and hash_element depth width element =
    let (ns, width) = hash depth width element.ns in
    let (label, width) = hash depth width element.label in
    let (attributes, width) = hash_attributes depth width element.attributes in
    let (contents, width) = hash depth width element.contents in
      (Hashtbl.hash (ns, label, attributes, contents), width)
  and hash_attributes depth width attributes =
    let (map, width) =
      String_map.fold
        (fun name attribute (hash, width) ->
           let (attribute, width) = hash_attribute depth width attribute in
           (Hashtbl.hash (hash, name, attribute), width))
        attributes.map (0, width) in
    (Hashtbl.hash (map, attributes.closed), width)
  and hash_attribute depth width attribute =
    let (value, width) = hash depth width attribute.value in
    (Hashtbl.hash
       (Hashtbl.hash attribute.key, value,
        Hashtbl.hash attribute.mandatory), width)

  let to_buffer buf ty =
    let rec to_buffer ancestors ty =
      assert (not (List.memq ty ancestors));
      let ancestors = ty :: ancestors in
      match ty.name with
          Fixed name -> Buffer.add_string buf name
        | Operator (ty, name) ->
            Buffer.add_char buf '(';
            to_buffer ancestors ty;
            Buffer.add_char buf ')';
            Buffer.add_string buf name
        | No_name ->
            match ty.desc with
                Not_defined -> Buffer.add_string buf "???"
              | Empty -> Buffer.add_string buf "Empty"
              | Any -> Buffer.add_string buf "Any"
              | Epsilon -> Buffer.add_string buf "Epsilon"
              | Sequence list ->
                  list_to_buffer " " (to_buffer ancestors) buf list
              | Union list ->
                  Buffer.add_char buf '(';
                  list_to_buffer " | " (to_buffer ancestors) buf list;
                  Buffer.add_char buf ')'
              | Element element ->
                  Buffer.add_char buf '<';
                  to_buffer ancestors element.ns;
                  Buffer.add_char buf ':';
                  to_buffer ancestors element.label;
                  Buffer.add_char buf '>';
                  to_buffer ancestors element.contents;
                  Buffer.add_string buf "</>";
              | Char_set s ->
                  Int_set.to_buffer
                    (function i ->
                       Buffer.add_char buf '\'';
                       Buffer.add_char buf (char_of_int i);
                       Buffer.add_char buf '\'')
                    buf s in
    if Settings.get_value xml_typing then
      to_buffer [] ty
    else
      Buffer.add_string buf "XML"

  let to_string = to_string_from_to_buffer to_buffer

  let rec concrete ty =
    if not (ty.concrete) then
      begin
        ty.hash <- Hashtbl.hash ty;
        ty.concrete <- true;
        try
          begin
            match ty.desc with
                Not_defined -> invalid_arg ("Not defined type: " ^ to_string ty)
              | Empty
              | Any
              | Epsilon
              | Char_set _ -> ()
              | Element element -> element_iter concrete element
              | Sequence list ->
                  List.iter concrete list;
                  (*              ty.desc <- Sequence(
                                  remove_useless (function t -> t.desc = Epsilon) (List.sort (fun a b -> a.hash - b.hash) list));
                                  begin
                                  match ty.desc with
                                  Sequence [] -> ty.desc <- Epsilon
                                  | Sequence [a] -> ty.desc <- a.desc
                                  | _ -> ()
                                  end;
                                  ty.hash <- fst (hash 16 64 ty)*)
              | Union list ->
                  List.iter concrete list;
                  (*              ty.desc <- Union (
                                  remove_useless (function t -> t.desc = Empty) (List.sort (fun a b -> a.hash - b.hash) list));
                                  begin
                                  match ty.desc with
                                  Union [] -> ty.desc <- Empty
                                  | Union [a] -> ty.desc <- a.desc
                                  | _ -> ()
                                  end;
                                  ty.hash <- fst (hash 16 64 ty);*)
          end
        with e ->
          ty.concrete <- false;
          raise e
      end
        

  let set ty desc =
    if ty.desc = Not_defined then ty.desc <- desc
    else invalid_arg "Xml.Type.set: Type already defined"

  let empty = create None Empty

  let any = create None Any

  let element element = create None (Element element)

  let union a b = create None (Union [a; b])

  let concat a b = create None (Sequence [a; b])

  let epsilon = create None Epsilon

  let star t =
    let result = create' (Operator (t, "*")) Not_defined in
    set result (Union [epsilon; create None (Sequence [t; result])]);
    result

  let one_or_more t =
    create' (Operator (t, "+")) (Sequence [t; star t])

  let optional t =
    create' (Operator (t, "?")) (Union [t; epsilon])

  let latin1_char = create (Some "Latin1") (Char_set (Int_set.segment 0 256))

  let digit =
    create None
      (Char_set (Int_set.segment (int_of_char '0') (int_of_char '9' + 1)))

  let latin1_string = star latin1_char

  let digit_string = star digit

  let from_char c =
    create None (Char_set (Int_set.singleton (int_of_char c)))

  let from_string s =
    create None (Sequence (String.to_list_map from_char s))
end

module Inference = struct
  type t = desc Unionfind.point
  and desc = 
      { rules : rule list;
        mutable inferred_type : Type.t;
        index : int (** Just a unique number, to allow fast comparaison. *) }
  and rule =
      Less_than of Type.t
    | Greater_than of Type.t
    | Element of t element
    | Concat of t * t

  module Show_t = Show.ShowDefaults (struct
    type a = t

    let showBuf _ =
      not_implemented "Xml.Show_t.showBuf"
  end)

  module Pickle_t = Pickle.Pickle_defaults (struct
    type a = t

    let pickle _ =
      not_implemented "Xml.Pickle_t.pickle"

    let unpickle _ =
      not_implemented "Xml.Pickle_t.unpickle"
   end)

  module Point = struct
    (* Import the t type of the parent structure. *)
    type point = t

    type t = point

    let compare a b = (Unionfind.find a).index - (Unionfind.find b).index

    let hash point = Hashtbl.hash ((Unionfind.find point).index)

    let equal a b = (Unionfind.find a).index = (Unionfind.find b).index
  end

  module Point_set = Set.Make (Point)

  module Point_hash_set = Hash_set (Point)

  let changed = ref false

  let representants = Point_hash_set.create 64

  let new_desc index rules =
    { rules = rules;
      inferred_type = Type.any;
      index = index }

  let next_index = ref 0

  let create rules =
    changed := true;
    let point = Unionfind.fresh (new_desc !next_index rules) in
    incr next_index;
    Point_hash_set.add representants point;
    point
 
  let fresh () = create []

  let from_type t =
    Type.concrete t;
    let result = create [Greater_than t] in
    (Unionfind.find result).inferred_type <- t;
    result

  let retrieve_type t = (Unionfind.find t).inferred_type

  let change_rules f t = 
    changed := true;
    if Settings.get_value xml_typing then
      let t = Unionfind.repr t in
      let desc = Unionfind.find t in
      Unionfind.change t
        { desc with rules = f desc.rules }

  let less_than t ty =
    change_rules (function rules -> Less_than ty :: rules) t

  let equal_to t ty =
    change_rules (function rules -> Less_than ty :: Greater_than ty :: rules) t

  let greater_than t ty =
    change_rules (function rules -> Greater_than ty :: rules) t

  let empty = from_type Type.empty

  let any = from_type Type.any

  let epsilon = from_type Type.epsilon

  let latin1_char = from_type Type.latin1_char

  let digit = from_type Type.digit

  let latin1_string = from_type Type.latin1_string

  let digit_string = from_type Type.digit_string

  let concat l r = create [Concat (l, r)]

  let element element = create [Element element]

  exception Cyclic of t

  let compute_representant_list () =
    let sorted = Point_hash_set.create (Point_hash_set.length representants) in
    let rec add_point ancestors point stack =
      if Point_hash_set.mem sorted point then stack
      else if Point_set.mem point ancestors then raise (Cyclic point)
      else
        let ancestors = Point_set.add point ancestors in
        Point_hash_set.add sorted point;
        let add_rule stack rule =
          match rule with
              Less_than _ | Greater_than _ -> stack
            | Element element -> add_point ancestors element.contents stack
            | Concat (left, right) ->
                 add_point ancestors left
                   (add_point ancestors right stack) in 
        let stack =
          List.fold_left add_rule stack (Unionfind.find point).rules in
        point :: stack in
    List.rev (Point_hash_set.fold (add_point Point_set.empty) representants [])

  let get_inferred_type point = (Unionfind.find point).inferred_type

  let printed = Hashtbl.create 16

  let solve () =
    let list = compute_representant_list () in
    let solve_point point =
      let desc = Unionfind.find point in
      let collect_rule (build_list, check_list) rule =
        match rule with
            Less_than t -> (build_list, t :: check_list)
          | Greater_than t -> (t :: build_list, check_list)
          | Element element ->
              (Type.create None
                 (Type.Element (element_map get_inferred_type element)) :: build_list,
               check_list)
          | Concat (a, b) ->
              (Type.create None
                 (Type.Sequence [get_inferred_type a; get_inferred_type b])
               :: build_list,
               check_list) in
      let (build_list, check_list) =
        List.fold_left collect_rule ([], []) desc.rules in
      desc.inferred_type <- Type.create None (Type.Union build_list);
      let check_smaller to_type =
        let msg = Type.to_string desc.inferred_type ^ " <= "
           ^ Type.to_string to_type in
        if not (Hashtbl.mem printed msg) then
          begin
            Hashtbl.add printed msg ();
            prerr_endline msg
          end in
      List.iter check_smaller check_list in
    List.iter solve_point list

  let extract_inferred_type v =
    if !changed && Settings.get_value xml_typing then
      begin
        solve ();
        changed := false;
      end;
    (Unionfind.find v).inferred_type

  let to_buffer buf v =
    Type.to_buffer buf (extract_inferred_type v)

  let to_string = to_string_from_to_buffer to_buffer

  let unify a b = 
    if Settings.get_value xml_typing then
      begin
        let a = Unionfind.repr a and b = Unionfind.repr b in
        if not (Unionfind.equivalent a b) then
          let a_desc = Unionfind.find a and b_desc = Unionfind.find b in
          changed := true;
          Point_hash_set.remove representants a;
          Unionfind.union a b;
          Unionfind.change b
            (new_desc b_desc.index (a_desc.rules @ b_desc.rules))
      end

  let same a b =
    if Settings.get_value xml_typing then Unionfind.equivalent a b
    else true
end
