(*pp deriving *)

(** A type is viewed as the application of type constructors to zero
    or more type arguments.  We provide equality and ordering
    operations on types.  The ordering is unspecified, but consistent
    within a process, i.e. sufficient for use in Map etc.

    This might be considered to break abstraction, since it exposes
    the fact that two types are the same, even if that fact has been
    hidden by type abstraction (modules etc.).  This is considered a
    good thing, since it assists with the intended use, which is to
    maximise value sharing.
*)

let memoize f =
  let t = ref None in 
    fun () ->
      match !t with
        | Some t -> t
        | None ->
            let r = f () in
              t := Some r;
              r


module TypeRep :
sig
  type t
  type delayed = unit -> t
  val compare : t -> t -> int
  val eq : t -> t -> bool
  val mkFresh : string -> delayed list -> delayed
  val mkTuple : delayed list -> delayed
  val mkPolyv : (string * delayed option) list -> delayed list -> delayed
end =
struct
  module StringMap = Map.Make(Interned)
  module IntMap = Map.Make(struct type t = int let compare = Pervasives.compare end)
  module StringSet = Set.Make(Interned)

  let counter = ref 0 
  let fresh () = 
    let c = !counter in 
      incr counter;
      c
  type t = 
      [`Variant of (delayed option StringMap.t)
      |`Gen of Interned.t * delayed list ] * int

  and delayed = unit -> t

  let make_fresh row : t =
    (* Just allocate a pointer for now.  Dereference the row later *)
    `Variant row, fresh ()

  module EqualMap =
  struct
    type map = int list IntMap.t
    let equalp : map -> int -> int -> bool
      = fun map l r -> 
        try List.mem r (IntMap.find l map)
        with Not_found -> false

    let record_equality : map -> int -> int -> map =
      fun map l r ->
        let add map l r = 
          try 
            let vals = IntMap.find l map
            in IntMap.add l (r::vals) map
          with Not_found ->
            IntMap.add l [r] map
        in add (add map l r) r l
  end

  let keys : 'a StringMap.t -> StringSet.t =
    fun m -> 
      StringMap.fold (fun k _ set -> StringSet.add k set) m StringSet.empty

  let rec equal : EqualMap.map -> t -> t -> bool
    = fun equalmap (l,lid) (r,rid) ->
      if lid = rid then true
      else if EqualMap.equalp equalmap lid rid then true
      else match l, r with
        | `Variant lrow, `Variant rrow ->
            (* distinct types.  assume they're equal for now; record
               that fact in the map, then look inside the types for
               evidence to the contrary *)
            equal_rows (EqualMap.record_equality equalmap lid rid) lrow rrow
        | `Gen (lname, ls), `Gen (rname, rs) when Interned.eq lname rname ->
            List.for_all2 (fun l r -> equal equalmap (l ()) (r ())) ls rs
        | _ -> false
  and equal_rows equalmap lfields rfields = 
    equal_names lfields rfields
    && StringMap.fold 
      (fun name t eq ->
         let t' = StringMap.find name rfields in
           match t, t' with
             | None, None -> eq
             | Some t, Some t' -> 
                 equal equalmap (t ()) (t' ()) && eq
             | _ -> false)
      lfields
      true
  and equal_names lmap rmap = 
    StringSet.equal (keys lmap) (keys rmap)

  let mkFresh name args =
    let t : t = `Gen (Interned.intern name, args), fresh () in
      fun () -> t

  let mkTuple args = 
    mkFresh (string_of_int (List.length args)) args

  let mkPolyv (args : (string * delayed option) list) (extends : delayed list) : delayed = 
    (* assume all extensions have to be completely known types at this
       point *)
    let initial = 
      List.fold_left
        (fun map extension ->
           match fst (extension ()) with
         | `Variant map' -> 
             StringMap.fold StringMap.add map map'
         | `Gen _ -> assert false)
        StringMap.empty 
        extends
    in
    let row = 
      List.fold_left
        (fun map (name, t) ->
           StringMap.add (Interned.intern name) t map)
        initial
        args in
    let fresh = make_fresh row in
      fun () -> fresh
  let eq = equal IntMap.empty

  let rec compare recargs (lrep,lid as l) (rrep,rid as r) = 
    if eq l r then 0
    else if EqualMap.equalp recargs lid rid then 0
    else match lrep, rrep with 
      | `Gen (lname, ls), `Gen (rname, rs) ->
          begin match Pervasives.compare lname rname with
            | 0 -> 
                begin match Pervasives.compare (List.length ls) (List.length rs) with
                  | 0 -> 
                      List.fold_left2
                        (fun cmp l r -> 
                           if cmp <> 0 then cmp
                           else compare recargs (l ()) (r ()))
                        0 ls rs
                  | n -> n
                end
            | n -> n
          end
      | `Variant lrow, `Variant rrow ->
          compare_rows (EqualMap.record_equality recargs lid rid) lrow rrow
      | `Variant _, `Gen _ -> -1
      | `Gen _, `Variant _ -> 1
  and compare_rows recargs lrow rrow = 
    match StringSet.compare (keys lrow) (keys rrow) with
      | 0 -> StringMap.compare 
          (fun l r -> match l, r with
             | None, None -> 0
             | Some l, Some r -> compare recargs (l ()) (r ())
             | None, Some _ -> -1
             | Some _, None -> 1) lrow rrow
      | n -> n

  let compare = compare IntMap.empty
end

(* Dynamic types *)
type dynamic = Obj.t * TypeRep.t
let tagOf (_, tag) = tag
let untag (obj, tag) target = 
  if TypeRep.eq tag target 
  then Some obj
  else None

(* Signature for type representations *)
module type Typeable =
sig
  type a
  val type_rep : unit -> TypeRep.t
  val has_type : dynamic -> bool
  val cast : dynamic -> a option
  val throwing_cast : dynamic -> a
  val make_dynamic : a -> dynamic
end

exception CastFailure of string

module Defaults (T : (sig
                        type a
                        val type_rep : unit -> TypeRep.t
                      end))
  : Typeable with type a = T.a =
struct
  include T
  let has_type o = tagOf o = type_rep ()
  let cast d =
    match untag d (type_rep ()) with
      | Some c -> Some (Obj.magic c)
      | None -> None
  let make_dynamic o = (Obj.repr o, type_rep ())
  let throwing_cast d = 
    match cast d with
      | None -> (*raise (CastFailure ("cast from type "^
                                      TypeRep.Show_t.show (tagOf d) ^" to type "^
                                      TypeRep.Show_t.show (T.type_rep ()) ^" failed"))*)
          raise (CastFailure "cast failed")
      | Some s -> s
end

module Typeable_list (A:Typeable) : Typeable with type a = A.a list = 
  Defaults(struct type a = A.a list
                  let type_rep = TypeRep.mkFresh "Primitive.list" [A.type_rep]
           end)

module Typeable_option (A:Typeable) : Typeable with type a = A.a option =
  Defaults(struct type a = A.a option
                  let type_rep = TypeRep.mkFresh "Primitive.option" [A.type_rep]
           end)

module Primitive_typeable (T : sig type t val magic : string end) : Typeable with type a = T.t =
  Defaults(struct type a = T.t
                  let type_rep = TypeRep.mkFresh T.magic []
           end)
module Typeable_unit   = Primitive_typeable(struct type t = unit let magic = "Primitive.unit" end)
module Typeable_int    = Primitive_typeable(struct type t = int let magic = "Primitive.int" end)
module Typeable_num    = Primitive_typeable(struct type t = Num.num let magic = "Primitive.Num.num" end)
module Typeable_float  = Primitive_typeable(struct type t = float let magic = "Primitive.float" end)
module Typeable_bool   = Primitive_typeable(struct type t = bool let magic = "Primitive.bool" end)
module Typeable_string = Primitive_typeable(struct type t = string let magic = "Primitive.string" end)
module Typeable_char   = Primitive_typeable(struct type t = char let magic = "Primitive.char" end)

module Typeable_ref(A : Typeable) : Typeable with type a = A.a ref =
  Defaults(struct type a = A.a ref
                  let type_rep = TypeRep.mkFresh "Primitive.ref" [A.type_rep]
           end)

