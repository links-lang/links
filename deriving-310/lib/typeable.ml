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

module TypeRep :
sig
  type t
  type delayed = t lazy_t
  val compare : t -> t -> int
  val eq : t -> t -> bool
  val mkFresh : string -> delayed list -> delayed
  val mkTuple : delayed list -> delayed
  val mkPolyv : (string * delayed option) list -> delayed list -> delayed
  val show : t -> string
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

  and delayed = t lazy_t

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
            List.for_all2 (fun l r -> equal equalmap (Lazy.force l) (Lazy.force r)) ls rs
        | _ -> false
  and equal_rows equalmap lfields rfields = 
    equal_names lfields rfields
    && StringMap.fold 
      (fun name t eq ->
         let t' = StringMap.find name rfields in
           match t, t' with
             | None, None -> eq
             | Some t, Some t' -> 
                 equal equalmap (Lazy.force t) (Lazy.force t') && eq
             | _ -> false)
      lfields
      true
  and equal_names lmap rmap = 
    StringSet.equal (keys lmap) (keys rmap)

  let rec show : t -> string = 
    fun (t, id) -> 
        match t with
      | `Variant fields ->
          "["^
            String.concat " | "
            (StringMap.fold
               (fun name _ output -> ("`" ^ Interned.name name ^ " ..") :: output)
               fields
               [])
          ^"]"
        | `Gen (lname, []) -> 
              Interned.name lname
        | `Gen (lname, ts) -> 
            "(" ^ 
              String.concat ", " (List.map (fun d -> show (Lazy.force d)) ts)
            ^ ")" ^ Interned.name lname

  let mkFresh name args =
    let t : t = `Gen (Interned.intern name, args), fresh () in
      Lazy.lazy_from_val t

  let mkTuple args = 
    mkFresh (string_of_int (List.length args)) args

  let mkPolyv (args : (string * delayed option) list) (extends : delayed list) : delayed = 
    (* assume all extensions have to be completely known types at this
       point *)
    let initial = 
      List.fold_left
        (fun map extension ->
           match fst (Lazy.force extension) with
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
      Lazy.lazy_from_val fresh

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
                           else compare recargs (Lazy.force l) (Lazy.force r))
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
             | Some l, Some r -> compare recargs (Lazy.force l) (Lazy.force r)
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
type 'a typeable = {type_rep : TypeRep.delayed }

exception CastFailure of string

let has_type type_rep o = tagOf o = Lazy.force type_rep.type_rep
let cast type_rep d =
  match untag d (Lazy.force type_rep.type_rep) with
    | Some c -> Some (Obj.obj c)
    | None -> None
let make_dynamic type_rep o = (Obj.repr o, Lazy.force type_rep.type_rep)
let mk = make_dynamic
let type_rep t = Lazy.force t.type_rep
let throwing_cast tr d = 
  match cast tr d with
    | None -> 
        (*          raise (CastFailure ("cast from type "^
                    TypeRep.show (tagOf d) ^" to type "^
                    TypeRep.show (T.type_rep ()) ^" failed"))*)
        raise (CastFailure "cast failed")
    | Some s -> s

let typeable_list a = { type_rep = TypeRep.mkFresh "Primitive.list" [a.type_rep] }
let typeable_option a = {type_rep = TypeRep.mkFresh "Primitive.option" [a.type_rep]}

let primitive_typeable (magic : string) = { type_rep = TypeRep.mkFresh magic [] }
let typeable_unit   = primitive_typeable "Primitive.unit"
let typeable_int    = primitive_typeable "Primitive.int"
let typeable_num    = primitive_typeable "Primitive.Num.num"
let typeable_float  = primitive_typeable "Primitive.float"
let typeable_bool   = primitive_typeable "Primitive.bool"
let typeable_string = primitive_typeable "Primitive.string"
let typeable_char   = primitive_typeable "Primitive.char"

let typeable_ref a = {type_rep = TypeRep.mkFresh "Primitive.ref" [a.type_rep] }

let typeable_6 a1 a2 a3 a4 a5 a6 = { type_rep = TypeRep.mkTuple [a1.type_rep; a2.type_rep; a3.type_rep; a4.type_rep; a5.type_rep; a6.type_rep] }

let typeable_5 a1 a2 a3 a4 a5 = { type_rep = TypeRep.mkTuple [a1.type_rep; a2.type_rep; a3.type_rep; a4.type_rep; a5.type_rep] }

let typeable_4 a1 a2 a3 a4 = { type_rep = TypeRep.mkTuple [a1.type_rep; a2.type_rep; a3.type_rep; a4.type_rep] }

let typeable_3 a1 a2 a3 = { type_rep = TypeRep.mkTuple [a1.type_rep; a2.type_rep; a3.type_rep] }

let typeable_2 a1 a2 = { type_rep = TypeRep.mkTuple [a1.type_rep; a2.type_rep] }
