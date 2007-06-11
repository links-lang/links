(*pp derivingpp *)

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

(* Abstract type for type tags. *)
module Tag : 
sig
  type tag
  val fresh : unit -> tag
end =
struct
  type tag = int
  let fresh = 
    let current = ref 0 in
      fun () -> incr current; !current
end

let memoize f =
  let t = ref None in 
    fun () ->
      match !t with
        | Some t -> t
        | None ->
            let r = f () in
              t := Some r;
              r

type interned = Interned.t
module Show_interned = Show.ShowDefaults(struct
  type a = interned
  let format f s = Show.Show_string.format f (Interned.to_string s)
end)
(* Type of type representations *)
module TypeRep =
struct 
  type t = | Fresh of (interned * delayed list) 
           | Polyv of (interned * int * (string * delayed option) list)
           | Tuple of (int * delayed list)
  and delayed = unit -> t

  let compareList cmp l r =
    try
      List.fold_right2
        (fun l r n ->
           if n <> 0 then n
           else cmp l r)
        l r 0
    with Invalid_argument _ -> -1

  let rec compare : t -> t -> int = 
    (* naive, no-recursion implementation *)
      fun l r ->  match l, r with
        | Fresh (l,ls), Fresh (r,rs) ->
            begin match Interned.compare l r with
              | 0 -> compareList compare' ls rs
              | n -> n end
        | Polyv (_,ln,ls), Polyv (_,rn,rs) -> 
            begin match Pervasives.compare ln rn with
              | 0 -> compareList 
                  (fun (l,lt) (r,rt) -> 
                     match Pervasives.compare l r with
                       | 0 -> 
                           begin match lt, rt with
                             | None, Some _ -> -1
                             | None, None -> 0
                             | Some _, None -> 1
                             | Some lt, Some rt -> compare' lt rt end
                       | n -> n)
                    ls rs
              | n -> n end
        | Tuple (ln,ls), Tuple (rn,rs)  ->
            begin match Pervasives.compare ln rn with
              | 0 -> compareList compare' ls rs
              | n -> n end
        | l, r -> 
            let ctornum = function
              | Fresh _ -> 0
              | Polyv _ -> 1
              | Tuple _ -> 2 
            in
              Pervasives.compare (ctornum l) (ctornum r)
  and compare' (l : unit -> t) (r : unit -> t) = compare (l()) (r())


  let rec eq = (* naive, no-recursion implementation *)
      fun l r ->  try begin match l, r with
        | Fresh (l,ls), Fresh (r,rs) when Interned.eq l r -> List.for_all2 eq' ls rs
        | Polyv (_,ln,ls), Polyv (_,rn,rs) when ln = rn ->
            List.for_all2 (fun (ll,lt) (rl,rt) -> 
                             match lt, rt with
                               | Some lt, Some rt -> ll = rl && eq' lt rt
                               | None, None -> true
                               | _ -> false) ls rs
        | Tuple (ln,ls), Tuple (rn,rs) when ln = rn -> List.for_all2 eq' ls rs
        | _ -> false
      end with Invalid_argument _ -> false
  and eq' l r = eq (l()) (r())

  let mkFresh (magic : string) (args : (unit -> t) list) : unit -> t = 
    let interned = Interned.intern magic in
      memoize (fun () -> Fresh (interned, args))

  let mkTuple tuple =
    memoize (fun () -> Tuple (List.length tuple, tuple))

  let mkPolyv (magic : string) fields extends =
    let interned = Interned.intern magic in
    memoize (fun () -> 
               let efields = 
                 List.concat (List.map (fun f -> match f () with Polyv (_,_,f) -> f
                                          | _ -> assert false) extends) in
                 Polyv (interned, 
                        List.length fields + List.length efields,
                        List.sort (fun (l,_) (r,_) -> String.compare l r)
                          fields @ efields))
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
  val typeRep : unit -> TypeRep.t
  val hasType : dynamic -> bool
  val cast : dynamic -> a option
  val throwingCast : dynamic -> a
  val makeDynamic : a -> dynamic
end

exception CastFailure of string

module Typeable_defaults (T : (sig
                                 type a
                                 val typeRep : unit -> TypeRep.t
                               end))
  : Typeable with type a = T.a =
struct
  include T
  let hasType o = tagOf o = typeRep ()
  let cast d =
    match untag d (typeRep ()) with
      | Some c -> Some (Obj.magic c)
      | None -> None
  let makeDynamic o = (Obj.repr o, typeRep ())
  let throwingCast d = 
    match cast d with
      | None -> (*raise (CastFailure ("cast from type "^
                                      TypeRep.Show_t.show (tagOf d) ^" to type "^
                                      TypeRep.Show_t.show (T.typeRep ()) ^" failed"))*)
          raise (CastFailure "cast failed")
      | Some s -> s
end

module Typeable_list (A:Typeable) : Typeable with type a = A.a list = 
  Typeable_defaults(struct type a = A.a list
                           let typeRep = TypeRep.mkFresh "Primitive.list" [A.typeRep]
                    end)

module Typeable_option (A:Typeable) : Typeable with type a = A.a option =
  Typeable_defaults(struct type a = A.a option
                           let typeRep = TypeRep.mkFresh "Primitive.option" [A.typeRep]
                    end)

module Primitive_typeable (T : sig type t val magic : string end) : Typeable with type a = T.t =
  Typeable_defaults(struct type a = T.t
                           let typeRep = TypeRep.mkFresh T.magic []
                    end)
module Typeable_unit   = Primitive_typeable(struct type t = unit let magic = "Primitive.unit" end)
module Typeable_int    = Primitive_typeable(struct type t = int let magic = "Primitive.int" end)
module Typeable_num    = Primitive_typeable(struct type t = Num.num let magic = "Primitive.Num.num" end)
module Typeable_float  = Primitive_typeable(struct type t = float let magic = "Primitive.float" end)
module Typeable_bool   = Primitive_typeable(struct type t = bool let magic = "Primitive.bool" end)
module Typeable_string = Primitive_typeable(struct type t = string let magic = "Primitive.string" end)
module Typeable_char   = Primitive_typeable(struct type t = char let magic = "Primitive.char" end)

module Typeable_ref(A : Typeable) : Typeable with type a = A.a ref =
  Typeable_defaults(struct type a = A.a ref
                           let typeRep = TypeRep.mkFresh "Primitive.ref" [A.typeRep]
                    end)

