

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
(* Type of type representations *)
module TypeRep =
struct 
  type t = | Fresh of (Interned.t * t list) 
           | Polyv of (string * t) list
           | Tuple of t list
  type delayed = unit -> t
  let compare : t -> t -> int = compare
  let eq = (=)
  let mkFresh magic args = 
    let interned = Interned.intern magic in
      memoize (fun () -> Fresh (interned, List.map (fun f -> f ()) args))
  let mkTuple tuple =
    memoize (fun () -> Tuple (List.map (fun f -> f ()) tuple))
  let mkPolyv fields extends =
    memoize (fun () -> 
               let fields = List.map (fun (f,t) -> (f,t())) fields in
               let efields = List.map (fun f -> match f () with Polyv f -> f
                                         | _ -> assert false) extends in
                 Polyv (fields @ List.concat efields))
end

(* Dynamic types *)
type dynamic = Obj.t * TypeRep.t
let tagOf (_, tag) = tag
let untag (obj, tag) target = 
  if tag = target 
  then Some obj
  else None

(* Signature for type representations *)
module type Typeable =
sig
  type a
  val typeRep : unit -> TypeRep.t
  val hasType : dynamic -> bool
  val cast : dynamic -> a option
  val makeDynamic : a -> dynamic
end

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

