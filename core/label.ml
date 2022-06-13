open Utility
open CommonTypes

let internal_error message =
  Errors.internal_error ~filename:"label.ml" ~message
let local_error show lbl =
  internal_error ("Label " ^ show lbl ^ " is local but a global label was expected")
let not_local_error show lbl =
  internal_error ("Label " ^ show lbl ^ " is global but a local label was expected")
let not_free_error show lbl =
  internal_error ("Label " ^ show lbl ^ " is not free")

module Uid = struct
    type t = Id of Int.t | Free

    let show = function
        | Free -> "free"
        | Id id -> string_of_int id

    let compare uid uid' = match uid, uid' with
        | Id id, Id id' -> Int.compare id id'
        | Free, Free -> 0
        | Id _, _ -> -1
        | _, Id _ -> 1

    let counter = ref 0

    let new_uid () = counter:=!counter+1 ; Id !counter

    let is_free = function
        | Free -> true
        | _ -> false
end

module Label = struct
    type local = Name.t * Uid.t
    type global = Name.t
    (* type unresolved = Name.t *)

    type t =
        | Lcl of local
        | Gbl of global
        (* | Unr of unresolved *)

    let show = function
        | Lcl (name, id) -> "`" ^ name ^ "(" ^ Uid.show id ^ ")"
        | Gbl name -> name
        (* | Unr name -> "`?" ^ name *)

    let pp f l =
      Format.pp_print_string f begin match l with
        | Lcl (name, _) -> "`"^name
        | Gbl name -> name
        (* | Unr name -> "`?" ^ name *)
      end

    let mk_local name = Lcl (name, Uid.Free)

    let mk_global name = Gbl name

    (* let mk_unresolved name = Unr name *)

    let make ?(local=false) name =
        if local then
            mk_local name
        else
            mk_global name

    let mk_int i = mk_global (string_of_int i)

    let to_int = function
        | Gbl g -> int_of_string g
        | l -> raise (local_error show l)

    let name = function
        (* | Gbl name | Lcl (name,_) | Unr name -> name *)
        | Lcl (name,_) ->  "`"^name
        | Gbl name -> name
        (* | Unr name -> name *)


    let compare lbl lbl' = match lbl,lbl' with
        | Lcl(name, Uid.Free), Lcl(name', Uid.Free) -> String.compare name name'
        | Lcl(_, uid), Lcl(_, uid') -> Uid.compare uid uid'
        | Gbl g, Gbl g' -> String.compare g g'
        (* | Unr u, Unr u' -> String.compare u u' *)
        (* | Unr _, _ *)
        | Lcl _, Gbl _ -> 1
        | _, _ -> -1

    let eq lbl lbl' = compare lbl lbl' = 0

    let eq_name lbl name = eq lbl (mk_global name)

    let is_local = function
        | Lcl _ -> true
        | _ -> false

    let is_global = function
        | Gbl _ -> true
        | _ -> false

    let is_free = function
        | Lcl (_, uid) -> Uid.is_free uid
        | l -> raise (not_local_error show l)

    let uid = function
        | Lcl (_, uid) -> uid
        | l -> raise (not_local_error show l)

    let bind_local ?bind_with lbl = match bind_with, lbl with
        | Some bind_lbl, Lcl (name, Uid.Free) -> Lcl (name, uid bind_lbl)
        | Some _       , Lcl _                -> raise (not_free_error show lbl)
        | None         , Lcl (name, _)        -> Lcl (name, Uid.new_uid ())
        | _ -> raise (not_local_error show lbl)


    (* let is_resolved = function *)
    (*     | Unr _ -> false *)
    (*     | _ -> true *)


    let one = mk_global "1"
    let two = mk_global "2"
    let return = mk_global "Return"


    (* let resolve_global = function *)
    (*     | Unr name -> Gbl name *)
    (*     | _ -> failwith "already resolved" *)

    (* let resolve_local = function *)
    (*     | Unr name -> mk_local name *)
    (*     | _ -> failwith "already resolved" *)
end

include Label

module type LABELMAP = Utility.Map with type key = t
module Map : LABELMAP = Utility.Map.Make(Label)

module type LABELSET = Utility.Set with type elt = t
module Set : LABELSET = Utility.Set.Make(Label)

let string_to_label_map m =
  Utility.StringMap.fold
    (fun k v m -> Map.add (Label.make k) v m)
    m Map.empty

let label_to_string_map m =
  Map.fold
    (fun k v m -> Utility.StringMap.add (Label.name k) v m)
    m Utility.StringMap.empty

let string_to_label_set m =
  Utility.StringSet.fold
    (fun k m -> Set.add (Label.make k) m)
    m Set.empty

let label_to_string_set m =
  Set.fold
    (fun k m -> Utility.StringSet.add (Label.name k) m)
    m Utility.StringSet.empty
