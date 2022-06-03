open Utility
open CommonTypes

module Label = struct
    type local = Int.t
      [@@deriving show]
    type global = Name.t
      [@@deriving show]

    type t =
        | Lcl of local
        | Gbl of global
      [@@deriving show]

    let counter = ref 0

    let mk_fresh () = Lcl (counter:=!counter+1 ; !counter)

    let mk_global name = Gbl name

    let make = mk_global

    let mk_int i = Gbl (string_of_int i)

    let get_int = function
        | Gbl g -> int_of_string g
        | _ -> failwith "local label"

    let compare lbl lbl' = match lbl,lbl' with
        | Lcl l, Lcl l' -> Int.compare l l'
        | Gbl g, Gbl g' -> String.compare g g'
        | Lcl _, _ -> 1
        | Gbl _, _ -> -1

    let is_local = function
        | Lcl _ -> true
        | _ -> false

    let is_global = function
        | Gbl _ -> true
        | _ -> false

    let one = Gbl "1"
end
include Label
