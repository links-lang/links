open Utility
open CommonTypes

let show_unique_labels_idents =
  Settings.(flag ~default:false "show_unique_labels_idents"
            |> privilege `User
            |> synopsis "Show the internal numeric identifier for each unique label"
            |> convert parse_bool
            |> sync)

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

    type t =
        | Local of local
        | Global of global
        | Number of int
    type label = t

    let show = function
        | Local (name, id) ->
          if Settings.get show_unique_labels_idents
          then Printf.sprintf "`%s<%s>" name (Uid.show id)
          else Printf.sprintf "`%s" name
        | Global name -> name
        | Number n -> string_of_int n

    let pp f l =
      Format.pp_print_string f begin match l with
        | Local (name, id) -> "`"^name ^ "<" ^ Uid.show id ^ ">"
        | Global name -> name
        | Number n -> string_of_int n
      end

    let of_int i = Number i

    let to_int = function
        | Number n -> n
        | Global i -> int_of_string i
        | l -> raise (local_error show l)

    let make_local ?(uid=Uid.Free) name = Local (name, uid)

    let make_global name = Global name

    let make textual_name =
      let is_digit ch =
        Char.code ch >= 48 && Char.code ch <= 57
      in
      if String.for_all is_digit textual_name
      then of_int (int_of_string textual_name)
      else make_global textual_name

    let name = function
        | Local (name,_) ->  "`"^name
        | Global name -> name
        | Number n -> string_of_int n

    let compare lbl lbl' = match lbl,lbl' with
        | Local (name, Uid.Free), Local (name', Uid.Free) -> String.compare name name'
        | Local (_, uid), Local (_, uid') -> Uid.compare uid uid'
        | Global g, Global g' -> String.compare g g'
        | Local _, Global _ -> 1
        | Local _, Number _ -> 1
        | Global _, Number _ -> 1
        | Number _, Global _ -> -1
        | Number _, Local _ -> -1
        | Global _, Local _ -> -1
        | Number n, Number m -> Int.compare n m

    let equal lbl lbl' = compare lbl lbl' = 0

    let textual_equal lbl lbl' = String.compare (name lbl) (name lbl') = 0

    let name_is lbl name' = String.compare (name lbl) name' = 0

    let is_local = function
        | Local _ -> true
        | _ -> false

    let is_global l = not (is_local l)

    let is_free = function
        | Local (_, uid) -> Uid.is_free uid
        | l -> raise (not_local_error show l)

    let uid = function
        | Local (_, uid) -> uid
        | l -> raise (not_local_error show l)

    let bind_local ?bind_with lbl = match bind_with, lbl with
        | Some bind_lbl, Local (name, Uid.Free) -> Local (name, uid bind_lbl)
        | Some _       , Local _                -> raise (not_free_error show lbl)
        | None         , Local (name, _)        -> Local (name, Uid.new_uid ())
        | _ -> raise (not_local_error show lbl)

    let one = of_int 1
    let two = of_int 2
    let return = make_global "return"
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

module Env = struct
    module M = Utility.StringMap

    type t = (label list) M.t

    let pp = M.pp (Format.pp_print_list Label.pp)

    let empty = M.empty

    let extend = M.superimpose

    let bind env label =
        let old_ls = match M.find_opt (name label) env with
        | None    -> []
        | Some ls -> ls in
        M.add (name label) (label::old_ls) env

    let unbind env label =
        match M.find_opt (name label) env with
        | None               -> env
        | Some [] | Some [_] -> M.remove (name label) env
        | Some (_::ls)        -> M.add (name label) ls env

    let bind_labels labels env = List.fold_left bind env labels
    let unbind_labels labels env  = List.fold_left unbind env labels

    let find_homonyms l env = match M.find_opt (name l) env with
        | Some ls -> ls
        | None -> []
end
