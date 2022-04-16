open Links_core
open Utility
open State.Syntax

module ReprState = struct
  type t = {
    reserved_tids : IntSet.t;
    reserved_ids : IntSet.t;
    used_names : string list;
    used_tnames : string list;
  }

  let initial =
    {
      reserved_tids = IntSet.empty;
      reserved_ids = IntSet.empty;
      used_names = [];
      used_tnames = [];
    }

  let add_tnames ~tnames st =
    let used_tnames = tnames @ st.used_tnames in
    { st with used_tnames }

  let add_names ~names st =
    let used_names = names @ st.used_names in
    { st with used_names }

  let add_tids ~tids st =
    let reserved_tids = IntSet.union (IntSet.from_list tids) st.reserved_tids in
    { st with reserved_tids }

  let add_ids ~ids st =
    let reserved_ids = IntSet.union (IntSet.from_list ids) st.reserved_ids in
    { st with reserved_ids }

  let fresh_id =
    let* st = State.get in
    let id =
      match IntSet.max_elt_opt st.reserved_ids with
      | None -> 0
      | Some i -> i + 1
    in
    let reserved_ids = IntSet.add id st.reserved_ids in
    let+ () = State.put { st with reserved_ids } in
    id
end

module Stage2 = struct
  type t = { tname_to_id : int StringMap.t; varname_to_id : int StringMap.t }

  let of_stage_1 vs =
    let rec add_binding reserved next_id used map =
      match (used, IntSet.mem next_id reserved) with
      | [], _ -> map
      | _, true -> add_binding reserved (next_id + 1) used map
      | name :: rem_names, false ->
          add_binding reserved (next_id + 1) rem_names
            (StringMap.add name next_id map)
    in
    let open ReprState in
    let varname_to_id =
      add_binding vs.reserved_ids 0 vs.used_names StringMap.empty
    in
    let tname_to_id =
      add_binding vs.reserved_tids 0 vs.used_tnames StringMap.empty
    in
    { tname_to_id; varname_to_id }

  let lookup_tname ~tname st = StringMap.find tname st.tname_to_id

  let lookup_name ~name st = StringMap.find name st.varname_to_id
end

exception SchinksError of string

type 'a lookup = ('a, Stage2.t) State.t

type 'a stage1 = ('a, ReprState.t) State.t

type 'a t = 'a lookup stage1

let make ?(state = ReprState.initial) mk =
  let lkp, state = State.run_state ~init:state mk in
  let state = Stage2.of_stage_1 state in
  State.run_state ~init:state lkp |> fst

let lookup_tname ~tname =
  let+ st = State.get in
  Stage2.lookup_tname ~tname st

let lookup_name ~name =
  let+ st = State.get in
  Stage2.lookup_name ~name st

let add_tnames ~tnames = State.map_state (ReprState.add_tnames ~tnames)

let add_names ~names = State.map_state (ReprState.add_names ~names)

let add_tids ~tids = State.map_state (ReprState.add_tids ~tids)

let add_ids ~ids = State.map_state (ReprState.add_ids ~ids)

let add_tname ~tname = add_tnames ~tnames:[ tname ]

let add_name ~name = add_names ~names:[ name ]

let add_tid ~tid = add_tids ~tids:[ tid ]

let add_id ~id = add_ids ~ids:[ id ]

let fresh_id = ReprState.fresh_id
