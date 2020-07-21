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

  let add_tname ~tname st =
    let used_tnames = tname :: st.used_tnames in
    { st with used_tnames }

  let add_name ~name st =
    let used_names = name :: st.used_names in
    { st with used_names }

  let add_tid ~tid st =
    let reserved_tids = IntSet.add tid st.reserved_tids in
    { st with reserved_tids }

  let add_id ~id st =
    let reserved_ids = IntSet.add id st.reserved_ids in
    { st with reserved_ids }
end

module Stage2 = struct
  type t = { tname_to_id : int StringMap.t; varname_to_id : int StringMap.t }

  let initial =
    { tname_to_id = StringMap.empty; varname_to_id = StringMap.empty }

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

module E = struct
  type t = Unexpected_error of string
end

type 'a lookup = ('a, Stage2.t) State.t

type 'a stage1 = ('a, ReprState.t) State.t

type 'a maker = 'a lookup stage1

type 'a t = ('a, E.t) Result.t maker

let make ?(state = ReprState.initial) mk =
  let lkp, state = State.run_state ~init:state mk in
  let state = Stage2.of_stage_1 state in
  let res = State.run_state ~init:state lkp |> fst in
  Result.Ok res

let lookup_tname ~tname =
  let+ st = State.get in
  Stage2.lookup_tname ~tname st

let lookup_name ~name =
  let+ st = State.get in
  Stage2.lookup_name ~name st

let add_tname ~tname = State.map_state (ReprState.add_tname ~tname)

let add_name ~name = State.map_state (ReprState.add_name ~name)

let add_tid ~tid = State.map_state (ReprState.add_tid ~tid)

let add_id ~id = State.map_state (ReprState.add_id ~id)
