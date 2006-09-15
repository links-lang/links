exception Bug of string

exception Not_implemented of string

let bug s = raise (Bug s)

let not_implemented s = raise (Not_implemented s)

module Definition = struct
  include Xml_lib.Definition

  module Show = Show.ShowDefaults (struct
    type a = t

    let format ppf t = not_implemented "Xml.Type.Show.format"
  end)

  module Pickle = Pickle.Pickle_defaults (struct
    type a = t

    let pickle buffer t = not_implemented "Xml.Type.Pickle.pickle"

    let unpickle stream = not_implemented "Xml.Type.Pickle.unpickle"
  end)
end

module Type = struct
  include Xml_lib.Type

  module Show = Show.ShowDefaults (struct
    type a = t

    let format ppf t = not_implemented "Xml.Type.Show.format"
  end)

  module Pickle = Pickle.Pickle_defaults (struct
    type a = t

    let pickle buffer t = not_implemented "Xml.Type.Pickle.pickle"

    let unpickle stream = not_implemented "Xml.Type.Pickle.unpickle"
  end)
end

module Inference = struct
  type type_constraint =
    | Id of t
    | Concat of t * t
    | Element of string * (string * bool) list * bool * t
  and inferred_type =
    | Known of Type.t
    | Unknown
  and type_info =
      { mutable visited : bool;
        mutable inferred : inferred_type;
        mutable constraints : type_constraint list; }
  and descr =
    | Type of type_info
    | Link of t
  and t = descr ref

  let disabled = ref false

  let is_activated () = not (!disabled)

  let new_type inferred constraints =
    let type_info =
      { visited = false;
        inferred = inferred;
        constraints = constraints; } in
    ref (Type type_info)

  let of_type ty = new_type (Known ty) []

  let new_unknown constraints = new_type Unknown constraints

  let unknown () = new_unknown []

  let any = of_type Type.any

  let string = of_type Type.string

  let epsilon = of_type Type.epsilon

  let of_string s = of_type (Type.of_string s)

  let equal _ _ = false

  let rec root t =
    match !t with
      | Link t' ->
          let root, ty = root t' in
          t := Link root;
          root, ty
      | Type ty -> t, ty

  let rec unify t1 t2 =
    if is_activated () then
      let t1, t1_info = root t1 and t2, t2_info = root t2 in
      if t1 != t2 then
        let constraints = t1_info.constraints @ t2_info.constraints in
        let visited = t1_info.visited || t2_info.visited in
        match t1_info.inferred, t2_info.inferred with
          | Unknown, _ ->
              t2_info.constraints <- constraints;
              t2_info.visited <- visited;
              t1 := Link t2;
              delete_id_cycles t2
          | _, Unknown ->
              t1_info.constraints <- constraints;
              t1_info.visited <- visited;
              t2 := Link t1;
              delete_id_cycles t1
          | Known t1, Known t2 ->
              if not (Type.subtype t1 t2 && Type.subtype t2 t1) then
                failwith "Unification"
  and delete_id_cycle parents t =
    let t, t_info = root t in
    if List.memq t parents then
      begin
        List.iter (unify t) parents;
        true
      end
    else
      let parents = t :: parents in
      let sub_delete constr =
        match constr with
          | Id t' -> delete_id_cycle parents t'
          | _ -> false in
      List.exists sub_delete t_info.constraints
  and delete_id_cycles t =
    if delete_id_cycle [] t then delete_id_cycles t

  let identity source =
    let result = new_unknown [Id source] in
    result

  let concat t1 t2 =
    let t1, t1_info = root t1 and t2, t2_info = root t2 in
    match t1_info.inferred, t2_info.inferred with
      | Known t1_inferred, Known t2_inferred ->
          of_type (Type.concat t1_inferred t2_inferred)
      | _ -> new_unknown [Concat (t1, t2)]

  let element name attr attr_open content_type =
    let content_type, content_type_info = root content_type in
    match content_type_info.inferred with
      | Known content_type_inferred ->
          of_type (Type.element name attr attr_open content_type_inferred)
      | _ -> new_unknown [Element (name, attr, attr_open, content_type)]

  exception Cyclic of type_constraint list

  let rec solve to_check t =
    if is_activated () then
      let t, t_info = root t in
      match t_info.inferred with
        | Known inferred ->
            if t_info.visited then
              to_check, inferred
            else
              begin
                t_info.visited <- true;
                (t, inferred) :: to_check, inferred
              end
        | Unknown ->
            if t_info.visited then raise (Cyclic [])
            else
              begin
                t_info.visited <- true;
                let to_check, result = get_smallest_type to_check t in
                t_info.inferred <- Known result;
                to_check, result
              end
    else to_check, Type.any
  and get_smallest_type to_check t =
    let t, t_info = root t in
    let constraint_id to_check t' =
      delete_id_cycles t';
      let t, _ = root t and t', _ = root t' in
      if t' == t then to_check, Type.empty
      else
        let to_check, t_inferred = solve to_check t' in
        to_check, t_inferred
    and constraint_concat to_check t1 t2 =
      let t1, _ = root t1 and t2, _ = root t2 in
      let to_check, t1_inferred = solve to_check t1 in
      let to_check, t2_inferred = solve to_check t2 in
      let t_inferred = Type.concat t1_inferred t2_inferred in
      to_check, t_inferred
    and constraint_element to_check name attr attr_open content_type =
      let content_type, _ = root content_type in
      let to_check, content_type_inferred = solve to_check content_type in
      let t_inferred =
        Type.element name attr attr_open content_type_inferred in
      to_check, t_inferred in
    let add_constraint (to_check, smallest_type) type_constraint =
      let to_check, constraint_type =
        try
          match type_constraint with
            | Id t -> constraint_id to_check t
            | Concat (t1, t2) -> constraint_concat to_check t1 t2
            | Element (name, attr, attr_open, content_type) ->
                constraint_element to_check name attr attr_open content_type
        with Cyclic list ->
          raise (Cyclic (type_constraint :: list)) in
      to_check, Type.union constraint_type smallest_type in
    List.fold_left add_constraint (to_check, Type.empty) t_info.constraints

  let to_type t =
    try
      let to_check, result = solve [] t in
      let check (t, inferred) =
        let _, smallest = get_smallest_type [] t in
        if not (Type.subtype smallest inferred) then
          failwith "Constraint" in
      List.iter check to_check;
      result
    with Cyclic list ->
      let buf = Buffer.create 64 in
      Buffer.add_string buf "Cycle between";
      let add_constraint constr =
        match constr with
          | Id _ -> ()
          | Concat _ -> Buffer.add_string buf " concatenation"
          | Element _ -> Buffer.add_string buf " element" in
      List.iter add_constraint list;
      failwith (Buffer.contents buf)

  module Show = Show.ShowDefaults (struct
      type a = t

      let format ppf t = not_implemented "Xml.Inference.Show.format"
    end)

  module Pickle = Pickle.Pickle_defaults (struct
      type a = t

      let pickle buffer t = not_implemented "Xml.Inference.Pickle.pickle"

      let unpickle stream = not_implemented "Xml.Inference.Pickle.unpickle"
    end)
end
