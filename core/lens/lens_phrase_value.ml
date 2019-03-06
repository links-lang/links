open Lens_utility

type t =
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Tuple of t list
  | Record of (string * t) list
[@@deriving show]

type values = t list [@@deriving show]

let equal v1 v2 = v1 = v2

let unbox_error typ = Format.asprintf "Type error unboxing %s." typ |> failwith

let box_bool b = Bool b

let unbox_bool v = match v with Bool b -> b | _ -> unbox_error "Bool"

let box_int i = Int i

let unbox_int v = match v with Int b -> b | _ -> unbox_error "Int"

let box_float f = Float f

let unbox_float v = match v with Float f -> f | _ -> unbox_error "Float"

let box_string s = String s

let unbox_string v = match v with String s -> s | _ -> unbox_error "String"

let box_tuple t = Tuple t

let unbox_tuple v = match v with Tuple t -> t | _ -> unbox_error "Tuple"

let box_record t = Record t

let unbox_record v = match v with Record v -> v | _ -> unbox_error "Record"

module Record = struct
  let get t ~key =
    unbox_record t
    |> List.find ~f:(fun (k, _) -> k = key)
    |> Option.map ~f:(fun (_, v) -> v)

  let get_exn t ~key = get t ~key |> fun v -> Option.value_exn v

  let set t ~key ~value =
    unbox_record t
    |> List.map ~f:(fun (k, v) -> if k = key then (k, value) else (k, v))
    |> box_record

  let match_on t1 t2 ~on =
    List.for_all ~f:(fun key -> equal (get t1 ~key) (get t2 ~key)) on
end
