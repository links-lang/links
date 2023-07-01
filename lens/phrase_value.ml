open Lens_utility

type t =
  | Bool of bool
  | Int of int
  | Serial of [ `Key of int | `NewKey | `NewKeyMapped of int ]
  | Float of float
  | String of string
  | Char of char
  | Tuple of t list
  | Record of (string * t) list
[@@deriving show, sexp]

type values = t list [@@deriving show]

(** Equality considers two `NewKey values as different. *)
let equal v1 v2 =
  match (v1, v2) with
  | Serial `NewKey, _ -> false
  | _, Serial `NewKey -> false
  | _ -> v1 = v2

let is_new_key v =
  match v with
  | Serial `NewKey
   |Serial (`NewKeyMapped _) ->
      true
  | _ -> false

module Unbox_error = struct
  exception E of { value : t; expected : string }

  let pp f e =
    match e with
    | E { value; expected } ->
        Format.fprintf f "Type error unboxing %a as %s." pp value expected
    | _ -> ()

  let () =
    Printexc.register_printer (function
      | E _ as e -> Some (Format.asprintf "%a" pp e)
      | _ -> None)
end

let unbox_error value expected = raise (Unbox_error.E { value; expected })

let box_bool b = Bool b

let unbox_bool v =
  match v with
  | Bool b -> b
  | _ -> unbox_error v "Bool"

let box_int i = Int i

let unbox_int v =
  match v with
  | Int b -> b
  | _ -> unbox_error v "Int"

let unbox_serial_newkeymapped v =
  match v with
  | Serial (`NewKeyMapped v) -> v
  | _ -> unbox_error v "Serial `NewKeyMapped"

let unbox_serial_key v =
  match v with
  | Serial (`Key v) -> v
  | _ -> unbox_error v "Serial `Key"

let box_float f = Float f

let unbox_float v =
  match v with
  | Float f -> f
  | _ -> unbox_error v "Float"

let box_string s = String s

let unbox_string v =
  match v with
  | String s -> s
  | _ -> unbox_error v "String"

let box_tuple t = Tuple t

let unbox_tuple v =
  match v with
  | Tuple t -> t
  | _ -> unbox_error v "Tuple"

let box_record t = Record t

let unbox_record v =
  match v with
  | Record v -> v
  | _ -> unbox_error v "Record"

let rec type_of v =
  match v with
  | Bool _ -> Phrase_type.Bool
  | Int _ -> Phrase_type.Int
  | Serial _ -> Phrase_type.Serial
  | Char _ -> Phrase_type.Char
  | Float _ -> Phrase_type.Float
  | String _ -> Phrase_type.String
  | Tuple t -> Phrase_type.Tuple (List.map ~f:type_of t)
  | Record r ->
      Phrase_type.Record
        (List.map ~f:(fun (k, v) -> (k, type_of v)) r |> String.Map.from_alist)

let rec default_value t =
  match t with
  | Phrase_type.Bool -> Bool false
  | Phrase_type.Int -> Int 0
  | Phrase_type.Serial -> Serial `NewKey
  | Phrase_type.Char -> Char 'a'
  | Phrase_type.Float -> Float 0.0
  | Phrase_type.String -> String ""
  | Phrase_type.Tuple t -> Tuple (List.map ~f:default_value t)
  | Phrase_type.Record r ->
      Record (String.Map.to_list (fun k t -> (k, default_value t)) r)

let rec pp_pretty f t =
  match t with
  | Bool b -> Format.pp_print_bool f b
  | Int i -> Format.pp_print_int f i
  | Serial `NewKey -> Format.pp_constant "$$" f ()
  | Serial (`NewKeyMapped v) -> Format.fprintf f "$%i" v
  | Serial (`Key v) -> Format.fprintf f "@%i" v
  | Char c -> Format.pp_print_char f c
  | Float v -> Format.pp_print_float f v
  | String v -> Format.pp_print_string f v
  | Tuple t -> Format.fprintf f "(%a)" (Format.pp_comma_list pp_pretty) t
  | Record t ->
      let pp_entry f (k, v) = Format.fprintf f "%s = %a" k pp_pretty v in
      Format.fprintf f "(%a)" (Format.pp_comma_list pp_entry) t

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
    List.for_all
      ~f:(fun key -> (Option.equal equal) (get t1 ~key) (get t2 ~key))
      on
end
