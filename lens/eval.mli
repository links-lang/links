type behaviour = Classic | Incremental

module Error : sig
  type t =
    | InvalidDataType
    | InvalidData
    | ViolatesFunDepConstraint of Fun_dep.t
end

val put :
  ?behaviour:behaviour ->
  db:Database.t ->
  Value.t ->
  Phrase_value.t list ->
  (unit, Error.t) result
