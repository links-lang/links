open CommonTypes

val generalise : ?unwrap:bool -> Types.environment -> Types.datatype
              -> ((Quantifier.t list * Types.type_arg list) * Types.datatype)
val generalise_rigid : ?unwrap:bool -> Types.environment -> Types.datatype
                    -> ((Quantifier.t list * Types.type_arg list) * Types.datatype)

(* Like generalise, but does not mutate the generalised type variables by making them rigid.
  This means that the generalised type variables must be post-processed elsewhere! *)
val generalise_without_mutation : ?unwrap:bool -> Types.environment -> Types.datatype
              -> ((Quantifier.t list * Types.type_arg list) * Types.datatype)

val get_quantifiers_rigid : Types.environment -> Types.datatype -> Quantifier.t list
val rigidify_type_arg : Types.type_arg -> unit
