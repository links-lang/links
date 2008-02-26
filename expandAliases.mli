class type alias_expander =
object
  inherit SugarTraversals.fold_map
  method aliases : Types.alias_environment
end

val expand_aliases : Types.alias_environment -> alias_expander
val instantiate : string -> Types.datatype list -> Types.alias_environment -> Types.datatype
val expand : Types.alias_environment -> Types.datatype -> Types.datatype

(*val alias_free : Types.alias_environment -> SugarTraversals.predicate*)
