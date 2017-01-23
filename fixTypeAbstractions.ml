open Utility

(*
  - Extract quantifiers (e.g. [(a) -> b] |-> [a, b]).
  - Delete quantifiers that have been unified with quantifiers bound
  in an outer scope.

  Perhaps we should stick with one of 'tyvars' or 'quantifiers'.
  Using both can be confusing.

  Should we remove non-rigid quantifiers?
*)

class fix_type_abstractions env =
object (o : 'self_type)
  inherit (TransformSugar.transform env)

  val active_tyvars = IntSet.empty

  method lookup_tyvars : IntSet.t = active_tyvars
  method with_tyvars : IntSet.t -> 'self_type = fun tyvars ->
    {< active_tyvars = tyvars >}

  method! datatype : Types.datatype -> ('self_type * Types.datatype) =
    fun t ->
      (o, Types.normalise_datatype t)

  method! row : Types.row -> ('self_type * Types.row) =
    fun row ->
      (o, Types.normalise_row row)

  method fix_quantifiers : Types.quantifier list -> Types.quantifier list = fun qs ->
    (* extract *)
    let qs = Generalise.extract_quantifiers qs in

    (* filter out quantifiers bound at an outer scope *)
    let qs =
      List.filter
        (fun q ->
           not (IntSet.mem (Types.var_of_quantifier q) active_tyvars))
        qs
    in
      qs

  method extend_tyvars : Types.quantifier list -> IntSet.t = fun qs ->
    List.fold_right
      (fun x qs -> IntSet.add x qs)
      (List.map Types.var_of_quantifier qs)
      active_tyvars

  method! quantifiers : Types.quantifier list -> ('self_type * Types.quantifier list) =
    fun qs ->
      let qs = o#fix_quantifiers qs in
      let inner_tyvars = o#extend_tyvars qs in
      let o = o#with_tyvars inner_tyvars in
        (o, qs)
  method! backup_quantifiers : IntSet.t = o#lookup_tyvars
  method! restore_quantifiers : IntSet.t -> 'self_type = fun tyvars -> o#with_tyvars tyvars
end

let fix_type_abstractions env = ((new fix_type_abstractions env) : fix_type_abstractions :> TransformSugar.transform)


let rigid_quantifiers =
object
  inherit SugarTraversals.predicate

  val all_rigid = true
  method satisfied = all_rigid

  method! tyvar = fun q -> {< all_rigid = all_rigid && (Types.is_rigid_quantifier q) >}
end
