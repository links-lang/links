let relational_lenses =
  Settings.(
    flag ~default:true "relational_lenses"
    |> synopsis
         "Toggles whether to enable the incremental relational lenses extension"
    |> convert parse_bool
    |> sync)

let classic_lenses =
  (* Use naive/non-incremental relational lenses instead of incremental ones *)
  Settings.(
    flag "relational_lenses_classic"
    |> synopsis "Selects whether to use non-incremental relational lenses"
    |> depends relational_lenses
    |> convert parse_bool)

include Links_lens
