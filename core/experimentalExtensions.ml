(**
 Check whether an experimental feature is enabled before use.
 **)

open Sugartypes

module BS = Basicsettings

let get setting = Settings.get_value setting

let check =
object
  inherit SugarTraversals.map as super
  method! phrasenode =
    let relational_lenses_disabled =
      not (get Basicsettings.RelationalLenses.relational_lenses)
    in
    let relational_fail =
      Errors.settings_error "Code uses relational lenses, but relational lenses are not enabled. Please set the relational lenses flag."
    in
    function
    | LensLit _        when relational_lenses_disabled -> raise relational_fail
    | LensKeysLit _    when relational_lenses_disabled -> raise relational_fail
    | LensFunDepsLit _ when relational_lenses_disabled -> raise relational_fail
    | LensDropLit _    when relational_lenses_disabled -> raise relational_fail
    | LensSelectLit _  when relational_lenses_disabled -> raise relational_fail
    | LensJoinLit _    when relational_lenses_disabled -> raise relational_fail
    | LensGetLit _     when relational_lenses_disabled -> raise relational_fail
    | LensPutLit _     when relational_lenses_disabled -> raise relational_fail
    | e -> super#phrasenode e
end
