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
    let relational_fail () =
      failwith "Code uses relational lenses, but relational lenses are not enabled. Please set the relational lenses flag."
    in
    function
    | Handle _     when not (get BS.Handlers.enabled) ->
       failwith "Handlers are only allowed with setting enable_handlers set to true."
    | HandlerLit _ when not (get BS.Handlers.enabled) ->
       failwith "Handlers are only allowed with setting enable_handlers set to true."
    | LensLit _        when relational_lenses_disabled -> relational_fail ()
    | LensKeysLit _    when relational_lenses_disabled -> relational_fail ()
    | LensFunDepsLit _ when relational_lenses_disabled -> relational_fail ()
    | LensDropLit _    when relational_lenses_disabled -> relational_fail ()
    | LensSelectLit _  when relational_lenses_disabled -> relational_fail ()
    | LensJoinLit _    when relational_lenses_disabled -> relational_fail ()
    | LensGetLit _     when relational_lenses_disabled -> relational_fail ()
    | LensPutLit _     when relational_lenses_disabled -> relational_fail ()
    | e -> super#phrasenode e

  method! bindingnode = function
    | Handler _ when not (get BS.Handlers.enabled) ->
       failwith "Handlers are only allowed with setting enable_handlers set to true."
    | b -> super#bindingnode b
end
