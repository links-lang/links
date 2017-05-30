(**
 Check whether an experimental feature is enabled before use.
 **)

module BS = Basicsettings

let get setting = Settings.get_value setting

let check =
object
  inherit SugarTraversals.map as super
  method! phrasenode = function
    | `Handle _     when not (get BS.Handlers.enabled) ->
       failwith "Handlers are only allowed with setting enable_handlers set to true."
    | `HandlerLit _ when not (get BS.Handlers.enabled) ->
       failwith "Handlers are only allowed with setting enable_handlers set to true."
    | e -> super#phrasenode e

  method! bindingnode = function
    | `Handler _ when not (get BS.Handlers.enabled) ->
       failwith "Handlers are only allowed with setting enable_handlers set to true."
    | b -> super#bindingnode b
end
