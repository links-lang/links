open Sugartypes

let refine_bindings =
object (self)
  inherit SugarTraversals.map as super
  method phrasenode : phrasenode -> phrasenode = function
    |`Block (bindings, body) -> 
       let bindings = self#list (fun o -> o#binding) bindings in
       let body = self#phrase body in
         `Block (Sugartypes.refine_bindings bindings, body)
    | p -> super#phrasenode p

  method program : program -> program =
    fun (bindings, body) ->
      let bindings = self#list (fun o -> o#binding) bindings in
      let body = self#option (fun o -> o#phrase) body in
        Sugartypes.refine_bindings bindings, body

  method sentence : sentence -> sentence = function
    |`Definitions defs -> 
       let defs = self#list (fun o -> o#binding) defs in
         `Definitions (Sugartypes.refine_bindings defs)
    | d -> super#sentence d
end
