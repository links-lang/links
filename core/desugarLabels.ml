open Utility
open Sugartypes

module Env = Label.Env

let visitor =
    object (self)
    inherit SugarTraversals.map as super

    val mutable label_env : Env.t = Env.empty

    method! label lbl =
        if Label.is_global lbl || not (Label.is_free lbl) then
            lbl
        else
          match Env.find_homonyms lbl label_env with
            | bind_with :: _ -> Label.bind_local ~bind_with lbl
            | _             -> failwith ("The local label " ^ Label.show lbl ^ " is not bound")

    method! bindingnode = function
        | FreshLabel labels ->
          let labels = List.map Label.bind_local labels in
          label_env <- Env.bind_labels labels label_env ;
          FreshLabel labels
        | Module { binder; members } ->
          let env = label_env in
          let members = self#list (fun o -> o#binding) members in
          label_env <- env;
          Module { binder; members }
        | b -> super#bindingnode b

    method! phrasenode = function
       | Block (bs, e) ->
         let env = label_env in
         let bs = self#list (fun o -> o#binding) bs in
         let e = self#phrase e in
         label_env <- env;
         Block (bs, e)
       | e -> super#phrasenode e
  end

let program p = visitor#program p

let sentence = function
  | Definitions bs ->
      let bs' = visitor#list (fun o b -> o#binding b) bs in
      Definitions bs'
  | Expression p ->
      let p' = visitor#phrase p in
      Expression p'
  | Directive d -> Directive d

module Untyped = struct
    open Transform.Untyped

    let name = "labels"

    let program state program' =
        let program' = program program' in
        return state program'

    let sentence state sentence' =
        let sentence' = sentence sentence' in
        return state sentence'
end
