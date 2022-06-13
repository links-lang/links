open Utility
open Sugartypes

module Env = Utility.StringMap

let visitor =
    object (self)
    inherit SugarTraversals.map as super

    val mutable label_env : (Label.t list) Env.t = Env.empty

    method bind_labels = List.iter (fun l ->
        let old_ls = match Env.find_opt (Label.name l) label_env with
        | None    -> []
        | Some ls -> ls in
        label_env <- Env.add (Label.name l) (l::old_ls) label_env
    )

    method unbind_labels = List.iter (fun l ->
        match Env.find_opt (Label.name l) label_env with
        | None               -> ()
        | Some [] | Some [_] -> label_env <- Env.remove (Label.name l) label_env
        | Some (_::ls)        -> label_env <- Env.add (Label.name l) ls label_env
    )

    method! label lbl =
        if Label.is_global lbl || not (Label.is_free lbl) then
            lbl
        else
            let bind_with = match Env.find_opt (Label.name lbl) label_env with
            | Some (bw :: _) -> bw
            | _             -> failwith ("The local label " ^ Label.show lbl ^ " is not bound")
            in
            Label.bind_local ~bind_with lbl

    method! bindingnode = function
        | FreshLabel (labels, decls) ->
            let labels = List.map Label.bind_local labels in
            self#bind_labels labels ;
            let decls = List.map self#binding decls in
            self#unbind_labels labels ;
            FreshLabel(labels, decls)
        | b -> super#bindingnode b

    end

let program p = visitor#program p

let sentence = function
  | Definitions bs ->
      let bs' = visitor#list (fun o b -> o#binding b) bs in
        Definitions bs'
  | Expression  p  -> Expression p
  | Directive   d  -> Directive d

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
