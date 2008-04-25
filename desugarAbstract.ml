open Utility
open Sugartypes
module Env = Env.String

let signature_names : sigitem list -> StringSet.t * StringSet.t =
  List.fold_left
    (fun (vals, types) -> function
       | `Sig ((name,_,_), _) -> (StringSet.add name vals, types)
       | `Type (name, _, _)   -> (vals, StringSet.add name types))
    (StringSet.empty, StringSet.empty)

let fresh = Utility.gensym ~prefix:"_abstract"

(* TODO: check for duplicate bindings.  Let's just disallow those for
   now.
*)
let expand_abstract 
    (pos : position)
    (signature : sigitem list)
    (bindings : binding list) : binding list
    = let sigvars, sigtypes = signature_names signature in
      List.rev
        (fst
           (List.fold_left
              (fun ((bindings : binding list), substmap) (_, pos as node : binding) ->
                 match fst node with
(*[
| `Val      of pattern * phrase * location * datatype' option
| `Fun      of binder * funlit * location * datatype' option
| `Funs     of (binder * funlit * location * datatype' option) list
| `Foreign  of name * name * datatype'
| `Abstract of sigitem list * binding list
| `Infix
| `Exp      of phrase
]*)

                   | `Val (pat, phrase, loc, dt) -> 
                       let _, phrase = (SugarSubst.subst substmap)#phrase phrase in
                       let to_rename = StringSet.diff (Freevars.pattern pat) sigvars in
                       let renamings = StringSet.fold (fun k rs -> (k, fresh ()) :: rs) to_rename [] in
                       let pat = 
                         (object
                            inherit SugarTraversals.map
                            method binder (name, dt, pos) = 
                              (from_option name (lookup name renamings), dt, pos)
                          end)#pattern pat in
                         ((`Val (pat, phrase, loc, dt), pos) :: bindings,
                          renamings @ substmap)

                   | `Fun ((name, dt, pos as binder), rhs, loc, dt') ->
                       let _, rhs = (SugarSubst.subst substmap)#funlit rhs in
                         if StringSet.mem name sigvars then
                           ((`Fun (binder, rhs, loc, dt'), pos) :: bindings,
                            substmap)
                         else let name' = fresh () in
                           ((`Fun ((name', dt, pos), rhs, loc, dt'), pos) :: bindings,
                            (name, name') :: substmap)
                   | `Funs binds ->
                       let bound_names = List.fold_right
                                           (fun ((name,_,_),_,_,_) -> StringSet.add name)
                                           binds
                                           StringSet.empty in
                       let to_rename = StringSet.diff bound_names sigvars in
                       let renamings = StringSet.fold (fun k rs -> (k, fresh ()) :: rs) to_rename [] in
                       let substmap = renamings @ substmap in
                       let binds =
                         List.map
                           (fun ((name, pos, dt), rhs, loc, dt') ->
                              let name = from_option name (lookup name renamings) in
                                ((name, pos, dt), 
                                 snd ((SugarSubst.subst substmap)#funlit rhs),
                                 loc,
                                 dt')) 
                           binds in
                         ((`Funs binds, pos) :: bindings, substmap)

                   | `Foreign (lang, name, dt) ->
                       if StringSet.mem name sigvars then 
                         ((`Foreign (lang, name, dt),pos)::bindings, substmap)
                       else failwith "You can't hide foreign bindings in abstract blocks"


                   | `Type (tname, args, dt) as t -> 
                       (* we don't need to do any substitution with
                          type names, since all type names have been
                          resolved to semantic objects by this time.
                          We do give type names that do not appear in
                          the signature private names to prevent them
                          being referenced in the interactive shell *)
                       if StringSet.mem tname sigtypes then
                         ((t,pos)::bindings, substmap)
                       else let tname' = fresh () in
                         ((`Type (tname', args, dt), pos) :: bindings, substmap)
                   | `Abstract _ -> assert false
                   | `Infix      -> (node :: bindings, substmap)
                   | `Exp      e -> ((`Exp (snd ((SugarSubst.subst substmap)#phrase e)), pos)::bindings, 
                                     substmap))
                       
              ([], [])
              bindings))

let expand_bindings : binding list -> binding list =
  concat_map (function
                | `Abstract (signature, bindings), pos -> expand_abstract pos signature bindings
                | b -> [b])

let desugar_abstract =
object (self)
  inherit SugarTraversals.map as super
  method phrasenode = function
    | `Block (bindings, e) -> 
        let bindings = self#list (fun o -> o#binding) bindings in
        let e = self#phrase e in
          `Block (expand_bindings bindings, e)
    | e -> super#phrasenode e

  method bindingnode = function
    | `Abstract (signature, bindings) -> 
        let bindings = self#list (fun o -> o#binding) bindings in
          `Abstract (signature, expand_bindings bindings)
    | e -> super#bindingnode e

  method sentence = function
    | `Definitions bindings -> 
        let bindings = self#list (fun o -> o#binding) bindings in
          `Definitions (expand_bindings bindings)
    | e -> super#sentence e

  method program (bindings, e) = (expand_bindings bindings, e)
end

let has_no_abstracts =
object (self)
  inherit SugarTraversals.predicate as super

  val no_abstracts = true
  method satisfied = no_abstracts

  method bindingnode = function
    | `Abstract _ -> {< no_abstracts = false >}
    | e -> super#bindingnode e
end
