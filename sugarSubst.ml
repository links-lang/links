open Utility

module Substitution : 
sig
  type t
  val empty       : t
  val in_domain   : t -> string -> bool
  val in_codomain : t -> string -> bool
  val add         : t -> string * string -> t
  val remove      : t -> string -> t
  val find        : t -> string -> string
  val lookup      : t -> string -> string option
end =
struct
  type t = (string * string) list
  let empty           = []
  let in_domain l x   = List.mem_assoc x l
  let in_codomain l y = mem_rassoc y l
  let add l (x,y)     = 
    assert (not (List.exists (fst ->- ((=)x)) l));
    (x,y) :: l
  let remove l x      = List.remove_assoc x l
  let find l x        = List.assoc x l
  let lookup l x      = lookup x l
end

let fresh_name () : string = 
  Utility.gensym ~prefix:"_sugarSubst" ()

let binders = 
object
  val binders = []
  method binders = binders
  inherit SugarTraversals.fold
  method binder b = {< binders = b::binders >}
end

let subst (map : (string * string) list) =
object (self : 'self)
  inherit SugarTraversals.fold_map as super
  val subst : Substitution.t = 
    List.fold_right (fun pair subst -> Substitution.add subst pair) map Substitution.empty

  (* The scope of formlet bindings is not lexical in the usual sense.
     We need to gather them up, latent, until we hit a `Formlet node,
     at which point we bring them into scope in the yields clause. *)
  val formlet_bindings : Sugartypes.binder list = []

  method private subst_var v = 
    match Substitution.lookup subst v with
      | Some v' -> v'
      | None    -> v

  method private delimit : 'a 'b . ('a * 'b -> 'self * 'b) = 
    fun e -> self, snd e

  method binder (name, dt, pos) =
      (*
        If `name' is in the domain of the substitution then 
        delete it from the substitution.
      *)
    if Substitution.in_domain subst name then
      ({< subst = Substitution.remove subst name >}, (name, dt, pos))
      (*
        If `name' is in the codomain of the substitution then
        rename this variable as well.
      *)
    else if Substitution.in_codomain subst name then
      let name' = fresh_name () in
        ({< subst = Substitution.add subst (name, name') >}, (name', dt, pos))
    else
      (self, (name, dt, pos))

  method phrasenode = function
      (* We need to treat specially:
           * variables
           * anything which binds variables
           * anything which delimits the scope of a binding
       * *)
    | `Var v -> self, `Var (self#subst_var v)
    (* In each of these cases the standard will do *)
    | `FunLit _
    | `Escape _
    | `Iteration _
    | `Block _ as p ->
        self#delimit (super#phrasenode p)
    | `DBDelete (pat, db, body) ->
        self#delimit
          ((* Don't bind `pat' within `db' *)
            let self, db = self#phrase db in
            let self, pat = self#pattern pat in
            let self, body = self#option (fun o -> o#phrase) body in
              self, `DBDelete (pat, db, body))
    | `DBUpdate (pat, db, body, rest) ->
        (* Don't bind `pat' within `db' *)
        self#delimit
          ((* Don't bind `pat' within `db' *)
            let self, db = self#phrase db in
            let self, pat = self#pattern pat in
            let self, body = self#option (fun o -> o#phrase) body in
            let self, rest = self#list
              (fun o (name, phrase) ->
                  let self, phrase = o#phrase phrase in
                    self, (name, phrase))
              rest in
              self, `DBUpdate (pat, db, body, rest))

    | `Switch (p, matches, dt) ->
        let o, p = self#phrase p in
        let _, matches = 
          o#list
            (fun o (pat, cont) ->
               let o', pat  = o#pattern pat in
               let _,  cont = o'#phrase cont in
                 (o, (pat, cont)))
            matches in
          self, `Switch (p, matches, dt)
    | `Receive (matches, dt) ->
        let _, matches = 
          self#list
            (fun o (pat, cont) ->
               let o', pat  = o#pattern pat in
               let _,  cont = o'#phrase cont in
                 (o, (pat, cont)))
            matches in
          self, `Receive (matches, dt)

    | `Formlet (body, yields) ->
        self#delimit
          (let self, body = self#phrase body in
           let self, _ = self#list (fun o -> o#binder) formlet_bindings in
           let _, yields = self#phrase yields in
             self, `Formlet (body, yields))
    | `FormBinding (phrase, pat) ->
        let self, phrase = self#phrase phrase in
        ({< formlet_bindings = (binders#pattern pat)#binders @ formlet_bindings >},
         `FormBinding (phrase, pat))
    | p                   -> super#phrasenode p

  method iterpatt = function
    | `List (pat, phrase) ->
        (* don't bind `pat' within `phrase' *)
        let self, phrase = self#phrase phrase in
        let self, pat    = self#pattern pat in 
          self, `List (pat, phrase)
    | `Table (pat, phrase) ->
        (* don't bind `pat' within `phrase' *)
        let self, phrase = self#phrase phrase in
        let self, pat    = self#pattern pat in 
          self, `Table (pat, phrase)

  method bindingnode = function
    | `Val (pat, rhs, loc, dt) -> 
        (* Don't bind `pat' in `rhs' *)
        let self, rhs = self#phrase rhs in
        let self, pat = self#pattern pat in
          self, `Val (pat, rhs, loc, dt)
    | `Foreign (lang, name, dt) ->
        if Substitution.in_domain subst name then
          ({< subst = Substitution.remove subst name >}, 
           `Foreign (lang, name, dt))
        else if Substitution.in_codomain subst name then
          failwith ("Internal error: cannot rename a variable to "^ name ^", since it is used as a foreign name")
        else self, `Foreign (lang, name, dt)

    | `Funs binders -> 
        (* Make sure all function names are bound in all rhs *)
        let self, binders = 
          self#list
            (fun o (binder, funlit, location, dt) ->
               let o, binder = self#binder binder in
               (o, (binder, funlit, location, dt)))
            binders in
        let self, binders = 
          self#list
            (fun o (binder, funlit, location, dt) ->
               let o, funlit = self#funlit funlit in
                 (o, (binder, funlit, location, dt)))
            binders in
          (self, `Funs binders)

    | `Abstract (sigs, bindings) -> 
        (* Bind `sigs' outside the scope of `bindings' only *)
        let self, bindings = self#delimit (self#list (fun o -> o#binding) bindings) in
        let self, sigs = self#list (fun o -> o#sigitem) sigs in
          self, `Abstract (sigs, bindings)

    (* the default will do fine in the following cases *)
    | `Type     _
    | `Fun      _
    | `Infix
    | `Exp      _ as s -> super#bindingnode s
end
