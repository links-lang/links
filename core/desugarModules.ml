(* Desugaring of modules.

   Eliminates modules from the AST by prefixing names... *)

(* ... aka a poor person's name resolution. *)
(* I hope this code can be repurposed for a proper name resolution
   pass later. *)


(*
 * Desugars modules into plain binders.
 *
 * module Foo {
 *    val bobsleigh = ...;
 *    fun x() {
 *    }
 *
 *    module Bar {
 *      fun y() {
 *      }
 *    }
 * }
 * val x = ...;
 *
 *  --->
 *
 * val Foo_0$bobsleigh_0 = ...;
 * fun Foo_0$x1() { ...}
 * fun Foo_0$Bar_2$y_0() { ... }
 * val x1 = ...;
 *
 * The names are internal. The [Name.prettify] function in
 * [module_hacks.ml] attempts to recover the source representation of
 * names.
 *)

(* Resolution strategy: We make use of a two-level scope data
   structure to build up static scopes. The "outer" level represents
   the parent scope, whilst the "inner" represents the scope being
   built. Upon entry to a module scope the "inner" scope becomes an
   "outer" scope, and exploration of the module initiates with an
   empty "inner" scope. Meaning that after the exploration, the
   "inner" scope only contains the top-level bindings of the said
   module. After exploration we restore the old "outer" and "inner"
   context, and add the explored module to the "inner" context with
   its scope. *)


open Utility
open Sugartypes
open SourceCode.WithPos

(* TODO FIXME: use ropes rather than strings to build names. *)
module Epithet = struct
  type t =
    { mutable next: int;
      prefix: (int * string) list }

  let empty : t =
    { next = 0; prefix = []; }

  (* Persistent *)
  let remember ?(escapes=false) name st =
    let prefix =
      if escapes
      then let next = st.next in
           (st.next <- next + 1; (st.next, "?") :: st.prefix)
      else st.prefix
    in
    let next = st.next in
    st.next <- next + 1;
    { empty with prefix = (next, name) :: prefix }

  let build components =
    let components' =
      List.fold_left
        (fun suffix (i, name) ->
          (if i < 0 then name else Printf.sprintf "%s_%d" name i) :: suffix)
        [] components
    in
    String.concat "$" components'

  let expand st name =
    build ((-1, name) :: st.prefix)

  let expand_escapee st name =
    let next = st.next in
    st.next <- next + 1;
    build ((-1, name) :: (next, "?") :: st.prefix)
end

(* The following data structures model scopes. *)
module BasicScope = struct
  type t =
    { modules: module_member StringMap.t;
      typenames: type_member StringMap.t;
      terms: term_member StringMap.t }
  and module_member = t
  and type_member = string
  and term_member = string

  let empty =
    { modules = StringMap.empty;
      typenames = StringMap.empty;
      terms = StringMap.empty }

  module Resolve = struct
    let rec module' : name list -> t -> t
      = fun names scope ->
      match names with
      | [] -> assert false
      | [name] -> StringMap.find name scope.modules
      | prefix :: names -> module' names (StringMap.find prefix scope.modules)

    let rec var : name list -> t -> name
      = fun names scope ->
      match names with
      | [] -> assert false
      | [name] -> StringMap.find name scope.terms
      | prefix :: names ->
         var names (StringMap.find prefix scope.modules)

    let rec typename : name list -> t -> name
      = fun names scope ->
      match names with
      | [] -> assert false
      | [name] -> StringMap.find name scope.typenames
      | prefix :: names ->
         typename names (StringMap.find prefix scope.modules)
  end

  let shadow : t -> t -> t
    = fun scope mask ->
    let select_mask _ _ mask = Some mask in
    let modules =
      StringMap.union select_mask scope.modules mask.modules
    in
    let typenames =
      StringMap.union select_mask scope.typenames mask.typenames
    in
    let terms =
      StringMap.union select_mask scope.terms mask.terms
    in
    { modules; typenames; terms }

  module Extend = struct
    let module' module_name module_scope scope =
      { scope with modules = StringMap.add module_name module_scope scope.modules }

    let typename typename typename' scope =
      { scope with typenames = StringMap.add typename typename' scope.typenames }

    let var name name' scope =
      { scope with terms = StringMap.add name name' scope.terms }

    let rec synthetic_module path module_scope scope =
      match path with
      | [] -> assert false
      | [name] ->
         { scope with modules = StringMap.add name module_scope scope.modules }
      | prefix :: path ->
         synthetic_module path module_scope (StringMap.find prefix scope.modules)
  end
end

(* Refined notion of scope. *)
module Scope = struct
  module S = BasicScope
  type scope = S.t
  type t = { inner: scope; outer: scope }

  let empty = { inner = S.empty; outer = S.empty }

  module Resolve = struct
    (* We do not produce an error if a name fails to resolve, which
       happens if a variable is unbound. We defer error handling to the
       type checker. We produce a "best guess" of its name, which is
       simply its qualified form. *)
    let best_guess : name list -> name
      = String.concat "."

    let module' : name list -> t -> scope
      = fun names scopes ->
      try S.Resolve.module' names scopes.inner
      with Notfound.NotFound _ ->
        S.Resolve.module' names scopes.outer (* Allow any errors to propagate. *)

    let qualified_var : name list -> t -> name
      = fun names scopes ->
      try S.Resolve.var names scopes.inner
      with Notfound.NotFound _ ->
        try S.Resolve.var names scopes.outer
        with Notfound.NotFound _ -> best_guess names

    let qualified_typename : name list -> t -> name
      = fun names scopes ->
      try S.Resolve.typename names scopes.inner
      with Notfound.NotFound _ ->
        try S.Resolve.typename names scopes.outer
        with Notfound.NotFound _ -> best_guess names

    let var : name -> t -> name
      = fun name scopes -> qualified_var [name] scopes

    let typename : name -> t -> name
      = fun name scopes -> qualified_typename [name] scopes
  end

  module Extend = struct
    let module' : name -> t -> t -> t
      = fun module_name module_scope scopes ->
      { scopes with inner = S.Extend.module' module_name module_scope.inner scopes.inner }

    let var : name -> string -> t -> t
      = fun term_name prefixed_name scopes ->
      { scopes with inner = S.Extend.var term_name prefixed_name scopes.inner }

    let typename : name -> string -> t -> t
      = fun typename prefixed_name scopes ->
      { scopes with inner = S.Extend.typename typename prefixed_name scopes.inner }

    let synthetic_module : name list -> scope -> t -> t
      = fun path module_scope scopes ->
      { scopes with inner = S.Extend.synthetic_module path module_scope scopes.inner }
  end

  let open_module : scope -> t -> t
    = fun module_scope scopes ->
    let inner = S.shadow scopes.inner module_scope in
    { scopes with inner }

  let renew : t -> t
    = fun scopes ->
    let outer = S.shadow scopes.outer scopes.inner in
    { outer; inner = S.empty }
end

let rec desugar_module : ?toplevel:bool -> Epithet.t -> Scope.t -> Sugartypes.binding -> binding list * Scope.t
  = fun ?(toplevel=false) renamer scope binding ->
  match binding.node with
  | Module (name, bs) ->
     let visitor = desugar ~toplevel (Epithet.remember ~escapes:(not toplevel) name renamer) (Scope.renew scope) in
     let bs'    = visitor#bindings bs in
     let scope' = visitor#get_scope in
     let scope'' = Scope.Extend.module' name scope' scope in
     (bs', scope'')
  | _ -> assert false
and desugar ?(toplevel=false) (renamer : Epithet.t) (scope : Scope.t) =
  let open Sugartypes in
  object(self : 'self_type)
    inherit SugarTraversals.map as super

    val mutable scope = scope
    val mutable renamer = renamer
    method get_renamer = renamer
    method get_scope = scope

    method clone =
      desugar ~toplevel:false renamer scope

    method type_binder : name -> name
      = fun name ->
      (* Construct a prefixed name for [name]. *)
      let name' =
        if toplevel then Epithet.expand renamer name
        else Epithet.expand_escapee renamer name
      in
      self#bind_type name name'; name'

    method! binder : Binder.with_pos -> Binder.with_pos
      = fun bndr ->
      let name = Binder.to_name bndr in
      let name' = if toplevel then Epithet.expand renamer name else name in
      self#bind_term name name';
      Binder.set_name bndr name'

    method bind_term name name' =
      scope <- Scope.Extend.var name name' scope

    method bind_type name name' =
      scope <- Scope.Extend.typename name name' scope

    method open_module pos path =
      try
        let module_scope = Scope.Resolve.module' path scope in
        scope <- Scope.open_module module_scope scope;
      with Notfound.NotFound _ ->
        raise (Errors.module_error ~pos (Printf.sprintf "Unbound module %s" (Scope.Resolve.best_guess path)))

    method import_module pos path =
      try
        let module_scope = Scope.Resolve.module' path scope in
        scope <- Scope.Extend.synthetic_module path module_scope scope
      with Notfound.NotFound _ ->
        raise (Errors.module_error ~pos (Printf.sprintf "Unbound module %s" (Scope.Resolve.best_guess path)))

    method! funlit : funlit -> funlit
      = fun (paramss, body) ->
      let visitor = self#clone in
      let paramss' =
        List.map
          (fun params ->
            List.map (fun param -> visitor#pattern param) params)
          paramss
      in
      let body' = visitor#phrase body in
      (paramss', body')

    method cases : (Pattern.with_pos * phrase) list -> (Pattern.with_pos * phrase) list
      = fun cases ->
      List.map
        (fun (pat, body) ->
          let visitor = self#clone in
          let pat'  = visitor#pattern pat in
          let body' = visitor#phrase body in
          (pat', body'))
        cases

    method! binop op =
      let open Operators.BinaryOp in
      match op with
      | Name name -> Name (Scope.Resolve.var name scope)
      | _ -> super#binop op

    method! unary_op op =
      let open Operators.UnaryOp in
      match op with
      | Name name -> Name (Scope.Resolve.var name scope)
      | _ -> super#unary_op op

    method! section sect =
      let open Operators.Section in
      match sect with
      | Name name -> Name (Scope.Resolve.var name scope)
      | _ -> super#section sect

    method! phrasenode = function
      | Block (bs, body) ->
       (* Enters a new scope, which is thrown away on exit. *)
        let visitor = self#clone in
        let bs'= visitor#bindings bs in
        let body' = visitor#phrase body in
        Block (bs', body')
      | Var name ->
       (* Must be resolved. *)
        Var (Scope.Resolve.var name scope)
      | QualifiedVar names ->
       (* Must be resolved. *)
        Var (Scope.Resolve.qualified_var names scope)
      | Escape (bndr, body) ->
        let visitor = self#clone in
        let bndr' = visitor#binder bndr in
        let body' = visitor#phrase body in
        Escape (bndr', body')
      | Handle { sh_expr; sh_effect_cases; sh_value_cases; sh_descr } ->
         let sh_expr = self#phrase sh_expr in
         let shd_params =
           self#option (fun o -> o#handle_params) sh_descr.shd_params
         in
         let sh_effect_cases = self#cases sh_effect_cases in
         let sh_value_cases = self#cases sh_value_cases in
         Handle { sh_expr; sh_effect_cases; sh_value_cases; sh_descr = { sh_descr with shd_params } }
      | Switch (expr, cases, dt) ->
        let expr' = self#phrase expr in
        let cases' = self#cases cases in
        Switch (expr', cases', dt)
      | Receive (cases, dt) ->
        let cases' = self#cases cases in
        Receive (cases', dt)
      | FormBinding (body, pat) ->
        let visitor = self#clone in
        let body' = visitor#phrase body in
        let pat' = visitor#pattern pat in
        FormBinding (body', pat')
      | Offer (expr, cases, dt) ->
        let expr' = self#phrase expr in
        let cases' = self#cases cases in
        Offer (expr', cases', dt)
      | TryInOtherwise (expr, x, body, catch, dt) ->
        let expr' = self#phrase expr in
        let visitor = self#clone in
        let x' = visitor#pattern x in
        let body' = visitor#phrase body in
        let catch' = self#phrase catch in
        TryInOtherwise (expr', x', body', catch', dt)
      | CP cp_exp ->
       (* CP introduces a new scope. *)
         let visitor = self#clone in
         CP (visitor#cp_phrase cp_exp)
      | p -> super#phrasenode p

    method! datatypenode = let open Datatype in function
      | TypeApplication (name, args) ->
      (* Must be resolved. *)
        let args' = self#list (fun o -> o#type_arg) args in
        TypeApplication (Scope.Resolve.typename name scope, args')
      | QualifiedTypeApplication (names, args) ->
      (* Must be resolved. *)
        let args' = self#list (fun o -> o#type_arg) args in
        TypeApplication (Scope.Resolve.qualified_typename names scope, args')
      | dt -> super#datatypenode dt

    method! bindingnode = function
      | Fun (bndr, lin, (tvs, funlit), loc, dt) ->
       (* It is important to process [bndr] before processing
          [funlit] as functions are allowed to call themselves. *)
        let bndr' = self#binder bndr in
        let dt' = self#option (fun o -> o#datatype') dt in
        let funlit' = self#funlit funlit in
        Fun (bndr', lin, (tvs, funlit'), loc, dt')
      | Funs fs ->
        (* Assumes mutual typenames have been processed already,
           which appears to be guaranteed by the parser. *)
        (* Two passes:
           1) Register all the names such that they are available
              inside of each function body.
           2) Process the function bodies. *)
        let (fs' : recursive_function list) =
          List.fold_right
            (fun (bndr, lin, lit, loc, dt, pos) fs ->
              (self#binder bndr, lin, lit, loc, dt, pos) :: fs)
            fs []
        in
        let fs'' =
          List.fold_right
            (fun (bndr, lin, (tvs, funlit), loc, dt, pos) fs ->
              let dt' = self#option (fun o -> o#datatype') dt in
              let funlit' = self#funlit funlit in
              (bndr, lin, (tvs, funlit'), loc, dt', pos) :: fs)
            fs' []
        in
        Funs fs''
      | Typenames ts ->
       (* Must be processed before any mutual function bindings in
          the same mutual binding group. *)
       (* Same procedure as above. *)
         let ts' =
           List.fold_right
             (fun (name, tyvars, dt, pos) ts ->
               (self#type_binder name, tyvars, dt, pos) :: ts)
               ts []
         in
         let ts'' =
           List.fold_right
             (fun (name, tyvars, dt, pos) ts ->
                 let dt' = self#datatype' dt in
                 (name, tyvars, dt', pos) :: ts)
             ts' []
           in
           Typenames ts''
      | Val (pat, (tvs, body), loc, dt) ->
       (* It is important to process [body] before [pat] to avoid
          inadvertently bringing the binder(s) in [pat] into the
          scope of [body]. *)
         let body' = self#phrase body in
         let pat' = self#pattern pat in
         let dt' = self#option (fun o -> o#datatype') dt in
         Val (pat', (tvs, body'), loc, dt')
      | Foreign (bndr, raw_name, lang, ext_file, dt) ->
         let dt' = self#datatype' dt in
         let bndr' = self#binder bndr in
         Foreign (bndr', raw_name, lang, ext_file, dt')
      | AlienBlock (lang, lib, decls) ->
         let decls' =
           self#list
             (fun o (bndr, dt) ->
               let dt' = o#datatype' dt in
               let bndr' = o#binder bndr in
               (bndr', dt'))
             decls
         in
         AlienBlock (lang, lib, decls')
      | Module _ | Import _ | Open _ -> assert false (* Should have been processed by this point. *)
      | b -> super#bindingnode b

    method extension_guard pos =
      if not (Settings.get_value Basicsettings.modules) then
           raise (Errors.desugaring_error
                    ~pos ~stage:Errors.DesugarModules
                    ~message:("Modules are not enabled. To enable modules set the `modules' setting to true or use the flag `-m'."))

    method bindings = function
      | [] -> []
      | { node = Import names; pos } :: bs ->
         self#extension_guard pos;
         self#import_module pos names; self#bindings bs
      | { node = Open names; pos } :: bs ->
        (* Affects [scope]. *)
         self#extension_guard pos;
         self#open_module pos names; self#bindings bs
      | ({ node = Module (_name, _); pos } as module') :: bs ->
      (* Affects [scope] and hoists [bs'] *)
         self#extension_guard pos;
         let bs', scope' = desugar_module ~toplevel renamer scope module' in
         scope <- scope'; bs' @ self#bindings bs
      | b :: bs ->
         let b = self#binding b in
         b :: self#bindings bs

    method! program (bs, exp) =
      (* It is crucial that we enforce left-to-right evaluation of
         [bs] and [exp]. Note that OCaml uses right-to-left evaluation
         order. *)
      let bs' = self#bindings bs in
      (bs', self#option (fun o -> o#phrase) exp)

    method! sentence : sentence -> sentence = function
      | Definitions defs -> Definitions (self#bindings defs)
      | s -> super#sentence s
  end

let desugar_program : Sugartypes.program -> Sugartypes.program
  = fun program ->
  (* TODO move to this logic to the loader. *)
  let program = Chaser.add_dependencies program in
  let program = DesugarAlienBlocks.transform_alien_blocks program in
  (* Printf.fprintf stderr "Before elaboration:\n%s\n%!" (Sugartypes.show_program program); *)
  let result = (desugar ~toplevel:true Epithet.empty Scope.empty)#program program in
  (* Printf.fprintf stderr "After elaboration:\n%s\n%!" (Sugartypes.show_program result); *)
  result


let desugar_sentence : unit -> Sugartypes.sentence -> Sugartypes.sentence
  = fun () ->
  let scope : Scope.t ref = ref Scope.empty in
  let renamer : Epithet.t ref = ref Epithet.empty in
  fun sentence ->
  let sentence = Chaser.add_dependencies_sentence sentence in
  let sentence = DesugarAlienBlocks.sentence sentence in
  let visitor = desugar ~toplevel:true !renamer !scope in
  let result = visitor#sentence sentence in
  scope := visitor#get_scope; renamer := visitor#get_renamer; result
