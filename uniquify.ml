open Utility

(* Maps unique names to original names and an index *)

type unique_name = string
let make_unique_name name = gensym ~prefix=name ()

type unique_var_map = unique_name stringmap
type unique_ast = Sugartypes.program * unique_var_map

(* Gets the AST component of a uniquified AST *)
let get_ast (prog, _) = prog

(* Looks up a uniquified variable from the unique ast, resolves to a plain name *)
let lookup_var n u_ast =
  let (_, var_map) = u_ast in
  StringMap.find n var_map


let uniquify ast unique_map =
object (self)
  inherit SugarTraversals.fold_map as super
  val unique_map = unique_map
  (* Adds a uniquified name to the map *)
  method add_name old_name new_name = {< unique_map = StringMap.add name unique_map >}

  method binder (name, dt, pos) =
    let uniquified_name = make_unique_name name in
    (self#add_name name uniquified_name, uniquified_name)

  (* Some names in bindingnode need to be renamed, but aren't referred to using binders *)
  (* TODO: QualifiedImport *)
  method bindingnode = function
    | `Import n ->
      let uniquified_name = make_unique_name n in
      (self#add_name n uniquified_name, `Import uniquified_name)
    | `Type (n, xs, dt) ->
      let uniquified_name = make_unique_name n in
      (self#add_name n uniquified_name, `Type (uniquified_name, xs, dt'))
    (* For the algorithm to work in the presence of functions whose order doesn't matter,
     * we need to disambiguate between the occurrence as a binding, and the
     * occurrence to be used *within* the function for recursion. Wart, but eh. *)
    | `Fun (((n, dtopt1, pos), _, _, _, _) as fn_body)  ->
        let uniquified_name = make_unique_name n in
        let (o, fn_body1) = super#bindingnode (`Fun fn_body) in
        (`Def (uniquified_name, fn_body_1), o#add_name n uniquified_name)
    | `Module (n, p) ->
      let uniquified_name = make_unique_name n in
      let o = self#add_name n uniquified_name in
      let (o1, p1) = self#phrase p in
      (o1, `Module (uniquified_name, p))
    | bn -> super#bindingnode bn

  (* TODO: QualifiedVar *)
  method phrasenode = function
    | `Var name ->
        let uniquified_name = make_unique_name name in
        (self#add_name name uniquified_name, `Var uniquified_name)
    | p -> super#phrasenode p
end

(* Creates a uniquified AST from a plain AST *)
let uniquify_ast prog =
  let (o, prog1) = (uniquify ast StringMap.empty)#program prog in
  let unique_map = o#get_unique_map in
  (unique_map, prog1)

