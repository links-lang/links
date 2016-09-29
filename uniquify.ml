open Utility

(* Maps unique names to original names and an index *)

type unique_name = string
let make_unique_name name = gensym ~prefix:name ()

type unique_var_map = unique_name stringmap
type unique_ast = Sugartypes.program * unique_var_map

(* Gets the AST component of a uniquified AST *)
let get_ast (prog, _) = prog

(* Looks up a uniquified variable from the unique ast, resolves to a plain name *)
let lookup_var n u_ast =
  let (_, var_map) = u_ast in
  StringMap.find n var_map


let uniquify name_map =
object (self)
  inherit SugarTraversals.fold_map as super
  val unique_map = name_map
  (* Adds a uniquified name to the map *)
  method add_name old_name new_name = {< unique_map = StringMap.add new_name old_name unique_map >}
  method get_unique_map = unique_map

  method binder (name, dt, pos) =
    let uniquified_name = make_unique_name name in
    (self#add_name name uniquified_name, (uniquified_name, dt, pos))

  method datatype = function
    | `TypeVar (n, sk_opt, frdm) ->
        let uniquified_name = make_unique_name n in
        (self#add_name n uniquified_name, `TypeVar (uniquified_name, sk_opt, frdm))
    | `QualifiedTypeVar (ns, sk_opt, frdm) ->
        let (o1, ns1) =
          self#list (fun o n ->
            let uniquified_name = make_unique_name n in
            (o#add_name n uniquified_name, uniquified_name)) ns in
        (o1, `QualifiedTypeVar (ns1, sk_opt, frdm))
    | dt -> super#datatype dt

  (* Some names in bindingnode need to be renamed, but aren't referred to using binders *)
  method bindingnode = function
    | `QualifiedImport ns ->
        let (o, ns1) = self#list (fun o n ->
          let uniquified_name = make_unique_name n in
          (o#add_name n uniquified_name, uniquified_name)) ns in
        (o, `QualifiedImport ns1)
    | `Import n ->
      let uniquified_name = make_unique_name n in
      (self#add_name n uniquified_name, `Import uniquified_name)
    | `Type (n, xs, dt) ->
      let uniquified_name = make_unique_name n in
      (self#add_name n uniquified_name, `Type (uniquified_name, xs, dt))
    | `Module (n, p) ->
      let uniquified_name = make_unique_name n in
      let o = self#add_name n uniquified_name in
      let (o1, p1) = o#phrase p in
      (o1, `Module (uniquified_name, p1))
    | bn -> super#bindingnode bn

  method phrasenode = function
    | `QualifiedVar ns ->
        let (o, ns1) = self#list (fun o n ->
          let uniquified_name = make_unique_name n in
          (o#add_name n uniquified_name, uniquified_name)) ns in
        (o, `QualifiedVar ns1)
    | `Var name ->
        let uniquified_name = make_unique_name name in
        (self#add_name name uniquified_name, `Var uniquified_name)
    | p -> super#phrasenode p
end

(* Creates a uniquified AST from a plain AST *)
let uniquify_ast prog =
  let (o, prog1) = (uniquify StringMap.empty)#program prog in
  let unique_map = o#get_unique_map in
  (prog1, unique_map)

