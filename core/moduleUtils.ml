open Utility
open Printf
open SourceCode.WithPos
open Sugartypes

(* Paths to look for .links files in chasing pass *)
let links_file_paths
  = Settings.(multi_option ~default:["."] "links_file_paths"
              |> synopsis "Search paths for Links modules"
              |> hint "<dir[,dir']...>"
              |> keep_default
              |> to_string string_of_paths
              |> convert parse_paths
              |> CLI.(add (long "path"))
              |> sync)


(* Should we use the extra standard library definitions? *)
let use_stdlib
  = Settings.(flag ~default:true "use_stdlib"
              |> convert parse_bool
              |> sync)

(* Standard library path *)
let stdlib_path =
  Settings.(option ~default:(Some Linkspath.stdlib) "stdlib_path"
            |> to_string from_string_option
            |> convert Utility.(Sys.expand ->- some)
            |> sync)

let module_sep = "."

type term_shadow_table = string list stringmap
type type_shadow_table = string list stringmap
type shadow_table = string list stringmap

let try_parse_file filename =
  (* First, get the list of directories, with trailing slashes stripped *)
  let check_n_chop path =
    let dir_sep = Filename.dir_sep in
    if Filename.check_suffix path dir_sep then
      Filename.chop_suffix path dir_sep else path in

  let poss_stdlib_dir =
    if Settings.get use_stdlib then
      match Settings.get stdlib_path with
      | None ->
        (* Follow the same logic as for the prelude.
         * Firstly, check the current directory.
         * Secondly, check OPAM *)
        let chopped_path = check_n_chop @@ locate_file "stdlib" in
        [Filename.concat chopped_path "stdlib"]
      | Some stdlib_path ->
         [check_n_chop stdlib_path]
    else []
  in

  let poss_dirs =
    let paths = Settings.get links_file_paths in
    "" :: poss_stdlib_dir @ (List.map (check_n_chop) paths)
  in

  (* Loop through, trying to open the module with each path *)
  let rec loop = (function
    | [] -> raise (Errors.module_error ("Could not find file " ^ filename))
    | x :: xs ->
        let candidate_filename =
          if x = "" then filename else (x ^ Filename.dir_sep ^ filename) in
        if Sys.file_exists candidate_filename then
          Parse.parse_file Parse.program candidate_filename
        else
          loop xs) in
  loop poss_dirs

let has_no_modules =
object
  inherit SugarTraversals.predicate as super

  val has_no_modules = true
  method satisfied = has_no_modules

  method! bindingnode = function
    | Open _
    | Module _ -> {< has_no_modules = false >}
    | b -> super#bindingnode b

  method! datatypenode = function
    | Datatype.QualifiedTypeApplication _ -> {< has_no_modules = false >}
    | dt -> super#datatypenode dt

  method! phrasenode = function
    | QualifiedVar _ -> {< has_no_modules = false >}
    | pn -> super#phrasenode pn
end


let separate_modules =
  List.fold_left (fun (mods, binds) b ->
    match b with
      | {node = Module _; _} as m -> (m :: mods, binds)
      | b -> (mods, b :: binds)) ([], [])

type module_info = {
    simple_name : string; (* Note: not fully-qualified *)
    inner_modules : string list;
    type_names : string list;
    decl_names : string list
  }

let make_module_info simple_name inner_modules type_names decl_names =
  { simple_name = simple_name; inner_modules = inner_modules;
    type_names = type_names; decl_names = decl_names }

let get_pat_vars () =
  object(self)
    inherit SugarTraversals.fold as super
    val bindings = []
    method add_binding x = {< bindings = x :: bindings >}
    method get_bindings = bindings (* Order doesn't matter *)

    method! patternnode = function
      | Pattern.Variant (_n, p_opt) ->
         self#option (fun o p -> o#pattern p) p_opt
      (* | `Negative ns -> self#list (fun o p -> o#add_binding p) ns *)
      | Pattern.Record (ls, p_opt) ->
          let o1 = self#list (fun o (_, p) -> o#pattern p) ls in
          o1#option (fun o p -> o#pattern p) p_opt
      | Pattern.Variable bndr -> self#add_binding (Binder.to_name bndr)
      | p -> super#patternnode p
  end

let get_pattern_variables p = ((get_pat_vars ())#pattern p)#get_bindings

(* Gets the list of external FFI files to include *)
let get_ffi_files_obj =
  object(self)
    inherit SugarTraversals.fold as super
    val filenames = []
    method add_external_file x =
      if (List.mem x filenames) then
        self
      else
        {< filenames = x :: filenames >}

    method get_filenames = List.rev filenames

    method! bindingnode = function
      | Foreign alien ->
         self#add_external_file (Alien.object_file alien)
      | x -> super#bindingnode x
  end

let get_ffi_files prog = (get_ffi_files_obj#program prog)#get_filenames

let make_path_string xs name =
  if name = "" then "" else
    let xs1 = xs @ [name] in
    String.concat module_sep xs1

(* Need to get data constructors from type declarations *)
let get_data_constructors init_constrs =
    object (self)
        inherit SugarTraversals.fold as super
        val constrs = init_constrs
        method add_constr constr =
            {< constrs = StringSet.add constr constrs >}
        method get_constrs = StringSet.elements constrs

        method! datatypenode = function
            | Datatype.Variant (xs, _) ->
                self#list (fun o (lbl, _) -> o#add_constr lbl) xs
            | dt -> super#datatypenode dt
    end

let create_module_info_map program =
  (* Helper functions *)
  let module_map = ref StringMap.empty in
  let add_module_info fq_module_name info =
    let mm = !module_map in
    module_map := StringMap.add fq_module_name info mm in

  let rec create_and_add_module_info parent_path name bindings =
    (* Helper functions: traversing modules, and getting binding names *)
    (* Recursively traverse a list of modules *)
    let rec traverse_modules = function
      | [] -> []
      | {node=Module { binder; members };_} :: bs ->
          (* Recursively process *)
          let submodule_name = Binder.to_name binder in
          let new_path = if name = "" then [] else parent_path @ [name] in
          create_and_add_module_info new_path submodule_name members;
          (* Add the name to the list, process remainder. *)
          submodule_name :: (traverse_modules bs)
      | _bs -> assert false in (* List should only contain modules *)

    (* Getting binding names -- we're interested in function and value names *)
    let rec get_binding_names = function
      | [] -> []
      | { node = Val (pat, _, _, _); _ } :: bs ->
         (get_pattern_variables pat) @ get_binding_names bs
      | { node = Fun fn; _ } :: bs ->
         Binder.to_name fn.fun_binder :: (get_binding_names bs)
      | { node = Funs fs ; _ } :: bs ->
          (List.map (fun fn -> Binder.to_name fn.node.rec_binder) fs)
          @ get_binding_names bs
      | _ :: bs -> get_binding_names bs in (* Other binding types are uninteresting for this pass *)

    (* Getting type names -- we're interested in typename/effectname decls *)
    let rec get_type_names = function
      | [] -> []
      | b :: bs ->
          match node b with
            | Aliases ts ->
                let ns = ListUtils.concat_map (fun {node=(n, _, _); _} -> [n]) ts in
                ns @ (get_type_names bs)
            | _ -> get_type_names bs in

    (* Gets data constructors for variants *)
    let get_constrs bs = ((get_data_constructors StringSet.empty)#list
        (fun o -> o#binding) bs)#get_constrs in

    (* Next, separate out bindings *)
    let (inner_modules, other_bindings) = separate_modules bindings in
    (* Next, use our helper functions *)
    let inner_module_names = traverse_modules inner_modules in
    let constrs = get_constrs other_bindings in
    let binding_names = get_binding_names other_bindings @ constrs in
    let type_names = get_type_names other_bindings in
    (* Finally, construct the module info, and add to the table. *)
    let path_str = make_path_string parent_path name in
    let mod_info = make_module_info name inner_module_names type_names binding_names in
    add_module_info path_str mod_info in

  (* Toplevel *)
  let (bindings, _) = program in
  create_and_add_module_info [] "" bindings;
  !module_map

let print_mod_info k mi =
   printf "MODULE: %s\n" k;
   printf "Inner modules: %s\n" (print_list mi.inner_modules);
   printf "Type names: %s\n" (print_list mi.type_names);
   printf "Decl names: %s\n" (print_list mi.decl_names)

let _print_mt mt =
  printf "MT:\n";
  List.iter (fun (k, mi) -> print_mod_info k mi) (StringMap.bindings mt)

(* Given a binding name and fully-qualified name, adds it to the top of
 * the binding stack in the name shadowing table. For example, shadowing name foo with A.foo,
 * given a table
 *   foo |-> [B.foo]
 *   bar |-> [A.bar]
 * will result in
 *   foo |-> [A.foo, B.foo]
 *   bar |-> [A.bar]
 *)
let shadow_binding : string -> string -> (string list) stringmap -> (string list) stringmap = fun name fqn ht ->
  try
    let xs = StringMap.find name ht in
    StringMap.add name (fqn :: xs) ht
  with _ ->
    StringMap.add name [fqn] ht

(* Given a *fully qualified* module name and a name shadowing table, shadows
 * the appropriate bindings. For example, given a name resolution table:
 *   foo |-> [A.foo, foo]
 *   bar |-> [bar]
 * and a module B:
 *   module B {
 *     module C {
 *       def pines() { .. }
 *     }
 *
 *     def foo() { .. }
 *     val baz = ..
 *   }
 *  the resulting table would be:
 *  foo |-> [B.foo, A.foo, foo]
 *  bar |-> [bar]
 *  baz |-> [B.baz]
 *  B   |-> [B]
 *  C   |-> [B.C]
 *)
let shadow_open module_plain module_fqn module_table term_ht type_ht =
  (* print_mt module_table; *)
  try
    let mod_info = StringMap.find module_fqn module_table in
    (* Shadows bindings in a given table *)
    let shadow_all_bindings lst ht =
        List.fold_left (fun acc plain_binding_name ->
            let fq_binding_name =
                String.concat module_sep (module_fqn :: [plain_binding_name]) in
            shadow_binding plain_binding_name fq_binding_name acc) ht lst in

    (* Shadow both term and type tables *)
    let shadowed_term_ht = shadow_all_bindings mod_info.decl_names term_ht in
    let shadowed_type_ht = shadow_all_bindings mod_info.type_names type_ht in
    (* Next, do the modules *)
    let shadow_modules ht mods =
        List.fold_left (fun acc plain_module_name ->
              let fq_module_name =
                  String.concat module_sep (module_fqn :: [plain_module_name]) in
               shadow_binding plain_module_name fq_module_name acc) ht mods in

    let shadowed_term_ht =
        shadow_modules shadowed_term_ht mod_info.inner_modules in
    let shadowed_type_ht =
        shadow_modules shadowed_type_ht mod_info.inner_modules in

    (* Finally, need to add this module of course! *)
    let shadowed_term_ht =
        shadow_binding module_plain module_fqn shadowed_term_ht in
    let shadowed_type_ht =
        shadow_binding module_plain module_fqn shadowed_type_ht in
    (shadowed_term_ht, shadowed_type_ht)
  with Notfound.NotFound _ ->
    raise (Errors.module_error ("Error: Trying to import nonexistent module " ^ module_plain))

let lst_to_path = String.concat module_sep


let contains_modules prog = not ((has_no_modules#program prog)#satisfied)
