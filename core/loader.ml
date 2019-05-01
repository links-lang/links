open Utility
open SugarConstructors.SugartypesPositions
open Sugartypes

(* Filename of an external dependency *)
type ext_dep = string

(* Result of loading a file *)
type source = {
  program : Sugartypes.program;
  pos_context : SourceCode.source_code;
  external_dependencies: ext_dep list
}

let unpos  = SourceCode.WithPos.node

(* A module type is just a map telling us its (externally accessible / "public") submodules *)
type simpl_module_t = ModT of simpl_module_t StringMap.t


type module_env = simpl_module_t StringMap.t

(* Used only internally within this module *)
type toplevel_module_info = {
  (* The file AST as a single module binding *)
  tmi_ast : Sugartypes.program;
  tmi_mod_type : simpl_module_t;
  tmi_ffi_files : ext_dep list;
  tmi_pos_context : SourceCode.source_code;
}

let links_file_extension = ".links"
let module_name_regex = Str.regexp "[A-Z][a-zA-Z_0-9]*"

(* Given top-level module name, get expected filename *)
let top_level_filename module_name =
  (String.uncapitalize_ascii module_name) ^ links_file_extension


let path_sep = ":"





(* Turns a program corresponding to a file into a module by binding the potential expression to a variable.
    If*)
let modulify_program
    toplevel_module_name
    program
      : Sugartypes.program =
  let (bindings, phraseopt) = program in
  let open CommonTypes in
  match phraseopt with
    | None ->
      let themodule = with_dummy_pos (Module (toplevel_module_name, None, bindings)) in
      [themodule], None
    | Some phrase ->
      let phrase_var_name = "#thephrase" in
      let phrase_binder = with_dummy_pos (phrase_var_name, None) in
      let phrase_pattern_node : Pattern.with_pos = with_dummy_pos (Pattern.Variable phrase_binder) in
      let phrase_binding : Sugartypes.binding =
        with_dummy_pos
          (Val (phrase_pattern_node, ([], phrase), Location.Unknown, None)) in
      let combined_bindings = bindings @ [phrase_binding] in
      let themodule : Sugartypes.binding = with_dummy_pos (Module (toplevel_module_name, None, combined_bindings)) in
      let varnode : Sugartypes.phrasenode = (Var (QualifiedName.of_path [toplevel_module_name; phrase_var_name])) in
      let phrase_var_usage : Sugartypes.phrase = with_dummy_pos varnode in
      [themodule], Some phrase_var_usage


let try_parse_file module_name filename : string * Sugartypes.program * SourceCode.source_code =
  (* First, get the list of directories, with trailing slashes stripped *)
  let check_n_chop path =
    let dir_sep = Filename.dir_sep in
    if Filename.check_suffix path dir_sep then
      Filename.chop_suffix path dir_sep else path in

  let poss_stdlib_dir =
    let stdlib_path = Settings.get_value Basicsettings.StdLib.stdlib_path in
    if Settings.get_value Basicsettings.StdLib.use_stdlib then
      if stdlib_path <> "" then
        [check_n_chop stdlib_path]
      else
        (* Otherwise, follow the same logic as for the prelude.
         * Firstly, check the current directory.
         * Secondly, check OPAM *)
        let chopped_path = check_n_chop @@ Basicsettings.locate_file "stdlib" in
        [Filename.concat chopped_path "stdlib"]
    else [] in

  let poss_dirs =
    let path_setting = Settings.get_value Basicsettings.links_file_paths in
    let split_dirs = Str.split (Str.regexp path_sep) path_setting in
    "" :: "." :: poss_stdlib_dir @ (List.map (check_n_chop) split_dirs) in

  (* Loop through, trying to open the module with each path *)
  let rec loop = (function
    | [] -> failwith ("Could not find file " ^ filename)
    | x :: xs ->
        let candidate_filename =
          if x = "" then filename else (x ^ Filename.dir_sep ^ filename) in
        if Sys.file_exists candidate_filename then
          let program, pos_context = Parse.parse_file Parse.program candidate_filename in
          (candidate_filename, modulify_program module_name program, pos_context)
        else
          loop xs) in
  loop poss_dirs


(* File path to module name *)
let module_name_of_file_path path =
  let basename = Filename.basename path in
  let without_extension = Filename.chop_suffix basename links_file_extension in
  let module_name_candidate = String.capitalize_ascii without_extension in
  if Str.string_match module_name_regex module_name_candidate 0 then
    module_name_candidate
  else
    failwith ("Illegal file name for a links module: " ^ basename)


let _print_sorted_deps xs =
  print_list (List.map print_list xs)

let assert_no_cycles = function
  | [] -> ()
  | []::_ys -> ()
  | [_x]::_ys -> ()
  | (x::xs)::_ys -> failwith ("Error -- cyclic dependencies: " ^ (String.concat ", " (x :: xs)))


(* Sets up the initial environment such that the Lib and Prelude modules are always in scope,
   meaning that they will never be attempted to be resolved by loading corresponding files *)
let initial_module_env =
  let with_lib = StringMap.add Lib.BuiltinModules.lib (ModT StringMap.empty) StringMap.empty in
  let with_prelude = StringMap.add Lib.BuiltinModules.prelude (ModT StringMap.empty) with_lib in
  with_prelude



let disabled_method () = failwith "Internal Error. This function must be unreachable."


(* Traversal to find references to external modules and FFI file dependencies *)
let rec find_module_refs
    resolve_external (* If false, will not resolve external files but only detects FFI dependencies *)
    toplevel_module_name
    toplevel_module_file_path =
object(o : 'self_type)
  inherit SugarTraversals.fold as super

  (* The name of the currently processed toplevel module *)
  val toplevel_module_name = toplevel_module_name


  (* Path of the currently processed file*)
  val current_path = toplevel_module_file_path

  (* List of tuples (a,b), where a and be are paths, indicating that a depends on b *)
  val dependency_graph : (string * string) list = []

  (* FFI files that the *current* file uses *)
  val ffi_files = StringSet.empty

  (* Maps file names to their AST, module type, and path *)
  val file_env : toplevel_module_info StringMap.t = StringMap.empty

  (* Maps toplevel module names to file paths *)
  val toplevel_name_env = StringMap.empty

  (* Contains locally defined modules and those submodules of opened modules.
     Accessing anything in here does not result in a (new) dependency to a file *)
  val module_env: module_env = initial_module_env


  (* Updates the current object with info from an object that traversed other files *)
  method update (prev_o : 'self_type) : 'self_type =
  {< dependency_graph = prev_o#get_dependency_graph ();
     file_env = prev_o#get_file_env ();
     toplevel_name_env = prev_o#get_toplevel_name_env () >}


  method get_toplevel_name_env () = toplevel_name_env
  method set_toplevel_name_env toplevel_name_env = {< toplevel_name_env = toplevel_name_env >}
  method get_dependency_graph () = dependency_graph
  method get_ffi_files () = ffi_files
  method get_file_env () = file_env
  method set_file_env file_env = {< file_env = file_env >}
  method get_env () = module_env
  method set_env env = {< module_env = env >}
  (*method add_toplevel_module path mod_type ast ffi_files pos_context =
    {< file_env =
      StringMap.add
        path
        { tmi_ast = ast; tmi_mod_type = mod_type; tmi_ffi_files = ffi_files; tmi_pos_context = pos_context } file_env >} *)

  method! program _ = disabled_method ()
  method! binding _ = disabled_method ()
  method! bindingnode _ = disabled_method ()
  method! sentence _ = disabled_method ()



  method add_ffi_dependency filename =
    {< ffi_files = StringSet.add filename ffi_files >}

  method add_module_dependency from_module to_module =
      if List.mem (from_module, to_module) dependency_graph then
        o
      else
        let extended_dependencies = (from_module, to_module) :: dependency_graph in
        let nodes =
          List.fold_left
          (fun set (a,b)  -> StringSet.add a (StringSet.add b set))
          StringSet.empty
          extended_dependencies in
        let nodes_list = StringSet.fold (fun e l -> e :: l) nodes [] in
        assert_no_cycles (Graph.strongly_connected_components nodes_list extended_dependencies);
        {< dependency_graph = extended_dependencies >}

  method resolve_toplevel_module module_name =
    if resolve_external then
      match StringMap.find_opt module_name toplevel_name_env with
        | Some filepath ->
          let o = o#add_module_dependency current_path filepath in
          let {tmi_mod_type = module_type;_} = StringMap.find filepath file_env in
          o, module_type
        | None ->
          print_endline ("trying to resolve "^ module_name);
          let filename = top_level_filename module_name in
          let (filepath, (ast : Sugartypes.program), pos_context) =
            try_parse_file module_name filename in
          let o = o#add_module_dependency current_path filepath in

          let other_file_o = find_module_refs true module_name filepath in
          let other_file_o = other_file_o#update o in

          (* recurse into the module just loaded *)
          let (other_file_o, mod_type) = other_file_o#toplevel_module ast pos_context in

          o#update other_file_o, mod_type
    else
      o, ModT StringMap.empty



  method resolve_qualified_module_name qn  =
    let rec rslv mod_type qn = match mod_type, qn with
      | ModT map, `Ident m_name -> StringMap.find m_name map
      | ModT map, `Dot (m_name, remainder) ->
        let sub_mod_type = StringMap.find m_name map in
        rslv sub_mod_type remainder in
    let first_path_element = QualifiedName.head qn in
    match StringMap.find_opt first_path_element module_env, qn with
      | Some s_mod_type, `Dot _->
        begin
          try
            o, rslv s_mod_type (QualifiedName.tail qn)
          with
            NotFound _ ->
              (* We ignore missing modules here and let the type-checker tell the user
                about this problem later on *)
              o, ModT StringMap.empty
        end
      | Some s_mod_type, _ ->
        o, s_mod_type
      | None, _ ->
        let o, module_type = o#resolve_toplevel_module first_path_element in
        o, rslv module_type (QualifiedName.tail qn)


  method resolve_qualified_var_or_type_name qn =
    if QualifiedName.is_qualified qn then
      let qn_head = QualifiedName.head qn in
      match StringMap.find_opt qn_head module_env with
        | Some _ -> o
        | None -> fst (o#resolve_toplevel_module qn_head)
    else
      o


  method handle_scoped_phrase bindings phrase =
    let original_env = module_env in
      let o = List.fold_left
        (fun cur_o b ->
          fst (cur_o#bindingnode' (unpos b) ))
        o
        bindings in
      let o = o#phrase phrase in
      o#set_env original_env

  method! phrasenode = function
    | Block (bs, p) ->
      o#handle_scoped_phrase bs p
    | Var qname ->
      o#resolve_qualified_var_or_type_name qname
    | x -> super#phrasenode x

  method! cp_phrasenode = function
    | CPUnquote (bs, p) ->
      o#handle_scoped_phrase bs p
    | x -> o#cp_phrasenode x


  (* Returns module type, if binding a module *)
  method bindingnode' = fun (bn : Sugartypes.bindingnode) ->
    match bn with
      | Module (n, _, bs) ->
        (* Add imported stuff to env *)
        let (o, submodule_map) = List.fold_left
          (fun (prev_o, prev_submodule_map) b ->
            let o, mod_opt = prev_o#bindingnode' (unpos b) in
            let submodule_map =
              match mod_opt with
                | None -> prev_submodule_map
                | Some (sub_module_name, sub_module_type) ->
                  StringMap.add sub_module_name sub_module_type prev_submodule_map in
            o, submodule_map)
          (o, StringMap.empty)
          bs in
          (* update the original env with the module we just saw *)
          let extended_env = StringMap.add n (ModT submodule_map) module_env in
          let o = o#set_env extended_env in
          o, Some (n, ModT submodule_map)

      | Import qname ->
        let o, (ModT  imported_module_env) = o#resolve_qualified_module_name qname in
        let extended_env = StringMap.superimpose imported_module_env module_env in
        let o = o#set_env extended_env in
        o, None

      | Foreign (_, _, _, filename, _) as b ->
        let o = (super#bindingnode b)  in
        o#add_ffi_dependency filename, None

      | _ -> (super#bindingnode bn), None


  method toplevel_module (mod_program : Sugartypes.program) pos_context : 'self_type * simpl_module_t =
    match mod_program with
      | [mod_binding], _ ->
        let (o, mod_type_opt) = o#bindingnode' (unpos mod_binding) in
        begin match mod_type_opt with
          | None -> failwith "bindingnode' on toplevel module must yield module type"
          | Some (_, t) ->
            let entry_for_file =
              {
                tmi_ast = mod_program;
                tmi_mod_type = t;
                tmi_ffi_files = StringSet.fold (fun el l -> el :: l) (o#get_ffi_files  ()) [];
                tmi_pos_context = pos_context;
              } in
            let extended_file_env = StringMap.add current_path entry_for_file (o#get_file_env ()) in
            let extended_name_env = StringMap.add toplevel_module_name current_path (o#get_toplevel_name_env ()) in
            let o = o#set_file_env extended_file_env in
            let o = o#set_toplevel_name_env extended_name_env in
            (o, t)
        end
      | _ -> failwith "Toplevel program must contain a single binding"

end



(** Unmarshal an IR program from a file along with naming and typing
    environments and the fresh variable counters.
*)
(*let read_a filename : ('a) =
  let x, (gc, tc, vc) =
    call_with_open_infile filename ~binary:true Marshal.from_channel
  in
    Utility.gensym_counter := gc;
    Types.type_variable_counter := tc;
    Var.variable_counter := vc;
    x

let read_program filename : (envs * program) = read_a filename

(* measuring only *)
let read_program filename : (envs * program) =
  measure ("read_program "^filename) read_program filename*)

(* Parses a single file without resolving (i.e., loading) any of its dependenices (i.e., other files) *)
let load_source_file file_path : source =
  if Sys.file_exists file_path then
    let (plain_ast, source_context) = Parse.parse_file Parse.program file_path in
    let module_name = module_name_of_file_path file_path in
    let modulified_ast = modulify_program module_name plain_ast in
    let ffi_finder = find_module_refs false module_name file_path in
    let ffi_finder, _ = ffi_finder#toplevel_module modulified_ast source_context in
    {
      program = modulified_ast;
      pos_context = source_context;
      external_dependencies = StringSet.fold (fun el l -> el :: l) (ffi_finder#get_ffi_files  ()) []
    }
  else
    failwith ("Cannot load " ^ file_path ^ ", it does not exist.")


(* Parses a list offiles and also all of their dependencies, excluding the prelude.
   Returns a list representing a topological sort of all the given files and the dependencies.
   The only way the Prelude may ever end up in the returned list if it was part of the argument list *)
let load_source_files_and_dependencies file_paths : source list =
  let load file =
    let (plain_ast, pos_context) = Parse.parse_file Parse.program file in
    let module_name = module_name_of_file_path file in
    modulify_program module_name plain_ast, pos_context in

  match file_paths with
    | [] -> []
    | f :: fs ->
      let first = find_module_refs true (module_name_of_file_path f) f in
      let first_ast, first_pos_context = load f in
      let (first, _) = first#toplevel_module first_ast first_pos_context in

      let (o : 'self_type), _ = List.fold_left
        (fun (o, previous_path) path ->
          let this_file_o = find_module_refs true (module_name_of_file_path path) path in
          let ast, pos_context = load path in
          let this_file_o : 'self_type = this_file_o#update o in
          let next_o : 'self_type = fst (this_file_o#toplevel_module ast pos_context) in
          (* Enforces that the originally requested modules are processed in the order they were given *)
          let next_o : 'self_type = next_o#add_module_dependency path previous_path in
          next_o, path)
          (first, f)
          fs in

      let file_env = o#get_file_env () in
      let dependecy_graph_nodes = StringMap.fold (fun path _ paths -> path :: paths) file_env [] in
      let dependecy_graph_edges = o#get_dependency_graph () in

      let topo_sorted_paths = Graph.topological_sort dependecy_graph_nodes dependecy_graph_edges in
      let topo_sorted_results = List.map
        (fun path ->
          let file_env_entry = StringMap.find path file_env in
          {
            program = file_env_entry.tmi_ast;
            pos_context = file_env_entry.tmi_pos_context;
            external_dependencies = file_env_entry.tmi_ffi_files;
          }) topo_sorted_paths in
      topo_sorted_results





(*
(** Read source code from a file, parse, infer types and desugar to
    the IR *)
let read_file_source (nenv, tyenv) (filename:string) =
  let sugar, pos_context =
    ModuleUtils.try_parse_file filename in

  let module_name = String.capitalize_ascii (Filename.remove_extension (Filename.basename filename)) in
  let sugar = [modulify_program "Prelude" sugar], None in
  (*print_endline (Sugartypes.show_program sugar); flush stdout;*)

  (* printf "AST: \n %s \n" (Sugartypes.show_program sugar); *)
  let ((program, t, tenv), ffi_files) = Frontend.Pipeline.program tyenv pos_context sugar in
  let globals, main, nenv =
    Sugartoir.desugar_program
      (nenv,
       Var.varify_env (nenv, tyenv.FrontendTypeEnv.var_env),
       tyenv.FrontendTypeEnv.effect_row) program
  in
  {
    envs = (nenv, tenv);
    program = (globals, main, t);
    external_dependencies = ffi_files
  }

*)

(** Loads a named file and prints it as sugar syntax *)
let print file_path =
  let (plain_ast, _) = Parse.parse_file Parse.program file_path in
  print_string (Sugartypes.show_program plain_ast)
