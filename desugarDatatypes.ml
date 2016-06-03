open Types
open Sugartypes
open Utility
open List

module SEnv = Env.String

(* Check that no datatype is left undesugared. *)
let all_datatypes_desugared =
object (self)
  inherit SugarTraversals.predicate as super

  val all_desugared = true
  method satisfied = all_desugared

  method datatype' = function
      (_, None) -> {< all_desugared = false >}
    | _ -> self

  method phrasenode = function
    | `TableLit (_, (_, None), _, _, _) -> {< all_desugared = false >}
    | p -> super#phrasenode p
end

let concrete_subkind =
  function
  | Some subkind -> subkind
  | None         -> default_subkind

(* Find all unbound type variables in a term *)
let typevars =
object (self)
  inherit SugarTraversals.fold as super

  val tyvar_list : name list = []
  val tyvars : type_variable StringMap.t = StringMap.empty

  (* fill in subkind with the default *)
  val fill =
    function
    | (_, (_, Some _), _) as tv -> tv
    | (name, (pk, None), rest) -> (name, (pk, Some default_subkind), rest)

  method tyvar_list =
    List.map (fun name -> fill (StringMap.find name tyvars)) (List.rev tyvar_list)

  method private add_name name = {< tyvar_list = name :: tyvar_list >}
  method private register ((name, _, _) as tv) = {< tyvars = StringMap.add name tv tyvars >}
  method private bind tv = self#register tv

  method add ((name, (pk, sk), freedom) as tv) =
    if StringMap.mem name tyvars then
      let (_, (pk', sk'), freedom') = StringMap.find name tyvars in
      (* monotonically increase subkinding information *)
      let (sk, sk') =
        match sk, sk' with
        | Some sk, None  -> Some sk, Some sk
        | None, Some sk' -> Some sk', Some sk'
        | _, _           -> sk, sk' in
      let tv = (name, (pk, sk), freedom) in
      let tv' = (name, (pk', sk'), freedom') in
      (* check that duplicate type variables have the same kind *)
      if tv <> tv' then
        failwith ("kind mismatch in type variable: " ^
                  Sugartypes.Show_type_variable.show tv ^ " vs: " ^
                  Sugartypes.Show_type_variable.show tv');
      self#register tv
    else
      (self#register tv)#add_name name


  method bindingnode = function
    (* type declarations bind variables; exclude those from the
       analysis. *)
    | `Type _    -> self
    | b          -> super#bindingnode b

  method datatype = function
    | `TypeVar (x, k, freedom) -> self#add (x, (`Type, k), freedom)
    | `Mu (v, t)       -> let o = self#bind (v, (`Type, None), `Rigid) in o#datatype t
    | `Forall (qs, t)  ->
        let o =
          List.fold_left
            (fun o q ->
               o#bind (rigidify q))
            self
            qs
        in
          o#datatype t
    | dt                  -> super#datatype dt

  method row_var = function
    | `Closed               -> self
    | `Open (x, k, freedom) -> self#add (x, (`Row, k), freedom)
    | `Recursive (s, r)     -> let o = self#bind (s, (`Row, None), `Rigid) in o#row r

  method fieldspec = function
    | `Absent -> self
    | `Present t -> self#datatype t
    | `Var (x, k, freedom) -> self#add (x, (`Presence, k), freedom)
end

type var_env = { tenv : Types.meta_type_var StringMap.t;
                 renv : Types.meta_row_var StringMap.t;
                 penv : Types.meta_presence_var StringMap.t }

let empty_env = {tenv = StringMap.empty; renv = StringMap.empty; penv = StringMap.empty}

exception UnexpectedFreeVar of string

module Desugar =
struct
  let rec datatype var_env (alias_env : Types.tycon_environment) t =
  let datatype var_env t = datatype var_env alias_env t in
    let lookup_type t = StringMap.find t var_env.tenv in
      match t with
        | `TypeVar (s, _, _) -> (try `MetaTypeVar (lookup_type s)
                                with NotFound _ -> raise (UnexpectedFreeVar s))
        | `Function (f, e, t) ->
            `Function (Types.make_tuple_type (List.map (datatype var_env) f),
                       row var_env alias_env e,
                       datatype var_env t)
        | `Lolli (f, e, t) ->
            `Lolli (Types.make_tuple_type (List.map (datatype var_env) f),
                       row var_env alias_env e,
                       datatype var_env t)
        | `Mu (name, t) ->
            let var = Types.fresh_raw_variable () in
            (* FIXME: shouldn't we support other subkinds for recursive types? *)
            let point = Unionfind.fresh (`Var (var, default_subkind, `Flexible)) in
            let tenv = StringMap.add name point var_env.tenv in
            let _ = Unionfind.change point (`Recursive (var, datatype {var_env with tenv=tenv} t)) in
              `MetaTypeVar point
        | `Forall (qs, t) ->
            let desugar_quantifier (var_env, qs) =
              fun (name, kind, freedom) ->
                match kind with
                | `Type, subkind ->
                    let subkind = concrete_subkind subkind in
                    let var = Types.fresh_raw_variable () in
                    let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
                    let q = (var, subkind, `Type point) in
                    let var_env = {var_env with tenv=StringMap.add name point var_env.tenv} in
                      var_env, q::qs
                | `Row, subkind ->
                    let subkind = concrete_subkind subkind in
                    let var = Types.fresh_raw_variable () in
                    let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
                    let q = (var, subkind, `Row point) in
                    let var_env = {var_env with renv=StringMap.add name point var_env.renv} in
                      var_env, q::qs
                | `Presence, subkind ->
                    let subkind = concrete_subkind subkind in
                    let var = Types.fresh_raw_variable () in
                    let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
                    let q = (var, subkind, `Presence point) in
                    let var_env = {var_env with penv=StringMap.add name point var_env.penv} in
                      var_env, q::qs in

            let var_env, qs =
              List.fold_left desugar_quantifier (var_env, []) qs in
            let qs = List.rev qs in
            let t = datatype var_env t in
              `ForAll (Types.box_quantifiers qs, t)
        | `Unit -> Types.unit_type
        | `Tuple ks ->
            let labels = map string_of_int (Utility.fromTo 1 (1 + length ks)) in
            let unit = Types.make_empty_closed_row () in
            let present (s, x) = (s, `Present x)
            in
              `Record (fold_right2 (curry (Types.row_with -<- present)) labels (map (datatype var_env) ks) unit)
        | `Record r -> `Record (row var_env alias_env r)
        | `Variant r -> `Variant (row var_env alias_env r)
        | `Table (r, w, n) -> `Table (datatype var_env r, datatype var_env w, datatype var_env n)
        | `List k -> `Application (Types.list, [`Type (datatype var_env k)])
        | `TypeApplication (tycon, ts) ->
            begin match SEnv.find alias_env tycon with
              | None -> failwith (Printf.sprintf "Unbound type constructor %s" tycon)
              | Some (`Alias _) -> let ts = List.map (type_arg var_env alias_env) ts in
                  (* TODO: check that the kinds match up *)
                  Instantiate.alias tycon ts alias_env
              | Some (`Abstract abstype) ->
                  (* TODO: check that the kinds match up *)
                  `Application (abstype, List.map (type_arg var_env alias_env) ts)
            end
        | `Primitive k -> `Primitive k
        | `DB -> `Primitive `DB
        | (`Input _ | `Output _ | `Select _ | `Choice _ | `Dual _ | `End) as s -> session_type var_env alias_env s
  and session_type var_env alias_env =
    let lookup_type t = StringMap.find t var_env.tenv in
    (* HACKY *)
    function
    | `Input (t, s) -> `Input (datatype var_env alias_env t, datatype var_env alias_env s)
    | `Output (t, s) -> `Output (datatype var_env alias_env t, datatype var_env alias_env s)
    | `Select r -> `Select (row var_env alias_env r)
    | `Choice r -> `Choice (row var_env alias_env r)
    (* | `TypeVar (name, _, _) -> *)
    (*   begin *)
    (*     try `MetaSessionVar (lookup_type name) *)
    (*     with NotFound _ -> raise (UnexpectedFreeVar name) *)
    (*   end *)
    (* | `Mu (name, s) -> *)
    (*   let var = Types.fresh_raw_variable () in *)
    (*   let point = Unionfind.fresh (`Var (var, (`Any, `Session), `Flexible)) in *)
    (*   let tenv = StringMap.add name point var_env.tenv in *)
    (*   let _ = Unionfind.change point (`Recursive (var, *)
    (*                                               `Session (session_type {var_env with tenv=tenv} alias_env s))) in *)
    (*     `MetaSessionVar point *)
    | `Dual s -> `Dual (datatype var_env alias_env s)
    | `End -> `End
    | _ -> assert false

  and fieldspec var_env alias_env =
    let lookup_flag = flip StringMap.find var_env.penv in
      function
        | `Absent -> `Absent
        | `Present t -> `Present (datatype var_env alias_env t)
        | `Var (name, _, _) ->
            begin
              try `Var (lookup_flag name)
              with NotFound _ -> raise (UnexpectedFreeVar name)
            end
  and row var_env alias_env (fields, rv) =
    let lookup_row = flip StringMap.find var_env.renv in
    let seed =
      match rv with
        | `Closed -> Types.make_empty_closed_row ()
        | `Open (rv, _, _) ->
            begin
              try (StringMap.empty, lookup_row rv, false)
              with NotFound _ -> raise (UnexpectedFreeVar rv)
            end
        | `Recursive (name, r) ->
            let var = Types.fresh_raw_variable () in
            let point = Unionfind.fresh (`Var (var, default_subkind, `Flexible)) in
            let renv = StringMap.add name point var_env.renv in
            let _ = Unionfind.change point (`Recursive (var, row {var_env with renv=renv} alias_env r)) in
              (StringMap.empty, point, false) in
    let fields =
        List.map
          (fun (k, p) ->
            (k, fieldspec var_env alias_env p))
          fields 
    in
      fold_right Types.row_with fields seed
  and type_arg var_env alias_env =
    function
      | `Type t -> `Type (datatype var_env alias_env t)
      | `Row r -> `Row (row var_env alias_env r)
      | `Presence f -> `Presence (fieldspec var_env alias_env f)
            
  (* pre condition: all subkinds have been filled in *)
  let generate_var_mapping (vars : type_variable list) : (Types.quantifier list * var_env) =
    let addt x t envs = {envs with tenv = StringMap.add x t envs.tenv} in
    let addr x r envs = {envs with renv = StringMap.add x r envs.renv} in
    let addf x f envs = {envs with penv = StringMap.add x f envs.penv} in
    let vars, var_env =
      List.fold_left
        (fun (vars, envs) ->
           let var = Types.fresh_raw_variable () in
             fun (x, kind, freedom) ->
             match (kind, freedom) with
             | (`Type, Some subkind), freedom ->
               let t = Unionfind.fresh (`Var (var, subkind, freedom)) in
                 (var, subkind, `Type t)::vars, addt x t envs
             | (`Row, Some subkind), freedom ->
               let r = Unionfind.fresh (`Var (var, subkind, freedom)) in
                 (var, subkind, `Row r)::vars, addr x r envs
             | (`Presence, Some subkind), freedom ->
               let f = Unionfind.fresh (`Var (var, subkind, freedom)) in
                 (var, subkind, `Presence f)::vars, addf x f envs)
        ([], empty_env)
        vars
    in
      List.rev vars, var_env

  let datatype' map alias_env (dt, _ : datatype') =
    (dt, Some (datatype map alias_env dt))

  (* Desugar a typename declaration.  Free variables are not allowed
     here (except for the parameters, of course). *)
  let typename alias_env name args (rhs : Sugartypes.datatype') =
      try
        let empty_envs =
          {tenv=StringMap.empty; renv=StringMap.empty; penv=StringMap.empty} in
        let args, envs =
          ListLabels.fold_right ~init:([], empty_envs) args
            ~f:(fun (q, _) (args, {tenv=tenv; renv=renv; penv=penv}) ->
                  let var = Types.fresh_raw_variable () in
                    match q with
                      | (name, (`Type, subkind), _freedom) ->
                          let subkind = concrete_subkind subkind in
                          let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
                            ((q, Some (var, subkind, `Type point))::args,
                             {tenv=StringMap.add name point tenv; renv=renv; penv=penv})
                      | (name, (`Row, subkind), _freedom) ->
                          let subkind = concrete_subkind subkind in
                          let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
                            ((q, Some (var, subkind, `Row point))::args,
                             {tenv=tenv; renv=StringMap.add name point renv; penv=penv})
                      | (name, (`Presence, subkind), _freedom) ->
                          let subkind = concrete_subkind subkind in
                          let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
                            ((q, Some (var, subkind, `Presence point))::args,
                             {tenv=tenv; renv=renv; penv=StringMap.add name point penv})) in
          (args, datatype' envs alias_env rhs)
      with
        | UnexpectedFreeVar x ->
            failwith ("Free variable ("^ x ^") in definition of typename "^ name)

  (* Desugar a foreign function declaration.  Foreign declarations
     cannot use type variables from the context.  Any type variables
     found are implicitly universally quantified at this point. *)
  let foreign alias_env dt =
    let tvars = (typevars#datatype' dt)#tyvar_list in
      datatype' (snd (generate_var_mapping tvars)) alias_env dt
        

 (* Desugar a table literal.  No free variables are allowed here.
    We generate both read and write types by looking for readonly constraints *)
  let tableLit alias_env constraints dt =
    try
      let (_, Some read_type) = datatype' empty_env alias_env (dt, None) in
      let write_row, needed_row =
        match TypeUtils.concrete_type read_type with
        | `Record (fields, _, _) ->
           StringMap.fold
             (fun label t (write, needed) ->
              match lookup label constraints with
              | Some cs ->
                 if List.exists ((=) `Readonly) cs then
                   (write, needed)
                 else (* if List.exists ((=) `Default) cs then *)
                   (Types.row_with (label, t) write, needed)
              | _  ->
                 let add = Types.row_with (label, t) in
                 (add write, add needed))
             fields
             (Types.make_empty_closed_row (), Types.make_empty_closed_row ())
        | _ -> failwith "Table types must be record types"
      in
      (* We deliberately don't concretise the returned read_type in
          the hope of improving error messages during type
          inference. *)
      read_type, `Record write_row, `Record needed_row
    with UnexpectedFreeVar x ->
      failwith ("Free variable ("^ x ^") in table literal")
end


(* convert a syntactic type into a semantic type, using `map' to resolve free type variables *)
let desugar initial_alias_env map =
object (self)
  inherit SugarTraversals.fold_map as super

  val alias_env = initial_alias_env

  method patternnode = function
    | `HasType (pat, dt) ->
        let o, pat = self#pattern pat in
          o, `HasType (pat, Desugar.datatype' map alias_env dt)
    | p -> super#patternnode p


  method phrasenode = function
    | `Block (bs, p) ->
        (* aliases bound in `bs'
           should not escape the scope of the block *)
        let o       = {<>} in
        let o, bs  = o#list (fun o -> o#binding) bs in
        let _o, p  = o#phrase p in
          (* NB: we return `self' rather than `_o' in order to return
             to the outer scope; any aliases bound in _o are
             unreachable from outside the block *)
          self, `Block (bs, p)
    | `TypeAnnotation (p, dt) ->
        let o, p = self#phrase p in
          o, `TypeAnnotation (p, Desugar.datatype' map self#aliases dt)
    | `Upcast (p, dt1, dt2) ->
        let o, p = self#phrase p in
          o, `Upcast (p, Desugar.datatype' map alias_env dt1, Desugar.datatype' map alias_env dt2)
    | `TableLit (t, (dt, _), cs, keys, p) -> 
        let read, write, needed = Desugar.tableLit alias_env cs dt in
        let o, t = self#phrase t in
	let o, keys = o#phrase keys in
        let o, p = o#phrase p in
          o, `TableLit (t, (dt, Some (read, write, needed)), cs, keys, p)
    (* Switch and receive type annotations are never filled in by
       this point, so we ignore them.  *)
    | p -> super#phrasenode p

  method bindingnode = function
    | `Type (t, args, dt) ->
        let args, dt' = Desugar.typename alias_env t args dt in
        let (name, vars, (t, Some dt)) = (t, args, dt') in
          (* NB: type aliases are scoped; we allow shadowing.
             We also allow type aliases to shadow abstract types. *)
          ({< alias_env = SEnv.bind alias_env (name, `Alias (List.map (snd ->- val_of) vars, dt)) >},
           `Type (name, vars, (t, Some dt)))

    | `Val (tyvars, pat, p, loc, dt) ->
        let o, pat = self#pattern pat in
        let o, p   = o#phrase p in
        let o, loc = o#location loc in
          o, `Val (tyvars, pat, p, loc, opt_map (Desugar.datatype' map alias_env) dt)
    | `Fun (bind, lin, (tyvars, fl), loc, dt) ->
        let o, bind = self#binder bind in
        let o, fl   = o#funlit fl in
        let o, loc  = o#location loc in
          o, `Fun (bind, lin, (tyvars, fl), loc, opt_map (Desugar.datatype' map alias_env) dt)
    | `Funs binds ->
        let o, binds =
          super#list
            (fun o (bind, lin, (tyvars, fl), loc, dt, pos) ->
               let o, bind = o#binder bind in
               let o, fl   = o#funlit fl in
               let o, loc  = o#location loc in
               let    dt   = opt_map (Desugar.datatype' map alias_env) dt in
               let o, pos  = o#position pos
               in (o, (bind, lin, (tyvars, fl), loc, dt, pos)))
            binds
        in o, `Funs binds
    | `Foreign (bind, lang, dt) ->
        let o, bind = self#binder bind in
        let dt' = Desugar.foreign alias_env dt in
          self, `Foreign (bind, lang, dt')
    | b -> super#bindingnode b

  method sentence =
    (* return any aliases bound to the interactive loop so that they
       are available to future input.  The default definition will
       do fine here *)
    super#sentence

  method program (bindings, e) =
    (* as with a block, bindings should not escape here *)
    let o           = {<>} in
    let o, bindings = o#list (fun o -> o#binding) bindings in
    let _o, e       = o#option (fun o -> o#phrase) e in
      self, (bindings, e)

  method aliases = alias_env
end

let phrase alias_env p =
  let tvars = (typevars#phrase p)#tyvar_list in
    (desugar alias_env (snd (Desugar.generate_var_mapping tvars)))#phrase p

let binding alias_env b =
  let tvars = (typevars#binding b)#tyvar_list in
    (desugar alias_env (snd (Desugar.generate_var_mapping tvars)))#binding b

let toplevel_bindings alias_env bs =
  let alias_env, bnds =
    List.fold_left
      (fun (alias_env, bnds) bnd ->
         let o, bnd = binding alias_env bnd in
           (o#aliases, bnd::bnds))
    (alias_env, [])
      bs
  in alias_env, List.rev bnds

let program alias_env (bindings, p : Sugartypes.program) : Sugartypes.program =
  let alias_env, bindings = toplevel_bindings alias_env bindings in
    (bindings, opt_map (phrase alias_env ->- snd) p)

let sentence typing_env = function
  | `Definitions bs ->
      let alias_env, bs' = toplevel_bindings typing_env.tycon_env bs in
        {typing_env with tycon_env = alias_env}, `Definitions bs'
  | `Expression  p  -> let o, p = phrase typing_env.tycon_env p in
      {typing_env with tycon_env = o#aliases}, `Expression p
  | `Directive   d  -> typing_env, `Directive d

let read ~aliases s =
  let dt, _ = Parse.parse_string ~in_context:(Parse.fresh_context ()) Parse.datatype s in
  let vars, var_env = Desugar.generate_var_mapping (typevars#datatype dt)#tyvar_list in
  let () = List.iter Generalise.rigidify_quantifier vars in
    (Types.for_all (vars, Desugar.datatype var_env aliases dt))
