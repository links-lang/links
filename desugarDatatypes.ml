open Types
open Sugartypes
open Utility
open List

(* Find all 'unbound' type variables in a term *)
let typevars = 
object (self)
  inherit SugarTraversals.fold as super

  val tyvars : quantifier list = []
  val bound  : quantifier list = []

  method tyvars = Utility.unduplicate (=) tyvars

  method add tv = 
    if List.mem tv bound then self
    else {< tyvars = tv :: tyvars >}

  method add_all tvs = 
    let tvs = List.filter (fun tv -> not (List.mem tv bound)) tvs in
      {< tyvars = tvs @ tyvars >}

  method bind tv = {< bound = tv :: bound >}

  method bindingnode = function
    (* type declarations bind variables; exclude those from the
       analysis. *)
    | `Type _    -> self
    | b          -> super#bindingnode b

  method datatype = function
    | TypeVar t         -> self#add (`TypeVar t)
    | RigidTypeVar s    -> self#add (`RigidTypeVar s)
    | MuType (v, k)     -> let o = self#bind (`RigidTypeVar v) in o#datatype k
    | dt                -> super#datatype dt
        
  method row_var = function
    | `Closed           -> self
    | `OpenRigid s      -> self#add (`RigidRowVar s)
    | `Open s           -> self#add (`RowVar s)
    | `Recursive (s, r) -> let o = self#bind (`RigidRowVar s) in o#row r
end

type var_env = { tenv : Types.meta_type_var StringMap.t;
                 renv : Types.meta_row_var StringMap.t }

let empty_env = {tenv = StringMap.empty; renv = StringMap.empty}

exception UnexpectedFreeVar of string

let rec desugar' ({tenv=tenv; renv=renv} as var_env) =
  let lookup_type t = StringMap.find t tenv in function
    | TypeVar s -> (try `MetaTypeVar (lookup_type s)
                    with NotFound _ -> raise (UnexpectedFreeVar s))
    | RigidTypeVar s -> (try `MetaTypeVar (lookup_type s)
                         with NotFound _ -> raise (UnexpectedFreeVar s))
    | FunctionType (f, m, t) ->
        `Function (Types.make_tuple_type (List.map (desugar' var_env) f), 
                   desugar' var_env m, 
                   desugar' var_env t)
    | MuType (name, t) ->
        let var = Types.fresh_raw_variable () in
        let point = Unionfind.fresh (`Flexible var) in
        let tenv = StringMap.add name point tenv in
        let _ = Unionfind.change point (`Recursive (var, desugar' {tenv=tenv; renv=renv} t)) in
          `MetaTypeVar point
    | UnitType -> Types.unit_type
    | TupleType ks -> 
        let labels = map string_of_int (Utility.fromTo 1 (1 + length ks)) 
        and unit = Types.make_empty_closed_row ()
        and present (s, x) = (s, `Present x)
        in `Record (fold_right2 (curry (Types.row_with -<- present)) labels (map (desugar' var_env) ks) unit)
    | RecordType row -> `Record (desugar_row var_env row)
    | VariantType row -> `Variant (desugar_row var_env row)
    | TableType (r, w) -> `Table (desugar' var_env r, desugar' var_env w)
    | ListType k -> `Application ("List", [desugar' var_env k])
    | TypeApplication (t, k) -> `Application (t, List.map (desugar' var_env) k)
    | PrimitiveType k -> `Primitive k
    | DBType -> `Primitive `DB
and desugar_row ({tenv=tenv; renv=renv} as var_env) (fields, rv) =
  let lookup_row = flip StringMap.find renv in
  let seed = match rv with
    | `Closed    -> Types.make_empty_closed_row ()
    | `OpenRigid rv
    | `Open rv -> (StringMap.empty, lookup_row rv)
    | `Recursive (name, r) ->
        let var = Types.fresh_raw_variable () in
        let point = Unionfind.fresh (`Flexible var) in
        let renv = StringMap.add name point renv in
        let _ = Unionfind.change point (`Recursive (var, desugar_row {tenv=tenv; renv=renv} r)) in
          (StringMap.empty, point)
  and fields = map (fun (k, v) -> match v with
                      | `Absent -> (k, `Absent)
                      | `Present v -> (k, `Present (desugar' var_env v))) fields 
  in fold_right Types.row_with fields seed

let generate_var_mapping (vars : quantifier list) : (Types.quantifier list * var_env) =
  let addt x tvar envs = {envs with tenv = StringMap.add x (Unionfind.fresh tvar) envs.tenv}
  and addr x rvar envs = {envs with renv = StringMap.add x (Unionfind.fresh rvar) envs.renv} in
  let vars, var_env =
    List.fold_left
      (fun (vars, envs) v ->
         let var = Types.fresh_raw_variable () in
           match v with
             | `TypeVar      x -> `TypeVar var::vars     , addt x (`Flexible var) envs
             | `RigidTypeVar x -> `RigidTypeVar var::vars, addt x (`Rigid var)    envs
             | `RowVar       x -> `RowVar var::vars      , addr x (`Flexible var) envs
             | `RigidRowVar  x -> `RigidRowVar var::vars , addr x (`Rigid var)    envs)
      ([], empty_env)
      vars
  in
    List.rev vars, var_env

let datatype' map (dt, _ : datatype') = 
  (dt, Some (desugar' map dt))

(* Desugar a typename declaration.  Free variables are not allowed
   here (except for the parameters, of course). *)
let typename name args rhs = 
  let mailbox_vars dt =
    ((object
       inherit SugarTraversals.fold as super
       val tyvars = StringMap.empty

       method tyvars = tyvars

       method datatype = function
         | FunctionType (f, TypeVar x, t) -> 
             let var = `Flexible (Types.fresh_raw_variable()) in
             let self = {< tyvars = StringMap.add x (Unionfind.fresh var) tyvars >} in
             let o = self#list (fun o -> o#datatype) f in
             let o = o#datatype t in
               o
         | dt -> super#datatype dt
     end)#datatype' dt) # tyvars in
  try
    let args, tmap = 
      ListLabels.fold_right ~init:([], StringMap.empty) args
        ~f:(fun (x, _) (args, map) -> 
              let tv = Types.fresh_raw_variable () in
                ((x, Some tv) :: args, 
                 StringMap.add x (Unionfind.fresh (`Rigid tv)) map)) in
      (args, datatype' {tenv=StringMap.union_disjoint tmap (mailbox_vars rhs); renv=StringMap.empty} rhs)
  with UnexpectedFreeVar x ->
    failwith ("Free variable ("^ x ^") in definition of typename "^ name)

(* Desugar a foreign function declaration.  Foreign declarations
   cannot use type variables from the context.  Any type variables
   found are implicitly universally quantified at this point. *)
let foreign dt = 
  let tvars = (typevars#datatype' dt)#tyvars in
    datatype' (snd (generate_var_mapping tvars)) dt

(* Desugar a table literal.  No free variables are allowed here.
   We generate both read and write types by looking for readonly constraints *)
let tableLit constraints dt =
  try
    let (_, Some read_type) = datatype' empty_env (dt, None) in
    let write_type = 
      match read_type with
        | `Record (fields, _) -> 
            `Record
              (StringMap.fold
                 (fun label t row ->
                    match lookup label constraints with 
                      | Some cs when List.exists (function `Readonly | `Identity -> true) cs -> row
                      | _  -> Types.row_with (label, t) row)
                 fields
                 (Types.make_empty_closed_row ()))
        | _ -> failwith "Table types must be record types"
    in read_type, write_type
  with UnexpectedFreeVar x ->
    failwith ("Free variable ("^ x ^") in table literal")

(* convert a syntactic type into a semantic type, using `map' to resolve free type variables *)
let desugar map =
object (self)
  inherit SugarTraversals.map as super

  method patternnode = function
    | `HasType (pat, dt) ->
        `HasType (self#pattern pat, datatype' map dt)
    | p -> super#patternnode p

  method phrasenode = function
    | `TypeAnnotation (p, dt) ->
        `TypeAnnotation (self#phrase p, datatype' map dt)
    | `Upcast (p, dt1, dt2) ->
        `Upcast (self#phrase p, datatype' map dt1, datatype' map dt2)
    | `TableLit (t, (dt, _), cs, p) -> 
        let read, write = tableLit cs dt in
          `TableLit (self#phrase t, (dt, Some (read, write)), cs, self#phrase p)
    | p -> super#phrasenode p

  method bindingnode = function
    | `Val (pat, p, loc, dt) -> 
        `Val (self#pattern pat, self#phrase p, self#location loc, opt_map (datatype' map) dt)
    | `Fun (bind, fl, loc, dt) ->
        `Fun (self#binder bind, self#funlit fl, self#location loc, opt_map (datatype' map) dt)
    | `Funs defs ->
        let defs = (ListLabels.map defs
                      ~f:(fun (bind, fl, loc, dt) -> 
                            (self#binder bind, self#funlit fl, self#location loc,
                             opt_map (datatype' map) dt))) in
          `Funs defs
    | `Foreign (x, lang, dt) ->
        let dt' = foreign dt in
          `Foreign (x, lang, dt')
    | `Type (t, args, dt) ->
        let args, dt' = typename t args dt in
          `Type (t, args, dt')
    | b -> super#bindingnode b
end

let phrase p =
  let tvars = (typevars#phrase p)#tyvars in
    (desugar (snd (generate_var_mapping tvars)))#phrase p

let binding b = 
  let tvars = (typevars#binding b)#tyvars in
    (desugar (snd (generate_var_mapping tvars)))#binding b

let program (bindings, p) = 
  List.map binding bindings, opt_map phrase p
  
let sentence = function
  | `Definitions bs -> `Definitions (List.map binding bs)
  | `Expression  p  -> `Expression (phrase p)
  | `Directive   d  -> `Directive d

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
    | `TableLit (_, (_, None), _, _) -> {< all_desugared = false >}
    | p -> super#phrasenode p
end




open Utility

module SEnv = Env.String

(* This should really be done with a generic traversal function. *)
let rec expand_aliases
    (rec_tys, rec_rows as rec_envs : Types.meta_type_var Env.Int.t * Types.meta_row_var Env.Int.t) 
    env t =
  let rec expand rec_envs = expand_aliases rec_envs env
  and expand_row rec_vars (fsm, rv) = 
    let fsm', rv = 
      match Unionfind.find rv with
        | `Closed
        | `Flexible _
        | `Rigid _    -> fsm, rv
        | `Recursive (var, body) ->
            begin match Env.Int.find rec_rows var with
              | Some t -> fsm, t
              | None   -> 
                  let point = Unionfind.fresh (`Recursive (var, (StringMap.empty,
                                                                 Unionfind.fresh (`Flexible var)))) in
                  let envs  = (rec_tys, Env.Int.bind rec_rows (var, point)) in
                  let body' = expand_row envs body in
                  let ()    = Unionfind.change point (`Recursive (var, body')) in
                    fsm, point
            end
        | `Body t -> expand_row rec_envs t
    in
    let f = function
      | `Present t -> `Present (expand rec_vars t)
      | `Absent    -> `Absent in
      (StringMap.map f fsm', rv)
  in
    match t with
      | `Not_typed
      | `Primitive _ as t -> t
      | `Function (f, m, t) -> `Function (expand rec_envs f, expand rec_envs m, expand rec_envs t)
      | `Record r -> `Record (expand_row rec_envs r)
      | `Variant r -> `Variant (expand_row rec_envs r)
      | `Table (t1, t2) -> `Table (expand rec_envs t1, expand rec_envs t2)
      | `Alias _ -> assert false (* There shouldn't be an alias here yet *)
      | `Application (s, ds) when SEnv.has env s -> 
          let ds = List.map (expand rec_envs) ds in
            Instantiate.alias s ds env
      | `Application (s, ds) -> 
          `Application (s, List.map (expand rec_envs) ds)
      | `ForAll (qs, dt) -> `ForAll (qs, expand rec_envs dt)
      | `MetaTypeVar point -> 
          match Unionfind.find point with
            | `Flexible _ 
            | `Rigid _ -> t
            | `Recursive (var, body) ->
                begin match Env.Int.find rec_tys var with
                  | Some t -> `MetaTypeVar t
                  | None ->
                      let point = Unionfind.fresh (`Recursive (var, `Not_typed))   in
                      let envs  = (Env.Int.bind rec_tys (var, point), rec_rows)    in
                      let body' = expand envs body                                 in
                      let ()    = Unionfind.change point (`Recursive (var, body')) in
                        `MetaTypeVar point
                end
            | `Body t -> expand rec_envs t

let expand_aliases env t = 
  expand_aliases (Env.Int.empty, Env.Int.empty) env t

class type alias_expander =
object
  inherit SugarTraversals.fold_map
  method aliases : Types.alias_environment
end

let expand_aliases (initial_env : Types.alias_environment) =
object (self)
  inherit SugarTraversals.fold_map as super

  val aliases = initial_env

  method phrasenode = function
    | `Block (bs, p) -> 
        (* aliases bound in `bs'
           should not escape the scope of the block *)
        let o = {<>} in
        let o, bs = o#list (fun o -> o#binding) bs in
        let o, p  = o#phrase p in 
          self, `Block (bs, p)
    | `TableLit (p1, (t, Some (t1, t2)), fcs, p2) ->
        let o, p1 = self#phrase p1 in
        let o, t  = o#datatype t in
        let t1    = expand_aliases aliases t1
        and t2    = expand_aliases aliases t2 in
        let o, p2 = o#phrase p2 in
          o, `TableLit (p1, (t, Some (t1, t2)), fcs, p2)

    (* Switch and receive type annotations are never filled in by
       this point, so we ignore them.  *)
    | p              -> super#phrasenode p

  method sentence = 
    (* return any aliases bound to the interactive loop so that they
       are available to future input.  The default definition will
       do fine here *)
    super#sentence

  method program (bindings, e) = 
    (* as with a block, bindings should not escape here *)
    let o = {<>} in
    let o, bindings = o#list (fun o -> o#binding) bindings in
    let o, e = o#option (fun o -> o#phrase) e in
      o, (bindings, e)

  method bindingnode = function
    | `Type (name, vars, (t, Some dt)) -> 
        let dt = expand_aliases aliases dt in 
          (* NB: type aliases are scoped; we allow shadowing.
             We also allow type aliases to shadow abstract types. *)
          {< aliases = SEnv.bind aliases (name, (List.map (snd ->- val_of) vars, dt)) >},
          `Type (name, vars, (t, Some dt))
    | bn -> super#bindingnode bn

  method datatype' = function
    | (t, Some dt) -> self, (t, Some (expand_aliases aliases dt))
    | _ ->
        failwith "Internal error: unexpanded datatype encountered during alias expansion"

  method aliases = aliases
end

let expand aliases dt =
  let _, (_, Some dt) = (expand_aliases aliases)#datatype' (Sugartypes.UnitType, Some dt) in
    dt

let read ?(aliases=Env.String.empty) s =
  let dt, _ = Parse.parse_string ~in_context:(Parse.fresh_context ()) Parse.datatype s in
  let vars, var_env = generate_var_mapping (typevars#datatype dt)#tyvars in
    expand aliases (Types.for_all (vars, desugar' var_env dt))


let sentence (env : Types.typing_environment) sent =
  let sentence = sentence sent in
  let s, sentence = (expand_aliases env.Types.tycon_env)#sentence sentence in
    {env with Types.tycon_env = s#aliases}, sentence

let program tyenv = program ->- (expand_aliases tyenv)#program ->- snd
