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
    | `TableLit (_, (_, None), _, _) -> {< all_desugared = false >}
    | p -> super#phrasenode p
end

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

module Desugar =
struct
  let rec datatype ({tenv=tenv; renv=renv} as var_env) (alias_env : Types.tycon_environment) t =
  let datatype var_env t = datatype var_env alias_env t in
    let lookup_type t = StringMap.find t tenv in 
      match t with
      | TypeVar s -> (try `MetaTypeVar (lookup_type s)
                      with NotFound _ -> raise (UnexpectedFreeVar s))
      | RigidTypeVar s -> (try `MetaTypeVar (lookup_type s)
                           with NotFound _ -> raise (UnexpectedFreeVar s))
      | FunctionType (f, m, t) ->
          `Function (Types.make_tuple_type (List.map (datatype var_env) f), 
                     datatype var_env m, 
                     datatype var_env t)
      | MuType (name, t) ->
          let var = Types.fresh_raw_variable () in
          let point = Unionfind.fresh (`Flexible var) in
          let tenv = StringMap.add name point tenv in
          let _ = Unionfind.change point (`Recursive (var, datatype {tenv=tenv; renv=renv} t)) in
            `MetaTypeVar point
      | UnitType -> Types.unit_type
      | TupleType ks -> 
          let labels = map string_of_int (Utility.fromTo 1 (1 + length ks)) 
          and unit = Types.make_empty_closed_row ()
          and present (s, x) = (s, `Present x)
          in `Record (fold_right2 (curry (Types.row_with -<- present)) labels (map (datatype var_env) ks) unit)
      | RecordType r -> `Record (row var_env alias_env r)
      | VariantType r -> `Variant (row var_env alias_env r)
      | TableType (r, w) -> `Table (datatype var_env r, datatype var_env w)
      | ListType k -> `Application (Types.list, [datatype var_env k])
      | TypeApplication (tycon, ts) ->
          begin match SEnv.find alias_env tycon with
            | None -> failwith (Printf.sprintf "Unbound type constructor %s" tycon)
            | Some (`Alias _) -> let ts = List.map (datatype var_env) ts in
                                   Instantiate.alias tycon ts alias_env
            | Some (`Abstract abstype) -> 
                `Application (abstype, List.map (datatype var_env) ts)
          end
      | PrimitiveType k -> `Primitive k
      | DBType -> `Primitive `DB
  and row ({tenv=tenv; renv=renv} as var_env) alias_env (fields, rv) =
    let lookup_row = flip StringMap.find renv in
    let seed = match rv with
      | `Closed    -> Types.make_empty_closed_row ()
      | `OpenRigid rv
      | `Open rv -> (StringMap.empty, lookup_row rv)
      | `Recursive (name, r) ->
          let var = Types.fresh_raw_variable () in
          let point = Unionfind.fresh (`Flexible var) in
        let renv = StringMap.add name point renv in
        let _ = Unionfind.change point (`Recursive (var, row {tenv=tenv; renv=renv} alias_env r)) in
          (StringMap.empty, point)
    and fields = map (fun (k, v) -> match v with
                        | `Absent -> (k, `Absent)
                        | `Present v -> (k, `Present (datatype var_env alias_env v))) fields 
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
        
  let datatype' map alias_env (dt, _ : datatype') = 
    (dt, Some (datatype map alias_env dt))

  (* Desugar a typename declaration.  Free variables are not allowed
     here (except for the parameters, of course). *)
  let typename alias_env name args (rhs : Sugartypes.datatype') = 
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
          (args, datatype' {tenv=StringMap.union_disjoint tmap (mailbox_vars rhs); renv=StringMap.empty} alias_env rhs)
      with UnexpectedFreeVar x ->
        failwith ("Free variable ("^ x ^") in definition of typename "^ name)

  (* Desugar a foreign function declaration.  Foreign declarations
     cannot use type variables from the context.  Any type variables
     found are implicitly universally quantified at this point. *)
  let foreign alias_env dt = 
    let tvars = (typevars#datatype' dt)#tyvars in
      datatype' (snd (generate_var_mapping tvars)) alias_env dt
        
  (* Desugar a table literal.  No free variables are allowed here.
     We generate both read and write types by looking for readonly constraints *)
  let tableLit alias_env constraints dt =
    try
      let (_, Some read_type) = datatype' empty_env alias_env (dt, None) in
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
    | `TableLit (t, (dt, _), cs, p) -> 
        let read, write = Desugar.tableLit alias_env cs dt in
        let o, t = self#phrase t in
        let o, p = o#phrase p in
          o, `TableLit (t, (dt, Some (read, write)), cs, p)
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
        let o, pat = self#typattern pat in
        let o, p   = o#phrase p in
        let o, loc = o#location loc in
          o, `Val (tyvars, pat, p, loc, opt_map (Desugar.datatype' map alias_env) dt)
    | `Fun (bind, fl, loc, dt) ->
        let o, bind = self#tybinder bind in
        let o, fl   = o#funlit fl in
        let o, loc  = o#location loc in
          o, `Fun (bind, fl, loc, opt_map (Desugar.datatype' map alias_env) dt)
    | `Funs binds ->
        let o, binds =
          super#list
            (fun o (bind, fl, loc, dt) ->
               let o, bind = o#tybinder bind in
               let o, fl   = o#funlit fl in
               let o, loc  = o#location loc in
               let    dt   = opt_map (Desugar.datatype' map alias_env) dt
               in (o, (bind, fl, loc, dt)))
            binds
        in o, `Funs binds
    | `Foreign (bind, lang, dt) ->
        let o, bind = self#tybinder bind in
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
  let tvars = (typevars#phrase p)#tyvars in
    (desugar alias_env (snd (Desugar.generate_var_mapping tvars)))#phrase p

let binding alias_env b = 
  let tvars = (typevars#binding b)#tyvars in
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
  let vars, var_env = Desugar.generate_var_mapping (typevars#datatype dt)#tyvars in
    (Types.for_all (vars, Desugar.datatype var_env aliases dt))
