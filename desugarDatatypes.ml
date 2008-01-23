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

  (* TODO: exclude type vars found in type alias and foreign definitions *)

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

let rec desugar ({tenv=tenv; renv=renv} as var_env) =
  let lookup_type t = StringMap.find t tenv in function
    | TypeVar s -> (try `MetaTypeVar (lookup_type s)
                    with NotFound _ -> raise (UnexpectedFreeVar s))
    | RigidTypeVar s -> (try `MetaTypeVar (lookup_type s)
                         with NotFound _ -> raise (UnexpectedFreeVar s))
    | FunctionType (f, m, t) ->
        `Function (Types.make_tuple_type (List.map (desugar var_env) f), 
                   desugar var_env m, 
                   desugar var_env t)
    | MuType (name, t) ->
        let var = Types.fresh_raw_variable () in
        let point = Unionfind.fresh (`Flexible var) in
        let tenv = StringMap.add name point tenv in
        let _ = Unionfind.change point (`Recursive (var, desugar {tenv=tenv; renv=renv} t)) in
          `MetaTypeVar point
    | UnitType -> Types.unit_type
    | TupleType ks -> 
        let labels = map string_of_int (Utility.fromTo 1 (1 + length ks)) 
        and unit = Types.make_empty_closed_row ()
        and present (s, x) = (s, `Present x)
        in `Record (fold_right2 (curry (Types.row_with -<- present)) labels (map (desugar var_env) ks) unit)
    | RecordType row -> `Record (desugar_row var_env row)
    | VariantType row -> `Variant (desugar_row var_env row)
    | TableType (r, w) -> `Table (desugar var_env r, desugar var_env w)
    | ListType k -> `Application ("List", [desugar var_env k])
    | TypeApplication (t, k) -> `Application (t, List.map (desugar var_env) k)
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
                      | `Present v -> (k, `Present (desugar var_env v))) fields 
  in fold_right Types.row_with fields seed

let generate_var_mapping (vars : quantifier list) : (Types.quantifier list * var_env) =
  let addt x tvar envs = {envs with tenv = StringMap.add x (Unionfind.fresh tvar) envs.tenv}
  and addr x rvar envs = {envs with renv = StringMap.add x (Unionfind.fresh rvar) envs.renv} in
    List.fold_right
      (fun v (vars, envs) ->
         let var = Types.fresh_raw_variable () in
           match v with
             | `TypeVar      x -> `TypeVar var::vars     , addt x (`Flexible var) envs
             | `RigidTypeVar x -> `RigidTypeVar var::vars, addt x (`Rigid var)    envs
             | `RowVar       x -> `RowVar var::vars      , addr x (`Flexible var) envs
             | `RigidRowVar  x -> `RigidRowVar var::vars , addr x (`Rigid var)    envs)
      vars
      ([], empty_env)

let datatype' map (dt, _ : datatype') = 
  (dt, Some (desugar map dt))

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
    (* TODO: mailbox args *)
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
                      | Some cs when List.exists (function `Readonly -> true) cs -> row
                      | _  -> Types.row_with (label, t) row)
                 fields
                 (Types.make_empty_closed_row ()))
        | _ -> failwith "Table types must be record types"
    in read_type, write_type
  with UnexpectedFreeVar x ->
    failwith ("Free variable ("^ x ^") in table literal")

let read s =
  let dt, _ = Parse.parse_string ~in_context:(Parse.fresh_context ()) Parse.datatype s in
  let vars, var_env = generate_var_mapping (typevars#datatype dt)#tyvars in
    Types.for_all (vars, desugar var_env dt)

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


