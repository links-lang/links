open Types
open Sugartypes
open Utility
open List

let rec typevars : datatype -> quantifier list = 
  let rec rvars (fields, rv) =
    let rowvars = match rv with
      | `Closed   -> []
      | `OpenRigid s -> [`RigidRowVar s]
      | `Open s -> [`RowVar s]
      | `Recursive (s, r) ->
          snd (partition ((=)(`RigidRowVar s)) (rvars r))
    in
      (Utility.concat_map 
         (function (_, `Present k) -> typevars k
            | _ -> [])
         fields) @ rowvars
  in function
    | TypeVar s -> [`TypeVar s]
    | RigidTypeVar s -> [`RigidTypeVar s]
    | FunctionType (s, m, t) -> Utility.unduplicate (=) (concat_map typevars s @ typevars m @ typevars t)
    | MuType (v, k) -> snd (partition ((=)(`RigidTypeVar v)) (typevars k))
    | TupleType ks -> Utility.concat_map typevars ks
    | RecordType r
    | VariantType r -> rvars r
    | TableType (r, w) -> typevars r @ typevars w
    | ListType k -> typevars k
    | TypeApplication (_,ks) -> Utility.concat_map typevars ks
    | UnitType
    | PrimitiveType _
    | DBType -> []

let generalize (k : datatype) : assumption =
  typevars k, k

let desugar_datatype', desugar_row =
  let rec desugar ((tenv, renv) as var_env) =
    let lookup_type = flip StringMap.find tenv in
      function
        | TypeVar s -> (try `MetaTypeVar (lookup_type s)
                        with NotFound _ -> failwith ("Not found `"^ s ^ "' while desugaring assumption"))
        | RigidTypeVar s -> (try `MetaTypeVar (lookup_type s)
                             with NotFound _ -> failwith ("Not found `"^ s ^ "' while desugaring assumption"))
        | FunctionType (f, m, t) ->
            `Function (Types.make_tuple_type (List.map (desugar var_env) f), 
                       desugar var_env m, 
                       desugar var_env t)
        | MuType (name, t) ->
            let var = Types.fresh_raw_variable () in
            let point = Unionfind.fresh (`Flexible var) in
            let tenv = StringMap.add name point tenv in
            let _ = Unionfind.change point (`Recursive (var, desugar (tenv, renv) t)) in
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
  and desugar_row ((tenv, renv) as var_env) (fields, rv) =
    let lookup_row = flip StringMap.find renv in
    let seed = match rv with
      | `Closed    -> Types.make_empty_closed_row ()
      | `OpenRigid rv
      | `Open rv ->
          (StringMap.empty, lookup_row rv)
      | `Recursive (name, r) ->
          let var = Types.fresh_raw_variable () in
          let point = Unionfind.fresh (`Flexible var) in
          let renv = StringMap.add name point renv in
          let _ = Unionfind.change point (`Recursive (var, desugar_row (tenv, renv) r)) in
            (StringMap.empty, point)
    and fields = map (fun (k, v) -> match v with
                        | `Absent -> (k, `Absent)
                        | `Present v -> (k, `Present (desugar var_env v))) fields 
    in fold_right Types.row_with fields seed
  in desugar, desugar_row

type var_env =
    Types.meta_type_var StringMap.t *
      Types.meta_row_var StringMap.t 

let generate_var_mapping : quantifier list -> (Types.quantifier list * var_env) =
  fun vars ->
    let qs, env =
      List.fold_left
        (fun (vars, (tenv, renv)) v ->
           let var = Types.fresh_raw_variable () in
             match v with
               | `TypeVar name ->
                   (`TypeVar var::vars,
                    (StringMap.add name
                       (Unionfind.fresh (`Flexible var)) tenv, renv))
               | `RigidTypeVar name ->
                   (`RigidTypeVar var::vars,
                    (StringMap.add name
                       (Unionfind.fresh (`Rigid var)) tenv, renv))
               | `RowVar name ->
                   (`RowVar var::vars,
                    (tenv, StringMap.add name
                       (Unionfind.fresh (`Flexible var)) renv))
               | `RigidRowVar name ->
                   (`RigidRowVar var::vars,
                    (tenv, StringMap.add name
                       (Unionfind.fresh (`Rigid var)) renv)))
        ([], (StringMap.empty, StringMap.empty))
        vars
    in
      List.rev qs, env

let desugar_assumption ((vars, k)  : assumption) : Types.datatype = 
  let vars, var_env = generate_var_mapping vars in
    Types.for_all (vars, desugar_datatype' var_env k)

let desugar_datatype = generalize ->- desugar_assumption

let make_write_row (fields, rest : row) (constraints : (string * fieldconstraint list) list) : row  =
  let rec mr =
    function
      | [] -> []
      | ((name, body) :: fields) ->
          if List.mem_assoc name constraints
            && List.exists (function
                              | `Readonly -> true) (List.assoc name constraints) then
              mr fields
          else
            (name, body) :: mr fields
  in (mr fields, rest)


let rec get_type_vars : binding -> quantifier list =
  let empty = [] in
  let union = (unduplicate (=)) -<- List.concat in
  let rec get_type_vars (s,_: binding) : quantifier list list =
    let tv datatype = [typevars datatype] in
    let rec phrases v = flatten (List.map phrase v) 
    and opt_phrase = function
      | None -> empty
      | Some e -> phrase e
    and ptv = get_pattern_type_vars
    and btv (p, e) = flatten [ptv p; phrase e]
    and btvs b = flatten (List.map btv b)
    and gtv = function
      | `List b
      | `Table b -> btv b
    and ftv (_, e) = phrase e
    and ftvs f = flatten (List.map ftv f)
    and funlit (p, b : funlit) = (flatten (concat_map (List.map ptv) p) @ phrase b)
    and block (bs, p : binding list * phrase) = flatten (List.map (fst ->- binding) bs) @ phrase p
    and binding (b) = match b with
      | `Fun (_, f, _, k) -> 
          flatten [funlit f; opt_app tv [] k]
      | `Val (p, e, _, k) -> flatten [ptv p; phrase e; opt_app tv [] k]
      | `Foreign (_, _, datatype) -> tv datatype
      | `Include _ -> []
      | `Type (_, args, datatype) -> [List.map (fun k -> `RigidTypeVar k) args] @ tv datatype
      | `Funs fs -> 
          concat_map
            (fun (_, f, _, k) -> flatten [funlit f; opt_app tv [] k])
            fs
      | `Exp e -> phrase e
      | `Infix -> []
    and phrase (p, _) = match p with
      | `Constant _
      | `InfixDecl
      | `Var _ -> empty
      | `FunLit f -> funlit f
      | `Spawn e -> phrase e
      | `SpawnWait e -> phrase e
      | `ListLit es -> phrases es
      | `Iteration (generators, body, filter, sort) ->
          flatten [concat_map gtv generators; phrase body; opt_phrase filter; opt_phrase sort]
      | `Escape (_, e) ->  phrase e
      | `Section _ -> empty
      | `Conditional (e1, e2, e3) -> flatten [phrase e1; phrase e2; phrase e3]
      | `Block b -> block b
      | `InfixAppl (_, e1, e2) -> flatten [phrase e1; phrase e2]
      | `Regex _ -> empty
      | `UnaryAppl (_, e) -> phrase e
      | `FnAppl (fn, ps) -> flatten [phrase fn; phrases ps]
      | `TupleLit fields -> phrases fields
      | `RecordLit (fields, e) ->
          flatten ((List.map (fun (_, field) -> phrase field) fields) @ [opt_phrase e])
      | `With (e, fields) -> 
          flatten ((List.map (fun (_, field) -> phrase field) fields) @ [phrase e])
      | `Projection (e, _) -> phrase e
      | `TypeAnnotation(e, k) -> flatten [phrase e; tv k]
      | `Upcast(e, t1, t2) -> flatten [phrase e; tv t1; tv t2]
      | `ConstructorLit (_, e) -> opt_phrase e
      | `Switch (exp, binders, _) -> flatten [phrase exp; btvs binders]
      | `Receive (binders, _) -> btvs binders

      | `DatabaseLit (name, (opt_driver, opt_args)) -> flatten [phrase name; opt_phrase opt_driver; opt_phrase opt_args]
      | `TableLit (_, datatype, _, db) -> flatten [tv datatype; phrase db]
      | `DBInsert (e1, e2) -> flatten [phrase e1; phrase e2]
      | `DBDelete (p, e1, e2) -> flatten [ptv p; phrase e1; opt_phrase e2]
      | `DBUpdate (p, e1, e2, fs) -> flatten [ptv p; phrase e1; opt_phrase e2; ftvs fs]

      | `Xml (_, attrs, attrexp, subnodes) ->
          flatten ((List.map (fun (_, es) -> phrases es) attrs) @ [opt_phrase attrexp] @ [phrases subnodes])
      | `TextNode _ -> empty
      | `FormletPlacement (e1, e2, e3) -> flatten [phrase e1; phrase e2; phrase e3]
      | `Formlet (e1, e2) -> flatten [phrase e1; phrase e2]
      | `PagePlacement e -> phrase e
      | `Page e -> phrase e
      | `FormBinding (e, p) -> flatten [phrase e; ptv p]
    in binding s
  and get_pattern_type_vars (p, _ : pattern) = (* fold *)
    match p with 
      | `Any
      | `Nil
      | `Variable _
      | `Constant _
      | `Negative _
      | `Variant (_, None)   -> []
      | `Variant (_, Some p)
      | `As (_, p)           -> get_pattern_type_vars p
      | `Cons (l, r)         -> get_pattern_type_vars l @ get_pattern_type_vars r
      | `Record (ps, Some p) -> concat_map (snd ->- get_pattern_type_vars) ps @ get_pattern_type_vars p
      | `Record (ps, None)   -> concat_map (snd ->- get_pattern_type_vars) ps
      | `List ps
      | `Tuple ps            -> concat_map get_pattern_type_vars ps 
      | `HasType (p, t)      -> get_pattern_type_vars p @ [typevars t]
  in
    union -<- get_type_vars

                                              
let read_datatype s = 
  let dt, _ = Parse.parse_string ~in_context:(Parse.fresh_context ()) Parse.datatype s in
    desugar_datatype dt

let var_mapping_from_binding = generate_var_mapping -<- get_type_vars
