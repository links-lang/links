

open Utility
open Sugartypes

module rec Typed
  : Phrase with module P = TypedArgs
  = Typed
and TypedArgs : sig
  type phrase   = Typed.phrasenode * (pposition * Types.datatype)
  type ppattern = Typed.pattern * (pposition * (Types.environment * Types.datatype))
  type toplevel = Typed.toplevel' * (pposition * Types.datatype)
end
  = TypedArgs


module Utils : sig
  val unify : Types.alias_environment -> Types.datatype -> Types.datatype -> unit
  val unify_rows : Types.alias_environment -> Types.row -> Types.row -> unit
  val instantiate : Types.environment -> string -> Types.datatype
end =
struct
  let unify _ = failwith ""
  let unify_rows _ = failwith ""
  let instantiate _ = failwith ""
end

let mailbox = "_MAILBOX_"
let mailbox_type env = Utils.instantiate env mailbox

let constant_type = function
  | `Float _  -> `Primitive `Float
  | `Int _    -> `Primitive `Int
  | `Bool _   -> `Primitive `Bool
  | `Char _   -> `Primitive `Char
  | `String _ ->  Types.string_type

let type_section env (`Section s as s') = s', match s with
    `Minus         -> Utils.instantiate env "-"
  | `FloatMinus    -> Utils.instantiate env "-."
  | `Project label ->
      let f = Types.fresh_type_variable () in
      let r = `Record (Types.make_singleton_open_row (label, `Present f)) in
        `Function (r, mailbox_type env, f)
  | `Name var      -> Utils.instantiate env var

let type_pattern lookup_pos alias_env : Untyped.ppattern -> Typed.ppattern =
  let rec type_pattern  (pattern, pos) : Typed.ppattern =
    let unify = Utils.unify alias_env
    and unify_rows = Utils.unify_rows alias_env 
    and typ (_,(_,(_,t))) = t
    and env (_,(_,(e,_))) = e 
    and (++) = Types.concat_environments in
    let (p, e, t : Typed.pattern * Types.environment * Types.datatype) =
      match (pattern : Untyped.pattern) with
        | `Any                   -> `Any,
            Types.empty_environment, Types.fresh_type_variable ()
        | `Nil                   -> `Nil,
            Types.empty_environment, (Types.make_list_type
                                        (Types.fresh_type_variable ()))
        | `Constant c as c'      -> c', Types.empty_environment, constant_type c
        | `Variable x            -> 
            let xtype = Types.fresh_type_variable () in
              (`Variable x,
               (Types.bind x ([], xtype) Types.empty_environment),
               xtype)
        | `Cons (p1, p2)         -> 
            let p1 = type_pattern p1
            and p2 = type_pattern p2 in
            let _ = unify (Types.make_list_type (typ p1)) (typ p2) in
              `Cons (p1, p2), env p1 ++ env p2, typ p2
        | `List ps               -> 
            let ps' = List.map type_pattern ps in
            let env' = List.fold_right (env ->- (++)) ps' Types.empty_environment in
            let element_type = 
              match ps' with
                | [] -> Types.fresh_type_variable ()
                | p::ps -> 
                    let _ = List.iter (typ ->- unify (typ p)) ps in
                      typ p
            in `List ps', env', Types.make_list_type element_type
        | `Variant (name, None)       -> 
            let vtype = `Variant (Types.make_singleton_open_row (name, `Present Types.unit_type)) in
              `Variant (name, None), Types.empty_environment, vtype
        | `Variant (name, Some p)     -> 
            let p = type_pattern p in
            let vtype = `Variant (Types.make_singleton_open_row (name, `Present (typ p))) in
              `Variant (name, Some p), env p, vtype
        | `Record (ps, default)  -> 
            let ps = alistmap type_pattern ps
            and default = opt_map type_pattern default in
            let initial, denv = match default with
              | None -> (Types.make_empty_closed_row (),
                         Types.empty_environment)
              | Some r -> 
                  let row = Types.make_empty_open_row () in
                  let _ = unify (`Record row) (typ r) in
                    row, env r in
            let rtype = 
              `Record (List.fold_right
                         (fun (l, f) -> Types.row_with (l, `Present (typ f)))
                         ps initial)
            and penv = 
              List.fold_right (snd ->- env ->- (++)) ps Types.empty_environment in
              `Record (ps, default), penv ++ denv, rtype
        | `Tuple ps              -> 
            let ps' = List.map type_pattern ps in
            let env' = List.fold_right (env ->- (++)) ps' Types.empty_environment in
            let typ' = Types.make_tuple_type (List.map typ ps') in
              `Tuple ps', env', typ'
        | `As (x, p)             -> 
            let p = type_pattern p in
            let env' = Types.bind x ([], typ p) (env p) in
              `As (x, p), env', (typ p)
        | `HasType (p, t)        -> 
            let p = type_pattern p in
            let _ = unify (typ p) (snd (Sugar.desugar_datatype t)) in
              `HasType (p, t), env p, typ p
    in
      p, (pos, (e,t))
  in
    type_pattern

let type_check lookup_pos = 
  let rec type_check ((env, alias_env) as typing_env) (expr, pos) =
    let unify = Utils.unify alias_env
    and unify_rows = Utils.unify_rows alias_env 
    and typ (_,(_,t)) = t 
    and pattern_typ (_, (_,(_,t))) = t
    and type_pattern = type_pattern lookup_pos in
    let e, t =
      match (expr : Untyped.phrasenode) with
        | `Var v            -> `Var v, Utils.instantiate env v
        | `Section _ as s   -> type_section env s


        (* literals *)
        | `Constant c as c' -> c', constant_type c
        | `TupleLit ps ->
            let ps = List.map (type_check typing_env) ps in
              `TupleLit ps, Types.make_tuple_type (List.map typ ps)
        | `RecordLit _ ->       assert false
        | `ListLit es ->
            begin match List.map (type_check typing_env) es with
              | [] -> `ListLit [], `Application ("List", [Types.fresh_type_variable ()])
              | e :: es -> 
                  List.iter (typ ->- unify (typ e)) es;
                  `ListLit es, `Application ("List", [typ e])
            end
        | `FunLit _ ->          assert false
        | `ConstructorLit (c, None) ->
            let type' = `Variant (Types.make_singleton_open_row 
                                    (c, `Present Types.unit_type)) in
              `ConstructorLit (c, None), type'
        | `ConstructorLit (c, Some v) ->
            let v = type_check typing_env v in
            let type' = `Variant (Types.make_singleton_open_row
                                    (c, `Present (typ v))) in
              `ConstructorLit (c, Some v), type'

        (* database *)
        | `DatabaseLit (name, (driver, args)) ->
            let driver = opt_map (type_check typing_env) driver
            and args   = opt_map (type_check typing_env) args
            and name   = type_check typing_env name in
              `DatabaseLit (name, (driver, args)), `Primitive `DB

        | `TableLit (tname, dtype, constraints, db) ->
            let tname = type_check typing_env tname 
            and db = type_check typing_env db in
            let _ = unify (typ tname) Types.string_type
            and _ = unify (typ db) Types.database_type in
            let read_row = match dtype with
              | RecordType row ->
                  row
              | UnitType ->
                  raise (Syntax.ASTSyntaxError(lookup_pos pos, "Tables must have at least one field"))
              | _ ->
                  raise (Syntax.ASTSyntaxError(lookup_pos pos, "Tables must take a non-empty record type")) in
            let write_row = Sugar.make_write_row read_row constraints  in
              `TableLit (tname, dtype, constraints, db), 
              `Table (snd (Sugar.desugar_datatype (RecordType read_row)),
                      snd (Sugar.desugar_datatype (RecordType write_row)))

        | `DBDelete (generator,None) ->
            assert false
        | `DBDelete (generator,Some where) ->
            assert false
        | `DBInsert _ ->        assert false
        | `DBUpdate _ ->        assert false

        (* concurrency *)
        | `Spawn p ->
            (* (() -{b}-> d) -> Mailbox (b) *)
            assert false
        | `Receive _ -> 
            assert false

        (* applications of various sorts *)
        | `UnaryAppl (op, p) -> 
            let op = assert false
            and p = type_check typing_env p
            and rettyp = Types.fresh_type_variable () in
              unify (typ op) (`Function (Types.make_tuple_type [typ p], 
                                         mailbox_type env, rettyp));
              `UnaryAppl (assert false, p), rettyp
        | `InfixAppl (op, l, r) ->
            let op = assert false
            and l = type_check typing_env l
            and r = type_check typing_env r 
            and rettyp = Types.fresh_type_variable () in
              unify (typ op) (`Function (Types.make_tuple_type [typ l; typ r], 
                                         mailbox_type env, rettyp));
              `InfixAppl (assert false, l, r), rettyp
        | `FnAppl (f, (ps, pos')) ->
            let f = type_check typing_env f
            and ps = List.map (type_check typing_env) ps
            and rettyp = Types.fresh_type_variable () in
              unify (typ f) (`Function (Types.make_tuple_type (List.map typ ps), 
                                        mailbox_type env, rettyp));
              `FnAppl (f, (ps, pos')), rettyp

        (* xml *)
        | `Xml (tag,attrs,children) ->
            assert false
        | `TextNode _ as t -> t, Types.xml_type
        | `Formlet (body, yields) ->
            let body = type_check typing_env body
            and yields = type_check typing_env yields in
              unify (typ body) Types.xml_type;
              (* TODO: extract the bindings from body
                 and put them in the typing environment for
                 yields *)
              assert false
        | `FormBinding (e, pattern) ->
            let e = type_check typing_env e
            and pattern = type_pattern alias_env pattern in
            let a = Types.fresh_type_variable () in
            let ft = Types.make_formlet_type a in
              unify (typ e) ft;
              unify (pattern_typ pattern) a;
              `FormBinding (e, pattern), Types.xml_type

        (* various expressions *)
        | `Iteration _ ->       assert false
        | `Escape _ ->          assert false
        | `Conditional (i,t,e) ->
            let i = type_check typing_env i
            and t = type_check typing_env t
            and e = type_check typing_env e in
              unify (typ i) (`Primitive `Bool);
              unify (typ t) (typ e);
              `Conditional (i,t,e), (typ t)
        | `Block _ ->           assert false
        | `Regex _ ->           assert false
        | `Projection (r,l) ->
            let r = type_check typing_env r in
            let fieldtype = Types.fresh_type_variable () in
	      unify (typ r) (`Record (Types.make_singleton_open_row 
                                        (l, `Present fieldtype)));
              `Projection (r, l), fieldtype
        | `With _ ->            assert false
        | `TypeAnnotation _ ->  assert false
        | `Switch _ ->          assert false
    in e, (pos, t)
  in type_check

let type_top_level lookup_pos =
  let rec type_top_level ((env, alias_env) as typing_env) (def, pos) =
    let unify = Utils.unify alias_env
    and unify_rows = Utils.unify_rows alias_env 
    and typ (_,(_,t)) = t 
    and type_pattern = type_pattern lookup_pos in
    let def, t =
      match (def : Untyped.toplevel') with
        (* declarations *)
        | `VarDefinition _   -> assert false
        | `FunDefinition _   -> assert false
        | `Foreign _         -> assert false
        | `TypeDeclaration _ -> assert false
        | `InfixDecl         -> assert false
        | #phrasenode as e ->
            type_check lookup_pos typing_env (e, pos)
    in
      def, (pos, t)
  in
    type_top_level
    
