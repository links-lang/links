

open Utility
open Sugartypes

module rec Typed
  : Phrase with module P = TypedArgs
  = Typed
and TypedArgs : sig
  type phrase   = Typed.phrasenode * (pposition * Types.datatype)
  type ppattern = Typed.pattern * (pposition * (Types.environment * Types.datatype))
  type binding = Typed.binding' * pposition
end
  = TypedArgs


module Utils : sig
  val unify : Types.alias_environment -> Types.datatype -> Types.datatype -> unit
  val unify_rows : Types.alias_environment -> Types.row -> Types.row -> unit
  val instantiate : Types.environment -> string -> Types.datatype
  val generalise : Types.environment -> Types.datatype -> Types.assumption
  val register_alias : name * name list * Types.datatype -> Types.alias_environment -> Types.alias_environment
end =
struct
  let unify _ = assert false
  let unify_rows _ = assert false
  let instantiate _ = assert false
  let generalise _ = assert false
  let register_alias _ = assert false
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

let datatype = Parse.parse_string Parse.datatype ->- snd

let type_unary_op env = function
    | `Minus      -> datatype "(Int) -> Int"
    | `FloatMinus -> datatype "(Float) -> Float"
    | `Name n     -> Utils.instantiate env n
    | `Abs        -> (* Probably doesn't parse at present.
                        See the typing rules given in the note for r975. *)
                     datatype "(((|a)) -> b) -> *(|a) -> b"

let type_binary_op env = function
  | `Minus        -> datatype "(Int,Int) -> Int"
  | `FloatMinus   -> datatype "(Float,Float) -> Float"
  | `RegexMatch flags -> 
      let nativep  = List.exists ((=) `RegexNative)  flags
      and listp    = List.exists ((=) `RegexList)    flags 
      and replacep = List.exists ((=) `RegexReplace) flags in
        (match replacep, listp, nativep with
           | true,   _   , true  -> (* sntilde *) datatype "(NativeString, Regex) -> NativeString"
           | true,   _   , false -> (* stilde  *) datatype "(String, Regex) -> String"
           | false, true , true  -> (* lntilde *) datatype "(NativeString, Regex) -> [String]"
           | false, true , false -> (* ltilde *)  datatype "(String, Regex) -> [String]"
           | false, false, true  -> (* ntilde *)  datatype "(NativeString, Regex) -> Bool"
           | false, false, false -> (* tilde *)   datatype "(String, Regex) -> Bool")
  | `And
  | `Or           -> datatype "(Bool,Bool) -> Bool"
  | `Cons         -> datatype "(a, [a]) -> [a]"
  | `Name n       -> Utils.instantiate env n
  | `App          -> (* Probably doesn't parse at present.  
                        See the typing rules given in the note for r975. *)
                     datatype "(*(|a) -> b) -> ((|a)) -> b"

module Env = Env.String

let type_pattern lookup_pos alias_env : Untyped.ppattern -> Typed.ppattern =
  let rec type_pattern  (pattern, pos) : Typed.ppattern =
    let unify = Utils.unify alias_env
    and unify_rows = Utils.unify_rows alias_env 
    and typ (_,(_,(_,t))) = t
    and env (_,(_,(e,_))) = e
    (* TODO: check for duplicate bindings *)
    and (++) = Env.extend in
    let (p, e, t : Typed.pattern * Types.environment * Types.datatype) =
      match (pattern : Untyped.pattern) with
        | `Any                   -> `Any,
            Env.empty, Types.fresh_type_variable ()
        | `Nil                   -> `Nil,
            Env.empty, (Types.make_list_type
                          (Types.fresh_type_variable ()))
        | `Constant c as c'      -> c', Env.empty, constant_type c
        | `Variable x            -> 
            let xtype = Types.fresh_type_variable () in
              (`Variable x,
               (Env.bind Env.empty (x, ([], xtype))),
               xtype)
        | `Cons (p1, p2)         -> 
            let p1 = type_pattern p1
            and p2 = type_pattern p2 in
            let _ = unify (Types.make_list_type (typ p1)) (typ p2) in
              `Cons (p1, p2), env p1 ++ env p2, typ p2
        | `List ps               -> 
            let ps' = List.map type_pattern ps in
            let env' = List.fold_right (env ->- (++)) ps' Env.empty in
            let element_type = 
              match ps' with
                | [] -> Types.fresh_type_variable ()
                | p::ps -> 
                    let _ = List.iter (typ ->- unify (typ p)) ps in
                      typ p
            in `List ps', env', Types.make_list_type element_type
        | `Variant (name, None)       -> 
            let vtype = `Variant (Types.make_singleton_open_row (name, `Present Types.unit_type)) in
              `Variant (name, None), Env.empty, vtype
        | `Variant (name, Some p)     -> 
            let p = type_pattern p in
            let vtype = `Variant (Types.make_singleton_open_row (name, `Present (typ p))) in
              `Variant (name, Some p), env p, vtype
        | `Record (ps, default)  -> 
            let ps = alistmap type_pattern ps
            and default = opt_map type_pattern default in
            let initial, denv = match default with
              | None -> (Types.make_empty_closed_row (),
                         Env.empty)
              | Some r -> 
                  let row = Types.make_empty_open_row () in
                  let _ = unify (`Record row) (typ r) in
                    row, env r in
            let rtype = 
              `Record (List.fold_right
                         (fun (l, f) -> Types.row_with (l, `Present (typ f)))
                         ps initial)
            and penv = 
              List.fold_right (snd ->- env ->- (++)) ps Env.empty in
              `Record (ps, default), penv ++ denv, rtype
        | `Tuple ps              -> 
            let ps' = List.map type_pattern ps in
            let env' = List.fold_right (env ->- (++)) ps' Env.empty in
            let typ' = Types.make_tuple_type (List.map typ ps') in
              `Tuple ps', env', typ'
        | `As (x, p)             -> 
            let p = type_pattern p in
            let env' = Env.bind (env p) (x, ([], typ p)) in
              `As (x, p), env', (typ p)
        | `HasType (p, t)        -> 
            let p = type_pattern p in
            let _ = unify (typ p) (snd (Sugar.desugar_datatype t)) in
              `HasType (p, t), env p, typ p
    in
      p, (pos, (e,t))
  in
    type_pattern

let rec extract_formlet_bindings (expr, pos) =
  let pattern_env  (_,(_,(e,_))) = e in
    match expr with
      | `FormBinding (f, pattern) -> pattern_env pattern
      | `Xml (_, _, children) ->
          List.fold_right
            (fun child env ->
               Env.extend env (extract_formlet_bindings child))
            children Env.empty
      | _ -> Env.empty
          
let rec type_check lookup_pos : Types.typing_environment -> Untyped.phrase -> Typed.phrase = 
  let rec type_check ((env, alias_env) as typing_env) (expr, pos) =
    let type_pattern = type_pattern lookup_pos in
    let unify = Utils.unify alias_env
    and unify_rows = Utils.unify_rows alias_env 
    and (++) env' (env, alias_env) = (Env.extend env' env, alias_env)
    and typ (_,(_,t)) = t 
    and pattern_typ (_, (_,(_,t))) = t
    and pattern_env (_, (_,(e,_))) = e
    and tp = type_pattern alias_env
    and tc = type_check typing_env in
    let e, t =
      match (expr : Untyped.phrasenode) with
        | `Var v            -> `Var v, Utils.instantiate env v
        | `Section _ as s   -> type_section env s


        (* literals *)
        | `Constant c as c' -> c', constant_type c
        | `TupleLit ps ->
            let ps = List.map (type_check typing_env) ps in
              `TupleLit ps, Types.make_tuple_type (List.map typ ps)
        | `RecordLit (fields, rest) ->
            let rest = opt_map (type_check typing_env) rest in
            let row = Types.make_empty_open_row () in
            let _ = match rest with
              | None -> ()
              | Some r -> unify (`Record row) (typ r) in
            let fields = alistmap (type_check typing_env) fields in
            let rtype =
              `Record (List.fold_right
                         (fun (lab, rhs) row ->
                            Types.row_with (lab, `Present (typ rhs)) row)
                         fields row) in
                `RecordLit (fields, rest), rtype
                         
        | `ListLit es ->
            begin match List.map (type_check typing_env) es with
              | [] -> `ListLit [], `Application ("List", [Types.fresh_type_variable ()])
              | e :: es -> 
                  List.iter (typ ->- unify (typ e)) es;
                  `ListLit es, `Application ("List", [typ e])
            end
        | `FunLit (pats, body) ->
            let pats = List.map (List.map tp) pats in
            let fold_in_envs = List.fold_left (fun env pat' -> (pattern_env pat') ++ env) in
            let env', aliases = List.fold_left fold_in_envs typing_env pats in
            let body = type_check (Env.bind env' (mailbox, ([], Types.fresh_type_variable ())), aliases) body in
            let ftype = 
              List.fold_right
                (fun pat rtype ->
                   let args = Types.make_tuple_type (List.map pattern_typ pat) in
                     `Function (args, Types.fresh_type_variable (), rtype))
                pats (typ body) in
              `FunLit (pats, body), ftype

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

        | `DBDelete ((pat, from), where) ->
            let pat  = tp pat 
            and from = type_check typing_env from
            and read  = `Record (Types.make_empty_open_row ())
            and write = `Record (Types.make_empty_open_row ()) in
            let _ = unify (typ from) (`Table (read, write))
            and _ = unify (pattern_typ pat) write in
            let where = opt_map (type_check ((pattern_env pat) ++ typing_env)) where in
            let _     = opt_iter (typ ->- unify Types.bool_type) where in
              `DBDelete ((pat, from), where), Types.unit_type
        | `DBInsert (into, values) ->
            let into   = type_check typing_env into
            and values = type_check typing_env values
            and read  = `Record (Types.make_empty_open_row ())
            and write = `Record (Types.make_empty_open_row ()) in
            let _ = unify (typ into) (`Table (read, write))
            and _ = unify write (Types.make_list_type write) in
              `DBInsert (into, values), Types.unit_type
        | `DBUpdate ((pat, from), where, set) ->
            let pat  = tp pat
            and from = type_check typing_env from
            and read =  `Record (Types.make_empty_open_row ())
            and write = `Record (Types.make_empty_open_row ()) in
            let _ = unify (typ from) (`Table (read, write))
            and _ = unify (pattern_typ pat) write in
            let typing_env' = (pattern_env pat) ++ typing_env in
            let where = opt_map (type_check typing_env') where in
            let _     = opt_iter (typ ->- unify Types.bool_type) where in
            let set = List.map 
              (fun (name, exp) ->
                 let exp = type_check typing_env exp in
                 let _ = unify write (`Record (Types.make_singleton_open_row
                                                 (name, `Present (typ exp)))) in
                   (name, exp)) set in
              `DBUpdate ((pat, from), where, set), Types.unit_type

        (* concurrency *)
        | `Spawn p ->
            (* (() -{b}-> d) -> Mailbox (b) *)
            let pid_type = Types.fresh_type_variable () in
            let typing_env' = Env.bind env (mailbox, ([], pid_type)), alias_env in
            let p = type_check typing_env' p in
              `Spawn p, Types.make_mailbox_type pid_type
        | `Receive binders ->
            let mbtype = mailbox_type env 
            and rtype = Types.fresh_type_variable () in
            let binders = 
              List.fold_right
                (fun (pat, cont) binders ->
                   let pat = tp pat in
                   let cont = type_check (pattern_env pat ++ typing_env) cont in
                   let _ = unify (pattern_typ pat) mbtype 
                   and _ = unify (typ cont) rtype in
                     (pat,cont)::binders)
                binders [] in
              `Receive binders, rtype

        (* applications of various sorts *)
        | `UnaryAppl (op, p) -> 
            let op = op, (pos, type_unary_op env op)
            and p = type_check typing_env p
            and rettyp = Types.fresh_type_variable () in
              unify (typ op) (`Function (Types.make_tuple_type [typ p], 
                                         mailbox_type env, rettyp));
              `UnaryAppl (fst op, p), rettyp
        | `InfixAppl (op, l, r) ->
            let op = op, (pos, type_binary_op env op)
            and l = type_check typing_env l
            and r = type_check typing_env r 
            and rettyp = Types.fresh_type_variable () in
              unify (typ op) (`Function (Types.make_tuple_type [typ l; typ r], 
                                         mailbox_type env, rettyp));
              `InfixAppl (fst op, l, r), rettyp
        | `FnAppl (f, (ps, pos')) ->
            let f = type_check typing_env f
            and ps = List.map (type_check typing_env) ps
            and rettyp = Types.fresh_type_variable () in
              unify (typ f) (`Function (Types.make_tuple_type (List.map typ ps), 
                                        mailbox_type env, rettyp));
              `FnAppl (f, (ps, pos')), rettyp

        (* xml *)
        | `Xml (tag, attrs, children) ->
            let attrs = alistmap (List.map (type_check typing_env)) attrs
            and children = List.map (type_check typing_env) children in
            let _ = List.iter (snd ->- List.iter (typ ->- unify Types.string_type)) attrs
            and _ = List.iter (typ ->- unify Types.xml_type) children in
              `Xml (tag, attrs, children), Types.xml_type
        | `TextNode _ as t -> t, Types.xml_type
        | `Formlet (body, yields) ->
            let body = type_check typing_env body in
            let typing_env = (extract_formlet_bindings body) ++ typing_env in
            let yields = type_check typing_env yields in
              unify (typ body) Types.xml_type;
              `Formlet (body, yields), Types.make_formlet_type (typ yields)
        | `FormBinding (e, pattern) ->
            let e = type_check typing_env e
            and pattern = tp pattern in
            let a = Types.fresh_type_variable () in
            let ft = Types.make_formlet_type a in
              unify (typ e) ft;
              unify (pattern_typ pattern) a;
              `FormBinding (e, pattern), Types.xml_type

        (* various expressions *)
        | `Iteration (binder, body, where, orderby) ->            
            let binder, typing_env =
              let a = Types.fresh_type_variable () in
              let lt = Types.make_list_type a in
                match binder with
                | `List (pattern, e) ->
                    let pattern = tp pattern
                    and e = tc e in             
                      unify lt (typ e);
                      unify lt (pattern_typ pattern);
                      `List (pattern, e), pattern_env pattern ++ typing_env
                | `Table (pattern, e) ->
                    let tt = Types.make_table_type (a, Types.fresh_type_variable ()) in
                    let pattern = tp pattern
                    and e = tc e in
                      unify tt (typ e);
                      unify lt (pattern_typ pattern);
                      `Table (pattern, e), pattern_env pattern ++ typing_env in
            let tc = type_check typing_env in
            let body = tc body in
            let where = opt_map tc where in
            let orderby = opt_map tc orderby in
              unify (Types.make_list_type (Types.fresh_type_variable ())) (typ body);
              opt_iter (fun where -> unify (Types.bool_type) (typ where)) where;
              `Iteration (binder, body, where, orderby), (typ body)

        | `Escape (name, e) ->
            (* There's a question here whether to generalise the
               return type of continuations.  With `escape'
               continuations are let-bound, so generalising the return
               type is sound.  With `call/cc' continuations are
               lambda-bound so the return type cannot be generalised.
               If we do generalise here then we can accept more valid
               programs, since the continuation can then be used in
               any context, e.g.:
               
                 escape y in {
                   var _ = y(1) == "";
                   var _ = y(1) == true;
                   2
                 }

               However, currently we desugar escape to call/cc, so
               generalising will mean accepting programs that have an
               invalid type in the IR (although they're guaranteed not
               to "go wrong".)

               (Also, should the mailbox type be generalised?)
            *)
            let f = Types.fresh_type_variable ()
            and t = Types.fresh_type_variable ()
            and m = Types.fresh_type_variable () in
            let cont_type = `Function (Types.make_tuple_type [f], m, t) in
            let typing_env' = Env.bind env (name, ([], cont_type)), alias_env in
            let e = type_check typing_env' e in
            let _ = unify f (typ e) in
              `Escape (name, e), (typ e)
        | `Conditional (i,t,e) ->
            let i = type_check typing_env i
            and t = type_check typing_env t
            and e = type_check typing_env e in
              unify (typ i) (`Primitive `Bool);
              unify (typ t) (typ e);
              `Conditional (i,t,e), (typ t)
        | `Block (bindings, e) ->
            let rec type_bindings typing_env =
              function
                | [] -> [], typing_env
                | b :: bs ->
                    let b, typing_env' = type_binding lookup_pos typing_env b in
                    let bs, typing_env = type_bindings (Types.concat_typing_environment typing_env' typing_env) bs in
                      b :: bs, typing_env in
            let bindings, typing_env = type_bindings typing_env bindings in
            let e = type_check typing_env e in
              `Block (bindings, e), typ e
        | `Regex r ->
            `Regex (type_regex lookup_pos typing_env r), `Application ("Regex", [])
        | `Projection (r,l) ->
            let r = type_check typing_env r in
            let fieldtype = Types.fresh_type_variable () in
	      unify (typ r) (`Record (Types.make_singleton_open_row 
                                        (l, `Present fieldtype)));
              `Projection (r, l), fieldtype
        | `With (r, fields) ->
            let r = tc r
            and fields = alistmap tc fields in
            let rtype = typ r 
            and fields_type =
              `Record (List.fold_right
                         (fun (lab, rhs) row ->
                            Types.row_with (lab, `Present (typ rhs)) row)
                         fields (Types.make_empty_open_row ())) in
                unify fields_type rtype;
                `With (r, fields), rtype
        | `TypeAnnotation (e, t) ->
            let e = tc e
            and t' = snd (Sugar.desugar_datatype t) in
              unify (typ e) t';
              `TypeAnnotation (e, t), t'
        | `Switch (e, binders) ->
            let e = tc e in
            let et = typ e
            and t = Types.fresh_type_variable () in
            let binders = 
              List.fold_right
                (fun (pat, cont) binders ->
                   let pat = tp pat in
                   let cont = type_check (pattern_env pat ++ typing_env) cont in
                   let _ = unify (pattern_typ pat) et 
                   and _ = unify (typ cont) t in
                     (pat,cont)::binders)
                binders [] in
              `Switch (e, binders), t


    in e, (pos, t)
  in type_check
and type_binding lookup_pos : Types.typing_environment -> Untyped.binding -> Typed.binding * Types.typing_environment =
  let rec type_top_level ((env, alias_env) as typing_env) (def, pos) =
    let type_check = type_check lookup_pos
    and type_pattern = type_pattern lookup_pos in
    let unify = Utils.unify alias_env
    and unify_rows = Utils.unify_rows alias_env 
    and typ (_,(_,t)) = t
    and pattern_typ (_, (_,(_,t))) = t
    and tc = type_check typing_env
    and tp = type_pattern alias_env
    and pattern_env  (_,(_,(e,_))) = e
    and (++) env' (env, alias_env) = (Env.extend env' env, alias_env) in
    let typed, env = match (def : Untyped.binding') with
        | `Val (pat, body, location, datatype) -> 
            let body = tc body in
            let pat = tp pat in
            let _ = opt_iter (Sugar.desugar_datatype ->- snd ->- unify (typ body)) datatype in
              (* TODO: generalisation *)
              (`Val (pat, body, location, datatype), 
               (Env.extend env (pattern_env pat), alias_env))
        | `Fun (name, (pats, body), location, t) ->
            let pats = List.map (List.map tp) pats in
            let fold_in_envs = List.fold_left (fun env pat' -> (pattern_env pat') ++ env) in
            let body_env, alias_env = List.fold_left fold_in_envs typing_env pats in
            let body = type_check (Env.bind body_env (mailbox, ([], Types.fresh_type_variable ())), alias_env) body in
            let ft =
                List.fold_right
                  (fun pat rtype ->
                     let args = Types.make_tuple_type (List.map pattern_typ pat) in
                       `Function (args, Types.fresh_type_variable (), rtype))
                  pats (typ body) in
            let _ = opt_iter (Sugar.desugar_datatype ->- snd ->- unify ft) t in
              (`Fun (name, (pats, body), location, t),
               (Env.bind env (name, Utils.generalise env ft), alias_env))
        | `Funs (defs) ->
            let fbs, patss =
              List.split 
                (List.map
                   (fun (name, (pats, body), _, t) ->
                      let pats = List.map (List.map tp) pats in
                      let ft =
                        List.fold_right
                          (fun pat rtype ->
                             let args = Types.make_tuple_type (List.map pattern_typ pat) in
                               `Function (args, Types.fresh_type_variable (), rtype))
                          pats (Types.fresh_type_variable ()) in
                      let _ = opt_iter (Sugar.desugar_datatype ->- snd ->- unify ft) t in
                        ((name, ft), pats)) defs) in
            let fbs = List.map (fun (name, t) -> name, Utils.generalise env t) fbs in
            let env = List.fold_left (fun env (name, t) -> Env.bind env (name, t)) env fbs in
            let typing_env = env, alias_env in
            let fold_in_envs = List.fold_left (fun env pat' -> (pattern_env pat') ++ env) in

            let defs =
              List.rev
                (List.fold_left2
                   (fun defs (name, (_, body), location, t) pats ->
                      let body_env, alias_env = List.fold_left fold_in_envs typing_env pats in
                      let body = type_check (Env.bind body_env (mailbox, ([], Types.fresh_type_variable ())), alias_env) body in
                        (name, (pats, body), location, t) :: defs) [] defs patss)
            in
              (`Funs defs, typing_env)
        | `Foreign (language, name, datatype) ->
            let assumption = Sugar.desugar_datatype datatype in
              (`Foreign (language, name, datatype),
               (Env.bind env (name, assumption), alias_env))
        | `Type (typename, args, datatype) as t ->
            let _, dtype = Sugar.desugar_datatype datatype in
              (t, (env, Utils.register_alias (typename, args, dtype) alias_env))
        | `Infix -> `Infix, typing_env
        | `Exp e ->
            let e = tc e in
              `Exp e, (Env.empty, Env.empty)
    in (typed, pos), env
  in
    type_top_level
and type_regex lookup_pos typing_env : Untyped.regex -> Typed.regex =
  let tr = type_regex lookup_pos typing_env in
    function
      | (`Range _ | `Simply _ | `Any  | `StartAnchor | `EndAnchor) as r -> r
      | `Quote r -> `Quote (tr r)
      | `Seq rs -> `Seq (List.map tr rs)
      | `Alternate (r1, r2) -> `Alternate (tr r1, tr r2)
      | `Group r -> `Group (tr r)
      | `Repeat (repeat, r) -> `Repeat (repeat, tr r)
      | `Splice e -> `Splice (type_check lookup_pos typing_env e)
      | `Replace (r, `Literal s) -> `Replace (tr r, `Literal s)
      | `Replace (r, `Splice e) -> `Replace (tr r, `Splice (type_check lookup_pos typing_env e))
    
