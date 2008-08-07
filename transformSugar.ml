open Utility
open Sugartypes

module TyEnv = Env.String

(* TODO:

   should probably move this to TypeUtils
*)
let apply_type : Types.datatype -> Types.type_arg list -> Types.datatype = fun  t tyargs ->
  let vars = TypeUtils.quantifiers t in
  let tenv, renv =
    List.fold_right2
      (fun var t (tenv, renv) ->
         match (var, t) with
           | ((`TypeVar var | `RigidTypeVar var), `Type t) ->
               (IntMap.add var t tenv, renv)
           | ((`RowVar var | `RigidRowVar var), `Row row) ->
               (* 
                  TODO:
                  
                  Work out how to put the row in the row_var
                  environment.

                  We could simply wrap it in a `Body tag, but then we need to be careful
                  about which bits of the compiler are assuming that
                  rows are already flattened.
               *)
               (tenv, IntMap.add var (assert false) renv))
      vars tyargs (IntMap.empty, IntMap.empty)
  in
    Instantiate.datatype (tenv, renv) (Types.freshen_mailboxes t)

let fresh_type_quantifier () =
  let var = Types.fresh_raw_variable () in
    `RigidTypeVar var, Types.make_rigid_type_variable var

let fresh_row_quantifier () =
  let var = Types.fresh_raw_variable () in
    `RigidTypeVar var, Types.make_rigid_row_variable var

let type_section env =
  function
    | `Minus         -> TyEnv.lookup env "-"
    | `FloatMinus    -> TyEnv.lookup env "-."
    | `Project label ->
        let fb, f = fresh_type_quantifier () in
        let mb, m = fresh_type_quantifier () in
        let r = `Record (Types.make_singleton_open_row (label, `Present f)) in
          `ForAll ([fb; mb],
                   `Function (Types.make_tuple_type [r], m, f))
    | `Name var      -> TyEnv.lookup env var

let type_unary_op env tycon_env =
  let datatype = DesugarDatatypes.read ~aliases:tycon_env in function
    | `Minus      -> datatype "(Int) -> Int"
    | `FloatMinus -> datatype "(Float) -> Float"
    | `Name n     -> TyEnv.lookup env n
    | `Abs        -> 
        (* forall (rho, mb, a, mb2).(((|rho)) -{mb}-> a) -{mb2}-> *(|rho) -{mb}-> a *)
        let rhob, rho = fresh_row_quantifier () in
        let row = `Record (StringMap.empty, rho) in
        let mb, m = fresh_type_quantifier () in
        let ab, a = fresh_type_quantifier () in
        let mb2, m2 = fresh_type_quantifier () in
          `ForAll
            ([rhob; mb; ab; mb2],
             `Function (Types.make_tuple_type [
                          `Function (row, m, a)
                        ], m2,
                        `Function (row, m, a)))

let type_binary_op env tycon_env =
  let datatype = DesugarDatatypes.read ~aliases:tycon_env in function
  | `Minus        -> TyEnv.lookup env "-"
  | `FloatMinus   -> TyEnv.lookup env "-."
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
  | `Cons         -> TyEnv.lookup env "Cons"
  | `Name "++"    -> TyEnv.lookup env "Concat"
  | `Name ">"
  | `Name ">="
  | `Name "=="
  | `Name "<"
  | `Name "<="
  | `Name "<>" ->
      let ab, a = fresh_type_quantifier () in
      let mb, m = fresh_type_quantifier () in
        `ForAll ([ab; mb],
                 `Function (Types.make_tuple_type [a; a], m, `Primitive `Bool))
  | `Name "!"     -> TyEnv.lookup env "send"
  | `Name n       -> TyEnv.lookup env n
  | `App          -> 
      (* forall (rho, mb, a, mb2).((|rho) -{mb}-> a, (|rho)) -{mb2}-> a *)
      let rhob, rho = fresh_row_quantifier () in
      let row = `Record (StringMap.empty, rho) in
      let mb, m = fresh_type_quantifier () in
      let mb2, m2 = fresh_type_quantifier () in
      let ab, a = fresh_type_quantifier () in
        `ForAll
          ([rhob; mb; ab; mb2],
           `Function (Types.make_tuple_type [
                        `Function (row, m, a);
                        row],
                      m2, a))


class transform (env : (Types.environment * Types.tycon_environment)) =
  object (o : 'self_type)
    val var_env = fst env
    val tycon_env = snd env

    method lookup_type : name -> Types.datatype = fun var ->
      TyEnv.lookup var_env var

    method lookup_mb : unit -> Types.datatype = fun () ->
      o#lookup_type "_MAILBOX_"

    method with_mb : Types.datatype -> 'self_type = fun mb ->
      {< var_env = TyEnv.bind var_env ("_MAILBOX_", mb) >}

    method option :
      'a.
        ('self_type -> 'a -> ('self_type * 'a * Types.datatype)) ->
          'a option -> ('self_type * ('a option) * (Types.datatype option)) =
      fun f ->
        function
        | None -> (o, None, None)
        | Some x -> let (o, x, t) = f o x in (o, Some x, Some t)
      
    method optionu :
      'a.
        ('self_type -> 'a -> ('self_type * 'a)) ->
          'a option -> ('self_type * ('a option)) =
      fun f ->
        function
        | None -> (o, None)
        | Some x -> let (o, x) = f o x in (o, Some x)

    method list :
      'a.
        ('self -> 'a -> 'self * 'a * Types.datatype) ->
          'a list -> 'self * 'a list * Types.datatype list =
      fun f ->
        function
          | [] -> (o, [], [])
          | x :: xs ->
              let (o, x, t) = f o x in
              let (o, xs, ts) = o#list f xs in (o, x::xs, t::ts)

    method listu :
      'a.
        ('self -> 'a -> 'self * 'a) ->
          'a list -> 'self * 'a list =
      fun f ->
        function
          | [] -> (o, [])
          | x :: xs ->
              let (o, x) = f o x in
              let (o, xs) = o#listu f xs in (o, x::xs)

    method unary_op : unary_op -> ('self_type * unary_op * Types.datatype) =
      fun op ->
        (o, op, type_unary_op var_env tycon_env op)

    method binop : binop -> ('self_type * binop * Types.datatype) =
      fun op ->
        (o, op, type_binary_op var_env tycon_env op)

    method sec : sec -> ('self_type * sec * Types.datatype) =
      fun sec ->
        (o, sec, type_section var_env sec)


(* Do we want to implement these? *)
(*
    method sentence' : sentence' -> ('self_type * sentence') =
      function
      | `Definitions _x ->
          (o, (`Definitions _x))
      | `Expression _x -> (o, (`Expression _x))
      | `Directive _x -> let (o, _x) = o#directive _x in (o, (`Directive _x))
      
    method sentence : sentence -> ('self_type * sentence) =
      function
      | `Definitions _x ->
          let (o, _x) = o#list (fun o -> o#binding) _x
          in (o, (`Definitions _x))
      | `Expression _x -> let (o, _x) = o#phrase _x in (o, (`Expression _x))
      | `Directive _x -> let (o, _x) = o#directive _x in (o, (`Directive _x))

    method directive : directive -> ('self_type * directive) =
      fun (_x, _x_i1) ->
        let (o, _x) = o#string _x in
        let (o, _x_i1) = o#list (fun o -> o#string) _x_i1 in (o, (_x, _x_i1))     
*)

    method regex : regex -> ('self_type * regex) =
      function
        | (`Range _ | `Simply _ | `Any  | `StartAnchor | `EndAnchor) as r -> (o, r)
        | `Quote r -> let (o, r) = o#regex r in (o, `Quote r)
        | `Seq rs ->
            let (o, rs) = o#listu (fun o -> o#regex) rs in (o, `Seq rs)
        | `Alternate (r1, r2) ->
            let (o, r1) = o#regex r1 in
            let (o, r2) = o#regex r2 in
              (o, `Alternate (r1, r2))
        | `Group r -> let (o, r) = o#regex r in (o, `Group r)
        | `Repeat (repeat, r) ->
            let (o, r) = o#regex r in (o, `Repeat (repeat, r))
        | `Splice e -> let (o, e, _) = o#phrase e in (o, `Splice e)
        | `Replace (r, `Literal s) ->
            let (o, r) = o#regex r in
              (o, `Replace (r, `Literal s))
        | `Replace (r, `Splice e) ->
            let (o, r) = o#regex r in
            let (o, e, _) = o#phrase e in
              (o, `Replace (r, `Splice e))
      
    method program : program -> ('self_type * program * Types.datatype option) =
      fun (bs, e) ->
        let (o, bs) = o#listu (fun o -> o#binding) bs in
        let (o, e, t) = o#option (fun o -> o#phrase) e
        in (o, (bs, e), t)

    method phrasenode : phrasenode -> ('self_type * phrasenode * Types.datatype) =
      function
      | `Constant c -> let (o, c, t) = o#constant c in (o, (`Constant c), t)
      | `Var var -> (o, `Var var, o#lookup_type var)
      | `FunLit (Some argss, (pss, e)) ->
          (*
            TODO:

            Make sure that the right mailbox is in scope for the body.
            Need to do something similar for `Fun and `Funs.

            It would probably make most sense to add an extra mailbox
            argument to the funlit method.
          *)
          let (o, pss) = o#listu (fun o -> o#listu (fun o -> o#pattern)) pss in
          let (o, e, rt) = o#phrase e in
          let t =
            List.fold_right
              (fun (args, mb) rt ->
                 `Function (args, mb, rt))
              argss
              rt
          in
            (o, `FunLit (Some argss, (pss, e)), t)
      | `Spawn (body, Some inner_mb) ->
          (*
            HACK:

            This code attempts to ensure that the right mailbox is in
            scope at the right time.
          *)
          let outer_mb = o#lookup_mb () in
          let o = o#with_mb inner_mb in
          let (o, body, _) = o#phrase body in
          let o = o#with_mb outer_mb in
            (o, (`Spawn (body, Some inner_mb)), inner_mb)
      | `SpawnWait (body, Some inner_mb) ->
          (*
            HACK:

            This code attempts to ensure that the right mailbox is in
            scope at the right time.
          *)
          let outer_mb = o#lookup_mb () in
          let o = o#with_mb inner_mb in
          let (o, body, body_type) = o#phrase body in
          let o = o#with_mb outer_mb in
            (o, `SpawnWait (body, Some inner_mb), body_type)
      | `ListLit (es, Some t) ->
          let (o, es, _) = o#list (fun o -> o#phrase) es in (o, `ListLit (es, Some t), t)
      | `RangeLit (e1, e2) ->
          let (o, e1, _) = o#phrase e1 in
          let (o, e2, _) = o#phrase e2 in
            (o, `RangeLit (e1, e2), Types.make_list_type Types.int_type)
      | `Iteration (gens, body, cond, orderby) ->
          let (o, gens) = o#listu (fun o -> o#iterpatt) gens in
          let (o, body, t) = o#phrase body in
          let (o, cond, _) = o#option (fun o -> o#phrase) cond in
          let (o, orderby, _) = o#option (fun o -> o#phrase) orderby in
            (o, `Iteration (gens, body, cond, orderby), t)
      | `Escape (b, e) ->
          let (o, b) = o#binder b in
          let (o, e, t) = o#phrase e in
            (o, `Escape (b, e), t)
      | `Section sec -> (o, `Section sec, type_section var_env sec)
      | `Conditional (p, e1, e2) ->
          let (o, p, _) = o#phrase p in
          let (o, e1, t) = o#phrase e1 in
          let (o, e2, _) = o#phrase e2
          in (o, `Conditional (p, e1, e2), t)
      | `Block (bs, e) ->
          let (o, bs) = o#listu (fun o -> o#binding) bs in
          let (o, e, t) = o#phrase e in (o, `Block (bs, e), t)
      | `InfixAppl ((tyargs, op), e1, e2) ->
          let (o, op, t) = o#binop op in
          let t = apply_type t tyargs in
          let (o, e1, _) = o#phrase e1 in
          let (o, e2, _) = o#phrase e2 in
            (o, `InfixAppl ((tyargs, op), e1, e2), t)
      | `Regex r ->
          let (o, r) = o#regex r in
            (o, `Regex r, Instantiate.alias "Regex" [] tycon_env)
      | `UnaryAppl ((tyargs, op), e) ->
          let (o, op, t) = o#unary_op op in
          let t = apply_type t tyargs in
          let (o, e, _) = o#phrase e in
            (o, `UnaryAppl ((tyargs, op), e), t)
      | `FnAppl (f, args) ->
          let (o, f, ft) = o#phrase f in
          let (o, args, _) = o#list (fun o -> o#phrase) args
          in (o, `FnAppl (f, args), TypeUtils.return_type ft)
      | `TAppl (e, tyargs) ->
          let (o, e, t) = o#phrase e in
          let t = apply_type t tyargs in
            (o, `TAppl (e, tyargs), t)
      | `TupleLit es ->
          let (o, es, ts) = o#list (fun o -> o#phrase) es in
            (o, `TupleLit es, Types.make_tuple_type ts)
      | `RecordLit (fields, base) ->
          let (o, fields, field_types) =
            let rec list o =
              function
                | [] -> (o, [], StringMap.empty)
                | (name, e)::fields ->
                    let (o, e, t) = o#phrase e in
                    let (o, fields, field_types) = list o fields in
                      (o,
                       (name, e)::fields,
                       StringMap.add name t field_types)
            in
              list o fields in
          let (o, base, base_type) = o#option (fun o -> o#phrase) base in
          let t =
            match base_type with
              | None -> Types.make_record_type field_types
              | Some t ->
                  begin
                    match TypeUtils.concrete_type t with
                      | `Record row ->
                          `Record (Types.extend_row field_types row)
                      | _ -> assert false
                  end
          in
            (o, `RecordLit (fields, base), t)
      | `Projection (e, name) ->
          let (o, e, t) = o#phrase e in
            (o, `Projection (e, name), TypeUtils.project_type name t)
      | `With (e, fields) ->
          let (o, e, t) = o#phrase e in
          let (o, fields) =
            let rec list o =
              function
                | [] -> (o, [])
                | (name, e)::fields ->
                    let (o, e, _) = o#phrase e in
                    let (o, fields) = list o fields in
                      (o, (name, e)::fields)
            in
              list o fields
          in
            (o, `With (e, fields), t)
      | `TypeAnnotation (e, ((_, Some t) as ann_type)) ->
          let (o, e, _) = o#phrase e in
            (o, `TypeAnnotation (e, ann_type), t)
      | `Upcast (e, ((_, Some t) as to_type), from_type) ->
          let (o, e, _) = o#phrase e in
            (o, `Upcast (e, to_type, from_type), t)
      | `ConstructorLit (name, e, Some t) ->
          let (o, e, _) = o#option (fun o -> o#phrase) e in
            (o, `ConstructorLit (name, e, Some t), t)
      | `Switch (v, cases, Some t) ->
          let (o, v, _) = o#phrase v in
          let (o, cases) =
            o#listu
              (fun o (p, e) ->
                 let (o, p) = o#pattern p in
                 let (o, e, _) = o#phrase e in (o, (p, e)))
              cases
          in
            (o, `Switch (v, cases, Some t), t)
      | `Receive (cases, Some t) ->
          let (o, cases) =
            o#listu
              (fun o (p, e) ->
                 let (o, p) = o#pattern p in
                 let (o, e, _) = o#phrase e in (o, (p, e)))
              cases
          in
            (o, `Receive (cases, Some t), t)
      | `DatabaseLit (name, (driver, args)) ->
          let (o, name, _) = o#phrase name in
          let (o, driver, _) = o#option (fun o -> o#phrase) driver in
          let (o, args, _) = o#option (fun o -> o#phrase) args in
            (o, `DatabaseLit (name, (driver, args)), `Primitive `DB)
      | `TableLit (name, (dtype, Some (read_row, write_row)), constraints, db) ->
          let (o, name, _) = o#phrase name in
          let (o, db, _) = o#phrase db in
            (o, `TableLit (name, (dtype, Some (read_row, write_row)), constraints, db), `Table (read_row, write_row))
      | `DBDelete (p, from, where) ->
          let (o, p) = o#pattern p in
          let (o, from, _) = o#phrase from in
          let (o, where, _) = o#option (fun o -> o#phrase) where in
            (o, `DBDelete (p, from, where), Types.unit_type)
      | `DBInsert (into, values, id) ->
          let (o, into, _) = o#phrase into in
          let (o, values, _) = o#phrase values in
          let (o, id, _) = o#option (fun o -> o#phrase) id in
            (o, `DBInsert (into, values, id), Types.unit_type)
      | `DBUpdate (p, from, where, set) ->
          let (o, p) = o#pattern p in
          let (o, from, _) = o#phrase from in
          let (o, where, _) = o#option (fun o -> o#phrase) where in
          let (o, set) =
            o#listu
              (fun o (name, value) ->
                 let (o, value, _) = o#phrase value in (o, (name, value)))
              set
          in
            (o, `DBUpdate (p, from, where, set), Types.unit_type)
      | `Xml (tag, attrs, attrexp, children) ->
          let (o, attrs) =
            o#listu
              (fun o (name, value) ->
                 let (o, value, _) = o#list (fun o -> o#phrase) value in
                   (o, (name, value)))
              attrs in
          let (o, attrexp, _) = o#option (fun o -> o#phrase) attrexp in
          let (o, children, _) = o#list (fun o -> o#phrase) children in
            (o, `Xml (tag, attrs, attrexp, children), Types.xml_type)
      | `TextNode s -> (o, `TextNode s, Types.xml_type)
      | `Formlet (body, yields) ->
          (*
            HACK:

            This code attempts to ensure that the formlet bindings are only in
            scope in the yields clause.
          *)
          let var_env = var_env in
          let (o, body, _) = o#phrase body in
          let (o, yields, t) = o#phrase yields in
          let o = {< var_env=var_env >} in 
            (o, `Formlet (body, yields), Instantiate.alias "Formlet" [t] tycon_env)
      | `Page e -> let (o, e, _) = o#phrase e in (o, `Page e, Instantiate.alias "Page" [] tycon_env)
      | `FormletPlacement (f, h, attributes) ->
          let (o, f, _) = o#phrase f in
          let (o, h, _) = o#phrase h in
          let (o, attributes, _) = o#phrase attributes in
            (o, `FormletPlacement (f, h, attributes), Types.xml_type)
      | `PagePlacement e ->
          let (o, e, _) = o#phrase e in (o, `PagePlacement e, Types.xml_type)
      | `FormBinding (f, p) ->
          let (o, f, _) = o#phrase f in
          let (o, p) = o#pattern p in
            (o, `FormBinding (f, p), Types.xml_type)
      
    method phrase : phrase -> ('self_type * phrase * Types.datatype) =
      fun (e, pos) ->
        let (o, e, t) = o#phrasenode e in (o, (e, pos), t)
      
    method patternnode : patternnode -> ('self_type * patternnode) =
      function
      | `Any -> (o, `Any)
      | `Nil -> (o, `Nil)
      | `Cons (p, ps) ->
          let (o, p) = o#pattern p in
          let (o, ps) = o#pattern ps in (o, `Cons (p, ps))
      | `List p ->
          let (o, p) = o#listu (fun o -> o#pattern) p in (o, `List p)
      | `Variant (name, p) ->
          let (o, p) = o#optionu (fun o -> o#pattern) p
          in (o, `Variant (name, p))
      | `Negative name -> (o, `Negative name)
      | `Record (fields, rest) ->
          let (o, fields) =
            o#listu
              (fun o (name, p) ->
                 let (o, p) = o#pattern p in (o, (name, p)))
              fields in
          let (o, rest) = o#optionu (fun o -> o#pattern) rest
          in (o, `Record (fields, rest))
      | `Tuple ps ->
          let (o, ps) = o#listu (fun o -> o#pattern) ps in (o, `Tuple ps)
      | `Constant c -> let (o, c, _) = o#constant c in (o, `Constant c)
      | `Variable x -> let (o, x) = o#binder x in (o, `Variable x)
      | `As (x, p) ->
          let (o, x) = o#binder x in
          let (o, p) = o#pattern p in (o, (`As (x, p)))
      | `HasType (p, t) ->
          let (o, p) = o#pattern p in (o, (`HasType (p, t)))
      
    method pattern : pattern -> ('self_type * pattern) =
      fun (p, pos) ->
        let (o, p) = o#patternnode p in (o, (p, pos))
            
    method iterpatt : iterpatt -> ('self_type * iterpatt) =
      function
      | `List (p, e) ->
          let (o, e, _) = o#phrase e in
          let (o, p) = o#pattern p in
            (o, `List (p, e))
      | `Table (p, e) ->
          let (o, e, _) = o#phrase e in
          let (o, p) = o#pattern p in
           (o, `Table (p, e))
      
    method funlit : funlit -> ('self_type * funlit) =
      fun (pss, e) ->
        let (o, pss) = o#listu (fun o -> o#listu (fun o -> o#pattern)) pss in
        let (o, e, _) = o#phrase e in
          (o, (pss, e))

    method constant : constant -> ('self_type * constant * Types.datatype) =
      function
      | `Float v -> (o, `Float v, Types.float_type)
      | `Int v -> (o, `Int v, Types.int_type)
      | `String v -> (o, `String v, Types.string_type)
      | `Bool v -> (o, `Bool v, Types.bool_type)
      | `Char v -> (o, `Char v, Types.char_type)
      
    method bindingnode : bindingnode -> ('self_type * bindingnode) =
      function
      | `Val (tyvars, p, e, location, t) ->
          let (o, e, _) = o#phrase e in
          let (o, p) = o#pattern p in
            (o, `Val (tyvars, p, e, location, t))
      | `Fun (f, (tyvars, lam), location, t) ->
          let (o, f) = o#binder f in
          let (o, lam) = o#funlit lam in
            (o, `Fun (f, (tyvars, lam), location, t))
      | `Funs defs ->
          let (o, defs) =
            o#listu
              (fun o (f, (tyvars, lam), location, t, pos) ->
                 let (o, f) = o#binder f in
                 let (o, lam) = o#funlit lam in
                   (o, (f, (tyvars, lam), location, t, pos)))
              defs
          in
            (o, (`Funs defs))
      | `Foreign (f, language, t) ->
          let (o, f) = o#binder f in
            (o, `Foreign (f, language, t))
      | `Include _ ->
          failwith "Includes aren't supported yet"
      | `Type _ ->
          assert false
      | `Infix -> (o, `Infix)
      | `Exp e -> let (o, e, _) = o#phrase e in (o, `Exp e)
      
    method binding : binding -> ('self_type * binding) =
      fun (b, pos) ->
        let (o, b) = o#bindingnode b in (o, (b, pos))
      
    method binder : binder -> ('self_type * binder) =
      fun (name, Some t, pos) ->
        let var_env = TyEnv.bind var_env (name, t) in        
          ({< var_env=var_env >}, (name, Some t, pos))
  end

 
 
