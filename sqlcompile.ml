(*pp deriving *)

(* TODO
   1. sort (?) -
   2. tests
   2. length (count), max, min, average -
   3. projection (via typing trick).
   4. rearranging queries to maximise compilability.
*)

open Utility
open List
open Num

open SqlQuery

let debugging = ref false

(* [sqlable_primtype ty] is true if [ty] corresponds to a primitive
   SQL type. *)
let sqlable_primtype ty = 
  match Types.concrete_type ty with
      `Primitive ty' -> (match ty' with 
                            `Bool | `Int | `Char | `Float -> true
                          | _ -> Debug.print("non-sqlable primitive: " ^ 
                                             Types.string_of_datatype ty);
                              false)
    | `Application ("String", []) -> true
    | `Application ("List", [`Primitive `Char]) -> true
    | _ -> Debug.print("non-primitive in record was " ^ 
                         Types.string_of_datatype ty);
        false

(* [sqlable_record ty] is true if [ty] is a record type that
   corresponds to a valid SQL result--that is, it is essentially 
   flat with fields types that are SQL primitive types. *)
let sqlable_record =
  function
    | `Record (fields, _) -> 
        StringMap.for_all (function
                             | `Absent -> true
                             | `Present ty ->
                                 sqlable_primtype ty) fields
    | _ -> false


module Prepare =
struct
  open Syntax

  let unpack : 'a StringMap.t -> 'a list option = 
    let rec aux n input output = 
      if input = StringMap.empty then Some (List.rev output)
      else
        try 
          let item, rest = StringMap.pop (string_of_int n) input in
            aux (n+1) rest (item::output)
        with Not_found -> None
    in fun map -> aux 1 map []

  let remove_unused_variables : RewriteSyntax.rewriter = function
    | Let (var, expr, body, _) when Syntax.pure expr
                                    && not (StringSet.mem var (freevars body))
        -> Some body
    | _ -> None

    (* This module will go away once we stop using Record_selection *)
  module NormalizeProjections : 
  sig
    val normalize_projections : RewriteSyntax.rewriter
  end =
  struct
    (** The purpose of this set of rewrite rules is to replace all
        occurrences of Record_selection with Project and Erase
        (minimising the number of times Erase occurs.) *)
    
    open RewriteSyntax
      
    (** eliminate all occurrences of Record_selection in favour of
        Project and Erase *)
    let convert_projections : rewriter = function
      | Record_selection (label, labelvar, etcvar, (Variable _ as v), body, data) ->
          Some (Let (labelvar, Project (v, label, data),
                     Let (etcvar, Erase (v, label, data),
                          body, data), data))
      | Record_selection (label, labelvar, etcvar, value, body, data) ->
          let name = gensym ~prefix:"recordvar" () in
            Some(Let(name, value, 
                     Let(labelvar, Project (Variable (name, data), label, data),
                         Let(etcvar, Erase (Variable (name, data), label, data),
                             body, data), data), data))
      | _ -> None
          
    (** Replace projection from an etc. var with projection from the
        record var wherever possible *)
    type env = (string * string) list
    let replace_erase : rewriter =
      fun expr ->
        let rec aux default expression env = match expression with
          | Project (Variable (var, d), l, d') when mem_assoc var env ->
              Project (Variable (assoc var env, d), l, d')
          | Let (etcvar, Erase (Variable (var, d), l, d'), body, d'') ->
              Let (etcvar, 
                   Erase (Variable (var, d), l, d'), 
                   aux default body ((etcvar,var)::env), 
                   d'')
          | e -> default e env in 
        let combiner (exp, subnodes) env : expression =
          Syntax.set_subnodes exp (List.map (fun e -> e env) subnodes)
        in Some (reduce_expression aux combiner expr [])
             
    (** Don't bind the result of a projection to a variable; just use it
        in place *)
    let inline_projections : rewriter = function
      | Let (var, (Project (Variable _, _, _) as p), body, _) ->
          Some (subst_fast var p body)
      | _ -> None
          
    (** Check post conditions hold (not really a rewriter) *)
    let check_projections : rewriter = function
      | Project (Variable _, _, _) -> None
      | Project  _
      | Record_selection _         -> assert false
      | _                          -> None

    let normalize_projections = bottomup (all [
                                            convert_projections;
                                            replace_erase;
                                            remove_unused_variables;
                                            inline_projections;
                                            check_projections
                                          ])
  end

  module Simplify :
  sig
    val simplify : RewriteSyntax.rewriter
  end =
  struct
    open RewriteSyntax
      
    let simplify_concat : rewriter = function
      | Concat ((List_of _ as l), Nil _, _)
      | Concat (Nil _, (List_of _ as l), _) -> Some l
      | _ -> None
          
    let simplify_comprehension : rewriter = function
      | For (List_of (Variable (x, _), _), x', expr, _) when x = x' ->
          Some expr
      | _ -> None
          
    let simplify = bottomup (all [
                               simplify_concat;
                               simplify_comprehension;
                             ])
  end

  let trace_normalize = ref false
    
  (* Not sound! *)
  let simplify_table_query : Syntax.RewriteSyntax.rewriter = function
    | Syntax.Let (_, Syntax.TableHandle _,(Syntax.TableQuery _ as t),_) -> Some t
    | _ -> None
        
  let lift_nonrecord_compn : RewriteSyntax.rewriter = function
      For(List_of(elem, data_body), var, src, data_for) 
        when sqlable_primtype(node_datatype elem)
          -> 
            Debug.print("lifting non-record in comp'n body");

            let ty = Types.concrete_type(node_datatype elem) in
            let recd_ty = Types.make_record_type[("a", ty)] in
            let field_name = "a" in
            let elem_record = 
              set_node_datatype(Record_intro(StringMap.from_alist
                                               [(field_name, elem)], 
                                             None, data_body), recd_ty) in
            let proj = Abstr(["x"], Project(Variable("x", data_for), field_name,
                                            data_for), 
                             data_for) in
            let result = Apply(Variable("map", data_for),
                               [proj;
                                For(List_of(elem_record, 
                                            data_body),
                                    var, src, data_for)], data_for) in
              Debug.print(" to: " ^
                            string_of_expression(result));
              Some result
    | _ -> None

  let normalize : Syntax.expression -> Syntax.expression =
    let rules = [
      NormalizeProjections.normalize_projections;
      Syntax.RewriteSyntax.bottomup lift_nonrecord_compn;
      Simplify.simplify;
      Syntax.RewriteSyntax.topdown simplify_table_query;
      Syntax.RewriteSyntax.topdown remove_unused_variables;
    ] in
      fun e -> fromOption (Variable("FUD", Syntax.no_expr_data)) (Syntax.RewriteSyntax.all rules e)
        
  let normalize e = 
    let r = normalize e in
      (if !trace_normalize then
         prerr_endline ("normalize output : " ^ 
         Syntax.Show_stripped_expression.show (Syntax.strip_data r)));
      r
end


module Compile:
sig
  val compile : Syntax.expression -> expr option
end =
struct
  type var = string deriving (Show)

  module Env :
  sig
    (* map table/field pairs to variable names (essentially an
       implementation of the "normalize" rule) *)
    type t deriving (Show)
    type field = {var : var; label : string} deriving (Show)
    val empty : t
    val bindf : field -> var -> t -> t
    val lookupf : field -> t -> var option
    val bindv : var -> baseexpr -> t -> t
    val lookupv : var -> t -> baseexpr option
  end =
  struct
    type field = {var : var; label : string} deriving (Show)
    type key =  field deriving (Show)
    module C = (struct type t = key let compare = Pervasives.compare module Show_t = Show_key end)
    module Mfield = Map.Make(C) 
    module Mvar = StringMap
    type t = var Mfield.t * baseexpr Mvar.t
    module Show_t : Show.Show with type a = t
      = Show.Show_2
      (Mfield.Show_t(Show_var))
      (Show_stringmap(Show_baseexpr))
     let empty = Mfield.empty, Mvar.empty
     let bindf f v (m,n) = (Mfield.add f v m, n)
     let lookupf f (m,_) = Mfield.lookup f m
     let bindv k v (m,n) = (m, Mvar.add k v n)
     let lookupv f (_,n) = Mvar.lookup f n
  end

  open Syntax

  exception Uncompilable of expression
  let uncompilable e = raise (Uncompilable e)

  let present_fields fields = 
    StringMap.fold (fun name spec fields ->
                      match spec with 
                        | `Present t -> (name,t)::fields
                        | `Absent -> assert false) fields []

      
  let counter = ref 0 

  let debug msg = 
    if !debugging then prerr_endline (Lazy.force msg)
    else ()

  let trycompile =
      fun msg f env e -> 
        let i = !counter in
          incr counter;
          try 
            debug(lazy(string_of_int i ^ " " ^ msg ^ 
                         " attempting to compile : " ^ 
                         Syntax.Show_stripped_expression.show 
                         (Syntax.strip_data e)));
            let r = f env e in
              debug(lazy(string_of_int i ^ " success!"));
              r
          with (Uncompilable _) as ex -> 
            debug(lazy(string_of_int i ^ " " ^ msg ^ " failure!"));
            raise ex

  let compileRegex (e : 'a Syntax.expression') : like_expr = 
    let quote = Str.global_replace (Str.regexp_string "%") "\\%" in
    let rec unlist = function
      | List_of (x,_) -> [x]
      | Concat (l, r,_) -> unlist l @ unlist r 
      | Nil _ -> [] 
      | _ -> assert false in
    let rec compile : 'a Syntax.expression' -> like_expr = function
      | Variant_injection ("Repeat", Record_intro (pair, None,_), _) as e -> 
          (match Prepare.unpack pair with 
             | Some [Variant_injection ("Star", _, _); 
                     Variant_injection ("Any", _, _)] -> `Percent
             | _ -> uncompilable e)
      | Variant_injection ("Simply", Constant(String s, _), _) -> `Str (quote s)
      | Variant_injection ("Quote", (* Variables are always quoted in the
                                       inner representation. *)
          Variant_injection ("Simply", 
            Syntax.Variable (name, _), _), _) -> `Var name
      | Variant_injection ("Seq", rs, _) -> `Seq (map compile (unlist rs))
          (* FIXME: we are not properly handling the ABSENCE of anchors *)
      | Variant_injection ("StartAnchor", _, _) -> `Seq []
      | Variant_injection ("EndAnchor", _, _) -> `Seq []
      | e -> uncompilable e
    in compile e


  let rec compileB (env : Env.t) : expression -> baseexpr = function
    | Apply (Variable ("not", _), [b], _)  -> 
        `Not ((trycompile "B" compileB) env b)
    | Apply (Variable ("tilde", _), [b; regex], _) ->
        `Like ((trycompile "B" compileB) env b,
               compileRegex regex)
    | Variable (v, _)                    -> 
        begin match Env.lookupv v env with
          | Some b -> b
          | None   -> `Var v
        end
    | Project (Variable (v,_), label, _) as e -> 
        begin match Env.lookupf {Env.var=v;Env.label=label} env with
          | Some v -> `Var v
          | None   -> 
              debug(lazy("env : " ^ Env.Show_t.show env));
              uncompilable e
        end
    | Record_intro (fields, None, _) ->
        `Rec (StringMap.fold (fun label expr output ->
                                (label, (trycompile "B" compileB) env expr)::output) fields [])
    | Constant(Boolean true, _)      -> `True
    | Constant(Boolean false, _)     -> `False
    | Constant(Integer n, _)         -> `N n
    | Constant(String  s, _)         -> `Str s
    | Let (v, b, s, _) -> `Let (v, (trycompile "B" compileB) env b, (trycompile "B" compileB) env s)
    | Comparison (l, c, r,_) -> 
        `Op ((c:>op), (trycompile "B" compileB) env l, (trycompile "B" compileB) env r)
    | Condition (c,s,Constant(Boolean false, _),_) ->
        `Op (`And, (trycompile "B" compileB) env c, (trycompile "B" compileB) env s)
    | Condition (c, Constant(Boolean true, _), b, _) ->
        `Op (`Or, (trycompile "B" compileB) env c, (trycompile "B" compileB) env b)
    | e                      -> uncompilable e

  let rec compileS env : expression -> simpleExpr = function
    | For (body, var, src, _) as e -> 
        debug(lazy("attempting to compile comprehension!"));
        begin match Types.concrete_type (node_datatype src) with
          | `Application ("List",[r]) ->
              begin match Types.concrete_type r with 
                | `Record (row,_) ->
                    let fields = present_fields row in
                    let fieldinfo = 
                      List.map (fun (label, _) -> 
                                  (
                                    (let l = {Env.var = var; Env.label = label} in
                                       debug(lazy("binding : " ^ Env.Show_field.show l));
                                       l), gensym())) fields in
                    let env' = fold_right (uncurry Env.bindf) fieldinfo env in
                    let env' = Env.bindv var (`Rec (List.map (fun (f,v) -> f.Env.label, `Var v) fieldinfo)) env' in
                      begin
                        let s = (trycompile "S" compileS) env src
                        and t = (trycompile "S" compileS) env' body in
                          debug(lazy "SUCCESSFULLY compiled comprehension");
                          `For((List.map (fun (field, fname) -> 
                                            (field.Env.label, fname))
                                  fieldinfo), s, t)
                      end
                | _ -> uncompilable e
              end
          | t -> 
              debug(lazy ("comprehension source: wrong type : " ^
                            Types.Show_datatype.show t));
              uncompilable e
        end

    | TableQuery ([table_alias, Variable(th_var, _)], q, `T (_,ty,_)) as e ->
        debug(lazy("attempting to compile table query!"));
        begin match q, Types.concrete_type ty with 
          | {cols = _;
	     tabs = [_]; (* single table *)
	     cond = [`True];
	     most = Inf;
	     from = Int 0;
             sort = []},
            ty ->
              let fields = (match ty with
                              | `Application ("List", [`Record (fields,_)]) -> fields
                              | s -> failwith ("Unexpected table type in:" ^
                                               Types.Show_datatype.show s)) in
                `Table(present_fields fields, th_var, table_alias)
          | _ -> 
              debug(lazy "could not compile table query");
              uncompilable e
        end
    | TableQuery _ -> failwith "Unexpected form of TableQuery"
    | Condition (c, t, Nil _, _) ->
        `Where ((trycompile "B" compileB) env c, (trycompile "S" compileS) env t)
    | Condition (c, Nil _, t, _) ->
        `Where (`Not ((trycompile "B" compileB) env c), (trycompile "S" compileS) env t)
    | Let (v, b, s, _) -> `Let (v, (trycompile "B" compileB) env b, (trycompile "S" compileS) env s)
    | List_of (b, _) as e -> 
        if (sqlable_record(Types.concrete_type (node_datatype b))) then 
          match (trycompile "B" compileB) env b with
            | `Rec _ as b -> `Return b
            | _ -> uncompilable e
        else (debug(lazy("List_of body was not a tuple, it was: " ^ 
                           Types.Show_datatype.show(Types.concrete_type(node_datatype b))));
              uncompilable e)
    | e -> uncompilable e

  let rec compileE env : expression -> expr = function
    | Apply (Variable ("take"|"drop" as f,_), [arg1; arg2], _) as e -> 
        begin match (arg1, arg2), f with
          | (Constant (Integer n,_), e), "take" -> `Take (n, (trycompile "E" compileE) env e)
          | (Constant (Integer n,_), e), "drop" -> `Drop (n, (trycompile "E" compileE) env e)
          | _ -> uncompilable e end
    | e -> ((trycompile "S" compileS) env e :> expr)

  let compile e = 
    try Some ((trycompile "E" compileE) Env.empty e)
    with Uncompilable _ -> None
end


module Eval:
sig
  val eval : expr -> sqlQuery
end =
struct
  module Env:
  sig
    type t
    val empty : t
(*    val bindf : name -> field -> t -> t*)
    val binde : name -> sqlexpr -> t -> t
    val lookup : name -> t -> [`F of field | `E of sqlexpr | `Absent ]
  end =
  struct
    type t = [`F of field | `E of sqlexpr | `Absent ] stringmap
    let empty = StringMap.empty
    let bindf name f = StringMap.add name (`F f)
    let binde name e = StringMap.add name (`E e)
    let lookup name env = 
      match StringMap.lookup name env with
        | Some s -> s
        | None -> `Absent
  end

  module Auxiliary =
  struct
    let minimum l r = 
      match l, r with
        | Inf, n
        | n, Inf -> n
        | I n, I m -> I (min_num n m)
            
    (* Find the output column with a particular name *)
    let find_output_column (q : sqlQuery) name : sqlexpr = rassoc name q.cols

    let fresh_table_name = Utility.gensym ~prefix:"Table"
  end

  let rec subst_tabnames substs : sqlexpr -> sqlexpr =
    function 
      | #literal as l -> l
      |`V _ as v -> v
      |`Rec bindings -> `Rec (map (fun (l,r) -> (l, subst_tabnames substs r)) bindings)
      |`Op (op, l, r) -> `Op (op, subst_tabnames substs l, subst_tabnames substs r)
      |`Not e -> `Not (subst_tabnames substs e)
      |`Like (e,l) -> `Like (subst_tabnames substs e,l)
      |`F ({table=t} as f) when mem_assoc t substs ->
         `F {f with table=assoc t substs}
      |`F _ as f -> f

  let rec evalb env : baseexpr -> sqlexpr = function
    | `Op (op, l, r) -> `Op (op, evalb env l, evalb env r)
    | `Not b -> `Not (evalb env b)
    | `Rec fields -> `Rec (List.map (fun (k,v) -> (k,evalb env v)) fields)
    | `Var v -> begin match Env.lookup v env with
                      | `F f -> `F f
                      | `E e -> e
                      | `Absent -> `V v end
    | `Like (l, r) -> `Like (evalb env l, r)
    | `Let (v, b, e) -> evalb (Env.binde v (evalb env b) env) e
    | #literal as l -> (l :> sqlexpr)

  let rec evalS env : simpleExpr -> sqlQuery = function
    | `For (fields, s, t) -> 
        let q1 = evalS env s in
        let tab_map = map (function (`TableVar(t,oldas)) -> (t, oldas, Auxiliary.fresh_table_name ())
                             | _ ->assert false) q1.tabs in
        let tab_alias_map = map (fun (_,oldas,newas) -> (oldas,newas)) tab_map in
        let q1 = {q1 with cols = map (fun (expr, alias) -> (subst_tabnames tab_alias_map expr, alias)) q1.cols
                        ; tabs = map (fun (t,_,newas) -> `TableVar(t, newas)) tab_map
                        ; cond = map (subst_tabnames tab_alias_map) q1.cond} in
        (* (substitute expressions: see JOIN rule) *)
        let env' = (fold_right
                      (fun (l,v) -> Env.binde v (Auxiliary.find_output_column q1 l))
                      fields
                      env) in
        let q2 = evalS env' t in
          { cols = q2.cols;
            tabs = q1.tabs @ q2.tabs;
            cond = q1.cond @ q2.cond;
            most = (match q1.most, q2.most with Inf, Inf -> Inf | _ -> assert false);
            from = (match q1.from, q2.from with Int 0, Int 0 -> Int 0 | _ -> assert false);
            sort = []}
    | `Let (v, b, e) -> evalS (Env.binde v (evalb env b) env) e
    | `Where (b, s) -> 
        let cond = evalb env b
        and q = evalS env s in
          {q with cond = cond :: q.cond}
    | `Table (cols, table_var, table_alias) ->
        { cols = map (fun (col,ty) ->
                        (`F {table=table_alias; column=col; ty=ty}, col)) cols;
          tabs = [`TableVar(table_var, table_alias)];
          cond = [];
          most = Inf;
          from = num_of_int 0;
          sort = []}
    | `Return b ->
        begin match evalb env b with
          | `Rec fields -> 
              { cols = List.map (fun (alias,expr)->(expr,alias)) fields;
                tabs = [];
                cond = [];
                most = Inf;
                from = num_of_int 0;
                sort = []}
          | _ -> assert false (* FIXME *) end

  let rec evalE : expr -> sqlQuery = function
    | `Take (i,e) ->
        let q = evalE e in
          {q with most = Auxiliary.minimum q.most (I i)}
    | `Drop (i,e) -> 
        let q = evalE e in
          {q with from = add_num q.from i; 
             most = match q.most with 
               | Inf -> Inf
               | I m -> I (sub_num m i)}
    | #simpleExpr as t -> evalS Env.empty t

  let eval = evalE
end

(* For now, we have to reconstruct the pairings of the 
   aliases with the variables that hold the actual table 
   value *)
let table_names q =
  map (function
           (`TableVar(x, alias)) -> 
             alias, (Syntax.Variable(x, (Syntax.no_expr_data)))
         | `TableName _ -> assert false) q.tabs

let underscore_numeric_names cols =
  map (fun (expr, col) ->
         if is_numeric col then
           (col, "_" ^ col) 
         else
           (col, col) )
    cols

let rename_cols renamings q =
  {q with cols = 
      map (fun (e, name) ->
             (e, try assoc name renamings with Not_found -> name))
        q.cols}
    
(** Inject a query as a Links expression. 
    Also handles the renaming of numeric column names. Renaming is done
    here because it involves both an operation on the query and also a
    wrapping expression that projects out the needed data at the end. *)
let injectQuery q data =
  let names = table_names q in
  let needs_renaming = exists (snd ->- is_numeric) q.cols in
  let renamings = underscore_numeric_names q.cols in
  let renamer =
    let fields = map
      (fun (expr, name) ->
         (name,
          (Syntax.Project(Syntax.Variable("row", Syntax.no_expr_data),
                          (try List.assoc name renamings
                           with Not_found -> name),
                          Syntax.no_expr_data))))
      q.cols in
    let record = Syntax.record_expr fields Syntax.no_expr_data in
      Syntax.Abstr(["row"],
                   record,
                   Syntax.no_expr_data)
  in
  let q = if needs_renaming then rename_cols renamings q else q in
    if needs_renaming then
      Syntax.Apply(Syntax.Variable("map", Syntax.no_expr_data),
                   [renamer;
                    Syntax.TableQuery(names, q, data)],
                   Syntax.no_expr_data)
    else
      Syntax.TableQuery(names, q, data)
      
let sql_compile_prepared (expr : Syntax.expression) : Syntax.expression option =
  let expr_data = Syntax.expression_data expr in
  match Compile.compile expr with
      None -> None
    | Some qexpr ->
        let evaled = Eval.eval qexpr in
          (* a bit hackish; compile is too aggressive, making queries
             out of simple expressions. an easy heuristic is that if no
             tables are involved, it's not really a query: *)
          if evaled.tabs == [] then None else
            Some (injectQuery evaled expr_data)
                                                   
let sql_compile (expr : Syntax.expression) : Syntax.expression option =
  let expr = Prepare.normalize expr in
    Syntax.RewriteSyntax.maxonce_td sql_compile_prepared expr

