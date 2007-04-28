(*pp deriving *)

open Utility
open List
open Num

let debugging = ref false

type name = string deriving (Show)
type label = name deriving (Show)
type op = [Syntax.comparison| `And | `Or]deriving (Show)
type pat = (label * name) list
type literal = [`True | `False | `Str of string | `N of num]deriving (Show)
type like_expr = [
|`Percent
| `Seq of like_expr list
| `Str of string
| `Var of name ]deriving (Show)
type baseexpr = [
|`Op  of op * baseexpr * baseexpr
|`Let of name * baseexpr * baseexpr (* not in the paper, but an easy extension *)
|`Like of baseexpr * like_expr
|`Not of baseexpr
|`Rec of (label * baseexpr) list
|`Var of name
| literal] deriving (Show)
type simpleExpr = [
|`For    of pat * simpleExpr * simpleExpr
|`Where  of baseexpr * simpleExpr
|`Let    of name * baseexpr * simpleExpr
|`Table  of (name * Types.datatype) list * string
|`Return of baseexpr]
type expr = [
|`Take of num * expr
|`Drop of num * expr
| simpleExpr]
type field = {table:name;column:name} deriving (Show)
type sqlexpr = [
| literal
|`Rec of (name * sqlexpr) list
|`Op  of op * sqlexpr * sqlexpr
|`Not of sqlexpr
|`Like of sqlexpr * like_expr
|`F   of field
|`V   of name]
type ninf = I of num | Inf
type sqlQuery = {
  cols : (sqlexpr*name) list;
  tabs : (name * name) list;
  cond : sqlexpr list;
  most : ninf;
  from : num;
}

module Prepare =
struct
  open Syntax

  let unpack : 'a StringMap.t -> 'a list option = 
    let rec aux n input output = 
      if input = StringMap.empty then Some (List.rev output)
      else
        try 
          let item, rest = StringMapUtils.pop (string_of_int n) input in
            aux (n+1) rest (item::output)
        with Not_found -> None
    in fun map -> aux 1 map []

  let remove_unused_variables : RewriteSyntax.rewriter = function
    | Let (var, expr, body, _) when Optimiser.pure expr && not (mem var (freevars body))
        -> Some body
    | _ -> None




    (* This module will go away once we stop using Record_selection *)
  module NormalizeProjections : 
  sig
    val normalize_projections : RewriteSyntax.rewriter
  end =
  struct
    (* The purpose of this set of rewrite rules is to replace all
       occurrences of Record_selection with Project and Erase
       (minimising the number of times Erase occurs.) *)
    
    open RewriteSyntax
      
    (* eliminate all occurrences of Record_selection in favour of Project and Erase *)
    let convert_projections : rewriter = function
      | Record_selection (label, labelvar, etcvar, (Variable _ as v), body, data) ->
          Some (Let (labelvar, Project (v, label, data),
                     Let (etcvar, Erase (v, label, data),
                          body, data), data))
      | Record_selection (label, labelvar, etcvar, value, body, data) ->
          let name = gensym ~prefix:"recordvar" () in
            Some (Let (name, value, 
                       Let (labelvar, Project (Variable (name, data), label, data),
                            Let (etcvar, Erase (Variable (name, data), label, data),
                                 body, data), data), data))
      | _ -> None
          
    (* replace projection from an etc. var with projection from the
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
             
    (* Don't bind the result of a projection to a variable; just use it
       in place *)
    let inline_projections : rewriter = function
      | Let (var, (Project (Variable _, _, _) as p), body, _) ->
          Some (subst_fast var p body)
      | _ -> None
          
    (* check post conditions hold (not really a rewriter) *)
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
    | Syntax.Let (_, Syntax.TableHandle _, (Syntax.TableQuery _ as t),_) -> Some t
    | _ -> None
        
  let normalize : Syntax.expression -> Syntax.expression =
    let rules = [
      NormalizeProjections.normalize_projections;
      Simplify.simplify;
      Syntax.RewriteSyntax.topdown Optimiser.renaming;
      Syntax.RewriteSyntax.topdown Optimiser.sql_aslist;
      Syntax.RewriteSyntax.topdown simplify_table_query;
      Syntax.RewriteSyntax.topdown Optimiser.unused_variables;
    ] in
      fun e -> fromOption e (Syntax.RewriteSyntax.all rules e)
        
  let normalize e = 
    let r = normalize e in
      (if !trace_normalize then
         prerr_endline ("normalize output : " ^ Syntax.Show_stripped_expression.show (Syntax.strip_data e)));
      r
end

module Compile:
sig
  val compile : Syntax.expression -> expr option
end =
struct
  type var = string deriving (Show)

  module Env:
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
    module C = (struct type t = key let compare = Pervasives.compare end)
    module Mfield = Map.Make(C) 
    module Mvar = StringMap
    module Utils = MapUtils(Mfield)
    module Utils' = MapUtils(Mvar)
    type t = var Mfield.t * baseexpr Mvar.t
    module Show_t
      = Show.Show_2
      (Show.Show_map(C)
         (Show_key)(Show_var))
      (Show_stringmap(Show_baseexpr))
     let empty = Mfield.empty, Mvar.empty
     let bindf f v (m,n) = (Mfield.add f v m, n)
     let lookupf f (m,_) = Utils.lookup f m
     let bindv k v (m,n) = (m, Mvar.add k v n)
     let lookupv f (_,n) = Utils'.lookup f n
  end

  open Syntax

  exception Uncompilable of expression
  let uncompilable e = raise (Uncompilable e)

  let resolve_type = Types.concrete_type

  let present_fields fields = 
    StringMap.fold (fun name spec fields ->
                      match spec with 
                        | `Present t -> (name,t)::fields
                        | `Absent -> assert false) fields []

  
  let counter = ref 0 

  let debug msg = 
    if !debugging then prerr_endline msg
    else ()

  let trycompile =
      fun msg f env e -> 
        let i = !counter in
          incr counter;
          try 
            debug (string_of_int i ^ " " ^ msg ^ " attempting to compile : " ^ Syntax.Show_stripped_expression.show (Syntax.strip_data e));
            let r = f env e in
              debug (string_of_int i ^ " success!");
              r
          with (Uncompilable _) as ex -> 
            debug (string_of_int i ^ " " ^ msg ^ " failure!");
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
      | Variant_injection ("Simply", Syntax.Variable (name, _), _) -> `Var name
      | Variant_injection ("Seq", rs, _) -> `Seq (map compile (unlist rs))
      | e -> uncompilable e
    in compile e


  let rec compileB (env : Env.t) : expression -> baseexpr = function
    | Apply (Variable ("not", _), [b], _)  -> `Not ((trycompile "B" compileB) env b)
    | Apply (Variable ("~", _), [b; regex], _)  -> `Like ((trycompile "B" compileB) env b,
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
              debug ("env : " ^ Env.Show_t.show env);
              uncompilable e
        end
    | Record_intro (fields, None, _) ->
        `Rec (StringMap.fold (fun label expr output ->
                          (label, (trycompile "B" compileB) env expr)::output) fields [])
    | Constant(Boolean true, _)      -> `True
    | Constant(Boolean false, _)     -> `False
    | Constant(Integer n, _)         -> `N n
    | Constant(String  s, _)       -> `Str s
    | Let (v, b, s, _) -> `Let (v, (trycompile "B" compileB) env b, (trycompile "B" compileB) env s)
    | Comparison (l, c, r,_) -> 
        `Op ((c:>op), (trycompile "B" compileB) env l, (trycompile "B" compileB) env r)
    | Condition (c,s,Constant(Boolean false, _),_) ->
        `Op (`And, (trycompile "B" compileB) env c, (trycompile "B" compileB) env s)
    | Condition (c, Constant(Boolean true, _), b, _) ->
        `Op (`Or, (trycompile "B" compileB) env c, (trycompile "B" compileB) env b)
    | e                      -> uncompilable e

  let rec compileS env : expression -> simpleExpr = function
    | For (body, var, expr, _) as e -> 
        debug "attempting to compile comprehension!";
        begin match resolve_type (node_datatype expr) with
          | `Application ("List",[r]) ->
              begin match resolve_type r with 
                | `Record (row,_) ->
                    let fields = present_fields row in
                    let fieldinfo = 
                      List.map (fun (label, _) -> 
                                  (
                                    (let l = {Env.var = var; Env.label = label} in
                                      debug ("binding : " ^ Env.Show_field.show l);
                                      l), gensym())) fields in
                    let env' = fold_right (uncurry Env.bindf) fieldinfo env in
                    let env' = Env.bindv var (`Rec (List.map (fun (f,v) -> f.Env.label, `Var v) fieldinfo)) env' in
                      begin
                        let s = (trycompile "S" compileS) env expr
                        and t = (trycompile "S" compileS) env' body in
                          debug "successfully compiled comprehension";
                          `For ((List.map (fun (field, fname) -> (field.Env.label, fname)) fieldinfo), s, t)
                      end
                | t -> failwith ("comprehension source: wrong inner type : " ^Types.Show_datatype.show t);
              end
          | t -> 
              debug ("comprehension source: wrong type : " ^Types.Show_datatype.show t);
              uncompilable e
        end

    | TableQuery (_, q, `T (_,t,_)) as e ->
        debug "attempting to compile table query!";
        begin match q, resolve_type t with 
          | {Query.distinct_only = false;
	     Query.result_cols = _;
	     Query.tables = [(`TableVariable th_var, _)]; (* need a table name to do a proper translation *)
	     Query.condition = Query.Boolean true;
	     Query.sortings = [];
	     Query.max_rows = None;
	     Query.offset = Query.Integer (Int 0)},
            t ->
              let fields = (match t with
                              | `Application ("List", [`Record (fields,_)]) -> fields
                              | s -> failwith ("unexpected type:"^Types.Show_datatype.show s)) in
              (`Table (present_fields fields, th_var))
          | _ -> 
              debug "could not compile table query";
              uncompilable e
        end
    | Condition (c, t, Nil _, _) ->
        `Where ((trycompile "B" compileB) env c, (trycompile "S" compileS) env t)
    | Condition (c, Nil _, t, _) ->
        `Where (`Not ((trycompile "B" compileB) env c), (trycompile "S" compileS) env t)
    | Let (v, b, s, _) -> `Let (v, (trycompile "B" compileB) env b, (trycompile "S" compileS) env s)
    | List_of (b, _) -> `Return ((trycompile "B" compileB) env b)
    | e -> uncompilable e

  let rec compileE env : expression -> expr = function
    | Apply (Variable ("take"|"drop" as f,_), [arg1; arg2], _) as e -> 
        begin match (arg1, arg2), f with
          | (Constant (Integer n,_), e), "take" -> `Take (n, (trycompile "E" compileE) env e)
          | (Constant (Integer n,_), e), "drop" -> `Drop (n, (trycompile "E" compileE) env e)
          | _ -> uncompilable e end
    | e -> ((trycompile "S" compileS) env e :> expr)

  let compile e = 
    debug "NEW!";
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
      match StringMapUtils.lookup name env with
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
    
  let rec rename tabs : sqlexpr -> sqlexpr =
    function 
      | #literal as l -> l
      |`V _ as v -> v
      |`Rec bindings -> `Rec (map (fun (l,r) -> (l, rename tabs r)) bindings)
      |`Op (op, l, r) -> `Op (op, rename tabs l, rename tabs r)
      |`Not e -> `Not (rename tabs e)
      |`Like (e,l) -> `Like (rename tabs e,l)
      |`F ({table=t}as f) when mem_assoc t tabs ->
         `F {f with table=assoc t tabs}
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
        let tabs = map (fun (t,oldas) -> (t, oldas,Auxiliary.fresh_table_name ())) q1.tabs in
        let q1 = {q1 with tabs = map (fun (t,_,newas) -> t,newas) tabs
                    ; cond = map (rename (map (fun (_,oldas,newas) -> (oldas,newas)) tabs)) q1.cond} in
        (* (substitute expressions: see join-up rule) *)
        let env' = (fold_right
                      (fun (l,v) -> Env.binde v (Auxiliary.find_output_column q1 l))
                      fields
                      env) in
        let q2 = evalS env' t in
          { cols = q2.cols;
            tabs = q1.tabs @ q2.tabs;
            cond = q1.cond @ q2.cond;
            most = (match q1.most, q2.most with Inf, Inf -> Inf | _ -> assert false);
            from = (match q1.from, q2.from with Int 0, Int 0 -> Int 0 | _ -> assert false) }
    | `Let (v, b, e) -> evalS (Env.binde v (evalb env b) env) e
    | `Where (b, s) -> 
        let cond = evalb env b
        and q = evalS env s in
          {q with cond = cond :: q.cond}
    | `Table (cols, tablename) ->
        { cols = map (fun (col,_) -> (`F {table=tablename;column=col},col)) cols;
          tabs = [tablename,tablename];
          cond = [];
          most = Inf;
          from = num_of_int 0 }
    | `Return b ->
        begin match evalb env b with
          | `Rec fields -> 
              { cols = List.map (fun (x,y)->(y,x)) fields;
                tabs = [];
                cond = [];
                most = Inf;
                from = num_of_int 0 }
          | _ -> assert false (* fixme? *) end 

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

(* TODO
   1. sort (?)
   2. tests
   2. length (count), max, min, average
   3. projection (via typing trick).
   4. rearranging queries to maximise compilability.
*)
