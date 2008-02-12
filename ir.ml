(*pp deriving *)
(** Monadic IR *)

open Utility

type scope = [ `Local | `Global ]
  deriving (Show)
(* term variables *)
type var = int
  deriving (Show)
type var_info = Types.datatype * string * scope
  deriving (Show)
type binder = var * var_info
  deriving (Show)

(* type variables *)
type tyvar = int
  deriving (Show)
type tyname = string
  deriving (Show)
(* type tybinder = tyvar * var_info *)

type name = string
  deriving (Show)
type 'a name_map = 'a Utility.stringmap
  deriving (Show)

type language = string
  deriving (Show)
(*
type constant =
  | Boolean of bool
  | Integer of Num.num
  | Char of char
  | String of string
  | Float of float
*)

type constant = Syntax.constant
  deriving (Show)

type location = Syntax.location
  deriving (Show)

type value =
  [ `Constant of constant
  | `Variable of var
  | `Extend of (value name_map * value option)
  | `Project of (name * value)
  | `Erase of (name * value)
  | `Inject of (name * value)

  | `XmlNode of (name * value name_map * value list)

  | `ApplyPure of (value * value list)

  (* should really be implemented as constants *)
  | `Comparison of (value * Syntaxutils.comparison * value)

  | `Coerce of (value * Types.datatype)
  | `Abs of value
  ]
and tail_computation =
  [ `Return of (value)
  | `Apply of (value * value list)

  | `Special of special

  | `Case of (value * (binder * computation) name_map * (binder * computation) option)
  | `If of (value * computation * computation)
  ]
and binding =
  [ `Let of (binder * tail_computation)
  | `Fun of (binder * binder list * computation * location)
  | `Rec of (binder * binder list * computation * location) list
  | `Alien of (binder * language)
  | `Alias of (tyname * tyvar list * Types.datatype)
  | `Module of (string * binding list option) ]
and special =
  [ `App of value * value
  | `Wrong of Types.datatype
  | `Database of value
  | `Query of SqlQuery.sqlQuery
  | `Table of (value * value * (Types.datatype * Types.datatype))
  | `CallCC of (value) ]
and computation = binding list * tail_computation
  deriving (Show)  

let rec is_atom =
  function
    | `Constant (Syntax.Boolean _)
    | `Constant (Syntax.Integer _)
    | `Constant (Syntax.Char _)
    | `Constant (Syntax.Float _)
    | `Variable _ -> true
    | `Erase (_, v)
    | `Coerce (v, _)
    | `Abs v -> is_atom v
    | _ -> false

type program = computation

let string_of_var = string_of_int

let string_of_value _ = "[VALUE]"
let string_of_tail_computation _ = "[TAIL_COMPUTATION]"
let string_of_binding _ = "[BINDING]"
let string_of_special _ = "[SPECIAL]"
let string_of_computation _ = "[COMPUTATION]"
let string_of_program _ = "[PROGRAM]"

module type TRANSFORM =
sig
  type environment = Types.datatype Env.Int.t
  type alias_environment = Types.alias_environment
  type typing_environment = environment * alias_environment

  class visitor : typing_environment ->
  object ('self_type)
    val tyenv : typing_environment
    val tenv : environment
    val alias_env : alias_environment

    method lookup_type : var -> Types.datatype
    method constant : constant -> (constant * Types.datatype * 'self_type)
    method option :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a option -> 'a option * Types.datatype option * 'self_type
    method list :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a list -> 'a list * Types.datatype list * 'self_type
    method name_map :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a name_map -> 'a name_map * Types.datatype name_map * 'self_type        
    method var : var -> (var * Types.datatype * 'self_type)
    method value : value -> (value * Types.datatype * 'self_type)
                                                
    method tail_computation :
      tail_computation -> (tail_computation * Types.datatype * 'self_type)
    method special : special -> (special * Types.datatype * 'self_type)      
    method bindings : binding list -> (binding list * 'self_type)
    method computation : computation -> (computation * Types.datatype * 'self_type)
    method binding : binding -> (binding * 'self_type)
    method binder : binder -> (binder * 'self_type)
  end  
end

(* Traversal with type reconstruction *)
(*
  Essentially this is a map-fold operation over the IR datatypes that also
  constructs the type as it goes along (using type annotations on
  binders).
*)
module Transform : TRANSFORM =
struct
  open Types
  open TypeUtils

  type environment = datatype Env.Int.t
  type alias_environment = Types.alias_environment
  type typing_environment = environment * alias_environment

  let info_type (t, _, _) = t

  (*
    HACK:

    The ir_ignore_type_errors setting tries to ignore any type errors
    during type deconstruction. It is only necessary because the
    optimisations on Syntax.expression don't maintain type correctness.

    In fact, now that it is possible to run type inference after
    optimisation (to restore type correctness), this setting is
    unnecessary.
  *)
  let ignore_type_errors = Settings.add_bool("ir_ignore_type_errors", false, `User)
  let deconstruct f t =
    if not (Settings.get_value ignore_type_errors) then
      f t
    else
      try
        f t
      with
          TypeDestructionError _ -> t

  module Env = Env.Int

  class visitor ((tenv, alias_env) as tyenv : typing_environment) =
  object ((o : 'self_type))
    val tyenv = tyenv
    val tenv = tenv
    val alias_env = alias_env

    method lookup_type : var -> datatype = fun var ->
      Env.lookup tenv var
        
    method constant : constant -> (constant * datatype * 'self_type) = fun c ->
      match c with
        | Syntax.Boolean _ -> c, bool_type, o
        | Syntax.Integer _ -> c, int_type, o
        | Syntax.Char _ -> c, char_type, o
        | Syntax.String _ -> c, string_type, o
        | Syntax.Float _ -> c, float_type, o

    method option :
      'a.
      ('self_type -> 'a -> ('a * datatype * 'self_type)) ->
      'a option -> 'a option * datatype option * 'self_type =
      fun f v ->
        match v with
          | None -> None, None, o
          | Some v ->
              let v, t, o = f o v in
                Some v, Some t, o
        
    method list :
      'a.
      ('self_type -> 'a -> ('a * datatype * 'self_type)) ->
      'a list -> 'a list * datatype list * 'self_type =
      fun f v ->
        let vs, ts, o =
          List.fold_left
            (fun (vs, ts, o) v ->
               let (v, t, o) = f o v in
                 v::vs, t::ts, o)
            ([], [], o)
            v
        in
          List.rev vs, List.rev ts, o
          
    method name_map :
      'a.
      ('self_type -> 'a -> ('a * datatype * 'self_type)) ->
      'a name_map -> 'a name_map * datatype name_map * 'self_type =
      fun f vmap ->
        StringMap.fold
          (fun name v (vmap, tmap, o) ->
             let (v, t, o) = f o v in
               (StringMap.add name v vmap,
                StringMap.add name t tmap,
                o))
          vmap
          (StringMap.empty, StringMap.empty, o)

    method var : var -> (var * datatype * 'self_type) =
      fun var -> (var, o#lookup_type var, o)
        
    method value : value -> (value * datatype * 'self_type) =
      function
        | `Constant c -> let (c, t, o) = o#constant c in `Constant c, t, o
        | `Variable x -> let (x, t, o) = o#var x in `Variable x, t, o
        | `Extend (fields, base) ->
            let (fields, field_types, o) = o#name_map (fun o -> o#value) fields in
            let (base, base_type, o) = o#option (fun o -> o#value) base in

            let t =
              match base_type with
                | None -> make_record_type field_types
                | Some t ->
                    begin
                      match TypeUtils.expand_aliases ~aenv:alias_env t with
                        | `Record row ->
                            `Record (extend_row field_types row)
                        | _ -> assert false
                    end
            in
              `Extend (fields, base), t, o
        | `Project (name, v) ->
            (*             Debug.print ("project_e: " ^ Show_value.show (`Project (name, v))); *)
            let (v, vt, o) = o#value v in
(*               Debug.print ("project_vt: " ^ Types.string_of_datatype vt); *)
              `Project (name, v), deconstruct (project_type ~aenv:alias_env name) vt, o
        | `Erase (name, v) ->
(*             Debug.print ("erase_e: " ^ Show_value.show (`Erase (name, v))); *)
            let (v, vt, o) = o#value v in
            let t = deconstruct (erase_type ~aenv:alias_env name) vt in
(*               Debug.print ("erase_vt: " ^ Types.string_of_datatype vt); *)
(*               Debug.print ("erase_t: " ^ Types.string_of_datatype t); *)
              `Erase (name, v), t, o
        | `Inject (name, v) ->
            let v, t, o = o#value v in
              `Inject (name, v), inject_type name t, o
        | `XmlNode (tag, attributes, children) ->
            let (attributes, attribute_types, o) = o#name_map (fun o -> o#value) attributes in
            let (children, children_types, o) = o#list (fun o -> o#value) children in

              (*
                let _ = assert (StringMap.for_all (fun t -> t=string_type) attribute_types) in
                let _ = assert (List.for_all (fun t -> t=xml_type) children_types) in
              *)
              `XmlNode (tag, attributes, children), xml_type, o            
        | `ApplyPure (f, args) ->
            let (f, ft, o) = o#value f in
            let (args, arg_types, o) = o#list (fun o -> o#value) args in
              (* TODO: check arg types match *)
              `ApplyPure (f, args), deconstruct (return_type ~aenv:alias_env) ft, o
        | `Comparison (v, op, w) ->
            let v, _, o = o#value v in
            let w, _, o = o#value w in
              `Comparison (v, op, w), bool_type, o
            (* TODO: get rid of comparison *)
        | `Coerce (v, t) ->
            let v, vt, o = o#value v in
            (* TODO: check that vt <: t *)
              `Coerce (v, t), t, o
        | `Abs v ->
            let v, t, o = o#value v in
              `Abs v, abs_type t, o

    method tail_computation :
      tail_computation -> (tail_computation * datatype * 'self_type) =
      function
          (* TODO: type checking *)
        | `Return v ->
            let v, t, o = o#value v in
              `Return v, t, o
        | `Apply (f, args) ->
            let f, ft, o = o#value f in
            let args, arg_types, o = o#list (fun o -> o#value) args in
              (* TODO: check arg types match *)
(*               Debug.print ("apply: " ^ Show_tail_computation.show (`Apply (f, args))); *)
              `Apply (f, args), deconstruct (return_type ~aenv:alias_env) ft, o
        | `Special special ->
            let special, t, o = o#special special in
              `Special special, t, o

        | `Case (v, cases, default) ->
            let v, _, o = o#value v in
            let cases, case_types, o =
              o#name_map
                (fun o (b, c) ->
                   let b, o = o#binder b in
                   let c, t, o = o#computation c in
                     (b, c), t, o) cases in
            let default, default_type, o =
              o#option (fun o (b, c) ->
                          let b, o = o#binder b in
                          let c, t, o = o#computation c in
                            (b, c), t, o) default in
            let t =
              if not (StringMap.is_empty case_types) then
                (StringMap.to_alist ->- List.hd ->- snd) case_types
              else
                val_of default_type
            in
              `Case (v, cases, default), t, o
        | `If (v, left, right) ->
            let v, _, o = o#value v in
            let left, t, o = o#computation left in
            let right, _, o = o#computation right in
              `If (v, left, right), t, o
                 
    method special : special -> (special * datatype * 'self_type) =
      function
        | `App (v, w) ->
            let v, vt, o = o#value v in
            let w, wt, o = o#value w in
              `App (v, w), app_type vt wt, o
        | `Wrong t -> `Wrong t, t, o
        | `Database v ->
            let v, _, o = o#value v in
              `Database v, `Primitive `DB, o
        | `Query q ->
            let row =
	      (List.fold_right
	         (fun (expr, alias) env ->
                    match expr with 
                      | `F field -> 
                          StringMap.add alias (`Present field.SqlQuery.ty) env
                      | _ -> assert(false) (* can't handle other kinds of expressions *))
	         q.SqlQuery.cols StringMap.empty, Unionfind.fresh `Closed) in
            let t =  `Application ("List", [`Record row]) in
              `Query q, t, o
        | `Table (db, table_name, (rt, wt)) ->
            let db, _, o = o#value db in
            let table_name, _, o = o#value table_name in
              `Table (db, table_name, (rt, wt)), `Table (rt, wt), o
        | `CallCC v ->
            let v, t, o = o#value v in
              `CallCC v, deconstruct (return_type ~aenv:alias_env) t, o
      
    method bindings : binding list -> (binding list * 'self_type) =
      fun bs ->
        let bs, o =
          List.fold_left
            (fun (bs, o) b ->
               let (b, o) = o#binding b in
                 (b::bs, o))
            ([], o)
            bs
        in
          List.rev bs, o

    method computation : computation -> (computation * datatype * 'self_type) =
      fun (bs, tc) ->
(*         Debug.print ("computation: " ^ Show_computation.show (bs, tc)); *)
        let bs, o = o#bindings bs in
        let tc, t, o = o#tail_computation tc in
          (bs, tc), t, o
                                                       
    method binding : binding -> (binding * 'self_type) =
      function
        | `Let (x, tc) ->
            let (xv, (xt, _, _) as x), o = o#binder x in
            let tc, t, o = o#tail_computation tc in
(*               Debug.print ("bound "^string_of_int(xv)^" of type "^string_of_datatype xt^ *)
(*                              " to expression of type "^string_of_datatype t); *)
              `Let (x, tc), o
        | `Fun (f, xs, body, location) ->
            let xs, body, o =
              let (xs, o) =
                List.fold_right
                  (fun x (xs, o) ->
                     let x, o = o#binder x in
                       (x::xs, o))
                  xs
                  ([], o) in
              let body, _, o = o#computation body in
                xs, body, o in
            let f, o = o#binder f in
              (* TODO: check that xs and body match up with f *)
              `Fun (f, xs, body, location), o
        | `Rec defs ->
            let fs, o =
              List.fold_right
                (fun (f, _, _, _) (fs, o) ->
                   let f, o = o#binder f in
                     (f::fs, o))
                defs
                ([], o) in

            let defs, o =
              List.fold_left
                (fun (defs, o) (f, xs, body, location) ->
                   let xs, o =
                     List.fold_right
                       (fun x (xs, o) ->
                          let (x, o) = o#binder x in
                            (x::xs, o))
                       xs
                       ([], o) in
                  let body, _, o = o#computation body in
                    (f, xs, body, location)::defs, o)
                ([], o)
                defs in
            let defs = List.rev defs in
              `Rec defs, o
        | `Alien (x, language) ->
            let x, o = o#binder x in
              `Alien (x, language), o
        | `Alias (name, quantifiers, t) ->
(*             Debug.print ("registering alias: "^name);*)
            let alias_env = register_alias (name, quantifiers, t) alias_env in
(*              Debug.print ("updated alias env: "^Types.Show_alias_environment.show alias_env);*)
              `Alias (name, quantifiers, t), {< alias_env=alias_env;  tyenv=(tenv, alias_env) >}
        | `Module (name, defs) ->
            let defs, o =
              match defs with
                | None -> None, o
                | Some defs ->
                    let defs, o = o#bindings defs
                    in
                      Some defs, o
            in
              `Module (name, defs), o

    method binder : binder -> (binder * 'self_type) =
      fun (var, info) ->
(*        Debug.print ("var: "^string_of_int var^", type: "^(Types.string_of_datatype (info_type info)));*)
        let tenv = Env.bind tenv (var, info_type info) in
          (var, info), {< tenv=tenv; tyenv=(tenv, alias_env) >}
  end
end

module Inline =
struct
  let rec is_inlineable_value =
    function
      | v when is_atom v -> true
      | `Project (_, v)
      | `Inject (_, v) -> is_inlineable_value v
      | _ -> false

  let inliner tyenv env =
  object (o)
    inherit Transform.visitor(tyenv) as super

    val env = env

    method with_env env =
      {< env = env >}

    method value =
      function
        | `Variable var when IntMap.mem var env -> IntMap.find var env, o#lookup_type var, o
        | v -> super#value v

    method bindings =
      function
        | b :: bs ->
            let b, o = o#binding b in
              begin
                match b with
                  | `Let ((x, (_, _, `Local)), `Return v) when is_inlineable_value v ->
                      (o#with_env (IntMap.add x (fst3 (o#value v)) env))#bindings bs
                  | _ ->
                      let bs, o = o#bindings bs in
                        b :: bs, o
              end
        | [] -> [], o
  end

  let program typing_env p =
(*    Debug.print (Show_computation.show p);*)
    fst3 ((inliner typing_env IntMap.empty)#computation p)
end

(*
  Eliminate dead functions and value bindings.

  Currently this is very basic. It only does one pass, and it only
  eliminates variables if they are never used anywhere.

  A much more effective approach is to use one of Appel and Jim's
  algorithms described in `Shrinking lambda reductions in linear
  time'.

  They describe three algorithms. All of them eliminate all dead
  variables (as well as inlining linear variables, though that aspect
  is neither here nor there really).

  The naive algorithm gathers a census of variable counts, uses it to
  perform inlining, and is applied repeatedly until there are no dead
  variables left.

  The improved algorithm does the same, but updates the census as it
  goes along (e.g. whenever it deletes a function it passes over the
  body of the function and adjusts the census to take account of any
  uses of variables that have just been deleted).

  Both the naive algorithm and the improved algorithm are quadratic in
  the worst case, though the improved algorithm works quite well in
  practice. The improved algorithm is used in SML/NJ and MLton, and it
  used to be used in SML.NET. Appel and Jim suggest just bounding the
  number of times the improved algorithm is iterated rather than
  trying to run it exhaustively. In all but pathological cases this
  gets rid of most dead functions.

  The graphical algorithm depends on a graphical representation of
  terms (connecting definitions to uses of variables). It takes linear
  time and is the algorithm now used in SML.NET. It is extremely fast
  in practice and eliminates all dead variables in one
  pass. Unfortunately our terms are represented as trees, so we cannot
  use this algorithm here.
*)
module ElimDeadDefs =
struct
  let counter tyenv =
  object (o)
    inherit Transform.visitor(tyenv) as super
      
    val env = IntMap.empty
      
    method with_env env =
      {< env = env >}

    method init (x, (_, name, _)) =
      (*      Debug.print ("init "^name^" as: "^string_of_int x);*)
      o#with_env (IntMap.add x 0 env)

    method inc x =
(*      Debug.print ("use of: "^string_of_int x);*)
      o#with_env (IntMap.add x ((IntMap.find x env)+1) env)

    method var =
      fun x ->
        if IntMap.mem x env then
          x, o#lookup_type x, o#inc x
        else
          super#var x

    method super_binding = super#binding

    method binding b =
      match b with
        | `Let (x, `Return _) ->
            let b, o = super#binding b in
              b, o#init x
        | `Fun (f, _, _, _) ->
            let b, o = super#binding b in
              b, o#init f
        | `Rec defs ->
            let o =
              List.fold_left
                (fun o (f, _, _, _) -> o#init f)
                o
                defs
            in
              o#super_binding b
        | _ ->
            super#binding b

    method get_env () = env
  end

  let eliminator tyenv env =
  object (o)
    inherit Transform.visitor(tyenv) as super
      
    val env = env
      
    method is_dead (x, _) =
      IntMap.mem x env && (IntMap.find x env = 0)

    method bindings =
      function
        | b :: bs ->
            begin
              let b, o = o#binding b in
                match b with
                  | `Let ((_, (_, name, _)) as x, _) when o#is_dead x ->
(*                      Debug.print ("Eliminating let-val: "^name);*)
                      o#bindings bs
                  | `Fun ((_, (_, name, _)) as f, _, _, _) when o#is_dead f ->
(*                      Debug.print ("Eliminating non-rec fun: "^name);*)
                      o#bindings bs
                  | `Rec defs ->
                      let defs =
                        List.fold_left
                          (fun defs (((_, (_, name, _)) as f, _, _, _) as def) ->
                             if o#is_dead f then
                               begin
(*                                 Debug.print ("Eliminating rec fun: "^name);*)
                                 defs
                               end
                             else def :: defs)
                          []
                          defs in
                      let defs = List.rev defs in
                        begin
                          match defs with
                            | [] -> o#bindings bs
                            | defs ->
                                let bs, o = o#bindings bs in
                                  `Rec defs :: bs, o
                        end                              
                  | _ ->
                      let bs, o = o#bindings bs in
                        b :: bs, o
            end
        | [] -> [], o
  end

  let count tyenv p =
    let _, _, o = (counter tyenv)#computation p in
      o#get_env()

  let program tyenv p =
    let env = count tyenv p in
(*      Debug.print ("before elim dead defs: " ^ Show_computation.show p);*)
    let p, _, _ = (eliminator tyenv env)#computation p in
(*      Debug.print ("after elim dead defs: " ^ Show_computation.show p);*)
      p
end
