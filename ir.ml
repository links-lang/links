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

  | `ApplyPrim of (value * value list)

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
  | `Alias of (tyname * tyvar list * Types.datatype) ]
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
  
module MapTy =
struct
  open Types

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

  class maptyenv ((tenv, alias_env) as tyenv : typing_environment) =
  object ((o : 'self_type))
    val tyenv = tyenv
    val tenv = tenv
    val alias_env = alias_env

    method lookup_type : var -> datatype = fun var ->
      Env.lookup tenv var
        
    method constant : constant -> (constant * datatype) = fun c ->
      match c with
        | Syntax.Boolean _ -> c, bool_type
        | Syntax.Integer _ -> c, int_type
        | Syntax.Char _ -> c, char_type
        | Syntax.String _ -> c, string_type
        | Syntax.Float _ -> c, float_type

    method option :
      'a.
      ('self_type -> 'a -> ('a * datatype)) ->
      'a option -> 'a option * datatype option =
      fun f -> opt_split -<- (opt_map (f o))
        
    method list :
      'a.
      ('self_type -> 'a -> ('a * datatype)) ->
      'a list -> 'a list * datatype list =
      fun f -> List.split -<- (List.map (f o))

    method name_map :
      'a.
      ('self_type -> 'a -> ('a * datatype)) ->
      'a name_map -> 'a name_map * datatype name_map =
      fun f -> StringMap.split -<- (StringMap.map (f o))
        
    method var : var -> (var * datatype) =
      fun var -> (var, o#lookup_type var)
        
    method value : value -> (value * datatype) =
      function
        | `Constant c -> let (c, t) = o#constant c in `Constant c, t
        | `Variable x -> let (x, t) = o#var x in `Variable x, t
        | `Extend (fields, base) ->
            let (fields, field_types) = o#name_map (fun o -> o#value) fields in
            let (base, base_type) = o#option (fun o -> o#value) base in

            let t =
              match base_type with
                | None -> make_record_type field_types
                | Some (`Record row) ->
                    `Record (extend_row field_types row)
                | Some _ -> assert false
            in
              `Extend (fields, base), t
        | `Project (name, v) ->
            (*             Debug.print ("project_e: " ^ Show_value.show (`Project (name, v))); *)
            let (v, vt) = o#value v in
(*               Debug.print ("project_vt: " ^ Types.string_of_datatype vt); *)
              `Project (name, v), deconstruct (project_type ~aenv:alias_env name) vt
        | `Erase (name, v) ->
(*             Debug.print ("erase_e: " ^ Show_value.show (`Erase (name, v))); *)
            let (v, vt) = o#value v in
            let t = deconstruct (erase_type ~aenv:alias_env name) vt in
(*               Debug.print ("erase_vt: " ^ Types.string_of_datatype vt); *)
(*               Debug.print ("erase_t: " ^ Types.string_of_datatype t); *)
              `Erase (name, v), t
        | `Inject (name, v) ->
            let (v, t) = o#value v in
              `Inject (name, v), inject_type name t
        | `XmlNode (tag, attributes, children) ->
            let (attributes, attribute_types) = o#name_map (fun o -> o#value) attributes in
            let (children, children_types) = o#list (fun o -> o#value) children in

              (*
                let _ = assert (StringMap.for_all (fun t -> t=string_type) attribute_types) in
                let _ = assert (List.for_all (fun t -> t=xml_type) children_types) in
              *)
              `XmlNode (tag, attributes, children), xml_type              
        | `ApplyPrim (f, args) ->
            let (f, ft) = o#value f in
            let (args, arg_types) = o#list (fun o -> o#value) args in
              (* TODO: check arg types match *)
              `ApplyPrim (f, args), deconstruct (return_type ~aenv:alias_env) ft
        | `Comparison (v, op, w) ->
            let v, _ = o#value v
            and w, _ = o#value w in
              `Comparison (v, op, w), bool_type
            (* TODO: get rid of comparison *)
        | `Coerce (v, t) ->
            let (v, vt) = o#value v in
            (* TODO: check that vt <: t *)
              `Coerce (v, t), t
        | `Abs v ->
            let (v, t) = o#value v in
              `Abs v, abs_type t
                                                         
    method tail_computation :
      tail_computation -> (tail_computation * datatype) =
      function
          (* TODO: type checking *)
        | `Return v ->
            let (v, t) = o#value v in
              `Return v, t
        | `Apply (f, args) ->
            let (f, ft) = o#value f in
            let (args, arg_types) = o#list (fun o -> o#value) args in
              (* TODO: check arg types match *)
(*               Debug.print ("apply: " ^ Show_tail_computation.show (`Apply (f, args))); *)
              `Apply (f, args), deconstruct (return_type ~aenv:alias_env) ft
        | `Special special ->
            let (special, t) = o#special special in
              `Special special, t

        | `Case (v, cases, default) ->
            let v, _ = o#value v in
            let cases, case_types =
              o#name_map
                (fun o (b, c) ->
                   let (b, o) = o#binder b in
                   let (c, t) = o#computation c in
                     (b, c), t) cases in
            let default, default_type =
              o#option (fun o (b, c) ->
                          let (b, o) = o#binder b in
                          let (c, t) = o#computation c in
                            (b, c), t) default in
            let t =
              if not (StringMap.is_empty case_types) then
                (StringMap.to_alist ->- List.hd ->- snd) case_types
              else
                val_of default_type
            in
              `Case (v, cases, default), t
        | `If (v, left, right) ->
            let v, _ = o#value v in
            let left, t = o#computation left in
            let right, _ = o#computation right in
              `If (v, left, right), t
                 
    method special : special -> (special * datatype) =
      function
        | `App (v, w) ->
            let v, vt = o#value v in
            let w, wt = o#value w in
              `App (v, w), app_type vt wt
        | `Wrong t -> `Wrong t, t
        | `Database v ->
            let v, _ = o#value v in
              `Database v, `Primitive `DB
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
              `Query q, t
        | `Table (db, table_name, (rt, wt)) ->
            let db, _ = o#value db in
            let table_name, _ = o#value table_name in
              `Table (db, table_name, (rt, wt)), `Table (rt, wt)
        | `CallCC v ->
            let v, t = o#value v in
              `CallCC v, deconstruct (return_type ~aenv:alias_env) t
      
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

    method computation : computation -> (computation * datatype) =
      fun (bs, tc) ->
(*         Debug.print ("computation: " ^ Show_computation.show (bs, tc)); *)
        let bs, o = o#bindings bs in
        let tc, t = o#tail_computation tc in
          (bs, tc), t
                                                       
    method binding : binding -> (binding * 'self_type) =
      function
        | `Let (x, tc) ->
            let (xv, (xt, _, _) as x), o = o#binder x in
            let (tc, t) = o#tail_computation tc in
(*               Debug.print ("bound "^string_of_int(xv)^" of type "^string_of_datatype xt^ *)
(*                              " to expression of type "^string_of_datatype t); *)
              `Let (x, tc), o
        | `Fun (f, xs, body, location) ->
            let xs, body =
              let (xs, o) =
                List.fold_right
                  (fun x (xs, o) ->
                     let x, o = o#binder x in
                       (x::xs, o))
                  xs
                  ([], o) in
              let (body, _) = o#computation body in
                xs, body in
            let (f, o) = o#binder f in
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

            let defs =
              List.map
                (fun (f, xs, body, location) ->
                  let (xs, o) =
                    List.fold_right
                      (fun x (xs, o) ->
                         let (x, o) = o#binder x in
                           (x::xs, o))
                      xs
                      ([], o) in
                  let (body, _) = o#computation body in
                    (f, xs, body, location))
                defs
            in
              `Rec defs, o
        | `Alien (x, language) ->
            let x, o = o#binder x in
              `Alien (x, language), o
        | `Alias (name, quantifiers, t) ->
(*             Debug.print ("registering alias: "^name); *)
            let alias_env = register_alias (name, quantifiers, t) alias_env in
(*              Debug.print ("updated alias env: "^Types.Show_alias_environment.show alias_env);*)
              `Alias (name, quantifiers, t), {< alias_env=alias_env;  tyenv=(tenv, alias_env) >}

    method binder : binder -> (binder * 'self_type) =
      fun (var, info) ->
        let tenv = Env.bind tenv (var, info_type info) in
          (var, info), {< tenv=tenv; tyenv=(tenv, alias_env) >}
  end
end


module InlineInlined =
struct
  (*
    A rather ad-hoc mostly-harmless let-inliner
    
    This inlines atoms, projections and injections. It isn't clear
    that it's always a good idea to inline projections and
    injections, but it's unlikely to cost much for small examples
    and it does lead to faster and more readable javascript.
  *)
  
  let rec is_inlineable_value =
    function
      | v when is_atom v -> true
      | `Project (_, v)
      | `Inject (_, v) -> is_inlineable_value v
      | _ -> false

  (* 
     NOTE:
     
     Most of this is just boilerplate. It would be nice if we could
     generate it automatically somehow...
  *)

  let rec value env v : value =
    let iv = value env in
      match v with
        | `Variable var when IntMap.mem var env -> IntMap.find var env
        | `Constant _ | `Variable _ -> v
        | `Extend (vmap, vopt) -> `Extend (StringMap.map iv vmap, opt_map iv vopt)
        | `Project (name, v) -> `Project (name, iv v)
        | `Erase (name, v) -> `Erase (name, iv v)
        | `Inject (name, v) -> `Inject (name, iv v)
        | `XmlNode (name, vmap, vs) -> `XmlNode (name, StringMap.map iv vmap, List.map iv vs)
        | `ApplyPrim (v, vs) -> `ApplyPrim (iv v, List.map iv vs)
        | `Comparison (v, c, w) -> `Comparison (iv v, c, iv w)
        | `Coerce (v, t) -> `Coerce (iv v, t)
        | `Abs v -> `Abs (iv v)

  and tail_computation env tc : tail_computation =
    let iv = value env in
    let ic = computation env in
      match tc with
        | `Return v -> `Return (iv v)
        | `Apply (v, vs) -> `Apply (iv v, List.map iv vs)
        | `Special s -> `Special (special env s)
        | `Case (v, cases, default) ->
            `Case (iv v, StringMap.map (fun (x, c) -> (x, ic c)) cases, opt_map (fun (x, c) -> (x, ic c)) default)
        | `If (v, c1, c2) ->
            `If (iv v, ic c1, ic c2)

  and special env s =
    let iv = value env in
      match s with
        | `App (v, w) -> `App (iv v, iv w)
        | `Wrong t -> `Wrong t
        | `Database v -> `Database (iv v)
        | `Query q -> `Query q
            (* WARNING: perhaps we need to look inside the query *)
        | `Table (v, w, t) -> `Table (iv v, iv w, t)
        | `CallCC v -> `CallCC (iv v)
    
  and binding env =
    function
      | `Let (x, tc) -> `Let (x, tail_computation env tc)
      | `Fun (f, xs, c, l) -> `Fun (f, xs, computation env c, l)
      | `Rec (defs) -> `Rec (List.map (fun (f, xs, c, l) -> (f, xs, computation env c, l)) defs)
      | (`Alien _ | `Alias _) as b -> b
(*      | `For (x, v) -> `For (x, value env v) *)

  and bindings env =
    function
      | `Let ((x, (_, _, `Local)), `Return v) :: bs when is_inlineable_value v ->
          bindings (IntMap.add x (value env v) env) bs
      | b :: bs ->
          let b = binding env b in
          let env, bs = bindings env bs in
            env, b :: bs
      | [] ->
          env, []
          
  and computation env (bs, tc)  : computation =
    let env, bs = bindings env bs in
      (bs, tail_computation env tc)

  let program typing_environment c =
(*     Debug.print ("initial alias env: "^Types.Show_alias_environment.show (snd typing_environment)); *)
    let o = new MapTy.maptyenv(typing_environment) in    
    let c', _ = o#computation c in
      computation (IntMap.empty) c
end


module InlineMapTy =
struct
  let rec is_inlineable_value =
    function
      | v when is_atom v -> true
      | `Project (_, v)
      | `Inject (_, v) -> is_inlineable_value v
      | _ -> false

  let inliner tyenv env =
  object (o)
    inherit MapTy.maptyenv(tyenv) as super

    val env = env

    method with_env env =
      {< env = env >}

    method value =
      function
        | `Variable var when IntMap.mem var env -> IntMap.find var env, o#lookup_type var
        | v -> super#value v

    method bindings =
      function
        | b :: bs ->
            let b, o = o#binding b in
              begin
                match b with
                  | `Let ((x, (_, _, `Local)), `Return v) when is_inlineable_value v ->
                      (o#with_env (IntMap.add x (fst (o#value v)) env))#bindings bs
                  | _ ->
                      let bs, o = o#bindings bs in
                        b :: bs, o
              end
        | [] -> [], o
  end

  let program typing_env p =
    fst ((inliner typing_env IntMap.empty)#computation p)
end


module Inline = InlineMapTy
(* module Inline = InlineInlined *)
