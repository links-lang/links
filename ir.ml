(*pp deriving *)
(** Monadic IR *)

open Notfound

open Utility
open PP

type num = Num.num

type scope = Var.scope
  deriving (Show)
(* term variables *)
type var = Var.var
  deriving (Show, Eq, Hash, Typeable, Pickle, Dump)
type var_info = Var.var_info
  deriving (Show)
type binder = Var.binder
  deriving (Show)

(* type variables *)
type tyvar = Types.quantifier
  deriving (Show)
type tyarg = Types.type_arg
  deriving (Show)

type name = string
  deriving (Show)

type name_set = Utility.stringset
  deriving (Show)
type 'a name_map = 'a Utility.stringmap
  deriving (Show)

type language = string
  deriving (Show)

let var_of_binder (x, _) = x

type constant = Constant.constant
  deriving (Show)

type location = Sugartypes.location
  deriving (Show)

type value =
  [ `Constant of constant
  | `Variable of var
  | `Extend of (value name_map * value option)
  | `Project of (name * value)
  | `Erase of (name_set * value)
  | `Inject of (name * value * Types.datatype)

  | `TAbs of tyvar list * value
  | `TApp of value * tyarg list

  | `XmlNode of (name * value name_map * value list)
  | `ApplyPure of (value * value list)

  | `Coerce of (value * Types.datatype)
  ]
and tail_computation =
  [ `Return of (value)
  | `Apply of (value * value list)

  | `Special of special

  | `Case of (value * (binder * computation) name_map * (binder * computation) option)
  | `If of (value * computation * computation)
  ]
and binding =
  [ `Let of binder * (tyvar list * tail_computation)
  | `Fun of (binder * (tyvar list * binder list * computation) * location)
  | `Rec of (binder * (tyvar list * binder list * computation) * location) list
  | `Alien of (binder * language)
  | `Module of (string * binding list option) ]
and special =
  [ `Wrong of Types.datatype
  | `Database of value 
  | `Table of (value * value * (Types.datatype * Types.datatype * Types.datatype))
  | `Query of (value * value) option * computation * Types.datatype
  | `Update of (binder * value) * computation option * computation
  | `Delete of (binder * value) * computation option
  | `CallCC of (value) ]
and computation = binding list * tail_computation
  deriving (Show)  

let letm (b, tc) = `Let (b, ([], tc))
let letmv (b, v) = letm (b, `Return v)
(*let letv (b, v) = `Let (b, `Return v)*)

let rec is_atom =
  function
    | `Constant (`Bool _)
    | `Constant (`Int _)
    | `Constant (`Char _)
    | `Constant (`Float _)
    | `Variable _ -> true
    | `Erase (_, v)
    | `Coerce (v, _) -> is_atom v
    | _ -> false

let with_bindings bs' (bs, tc) = (bs' @ bs, tc)

type program = computation
  deriving (Show)

let string_of_var = string_of_int

let string_of_value _ = "[VALUE]"
let string_of_tail_computation _ = "[TAIL_COMPUTATION]"
let string_of_binding _ = "[BINDING]"
let string_of_special _ = "[SPECIAL]"
let string_of_computation _ = "[COMPUTATION]"
let string_of_program _ = "[PROGRAM]"

(** Traversal with type reconstruction

    Essentially this is a map-fold operation over the IR datatypes
    that also constructs the type as it goes along (using type
    annotations on binders).
*)
module type TRANSFORM =
sig
  type environment = Types.datatype Env.Int.t

  class visitor : environment ->
  object ('self_type)
    val tyenv : environment

    method lookup_type : var -> Types.datatype
    method constant : constant -> (constant * Types.datatype * 'self_type)
    method optionu :
      'a.
      ('self_type -> 'a -> ('a * 'self_type)) ->
      'a option -> 'a option * 'self_type
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

    method program : program -> (program * Types.datatype * 'self_type)

    method get_type_environment : environment
  end
end

module Transform : TRANSFORM =
struct
  open Types
  open TypeUtils

  type environment = datatype Env.Int.t

  let info_type (t, _, _) = t

  let deconstruct f t = f t

  module Env = Env.Int

  class visitor (tyenv : environment) =
  object ((o : 'self_type))
    val tyenv = tyenv

    method lookup_type : var -> datatype = fun var ->
      Env.lookup tyenv var
        
    method constant : constant -> (constant * datatype * 'self_type) = fun c ->
      match c with
        | `Bool _ -> c, bool_type, o
        | `Int _ -> c, int_type, o
        | `Char _ -> c, char_type, o
        | `String _ -> c, string_type, o
        | `Float _ -> c, float_type, o


    method optionu :
      'a.
      ('self_type -> 'a -> ('a * 'self_type)) ->
      'a option -> 'a option * 'self_type =
      fun f v ->
        match v with
          | None -> None, o
          | Some v ->
              let v, o = f o v in
                Some v, o

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
                      match TypeUtils.concrete_type t with
                        | `Record row ->
                            `Record (extend_row field_types row)
                        | _ -> assert false
                    end
            in
              `Extend (fields, base), t, o
        | `Project (name, v) ->
            let (v, vt, o) = o#value v in
              `Project (name, v), deconstruct (project_type name) vt, o
        | `Erase (names, v) ->
            let (v, vt, o) = o#value v in
            let t = deconstruct (erase_type names) vt in
              `Erase (names, v), t, o
        | `Inject (name, v, t) ->
            let v, _vt, o = o#value v in
              `Inject (name, v, t), t, o
        | `TAbs (tyvars, v) ->
            let v, t, o = o#value v in
            let t = Types.for_all (tyvars, t) in
              `TAbs (tyvars, v), t, o
        | `TApp (v, ts) ->
            let v, t, o = o#value v in
              begin try
                let t = Instantiate.apply_type t ts in
                  `TApp (v, ts), t, o
              with
                  Instantiate.ArityMismatch ->
                    prerr_endline ("Arity mismatch in type application (Ir.Transform)");
                    prerr_endline ("expression: "^Show.show show_value (`TApp (v, ts)));
                    prerr_endline ("type: "^Types.string_of_datatype t);
                    prerr_endline ("tyargs: "^String.concat "," (List.map Types.string_of_type_arg ts));
                    failwith "fatal internal error"
              end
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
              `ApplyPure (f, args), deconstruct return_type ft, o
        | `Coerce (v, t) ->
            let v, vt, o = o#value v in
            (* TODO: check that vt <: t *)
              `Coerce (v, t), t, o

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
              `Apply (f, args), deconstruct return_type ft, o
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
        | `Wrong t -> `Wrong t, t, o
        | `Database v ->
            let v, _, o = o#value v in
              `Database v, `Primitive `DB, o
        | `Table (db, table_name, (rt, wt, nt)) ->
            let db, _, o = o#value db in
            let table_name, _, o = o#value table_name in
              `Table (db, table_name, (rt, wt, nt)), `Table (rt, wt, nt), o
        | `Query (range, e, t) ->
            let range, o =
              o#optionu
                (fun o (limit, offset) ->
                   let limit, _, o = o#value limit in
                   let offset, _, o = o#value offset in
                     (limit, offset), o)
                range in
            let e, t, o = o#computation e in
              `Query (range, e, t), t, o
        | `Update ((x, source), where, body) ->
            let source, _, o = o#value source in
            let x, o = o#binder x in
            let where, _, o = o#option (fun o -> o#computation) where in
            let body, _, o = o#computation body in
              `Update ((x, source), where, body), Types.unit_type, o
        | `Delete ((x, source), where) ->
            let source, _, o = o#value source in
            let x, o = o#binder x in
            let where, _, o = o#option (fun o -> o#computation) where in
              `Delete ((x, source), where), Types.unit_type, o
        | `CallCC v ->
            let v, t, o = o#value v in
              `CallCC v, deconstruct return_type t, o
      
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
        let bs, o = o#bindings bs in
        let tc, t, o = o#tail_computation tc in
          (bs, tc), t, o
                                                       
    method binding : binding -> (binding * 'self_type) =
      function
        | `Let (x, (tyvars, tc)) ->
            let x, o = o#binder x in
            let tc, t, o = o#tail_computation tc in
              `Let (x, (tyvars, tc)), o
        | `Fun (f, (tyvars, xs, body), location) ->
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
              `Fun (f, (tyvars, xs, body), location), o
        | `Rec defs ->
            let _, o =
              List.fold_right
                (fun (f, _, _) (fs, o) ->
                   let f, o = o#binder f in
                     (f::fs, o))
                defs
                ([], o) in

            let defs, o =
              List.fold_left
                (fun (defs, o) (f, (tyvars, xs, body), location) ->
                   let xs, o =
                     List.fold_right
                       (fun x (xs, o) ->
                          let (x, o) = o#binder x in
                            (x::xs, o))
                       xs
                       ([], o) in
                  let body, _, o = o#computation body in
                    (f, (tyvars, xs, body), location)::defs, o)
                ([], o)
                defs in
            let defs = List.rev defs in
              `Rec defs, o
        | `Alien (x, language) ->
            let x, o = o#binder x in
              `Alien (x, language), o
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
        let tyenv = Env.bind tyenv (var, info_type info) in
          (var, info), {< tyenv=tyenv >}

    method program : program -> (program * datatype * 'self_type) = o#computation

    method get_type_environment : environment = tyenv
  end
end

module Inline =
struct
  let rec is_inlineable_value =
    function
      | v when is_atom v -> true
      | `Project (_, v)
      | `Inject (_, v, _) -> is_inlineable_value v
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
                  | `Let ((x, (_, _, `Local)), (tyvars, `Return v)) when is_inlineable_value v ->
                      let v =
                        match tyvars with
                          | [] -> v
                          | tyvars -> `TAbs (tyvars, v)
                      in
                        (o#with_env (IntMap.add x (fst3 (o#value v)) env))#bindings bs
                  | _ ->
                      let bs, o = o#bindings bs in
                        b :: bs, o
              end
        | [] -> [], o
  end

  let program typing_env p =
    fst3 ((inliner typing_env IntMap.empty)#computation p)
end

(*
  Eliminate dead functions and value bindings.

  Currently this is rather basic. It only does one pass, and it only
  eliminates variables in the following situations:

    - never used anywhere
    - only used recursively, but not mutually recursively
    - only used mutually recursively, and all the other mutually
    recursive bindings are only used mutually recursively

  If we partition mutually recursive bindings into strongly connected
  components beforehand then this will help eliminate more recursive
  bindings.
  
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
  let show_rec_uses = Settings.add_bool("show_rec_uses", false, `User)

  let counter tyenv =
  object (o)
    inherit Transform.visitor(tyenv) as super
      
    val env = IntMap.empty
    val rec_env = IntMap.empty
    val mutrec_env = IntMap.empty
      
    method with_env env =
      {< env = env >}

    method with_envs (env, rec_env, mutrec_env) =
      {< env = env; rec_env = rec_env; mutrec_env = mutrec_env >}

    method init (x, (_, name, _)) =
      o#with_env (IntMap.add x 0 env)

    method initrec (x, (_, name, _)) =
      o#with_envs (IntMap.add x 0 env, IntMap.add x (0, false) rec_env, IntMap.add x (0, true) mutrec_env)

    method set_rec_status f (r,m) =
      let (count, _) = IntMap.find f rec_env in
      let rec_env = IntMap.add f (count, r) rec_env in
      let (count, _) = IntMap.find f mutrec_env in
      let mutrec_env = IntMap.add f (count, m) mutrec_env in
        o#with_envs (env, rec_env, mutrec_env)

    method set_rec f =
      o#set_rec_status f (true, false)

    method set_mutrec f =
      o#set_rec_status f (false, true)

    method set_nonrec f =
      o#set_rec_status f (false, false)

    method set_nonrecs fs =
      IntSet.fold (fun f o -> o#set_nonrec f) fs o

    method inc x =
      if IntMap.mem x rec_env then
        let count = IntMap.find x env
        and rcount, ractive = IntMap.find x rec_env
        and mcount, mactive = IntMap.find x mutrec_env in
        let envs =
          match ractive, mactive with
            | false, false -> IntMap.add x (count+1) env, rec_env, mutrec_env
            | true, false -> env, IntMap.add x (rcount+1, ractive) rec_env, mutrec_env
            | false, true -> env, rec_env, IntMap.add x (mcount+1, mactive) mutrec_env
            | true, true -> assert false
        in
          o#with_envs envs
      else if IntMap.mem x env then
        o#with_env (IntMap.add x ((IntMap.find x env)+1) env)
      else
        o#with_env (IntMap.add x 1 env)

    method var =
      fun x ->         
        if IntMap.mem x env then
          x, o#lookup_type x, o#inc x
        else
          super#var x

    method binding b =
      match b with
        | `Let (x, (tyvars, `Return _)) ->
            let b, o = super#binding b in
              b, o#init x
        | `Fun (f, (tyvars, _, _), _) ->
            let b, o = super#binding b in
              b, o#init f
        | `Rec defs ->
            let fs, o =
              List.fold_right
                (fun (f, _, _) (fs, o) ->
                   let f, o = o#binder f in
                     (IntSet.add (var_of_binder f) fs, o#initrec f))
                defs
                (IntSet.empty, o) in

            let defs, o =
              List.fold_left
                (fun (defs, o) (f, (tyvars, xs, body), location) ->
                   let xs, o =
                     List.fold_right
                       (fun x (xs, o) ->
                          let (x, o) = o#binder x in
                            (x::xs, o))
                       xs
                       ([], o) in
                   let o = o#set_rec (var_of_binder f) in
                   let body, _, o = o#computation body in
                   let o = o#set_mutrec (var_of_binder f) in
                     (f, (tyvars, xs, body), location)::defs, o)
                ([], o)
                defs in
            let o = o#set_nonrecs fs in
            let defs = List.rev defs in
              `Rec defs, o
        | _ ->
            super#binding b

    method get_envs () = (env, rec_env, mutrec_env)
  end

  let eliminator tyenv (env, rec_env, mutrec_env) =
  object (o)
    inherit Transform.visitor(tyenv) as super
      
    val env = env
    val rec_env = rec_env
    val mutrec_env = mutrec_env
      
    method is_dead x =
      IntMap.mem x env && (IntMap.find x env = 0)

    method is_dead_rec f =
      IntMap.mem f env && (IntMap.find f env = 0
          && (not (IntMap.mem f mutrec_env) || fst (IntMap.find f mutrec_env) = 0))

    method bindings =
      function
        | b :: bs ->
            begin
              let b, o = o#binding b in
                match b with
                  | `Let ((x, (_, name, _)), (_tyvars, _)) when o#is_dead x ->
                      o#bindings bs
                  | `Fun ((f, (_, name, _)), _, _) when o#is_dead f ->
                      o#bindings bs
                  | `Rec defs ->
                      Debug.if_set show_rec_uses (fun () -> "Rec block:");
                      let fs, defs =
                        List.fold_left
                          (fun (fs, defs) (((f, (_, name, _)), _, _) as def) ->
                             Debug.if_set show_rec_uses
                               (fun () ->
                                  "  (" ^ name ^ ") non-rec uses: "^string_of_int (IntMap.find f env)^
                                    ", rec uses: "^string_of_int (fst (IntMap.find f rec_env))^
                                    ", mut-rec uses: "^string_of_int (fst (IntMap.find f mutrec_env)));
                             if o#is_dead_rec f then fs, defs
                             else
                               IntSet.add f fs, def :: defs)
                          (IntSet.empty, [])
                          defs in

                      (*
                         If none of the mutually recursive bindings appear elsewhere
                         then we can delete them all.
                      *)
                      let defs =
                        if IntSet.for_all o#is_dead fs then []
                        else
                          List.rev defs
                      in
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
      o#get_envs()

  let program tyenv p =
    let envs = count tyenv p in
    let p, _, _ = (eliminator tyenv envs)#computation p in
      p
end

(** Compute free variables *)
module FreeVars =
struct
  class visitor tenv bound_vars =
  object (o)
    inherit Transform.visitor(tenv) as super
      
    val free_vars = IntSet.empty
    val bound_vars = bound_vars

    method bound x =
      {< bound_vars = IntSet.add x bound_vars >}

    method free x =
      if IntSet.mem x bound_vars then o
      else {< free_vars = IntSet.add x free_vars >}

    method binder =
      fun b ->
        let b, o = super#binder b in
          b, o#bound (Var.var_of_binder b)
            
    method var =
      fun x ->
        let x, t, o = super#var x in
          x, t, o#free x
            
    method get_free_vars = free_vars
  end

  let value tyenv bound_vars v =
    let _, _, o = (new visitor tyenv bound_vars)#value v in
      o#get_free_vars

  let tail_computation tyenv bound_vars e =
    let _, _, o = (new visitor tyenv bound_vars)#tail_computation e in
      o#get_free_vars

  let binding tyenv bound_vars bs =
    let _, o = (new visitor tyenv bound_vars)#binding bs in
      o#get_free_vars

  let bindings tyenv bound_vars bs =
    let _, o = (new visitor tyenv bound_vars)#bindings bs in
      o#get_free_vars

  let computation tyenv bound_vars e =
    let _, _, o = (new visitor tyenv bound_vars)#computation e in
      o#get_free_vars

  let program = computation
end

(** The [closures] type represents the set of free variables of a
    collection of functions *)
type closures = intset intmap
    deriving (Show)

(** Compute the closures in an IR expression 

  This computes the free variables for every function
  and every continuation in an IR expression.
*)
module ClosureTable =
struct
  type t = closures

  class visitor tyenv bound_vars =
  object (o)
    inherit Transform.visitor(tyenv) as super
      
    val globals = bound_vars
    val relevant_vars = IntMap.empty

    method close f vars =
      {< relevant_vars = IntMap.add (Var.var_of_binder f) vars relevant_vars >}

    method global x =
      {< globals = IntSet.add x globals >}

    method binder b =
      let b, o = super#binder b in
        if Var.scope_of_binder b = `Global then
          b, o#global (Var.var_of_binder b)
        else
          b, o

    (** Incrementally compute the free variables for every possible
       continuation arising from a computation.

       The first argument is the free variables in the tail. The
       second argument is the list of bindings in reverse order.

       The list of bindings is in reverse order in order to make
       things both easier to express and more efficient.
    *)
    method close_cont fvs =
      function
        | [] -> o
        | `Let (x, (tyvars, body))::bs ->
            let fvs = IntSet.remove (Var.var_of_binder x) fvs in
            let fvs' = FreeVars.tail_computation o#get_type_environment globals body in
              (o#close x fvs)#close_cont (IntSet.union fvs fvs') bs
        | `Fun (f, (_tyvars, xs, body), _)::bs ->
            let fvs = IntSet.remove (Var.var_of_binder f) fvs in
            let bound_vars =
              List.fold_right
                (fun x bound_vars ->
                   IntSet.add (Var.var_of_binder x) bound_vars)
                xs
                globals in
            let fvs' = FreeVars.computation o#get_type_environment bound_vars body in
            let o = o#close f fvs' in
              o#close_cont (IntSet.union fvs fvs') bs
        | `Rec defs::bs ->
            let fvs, bound_vars =
              List.fold_right
                (fun (f, (_tyvars, xs, _body), _) (fvs, bound_vars) ->
                   let f = Var.var_of_binder f in
                   let fvs = IntSet.remove f fvs in
                   let bound_vars =
                     List.fold_right
                       (fun x bound_vars ->
                          IntSet.add (Var.var_of_binder x) bound_vars)
                       xs
                       (IntSet.add f bound_vars) in
                     fvs, bound_vars)
                defs
                (fvs, globals) in

            let fvs' =
              List.fold_left
                (fun fvs' (_f, (_tyvars, _xs, body), _location) ->
                   IntSet.union fvs' (FreeVars.computation o#get_type_environment bound_vars body))
                (IntSet.empty)
                defs in

            let o =
              List.fold_left
                (fun o (f, _, _) -> o#close f fvs')
                o
                defs
            in
              o#close_cont (IntSet.union fvs fvs') bs
        | `Alien (f, _language)::bs ->
            let fvs = IntSet.remove (Var.var_of_binder f) fvs in
              o#close_cont fvs bs            
        | `Module _::bs ->
            assert false

    method computation : computation -> (computation * Types.datatype * 'self_type) =
      fun (bs, tc) ->
        let bs, o = o#bindings bs in
        let tc, t, o = o#tail_computation tc in

        let free_vars = FreeVars.tail_computation o#get_type_environment globals tc in
        let o = o#close_cont free_vars (List.rev bs) in
          (bs, tc), t, o

    method binding b =
      match b with
(*         | `Fun (f, (tyvars, xs, body), location) ->              *)
(*             let xs, body, o = *)
(*               let (xs, o, bound_vars) = *)
(*                 List.fold_right *)
(*                   (fun x (xs, o, bound_vars) -> *)
(*                      let x, o = o#binder x in *)
(*                        (x::xs, o, IntSet.add (Var.var_of_binder x) bound_vars)) *)
(*                   xs *)
(*                   ([], o, globals) in *)
(*              let o = o#close f (FreeVars.computation o#get_type_environment bound_vars body) in *)
(*               let body, _, o = o#computation body in *)
(*                 xs, body, o in *)
(*             let f, o = o#binder f in *)
(*               `Fun (f, (tyvars, xs, body), location), o *)
(*         | `Rec defs -> *)
(*             let _, o = *)
(*               List.fold_right *)
(*                 (fun (f, _, _) (fs, o) -> *)
(*                    let f, o = o#binder f in *)
(*                      (f::fs, o)) *)
(*                 defs *)
(*                 ([], o) in *)

(*             let defs, o = *)
(*               List.fold_left *)
(*                 (fun (defs, o) (f, (tyvars, xs, body), location) -> *)
(*                    let xs, o, bound_vars = *)
(*                      List.fold_right *)
(*                        (fun x (xs, o, bound_vars) -> *)
(*                           let (x, o) = o#binder x in *)
(*                             (x::xs, o, IntSet.add (Var.var_of_binder x) bound_vars)) *)
(*                        xs *)
(*                        ([], o, globals) in *)
(*                    let o = o#close f (FreeVars.computation o#get_type_environment bound_vars body) in *)
(*                    let body, _, o = o#computation body in *)
(*                      (f, (tyvars, xs, body), location)::defs, o) *)
(*                 ([], o) *)
(*                 defs in *)
(*             let defs = List.rev defs in *)
(*               `Rec defs, o *)
        | _ -> super#binding b
            
    method get_relevant_vars = relevant_vars
  end

  let value tyenv bound_vars v =
    let _, _, o = (new visitor tyenv bound_vars)#value v in
      o#get_relevant_vars

  let tail_computation tyenv bound_vars e =
    let _, _, o = (new visitor tyenv bound_vars)#tail_computation e in
      o#get_relevant_vars

  let bindings tyenv bound_vars bs =
    let o = new visitor tyenv bound_vars in
    let _, _, o = o#computation (bs, `Return (`Extend (StringMap.empty, None))) in
      o#get_relevant_vars

  let computation tyenv bound_vars e =
    let _, _, o = (new visitor tyenv bound_vars)#computation e in
      o#get_relevant_vars

  let program = computation
end

let var_appln env name args =
  (`Apply(`Variable(Env.String.lookup env name), args) : tail_computation)
  
let rec funcmap_of_binding = function
  | `Let (b, (_, tc)) -> funcmap_of_tailcomp tc
  | (`Fun (b, (_,_,body), _) as f) -> 
      (Var.var_of_binder b, f) :: funcmap_of_computation body
  | `Rec defs -> concat_map (fun ((b, (_,_,body), _) as def) -> 
                               (Var.var_of_binder b, `Rec defs) :: 
                                 funcmap_of_computation body) defs
  | `Alien (b, _lang) -> []
  | `Module _ -> failwith "Not implemented."
and funcmap_of_tailcomp = function
  | `Return _ | `Apply _ | `Special _ -> []
  | `Case (_, branches, default) ->
      List.concat(StringMap.to_list
                    (fun _ (_, comp) -> funcmap_of_computation comp)
                    branches)
      @ (match default with
           | None -> []
           | Some (_, comp) -> funcmap_of_computation comp)
  | `If (_, t, f) ->
      funcmap_of_computation t @ funcmap_of_computation f
and funcmap_of_computation =
  fun (bs, tc) ->
    concat_map funcmap_of_binding bs @ funcmap_of_tailcomp tc

let funcmap = funcmap_of_computation
