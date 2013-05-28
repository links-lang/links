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
  (* An argument variable had to be spliced when entering a query function *)
  | `SplicedVariable of var
  (* Extend a (possibly empty) record *)
  | `Extend of (value name_map * value option)
  (* Project one of the fields of a record *)
  | `Project of (name * value)
  (* Remove a field from a record *)
  | `Erase of (name_set * value)
  (* Create a new variant *)
  | `Inject of (name * value * Types.datatype)

  (* Type abstraction and Type application, to handle polymorphism *)
  | `TAbs of tyvar list * value
  | `TApp of value * tyarg list

  | `XmlNode of (name * value name_map * value list)

  (* Apply "pure" fonction in the meaning of ANF *)
  | `ApplyPure of (value * value list)

  | `Coerce of (value * Types.datatype)
  ]
and tail_computation =
  [ `Return of (value)
  | `Apply of (value * value list)

(* We need those in the IR -> IRq transformation. You can see them as two projection from any functions to pl or db functions *)
  | `ApplyPL of (value * value list)
  | `ApplyDB of (value * value list)

  | `Special of special

  | `Case of (value * (binder * computation) name_map * (binder * computation) option)
  | `If of (value * computation * computation)
  ]
and binding =
  [ `Let of binder * (tyvar list * tail_computation)
  | `Fun of (binder * (tyvar list * binder list * computation) * location)
(* This can only be a query function *)
  | `FunQ of (binder * (tyvar list * binder list * computation) * location)
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

let tapp (v, tyargs) =
  match tyargs with
    | [] -> v
    | _ -> `TApp (v, tyargs)

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
(*
  This can only be an atom if
  Erase is just an upcast, and our language
  is properly parameteric.
*)
(*    | `Erase (_, v) *)
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

let var_name v n = if n = "" then "__"^(string_of_int v) else n

class stringIR venv = 
object (o : 'self_type)
  val venv = venv

  method add_bindings bs =
    let venv = List.fold_left 
      (fun e (v, (_, n, _)) -> Env.Int.bind e (v, (var_name v n))) venv bs in
      {< venv=venv >}
        
  method constant : constant -> doc = fun c ->
    let s = match c with
      | `Bool x -> string_of_bool x
      | `Int x -> Num.string_of_num x
      | `Char x -> "'" ^ Char.escaped x ^ "'"
      | `String x -> "\"" ^ String.escaped x ^ "\""
      | `Float x -> string_of_float x
    in text s

(*   method comparison : Syntaxutils.comparison -> doc = fun cmp -> *)
(*     match cmp with *)
(*       | `Less -> text "<" *)
(*       | `LessEq -> text "<=" *)
(*       | `Equal -> text "==" *)
(*       | `NotEq -> text "!=" *)

  method value : value -> doc = fun v ->
    match v with 
      | `Constant c -> o#constant c
      | `Variable v ->
          let name = match Env.Int.find venv v with
            | Some n -> n
            | None -> "NOT_FOUND"
          in
            if Str.string_match (Str.regexp "__[0-9]*" ) name 0 
            then text name
            else text (name ^ "/" ^ (string_of_int v))
      | `SplicedVariable v ->
          let name = match Env.Int.find venv v with
            | Some n -> n
            | None -> "NOT_FOUND"
          in
            if Str.string_match (Str.regexp "__[0-9]*" ) name 0 
            then text ( "#(" ^ name ^ ")" )
            else text ( "#(" ^ name ^ "/" ^ (string_of_int v) ^ ")" )

      | `Extend (r, v) ->
          (let r_doc = doc_concat (text "," ^^ break)
             (StringMap.to_list 
                (fun n v -> group(text n ^| text "=" ^| o#value v)) r) in
             match v with
                 None -> group (parens (r_doc))
               | Some v -> 
                   group (parens (r_doc ^| text "|" ^| 
                                      group (o#value v))))

      | `Project (n, v) -> group (o#value v ^^ text "." ^^ text n)
      | `Erase (ns, v) -> parens (group (o#value v ^^
                                           text "\\{" ^^
                                           StringSet.fold (fun n ns -> ns ^^ text n) ns empty ^^
                                           text "}"))
      | `Inject (n, v, _) -> group (text n ^^ parens(o#value v))
      | `TAbs (_,v) -> o#value v
      | `TApp (v, _) -> o#value v
      | `XmlNode _ -> text "XMLNODE"
      | `ApplyPure (v, vl) ->
          group (nest 2 (parens (o#value v ^| (doc_join o#value vl))))
(* Comparisons are now desugared as primitive functions *)
(*       | `Comparison (v1, cmp, v2) ->  *)
(*           group ( *)
(*             nest 2 ( *)
(*               group (o#value v1 ^| o#comparison cmp) ^| o#value v2)) *)
      | `Coerce (v,_) -> o#value v
          
  method tail_computation : tail_computation -> doc = fun tc ->
    match tc with
      `Return v -> o#value v
	  
    | `Apply (v, vl) -> 
        group (nest 2 (o#value v ^^ text "[any]" ^| 
          (if vl = [] then text "()" else doc_join o#value vl)))
    | `ApplyPL (v,vl) -> 
        group (nest 2 (o#value v ^^ text "[pl]" ^|  
          (if vl = [] then text "()" else doc_join o#value vl)))
    | `ApplyDB (v,vl) ->
        group (nest 2 (o#value v ^^ text "[db]"  ^|  
          (if vl = [] then text "()" else doc_join o#value vl)))


      | `Case (v, names, opt) ->
          let cases = 
            StringMap.fold
              (fun n (b, c) d -> 
                 let o = o#add_bindings [b] in
                   d ^| group (
                     nest 2 (
                       group (text n^^parens (o#binder b) ^| text "->") ^| 
                           o#computation c)))
              names empty in
          let default =
            match opt with
                None -> empty
              | Some (b, c) ->
                  group (
                    nest 2 (
                      text "DEFAULT: " ^|
                      group (o#binder b) ^| text "->") ^|
                        o#computation c) in
            group (
              nest 2 (
                group (text "case" ^| o#value v ^| text "of") ^|
                    cases ^| default) ^| text "end")              
                
      | `If (v, t, f) ->          
              group (
                nest 2 (text "if" ^| o#value v) ^|
                    nest 2 (text "then" ^| o#computation t) ^|
                        nest 2 (text "else" ^| o#computation f))
                      
      | `Special s ->
          match s with
              `CallCC v -> group (parens (text "call/cc" ^| o#value v))
            | `Database _ -> text "DATABASE"
            | `Table _ -> text "TABLE"
            | `Query (_,c,_) -> text "QUERY" ^| o#computation c
            | `Wrong _ -> text "WRONG"
            | `Delete _ -> text "DELETE"
            | `Update _ -> text "UPDATE"
                  
  method bindings : binding list -> 'self_type * doc = fun bs ->
    let (o, d) = List.fold_left
      (fun (o, accum_d) b -> let (o, d) = o#binding b in o, accum_d ^| d)
      (o, empty) bs in
      (o, d)

  method computation : computation -> doc = fun (bs, tc) ->
    match bs with
        [] -> o#tail_computation tc
      | _ -> 
          let (o, d) = o#bindings bs in
            group ( 
              nest 2 (text "let" ^| d) ^| 
                  nest 2 (text "in"  ^| o#tail_computation tc))

  method binding : binding -> 'self_type * doc = fun b ->
    match b with
        `Let (x, (_, tc)) ->
          let o = o#add_bindings [x] in
            o, group (
              nest 2 (
                group(text "val" ^| o#binder x ^| text "=") ^| 
                    o#tail_computation tc))
                     
      | `Fun (binder, (_, f_binders, comp), loc) ->
          let o = o#add_bindings (binder::f_binders) in
            o, group (
              nest 2 (
                group (text "fun" ^| o#binder binder ^| 
                           doc_join o#binder f_binders ^| text "=") ^| 
                    o#computation comp))
      | `FunQ (binder, (_, f_binders, comp), loc) ->
          let o = o#add_bindings (binder::f_binders) in
            o, group (
              nest 2 (
                group (text "funq" ^| o#binder binder ^| 
                           doc_join o#binder f_binders ^| text "=") ^| 
                    o#computation comp))
              
      | `Rec funs ->
          let o = o#add_bindings (List.map fst3 funs) in
          let (_, docs) = o#bindings (List.map (fun x -> `Fun x) funs) in
            o, group (
              nest 2 (text "rec" ^| docs) ^| 
                  text "end")

      | `Alien _ -> o, text "ALIEN"
      | `Module _ -> o, text "MODULE"

  method binder : binder -> doc = fun (v, (_, name, _)) ->
         let name = var_name v name in 
    if Str.string_match (Str.regexp "__[0-9]*" ) name 0 
    then text name
    else text (name ^ "/" ^ (string_of_int v))
        
end

let string_of_ir env (bs,tc) prelude =
  
  pretty 70 ((new stringIR (Env.invert_env env))#computation (prelude@bs,tc))

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
        | `SplicedVariable x -> let (x, t, o) = o#var x in `SplicedVariable x, t, o
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
            let t = deconstruct (erase_type_poly names) vt in
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
        | `ApplyPL (f, args) ->
            let f, ft, o = o#value f in
            let args, arg_types, o = o#list (fun o -> o#value) args in
              (* TODO: check arg types match *)
              `ApplyPL (f, args), deconstruct return_type ft, o
        | `ApplyDB (f, args) ->
            let f, ft, o = o#value f in
            let args, arg_types, o = o#list (fun o -> o#value) args in
              (* TODO: check arg types match *)
              `ApplyDB (f, args), deconstruct return_type ft, o
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
              if not (StringMap.is_empty case_types) 
              then
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
        | `FunQ (f, (tyvars, xs, body), location) ->
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
              `FunQ (f, (tyvars, xs, body), location), o
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
      | `Erase (_, v)
      | `Inject (_, v, _)
      | `TAbs (_, v)
      | `TApp (v, _) -> is_inlineable_value v 
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

(** Rename all variables in the given IR piece **)
module RenameVariable =
struct

  let remove tyenv = 
  object(o) 
         inherit Transform.visitor(tyenv) as super

         val new_names = IntMap.empty

         method binder_rec = super#binder
                
         method binder (var,vari) = 
                let new_var = Var.fresh_raw_var () in
                let o = {< new_names = IntMap.add var new_var new_names >} in
                o#binder_rec (new_var,vari)

         method var v =
                super#var ( match IntMap.lookup v new_names with None -> v | Some var -> var )
  end

let computation tyenv c = let c,_,_ = ((remove tyenv)#computation c) in c
let binding tyenv b = fst ((remove tyenv)#binding b)

end


(** Duplicate every function without wild effect in a pair of 
   a PL function and a DB function **)
module Doubling = 
  struct
    
    let duplication tenv =
      object(o)
        inherit Transform.visitor(tenv) as super
            
         (* A flag to know if we are in a query *)
        val in_query = false
        val duplicated_function = IntSet.empty
            
        method is_wild fun_type =
          let fields,_ = TypeUtils.effect_row fun_type in
          StringMap.mem "wild" fields 
            && fst (StringMap.find "wild" fields) = `Present
            
        method is_any t = 
                (* kinda hacky, but handle polymorphism *)
          try not (o#is_wild t) with _ -> false 
              
        method enter_query () = {< in_query = true >}
        method exit_query () = {< in_query = false >}
            
        method add_duplicated v = 
	  (*Printf.fprintf stderr "Duplicating %i \n" v;*)
          {< duplicated_function = IntSet.add v duplicated_function >}
        method replace_dup d = 
          {< duplicated_function = d >}
            
        method special sp = 
          match sp with
          | `Query _ when not in_query -> 
              let o = o#enter_query() in
              let s,t,o = o#special sp in
              s,t,o#exit_query()
          | _ -> super#special sp
		
        method binding b = 
          match b with
          | `FunQ _ when not in_query -> 
	      let o = o#enter_query() in 
	      let b,o = o#binding b in
	      b,o#exit_query()
          | _ -> super#binding b  
		
(* Attempt to fix problem with duplicated function parameters being missed *)
	method binder = 
	  fun (var, ((ftype,name,scope) as info)) -> 
	   (* Printf.fprintf stderr "Considering %s %i %s \n" name var (Types.string_of_datatype ftype);*)
	    let (var,info),o = super#binder (var,info) in
	    if o#is_any ftype 
	    then (var,info), o#add_duplicated var
	    else (var,info), o
		
	    
	    

(* Duplicate one list of bindings, replacing any-functions   
   with declarations of pl, db functions and defining 
   the original function name as a record containing pl and db fields.
   The bodies of the functions are not translated in this pass.
*)
	method duplicate_bindings bs = 
          match bs with
          | [] -> [], o
          | (`Fun (((_,(ftype,_,_)),_,_) as f))::q 
            when in_query && (* test not (o#is_wild ftype)*) o#is_any ftype -> 
              let tail, o = o#duplicate_bindings q in
              (`FunQ f)::tail, o
          | ((`Fun (((v,((ftype,_,_) as  vi)),((tyvar,_,_) as c),l) as f)))::q
            when (* test not (o#is_wild ftype)*) o#is_any ftype ->
              (* Generate fresh copies of functions *)
              let funpl = `Fun ((Var.fresh_raw_var (),vi),c,l)
              and fundb,remove = (RenameVariable.remove tyenv)#binding (`FunQ f)
              in
              let o = {< tyenv = remove#get_type_environment >} in
                         (* record this function has been duplicated *)
              let o = o#add_duplicated v in
                         (* Generate the record *)
              let record = 
                let get_id = function 
                    `Fun (b,_,_) 
                  | `FunQ (b,_,_) -> fst b 
                  | _ -> v  in 
                let values = 
                  StringMap.add "pl" (`Variable (get_id funpl)) 
                    (StringMap.add "db" (`Variable (get_id fundb)) 
                       StringMap.empty)
                in `Let ((v,vi),(tyvar,`Return (`Extend (values,None))))
              in
              let tail, o = o#duplicate_bindings q in
              funpl::fundb::record::tail, o
          | t::q ->             (* Wild Fun, Rec, Let handled normally
				Are they handled correctly? *)
              let tail,o = o#duplicate_bindings q in
              t::tail, o
                
        method get_var_id v = 
          match v with
          | `Variable id -> id 
          | `Coerce (var,_) 
          | `TAbs (_,var) 
          | `TApp (var,_) -> o#get_var_id var
          | _ -> failwith "This value isn't a variable"
                
        (* In an IR expression, there are no lambda abstraction values, 
           so all functions are named and so it is safe to assume that 
           a called function is essentially a variable. *)
        method tail_computation tc =
          match tc with 
          | `Apply ((v,vl) as f) 
            when IntSet.mem (o#get_var_id v) duplicated_function -> 
              (* Translate calls of duplicated functions appropriately based 
                 on context *)
              if in_query 
              then o#tail_computation(`ApplyDB f)
              else o#tail_computation(`ApplyPL f)          

          | _ -> super#tail_computation tc
		
		
	method computation (bs,tc) = 
          (* Compute the type information in the original term. *)
          let _,trans = 
            (new Transform.visitor o#get_type_environment)#bindings bs in
          (* Update this object with the resulting type info. *)
          let o = {< tyenv = trans#get_type_environment >} in
          (* Duplicate the bindings in the current binding list *)
          let bs,o = o#duplicate_bindings bs in
          (* Re-typecheck the translated term to get translated environment *)
          let _,trans = 
            (new Transform.visitor o#get_type_environment)#bindings bs in
	  let o = {< tyenv = trans#get_type_environment >} in
          (* Recursively translate the computation; this should 
             translate the bodies of the functions and the tail computation *)
	  super#computation (bs,tc)
	    
      end
	
    let program tyenv program = 
      fst3 ((duplication tyenv)#program program)
	
  end
    
module Splicing =
  struct
    
    let splice tyenv = 
      object(o)
	inherit Transform.visitor(tyenv) as super
	    
         (* A flag to know if we are in a query *)
	val in_query = false
	    
	method enter_query () = {< in_query = true >}
	method exit_query () = {< in_query = false >}
	    
         (* Register variable inside querys *)
        val query_var_env = IntSet.empty
	    
        method add_query_vars vl = 
          let aux s v = IntSet.add v s in
          {< query_var_env = List.fold_left aux query_var_env vl>}
        method is_query_var v = IntSet.mem v query_var_env  
	    
        method value v = 
	  match v with
          | `Variable x when in_query && (not (o#is_query_var x)) -> 
              super#value (`SplicedVariable x)
          | _ -> super#value v
		
        method special sp =
	  match sp with
          | `Query _ when not in_query -> 
	      let o = o#enter_query() in
              let s,t,o = o#special sp in
              s,t,o#exit_query()
          | _ -> super#special sp
		
        method binder b =
          let b,o = super#binder b in
          b, if in_query then o#add_query_vars [fst b] else o
	    
        method rec_binding = 
          super#binding
	    
        method binding b = 
	  match b with
          | `FunQ _ when not in_query -> 
	      let o = o#enter_query() in
              let b,o = o#binding b in
              b,o#exit_query()
          | _ -> super#binding b
	      
  end

let program tyenv program = 
  fst3 ((splice tyenv)#program program)

end


module RemoveApplyPure =
struct
  let unapplypurifier tyenv =
  object (o)
    inherit Transform.visitor(tyenv) as super

    val new_bindings = []

    method get_new_bindings () = List.hd new_bindings

    method add_binding = fun b -> 
      {< new_bindings=(b::List.hd new_bindings)::List.tl new_bindings >}

    method push () = {< new_bindings=[]::new_bindings >}

    method pop () = {< new_bindings=List.tl new_bindings >}

    method computation (bs, tc) =
      let bs, o' = o#bindings bs in
      let tc, t, o'' = (o'#push ())#tail_computation tc in
        (bs @ List.rev (o''#get_new_bindings ()), tc), t, o''#pop ()

    method bindings bs =
      let bs, o = 
        List.fold_left
          (fun (bs, o) b ->
             let (b, o) = (o#push ())#binding b in
               ((b::o#get_new_bindings ()) @ bs, o#pop ()))
          ([], o)
          bs
      in
        List.rev bs, o

    method value = fun v ->
      match super#value v with
        | `ApplyPure (f, args), t, o' ->
            let new_binder, new_var = Var.fresh_var_of_type t in
            let new_binding = `Let (new_binder, ([], `Apply (f, args))) in
              (`Variable new_var), t, o'#add_binding new_binding
        | v, t, o' -> v, t, o'

  end

let program tenv p =
  fst3 ((unapplypurifier tenv)#computation p)

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

    method private with_env env =
      {< env = env >}

    method private with_rec_env recenv =
      {< rec_env = rec_env >}

    method private with_mutrec_env mutrec_env =
      {< mutrec_env = mutrec_env >}

    method with_envs (env, rec_env, mutrec_env) =
       (* This three-stage update is a workaround for a camlp4 parsing bug 
          http://caml.inria.fr/mantis/view.php?id=4673
       *)
      ((o#with_env env)
         #with_rec_env rec_env)
         #with_mutrec_env mutrec_env
        
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
      if IntMap.mem x rec_env 
      then
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
        if IntMap.mem x env 
        then
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
                        if IntSet.for_all o#is_dead fs 
                        then []
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
      if IntSet.mem x bound_vars 
      then o
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
        if Var.scope_of_binder b = `Global 
        then
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
        | `Fun (f, (_tyvars, xs, body), _)::bs
        | `FunQ (f, (_tyvars, xs, body), _)::bs ->
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
  | `Fun (b, (_,_,body), _)
  | `FunQ (b, (_,_,body), _) as f -> 
      (Var.var_of_binder b, f) :: funcmap_of_computation body
  | `Rec defs -> concat_map (fun ((b, (_,_,body), _) as def) -> 
                               (Var.var_of_binder b, `Rec defs) :: 
                                 funcmap_of_computation body) defs
  | `Alien (b, _lang) -> []
  | `Module _ -> failwith "Not implemented."
and funcmap_of_tailcomp = function
  | `Return _ | `Apply _ | `ApplyDB _ | `ApplyPL _ | `Special _ -> []
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

