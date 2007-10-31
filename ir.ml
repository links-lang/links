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
  | `Alien of (binder * language * Types.assumption)
  | `Alias of (tyname * tyvar list * Types.datatype) ]
and special =
  [ `App of value * value
  | `Wrong
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

module Inline =
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
        | `Wrong -> `Wrong
        | `Database v -> `Database (iv v)
        | `Query q -> `Query q
            (* [WARNING] perhaps we need to look inside the query *)
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
          let env, bs' = bindings env bs in
            env, (binding env b) :: bs'
      | [] ->
          env, []
          
  and computation env (bs, tc)  : computation =
    let env, bs = bindings env bs in
      (bs, tail_computation env tc)

  let program = computation (IntMap.empty)  
end

type program = computation


class map =
  object (o)
    method string = fun x -> (x : string)
    method char = fun x -> (x : char)
    method int = fun x -> (x : int)
    method float = fun x -> (x : float)
    method bool = fun x -> (x : bool)
    method list : 'a 'b. ('a -> 'b) -> 'a list -> 'b list = List.map
    method option : 'a 'b. ('a -> 'b) -> 'a option -> 'b option =
      fun f -> function | None -> None | Some x -> Some (f x)
    method array : 'a 'b. ('a -> 'b) -> 'a array -> 'b array = Array.map
    method ref : 'a 'b. ('a -> 'b) -> 'a ref -> 'b ref =
      fun f { contents = x } -> { contents = f x; }
    method _Utility_stringmap : 'a 'b . ('a -> 'b) -> 'a Utility.stringmap -> 'b Utility.stringmap =
      fun f -> fun x -> Utility.StringMap.map f x
    method _Types_datatype : Types.datatype -> Types.datatype = fun x -> x
    method _Types_assumption : Types.assumption -> Types.assumption =
      fun x -> x
    method _Syntaxutils_comparison :
      Syntaxutils.comparison -> Syntaxutils.comparison = fun x -> x
    method _Syntax_location : Syntax.location -> Syntax.location = fun x -> x
    method _Syntax_constant : Syntax.constant -> Syntax.constant = fun x -> x
    method _SqlQuery_sqlQuery : SqlQuery.sqlQuery -> SqlQuery.sqlQuery =
      fun x -> x
    method var_info : var_info -> var_info =
      fun (_x0, _x1, _x2) ->
        ((o#_Types_datatype _x0), (o#string _x1), (o#scope _x2))
    method var : var -> var = o#int
    method value : value -> value =
      function
      | `Constant x -> `Constant (o#constant x)
      | `Variable x -> `Variable (o#var x)
      | `Extend x ->
          `Extend
            ((fun (_x0, _x1) ->
                ((o#name_map o#value _x0), (o#option o#value _x1)))
               x)
      | `Project x ->
          `Project ((fun (_x0, _x1) -> ((o#name _x0), (o#value _x1))) x)
      | `Erase x ->
          `Erase ((fun (_x0, _x1) -> ((o#name _x0), (o#value _x1))) x)
      | `Inject x ->
          `Inject ((fun (_x0, _x1) -> ((o#name _x0), (o#value _x1))) x)
      | `XmlNode x ->
          `XmlNode
            ((fun (_x0, _x1, _x2) ->
                ((o#name _x0), (o#name_map o#value _x1),
                 (o#list o#value _x2)))
               x)
      | `ApplyPrim x ->
          `ApplyPrim
            ((fun (_x0, _x1) -> ((o#value _x0), (o#list o#value _x1))) x)
      | `Comparison x ->
          `Comparison
            ((fun (_x0, _x1, _x2) ->
                ((o#value _x0), (o#_Syntaxutils_comparison _x1),
                 (o#value _x2)))
               x)
      | `Coerce x ->
          `Coerce
            ((fun (_x0, _x1) -> ((o#value _x0), (o#_Types_datatype _x1))) x)
      | `Abs x -> `Abs (o#value x)
    method tyvar : tyvar -> tyvar = o#int
    method tyname : tyname -> tyname = o#string
    method tail_computation : tail_computation -> tail_computation =
      function
      | `Return x -> `Return (o#value x)
      | `Apply x ->
          `Apply
            ((fun (_x0, _x1) -> ((o#value _x0), (o#list o#value _x1))) x)
      | `Special x -> `Special (o#special x)
      | `Case x ->
          `Case
            ((fun (_x0, _x1, _x2) ->
                ((o#value _x0),
                 (o#name_map
                    (fun (_x0, _x1) -> ((o#binder _x0), (o#computation _x1)))
                    _x1),
                 (o#option
                    (fun (_x0, _x1) -> ((o#binder _x0), (o#computation _x1)))
                    _x2)))
               x)
      | `If x ->
          `If
            ((fun (_x0, _x1, _x2) ->
                ((o#value _x0), (o#computation _x1), (o#computation _x2)))
               x)
    method special : special -> special =
      function
      | `App x -> `App ((fun (_x0, _x1) -> ((o#value _x0), (o#value _x1))) x)
      | `Wrong -> `Wrong
      | `Database x -> `Database (o#value x)
      | `Query x ->
          `Query
            ((fun _x0 ->
                (o#_SqlQuery_sqlQuery _x0))
               x)
      | `Table x ->
          `Table
            ((fun (_x0, _x1, _x2) ->
                ((o#value _x0), (o#value _x1),
                 ((fun (_x0, _x1) ->
                     ((o#_Types_datatype _x0), (o#_Types_datatype _x1)))
                    _x2)))
               x)
      | `CallCC x -> `CallCC (o#value x)
    method scope : scope -> scope =
      function | `Local -> `Local | `Global -> `Global
    method program : program -> program = o#computation
    method name_map : 'b0 'a0. ('a0 -> 'b0) -> 'a0 name_map -> 'b0 name_map =
      fun _f_a -> o#_Utility_stringmap _f_a
    method name : name -> name = o#string
    method location : location -> location = o#_Syntax_location
    method language : language -> language = o#string
    method constant : constant -> constant = o#_Syntax_constant
    method computation : computation -> computation =
      fun (_x0, _x1) -> ((o#list o#binding _x0), (o#tail_computation _x1))
    method binding : binding -> binding =
      function
      | `Let x ->
          `Let
            ((fun (_x0, _x1) -> ((o#binder _x0), (o#tail_computation _x1))) x)
      | `Fun x ->
          `Fun
            ((fun (_x0, _x1, _x2, _x3) ->
                ((o#binder _x0), (o#list o#binder _x1), (o#computation _x2),
                 (o#location _x3)))
               x)
      | `Rec x ->
          `Rec
            (o#list
               (fun (_x0, _x1, _x2, _x3) ->
                  ((o#binder _x0), (o#list o#binder _x1),
                   (o#computation _x2), (o#location _x3)))
               x)
      | `Alien x ->
          `Alien
            ((fun (_x0, _x1, _x2) ->
                ((o#binder _x0), (o#language _x1), (o#_Types_assumption _x2)))
               x)
      | `Alias x ->
          `Alias
            ((fun (_x0, _x1, _x2) ->
                ((o#tyname _x0), (o#list o#tyvar _x1),
                 (o#_Types_datatype _x2)))
               x)
    method binder : binder -> binder =
      fun (_x0, _x1) -> ((o#var _x0), (o#var_info _x1))
  end
class fold =
  object ((o : 'self_type))
    method string = fun (_ : string) -> (o : 'self_type)
    method char = fun (_ : char) -> (o : 'self_type)
    method int = fun (_ : int) -> (o : 'self_type)
    method float = fun (_ : float) -> (o : 'self_type)
    method bool = fun (_ : bool) -> (o : 'self_type)
    method list :
      'a. ('self_type -> 'a -> 'self_type) -> 'a list -> 'self_type =
      fun f -> List.fold_left f o
    method option :
      'a. ('self_type -> 'a -> 'self_type) -> 'a option -> 'self_type =
      fun f -> function | None -> o | Some x -> f o x
    method array :
      'a. ('self_type -> 'a -> 'self_type) -> 'a array -> 'self_type =
      fun f -> Array.fold_left f o
    method ref :
      'a. ('self_type -> 'a -> 'self_type) -> 'a ref -> 'self_type =
      fun f { contents = x } -> f o x
    method _Utility_stringmap : 'a . ('self_type -> 'a -> 'self_type) -> 'a Utility.stringmap -> 'self_type = fun _ -> fun _ -> o
    method _Types_datatype : Types.datatype -> 'self_type = fun _ -> o
    method _Types_assumption : Types.assumption -> 'self_type = fun _ -> o
    method _Syntaxutils_comparison : Syntaxutils.comparison -> 'self_type =
      fun _ -> o
    method _Syntax_location : Syntax.location -> 'self_type = fun _ -> o
    method _Syntax_constant : Syntax.constant -> 'self_type = fun _ -> o
    method _SqlQuery_sqlQuery : SqlQuery.sqlQuery -> 'self_type = fun _ -> o
    method var_info : var_info -> 'self_type =
      fun (_x0, _x1, _x2) -> ((o#_Types_datatype _x0)#string _x1)#scope _x2
    method var : var -> 'self_type = o#int
    method value : value -> 'self_type =
      function
      | `Constant x -> o#constant x
      | `Variable x -> o#var x
      | `Extend x ->
          (fun (_x0, _x1) ->
             (o#name_map (fun o -> o#value) _x0)#option (fun o -> o#value)
               _x1)
            x
      | `Project x -> (fun (_x0, _x1) -> (o#name _x0)#value _x1) x
      | `Erase x -> (fun (_x0, _x1) -> (o#name _x0)#value _x1) x
      | `Inject x -> (fun (_x0, _x1) -> (o#name _x0)#value _x1) x
      | `XmlNode x ->
          (fun (_x0, _x1, _x2) ->
             ((o#name _x0)#name_map (fun o -> o#value) _x1)#list
               (fun o -> o#value) _x2)
            x
      | `ApplyPrim x ->
          (fun (_x0, _x1) -> (o#value _x0)#list (fun o -> o#value) _x1) x
      | `Comparison x ->
          (fun (_x0, _x1, _x2) -> (o#_Syntaxutils_comparison _x1)#value _x2)
            x
      | `Coerce x -> (fun (_x0, _x1) -> o#_Types_datatype _x1) x
      | `Abs x -> o#value x
    method tyvar : tyvar -> 'self_type = o#int
    method tyname : tyname -> 'self_type = o#string
    method tail_computation : tail_computation -> 'self_type =
      function
      | `Return x -> o#value x
      | `Apply x ->
          (fun (_x0, _x1) -> (o#value _x0)#list (fun o -> o#value) _x1) x
      | `Special x -> o#special x
      | `Case x ->
          (fun (_x0, _x1, _x2) ->
             ((o#value _x0)#name_map
                (fun o (_x0, _x1) -> (o#binder _x0)#computation _x1) _x1)#
               option (fun o (_x0, _x1) -> (o#binder _x0)#computation _x1)
               _x2)
            x
      | `If x ->
          (fun (_x0, _x1, _x2) ->
             ((o#value _x0)#computation _x1)#computation _x2)
            x
    method special : special -> 'self_type =
      function
      | `App x -> (fun (_x0, _x1) -> (o#value _x0)#value _x1) x
      | `Wrong -> o
      | `Database x -> o#value x
      | `Query x -> (fun _x0 -> o#_SqlQuery_sqlQuery _x0) x
      | `Table x ->
          (fun (_x0, _x1, _x2) ->
             (fun (_x0, _x1) -> o#_Types_datatype _x1) _x2)
            x
      | `CallCC x -> o#value x
    method scope : scope -> 'self_type =
      function | `Local -> o | `Global -> o
    method program : program -> 'self_type = o#computation
    method name_map :
      'a0. ('self_type -> 'a0 -> 'self_type) -> 'a0 name_map -> 'self_type =
      fun _f_a -> o#_Utility_stringmap (fun o -> _f_a o)
    method name : name -> 'self_type = o#string
    method location : location -> 'self_type = o#_Syntax_location
    method language : language -> 'self_type = o#string
    method constant : constant -> 'self_type = o#_Syntax_constant
    method computation : computation -> 'self_type =
      fun (_x0, _x1) ->
        (o#list (fun o -> o#binding) _x0)#tail_computation _x1
    method binding : binding -> 'self_type =
      function
      | `Let x -> (fun (_x0, _x1) -> (o#binder _x0)#tail_computation _x1) x
      | `Fun x ->
          (fun (_x0, _x1, _x2, _x3) ->
             (((o#binder _x0)#list (fun o -> o#binder) _x1)#computation _x2)#
               location _x3)
            x
      | `Rec x ->
          o#list
            (fun o (_x0, _x1, _x2, _x3) ->
               (((o#binder _x0)#list (fun o -> o#binder) _x1)#computation _x2)#
                 location _x3)
            x
      | `Alien x -> (fun (_x0, _x1, _x2) -> o#_Types_assumption _x2) x
      | `Alias x -> (fun (_x0, _x1, _x2) -> o#_Types_datatype _x2) x
    method binder : binder -> 'self_type =
      fun (_x0, _x1) -> (o#var _x0)#var_info _x1
  end



class foldmap =
  object ((o : 'self_type))
    method string : string -> ('self_type * string) = o#unknown
      
    method option :
      'a.
        ('self_type -> 'a -> ('self_type * 'a)) ->
          'a option -> ('self_type * ('a option)) =
      fun _f_a ->
        function
        | None -> (o, None)
        | Some _x -> let (o, _x) = _f_a o _x in (o, (Some _x))
      
    method list :
      'a.
        ('self_type -> 'a -> ('self_type * 'a)) ->
          'a list -> ('self_type * ('a list)) =
      fun _f_a ->
        function
        | [] -> (o, [])
        | _x :: _x_i1 ->
            let (o, _x) = _f_a o _x in
            let (o, _x_i1) = o#list _f_a _x_i1 in (o, (_x :: _x_i1))
      
    method int : int -> ('self_type * int) = o#unknown
      
    method var_info : var_info -> ('self_type * var_info) =
      fun (_x, _x_i1, _x_i2) ->
        let (o, _x) = o#unknown _x in
        let (o, _x_i1) = o#string _x_i1 in
        let (o, _x_i2) = o#scope _x_i2 in (o, (_x, _x_i1, _x_i2))
      
    method var : var -> ('self_type * var) = o#int
      
    method value : value -> ('self_type * value) =
      function
      | `Constant _x -> let (o, _x) = o#constant _x in (o, (`Constant _x))
      | `Variable _x -> let (o, _x) = o#var _x in (o, (`Variable _x))
      | `Extend ((_x, _x_i1)) ->
          let (o, _x) = o#name_map (fun o -> o#value) _x in
          let (o, _x_i1) = o#option (fun o -> o#value) _x_i1
          in (o, (`Extend ((_x, _x_i1))))
      | `Project ((_x, _x_i1)) ->
          let (o, _x) = o#name _x in
          let (o, _x_i1) = o#value _x_i1 in (o, (`Project ((_x, _x_i1))))
      | `Erase ((_x, _x_i1)) ->
          let (o, _x) = o#name _x in
          let (o, _x_i1) = o#value _x_i1 in (o, (`Erase ((_x, _x_i1))))
      | `Inject ((_x, _x_i1)) ->
          let (o, _x) = o#name _x in
          let (o, _x_i1) = o#value _x_i1 in (o, (`Inject ((_x, _x_i1))))
      | `XmlNode ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#name _x in
          let (o, _x_i1) = o#name_map (fun o -> o#value) _x_i1 in
          let (o, _x_i2) = o#list (fun o -> o#value) _x_i2
          in (o, (`XmlNode ((_x, _x_i1, _x_i2))))
      | `ApplyPrim ((_x, _x_i1)) ->
          let (o, _x) = o#value _x in
          let (o, _x_i1) = o#list (fun o -> o#value) _x_i1
          in (o, (`ApplyPrim ((_x, _x_i1))))
      | `Comparison ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#value _x in
          let (o, _x_i1) = o#unknown _x_i1 in
          let (o, _x_i2) = o#value _x_i2
          in (o, (`Comparison ((_x, _x_i1, _x_i2))))
      | `Coerce ((_x, _x_i1)) ->
          let (o, _x) = o#value _x in
          let (o, _x_i1) = o#unknown _x_i1 in (o, (`Coerce ((_x, _x_i1))))
      | `Abs _x -> let (o, _x) = o#value _x in (o, (`Abs _x))
      
    method tyvar : tyvar -> ('self_type * tyvar) = o#int
      
    method tyname : tyname -> ('self_type * tyname) = o#string
      
    method tail_computation :
      tail_computation -> ('self_type * tail_computation) =
      function
      | `Return _x -> let (o, _x) = o#value _x in (o, (`Return _x))
      | `Apply ((_x, _x_i1)) ->
          let (o, _x) = o#value _x in
          let (o, _x_i1) = o#list (fun o -> o#value) _x_i1
          in (o, (`Apply ((_x, _x_i1))))
      | `Special _x -> let (o, _x) = o#special _x in (o, (`Special _x))
      | `Case ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#value _x in
          let (o, _x_i1) =
            o#name_map
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#binder _x in
                 let (o, _x_i1) = o#computation _x_i1 in (o, (_x, _x_i1)))
              _x_i1 in
          let (o, _x_i2) =
            o#option
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#binder _x in
                 let (o, _x_i1) = o#computation _x_i1 in (o, (_x, _x_i1)))
              _x_i2
          in (o, (`Case ((_x, _x_i1, _x_i2))))
      | `If ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#value _x in
          let (o, _x_i1) = o#computation _x_i1 in
          let (o, _x_i2) = o#computation _x_i2
          in (o, (`If ((_x, _x_i1, _x_i2))))
      
    method special : special -> ('self_type * special) =
      function
      | `App ((_x, _x_i1)) ->
          let (o, _x) = o#value _x in
          let (o, _x_i1) = o#value _x_i1 in (o, (`App ((_x, _x_i1))))
      | `Wrong -> (o, `Wrong)
      | `Database _x -> let (o, _x) = o#value _x in (o, (`Database _x))
      | `Query _x -> let (o, _x) = o#unknown _x in (o, (`Query _x))
      | `Table ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#value _x in
          let (o, _x_i1) = o#value _x_i1 in
          let (o, _x_i2) =
            (fun (_x, _x_i1) ->
               let (o, _x) = o#unknown _x in
               let (o, _x_i1) = o#unknown _x_i1 in (o, (_x, _x_i1)))
              _x_i2
          in (o, (`Table ((_x, _x_i1, _x_i2))))
      | `CallCC _x -> let (o, _x) = o#value _x in (o, (`CallCC _x))
      
    method scope : scope -> ('self_type * scope) =
      function | `Local -> (o, `Local) | `Global -> (o, `Global)
      
    method name_map :
      'a.
        ('self_type -> 'a -> ('self_type * 'a)) ->
          'a name_map -> ('self_type * ('a name_map)) =
      fun _f_a -> o#unknown
      
    method name : name -> ('self_type * name) = o#string
      
    method location : location -> ('self_type * location) = o#unknown
      
    method language : language -> ('self_type * language) = o#string
      
    method constant : constant -> ('self_type * constant) = o#unknown
      
    method computation : computation -> ('self_type * computation) =
      fun (_x, _x_i1) ->
        let (o, _x) = o#list (fun o -> o#binding) _x in
        let (o, _x_i1) = o#tail_computation _x_i1 in (o, (_x, _x_i1))
      
    method binding : binding -> ('self_type * binding) =
      function
      | `Let ((_x, _x_i1)) ->
          let (o, _x) = o#binder _x in
          let (o, _x_i1) = o#tail_computation _x_i1
          in (o, (`Let ((_x, _x_i1))))
      | `Fun ((_x, _x_i1, _x_i2, _x_i3)) ->
          let (o, _x) = o#binder _x in
          let (o, _x_i1) = o#list (fun o -> o#binder) _x_i1 in
          let (o, _x_i2) = o#computation _x_i2 in
          let (o, _x_i3) = o#location _x_i3
          in (o, (`Fun ((_x, _x_i1, _x_i2, _x_i3))))
      | `Rec _x ->
          let (o, _x) =
            o#list
              (fun o (_x, _x_i1, _x_i2, _x_i3) ->
                 let (o, _x) = o#binder _x in
                 let (o, _x_i1) = o#list (fun o -> o#binder) _x_i1 in
                 let (o, _x_i2) = o#computation _x_i2 in
                 let (o, _x_i3) = o#location _x_i3
                 in (o, (_x, _x_i1, _x_i2, _x_i3)))
              _x
          in (o, (`Rec _x))
      | `Alien ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#binder _x in
          let (o, _x_i1) = o#language _x_i1 in
          let (o, _x_i2) = o#unknown _x_i2
          in (o, (`Alien ((_x, _x_i1, _x_i2))))
      | `Alias ((_x, _x_i1, _x_i2)) ->
          let (o, _x) = o#tyname _x in
          let (o, _x_i1) = o#list (fun o -> o#tyvar) _x_i1 in
          let (o, _x_i2) = o#unknown _x_i2
          in (o, (`Alias ((_x, _x_i1, _x_i2))))
      
    method binder : binder -> ('self_type * binder) =
      fun (_x, _x_i1) ->
        let (o, _x) = o#var _x in
        let (o, _x_i1) = o#var_info _x_i1 in (o, (_x, _x_i1))
      
    method unknown : 'a. 'a -> ('self_type * 'a) = fun x -> (o, x)
      
  end
  
