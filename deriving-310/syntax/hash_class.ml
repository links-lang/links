(*pp camlp4of *)

open Base
open Utils
open Type
open Camlp4.PreCast

let classname = "Hash"

let wrap ~loc ~atype ~eq hash =
  <:expr< { _Eq = $eq$ ; hash = fun v _state -> $hash$ v} >>

class hash ~loc = 
object (self)
  inherit Base.deriver ~loc  ~classname ~allow_private:true

  val superclasses = ["Eq"]
  val methods = ["hash"]

  method private nargs : atomic list -> Ast.expr = 
    fun args ->
      snd
        (List.fold_left
           (fun (n, e) at -> (n+1, <:expr< $self#atomic at$.hash $lid:Printf.sprintf "v%d" n$ $e$ >>))
           (0, <:expr< _state >>)
           args)


  method tuple atype args = 
    let nargs = List.length args in
    let patt, _ = tuple ~loc ~param:"v" nargs in
      wrap ~loc ~atype ~eq:(self#eq_instance atype) <:expr< fun $patt$ -> $self#nargs args$ >> 
  
  method private polycase : Type.tagspec -> int -> Ast.match_case = 
    fun spec n -> match spec with
    | `Tag (name, None)   -> <:match_case< `$name$ -> hash_int.hash $`int:tag_hash name$ _state >>
    | `Tag (name, Some e) -> <:match_case< `$name$ l ->  $self#atomic e$.hash l (hash_int.hash  $`int:tag_hash name$ _state); >>
    | `Local (c, _ as a) -> <:match_case< (# $lid:c$ as l) -> $self#local a$.hash l _state >>
    | `Appl (qname, _ as a) -> let cname = Untranslate.qname ~loc qname in
        <:match_case< (# $id:cname$ as l) -> $self#constr a$.hash l _state >>

  method private case : Type.summand -> int -> Ast.match_case =
    fun (name,args) n ->
      match args with 
        | [] -> <:match_case< $uid:name$ -> hash_int.hash $`int:n$ _state >>
        | _ -> let patt, _ = tuple ~loc ~param:"v" (List.length args) in
                  <:match_case< $uid:name$ $patt$ -> let _state = hash_int.hash $`int:n$ _state in $self#nargs args$ >> 
              
  method private field : Type.field -> Ast.expr -> Ast.expr =
    (fun (name, typ, _) e -> <:expr< $self#atomic typ$.hash _v.$lid:name$ $e$ >>)

  method sum atype ?eq summands =
    wrap ~loc ~atype ~eq:(self#eq_instance atype)
    <:expr< function $list:List.mapn self#case summands$ >>

  method record atype ?eq fields = 
  let projs = 
    List.fold_right self#field fields <:expr< _state >> in
    wrap ~loc ~atype ~eq:(self#eq_instance atype)
      (<:expr< fun _v -> $projs$ >>)
           
  method variant atype (spec, tags) = 
    wrap ~loc ~atype ~eq:(self#eq_instance atype)
    <:expr< function $list:List.mapn self#polycase tags$ >>

  method private eq_instance (name, params) = 
    let args = List.map (fun (a, _) -> `Tyvar a) params in
      apply_functor ~loc
      <:expr< $lid:"eq_"^ name$ >>  
        (List.map (fun a -> <:expr< $self#atomic a$._Eq >>) args)
end

let _ = Base.register classname (new hash)
