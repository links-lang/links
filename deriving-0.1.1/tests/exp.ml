(*pp deriving *)

module Env = Bimap.Make(String)

type name = string deriving (Show, Dump, Typeable)
module Eq_string : Eq.Eq with type a = name =
struct
  type a = name
  let eq = (=)
end 
module Pickle_name
  = Pickle.Pickle_from_dump(Dump_string)(Eq_string)(Typeable_string)

module rec Exp :
sig
  type exp = Var of name
           | App of exp * exp 
           | Abs of name * exp
               deriving (Eq,Show,Pickle,Typeable,Dump)
end =
struct
  module Eq_exp = struct 
    open Exp
    type a = exp
    let eq : exp -> exp -> bool
      = let rec alpha_eq env l r = match l, r with
        | Var l, Var r when Env.mem l env -> 
            Env.find l env = r
        | Var l, Var r -> 
            not (Env.rmem r env) && l = r
        | App (fl,pl), App (fr,pr) ->
            alpha_eq env fl fr && alpha_eq env pl pr
        | Abs (vl,bl), Abs (vr,br) ->
            alpha_eq (Env.add vl vr env) bl br
        | _ -> false
      in alpha_eq Env.empty
  end
  type exp = Var of name
           | App of exp * exp 
           | Abs of name * exp
               deriving (Show, Typeable, Pickle,Dump)
end

open Exp
(*
let args = ref  []
*)
let discover_sharing : exp -> 'a =
  let find (next,dynmap) obj = 
    let repr = Obj.repr obj in
    try List.assq repr dynmap, next, dynmap
    with Not_found -> next,next+1,(repr,next)::dynmap in
  let rec discover (next,dynmap) = function
    | Var s as v ->
        let (id,next,dynmap) = find (next,dynmap) v in
          Printf.printf "Var %d\n" id;
        let (id,next,dynmap) = find (next,dynmap) s in 
          Printf.printf "string: %s %d\n" s id;
          (next, dynmap)

    | App (e1,e2) as a ->
        let (id,next,dynmap) = find (next,dynmap) a in
          Printf.printf "App %d\n" id;
          let (next,dynmap) = discover (next,dynmap) e1 in
          let (next,dynmap) = discover (next,dynmap) e2 in
            (next,dynmap)

    | Abs (s,e) as l ->
        let (id,next,dynmap) = find (next,dynmap) l in
          Printf.printf "Abs %d\n" id;
          let (id,next,dynmap) = find (next,dynmap) s in 
            Printf.printf "string: %s %d\n" s id;
            let (next,dynmap) = discover (next,dynmap) e in
              (next,dynmap)
  in fun e -> (discover (1,[]) e)

    

let y = 
  Abs ("a",
       App (Abs ("b",
                 App (Var "a",
                      Abs ("c", 
                           App (App (Var "b",
                                     Var "b"),
                                Var "c")))),
            Abs ("d",
                 App (Var "a",
                      Abs ("e", 
                           App (App (Var "d",
                                     Var "d"),
                                Var "e"))))))
let app e1 e2 = App (e1, e2)

let abs (v,e) = Abs (v,e)

let freevar x = Var x

let rec term_size = function
  | Var _ -> 1
  | App (e1,e2) -> term_size e1 + term_size e2
  | Abs (_, body) -> 1 + term_size body
