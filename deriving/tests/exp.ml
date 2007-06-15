(*pp deriving *)

module VarMap = Map.Make(String)
module VarSet = Set.Make(String)

type name = string deriving (Show, Pickle, Typeable)
module Eq_string : Eq.Eq with type a = name =
struct
  type a = name
  let eq = (=)
end 
module Shelve_name = Shelve.Shelve_from_pickle(Pickle_string)(Eq_string)(Typeable_string)


module rec Exp :
sig
  type exp = Var of name
           | Apply of exp * exp 
           | Lambda of name * exp
               deriving (Eq,Show,Shelve,Typeable)
end =
struct
  module Eq_exp = struct 
    open Exp
    type a = exp
    let eq : exp -> exp -> bool
      = let rec alpha_eq (env,rhsbound as context) l r =
        match l, r with
          | Var l, Var r when VarMap.mem l env ->
              VarMap.find l env = r
          | Var l, Var r -> 
              not (VarSet.mem r rhsbound) && l = r
          | Apply (fl,pl), Apply (fr,pr) ->
              alpha_eq context fl fr
              && alpha_eq context pl pr
          | Lambda (vl,bl), Lambda (vr,br) ->
              alpha_eq (VarMap.add vl vr env,
                        VarSet.add vr rhsbound)
                bl br
          | _ -> false
      in alpha_eq (VarMap.empty, VarSet.empty)
  end
    
  type exp = Var of name
           | Apply of exp * exp 
           | Lambda of name * exp
               deriving (Show, Typeable, Shelve)
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

    | Apply (e1,e2) as a ->
        let (id,next,dynmap) = find (next,dynmap) a in
          Printf.printf "Apply %d\n" id;
          let (next,dynmap) = discover (next,dynmap) e1 in
          let (next,dynmap) = discover (next,dynmap) e2 in
            (next,dynmap)

    | Lambda (s,e) as l ->
        let (id,next,dynmap) = find (next,dynmap) l in
          Printf.printf "Lambda %d\n" id;
          let (id,next,dynmap) = find (next,dynmap) s in 
            Printf.printf "string: %s %d\n" s id;
            let (next,dynmap) = discover (next,dynmap) e in
              (next,dynmap)
  in fun e -> (discover (1,[]) e)

    

let y = 
  Lambda ("a",
          Apply (Lambda ("b",
                         Apply (Var "a",
                                Lambda ("c", 
                                        Apply (Apply (Var "b",
                                                      Var "b"),
                                               Var "c")))),
                 Lambda ("d",
                         Apply (Var "a",
                                Lambda ("e", 
                                        Apply (Apply (Var "d",
                                                      Var "d"),
                                               Var "e"))))))
