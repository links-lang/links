open Camlp4.PreCast

(* Various utility bits *)
let rec range f t = 
  if f > t then []
  else f :: range (f+1) t
    
let rec endmarker = function
  | []    -> []
  | [_]   -> [true]
  | _::t  -> false::endmarker t

let rec index pred = 
  let rec aux n = function
    | []               -> raise Not_found
    | i::_ when pred i -> n
    | _::t             -> aux (n+1) t
  in aux 0

let curry f x y = f (x,y)
let uncurry f (x, y) = f x y

let random_id length = 
  let idchars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'" in
  let nidchars = String.length idchars in
  let s = String.create length in 
    for i = 0 to length - 1 do 
      s.[i] <- idchars.[Random.int nidchars]
    done;
    s

let tuple_expr loc = function  (* 0-tuples and 1-tuples are invalid *)
  | []  -> <:expr< () >>
  | [x] -> x
  | xs  -> failwith "nyi tuple_expr" (*<:expr< ( $list:xs$ ) >>*)
exception NotImpl of (Lexing.position * Lexing.position)

module AST = Quotation.Ast

type type_decl = (Loc.t 
                  * string (* type constructor *)
                  * AST.ctyp list (* parameters *)
                  * AST.ctyp (* rhs *)
                  * (AST.ctyp * AST.ctyp)  list) (* constraints *)

(* Generate instances of a particular class for a list of
   possibly-mutually-recursive type declarations *)
type instantiator = type_decl list -> AST.str_item
type sig_instantiator = type_decl list -> AST.sig_item

(* Display a fatal error and exit *)
let error loc msg = 
  begin 
    (*Ast.print_warning loc msg;*)
    failwith "print_warning  nyi"
(*    raise (NotImpl loc)*)
  end
    
type ltype = (string list * string) (* type parameters * type name *)

type thisinfo = {
  loc    : Loc.t;
  argmap :  (string * (string * string)) list; (* mapping from type parameters to functor arguments *)
  tname  : string;                 (* name of this type *)
  ltype  : ltype;                  (* The type name plus any parameters, e.g. 'c 'd t *)
  atype  : AST.ctyp;             (* The type name plus modularized parameters, e.g. V0.a V1.a t  *)
  rtype  : AST.ctyp;             (* The rhs of the type definitions *)
  currents : (string * string) list (* ? *)
}

(* Generate the 'a' type element of the generated module by applying
 * all the type parameters (looked up in the corresponding module
 * functor-parameters) to the type name *)
let gen_type_a loc = 
  List.fold_left
    (fun t (_,(_,mname)) -> <:ctyp< $t$ $uid:mname$ . a >>)

let gen_type_l id params = (List.map fst params, id)

let rec ltype_of_ctyp args = function
  | <:ctyp< $t1$ '$lid:tv$ >> -> ltype_of_ctyp (tv :: args) t1
  | <:ctyp< $lid:t1$ >>       -> Some (args, t1)
  | _ -> None

let ltype_of_ctyp = ltype_of_ctyp []

(* Generate a functor from a module and a list of type parameters (to
 * be converted to module functor-parameters).
 *)
let gen_functor loc classname : 'a list -> 'b -> 'b = 
  List.fold_right 
    (fun (_,(_,mname)) m -> <:module_expr< functor ($mname$ : $uid:classname$) -> $m$ >>)

(* Does a type declaration declare a "scheme" or a concrete type? *)
let is_polymorphic : type_decl -> bool = function
  | (_, _, (_::_), _, _) -> true
  | _                 -> false

(* Generate names for type parameters (type variables) *)
let param_names (params :  (string * (bool*bool)) list) :  (string * (string * string)) list =
  (List.map2
      (fun (p,_) n -> (p, (Printf.sprintf "v%d" n, Printf.sprintf "V%d" n)))
      params
      (range 0 (List.length params - 1)))

(* A association list of class names * instance generators *)
let instantiators : (string * (Loc.t -> instantiator)) list ref = ref []
let sig_instantiators : (string * (Loc.t -> sig_instantiator)) list ref = ref []

module Struct_utils
  (S : 
    sig
      val classname : string
      val defaults : string
    end) =
struct
  open S
  let apply_defaults ({loc=loc} as ti) decls = 
        <:module_expr< ($uid:defaults$ (struct
                                         type a = $ti.atype$
                                         $decls$
                                       end) : $uid:classname$ with type a = $ti.atype$) >>


  let gen_app t gen {loc=loc} (t1, t2) = <:module_expr< $gen t1$ $gen t2$ >>
  let gen_acc t gen {loc=loc} (t1,t2) = <:module_expr< $uid:t1$ . $gen t2$ >>
  let gen_lid t _ {loc=loc} (name) = <:module_expr< $uid:classname ^ "_" ^ name$ >>
  let gen_quo t _ ({loc=loc}as ti) (name) = <:module_expr< $uid:snd (List.assoc name ti.argmap)$ >>
  let gen_tup t gen {loc=loc} ts =
    failwith "nyi (now takes a type not a list of types)" 
(*    List.fold_left 
      (fun s param -> <:module_expr< $s$ $param$ >>)
    <:module_expr< $uid:Printf.sprintf "%s_%d" classname (List.length ts)$ >>
      (List.map gen ts)*)
  let gen_other t gen {loc=loc;tname=tname} _ =
    error loc ("Cannot currently generate "^S.classname^" instances for "^ tname)

  let gen_module_expr 
      ?(tyacc=gen_acc)
      ?(tyapp=gen_app)
      ?(tylid=gen_lid)
      ?(tyquo=gen_quo)
      ?(tytup=gen_tup)
      ?(other=gen_other)
      ~tyrec
      ~tysum
      ~tyvrn
      ({loc=loc; tname=tname; atype=_; rtype=rtype} as ti)
      = 
    let rec gen t = match t with
        (* See 
           $OCAML_ROOT/camlp4/Camlp4/Camlp4Ast.partial.ml
           for details on how to clean this lot up.
           Perhaps it should use quotations instead of constructors 
        *)

        (* sum type *)
        | <:ctyp< [ $list:variants$ ] >> -> tysum t gen ti variants

        (* polymorphic variant *)
        | _ ->
          failwith "nyi gen for variants (what about extends?)"
(*tyvrn t gen ti (fields, extends) *)

        (* record *)
        | <:ctyp< { $fields$ } >> -> tyrec t gen ti fields
            
        (* tuple *)
        | <:ctyp< ( $ctyps$ ) >> -> tytup t gen ti ctyps

        (* type name (lowercase identifier) *)
        | <:ctyp< $lid:string$ >> -> tylid t gen ti string

        (* type constructor application *)
        | <:ctyp< $ctyp1$ $ctyp2$ >> -> tyapp t gen ti (ctyp1, ctyp2)
            
        (* Module access *)
        | <:ctyp< $uid:_$.$lid:_$ >> -> failwith "nyi" (*tyacc t gen ti (name, ctyp2)*)
            
        (* type variable *)
        | Ast.TyQuo (_, string) -> tyquo t gen ti (string)

        (* summand *)
        | <:ctyp< $_$ of $_$ >>
             
        (* alias *)
(*        | <:ctyp< $_$ as $_$ >>  *)

        (* wildcard *)
        | <:ctyp< _ >> 
            
        (* function type *)
        | <:ctyp< $_$ -> $_$ >> 

        (* null type *)
        | <:ctyp< >> 

        (* class type *)
        | <:ctyp< #$_$ >>

          (* label type *)
        | <:ctyp< $_$: $_$ >>

        (* type name *)
        | <:ctyp< $lid:_$ >>

        (* ? "TyMan" *)
        | <:ctyp< $_$ = $_$ >>

        (* type declaration *)
(*       (* type t 'a 'b 'c = t constraint t = t constraint t = t *) *)
(*        | <:ctyp< type $_$ $_$ = $_$ $_$ >>*)

        (* object type *)
        | <:ctyp< < $_$ > >>

          (* optional label type *)
        | <:ctyp< ? $_$:$_$ >>

        (* polymorphic type ("TyPol") *)
(*        | <:ctyp< '$_$ . $_$ >>*)

       (* type variable *)
        | <:ctyp< ' $_$ >>

       (* covariant type variable *)
        | <:ctyp< +' $_$ >>

       (* contravariant type variable *)
        | <:ctyp< -' $_$ >>

        (* polymorphic variant constructor *)
        | <:ctyp< `$_$ >>

          (* record *)
        | <:ctyp< { $_$ } >>

        (* ? colon type, TyCol *)
        | <:ctyp< $_$ : $_$ >>

        (* ? semicolon type, TySem *)
        | <:ctyp< $_$ ; $_$ >>

        (* ? comma type, TyCom *)
        | <:ctyp< $_$ , $_$ >>

        (* and type, TyAnd *)
        | <:ctyp< $_$ and $_$ >>

        (* | type, TyAnd *)
        | <:ctyp< $_$ | $_$ >>

        (* private row *)
        | <:ctyp< private $_$ >>

        (* mutable field *)
        | <:ctyp< mutable $_$ >>

        (* `tuple' *)
        | <:ctyp< ( $_$ ) >>

        (* product *)
        | <:ctyp< $_$ * $_$ >>

        (* exact variant *)
        | <:ctyp< [ $_$ ] >>

        (* gt variant *)
        | <:ctyp< [ > $_$ ] >>

        (* lt variant *)
        | <:ctyp< [< $_$ ] >>

        (* gt lt variant *)
        | <:ctyp< [ < $_$ > $_$ ] >>

        (* & type *)
        | <:ctyp< $_$ & $_$ >>
            
        (* of-& type *)
        | <:ctyp< $_$ of & $_$ >>

        (* what is TyAnt? *)
           -> other t gen ti ()
    in gen

  let apply_functor loc funct params =
    List.fold_left 
      (fun expr (_,(_,param)) ->
         <:module_expr< $expr$ $uid:param$>>) funct params

  (* Generate n mutually-recursive instances (possibly functors) *)
  let gen_finstances loc ~tdl ~gen_module_expr = 
    failwith "nyi" (*
    let prefix = classname ^ "_" in
    match tdl with 
      | (_,params,_,_)::_ ->
          let tnames = List.map (fun ((_,tname),_,_,_) -> (tname, prefix ^ tname)) tdl in
            begin
              let params = param_names params in
              let modules = 
                let exprs = (List.map
                               (fun ((loc,tname),(*params*)_,ctype,(*constraints*)_) ->
                                  let atype = gen_type_a loc <:ctyp< $lid:tname$ >> params in 
                                    (prefix ^  tname, 
                                     <:module_type< $uid:classname$ with type a = $atype$ >>, 
                                         gen_module_expr {loc=loc;
                                                          argmap=params;
                                                          tname=tname;
                                                          atype=atype;
                                                          ltype= ([],tname);
                                                          rtype=ctype;
                                                          currents=tnames})) tdl) in
                  <:str_item< module rec $list:exprs$ >>
              in
              let (enclosing, projections) = 
                let rid = random_id 32 in
                let body = <:module_expr< struct $modules$ end >> in
                  (<:str_item< module $uid:prefix^ rid$ = $gen_functor loc classname params body$ >>,
                   List.map (fun ((*tname*)_, mname) ->
                               let body = 
                                 let funct = <:module_expr< $uid:prefix^ rid$ >> in
                                   <:module_expr< struct module S = $apply_functor loc funct params$ include S.$uid:mname$ end >>
                               in
                                 <:str_item< module $uid:mname$ = $gen_functor loc classname params body$ >>) tnames)
              in <:str_item< 
                               open $uid:classname$
                               open Primitives
                               $enclosing$
                               $list:projections$ 
                              >>
           end
      | _ -> assert false
                   *)
  (* replace each type variable `v' with the result of `lookup v' *)
  let instantiate lookup t = 
    let rec inst = function
        
      (* type variable *)
      | Ast.TyQuo (_ , name) -> lookup name
          
      | Ast.TyNil _
      | Ast.TyAny _ 
      | Ast.TyCls (_ , _) 
      | Ast.TyQuP (_ , _) 
      | Ast.TyQuM (_ , _) 
      | Ast.TyVrn (_ , _) 
      | Ast.TyId  (_ , _) 
      | Ast.TyAnt (_ , _)  as m       -> m
      | Ast.TyLab (loc, d1, c1)       -> Ast.TyLab (loc, d1, inst c1)
      | Ast.TyObj (loc, c1, d1)       -> Ast.TyObj (loc, inst c1, d1)
      | Ast.TyOlb (loc, d1, c1)       -> Ast.TyOlb (loc, d1, inst c1)
      | Ast.TyRec (loc, c1)           -> Ast.TyRec (loc, inst c1)
      | Ast.TySum (loc, c1)           -> Ast.TySum (loc, inst c1)
      | Ast.TyPrv (loc, c1)           -> Ast.TyPrv (loc, inst c1)
      | Ast.TyMut (loc, c1)           -> Ast.TyMut (loc, inst c1)
      | Ast.TyTup (loc, c1)           -> Ast.TyTup (loc, inst c1)
      | Ast.TyVrnEq (loc, c1)         -> Ast.TyVrnEq (loc, inst c1)
      | Ast.TyVrnSup (loc, c1)        -> Ast.TyVrnSup (loc, inst c1)
      | Ast.TyVrnInf (loc, c1)        -> Ast.TyVrnInf (loc, inst c1)
      | Ast.TyAli (loc, c1, c2)       -> Ast.TyAli (loc, inst c1, inst c2)
      | Ast.TyApp (loc, c1, c2)       -> Ast.TyApp (loc, inst c1, inst c2)
      | Ast.TyArr (loc, c1, c2)       -> Ast.TyArr (loc, inst c1, inst c2)
      | Ast.TyMan (loc, c1, c2)       -> Ast.TyMan (loc, inst c1, inst c2)
      | Ast.TyDcl (loc, d1, ctyps, 
                   c1, ctyps2)        -> Ast.TyDcl (loc, d1, List.map inst ctyps, inst c1, 
                                                    List.map (fun (t1,t2) -> inst t1, inst t2) ctyps2)
      | Ast.TyPol (loc, c1, c2)       -> Ast.TyPol (loc, inst c1, inst c2)
      | Ast.TyCol (loc, c1, c2)       -> Ast.TyCol (loc, inst c1, inst c2)
      | Ast.TySem (loc, c1, c2)       -> Ast.TySem (loc, inst c1, inst c2)
      | Ast.TyCom (loc, c1, c2)       -> Ast.TyCom (loc, inst c1, inst c2)
      | Ast.TyOf  (loc, c1, c2)       -> Ast.TyOf  (loc, inst c1, inst c2)
      | Ast.TyAnd (loc, c1, c2)       -> Ast.TyAnd (loc, inst c1, inst c2)
      | Ast.TyOr  (loc, c1, c2)       -> Ast.TyOr  (loc, inst c1, inst c2)
      | Ast.TySta (loc, c1, c2)       -> Ast.TySta (loc, inst c1, inst c2)
      | Ast.TyVrnInfSup (loc, c1, c2) -> Ast.TyVrnInfSup (loc, inst c1, inst c2)
      | Ast.TyAmp (loc, c1, c2)       -> Ast.TyAmp (loc, inst c1, inst c2)
      | Ast.TyOfAmp (loc, c1, c2)     -> Ast.TyOfAmp (loc, inst c1, inst c2)
    in inst t

  let instantiate_modargs ({loc=loc} as ti) t : Ast.ctyp = 
    let lookup var = 
      try
        <:ctyp< $uid:snd (List.assoc var ti.argmap)$.a >>
      with Not_found ->
        failwith ("Unbound type parameter '" ^ var)
    in instantiate lookup t

  let cast_pattern ({loc=loc} as ti) t ?(param="x") = 
    let t = instantiate_modargs ti t in
    (<:patt< $lid:param$ >>,
     Some <:expr<
            let module M = 
             struct
              type t = $t$
              let test = function #t -> True | _ -> False
             end in M.test $lid:param$ >>,
     <:expr<
       (let module M = 
            struct
              type t = $t$
              let cast = function #t as t -> t | _ -> assert False
            end in M.cast $lid:param$ )>>)
      
end

(* Utilities for generating module declarations in signatures *)
module Sig_utils =
struct
  let gen_functor_type loc classname : 'a list -> 'b -> 'b = 
    List.fold_right 
      (fun (_,(_,mname)) m -> <:module_type< functor ($mname$ : $uid:classname$) -> $m$ >>)
      
  let gen_sig (mname : string) (loc : Loc.t) 
      (loc, tname, params, _, _ : type_decl) = 
    failwith "nyi"
(*    let params = param_names params in
    let type_arg = gen_type_a loc <:ctyp< $lid:tname$ >> params  in
    let rhs =  <:module_type< ($uid:mname$ with type a = $type_arg$) >> in
    let module_expr = gen_functor_type loc mname params rhs in
      <:sig_item< open $uid:mname$ module $uid:(mname ^ "_" ^ tname)$ : $module_expr$ >>
*)
  let gen_sigs mname loc : sig_instantiator
      = fun tdl ->
        let decls = List.map (gen_sig mname loc) tdl in
          <:sig_item< $list:decls$ >>
end

module Gram = MakeGram(Lexer)

open Syntax

DELETE_RULE Gram str_item: "type"; LIST1 type_declaration SEP "and" END;
DELETE_RULE Gram sig_item: "type"; LIST1 type_declaration SEP "and" END;

EXTEND Gram
  str_item:
  [[ "type"; tdl = LIST1 type_declaration SEP "and" ->
       <:str_item< type $list:tdl$ >>
         | "type"; tdl = LIST1 type_declaration SEP "and" ; "deriving" ; "(" ; cl = LIST0 [x = UIDENT -> x] SEP ","  ; ")" ->
             let type_decl = <:str_item< type $list:tdl$ >> in 
             let instances = 
               List.map (fun name -> 
                           let instantiator = 
                             try List.assoc name !instantiators
                             with Not_found -> error loc (name ^" is not a known class") in
                             instantiator loc (*tdl*) (failwith "nyi") )
                 cl in
               <:str_item< $list:type_decl :: instances$ >>
  ]]
;
  sig_item:
  [[ "type"; tdl = LIST1 type_declaration SEP "and" ->
       <:sig_item< type $list:tdl$ >>
         | "type"; tdl = LIST1 type_declaration SEP "and" ; "deriving" ; "(" ; cl = LIST0 [x = UIDENT -> x] SEP ","  ; ")" ->
             let type_decl = <:sig_item< type $list:tdl$ >> in
             let instances  = 
               List.map (fun name -> 
                           let instantiator = 
                             try List.assoc name !sig_instantiators
                             with Not_found -> error loc (name ^" is not a known class (for signatures)") in
                             instantiator loc (*tdl*) (failwith "nyi"))
                 cl in
               <:sig_item< $list:type_decl :: instances$ >>
  ]]
;
END;
