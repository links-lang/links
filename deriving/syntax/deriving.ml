(* camlp4r *)
#load "pa_extend.cmo";;
#load "q_MLast.cmo" ;;

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
  | xs  -> <:expr< ( $list:xs$ ) >>

exception NotImpl of (Lexing.position * Lexing.position)

(* Generate instances of a particular class for a list of
   possibly-mutually-recursive type declarations *)
type instantiator = MLast.type_decl list -> MLast.str_item
type sig_instantiator = MLast.type_decl list -> MLast.sig_item

(* Display a fatal error and exit *)
let error loc msg = 
  begin 
    !Pcaml.warning loc msg;
    raise (NotImpl loc)
  end
    
type ltype = (string list * string) (* type parameters * type name *)

type thisinfo = {
  loc    : MLast.loc;
  argmap : list (string * (string * string)); (* mapping from type parameters to functor arguments *)
  tname  : string;                 (* name of this type *)
  ltype  : ltype;                  (* The type name plus any parameters, e.g. 'c 'd t *)
  atype  : MLast.ctyp;             (* The type name plus modularized parameters, e.g. V0.a V1.a t  *)
  rtype  : MLast.ctyp;             (* The rhs of the type definitions *)
  currents : list (string * string) (* ? *)
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
let is_polymorphic : MLast.type_decl -> bool = function
  | (_, (_::_), _, _) -> true
  | _                 -> false

(* Generate names for type parameters (type variables) *)
let param_names (params : list (string * (bool*bool))) : list (string * (string * string)) =
  (List.map2
      (fun (p,_) n -> (p, (Printf.sprintf "v%d" n, Printf.sprintf "V%d" n)))
      params
      (range 0 (List.length params - 1)))

(* A association list of class names * instance generators *)
let instantiators : (string * (MLast.loc -> instantiator)) list ref = ref []
let sig_instantiators : (string * (MLast.loc -> sig_instantiator)) list ref = ref []

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
                                         type a = $ti.atype$;
                                         $decls$;
                                       end) : $uid:classname$ with type a = $ti.atype$) >>


  let gen_app t gen {loc=loc} (t1, t2) = <:module_expr< $gen t1$ $gen t2$ >>
  let gen_acc t gen {loc=loc} (t1,t2) = <:module_expr< $uid:t1$ . $gen t2$ >>
  let gen_lid t _ {loc=loc} (name) = <:module_expr< $uid:classname ^ "_" ^ name$ >>
  let gen_quo t _ ({loc=loc}as ti) (name) = <:module_expr< $uid:snd (List.assoc name ti.argmap)$ >>
  let gen_tup t gen {loc=loc} ts =
    List.fold_left 
      (fun s param -> <:module_expr< $s$ $param$ >>)
    <:module_expr< $uid:Printf.sprintf "%s_%d" classname (List.length ts)$ >>
      (List.map gen ts)
  let gen_other t gen {loc=loc;tname=tname} _ =
    error loc ("Cannot currently generate show instances for "^ tname)

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
        (* sum type *)
        | MLast.TySum (_, variants) -> tysum t gen ti variants

        (* polymorphic variant *)
        | MLast.TyVrn (_, fields, extends) -> tyvrn t gen ti (fields, extends)

        (* record *)
        | MLast.TyRec (_, fields) -> tyrec t gen ti fields
            
        (* tuple *)
        | MLast.TyTup (_, ctyps) -> tytup t gen ti ctyps

        (* type name (lowercase identifier) *)
        | MLast.TyLid (_, string) -> tylid t gen ti string

        (* type constructor application *)
        | MLast.TyApp (_, ctyp1, ctyp2) -> tyapp t gen ti (ctyp1, ctyp2)
            
        (* Module access *)
        | MLast.TyAcc (_, MLast.TyUid (_, name), ctyp2) -> tyacc t gen ti (name, ctyp2)
            
        (* type variable *)
        | MLast.TyQuo (_, string) -> tyquo t gen ti (string)
            
        | MLast.TyAli _ (* alias (as-type) *)
        | MLast.TyAny _ (* wildcard *)
        | MLast.TyArr _ (* arrow (function) type *)
        | MLast.TyCls _ (* class path *)
        | MLast.TyLab _ (* label type *)
        | MLast.TyMan _ (* type manifest *)
        | MLast.TyObj _ (* object *)
        | MLast.TyOlb _ (* optional label *)
        | MLast.TyPol _ (* class path application *)
        | MLast.TyPrv _ (* private row *)
        | MLast.TyUid _ (* uppercase identifier *)
        | MLast.TyAcc _ (* module access *)
          -> other t gen ti ()
            
    in gen

  let apply_functor loc funct params =
    List.fold_left 
      (fun expr (_,(_,param)) ->
         <:module_expr< $expr$ $uid:param$>>) funct params

  (* Generate n mutually-recursive instances (possibly functors) *)
  let gen_finstances loc ~tdl ~gen_module_expr = 
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
                let body = <:module_expr< struct $modules$; end >> in
                  (<:str_item< module $uid:prefix^ rid$ = $gen_functor loc classname params body$ >>,
                   List.map (fun ((*tname*)_, mname) ->
                               let body = 
                                 let funct = <:module_expr< $uid:prefix^ rid$ >> in
                                   <:module_expr< struct module S = $apply_functor loc funct params$ ; include S.$uid:mname$; end >>
                               in
                                 <:str_item< module $uid:mname$ = $gen_functor loc classname params body$ >>) tnames)
              in <:str_item< declare
                               open $uid:classname$;
                               open Primitives;
                               $enclosing$;
                               declare $list:projections$ end;
                             end >>
           end
      | _ -> assert false

  let cast_pattern loc t = 
    (<:patt< x >>,
     Some <:expr<
            let module M = 
             struct
              type t = $t$;
              value test = fun [ #t -> True | _ -> False];
             end in M.test x >>,
     <:expr<
       (let module M = 
            struct
              type t = $t$;
              value cast = fun [#t as t -> t | _ -> assert False];
            end in M.cast x)>>)
      
end

(* Utilities for generating module declarations in signatures *)
module Sig_utils =
struct
  let gen_functor_type loc classname : 'a list -> 'b -> 'b = 
    List.fold_right 
      (fun (_,(_,mname)) m -> <:module_type< functor ($mname$ : $uid:classname$) -> $m$ >>)
      
  let gen_sig (mname : string) (loc : MLast.loc) (((_,tname),params,_,_ ) : MLast.type_decl) = 
    let params = param_names params in
    let type_arg = gen_type_a loc <:ctyp< $lid:tname$ >> params  in
    let rhs =  <:module_type< ($uid:mname$ with type a = $type_arg$) >> in
    let module_expr = gen_functor_type loc mname params rhs in
      <:sig_item< declare open $uid:mname$; module $uid:(mname ^ "_" ^ tname)$ : $module_expr$; end >>

  let gen_sigs mname loc : sig_instantiator
      = fun tdl ->
        let decls = List.map (gen_sig mname loc) tdl in
          <:sig_item< declare $list:decls$ end >>
end

DELETE_RULE Pcaml.str_item: "type"; LIST1 Pcaml.type_declaration SEP "and" END;
DELETE_RULE Pcaml.sig_item: "type"; LIST1 Pcaml.type_declaration SEP "and" END;

EXTEND
  Pcaml.str_item:
  [[ "type"; tdl = LIST1 Pcaml.type_declaration SEP "and" ->
       <:str_item< type $list:tdl$ >>
         | "type"; tdl = LIST1 Pcaml.type_declaration SEP "and" ; "deriving" ; "(" ; cl = LIST0 UIDENT SEP ","  ; ")" ->
             let type_decl = <:str_item< type $list:tdl$ >> in 
             let instances = 
               List.map (fun name -> 
                           let instantiator = 
                             try List.assoc name !instantiators
                             with Not_found -> error loc (name ^" is not a known class") in
                             instantiator loc tdl)
                 cl in
               <:str_item< declare $list:type_decl :: instances$ end >>
  ]]
;
  Pcaml.sig_item:
  [[ "type"; tdl = LIST1 Pcaml.type_declaration SEP "and" ->
       <:sig_item< type $list:tdl$ >>
         | "type"; tdl = LIST1 Pcaml.type_declaration SEP "and" ; "deriving" ; "(" ; cl = LIST0 UIDENT SEP ","  ; ")" ->
             let type_decl = <:sig_item< type $list:tdl$ >> in
             let instances  = 
               List.map (fun name -> 
                           let instantiator = 
                             try List.assoc name !sig_instantiators
                             with Not_found -> error loc (name ^" is not a known class (for signatures)") in
                             instantiator loc tdl)
                 cl in
               <:sig_item< declare $list:type_decl :: instances$ end >>
  ]]
;
END;
