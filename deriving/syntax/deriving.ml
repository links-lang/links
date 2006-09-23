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

exception NotImpl of (Lexing.position * Lexing.position)

(* Generate instances of a particular class for a list of
   possibly-mutually-recursive type declarations *)
type instantiator = MLast.type_decl list -> MLast.str_item

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
  rtype  : MLast.ctyp              (* The rhs of the type definitions *)
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

DELETE_RULE Pcaml.str_item: "type"; LIST1 Pcaml.type_declaration SEP "and" END;

EXTEND
  Pcaml.str_item:
  [ [ "type"; tdl = LIST1 Pcaml.type_declaration SEP "and" ->
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
  ] ]
;
END;
