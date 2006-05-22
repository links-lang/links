(*

  We can't handle mutually recursive polymorphic types with this
  compilation scheme because functors (which we use to encode
  instances for polymorphic types) are never permitted to be recursive.

  See 
    "A proposal for recursive modules in Objective Caml"
    Xavier Leroy
    Version 1.1, 13 May 2003.
    (footnote, p6)
*)

(* camlp4r *)

#load "pa_extend.cmo";
#load "q_MLast.cmo";

value version = "0.6";

value notyp = ref False;
Pcaml.add_option "-notyp" (Arg.Set notyp)
  "       Don't generate types definitions.";

value print_version () =
  do {
    Printf.eprintf "IoXML version %s\n" version;
    flush stderr;
    exit 1
  };

Pcaml.add_option "-ioxml_v" (Arg.Unit print_version)
  "     Print IoXML version number and exit.";

(* Various utility bits *)
value rec range f t = 
 if f > t then []
 else [f :: range (f+1) t];

value rec endmarker = fun [
  []     -> []
| [_]    -> [True]
| [_::t] -> [False::endmarker t]
];

value rec index pred = 
  let rec aux n = fun [
    []                 -> raise Not_found
  | [i::_] when pred i -> n
  | [_::t]             -> aux (n+1) t
  ] in aux 0
;

value curry f x y = f (x,y);
value uncurry f (x, y) = f x y;


exception NotImpl of (Lexing.position * Lexing.position);

(* Generate instances of a particular class for a list of
   possibly-mutually-recursive type declarations *)
type instantiator = list MLast.type_decl -> MLast.str_item;


(* Display a fatal error and exit *)
value error loc msg = do {
  Pcaml.warning.val loc msg;
  raise (NotImpl loc)
};

type ltype = (list string * string); (* type parameters * type name *)

type thisinfo = {
  loc    : MLast.loc;
  argmap : list (string * (string * string)); (* mapping from type parameters to functor arguments *)
  tname  : string;                 (* name of this type *)
  ltype  : ltype;                  (* The type name plus any parameters, e.g. 'c 'd t *)
  atype  : MLast.ctyp;             (* The type name plus modularized parameters, e.g. V0.a V1.a t  *)
  rtype  : MLast.ctyp              (* The rhs of the type definitions *)
};

value show_thisinfo i = 
  Printf.sprintf "{
  argmap : %s;
  tname  : %s;
  ltype  : %s;
  rtype  : %s;
}" ("["^ String.concat "," (List.map (fun (x,(y,z)) -> Printf.sprintf "(%s,(%s,%s))" x y z) i.argmap) ^ "]")
    i.tname
    "<lhs>"
    "<rhs>";

(* Generate the 'a' type element of the generated module by applying
 * all the type parameters (looked up in the corresponding module
 * functor-parameters) to the type name *)
value gen_type_a loc = 
    List.fold_left
      (fun t (_,(_,mname)) -> <:ctyp< $t$ $uid:mname$ . a >>)
;
value gen_type_l id params = (List.map fst params, id)
;

value rec ltype_of_ctyp args = fun [
  <:ctyp< $t1$ '$lid:tv$ >> -> ltype_of_ctyp [tv :: args] t1
| <:ctyp< $lid:t1$ >>       -> Some (args, t1)
| _ -> None
];
value ltype_of_ctyp = ltype_of_ctyp [];


(* Generate a functor from a module and a list of type parameters (to
 * be converted to module functor-parameters).
 *)
value gen_functor loc classname = 
  List.fold_right 
    (fun (_,(_,mname)) m -> <:module_expr< functor ($mname$ : $uid:classname$) -> $m$ >>)
;

(* Does a type declaration declare a "scheme" or a concrete type? *)
value is_polymorphic : MLast.type_decl -> bool = fun [
  (_, ([_::_]), _, _) -> True
| _                   -> False
];


(* A association list of class names * instance generators *)
value instantiators : ref (list (string * MLast.loc -> instantiator)) = ref [];


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
                                  try List.assoc name instantiators.val
                                  with [Not_found -> error loc (name ^" is not a known class")] in
                                  instantiator loc tdl)
                      cl in
                    <:str_item< declare $list:[type_decl :: instances]$ end >>
  ] ]
;
END;
