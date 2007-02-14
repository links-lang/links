#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open Deriving
include Deriving.Struct_utils(struct let classname="Shelve" let defaults="Shelve_defaults" end)

let bindop = ">>=" (* Can't use >>= directly in quotations because '>>' confuses the parser *)

let apply_functor' loc funct params component =
  List.fold_left 
    (fun expr (_,(_,param)) ->
       <:module_expr< $expr$ $uid:param$.$uid:component$>>) funct params

let typeable_module :   MLast.loc -> 'a list -> string -> MLast.module_expr
    = fun loc params aname ->
      let funct = <:module_expr< $uid:"Typeable_"^ aname$ >> in
        apply_functor' loc funct params "Typeable"
  
let eq_module :   MLast.loc -> 'a list -> string -> MLast.module_expr
    = fun loc params aname ->
      let funct = <:module_expr< $uid:"Eq_"^ aname$ >> in
        apply_functor' loc funct params "Eq"

let currentp currents = function
| None -> false
| Some (_, s) -> List.mem_assoc s currents

let module_name currents = function
| None -> assert false
| Some (_, s) -> List.assoc s currents

(* Generate a printer for each constructor parameter *)
let gen_printer = 
  let current default (t:MLast.ctyp) gen ({loc=loc;currents=currents} as ti) c = 
    let lt = ltype_of_ctyp t in
    if currentp currents lt then 
      <:module_expr< $uid:(module_name currents lt)$ >>
    else default t gen ti c
  in 
  gen_module_expr
    ~tyrec:gen_other ~tysum:gen_other ~tyvrn:gen_other
    ~tyapp:(current gen_app)
    ~tylid:(current gen_lid)

let gen_shelve_record ({loc=loc}as ti) fields = 
  let names = tuple_expr loc (List.map (fun (loc,k,_,_) -> (<:expr< $lid:k$ >>)) fields) in
  let field_patts = List.map (fun (loc,k,_,_) -> (<:patt< $lid:k$ >>, <:patt< $lid:k$ >>)) fields in
  let tuple_expr = gen_printer ti <:ctyp< ($list:List.map (fun (_,_,_,v) -> v) fields$) >> in
<:str_item<
   value rec shelve ({ $list:field_patts$ } as obj) = 
      let module M = $tuple_expr$ in M.shelve $names$
>>

let gen_shelve_case : thisinfo -> (MLast.loc*string*MLast.ctyp list) -> int -> (MLast.patt * MLast.expr option * MLast.expr) =
fun ti (loc, name, params') n ->
(*
 What should this look like? 
  S1.shelve v1 >>= fun id1 ->
  ...
  Sn.shelve vn >>= fun idn ->
    allocate_store_return (Typeable.makeDynamic obj) Comp.eq (make_repr ~constructor:$n$ [id1; ... idn])
*)

  (* Have to make:
       pattern:      $constructor$ $args$ as obj
       each line:    let module M = $foo$ in M.shelve $param$ >>= fun $id$ ->
       list of ids:  [id1 :: id2 :: []]
       constructor:  ~constructor:n
  *)
  let constructor_num = <:expr< $int:string_of_int n$ >> in
  let params = (List.map2 (fun p n -> (p, Printf.sprintf "v%d" n, Printf.sprintf "id%d" n))
                  params' (range 0 (List.length params' - 1))) in
  let ids = List.fold_right (fun (_,_,id) tail -> <:expr< [$lid:id$::$tail$] >>) params <:expr< [] >> in
  let closing = <:expr< allocate_store_return (Typeable.makeDynamic obj) Comp.eq (make_repr ~constructor:$constructor_num$ $ids$) >> in
  let expr = List.fold_right (fun (p,param,id) tail -> 
                                <:expr< let module M = $gen_printer ti p$ in 
                                        $lid:bindop$ (M.shelve $lid:param$)
                                                     (fun $lid:id$ -> $tail$)>>)
                  params closing in
  let patt = (List.fold_left 
                (fun patt (_,v,_) -> <:patt< $patt$ $lid:v$ >>) <:patt< $uid:name$>> params) in
    (<:patt< ($patt$ as obj) >>, None, expr)


let gen_shelve_sum ({loc=loc} as ti) ctors = 
<:str_item< 
   value rec shelve = 
     fun [ $list:List.map2 (gen_shelve_case ti) ctors (range 0 (List.length ctors - 1))$]
>>

let gen_shelve_polycase ({loc=loc} as ti) = function
  | (MLast.RfTag (name, _, params'), n) -> 
      let constructor_num = <:expr< $int:string_of_int n$ >> in
      let params = (List.map2 (fun p n -> (p, Printf.sprintf "v%d" n, Printf.sprintf "id%d" n)) 
                      params' (range 0 (List.length params' - 1))) in
      let ids = List.fold_right (fun (_,_,id) tail -> <:expr< [$lid:id$ :: $tail$] >>) params <:expr< [] >> in
      let patt = (List.fold_left 
                    (fun patt (_,v,_) -> <:patt< $patt$ $lid:v$ >>) <:patt< `$uid:name$>> params) in  (* the "uid" isn't really safe here *)
      let closing = <:expr< allocate_store_return (Typeable.makeDynamic obj)
                                                   Comp.eq
                                                   (make_repr ~constructor:$constructor_num$ [$list:ids$]) >> in
      let expr = List.fold_right (fun (p,v,id) tail ->
                                      <:expr< let module M = $gen_printer ti p$ in 
                                              $lid:bindop$ (M.shelve $lid:v$)
                                                           (fun [$lid:id$ -> $tail$]) >>)
                    params closing 
      in (<:patt< ($patt$ as obj) >>, None, expr)
      
  | (MLast.RfInh (<:ctyp< $lid:tname$ >> as ctyp), n) -> 
      (<:patt< ((# $[tname]$ as $lid:tname$) as obj) >>, 
      None, 
      <:expr< let module S = $gen_printer ti ctyp$ in 
              $lid:bindop$ (S.shelve $lid:tname$)
                           (fun [id -> allocate_store_return 
                                              (Typeable.makeDynamic obj)
                                              Comp.eq
                                              (make_repr ~constructor:$int:string_of_int n$ [id])]) >>)
  | (MLast.RfInh ctyp, n) ->
      let var, guard, expr = cast_pattern loc ctyp ~param:"x" in
        (<:patt< ($var$ as obj)>>, guard, 
         <:expr< let module S = $gen_printer ti ctyp$ in
              $lid:bindop$ (S.shelve $expr$)
                           (fun [id -> allocate_store_return 
                                              (Typeable.makeDynamic obj)
                                              Comp.eq
                                              (make_repr ~constructor:$int:string_of_int n$ [id])]) >>)


let gen_shelve_poly ({loc=loc} as ti) (row, _) =
<:str_item<
  value rec shelve =
     fun [ $list:List.map2 (curry (gen_shelve_polycase ti)) row (range 0 (List.length row - 1))$]
>>

let apply_defaults ({loc=loc;argmap=params;ltype=(_,aname)} as ti) decls = 
  let typeable = typeable_module loc params aname
  and eq = eq_module loc params aname in
<:module_expr< ((struct
                 module Typeable = $typeable$;
                 module Eq = $eq$;
                 module Comp = Dynmap.Comp (Typeable) (Eq);
                 open Shelvehelper;
                 type a = $ti.atype$;
                 $decls$;
               end) : Shelve with type a = $ti.atype$) >>


let gen_module_expr ti = 
  (* Doesn't handle typeable/eq/comp business *)
  gen_module_expr
    ~tyrec:(fun _ _ ti fields -> apply_defaults ti (gen_shelve_record ti fields))
    ~tysum:(fun _ _ ti ctors -> apply_defaults ti (gen_shelve_sum ti ctors))
    ~tyvrn: (fun _ _ ti row -> apply_defaults ti (gen_shelve_poly ti row))
    ti ti.rtype

(*
 let typeable_name = typeable_module loc params tname in
 let eq_name = eq_module loc params tname in
*)

let gen_instances loc tdl = gen_finstances ~gen_module_expr:gen_module_expr loc ~tdl:tdl

let _ = 
  begin
    instantiators := ("Shelve", gen_instances):: !instantiators;
    sig_instantiators := ("Shelve", Sig_utils.gen_sigs "Shelve"):: !sig_instantiators;
  end
