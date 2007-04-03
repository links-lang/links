(* FIXME:
   optimization: shouldn't pickle a tag for single-constructor types. *)

#load "pa_extend.cmo";;
#load "q_MLast.cmo";;
open Deriving

include Deriving.Struct_utils(struct let classname="Pickle" let defaults="Pickle_defaults"end)

let currentp currents = function
| None -> false
| Some (_, s) -> List.mem_assoc s currents


let module_name currents = function
| None -> assert false
| Some (_, s) -> List.assoc s currents


(* Generate a printer for each constructor parameter *)
let rec gen_printer = 
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

let gen_printers ({loc=loc} as ti) tuple params = 
 let m = (List.fold_left
            (fun s param -> <:module_expr< $s$ $param$ >>)
            <:module_expr< $uid:Printf.sprintf "Pickle_%d" (List.length params)$ >>
            (List.map (gen_printer ti) params)) in
   <:expr< let module S = $m$ in S.pickle buffer $tuple$ >>

(* Generate an individual case clause for the pickle function. 
case:
     | $(Ctor-name) $(params) -> 
         Buffer.add_string buffer $(Ctor-name + space)
         let module S = Pickle_$(|params|) $(modularized_param_types) in
             S.pickle $(params) buffer
*)
let gen_pickle_case ti (loc, name, params') (pos : int) =
  let params = (List.map2 (fun p n -> (p, Printf.sprintf "v%d" n)) 
                  params' (range 0 (List.length params' - 1))) in
  let patt = (List.fold_left 
                (fun patt (_,v) -> <:patt< $patt$ $lid:v$ >>) <:patt< $uid:name$>> params) in
  let tuple = tuple_expr loc (List.map (fun (_,p) -> <:expr< $lid:p$ >>) params)
  in (patt, None, <:expr< do { Pickle_int.pickle buffer $int:string_of_int pos$; $gen_printers ti tuple params'$ } >>)

let gen_unpickle_case ti (loc, name, params') (pos : int) =
  let params = (List.map2 (fun p n -> (p, Printf.sprintf "v%d" n)) 
                  params' (range 0 (List.length params' - 1))) in
  let expr = 
    List.fold_left
      (fun acc (_,v) -> <:expr< $acc$ $lid:v$ >>) 
      <:expr< $uid:name$ >>
      params
  in
  let patt = <:patt< $int:string_of_int pos$ >> in
    (patt, None, 
     List.fold_right 
       (fun (ptype, var) expr ->
          <:expr<  let module S = $gen_printer ti ptype$ in
          let $lid:var$ = S.unpickle stream in $expr$ >>)
       params
       expr)



let unpickle_failure ({loc=loc}as ti) = (<:patt< c >>, None, <:expr< failwith (Printf.sprintf "Unexpected tag %d during unpickling of type : %s" c $str:ti.tname$) >>)

(* Generate the pickle and unpickle functions. *)
let gen_sum ({loc=loc} as ti) ctors = <:str_item< 
   value rec pickle buffer = 
             fun [ $list:List.map2 (gen_pickle_case ti) ctors (range 0 (List.length ctors - 1))$ ]
   and unpickle stream =
           match Pickle_int.unpickle stream with [ $list:List.map2 (gen_unpickle_case ti) ctors (range 0 (List.length ctors - 1)) @ [unpickle_failure ti]$ ]
>>

let gen_funs_record ({loc=loc}as ti) fields = 
   let projections = List.map
     (fun (loc,k,_,v) ->
	<:expr< let module S = $gen_printer ti v$ in
                S.pickle buffer (obj . $lid:k$) >>
     ) fields in
   let subexprs = List.map (fun (loc, k,_,_) -> (<:patt< $lid:k$ >>, <:expr< $lid:k$ >> )) fields in
   let constructions = List.fold_right
     (fun (loc, k, _, v) expr ->
       <:expr< let module S = $gen_printer ti v$ in 
               let $lid:k$ = S.unpickle stream in
                 $expr$ >>
     ) fields <:expr< { $list:subexprs$  } >>
in
<:str_item<
   value rec pickle buffer obj = 
        do { $list:projections$ }
   and unpickle stream =
          $constructions$
>>


let gen_polycase ({loc=loc} as ti) = function
  | (MLast.RfTag (name, _, params'), n) -> 
      let params = (List.map2 (fun p n -> (p, Printf.sprintf "v%d" n)) 
                       params' (range 0 (List.length params' - 1))) in
      let patt = (List.fold_left 
                     (fun patt (_,v) -> <:patt< $patt$ $lid:v$ >>) <:patt< `$uid:name$>> params) in (* the "uid" isn't really safe here *)
        (let tuple = tuple_expr loc (List.map (fun (_,p) -> <:expr< $lid:p$ >>) params)
          in (patt, None, <:expr< do { Pickle_int.pickle buffer $int:string_of_int n$ ; 
			               $gen_printers ti tuple params'$ }
	      >>))
          
  | (MLast.RfInh (<:ctyp< $lid:tname$ >> as ctyp), n) -> 
      (<:patt< (# $[tname]$ as $lid:tname$) >>, 
      None, 
      <:expr< let module S = $gen_printer ti ctyp$ in do {
              Pickle_int.pickle buffer $int:string_of_int n$ ;
              S.pickle buffer $lid:tname$
        } >>)
  | (MLast.RfInh ctyp, n) -> 
      let var, guard, expr = cast_pattern ti ctyp ~param:"x" in
        (var, guard, 
         <:expr< let module S = $gen_printer ti ctyp$ in do {
              Pickle_int.pickle buffer $int:string_of_int n$ ;
              S.pickle buffer $expr$
        } >>)


let gen_unpickle_polycase ({loc=loc} as ti) = function
  |  (MLast.RfTag (name, _, params'), n) -> 
       let params = (List.map2 (fun p n -> (p, Printf.sprintf "v%d" n)) 
                        params' (range 0 (List.length params' - 1))) in
       let expr = 
         List.fold_left
           (fun acc (_,v) -> <:expr< $acc$ $lid:v$ >>) 
         <:expr< `$uid:name$ >>
           params
       in
         (<:patt< $int:string_of_int n$ >>, None, 
         List.fold_right 
           (fun (ptype, var) expr ->
             <:expr<  let module S = $gen_printer ti ptype$ in
                      let $lid:var$ = S.unpickle stream in $expr$ >>)
           params
           expr)
  | (MLast.RfInh ctyp, n) -> 
    (<:patt< $int:string_of_int n$ >>, None,
    <:expr<  let module S = $gen_printer ti ctyp$ in
               (S.unpickle stream :> a) >>)


let gen_funs_poly ({loc=loc} as ti) (row,_) =
<:str_item<
  value rec pickle buffer =
         fun [ $list:List.map2 (curry (gen_polycase ti)) row (range 0 (List.length row - 1))$]
     and unpickle stream =
           match Pickle_int.unpickle stream with
             [ $list:List.map2 (curry (gen_unpickle_polycase ti)) row  (range 0 (List.length row - 1)) @ [unpickle_failure ti]$ ] 
>>

(* TODO: merge with gen_printer *)
let gen_module_expr ti = 
  gen_module_expr
    ~tyrec:(fun _ _ ti fields -> apply_defaults ti (gen_funs_record ti fields))
    ~tysum:(fun _ _ ti ctors  -> apply_defaults ti (gen_sum ti ctors))
    ~tyvrn:(fun _ _ ti row    -> apply_defaults ti (gen_funs_poly ti row))
    ti ti.rtype

let gen_instances loc tdl = gen_finstances ~gen_module_expr:gen_module_expr loc ~tdl:tdl

let _ = 
  begin
    instantiators := ("Pickle", gen_instances) :: !instantiators;
    sig_instantiators := ("Pickle", Sig_utils.gen_sigs "Pickle"):: !sig_instantiators;
  end
