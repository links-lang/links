#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open Deriving

let gen_instance  ({loc=loc; tname=tname; atype=atype; rtype=rtype; argmap=params}) =
  let rec gen = function
    | <:ctyp< [ $list:_$ ] >>  (* sum *)
    | <:ctyp< { $list:_$ } >> (* record *)
    | <:ctyp< [= $list:_$ ] >>   (* polymorphic variant *) ->
      let paramList = 
        List.fold_right 
          (fun (_,(_,name)) cdr ->
             <:expr< [$uid:name$.typeRep()::$cdr$] >>) 
          params
        <:expr< [] >>  in
        <:module_expr< 
        struct
          type a = $atype$; 
(*          value typeRep = let t = TypeRep (Tag.fresh(), $paramList$) in fun _ -> t;*)
          value typeRep = 
            let rep = ref None in 
            fun _ -> 
             (match rep.contents with
              [ None -> let t = TypeRep (Tag.fresh(), $paramList$) in 
                        do {rep.contents := Some t; t}
              | Some r -> r ]);
        end >>
    | <:ctyp< '$lid:name$ >> ->
      <:module_expr< $uid:snd (List.assoc name params)$ >>
    | <:ctyp< $lid:name$ >> (* typename *) -> 
      let name = "Typeable_" ^ name in <:module_expr< $uid:name$ >>
    | <:ctyp< $f$ $p$ >> (* application *) ->
      <:module_expr< $gen f$ $gen p$ >>
    | <:ctyp< $uid:m$ . $t$ >> ->
      <:module_expr< $uid:m$ . $gen t$ >>
    | <:ctyp< ( $list:params$ ) >> (* tuple *) ->
      (List.fold_left 
         (fun s param -> <:module_expr< $s$ $param$ >>)
       <:module_expr< $uid:Printf.sprintf "Typeable_%d" (List.length params)$ >>
         (List.map gen params))
    | _                            -> error loc ("Cannot currently generate typeable instances for "^ tname)
  in <:module_expr< Typeable_defaults($gen rtype$) >>

let gen_instance tdl ((loc, tname), params, ctyp, constraints) =
  let currents = List.map (fun ((_,tname),_,_,_) -> (tname, "Typeable_" ^ tname)) tdl in
  let params = param_names params in
  let atype = gen_type_a loc <:ctyp< $lid:tname$ >> params in
  let ltype = gen_type_l tname params in
  let struct_expr = gen_instance {loc=loc; tname=tname; ltype=ltype; atype=atype; rtype=ctyp; argmap=params; currents=currents} in
    ("Typeable_"^ tname,
     (let rhs = <:module_type< (Typeable with type a = $atype$) >> in
        Sig_utils.gen_functor_type loc "Typeable" params rhs),
     gen_functor loc "Typeable" params struct_expr)
      
let gen_instances loc : instantiator = 
  begin
    fun tdl ->
      let mods = List.map (gen_instance tdl) tdl in
        <:str_item< declare open Typeable; open Primitives; module rec $list:mods$; end >>
  end

let _ = 
  begin
    instantiators := ("Typeable", gen_instances):: !instantiators;
    sig_instantiators := ("Typeable", Sig_utils.gen_sigs "Typeable"):: !sig_instantiators;
  end
