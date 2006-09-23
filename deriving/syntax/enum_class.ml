#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open Deriving

let fill_enum_template loc tname numbering = <:str_item< 
  declare
    open Enum;
    module $uid:"Enum_"^ tname$ = 
      EnumDefaults (struct 
                      type a = $lid:tname$; 
                      value numbering = $numbering$; 
                    end);
  end>>

let gen_enum_instance = function
  | ((loc, tname), _::_, _, _) -> error loc ("Not generating enumeration for polymorphic type "^ tname)
  | ((loc, tname), tvars, <:ctyp< [ $list:ctors$ ] >>, constraints) ->
      let numbering = 
        List.fold_right2 (fun n ctor list -> (match ctor with
                                           | (loc, name, [])    -> <:expr< [($uid:name$, $int:string_of_int n$) :: $list$] >>
                                           | (loc, name, args)  -> (error
                                                                      loc ("Not generating Enum instance for "^ tname
                                                                           ^" because constructor "^ name ^" is not nullary"))))
       (range 0 (List.length ctors - 1)) ctors <:expr< [] >> in
     fill_enum_template loc tname numbering
  | ((loc, tname), _, _, _) -> error loc ("Not generating Enum instance for non-sum type "^ tname)

let gen_enum_instances loc : instantiator = fun tdl ->
  <:str_item< declare $list:List.map gen_enum_instance tdl$ end >>

let _ = 
  instantiators := ("Enum", gen_enum_instances):: !instantiators
