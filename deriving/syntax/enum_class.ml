#load "pa_extend.cmo";
#load "q_MLast.cmo";
open Deriving;

value fill_enum_template loc tname numbering = <:str_item< 
  declare
    open Enum;
    module $uid:"Enum_"^ tname$ = 
      EnumDefaults (struct 
                      type a = $lid:tname$; 
                      value numbering = $numbering$; 
                    end);
  end>>;

value gen_enum_instance = fun [
  ((loc, tname), [_::_], _, _) -> error loc ("Not generating enumeration for polymorphic type "^ tname)
| ((loc, tname), tvars, <:ctyp< [ $list:ctors$ ] >>, constraints) ->
   let numbering = 
     List.fold_right2 (fun n ctor list -> (match ctor with [
                                             (loc, name, [])    -> <:expr< [($uid:name$, $int:string_of_int n$) :: $list$] >>
                                           | (loc, name, args)  -> (error
                                                                      loc ("Not generating Enum instance for "^ tname
                                                                           ^" because constructor "^ name ^" is not nullary"))]))
       (range 0 (List.length ctors - 1)) ctors <:expr< [] >> in
     fill_enum_template loc tname numbering
| ((loc, tname), _, _, _) -> error loc ("Not generating Enum instance for non-sum type "^ tname)
];

value gen_enum_instances loc : instantiator = fun tdl ->
  <:str_item< declare $list:List.map gen_enum_instance tdl$ end >>;

instantiators.val :=   [("Enum"    , gen_enum_instances):: instantiators.val];
