#load "pa_extend.cmo";;
#load "q_MLast.cmo";;
open Deriving

let rec last : 'a list -> 'a = function
  | []    -> raise (Invalid_argument "last")
  | [x]   -> x
  | _::xs -> last xs

let fill_bounded_template loc tname first last = 
<:str_item< declare
             open Bounded;
             module $uid:"Bounded_"^ tname$ = 
               (struct 
                  type a = $lid:tname$; 
                  value minBound = $uid:first$; 
                  value maxBound = $uid:last$; 
               end : Bounded with type a = $lid:tname$ );
             end >>

let gen_bounded_instance = function
  | ((loc,tname), (_::_), _, _) -> error loc ("Not generating Bounded instance for polymorphic type "^ tname)
  | ((loc, tname), tvars, <:ctyp< [ $list:ctors$ ] >> , _) -> 
      let ((_,first,_), (_,last,_)) = (List.hd ctors, last ctors) in 
        begin
          List.iter (function 
                      | (loc, name, [])   -> ()
                      | (loc, name, args) -> error loc ("Not generating Bounded instance for "^ tname
                                                         ^" because constructor "^ name ^" is not nullary"))
            ctors;
          fill_bounded_template loc tname first last
        end
  | ((loc, tname), _, _, _) -> error loc ("Not generating Bounded instance for non-sum type "^ tname)

let gen_bounded_instances loc : instantiator =
  fun tdl -> <:str_item< declare $list:List.map gen_bounded_instance tdl$ end >>

let _ =
  begin
    instantiators := ("Bounded", gen_bounded_instances):: !instantiators;
    sig_instantiators := ("Bounded", Sig_utils.gen_sigs "Bounded"):: !sig_instantiators;
  end
