(*pp deriving *)
open Utility
(*module Env = Env
open Env	       *)

type name_map = string intmap
(*		deriving (Show)*)
                
module TraverseIr = struct
  (** TODO: Remove binders_map after the bug in the class below has been fixed. **)
  let binders_map prog =    
    let rec computation : (Ir.var * string) list -> Ir.computation -> (Ir.var * string) list =
      fun map (bs, tc) ->
      tail_computation (bindings map bs) tc
    and tail_computation : (Ir.var * string) list -> Ir.tail_computation -> (Ir.var * string) list =
      fun map ->
      function
      | `Case (v, clauses, default_clause) ->
	 let map = Utility.StringMap.fold (fun _ (b,c) map -> computation (binder b :: map) c) clauses map in
	 map @ (Utility.from_option [] (Utility.opt_map (fun (b,c) -> computation (binder b :: map) c) default_clause))
      | `If (v, c1, c2) -> (computation map c2) @ (computation map c2)
      | `Special s -> special map s
      | _ -> map
    and special : (Ir.var * string) list -> Ir.special -> (Ir.var * string) list =
      fun map ->
      function
(*      | `Handle (v, clauses, _) ->
	 StringMap.fold
           (fun _ (cc,kb,c) map ->
             let map =
               match cc with
               | `Effect kb -> binder kb :: map
               | _ -> map
             in
             map @ (computation map c)) clauses map *)
      | _ -> map
    and bindings : (Ir.var * string) list -> Ir.binding list -> (Ir.var * string) list  =
      fun map -> 
      function
      | b :: bs ->
	 begin
	   match b with
	   | `Let (b, (_,tc)) -> bindings ((binder b)::map @ tail_computation map tc) bs
(*           | `Alien (b,_) -> assert false*)
           | `Fun (b, (_, args, comp), _, _) ->
	      let map = computation ((binder b :: (List.map binder args)) @ map) comp in
	      bindings map bs
           | `Rec ((b, (_, args, comp), _, _) :: rest) ->	      
	      let funs = List.map (fun f -> `Fun f) rest in
	      let map = computation map comp in
	      let map = bindings (binder b :: (List.map binder args) @ map) funs in
	      bindings map bs
	   | _ -> assert false
	 end
      | [] -> map
    and binder : Ir.binder -> (Ir.var * string) =     
      fun b ->
      match Var.name_of_binder b with
      | "" -> (Var.var_of_binder b, "_v")
      | name -> (Var.var_of_binder b, name)
    in
    let map = computation [] prog in
    (*    print_endline (Show_name_map.show map); *)
    IntMap.from_alist map
  
  class gatherer =
  object ((o : 'self_type))

    val globals : int IntMap.t = IntMap.empty
    method add_global : int -> 'self_type =
      fun uid ->
      let pos = IntMap.size globals in
      {< globals = IntMap.add uid pos globals >}
    method get_globals : int IntMap.t = globals    
           
    val name_map : string IntMap.t = IntMap.empty
    method add_binder : int -> string -> 'self_type =
      fun uid name ->
      (*      let _ = print_endline (Show_name_map.show name_map) in*)
      {< name_map = IntMap.add uid name name_map >}

    method get_name_map : string IntMap.t =
      name_map
					 	
    val operations = StringSet.empty
		       
    method add_operation : string -> 'self_type =
      fun name ->
      if String.compare name "Return" = 0 then
        {< operations = operations >}
      else
        {< operations = StringSet.add name operations >}

    method get_operation_env : int StringMap.t  =
      let operations = StringSet.fold (fun s acc -> (s, Var.fresh_raw_var ()) :: acc) operations [] in
      StringMap.from_alist operations
	   
    method computation : Ir.computation -> 'self_type =
      fun (bs,tc) ->
      let o = o#bindings bs in
      o#tail_computation tc
					     
    method tail_computation : Ir.tail_computation -> 'self_type =
      function
      | `Case (v, clauses, default_clause) ->
	 let o' = o#value v in
	 (* Collect binders *)
	 let collect_from = fun o (b,c) -> (o#binder b)#computation c in
	 let o'' = StringMap.fold (fun _ clause o -> collect_from o clause) clauses o' in
	 from_option o'' (opt_map (collect_from o'') default_clause)
      | `If (v, c1, c2) ->
         let o = o#value v in
	 (o#computation c1)#computation c2
      | `Special s -> o#special s
      | _ -> o

    method special : Ir.special -> 'self_type =
      function
(*      | `Handle (v, clauses, _) ->
	 let o = o#value v in
	 (* Collect binders *)
	 let o = StringMap.fold
           (fun opname (cc,b,c) o ->
             let o = o#add_operation opname in
             let o =
               match cc with
               | `Effect kb -> o#binder kb
               | _ -> o
             in
             (o#binder b)#computation c
           ) clauses o
         in
         o*)
      | `DoOperation (name, args, _) ->
	 o#add_operation name
      | _ -> o
		    
    method value : Ir.value -> 'self_type =
      function
      | _ -> o

    method bindings : Ir.binding list -> 'self_type =
      function
      | b :: bs -> (o#binding b)#bindings bs
      | [] -> o


    method binding : Ir.binding -> 'self_type =      
      function
      | `Let (b, (_, tc)) ->
         (o#binder b)#tail_computation tc
      (*| `Alien (b,_) -> o#binder b*)
      | `Fun (b, (_, args, comp), _, _) ->
         let o = o#binder b in
         let o = List.fold_left (fun o b -> o#binder b) o args in
         o#computation comp
      | `Rec ((b, (_, args, comp), _, _) :: rest) ->	      
	 let funs = List.map (fun f -> `Fun f) rest in
	 let o = o#computation comp in
         let o = o#binder b in
         let o = List.fold_left (fun o b -> o#binder b) o args in
	 o#bindings funs
      | _ -> assert false

    method binder : Ir.binder -> 'self_type =
      fun b ->
        let uid = Var.var_of_binder b in
        (*let _ = print_endline ("Collected " ^ (string_of_int uid)) in*)
        let name =
          match Var.name_of_binder b with
	  | "" -> "_v"
	  | n  -> n
        in
        o#add_binder uid name

    method program : Ir.program -> 'self_type =
      fun ((bs,_) as comp) ->
      let o = o#toplevel_bindings bs in
      o#computation comp

    method toplevel_binding : Ir.binding -> 'self_type = function
      | `Let (b, (_, _)) -> o#toplevel_binder b
(*      | `Alien (b,_) -> o#toplevel_binder b*)
      | `Fun (b,_, _, _) -> o#toplevel_binder b
      | `Rec ((b, _, _, _) :: rest) ->
         let o = o#toplevel_binder b in
	 List.fold_left (fun o (b, _, _, _) -> o#toplevel_binder b) o rest
      | _ -> assert false

    method toplevel_bindings : Ir.binding list -> 'self_type =  function
      | b :: bs -> (o#toplevel_binding b)#toplevel_bindings bs
      | [] -> o

    method toplevel_binder : Ir.binder -> 'self_type =
      fun b ->
      let var = Var.var_of_binder b in
      o#add_global var
  end
    
  let gather ir =
    new gatherer#program ir
end
