(*pp deriving *)
(* Translation of Links IR into OCaml Lambda IR *)
module L = Lambda	     

(* Hello world in Links
([`Fun ((1790, ((String) ~> (), "myprint", `Global )),
         ([(6429, (`Unl , `Any ), `Row ...)],
           [(1789, (String, "", `Local ))],
           ([`Let ((1788, (String, "s", `Local )),
                    ([], `Return `Variable 1789))],
             `Apply (`TApp (`TAbs ([(6427, (`Unl , `Any ), `Row ...)],
                                    `TApp (`Variable 61,
                                            [`Row ({}, ..., false)])),
                             [`Row ({}, ..., false)]), [`Variable 1788]))),
         None, `Unknown )],
  `Apply (`TApp (`TAbs ([(6431, (`Unl , `Any ), `Row ...)],
                         `TApp (`Variable 1790, [`Row ({}, ..., false)])),
                  [`Row ({}, ..., false)]),
           [`Constant `String "Hello World!"]))

 *)

type ocaml_function = { module_name : string ; function_name : string }
let ocaml_function module_name function_name = {module_name = module_name ; function_name = function_name}			
	         	   
let ocaml_of_links_function f =
  let stdlib = "Pervasives" in
  (* Links function, (module name, ocaml function) *)
  List.assoc f
	     [   "print", ocaml_function stdlib "print_endline"
	       ; "intToString", ocaml_function stdlib "string_of_int"
	     ]
	     
let compenv modulename function_name =
  Compmisc.init_path false;
  Ident.reinit ();
  Env.lookup_value
    (Longident.(Ldot (Lident modulename, function_name)))
    Env.empty

let arith_ops =
  [ "+" 
  ; "-"
  ; "*"
  ; "/"
  ; "^"
  ; "mod"
  ; "+."
  ; "-."
  ; "*."
  ; "/."
  ; "^."
  ]

(*let string_ops =
  ["^^"] *)
    
let rel_ops =
  [ "=="
  ; "<>"
  ; "<"
  ; ">"
  ; "<="
  ; ">="
  ]
    
let is_arithmetic_operation : string -> bool =
  fun op -> List.mem op arith_ops

let is_relational_operation : string -> bool =
  fun op -> List.mem op rel_ops

(*let is_string_operation : string -> bool =
  fun op -> List.mem op string_ops*)

let is_primitive : Var.var -> bool = Lib.is_primitive_var
let primitive_name : Var.var -> string option
  = fun var ->
  try Some (Lib.primitive_name var) with
  | _ -> None
    
let lam_apply f xs = L.Lapply (f, xs, L.no_apply_info)
	     
class translator env =
object ((o : 'self))
  val env = env

  (*method lookup var = Env_links.Int.find env var*)

  method program : string -> Ir.computation -> L.lambda =
    fun module_name comp ->
    let id = Ident.( { name = module_name ; flags = 1 ; stamp = 0 } ) in
    Lambda.(Lprim (Psetglobal id, [o#computation comp]))
					 
  method computation : Ir.computation -> L.lambda =
    fun (bs,tc) ->
    if List.length bs > 0
    then L.(Lsequence (o#bindings bs, o#tail_computation tc))
    else o#tail_computation tc

  method tail_computation : Ir.tail_computation -> L.lambda =
    function
    | `Apply (f, args) -> lam_apply (o#value f) (List.map o#value args)
    | _ -> assert false

  method special : Ir.special -> L.lambda =
    function
    | _ -> assert false
		  
  method value : Ir.value -> L.lambda =
    function
    | `TApp (v,_)
    | `TAbs (_,v) -> o#value v
    | `Constant c -> L.Lconst (o#constant c)
    | `Variable var ->       
       (* Figure out whether the variable is a primitive *)
       if is_primitive var then
	 let (Some name) = primitive_name var in
	 o#primitive name
       else
	 let name = "_x" ^ (string_of_int var) in
	 let ident = Ident.create_persistent name in
	 L.(Lvar ident)
    | `ApplyPure (f, args) ->
       begin
	 match o#is_primitive_operation f with
	 | Some name -> L.(Lprim (o#primitive_operation name, List.map o#value args))
	 | None -> o#tail_computation (`Apply (f,args))
       end
    | _ -> assert false
		  
  method constant : Constant.constant -> L.structured_constant =
    function
    | `String s -> L.Const_immstring s
    | `Int i    -> L.Const_base (Asttypes.Const_int i)
    | _ -> assert false

  method binder : Ir.binder -> Ident.t =
    fun b ->
    let name = "_x" ^ (string_of_int (Var.var_of_binder b)) in
    Ident.create_persistent name
				 		  
(*  method binding : Ir.binding -> (L.lambda -> L.lambda) =
    function
    | `Fun (b, (_, args, body), _,_) ->
       fun scope ->
       L.(Llet (Strict
	       , o#binder b
	       , Lfunction ({ kind   = Curried
			    ; params = List.map o#binder args
			    ; body   = o#computation body
			   })
	       , scope
	       ))
    | _ -> assert false*)
		  
  method bindings : Ir.binding list -> L.lambda =
    function
    | _ -> assert false
(*    | bs -> let lam = List.fold_left (fun lam b -> (o#binding b) lam) (fun x -> x) bs in
	    lam L.(Lconst (L.Const_base (Asttypes.Const_int 0)))
    | [] -> failwith "Empty list of bindings"*)
		  
  method primitive : string -> L.lambda =
    fun prim ->
    let {module_name ; function_name} = ocaml_of_links_function prim in
    L.(transl_path ~loc:Location.none Env.empty (fst (compenv module_name function_name)))

  method primitive_operation : string -> L.primitive =
    function     
    | "+" -> L.Paddint
    | _ -> assert false
     
  method is_primitive_operation : Ir.value -> string option
    = function
    | `Variable var ->
       begin
	 match primitive_name var with
	 | Some name -> if is_arithmetic_operation name || is_relational_operation name
			then Some name
			else None
	 | None -> None
       end
    | `TApp (v,_)   -> o#is_primitive_operation v
    | _ -> None
end

let invert env =
  let module Env = Env_links in
  Env.String.fold
    (fun name var env ->
      if Env.Int.has env var then
        failwith ("(invert_env) duplicate variable in environment")
      else
        Env.Int.bind env (var, name))
    env Env.Int.empty	    

(* Builds a map from Var ids to names *)    
let binders_map prog =
  let o =
    object (o)
      inherit Ir.Transform.visitor(Env_links.Int.empty) as super
							     
      val map : (Var.var * string) list = []

      method get_map = map
					    
      method with_map env =
	{< map = map >}
	  
      method bindings =
	function
	| b :: bs ->
	   begin
	     match b with
	     | `Let (b, _)
             |  `Alien (b,_)
             | `Fun (b, _, _, _) -> (o#with_map ((Var.var_of_binder b, Var.name_of_binder b)::map))#bindings bs
	     | `Rec ((b, _, _, _) :: rest) ->
		let funs = List.map (fun f -> `Fun f) rest in
		let (_,o) = (o#with_map ((Var.var_of_binder b, Var.name_of_binder b)::map))#bindings funs in
		o#bindings bs
	     | _ -> assert false
	   end	 
	| [] -> [], o
    end
  in
  let (_,_,o) = ((o#computation prog)) in
  Utility.IntMap.from_alist o#get_map
    
let lambda_of_ir env prog =
  let ir_translator = new translator (invert env) in
  ir_translator#program "Helloworld" prog
