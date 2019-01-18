(*pp deriving *)
(* Translation of Links IR into OCaml Lambda IR *)

open Links_core
open Utility


let ocaml_function module_name function_name = (module_name, function_name)

let ocaml_of_links_function f =
  let stdlib = "Pervasives" in
  let listlib = "List" in
  (* Links function, (module name, ocaml function) *)
  List.assoc f
    [   (*"print", ocaml_function stdlib "print_endline"*)
      "print", ("Builtins", "print")
	       ; "intToString", ocaml_function stdlib "string_of_int"
	       ; "floatToString", ocaml_function stdlib "string_of_float"
               ; "Concat", ocaml_function stdlib "@"
	       ; "^^", ocaml_function stdlib "^"
               ; "error", ocaml_function stdlib "failwith"
	       ; "hd", ocaml_function listlib "hd"
   	       ; "tl", ocaml_function listlib "tl"
               ; "length", ocaml_function listlib "length"
               ; "explode", ("Builtins", "explode")
               ; "implode", ("Builtins", "implode")
	     ]

let arith_ops =
  [ "+"
  ; "-"
  ; "*"
  ; "/"
(*  ; "^" *)
  ; "mod"
  ; "+."
  ; "-."
  ; "*."
  ; "/."
  (*  ; "^." *)
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

let logical_ops =
  [ "&&"
  ; "||"
  ]

let is_arithmetic_operation : string -> bool =
  fun op -> List.mem op arith_ops

let is_relational_operation : string -> bool =
  fun op -> List.mem op rel_ops

let is_logical_operation : string -> bool =
  fun op -> List.mem op logical_ops

let primop   : string -> Lambda.primitive option =
  fun op ->
  let open Lambda in
  try
    Some (
      if is_arithmetic_operation op then
	match op with
	| "+" -> Paddint
	| "-" -> Psubint
	| "*" -> Pmulint
	| "/" -> Pdivint Unsafe
	| "mod" -> Pmodint Unsafe
	| "+." -> Paddfloat
	| "-." -> Psubfloat
	| "*." -> Pmulfloat
	| "/." -> Pdivfloat
	| _ -> raise Not_found
      else if is_relational_operation op then
        match op with
        | "==" -> Pintcomp Ceq (*Pccall (LambdaDSL.prim_binary_op "caml_equal")*) (*Pintcomp Ceq*)
        | op -> let cmp =
                  match op with
                  | "<"  -> "caml_lessthan"
                  | ">"  -> "caml_greaterthan"
                  | "<=" -> "caml_lessequal"
                  | ">=" -> "caml_greaterequal"
                  | "<>" -> "caml_notequal"
                  | _ -> raise Not_found
                in
                Pccall (LambdaDSL.prim_binary_op cmp)
        (*	  Pintcomp (match op with
		  | "==" -> Ceq
		  | "<>" -> Cneq
		  | "<"  -> Clt
		  | ">"  -> Cgt
		  | "<=" -> Cle
		  | ">=" -> Cge
	          | _ -> raise Not_found)*)
      else if is_logical_operation op then
        match op with
        | "&&" -> Psequand
        | "||" -> Psequor
        | _ -> raise Not_found
      else raise Not_found
    )
  with
  | Not_found -> None

let is_primitive   : Var.var -> bool = Lib.is_primitive_var
let primitive_name : Var.var -> string option =
  fun var ->
  try
    Some (Lib.primitive_name var)
  with
  | Not_found -> None
(*
let translate (op_map,name_map) module_name ir =
  let open Lambda in
  let open LambdaDSL in
  let open Ir in
  let builtin fun_name = lookup "Builtins" fun_name in
  let op_map =
    snd
      (StringMap.fold
         (fun opname uid (pos,op_map) -> (pos+1,StringMap.add opname (uid,pos) op_map))
         op_map (0, StringMap.empty))
  in
  let lookup_op label =
    try
      Some (StringMap.find label op_map)
    with
    | _ -> None
  in
  let get_op_uid label = opt_map fst (lookup_op label) in
  let get_op_pos label = opt_map snd (lookup_op label) in
  let error s = failwith ("Internal compiler error: " ^ s ^ ".") in
  let ident (var_name,uid) = create_ident var_name uid in
  let ident_of_var var =
    try
      let name = IntMap.find var name_map in
      ident (name, var)
    with
    | _ -> error ("Cannot find name for var " ^ (string_of_int var))
  in
  let ident_of_binder b =
    let var = Var.var_of_binder b in
    match Var.name_of_binder b with
    | "" -> ident_of_var var
    | name -> ident (name, var)
  in
  let getglobal = lgetglobal (String.capitalize module_name) in
  let unit_binder = Var.fresh_binder (Types.unit_type, "_unit", `Local) in
  let fresh_ident name = ident (name, Var.fresh_raw_var ()) in

  let lproj_error =
    let msg = lstring "Fatal error: Projection failed." in
    let errfun = pervasives "failwith" in
    lfun [ident_of_binder unit_binder] (lapply errfun [msg])
  in
  let make_record translate map default_clause =
    let l' = fresh_ident "_lprime" in
    let eq a b = lprim (Pintcomp Ceq) [a ; b] in
    let switcher =
      StringMap.fold
        (fun l v clauses ->
          let l = linteger (hash_label l) in
          let v = translate v in
          lif (eq l (lvar l'))
            v
            clauses)
        map
        (lapply default_clause [lvar l'])
    in
    lfun [l'] switcher
  in
  let make_record_from_list translate xs =
    let map =
      List.fold_left
        (fun (i,map) x -> (i+1, StringMap.add (string_of_int i) x map))
        (1, StringMap.empty)
        xs
    in
    make_record translate (snd map) lproj_error
  in
  let project_from_record label row =
    let label = hash_label label in
    lapply row [linteger label]
  in
  let rec computation : computation -> lambda =
    fun (bs,tc) ->
    bindings bs (tail_computation tc)
  and tail_computation : tail_computation -> lambda =
    function
    | `Return v -> value v
    | `Special s -> special s
    | `Apply (f, args) ->
       let args' =
	 if List.length args > 0 then
	   List.map value args
	 else
	   [lconst unit]
       in
       (* First, figure out whether f is primitive *)
       begin
	 match is_primitive_function f with
	 | None     -> lapply (value f) args'
	 | Some uid ->
	    (* Next, figure out which type of primitive f is *)
	    let fname =
	      match primitive_name uid with
	      | Some name -> name
	      | None -> error ("Cannot find primitive name for var " ^ (string_of_int uid))
	    in
	    begin
	      match primop fname with
	      | Some instr -> lprim instr args'
	      | _ ->
		 begin
		   match fname with
		   | "Cons" -> lcons (List.hd args') (List.tl args') (*lprim box args'*)
                   | "Concat" -> let concat = pervasives "@" in
                                 lapply concat args'
                   | "^" -> failwith "Exponentiation is not supported"

(*		   | "random" ->
		      let random = lookup "Random" "float" in
		      lapply random [lfloat 1.0]*)
                   | "not" -> lprim Pnot args'
                   | "negate" -> lprim Pnegint args'
                   | "negatef" -> lprim Pnegfloat args'
(*                   | "^^"      -> let string_concat = pervasives "^" in
                                  lapply string_concat args'*)
		   | _ ->
		      try
			(*let (module_name, fun_name) = ocaml_of_links_function fname in*)
                        let f = builtin fname in
                        (*                        let f = lookup module_name fun_name in*)
			lapply f args'
		      with
		      | _ -> error ("Unsupported primitive function '" ^ fname ^ "'")
		 end
	    end
       end
    | `If (cond, trueb, falseb) ->
       lif (value cond) (computation trueb) (computation falseb)
    | `Case (v, clauses, default) ->
       let v = value v in
       let default =
	 match default with
	 | None -> lapply (pervasives "failwith") [lstring "Fatal error: Pattern-matching failed"]
	 | Some (b,c) -> llet (ident_of_binder b) v (computation c)
       in
       let v'' = ident ("_switch", Var.fresh_raw_var ()) in
       let switch_expr k =
         llet v'' (lproject 0 v) k
       (*llet v'' (lproject 0 v) k (* FIXME: assuming we always match on a "box" *)*)
       in
       switch_expr
	 (StringMap.fold
	    (fun v' (b,c) matchfail ->
	      lif (neq (lvar v'') (polyvariant v' None))
		matchfail
            (llet (ident_of_binder b) (lproject 1 v) (computation c))
	    ) clauses default
	 )
    | _ -> assert false
  and special : special -> lambda =
    function
    | `Wrong _ -> lapply (pervasives "failwith") [lstring "Fatal error."]
    | `DoOperation (label, args, _) ->
       let pos =
         match get_op_pos label with
         | Some pos -> pos
         | _ -> error ("Could not find unique location identifier for operation " ^ label)
       in
       let perform label args =
	 lperform (lprim (field pos)
		     [ getglobal ])
           [ make_record_from_list value args ]
       (*		  [ lprim box args ]*)
       in
       let id =
	 match get_op_uid label with
	 | Some id -> id
	 | None    -> error ("Cannot not find identifier for operation name " ^ label)
       in
    (*       lperform (ident (label, id)) (List.map value args) *)
(*       Lambda.(lprim (Pperform)
		     [ lprim (field 0)
		      [lprim (Lambda.Pgetglobal (Ident.create_persistent (String.capitalize module_name))) []]
         ])*)
       perform label args
    (*       perform label (List.map value args)*)
(*    | `Handle (v, (clauses : Ir.clause Ir.name_map), _) ->
       let (value_clause,clauses) = StringMap.pop "Return" clauses in
       let value_handler =
	 let (_,b, comp) = value_clause in
	 lfun [ident_of_binder b] (computation comp)
       in
       let exn_handler =
	 let exn = ident ("exn", Var.fresh_raw_var ()) in
	 lfun [exn] (lraise Lambda.Raise_reraise (lvar exn))
       in
       let eff_handler =
	 let eff  = ident ("eff", Var.fresh_raw_var ()) in
	 let cont = ident ("cont", Var.fresh_raw_var ()) in
	 let forward_effect = ldelegate eff cont in
	 let kid = ident ("_k", Var.fresh_raw_var ()) in
	 let k =
	   let param = ident ("param", Var.fresh_raw_var ()) in
           let kid'  = ident ("_k'", Var.fresh_raw_var ()) in
	   lfun [param]
             (llet kid'
                (lapply (obj "clone") [lvar kid])
		(lapply (pervasives "continue") [lvar kid' ; lvar param])) (* Multi-shot continuation *)
             (*             (lapply (pervasives "continue") [lvar kid ; lvar param ])*) (* Linear continuation *)
	 in
	 let bind_k scope =
	   llet ~kind:Alias
		kid
		(lprim (makeblock 0 Mutable) [lvar cont])
		scope
	 in
	 let compile (clauses : Ir.clause Ir.name_map) =
	   StringMap.fold
	     (fun label (cc,b,(bs,tc)) lam ->
	       let kid =
                 match cc with
                 | `Effect kb -> Some (ident_of_binder kb)
                 | _   -> None
               in
	       let clause body =
		 let bind_args_in body =
		   if List.length bs > 0 then
		     llet (ident_of_binder b)
			  (lproject 1 (lvar eff))
			  body
		   else
		     body
		 in
		 bind_args_in
		   (if is_some kid
                    then
                       let (Some kid) = kid in
                       llet kid
		         k
		         body
                    else
                       body)
	       in
	       let effname = lproject 0 (lvar eff) in
               let clauselabel =
                 let pos =
                   match get_op_pos label with
                   | Some pos -> pos
                   | _ -> error ("Could not find unique location identifier for operation " ^ label)
                 in
                 lprim (field pos) [ getglobal ]
               in
	       lif (eq effname clauselabel)
		   (clause (bindings bs (tail_computation tc)))
		   lam
	     )
	     clauses forward_effect
	 in
	 lfun [eff ; cont] (bind_k (compile clauses))
       in
       let alloc_stack =
	 lalloc_stack
	   value_handler
	   exn_handler
	   eff_handler
       in
       let thunk =
	 let unitb = ident_of_binder unit_binder in
	 lfun [unitb] (lapply (value v) [lconst unit])
       in
       lresume [ alloc_stack
	       ; thunk
	       ; lconst unit]
    | _ -> assert false
  and value : value -> lambda =
    function
    | `Constant c -> lconstant constant c
    | `Variable var ->
       if is_primitive var then
	 match primitive_name var with
	 | Some "Nil" -> lnil
	 | Some prim ->
            (*	    let (module_name, fun_name) = ocaml_of_links_function prim in*)
	 (*lookup module_name fun_name*)
            builtin prim
         | Some name -> error ("Unknown primitive " ^ name)
         | None -> error ("Cannot find primitive name for var " ^ (string_of_int var))
       else
	 lvar (ident_of_var var)*)
    | `TAbs (_, v)
    | `TApp (v, _) -> value v
    | `ApplyPure (f, args) -> tail_computation (`Apply (f, args))
    | `Inject (label, v, _) ->
       lpolyvariant label (Some [value v])
    | `Project (label, row) -> project_from_record label (value row)
    (*lproject ((int_of_string label)-1) (value row) (* FIXME: Assuming tuples! *)*)
    | `Extend (map, row) ->
(*       let lerror =
         let msg = lstring "Fatal error: Projection failed." in
         let errfun = pervasives "failwith" in
         lfun [ident_of_binder unit_binder] (lapply errfun [msg])
       in
       let l' = fresh_ident "_lprime" in
       let eq a b = lprim (Pintcomp Ceq) [a ; b] in
       let switcher default_clause =
         StringMap.fold
           (fun l v clauses ->
             let l = linteger (hash_label l) in
             let v = value v in
             lif (eq l (lvar l'))
               v
               clauses)
           map
           default_clause
         in*)
       let record = make_record value map in
       begin
         match row with
         | Some rho -> record (value rho)
         | None -> record lproj_error
       end
(*       begin
         match row with
         | Some pho ->
            let rest_row = lapply (value pho) [lvar l'] in
            lfun [l'] (switcher rest_row)
         | None -> lfun [l'] (switcher lerror)
         end*)

(*       let vs = StringMap.to_list (fun k v -> value v) map in
       begin
       match row with
       | Some r -> error "Record extension not yet implemented."
       | None -> lprim box vs
         end*)
    | `Coerce (v,_) -> value v
    | v -> error ("Unimplemented feature:\n" ^ (Ir.Show_value.show v))
  and bindings : binding list -> (lambda -> lambda) =
    function
    | b :: bs -> fun k -> binding b (bindings bs k)
    | [] -> fun k -> k
  and binding : binding -> (lambda -> lambda) =
    let translate_fundef (_, (_, params, body), _, _) =
      let params' =
	if List.length params > 0 then
	  params
	else
	  [unit_binder]
      in
      (lfun (List.map ident_of_binder params') (computation body))
    in
    fun b k ->
    match b with
    | `Let (b, (_, tc)) ->
       llet
	 (ident_of_binder b)
	 (tail_computation tc)
	 k
    | `Fun ((b, _, _, _) as f) ->
       llet
	 (ident_of_binder b)
	 (translate_fundef f)
	 k
    | `Rec funs ->
       let funs' =
	 List.fold_right
	   (fun ((b, _, _, _) as f) funs ->
	     (ident_of_binder b, translate_fundef f) :: funs)
	   funs []
       in
       lletrec funs' k
    | _ -> assert false
  and constant : Constant.constant -> structured_constant =
    function
    | `Int i      -> const_base int i
    | `Float f    -> const_base float f
    | `String s   -> string s
    | `Bool true  -> tt
    | `Bool false -> ff
    | `Char c     -> const_base char c
    | _ -> assert false
  and is_primitive_function : value -> int option =
    function
    | `Variable var -> if is_primitive var then Some var else None
    | `TAbs (_,v)
    | `TApp (v,_) -> is_primitive_function v
    | v -> error ("Unknown, possibly primitive, node:\n " ^ (Show_value.show v))
  and program : program -> lambda =
    fun prog ->
(*    Compmisc.init_path true;
      Ident.reinit ();*)
    (** preamble: Declare operations **)
    let preamble =
      fun k ->
      (StringMap.fold
	 (fun label (uid,pos) body ->
	   let efflabel = (String.capitalize module_name) ^ "." ^ label in
	   lseq
	     (llet (ident (label,uid))
		   (leffect efflabel)
		   (lprim (set_field_imm pos) [(lprim (Lambda.Pgetglobal (Ident.create_persistent (String.capitalize module_name))) []) ; lvar (ident (label,uid))]))
	     body
	 )
      ) op_map k
    in
(*    let ops = StringMap.fold
		(fun label (_,pos) xs -> (label,pos) :: xs) op_map []

    in*)
    let random_init k =
      (*let init = lookup "Random" "self_init" in
      llet (ident ("_rand_init", Var.fresh_raw_var ()))
	(lapply init [lconst unit])*)
	   k
    in
    (** translate to lambda **)
    let exit_success = lconst ff in (* in this context "lconst ff" represents the exit code 0 *)
    lseq (preamble (random_init (computation prog))) exit_success
  in
  program ir


let transform tenv ir =
  let transformer =
object (o)
  inherit Ir.Transform.visitor(tenv) as super
  method value =
    function
    | `Variable var ->
       if is_primitive var then
         match primitive_name var with
         | Some (">" as name) ->
            begin
              print_endline name;
              failwith (Types.string_of_datatype (o#lookup_type var))
            end
         | Some _   -> super#value (`Variable var)
         | None -> failwith "ERROR"
       else
         super#value (`Variable var)
    | v -> super#value v
end
  in
  transformer#program ir

*)
let lambda_of_ir module_name prog =
  let (openv, nenv, globals) =
    let gather = Gather.TraverseIr.gather prog in
    gather#get_operation_env, gather#get_name_map, gather#get_globals
  (*    (gather#get_operation_env, Gather.TraverseIr.binders_map prog)*)
  in
  let nenv = StringMap.fold (fun k v nenv -> IntMap.add v k nenv) openv nenv in
  let globals =
    let size = IntMap.size globals in
    let (_, globals) = StringMap.fold (fun _ v (pos,globals) -> (pos+1,IntMap.add v pos globals)) openv (size, globals) in
    globals
  in
  (*  let _ = transform tenv prog in*)
  (*  translate maps module_name prog*)

(*  let ir_translator = new translator (invert env) in
  ir_translator#program "Helloworld" prog*)
  let llambda = Irtollambda.ir_llambda module_name globals nenv openv prog in
  (1 + IntMap.size globals, Llambdatolambda.lambda_of_llambda module_name (globals,openv) llambda)
