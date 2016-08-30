(** Translate Links-Lambda to Lambda **)

let lambda_of_llambda : LLambda.program -> Lambda.lambda =
  fun prog ->
  let open LLambda in
  let open Lambda in
  let open LambdaDSL in
  let open Utility in
  let error s = failwith (Printf.sprintf "LLambda-to-Lambda translation error: %s" s) in
  let builtin fun_name = LambdaDSL.lookup "Builtins" fun_name in
  let identifier : LLambda.identifier -> Ident.t =
    fun {name = name' ; uid = uid' } -> create_ident name' uid'
  in
  let fresh_identifier name = create_ident name (Var.fresh_raw_var ()) in
  let rec translate : LLambda.program -> Lambda.lambda = function
    (* values *)
    | `Constant c -> lconstant constant c
    | `Unit -> lconstant (fun x -> x) unit
    | `Variable ident -> lvar (identifier ident)
    (*lprim (field 0) [lgetglobal "Links_/Globalfun"]*)
    | `Inject (label, v, _) -> lpolyvariant label (Some [translate v])
    | `Project (label, row) -> assert false
    | `Extend (fields, record) -> assert false
    | `Primitive prim -> primitive prim
    (* tail_computations *)
    | `Apply (f, args) -> lapply (translate f) (List.map translate args)
    | `If (cond, tbranch, fbranch) ->
       lif (translate cond)
           (translate tbranch)
           (translate fbranch)
    | `Case (v, clauses, default_clause) ->
       let v = translate v in
       let default_clause =
	 match default_clause with
	 | None -> lapply (pervasives "failwith") [lstring "Fatal error: Pattern-matching failed"]
	 | Some (b,c) -> llet (identifier b) v (translate c)
       in
       let label = fresh_identifier "_switch" in
       let switch_expr k =
         llet label (lproject 0 v) k
       in
       let vlabel = lvar label in
       switch_expr
	 (StringMap.fold
	    (fun label (b,c) matchfail ->
	      let label = linteger (hash_label label) in
	      lif (neq vlabel label) 
		matchfail
                (llet (identifier b)
                      (lproject 1 v)
                      (translate c)
                )
	    ) clauses default_clause
	 )
    (* bindings *)
    | `Sequence (lam, next) -> lseq (translate lam) (translate next)
    | `Let (ident, expr, body) ->
       llet (identifier ident)
            (translate expr)
            (translate body)
    | `Fun (args, body) -> lfun (List.map identifier args) (translate body)
    | `Rec (funs, body) ->
       let funs =
         List.fold_right
           (fun (ident,body) funs ->
             let ident = identifier ident in
             let body  = translate body in
             (ident,body) :: funs)
           funs []
       in
       lletrec funs (translate body)
    | `Wrong -> lapply (pervasives "failwith") [lstring "Unknown fatal error"]
    | `DoOperation (label, args) -> lperform (linteger (hash_label label)) (List.map translate args)
    | v -> error ("Unimplemented feature:\n" ^ (Show_llambda.show v))
  and primitive : LLambda.primitive -> Lambda.lambda = function
    | `Nil -> lnil
    | `Builtin name -> builtin name
    | `FnApply (name, args) ->
       let f = builtin name in
       let args = List.map translate args in
       lapply f args
    | `BinOp (binop, args) ->
       let args = List.map translate args in
       let prim = 
         match binop with
         | `Plus, Some `Int  -> Paddint
         | `Minus, Some `Int -> Psubint
         | `Mult, Some `Int  -> Pmulint
         | `Div, Some `Int   -> Pdivint
         | `Mod, _           -> Pmodint
         | `Plus, Some `Float  -> Paddfloat
         | `Minus, Some `Float -> Psubfloat
         | `Mult, Some `Float  -> Pmulfloat
         | `Div, Some `Float  -> Pdivfloat
         | `Eq, None          -> Pccall (prim_binary_op "caml_equal")
         | `Neq, None          -> Pccall (prim_binary_op "caml_notequal")
         | `Lt, None          -> Pccall (prim_binary_op "caml_lessthan")
         | `Gt, None          -> Pccall (prim_binary_op "caml_greaterthan")
         | `Le, None          -> Pccall (prim_binary_op "caml_lessequal")
         | `Ge, None          -> Pccall (prim_binary_op "caml_greaterequal")
         | `And, _            -> Psequand
         | `Or,  _            -> Psequor
         | _ -> assert false
       in
       lprim prim args
    | `Global (ident, num, module_name, get_set) ->
       let ident = identifier ident in
       let module_name = lgetglobal module_name in
       match get_set with
       | `Set -> lprim (set_field_imm num) [module_name ; lvar ident]
       | `Get -> lprim (field num) [ module_name ]
    | _ -> assert false
  and constant : LLambda.constant -> structured_constant =  function
    | `Int i      -> const_base int i
    | `Float f    -> const_base float f
    | `String s   -> string s
    | `Bool true  -> tt
    | `Bool false -> ff
    | `Char c     -> const_base char c
    | c -> error ("Unimplemented feature:\n" ^ Show_constant.show c)  
  in
  let exit_success = lconst ff in (* in this context "lconst ff" represents the exit code 0 *)
  translate prog
