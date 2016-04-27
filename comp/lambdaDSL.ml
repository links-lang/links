(** Shallow embedding of a subset of OCaml Lambda **)
open Lambda
open Asttypes       

module Option = struct
  let fmap f = function
    | Some x -> Some (f x)
    | None   -> None

  let alt f g =
    match f,g with
    | Some _, _ -> f
    | _, Some _ -> g
    | _,_       -> None
end
       
(*module LambdaDSL = struct*)
  let lvar : Ident.t -> lambda
    = fun i -> Lvar i

  let lconst : structured_constant -> lambda
    = fun c -> Lconst c

  let lapply : ?apply_info:apply_info -> lambda -> lambda list -> lambda =
    fun ?(apply_info=no_apply_info) f args ->
    Lapply (f, args, apply_info)
   
  let lfunction : ?kind:function_kind -> Ident.t list -> lambda -> lambda =
    fun ?(kind=Curried) params body -> Lfunction ({ kind   = kind
						  ; params = params
						  ; body   = body
						 })
  let int : int -> constant =
    fun i -> Const_int i

  let float : float -> constant =
    fun f -> Const_float (string_of_float f)

  let string : string -> structured_constant =
    fun s -> Const_immstring s

  let const_base : 'a . ('a -> constant) -> 'a -> structured_constant =
    fun f c -> Const_base (f c)
		       
  let unit : structured_constant = Lambda.const_unit
  let tt   : structured_constant = Const_pointer 1
  let ff   : structured_constant = Const_pointer 0

  let lconstant : 'a . ('a -> structured_constant) -> 'a -> lambda =
    fun f a -> lconst (f a)

  let linteger : int -> lambda = lconstant (const_base int)
  let lfloat   : float -> lambda = lconstant (const_base float)
  let lstring  : string -> lambda = fun s -> lconst (string s)
  let lbool : bool -> lambda =
    function
    | true -> lconst tt
    | false -> lconst ff
		      
  let makeblock : int -> mutable_flag -> primitive =
    fun tag flag -> Pmakeblock (tag, flag)

  let box : primitive = makeblock 0 Immutable
  let lprim : primitive -> lambda list -> lambda =
    fun prim args -> Lprim (prim, args)
			       
  let field : int -> primitive =
    fun i -> Pfield i

  let lproject : int -> lambda -> lambda =
    fun label row -> lprim (field label) [row]
		    
						 
  let polyvariant : string -> lambda list option -> lambda =
    fun label args ->
    let id = linteger (Btype.hash_variant label) in
    match args with
    | Some args -> lprim box ([id; lprim box args])
    | None -> id

  let lookup : string -> string -> lambda =
    fun module_name fun_name ->
    let lfun, _ = Env.lookup_value
		    (Longident.(Ldot (Lident module_name, fun_name)))
		    Env.empty
    in transl_path Env.empty lfun
		    
  let pervasives : string -> lambda =
    fun fun_name -> lookup "Pervasives" fun_name

  let lif : lambda -> lambda -> lambda -> lambda =
    fun cond trueb falseb -> Lifthenelse (cond, trueb, falseb)

  let lseq : lambda -> lambda -> lambda =
    fun e1 e2 -> Lsequence (e1, e2)

  let llet : ?kind:let_kind -> Ident.t -> lambda -> lambda -> lambda =
    fun ?(kind=Strict) b expr body -> Llet (kind, b, expr, body)

  let lletrec : (Ident.t * lambda) list -> lambda -> lambda =
    fun funs body -> Lletrec (funs, body)

  let neq : lambda -> lambda -> lambda =
    fun lhs rhs ->
    lprim (Pintcomp Cneq) [lhs;rhs]
			     
  (*let parith : string -> primitive option =
    fun op ->
    let linst =
      match op with
      | "+" -> Paddint
      | "-" -> Psubint
      | "*" -> Pmulint
      | "/" -> Pdivint
      | "mod" -> Pmodint		 
      | "+." -> Paddfloat
      | "-." -> Psubfloat
      | "*." -> Pmulfloat
      | "/." -> Pdivfloat
      | _ -> assert false
    in Some linst

  let pcmp : string -> comparison option =
    fun op ->
    let cmp = match op with
      | "==" -> Ceq
      | "!=" -> Cneq
      | "<"  -> Clt
      | ">"  -> Cgt
      | "<=" -> Cle
      | ">=" -> Cge
      | _ -> assert false
    in Some cmp

(*  let larithmetic : string -> lambda list -> lambda =
    fun op args -> lprim (parith op) args

  let lcompare : string -> lambda list -> lambda =
    fun op args -> lprim (pcmp op) args*)

  let pop : string -> primitive option =
    let fmap = Option.fmap in
    let (<|>) = Option.alt in
    fun op -> (parith op) <|> (fmap (fun cmp -> Pintcomp cmp) (pcmp op))*)
(*end*)

(*let test =
  let module LL = LambdaDSL in
  LL.(
    lapply (linteger 0)
	   (List.map lconst [unit;tt])
  )*)
