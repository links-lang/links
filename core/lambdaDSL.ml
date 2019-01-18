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

  let lapply f xs =
    let lapp =
      { ap_func = f;
        ap_args = xs;
        ap_loc = Location.none;
        ap_should_be_tailcall = false;
        ap_inlined = Default_inline;
        ap_specialised = Default_specialise }
    in
    Lapply lapp
   
  let lfunction ?(kind=`Curried) params body =
    let lfun =
      { kind =
          (match kind with
          | `Curried -> Curried
          | `Tupled -> Tupled);
        params = params;
        body = body;
        attr = Lambda.default_function_attribute;
        loc = Location.none }
    in
    Lfunction lfun
(* Multicore:  let lapply : lambda -> lambda list -> lambda =
    fun f args ->
    Lapply (f, args, Location.none)
   
  let lfunction : ?kind:function_kind -> Ident.t list -> lambda -> lambda =
    fun ?(kind=Curried) params body -> Lfunction (kind, params, body) *)

  let lfun = lfunction
	       
  let int : int -> constant =
    fun i -> Const_int i

  let float : float -> constant =
    fun f ->
    Const_float (string_of_float f)

  let char : char -> constant =
    fun c -> Const_char c
      
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
    fun tag flag -> Pmakeblock (tag, flag, None)

  let lprim : primitive -> lambda list -> lambda =
    fun prim args -> Lprim (prim, args, Location.none)
			       
  let field : int -> primitive =
    fun i -> Pfield i (* Multicore: Pfield (i, false, Immutable) *)

  let set_field_imm : int -> primitive =
    fun i -> Psetfield (i, Immediate, Assignment) (* Multicore: Psetfield (i, false, Immutable) *)

  let box : primitive = makeblock 0 Immutable
				  
  let leffect : string -> lambda =
    fun label ->
    (** Copied from translmod.ml **)
    let prim_set_oo_id =
      Pccall (Primitive.simple "caml_set_oo_id" 1 false) 
    in
    lprim
      prim_set_oo_id
      [lprim (makeblock 248 Mutable) [lstring label; linteger 0]]

  let lalloc_stack : lambda -> lambda -> lambda -> lambda =
    fun val_handler exn_handler eff_handler ->
    (** Copied from translmod.ml **)
    let prim_alloc_stack =
      Pccall (Primitive.simple "caml_alloc_stack"  3  true)
    in
    lprim
      prim_alloc_stack
      [val_handler ; exn_handler ; eff_handler]
(*
  let lresume : lambda list -> lambda =
    fun args ->
    lprim Presume args

  let lperform : lambda -> lambda list -> lambda =
    fun eff args ->
    lprim
      Pperform
      [lprim box (eff :: args) ]

  let ldelegate : Ident.t -> Ident.t -> lambda =
    fun eff cont ->
    lprim Pdelegate [lvar eff ; lvar cont]
    (* Multicore: lprim Preperform [lvar eff ; lvar cont] *)
*)
  let lproject : int -> lambda -> lambda =
    fun label row -> lprim (field label) [row]


  let lraise : raise_kind -> lambda -> lambda =
    fun kind lam ->
    lprim (Praise kind) [lam]

  let lgetglobal : string -> lambda =
    fun name ->
      lprim (Pgetglobal (Ident.create_persistent name)) []

  let lsetglobal : string -> lambda =
    fun name ->
      lprim (Psetglobal (Ident.create_persistent name)) []


  let lcons : ?elemtype:[< `Generic] -> lambda -> lambda list -> lambda =
    fun ?(elemtype=`Generic) x xs ->
      match elemtype with
      | `Generic -> lprim box (x :: xs)

  let lnil : lambda = (lconst ff)

  let hash_label : string -> int =
    fun label -> Btype.hash_variant label
      
  let polyvariant : string -> lambda list option -> lambda =
    fun label args ->
    let id = linteger (hash_label label) in
    match args with
    | Some args -> lprim box ([id; lprim box args])
    | None -> id

  let lpolyvariant : string -> lambda list option -> lambda =
    fun label args ->
    let id = linteger (hash_label label) in
    match args with
    | Some args -> lprim box (id :: args)
    | None -> id		

  let lookup : string -> string -> lambda =
    fun module_name fun_name ->
      let env = Compmisc.initial_env () in
      let lfun, _ = Env.lookup_value
        (Longident.(Ldot (Lident module_name, fun_name)))
        env
    (*Env.empty*)
    in transl_path Env.empty lfun
		    
  let pervasives : string -> lambda =
    fun fun_name -> lookup "Pervasives" fun_name

  let obj : string -> lambda =
    fun fun_name -> lookup "Obj" fun_name

  let lif : lambda -> lambda -> lambda -> lambda =
    fun cond trueb falseb -> Lifthenelse (cond, trueb, falseb)

  let lseq : lambda -> lambda -> lambda =
    fun e1 e2 -> Lsequence (e1, e2)

  let llet : ?kind:let_kind -> ?vkind:value_kind -> Ident.t -> lambda -> lambda -> lambda =
    fun ?(kind=Strict) ?(vkind=Pgenval) b expr body -> Llet (kind, vkind, b, expr, body)

  let lletrec : (Ident.t * lambda) list -> lambda -> lambda =
    fun funs body -> Lletrec (funs, body)

  let neq : lambda -> lambda -> lambda =
    fun lhs rhs ->
    lprim (Pintcomp Cneq) [lhs;rhs]

  let lprint : lambda = lprim (field 29) [lgetglobal "Pervasives" ]

  let eq : lambda -> lambda -> lambda =
    fun lhs rhs ->
    lprim (Pintcomp Ceq) [lhs;rhs]

  let create_ident : string -> int -> Ident.t =
    fun label id ->
    Ident.({ name = label ; stamp = id ; flags = 0 })	 
(*
  let prim_description : string -> int -> bool -> string -> bool -> Primitive.description =
    fun name arity allocates cname onfloats ->
      { prim_name         = name            (* Name of primitive  or C function *)
      ; prim_arity        = arity           (* Number of arguments *)
      ; prim_alloc        = allocates       (* Does it allocates or raise? *)
      ; prim_native_name  = cname           (* Name of C function for the nat. code gen. *)
      ; prim_native_float = onfloats }      (* Does the above operate on unboxed floats? *)
*)

  let prim_binary_op : string -> Primitive.description =
    fun name -> Primitive.simple name 2 true 

  let pcmp : string -> primitive =
    function
    | "==" -> Pintcomp Ceq
    | "!=" -> Pintcomp Cneq
    | op -> let cmp =
              match op with
              | "<"  -> "caml_lessthan"
              | ">"  -> "caml_greaterthan"
              | "<=" -> "caml_lessequal"
              | ">=" -> "caml_greaterequal"
            in
            Pccall (prim_binary_op cmp)
      
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
