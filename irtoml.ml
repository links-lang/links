(*pp deriving *)
open Num
open Utility
open Ir
open PP

exception Unsupported of string;;

type var = int
type name = string

type name_set = Utility.stringset
type 'a name_map = 'a Utility.stringmap

type code =
  | Bool of bool
  | Int of num
  | Char of char
  | NativeString of string
  | Float of float
  | Var of string
  | Rec of (string * string list * code) list * code
  | Fun of string list * code 
  | LetFunQ of string * query_value * code
  | Let of string * code * code
  | If of (code * code * code)
  | Call of (code * code list)
  | Pair of code * code
  | Triple of code * code * code
  | Lst of code list
  | Case of code * ((code * code) list) * ((code * code) option)
  | Die of code
  | Query of query_computation
  | Table of code * code * Types.primitive list
  | Tail of code
  | Empty

and query_value =
  [ `Constant of Constant.constant 
  | `Variable of var
  | `SplicedVariable of code
  | `Primitive of string
  | `Extend of query_value name_map * query_value option
  | `Project of name * query_value
  | `Erase of name_set * query_value
  | `Inject of name * query_value
  | `ApplyPure of query_value * query_value list
  | `Table of query_value * query_value * Types.primitive list
  | `Database of query_value
  | `Lambda of var list * query_computation
  ]	
and query_tail_computation =
  [ `Return of query_value
  | `Apply of query_value * query_value list
  | `ApplyDB of query_value * query_value list
  | `Case of query_value * (var * query_computation) name_map * (var * query_computation) option
  | `If of query_value * query_computation * query_computation
  ]	
and query_binding = 
  [ `Let of var * query_computation
  | `Fun of var * var list * query_computation
  | `FunQ of var * var list * query_computation
  ]	
and query_computation = query_binding list * query_tail_computation
		  

module type Boxer =
sig
  val wrap_func : bool -> string * string list * string * bool -> code -> code

  val box_bool : code -> code
  val box_record : code -> code
  val box_int : code -> code
  val box_char : code -> code
  val box_float : code -> code
  val box_variant : code -> code
  val box_string : code -> code
  val box_rec : code -> code
  val box_fun : code -> code
  val box_call : code -> code
  val box_if : code -> code
  val box_case : code -> code
  val box_xmlitem : code -> code
  val box_list : code -> code
  val box_unit : code -> code
end

let arg_names = mapIndex (fun _ i -> "arg_" ^ (string_of_int i))

let wrap_with name v =
  match name with
    | "id" -> v
    | _ -> Call (Var name, [v])

module FakeBoxer : Boxer =
struct
  let wrap_func cps (name, unboxers, _, needs_k) rest =
    let unboxed_name = "u_" ^ name in
    let f = 
      if cps && not needs_k then
        let args = arg_names unboxers in
        let v_args = List.map (fun a -> Var a) args in
          Fun("k"::args, Call (Var "k", [Call (Var unboxed_name,  v_args)]))
      else
        Var unboxed_name
    in
      Let (name, f, rest)

  let box_bool = identity
  let box_int = identity
  let box_char = identity
  let box_string = identity
  let box_float = identity
  let box_variant = identity
  let box_record = identity
  let box_rec = identity
  let box_fun = identity
  let box_call = identity
  let box_if = identity
  let box_case = identity
  let box_xmlitem = identity
  let box_list = identity
  let box_unit = identity
end

module CamlBoxer : Boxer =
struct
  let box_bool = wrap_with "box_bool"
  let box_int = wrap_with "box_int"
  let box_char = wrap_with "box_char"
  let box_string = wrap_with "box_string"
  let box_float = wrap_with "box_float"
  let box_variant = wrap_with "box_variant"
  let box_record = wrap_with "box_record"
  let box_xmlitem = wrap_with "box_xmlitem"
  let box_list = wrap_with "box_list"
  let box_unit = wrap_with "box_unit"

  let curry_box args body =
    List.fold_right
      (fun arg c -> Call (Var "box_func", [Fun ([arg], c)])) args body

  let wrap_func cps (name, unboxers, boxer, needs_k) rest = 
    let args = arg_names unboxers in
    let v_args = List.map (fun a -> Var a) args in
      
    let body_create v_args unboxers =
      wrap_with boxer (
        Call (
          Var ("u_" ^ name),
          List.map2 (fun ub arg -> wrap_with ub arg) unboxers v_args))
    in
      match (needs_k, cps) with
        | (false, false) ->
            let body = body_create v_args unboxers in
              Let (name, curry_box args body, rest)
        | (true, false) ->
            let body = Die (NativeString (name ^ " cannot be used in direct style."))
            in Let (name, curry_box args body, rest)
        | (true, _) ->
            let body = body_create ((Var "k")::v_args) ("unbox_func"::unboxers) in
              Let (name, curry_box ("k"::args) body, rest)
        | (_, true) ->
            let body = Call (
              wrap_with "unbox_func" (Var "k"), [body_create v_args unboxers]) in
              Let (name, curry_box ("k"::args) body, rest)

  let box_fun = function
    | Fun (a, b) -> curry_box a b
    | _ -> assert false

  let box_rec = function
    | Rec (funcs, rest) ->
        let rec box_funcs fs rest =
          match fs with
              [] -> rest
            | ((n, a, r)::fs) ->
                Let (n, Call (Var "box_func", [Var n]), box_funcs fs rest)
        in
          Rec (
            List.map 
              (fun (n, args, r) -> 
                 match args with 
                   | [] -> (n, [], (box_funcs funcs r))
                   | arg::args ->
                       (n, [arg], curry_box args (box_funcs funcs r))) funcs,
            box_funcs funcs rest)
    | _ -> assert false

  let box_call = function
	 | Call (f, []) -> Call (Call (Var "unbox_func",[f]), [])
    | Call (f, args) ->
        List.fold_left 
          (fun c arg -> Call(Call (Var "unbox_func", [c]), [arg])) f args
    | _ -> assert false

  let box_if = function 
    | If (b, t, f) ->
        If (Call (Var "unbox_bool", [b]), t, f)
    | _ -> assert false

  let box_case = function 
    | Case (v, c, d) ->
        Case (Call (Var "unbox_variant", [v]), c, d)
    | _ -> assert false
end

(* This is mostly stolen from irtojs.ml *)
module Symbols =
struct
  let words =
    CharMap.from_alist
      [ '!', "bang";
        '$', "dollar";
        '%', "percent";
        '&', "and";
        '*', "star";
        '+', "plus";
        '/', "slash";
        '<', "lessthan";
        '=', "equals";
        '>', "greaterthan";
        '?', "huh";
        '@', "monkey";
        '\\', "backslash";
        '^', "caret";
        '-', "hyphen";
        '.', "fullstop";
        '|', "pipe"; ]

  let has_symbols name =
    List.exists (not -<- Utility.Char.isWord) (explode name)

  let wordify name = 
    if has_symbols name then 
      ("s_" ^
         mapstrcat "_" 
         (fun ch ->
            if (Utility.Char.isWord ch) then
              String.make 1 ch
            else if CharMap.mem ch words then
             CharMap.find ch words
            else
              failwith("Internal error: unknown symbol character: "^String.make 1 ch))
         (Utility.explode name))
        (* TBD: it would be better if this split to chunks maximally matching
           (\w+)|(\W)
           then we would not split apart words in partly-symbolic idents. *)
    else
      name
end

let lib_funcs = [
  "l_int_add", ["unbox_int"; "unbox_int"], "box_int", false;
  "l_int_minus", ["unbox_int"; "unbox_int"], "box_int", false;
  "l_int_mult", ["unbox_int"; "unbox_int"], "box_int", false;
  "_mod", ["unbox_int"; "unbox_int"], "box_int", false;
  "_negate", ["unbox_int"], "box_int", false;

  "l_int_gt", ["unbox_int"; "unbox_int"], "box_bool", false;
  "l_int_gte", ["unbox_int"; "unbox_int"], "box_bool", false;
  "l_int_lt", ["unbox_int"; "unbox_int"], "box_bool", false;
  "l_int_lte", ["unbox_int"; "unbox_int"], "box_bool", false;

  "_tilde", ["unbox_string"; "id"], "box_bool", false;

  "l_equals", ["id"; "id"], "box_bool", false;
  "l_not_equals", ["id"; "id"], "box_bool", false;

  "l_cons", ["id"; "unbox_list"], "box_list", false;
  "l_concat", ["unbox_list"; "unbox_list"], "box_list", false;
  "_hd", ["unbox_list"], "id", false;
  "_tl", ["unbox_list"], "box_list", false;
  "_drop", ["unbox_int"; "unbox_list"], "box_list", false;
  "_take", ["unbox_int"; "unbox_list"], "box_list", false;

  "_not", ["unbox_bool"], "box_bool", false;

  "_addAttributes", ["unbox_list"; "unbox_list"], "box_list", false;

  "_intToXml", ["unbox_int"], "box_list", false;
  "_stringToXml", ["unbox_string"], "box_list", false;
  "_intToString", ["unbox_int"], "box_string", false;
  "_stringToInt", ["unbox_string"], "box_int", false;
  "_stringToFloat", ["unbox_string"], "box_float", false;

  "_environment", ["unbox_unit"], "box_list", false;
  "_redirect", ["unbox_string"], "id", true;
  "_exit", ["id"], "id", true;
  "_error", ["unbox_string"], "id", false;
  "_unsafePickleCont", ["id"], "box_string", false;
  "_reifyK", ["id"], "box_string", false;
  "_debug", ["unbox_string"], "box_unit", false;

  "s___caret_caret", ["unbox_string"; "unbox_string"], "box_string",false;

  "_print", ["unbox_string"], "box_unit", false;

  "_getDatabaseConfig",["unbox_unit"], "box_record", false;

]

(* TODO: Most of this can be handled generically *)
let ident_substs = StringMap.from_alist
  [ "+", "l_int_add";
    "-", "l_int_minus";
    "*", "l_int_mult";
    ">", "l_int_gt";
    "<", "l_int_lt";
    ">=", "l_int_gte";
    "<=", "l_int_lte";
    "!=", "l_not_equals";
    "==", "l_equals";
    "Nil", "l_nil";
    "Cons", "l_cons";
    "Concat", "l_concat";
  ]

(* Primitives that need to be reconized manualy in queries *)
let known_primitives = 
  [ "concatMap", "ConcatMap";
	 "map", "Map";
	 "empty", "Empty";
	 "sortByBase","SortBy";
  ]

(* HACK : one could consider storing real name and not having to do this *)
let get_primitive_name s =
  let test s = Str.string_match (Str.regexp "_\([^_]*\)_[0-9]*") s 0 in
  let rec aux = function 
	 | [] -> None
	 | (h1,h2)::t -> if test s && Str.matched_group 1 s = h1 then Some h2 else aux t
  in aux known_primitives
 

let subst_ident n = 
  if StringMap.mem n ident_substs then
    StringMap.find n ident_substs
  else
    n

let subst_primitive n =
  if Lib.is_primitive n then
    "_"^n
  else
    n

let make_var_name v n = 
  let name = 
    (if n = "" then 
      "v"
    else 
      "_"^n) ^ "_" ^ (string_of_int v)
  in 
    (Symbols.wordify -<- subst_primitive -<- subst_ident) name

let get_var_name v n =
  (Symbols.wordify -<- subst_primitive -<- subst_ident) n 

let bind_continuation k body =
  match k with 
    | Var _ -> body k
    | _ -> Let ("kappa", k, body (Var "kappa"))

(* We need to unpack table type *)
let unpack_table_type t_type =
  
  (* HACK : I'm not sure at all this is the right way to do it ... *)
  let table_fields = match t_type with
		`Record row -> Types.extract_tuple row | _ -> failwith "Wrong table type"
  in
  List.map (function `Primitive p -> p | _ -> failwith "Wrong table type") table_fields


(*
module QueryType =
struct
(*
  let rec of_query : query -> Types.datatype = fun v ->
	 let record fields : Types.datatype =
		Types.make_record_type (StringMap.map of_query fields)
	 in
    match v with
      | `Concat (v::_vs) -> of_query v
      | `For (gens, _os, body) -> of_query body
      | `Singleton (`Record fields) -> record fields
      | `If (_, t, _) -> of_query t
      | `Table (_, _, row) -> `Record row
      | `Constant (`Bool b) -> Types.bool_type
      | `Constant (`Int i) -> Types.int_type
      | `Constant (`Char c) -> Types.char_type
      | `Constant (`Float f) -> Types.float_type
      | `Constant (`String s) -> Types.string_type
      | `Project (`Var (x, field_types), name) -> StringMap.find name field_types
      | `Apply ("Empty", _) -> Types.bool_type (* HACK *)
      | `Apply (f, _) -> TypeUtils.return_type (Env.String.lookup Lib.type_env f)
      | e -> Debug.print("Can't deduce type for: " ^ string_of_t e); assert false
*)
  let get_row t = 
    let (fieldMap, _), _ = 
		Types.unwrap_row(TypeUtils.extract_row t) in
    let fields =
		StringMap.fold
		  (fun name t fields ->
          match t with
				| `Present, t -> (name, t)::fields
				| `Absent, _ -> assert false
				| `Var _, t -> assert false)
		  fieldMap
		  []

*)
module Translater (B : Boxer) =
struct

  class translateQuery translateCode =
  object (o : 'self_type)

	 val translateCode = translateCode
	 
	 method value : Ir.value -> query_value = function
		| `Inject (n,v,_) -> `Inject (n,o#value v)
		| `TAbs (_,v)
		| `TApp (v,_)
		| `Coerce (v,_) -> o#value v
		| `ApplyPure (v,vl) -> `ApplyPure (o#value v, List.map o#value vl)
		| `Extend (vm,vo) ->
			 `Extend (StringMap.map o#value vm,opt_map o#value vo)
		| `Project (name, v) -> `Project (name, o#value v)
		| `Erase (ns,v) -> `Erase (ns,o#value v)
		| `Constant c -> `Constant c
		| `Variable var -> 
			 if Lib.is_primitive_var var then
				`Primitive (Lib.primitive_name var)
			 else
				`Variable var 
		| `SplicedVariable var -> 
			 if Lib.is_primitive_var var then
				`Primitive (Lib.primitive_name var)
			 else (
				match translateCode#get_name var with 
				  | Some var_name -> ( match get_primitive_name var_name with
						| Some n -> `Primitive n
						| None -> `SplicedVariable (translateCode#value (`Variable var))
				  )
				  | None -> `SplicedVariable (translateCode#value (`Variable var))
			 ) 
		| `XmlNode _ -> failwith "XmlNode inside query"
			 
	 method tail_computation : Ir.tail_computation -> query_tail_computation = function
		| `Return v -> `Return (o#value v)
		| `Apply (v,vl) -> `Apply (o#value v, List.map o#value vl)
		| `ApplyDB (v,vl) -> `Apply (`Project ("db", o#value v), List.map o#value vl)
		| `ApplyPL _ -> failwith "Apply PL function inside a query"
		| `If (v,c1,c2) -> `If (o#value v, o#computation c1, o#computation c2)
		| `Case (v,m,vo) ->
			 let aux ((var,_),c) = (var,o#computation c) in
			 `Case (o#value v, StringMap.map aux m,opt_map aux vo)
		| `Special (`Table (t,db,(t_type,_,_))) ->
			 (*Debug.print (Types.string_of_datatype t_type) ;*)
			 `Return (`Table (o#value db, o#value t, unpack_table_type t_type ))
		| `Special (`Database db) ->
			 `Return (`Database (o#value db))
		| `Special (`Query _) -> failwith "Query"
		| `Special _ -> failwith "No special except Table, Database or Query inside a query"
			 
	 method bindings : Ir.binding list -> query_binding list = function
		| [] -> []
		| b::bl -> begin match b with
			 | `Let ((var,_) as b,(_,tc)) -> 
				  let o = {< translateCode = translateCode#add_bindings [b] >} in
				  `Let (var,o#computation ([],tc))::(o#bindings bl)
			 | `Fun ((var,_) as b,(_,bll,c),_) -> 
				  let o = {< translateCode = translateCode#add_bindings (b::bll) >} in
				  let arg_names = List.map fst bll in
				  `Fun (var,arg_names, o#computation c)::(o#bindings bl)
			 | `FunQ ((var,_) as b,(_,bll,c),_) -> 
				  let o = {< translateCode = translateCode#add_bindings (b::bll) >} in
				  let arg_names = List.map fst bll in
				  `FunQ (var,arg_names, o#computation c)::(o#bindings bl)
			 | `Rec _ | `Alien _ | `Module _ -> o#bindings bl
		end
		  
	 method computation (bl,tc) : query_computation =  match tc with 
		| `Special(`Query (_,(bl2,tc),types)) -> 
			 Debug.print (
				(Types.string_of_datatype -<- Types.concrete_type ) 
				  types) ; 
			 o#computation (bl@bl2,tc)
		| _ -> (o#bindings bl, o#tail_computation tc)
	  
		
  end

  class virtual codeIR env = 
  object (o : 'self_type)
    val env = env

	 method get_name v =
		Env.Int.find env v
		
    method wrap_func = B.wrap_func false

    method wrap_lib rest =
      List.fold_left
        (fun r x -> o#wrap_func x r) rest lib_funcs

    method add_bindings : binder list -> 'self_type = fun bs ->
      let env = List.fold_left 
        (fun e (v, (_, n, _)) -> Env.Int.bind e (v, (make_var_name v n))) env bs in
        {< env=env >}
          
    method constant : constant -> code = fun c ->
      match c with
        | `Bool x -> B.box_bool (Bool x)
        | `Int x -> B.box_int (Int x)
        | `Char x -> B.box_char (Char x)
        | `String x -> B.box_string (NativeString x)
        | `Float x -> B.box_float (Float x)
      
    method value : value -> code = fun v ->
      match v with 
        | `Constant c -> o#constant c

        | `Variable v | `SplicedVariable v -> 
				(match o#get_name v with
					 None -> failwith ("can't find variable number :" ^ string_of_int v)
				  | Some n -> Var (get_var_name v n )
				)

        | `Extend (r, v) ->
            let record = B.box_record (
              StringMap.fold 
                (fun n v m ->
                   Call (Var "StringMap.add",
                         [NativeString n; o#value v; m]))
                r (Var "StringMap.empty"))
            in
              begin 
                match v with
                    None -> record
                  | Some v -> 
                      Call (Var "_union", [o#value v; record])
              end
                
        | `Project (n, v) ->
            Call (Var "project", [o#value v; o#constant (`String n)])

        | `Erase (ns, v) ->
            Call (Var "erase", [o#value v; Lst (List.map (fun n -> o#constant (`String n)) (StringSet.elements ns))])
              
        | `Inject (n, v, _) ->
            B.box_variant (Pair (NativeString n, o#value v))

        | `TAbs (_, v) -> o#value v

        | `TApp (v, _) -> o#value v

        | `XmlNode (name, attrs, children) ->
            B.box_list(
              Lst [
                B.box_xmlitem (          
                  Call (Var "build_xmlitem", [
                          NativeString name;
                          Lst (
                            StringMap.fold
                              (fun n v a -> Pair(NativeString n, o#value v)::a) attrs []);
                          Lst (List.map o#value children)]))])
                        
        | `ApplyPure (v, vl) -> 
            B.box_call (Call (o#value v, List.map o#value vl))

        | `Coerce (v, _) -> o#value v

    method bindings : binding list -> ('self_type -> code) -> code = fun bs f ->
      match bs with
          [] -> f o
        | (b::bs) -> o#binding b (fun o' -> o'#bindings bs f)

    method binder : binder -> string = fun (v, (_, name, _)) ->
      make_var_name v name

    method virtual binding : binding -> ('self_type -> code) -> code

    method virtual program : program -> code

  end
    
  class direct env = 
  object (o : 'self_type)
    inherit codeIR env
      
    method tail_computation : tail_computation -> code = fun tc ->
      match tc with
          `Return v -> o#value v

        | `Apply (v, vl) -> 
            B.box_call (Call (o#value v, List.map o#value vl))

		  | `ApplyPL (v, vl) ->
				o#tail_computation (`Apply (`Project ("pl",v),vl))

		  | `ApplyDB (v, vl) ->
				failwith "ApplyDB inside pl"

        | `Case (v, cases, default) ->
            let gen_case n (b, c) =
              let o = o#add_bindings [b] in
                Pair (n, Var (o#binder b)),
              o#computation c
            in              
              B.box_case (
                Case (
                  o#value v,
                  StringMap.fold (fun n c l -> (gen_case (NativeString n) c)::l) cases [],
                  match default with 
                      None -> None
                    | Some c ->
                        Some (gen_case (Var "_") c)))

        | `If (v, t, f) ->
            B.box_if (
              If (o#value v, o#computation t, o#computation f))
              
        | `Special s ->
            match s with
              | `CallCC v -> 
                  Die (NativeString "CallCC not supported in direct style.")
              | `Database v -> Call (Var "_database",[o#value v])
				  | `Table (t,db,(t_type,_,_)) ->
						Table (o#value db, o#value t, unpack_table_type t_type)
              | `Query (_,c,_) -> Query ((new translateQuery o)#computation c)
				  | `Delete _
				  | `Update _ -> Die (NativeString "Delete and Update operations not supported.")
              | `Wrong _ -> Die (NativeString "Internal Error: Pattern matching failed")

    method program : program -> code = fun prog ->
      o#wrap_lib (o#computation prog)

    method computation : computation -> code = fun (bs, tc) ->
      o#bindings bs (fun o' -> Tail (o'#tail_computation tc))
        
    method binding : binding -> ('self_type -> code) -> code = fun b rest_f ->
      match b with
          `Let (x, (_, tc)) -> 
            let o' = o#add_bindings [x] in
              Let (o#binder x, o#tail_computation tc, rest_f o')
                
        | `Fun f ->
            o#binding (`Rec [f]) rest_f

		  | `FunQ (binder, (_, f_binders, comp), _) -> 
				let o' = o#add_bindings [binder] in
				let args = List.map fst f_binders in
				let o'' = o'#add_bindings f_binders in
				LetFunQ ( o'#binder binder, `Lambda (args,(new translateQuery o'')#computation comp) , 
				  rest_f o')
              
        | `Rec funs -> 
				B.box_rec (
				  let names = List.map fst3 funs in
				  let o' = o#add_bindings (List.map fst3 funs) in
				  Rec (
					 List.map (
						fun (binder, (_, f_binders, comp), _) ->
						  let args = List.map o'#binder f_binders in
                    let o'' = o'#add_bindings f_binders in
                    (o''#binder binder, 
                     args, 
                     o''#computation comp)) funs,
					 rest_f o'))
            
        | `Alien _ -> assert false
            
        | `Module _ -> assert false      
  end

  class cps env =
  object (o : 'self_type)
    inherit codeIR env as super

    method wrap_func = B.wrap_func true

    (* `ApplyPure is a pain. *)
    method value : value -> code = function
      | `ApplyPure (v, vl) ->
          B.box_call (Call (o#value v, (Var "id")::(List.map o#value vl)))
      | v -> super#value v

    method tail_computation : tail_computation -> code -> code = fun tc k ->
      match tc with
          `Return v -> B.box_call (Call (k, [o#value v]))

        | `Apply (v, vl) -> 
            B.box_call (Call (o#value v, k::(List.map o#value vl)))

		  | `ApplyPL (v, vl) ->
				o#tail_computation (`Apply (`Project ("pl",v),vl)) k

		  | `ApplyDB (v, vl) ->
				B.box_call (Call (Call (Var "call_db",[o#value v]),List.map o#value vl))

        | `Case (v, cases, default) ->
            bind_continuation k
              (fun k ->
                 let gen_case n (b, c) =
                   let o = o#add_bindings [b] in
                     Pair (n, Var (o#binder b)),
                   o#computation c k 
                 in
                   B.box_case (
                     Case (
                       o#value v,
                       StringMap.fold (fun n c l -> (gen_case (NativeString n) c)::l) cases [],
                       match default with 
                           None -> None
                         | Some c ->
                             Some (gen_case (Var "_") c))))


        | `If (v, t, f) ->
            bind_continuation k
              (fun k -> B.box_if (If (o#value v, o#computation t k, o#computation f k)))
              
        | `Special s ->
            match s with
               `CallCC v ->
                bind_continuation k
                  (fun k ->
                     (* This wrapper dumps the unnecessary continuation argument
                      * that the continuation will be passed when called *)
                     Let ("call_k", 
                          B.box_fun (Fun (["_"; "arg"], B.box_call (Call (k, [Var "arg"])))),
                          B.box_call (Call (o#value v, [k; Var "call_k"]))))
              | `Database v -> Call (Var "database",[o#value v])
				  | `Table (t,db,(t_type,_,_)) ->
						Table (o#value db, o#value t, unpack_table_type t_type )
              | `Query (_,c,_) -> Query ((new translateQuery o)#computation c)
				  | `Delete _ 
				  | `Update _ -> Die (NativeString "Database operations not supported.")
              | `Wrong _ -> Die (NativeString "Internal Error: Pattern matching failed")

    method computation : computation -> code -> code = fun (bs, tc) k ->
      o#bindings bs (fun o' -> Tail (o'#tail_computation tc k))

    method program : program -> code = fun prog -> 
      o#wrap_lib (o#computation prog (Var "start"))

    method binding : binding -> ('self_type -> code) -> code = fun b rest_f ->
      match b with
          `Let (x, (_, tc)) ->
            let o' = o#add_bindings [x] in
              o#tail_computation tc (B.box_fun (Fun ([o#binder x], rest_f o')))

        | `Fun  f ->
            o#binding (`Rec [f]) rest_f

		  | `FunQ (binder, (_, f_binders, comp), _) -> 
				let o' = o#add_bindings [binder] in
				let args = List.map fst f_binders in
				LetFunQ (
				  o'#binder binder, `Lambda (args,(new translateQuery o')#computation comp) , 
				  rest_f o')
              
        | `Rec funs ->
            B.box_rec (
              let names = List.map fst3 funs in
              let o' = o#add_bindings (List.map fst3 funs) in
                Rec (
                  List.map (
                    fun (binder, (_, f_binders, comp), _) ->
                      let o'' = o'#add_bindings f_binders in
                        (o''#binder binder, 
                         "kappa"::(List.map o''#binder f_binders), 
                         o''#computation comp (Var "kappa"))) funs,
                  rest_f o'))
              
        | `Alien _ -> Empty
            
        | `Module _ -> Empty            
  end
end

(* 
   Found the bottleneck :P 
   TODO: Find a tractable indentation scheme for CPS!
*)
let nest : int -> doc -> doc = fun i x -> x

let args_doc args =
  if args = [] then
    text "()"
  else
    doc_join text args

module MLof =
struct 

  let variant t l = 
	 text t ^^ parens (hsep (punctuate "," l))

  let option f o = match o with
		Some x -> text "Some" ^+^ (parens (f x))
	 | None -> text "None"

  let string t = 
	 text ("\"" ^ t ^ "\"")

  let name_map f n = 
	 StringMap.fold (fun k v t -> text "StringMap.add" ^+^ string k ^+^ parens (f v) ^| parens t) n
		(text "StringMap.empty") 

  let name_set ns = 
	 StringSet.fold (fun v t -> text "StringSet.add" ^+^ text v ^| parens t) ns
		(text "StringSet.empty")
		
  let constant (const : Constant.constant) = match const with
	 | `Float f -> text ("`Float " ^ string_of_float f)
	 | `Int i -> text ("`Int (Num.num_of_string \"" ^ (Num.string_of_num i) ^ "\")")
	 | `Bool b -> text ("`Bool " ^ string_of_bool b)
	 | `Char c -> text ("`Char " ^ string_of_char c)
	 | `String s -> text ("`String \"" ^  s ^ "\"")
  
	 
  let rec value (v : query_value) = match v with
	 | `Constant c -> variant "Constant" [constant c]
	 | `Variable var -> variant "Variable" [text (string_of_int var)]
	 | `Primitive f -> variant "Primitive" [string f]
	 | `Extend (vn,vo) -> variant "Extend" [name_map value vn; option value vo]
	 | `Project (n,v) -> variant "Project" [string n; value v]
	 | `Erase (ns,v) -> variant "Erase" [name_set ns; value v]
	 | `Inject (n,v) -> variant "Inject" [string n; value v]
	 | `ApplyPure (v,vl) -> variant "ApplyPure" [value v; list (List.map value vl)]
	 | `Table (t, db, _ns) -> variant "Table" [value t; value db; name_set StringSet.empty]
	 | `Database v -> variant "Database" [value v]
	 | `SplicedVariable c -> code (Call (Var "_splice",[c]))
	 | `Lambda (vl,c) -> variant "Lambda" [list (List.map (fun x -> text (string_of_int x)) vl); computation c]
		  
  and tail_computation (tc : query_tail_computation) = match tc with
	 | `Return v -> variant "Return" [value v]
	 | `Apply (v,vl) -> variant "Apply" [value v; list (List.map value vl)]
	 | `ApplyDB (v,vl) -> variant "ApplyDB" [value v; list (List.map value vl)]
	 | `Case (v, nm, o) -> 
		  let aux (v,c) = parens (text (string_of_int v) ^^ (text ", ") ^^ (computation c))
		  in variant "Case" [ value v ; name_map aux nm ; option aux o ]
	 | `If (v, c1, c2) -> variant "If" [ value v ; computation c1 ; computation c2 ]
		  
  and binding (b : query_binding) = match b with
	 | `Let (v,c) -> variant "Let" [text (string_of_int v) ; computation c]
	 | `Fun (v, vl, c) -> 
		  variant "Fun" 
			 [text (string_of_int v) ; list (List.map (fun i -> text (string_of_int i)) vl) ; computation c]
	 | `FunQ (v, vl, c) -> 
			 variant "FunQ" 
				[text (string_of_int v) ; list (List.map (fun i -> text (string_of_int i)) vl) ; computation c]
				
  and computation (b,tc) =
	 parens (list (List.map binding b) ^^ (text ", ") ^^ tail_computation tc)
			 
  and code_int c top_level = 
	 match c with
		| Bool x -> text (string_of_bool x)
		(* Represent integer literals as strings so we don't hit range problems. *)
		| Int x -> parens (text "num_of_string" ^| code_int (NativeString (Num.string_of_num x)) false )
		| Char x -> text ("'" ^ Char.escaped x ^ "'")
		| NativeString x -> text ("\"" ^ String.escaped x ^ "\"")
		| Float x -> text (string_of_float x)
			 
		| Var name -> text name
			 
		| Rec (fs, rest) ->
			 group (
				group (
              text "let rec" ^|
						doc_concat (break^^text "and"^^break)
						  (List.map 
							  (fun (name, args, body) ->
								 let args = if args = [] then text "_" else args_doc args in
								 nest 2 (
									group (text name ^| args ^| text "=") 
									^| code_int body false)) fs)) ^|
					 text ( if top_level then "\n" else "in")
				^| code_int rest top_level)

		| LetFunQ (name, body, rest) -> 
			 group ( 
				group ( text "let" ^| 
					 nest 2 (
						group (text name ^| text "=" ) ^| text "_funq" 
						  ^| parens (value body)
					 ) 
				) ^| code_int rest top_level 
			 )
				
		| Fun (args, body) ->
			 parens (
				group (          
              nest 2 (
					 group (text "fun" ^| args_doc args ^| text "->") 
					 ^|  code_int body false)))
            
		| Let (name, body, rest) ->
			 group (
				group (
              text "let" ^|
						nest 2 (
                    group (text name ^| text "=") 
                    ^| code_int body false)) ^|
					 text ( if top_level then "\n" else "in")
				  ^| code_int rest top_level)
				
		| If (b, t, f) ->
			 group (
				nest 2 (text "if" ^| code_int b false) ^|
					 nest 2 (text "then" ^| code_int t false) ^|
                    nest 2 (text "else" ^| code_int f false))
				
		| Case (v, cases, default) ->
			 let pp_case (b, c) =
				group (text "|" ^| code_int b false ^| text "->" ^| code_int c false)
			 in        
			 group (
				text "begin" ^|
					 nest 2 (
						group (text "match" ^| code_int v false ^| text "with") ^|
							 doc_join pp_case cases ^|
								  begin 
									 match default with
										| None -> empty
										| Some c -> pp_case c
								  end ^|
										text "| _ -> assert false") ^|
                    text "end")
				
		| Pair (v1, v2) ->
			 group (
				parens (code_int v1 false ^^ text "," ^| code_int v2 false))
				
		| Triple (v1, v2, v3) ->
			 group (
				nest 2 (
              parens (
					 group (code_int v1 false ^^ text "," ^| code_int v2 false ^^ text ",") ^| 
                    code_int v3 false)))
				
		| Lst vs ->
			 group (
				text "[" ^^ 
              doc_concat (text "; ") 
              (List.map (group -<- (fun c -> code_int c false)) vs) ^^ 
              text "]")
				
		| Call (f, args) -> 
          let args = if args = [] then text "l_unit" else doc_join (fun c -> code_int c false) args in
          parens (group (
            nest 2 ((code_int f false) ^| args)))
				
		| Die s ->
			 group (text "raise (InternalError" ^| code_int s false ^| text ")")
				
		| Empty -> empty

		| Tail c -> 
			 group (
				(if top_level then text "let _ =\n" else text "" ) ^|
					 code_int c false)
			 
		| Query q -> parens (group (nest 2 ( text "_query" ^| computation q)))

		| Table (s1,s2,_row) ->
			 parens (group (nest 2 (text "_table" ^| code_int s1 false ^| code_int s2 false ^| name_set StringSet.empty)))
				
  and code c = code_int c true
end

let postamble = "\n\nlet _ = run entry"
  
module BoxingCamlTranslater = Translater (CamlBoxer)
module NonBoxingCamlTranslater = Translater (FakeBoxer)

let ml_of_ir cps box no_prelude env prelude (bs, tc) =
  let env = Env.invert_env env in

  let preamble = "open Num\nopen Irquery\n" ^
    if box then "open Mllib\n\n" else "open Unboxed_mllib;;\n\n"
  in

  let comp = if box && not no_prelude then 
      prelude @ bs, tc
	 else 
      bs, tc
  in

  let c =
    if cps then 
      let t =
        if box then
          new BoxingCamlTranslater.cps env
        else
          new NonBoxingCamlTranslater.cps env
      in 
        t#program comp
    else
      let t = 
        if box then 
          new BoxingCamlTranslater.direct env
        else
          new NonBoxingCamlTranslater.direct env
      in 
        t#program comp
  in
    (* Hack: this needs to be fixed so top-level bindings are
       properly exposed. *)
    preamble ^
      (pretty 110 (MLof.code c))
