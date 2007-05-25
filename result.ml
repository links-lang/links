(*pp deriving *)
open Num
open List

open Types
open Syntax
open Utility

exception Runtime_error of string

class type otherfield = 
object 
  method show : string
end

module Show_otherfield = Show.ShowDefaults(
  struct
    type a = otherfield
    let format formatter obj = Format.pp_print_string formatter (obj # show)
  end)


type db_field_type = BoolField | TextField | IntField | FloatField
		     | SpecialField of otherfield
    deriving (Show)

let string_of_db_field_type = Show_db_field_type.show

type db_status = QueryOk | QueryError of string
    deriving (Show)

class virtual dbresult = object
  method virtual status : db_status
  method virtual nfields : int
  method virtual fname : int -> string
  method virtual ftype : int -> db_field_type
  method virtual get_all_lst : string list list
  method virtual error : string
end

class virtual database = object
  method virtual driver_name : unit -> string
  method virtual equal_types : Types.datatype -> db_field_type -> bool
  method virtual escape_string : string -> string
  method virtual exec : string -> dbresult
  method make_insert_query : (string * string list * string list list) -> string =
    fun (table_name, field_names, vss) ->
      "insert into " ^ table_name ^
        "("^String.concat "," field_names ^") values "^
        String.concat "," (List.map (fun vs -> "(" ^ String.concat "," vs ^")") vss)
end

module Eq_database = Eq.Eq_mutable(struct type a = database end)
module Typeable_database = Typeable.Primitive_typeable(struct type t = database end)
module Show_database = Show_unprintable (struct type a = database end)

(* Here we could do something better, like pickling enough information
   about the database to be able to restore the connection on
   deserialisation *)
module Pickle_database = Pickle.Pickle_unpicklable (struct type a = database let tname = "Result.database" end)
module Shelve_database : Shelve.Shelve with type a = database = 
struct
  module Typeable = Typeable_database
  module Eq = Eq_database
  type a = database
  let shelve _ = failwith "shelve database nyi"
end

type db_constructor = string -> (database * string)

(** {1 Database Drivers and Values} *)

(** [database_drivers]: a list of available database drivers.  Each
    driver registers itself at load time: the contents of the list
    depends on which drivers are built.. *)

let database_drivers = ref ([] : (string * db_constructor) list)

(** [register_driver (name, driver)] registers a DB [driver] under the
    name [name]. *[driver] is a function taking the database params (a
    string with db-specific colon-separated fields) to a pair of
    database object and params (I guess the params could be modified
    by the driver?) *)
let register_driver : (string * db_constructor) -> unit
  = fun ((name, _) as pair) -> 
    Debug.print ("registering driver for " ^ name);
    database_drivers := pair :: !database_drivers

let db_connect driver params =
  let constructor = 
    try assoc driver !database_drivers 
    with Not_found -> failwith ("No driver for database type `" ^ driver ^ "'")
  in constructor params

let parse_db_string : string -> (string * string) = 
  fun params -> 
    match Str.bounded_split (Str.regexp ":") params 2 with
      | [hd; tail] -> (hd, tail)
      | _ -> failwith ("Could not parse db connection string : " ^ params)
and reconstruct_db_string : (string * string) -> string =
  fun (x,y) -> x ^ ":" ^ y

(* Pickling a result value involves replacing expression nodes with
   identifiers.

   This (rexpr) is pulled out as a separate type so that we can
   provide a custom pickling strategy (namely replacing expressions
   with labels while pickling).
*)
type rexpr = expression
    deriving (Show, Eq, Typeable)

module Pickle_rexpr : Pickle.Pickle with type a = rexpr = Pickle.Pickle_defaults(
  struct
    type a = rexpr
    let pickle buffer e = 
      match expression_data e with
        | `T(_, _, Some l) -> Syntax.Pickle_label.pickle buffer l
        | _             -> failwith ("Not labeled: " ^ string_of_expression e)
    and unpickle stream = 
      let label = Syntax.Pickle_label.unpickle stream in
        (* sadly, we can't do resolution here at present because there's
           no way to pass in the table *)
        Syntax.Wrong (`T(Syntax.dummy_position, `Not_typed, Some label))
  end)

module Shelve_rexpr = Shelve.Shelve_primtype(Pickle_rexpr)(Eq_rexpr)(Typeable_rexpr)

type rdef = definition
    deriving (Show, Eq, Typeable)

module Pickle_rdef = Pickle.Pickle_unpicklable (struct type a = rdef let tname = "rdef"  end)
module Shelve_rdef = Shelve.Shelve_primtype(Pickle_rdef)(Eq_rdef)(Typeable_rdef)

type unop = MkColl
            | MkVariant of string
            | Abs
            | MkDatabase
            | VrntSelect of (string * string * rexpr * string option * 
                               rexpr option)
            | Erase of string
            | Project of string
            | QueryOp of (Query.query * (* table aliases: *) string list)
                deriving (Typeable, Show, Pickle, Eq, Shelve)
		
let string_of_unop = Show_unop.show

type binop = [ `Union | `App | `RecExt of string | `MkTableHandle of Types.row | Syntax.comparison]
                 deriving (Typeable, Show, Pickle, Eq, Shelve)

type xmlitem =   Text of string
               | Attr of (string * string)
               | Node of (string * xml)
and xml = xmlitem list
    deriving (Typeable, Show, Pickle, Eq, Shelve)

let is_attr = function
  | Attr _ -> true
  | _      -> false

let attrs = filter is_attr
and nodes = filter (not -<- is_attr)

let rec string_of_xml xml : string
    = String.concat "" (map string_of_item xml)
and string_of_item : xmlitem -> string = 
  let format_attrs attrs = match String.concat " " (map string_of_item attrs) with 
    | "" -> ""
    | a -> " " ^ a in
  let escape = Str.global_replace (Str.regexp "\"") "\\\""  in
    function
      | Attr (k, v) -> k ^ "=\"" ^ escape v ^ "\""
      | Text s -> xml_escape s
      | Node (tag, children) -> let attrs, nodes = attrs children, nodes children in
          match nodes with 
            | [] -> "<" ^ tag ^ format_attrs attrs ^ "/>"
            | _  -> ("<" ^ tag ^ format_attrs attrs ^ ">" 
                     ^ string_of_xml nodes
                     ^ "</" ^ tag ^ ">")

type table = (database * string) * string * Types.row
   deriving (Show, Pickle, Eq, Typeable, Shelve)


type primitive_value = [
| `Bool of bool
| `Char of char
| `Database of (database * string)
| `Table of table
| `Float of float
| `Int of num
| `XML of xmlitem
| `NativeString of string
                ]
    deriving (Typeable, Show, Pickle, Eq, Shelve)

let type_of_primitive : primitive_value -> datatype = function
  | `Bool _ -> `Primitive `Bool
  | `Int _ -> `Primitive `Int
  | `Float _ -> `Primitive `Float
  | `Char _ -> `Primitive `Char
  | `XML _ -> `Primitive `XmlItem
  | `Database _ -> `Primitive `DB
  | `Table _ -> `Primitive `Abstract
  | `NativeString _ ->`Primitive `NativeString

let string_of_primitive_type = type_of_primitive ->- string_of_datatype

type contin_frame = 
  | Definition of (environment * string)
  | FuncArg of (rexpr list * environment) (* FIXME: This is twiddled *)
  | FuncApply of (environment * result * rexpr list * result list)
  | FuncApplyFlipped of (environment * result)
  | ThunkApply of environment
  | LetCont of (environment * 
		  string * rexpr)
  | BranchCont of (environment * 
		     rexpr * rexpr)
  | BinopRight of (environment * 
		     binop * rexpr)
  | BinopApply of (environment * 
		     binop * result)
  | UnopApply of (environment * unop )
  | RecSelect of (environment * string * string * string * rexpr)
  | CollExtn of (environment * 
		   string * rexpr * 
		   result list list * result list)
  | StartCollExtn of (environment *
			string * rexpr)
  | XMLCont of (environment 
                * string                     (* tag *)
                * string option              (* attr name, if any *)
                * xml                        (* child nodes *)
                * (string * rexpr) list (* unevaluated attributes *)
                * rexpr list            (* unevaluated elements *)
                )
  | Ignore of (environment * rexpr )
  | IgnoreDef of (environment * rdef)
  | Recv of (environment)
and result = [
  | `PrimitiveFunction of string
  | `ClientFunction of (string)
  | `Function of  (string list * environment (*locals*) * unit (*globals*) * rexpr)
  | `Record of ((string * result) list)
  | `Variant of (string * result)
  | `Abs of result
  | `List of (result list)
  | `Continuation of continuation
  |  primitive_value
]
and continuation = contin_frame list
and binding = (string * result)
and environment = (binding list)
    deriving (Typeable, Eq, Show, Pickle, Shelve)

let rec string_of_value_type = function
  | #primitive_value as p -> string_of_primitive_type p
  | `List items -> "["^ string_of_value_type (hd items) ^"]"
(*   | `Application(ctor, args) -> ctor ^ "(" ^  *)
(*       mapstrcat ", " string_of_value_type args ^ ")" *)
  | _ -> "type unknown" (* FIXME *)

let expr_of_prim_val : result -> expression option = function
    `Bool b -> Some(Constant(Boolean b, Syntax.no_expr_data))
  | `Int i -> Some(Constant(Integer i, Syntax.no_expr_data))
  | `Char ch -> Some(Constant(Char ch, Syntax.no_expr_data))
  | `Float f -> Some(Constant(Float f, Syntax.no_expr_data))
  | _ -> None

let prim_val_of_expr : expression -> result option = function
  | Constant(c, _) ->
      begin
        match c with
          | Boolean b -> Some( `Bool b)
          | Integer i -> Some(`Int i)
          | Char ch -> Some(`Char ch)
          | Float f -> Some(`Float f)
      end
  | _ -> None

let (toplevel_cont: continuation) = []

let xmlitem_of : result -> xmlitem = function
  | `XML x -> x
  | _ -> raise (Match_failure ("", 0,0))

and bool b =`Bool b
and int i = `Int i
and float f = `Float f
and char c = `Char c
and listval es = `List es
and xmlnodeval contents = `XML (Node contents)

let is_char = function
  | `Char _ -> true | _ -> false

let is_string = function
  | `List elems -> for_all is_char elems
  | _ -> false

let make_tuple fields = 
  `Record(List.map2 (fun exp n -> string_of_int n, exp) fields 
            (fromTo 1 (1 + length fields)))

exception NotARecord of result
 
let recfields = function
  | `Record (fields) -> fields
  | value -> raise(NotARecord(value))

exception Match of string

let string_as_charlist s : result =
  `List (map (fun x -> (`Char x)) (explode s))

let pair_as_ocaml_pair = function 
  | (`Record [(_, a); (_, b)]) -> (a, b)
  | _ -> failwith ("Match failure in pair conversion")

let links_fst x = fst (pair_as_ocaml_pair x)
let links_snd x = snd (pair_as_ocaml_pair x)

let links_project name = function
  | (`Record fields) -> List.assoc name fields
  | _ -> failwith ("Match failure in record projection")

let escape = 
  Str.global_replace (Str.regexp "\\\"") "\\\""

let delay_expr expr = `Function([], [], (), expr)

let string_of_cont = Show_continuation.show
  
exception Not_tuple

let rec char_of_primchar = function 
    `Char c -> c
  | o -> raise (Match (string_of_result o))

and charlist_as_string chlist = 
  match chlist with
    | `List elems -> 
        Utility.implode (map char_of_primchar elems)
    | _ -> raise (Match("Non-string " ^ string_of_result chlist
                        ^ " used as string."))
    
and string_of_result : result -> string = function
  | #primitive_value as p -> string_of_primitive p
  | `PrimitiveFunction (name) -> name
  | `ClientFunction (name) -> name
  | `Abs result -> "abs " ^ string_of_result result
    (* Choose from fancy or simple printing of functions: *)
(*   | `Function(formal,env,_,body) -> "fun (" ^ formal ^ ") {" ^ Syntax.string_of_expression body ^ "}[" ^ string_of_environment env ^ "]" *)
  | `Function(formal,env,_,body) -> "fun"
  | `Record fields ->
      (try string_of_tuple fields
       with Not_tuple ->
         "(" ^ mapstrcat "," (fun (label, value) ->
                                label ^ "=" ^ string_of_result value)
           (List.sort (fun (l,_) (r, _) -> compare l r) fields) ^ ")")
  | `Variant (label, `Record []) -> label ^ "()"
  | `Variant (label, value) -> label ^ "(" ^ string_of_result value ^ ")"
  | `List [] -> "[]"
  | `List (`Char _::_) as c  -> "\"" ^ escape (charlist_as_string c) ^ "\""
  | `List ((`XML _)::_ as elems) -> mapstrcat "" string_of_xresult elems
  | `List (elems) -> "[" ^ String.concat ", " (map string_of_result elems) ^ "]"
  | `Continuation cont -> string_of_cont cont
and string_of_primitive : primitive_value -> string = function
  | `Bool value -> string_of_bool value
  | `Int value -> string_of_num value
  | `Float value -> string_of_float value
  | `Char c -> "'"^ Char.escaped c ^"'"
  | `XML x -> string_of_item x
  | `Database (_, params) -> "(database " ^ params ^")"
  | `Table (_, table_name, _) -> "(table " ^ table_name ^")"
  | `NativeString s -> "\"" ^ s ^ "\""
  | _  -> failwith "Unexpected primitive value"
				
and string_of_tuple (fields : (string * result) list) : string = 
    let fields = map (function
                        | x, y when numberp x  -> (int_of_string x, y)
                        | _ -> raise Not_tuple) fields in
    let sorted = sort (fun (x,_) (y, _) -> compare x y) fields in
    let numbers, values = split sorted in 
      if ordered_consecutive numbers && length numbers > 1 && hd numbers = 1 then
        "(" ^ String.concat ", " (map string_of_result values) ^ ")"
      else raise Not_tuple 

and numberp s = try ignore(int_of_string s); true with _ -> false

and string_of_binding : binding -> string = function (name, expr) ->
  name ^ " = " ^ (string_of_result expr)

and string_of_environment : binding list -> string = fun env ->
                            String.concat ", " (map (string_of_binding) env)

and string_of_xresult = function 
  | `Char c -> String.make 1 c
  | otherwise -> string_of_result otherwise

(** {0 generic visitation functions for results} *)

let rec map_result result_f expr_f contframe_f : result -> result = function
  | #primitive_value as x -> result_f x
  | `PrimitiveFunction (str) ->
      result_f (`PrimitiveFunction str)
  | `Abs (result) ->
      result_f (`Abs (map_result result_f expr_f contframe_f result))
  | `ClientFunction (str) ->
      result_f (`ClientFunction str)
  | `Function (str, locals, _globals, body) ->
      result_f(`Function(str, 
                         map_env result_f expr_f contframe_f locals,
(*                         map_env result_f expr_f contframe_f globals,*)
                         (),
                         map_expr result_f expr_f contframe_f body))
  | `Record fields -> result_f(`Record(alistmap (map_result result_f expr_f contframe_f) fields))
  | `Variant(tag, body) -> result_f(`Variant(tag, map_result result_f expr_f contframe_f body))
  | `List(elems) -> result_f(`List(map (map_result result_f expr_f contframe_f) elems))
  | `Continuation kappa -> result_f(`Continuation ((map_cont result_f expr_f contframe_f) kappa))
and map_contframe result_f expr_f contframe_f : contin_frame -> contin_frame = function
  | Recv (env) -> contframe_f(Recv(map_env result_f expr_f contframe_f env))
  | Definition (env, s) ->
      contframe_f(Definition(map_env result_f expr_f contframe_f env, s))
  | FuncArg(args, env) -> 
      contframe_f 
        (FuncArg (map (map_expr result_f expr_f contframe_f) args,
                  map_env result_f expr_f contframe_f env))
  | ThunkApply env ->
      contframe_f (ThunkApply (map_env result_f expr_f contframe_f env))
  | FuncApply(env, f, args, eargs) ->
      contframe_f( FuncApply(map_env result_f expr_f contframe_f env,
                             map_result result_f expr_f contframe_f f,
                             map (map_expr result_f expr_f contframe_f) args,
                             map (map_result result_f expr_f contframe_f) eargs))
  | FuncApplyFlipped(env, a) -> 
      contframe_f(FuncApplyFlipped((map_env result_f expr_f contframe_f) env,
                  (map_result result_f expr_f contframe_f) a))
  | LetCont(env, var, body) -> 
      contframe_f(LetCont((map_env result_f expr_f contframe_f) env, var, (map_expr result_f expr_f contframe_f) body))
  | BranchCont(env, tru, fls) -> 
      contframe_f(BranchCont((map_env result_f expr_f contframe_f) env, 
                             (map_expr result_f expr_f contframe_f) tru, (map_expr result_f expr_f contframe_f) fls))
  | BinopRight(env, op, rhs) -> 
      contframe_f(BinopRight((map_env result_f expr_f contframe_f) env, op, (map_expr result_f expr_f contframe_f) rhs))
  | BinopApply(env, op, lhs) -> 
      contframe_f(BinopApply((map_env result_f expr_f contframe_f) env, op,
                             (map_result result_f expr_f contframe_f) lhs))
  | UnopApply(env, op) -> 
      contframe_f(UnopApply((map_env result_f expr_f contframe_f) env, op))
  | RecSelect(env, label, var, label_var, body) -> 
      contframe_f(RecSelect((map_env result_f expr_f contframe_f) env, label, var, label_var, (map_expr result_f expr_f contframe_f) body))
  | CollExtn(env, var, body, results, inputs) ->
      contframe_f(CollExtn((map_env result_f expr_f contframe_f) env, var, (map_expr result_f expr_f contframe_f) body, 
                           map (map (map_result result_f expr_f contframe_f)) results, map (map_result result_f expr_f contframe_f) inputs))
  | StartCollExtn(env, var, body) ->
      contframe_f(StartCollExtn((map_env result_f expr_f contframe_f) env, var, (map_expr result_f expr_f contframe_f) body))
  | XMLCont(env, tag, attr_name, children, attr_exprs, elem_exprs) ->
      contframe_f(XMLCont((map_env result_f expr_f contframe_f) env, tag, attr_name, children,
                          alistmap (map_expr result_f expr_f contframe_f) attr_exprs, map (map_expr result_f expr_f contframe_f) elem_exprs))
  | Ignore(env, next) -> 
      contframe_f(Ignore((map_env result_f expr_f contframe_f) env, (map_expr result_f expr_f contframe_f) next))
  | IgnoreDef(env, next) -> 
      contframe_f(IgnoreDef((map_env result_f expr_f contframe_f) env, (map_def result_f expr_f contframe_f) next))
and map_expr _ expr_f _ expr =
  expr_f expr
and map_def _ expr_f _ = 
  transform_def expr_f
and map_env result_f expr_f contframe_f env =
  alistmap (map_result result_f expr_f contframe_f) env
and map_cont result_f expr_f contframe_f kappa =
  map (map_contframe result_f expr_f contframe_f) kappa

(** return a list of all the core-syntax expressions hidden within
    a result. A bit hackish. *)
let rec extract_code_from_result : result -> 'a expression' list = 
  function
    | #primitive_value -> []
    | `PrimitiveFunction _ -> []
    | `ClientFunction _ -> []
    | `Abs result -> extract_code_from_result result
    | `Function (str, locals, _globals, body) ->
        extract_code_from_env locals @ [body]
    | `Record fields -> concat_map (snd ->- extract_code_from_result) fields
    | `Variant(tag, body) -> extract_code_from_result body
    | `List(elems) -> concat_map (extract_code_from_result) elems
    | `Continuation kappa -> extract_code_from_cont kappa
and extract_code_from_env env = 
  concat_map (snd ->- extract_code_from_result) env
and extract_code_from_cont kappa = 
  concat_map (extract_code_from_contframe) kappa
and extract_code_from_contframe = 
  function
    | Recv (env) -> extract_code_from_env env
    | Definition (env, s) ->
        extract_code_from_env env
    | FuncArg(args, env) ->
        args @ extract_code_from_env env
    | FuncApply(env, f, args, eargs) -> 
        extract_code_from_env env
        @ extract_code_from_result f
        @ args
        @ concat_map extract_code_from_result eargs
    | ThunkApply env -> extract_code_from_env env
    | FuncApplyFlipped(env, a) -> 
        extract_code_from_result a @ extract_code_from_env env
    | LetCont(env, var, body) ->
        extract_code_from_env env @ [body]
    | BranchCont(env, tru, fls) -> 
        extract_code_from_env env @ [tru; fls]
    | BinopRight(env, op, rhs) -> 
        extract_code_from_env env @ [rhs]
    | BinopApply(env, op, lhs) -> 
        extract_code_from_env env @ extract_code_from_result lhs
    | UnopApply(env, op) -> 
        extract_code_from_env env
    | RecSelect(env, label, var, label_var, body) -> 
        assert(false)
    | CollExtn(env, var, body, results, inputs) ->
        assert(false)
    | StartCollExtn(env, var, body) ->
        assert(false)
    | XMLCont(env, tag, attr_name, children, attr_exprs, elem_exprs) ->
        assert(false)
    | Ignore(env, next) ->
        extract_code_from_env env @ [next]

(** {0 Resolving labels } *)

(** Compute a label table from a program. For use with
    [resolve_label], below. *)
(* Presently these tables are a-lists, which are slow, and we don't
   have any sanity check against redundant labels. Switch to hash tables?*)
let expr_label_table (expr : Syntax.expression) = 
  reduce_expression (fun visit_children expr ->
		       match expression_data expr with
			 | `T(_, _, Some label) ->
                             (label, expr) :: visit_children expr
 			 | `T(_, _, None) ->
			     visit_children expr
		    ) (fun (_, lists) -> concat lists) expr 

let program_label_table program =
  reduce_program 
    (fun visit_children expr ->
       match expression_data expr with
	 | `T(_, _, Some label) ->
             (label, expr) :: visit_children expr
 	 | `T(_, _, None) ->
	     visit_children expr)
    (fun (_, lists) -> concat lists)
    (fun (_, lists) -> concat lists)
    (fun (xs, x) -> concat (xs @ [x]))
    program
  

let val_label_table value =
  let exprs = extract_code_from_result value in
    concat_map expr_label_table exprs



(** resolve_label
    Given a label and a label table, return the expression having the
    corresponding label.
    Currently uses linear search
*)
let resolve_label table label : 'a expression' =
  try
    assoc label table
  with
      Not_found -> (Debug.print("label not found: ");
                    raise Not_found)

let resolve_placeholder table e =
  match expression_data e with
    | `T(_, _, Some l) -> resolve_label table l
    | _ -> e
      
let resolve_placeholders_result table rslt = 
  map_result identity (resolve_placeholder table) identity rslt

let resolve_placeholders_cont table rslt = 
  map_cont identity (resolve_placeholder table) identity rslt

let resolve_placeholders_env table env = 
  map_env identity (resolve_placeholder table) identity env

let resolve_placeholders_expr table expr = 
  map_expr identity (resolve_placeholder table) identity expr


let label_of_expression expr =
  let (_, _, label) = expression_data expr in fromOption "TUNLABELED" label

(* boxing and unboxing of primitive types *)
let box_bool b = `Bool b
and unbox_bool : result -> bool   = function
  | `Bool b  -> b | _ -> failwith "Type error unboxing bool"
and box_int i = `Int i      
and unbox_int  : result -> num    = function
  | `Int i   -> i
  | other -> failwith("Type error unboxing int (got "^ string_of_result other ^" : " ^ string_of_value_type other ^ ")")
and box_float f = `Float f  
and unbox_float : result -> float = function
  | `Float f -> f | _ -> failwith "Type error unboxing float"
and box_char c = `Char c    
and unbox_char :  result -> char = function
  | `Char f -> f | _ -> failwith "Type error unboxing char"
and box_xml x = `XML x      
and unbox_xml  :  result -> xmlitem = function
  | `XML x -> x | _ -> failwith "Type error unboxing xml"
and box_string = string_as_charlist
and unbox_string : result -> string = charlist_as_string
and box_list l = `List l
and unbox_list : result -> result list = function
  | `List l -> l | _ -> failwith "Type error unboxing list"
and box_unit : unit -> result 
  = fun () -> `Record []
and unbox_unit : result -> unit = function
  | `Record [] -> () | _ -> failwith "Type error unboxing unit"


(* Retain only bindings in env named by members of `names' *)
let retain names env = filter (fun (x, _) -> StringSet.mem x names) env

(* Pickling interface *)
(* TODO: re-open db connections as necessary *)

module Shelve_ExprEnv = Shelve.Shelve_2(Shelve_rexpr)(Shelve_environment)
module Pickle_ExprEnv = Pickle.Pickle_2(Pickle_rexpr)(Pickle_environment)

let marshal_continuation (c : continuation) : string
  = (* Debug.print("marshaling:"^ string_of_cont c); *)
  let pickle = Pickle_continuation.pickleS c in
    Debug.print("marshalled continuation size: " ^ 
                  string_of_int(String.length pickle));
    let result = Netencoding.Base64.encode pickle in
      result
    
let marshal_exprenv : (expression * environment) -> string
  = Pickle_ExprEnv.pickleS ->- Netencoding.Base64.encode

let marshal_value : result  -> string
  = Pickle_result.pickleS ->- Netencoding.Base64.encode

exception UnrealizableContinuation

let unmarshal_continuation valenv program : string -> continuation
  = 
  (* a bit hackish: we need to extract all the core-syntax expressions
     from the valenv, since placeholders may make reference to them. *)
  let table = (program_label_table program
                 @ concat_map val_label_table valenv) in
    Netencoding.Base64.decode
    ->- Pickle_continuation.unpickleS
    ->- resolve_placeholders_cont table 

let unmarshal_exprenv valenv program : string -> (expression * environment)
  = let resolve (expr, env) =     
    try
      let table = (program_label_table program
                   @ concat_map val_label_table valenv) in
        (resolve_placeholders_expr table expr, 
         resolve_placeholders_env table env) 
    with Not_found ->
      Debug.print("\nlabel didn't match code: " ^
                    labelled_string_of_expression expr);
      raise UnrealizableContinuation
 in
  Netencoding.Base64.decode
  ->- Pickle_ExprEnv.unpickleS
  ->- resolve

let unmarshal_value : string -> result =
  Netencoding.Base64.decode ->- Pickle_result.unpickleS
