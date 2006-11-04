(*pp deriving *)
open Num
open List

open Types
open Syntax
open Utility
open Debug

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
  method virtual equal_types : Types.datatype -> db_field_type -> bool
  method virtual escape_string : string -> string
  method virtual exec : string -> dbresult
  method make_insert_query : (string * string list * string list list) -> string =
    fun (table_name, field_names, vss) ->
      "insert into " ^ table_name ^
        "("^String.concat "," field_names ^") values "^
        String.concat "," (List.map (fun vs -> "(" ^ String.concat "," vs ^")") vss)
end

module Show_database = Show_unprintable (struct type a = database end)

(* Here we could do something better, like pickling enough information
   about the database to be able to restore the connection on
   deserialisation *)
module Pickle_database = Pickle.Pickle_unpicklable (struct type a = database let tname = "Result.database" end)

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
    debug ("registering driver for " ^ name);
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

type unop = MkColl
            | MkVariant of string
            | MkDatabase
            | VrntSelect of (string * string * expression * string option * 
                               expression option)
            | QueryOp of (Query.query * (* table aliases: *) string list)
                deriving (Show, Pickle)
		
let string_of_unop = Show_unop.show

type comparison = Syntax.comparison deriving (Show, Pickle)
type binop = [ `Union | `RecExt of string | `MkTableHandle of row | comparison]
                 deriving (Show, Pickle)

type xmlitem =   Text of string
               | Attr of (string * string)
               | Node of (string * xml)
and xml = xmlitem list
    deriving (Show, Pickle)

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

(* Pickling a result value involves replacing expression nodes with
   identifiers.

   This (rexpr) is pulled out as a separate type so that we can
   provide a custom pickling strategy (namely replacing expressions
   with labels while pickling).
*)
type rexpr = expression
    deriving (Show)

module Pickle_rexpr : Pickle with type a = rexpr = Pickle.Pickle_defaults(
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
        Syntax.Placeholder (label, (`T(Syntax.dummy_position, `Not_typed, Some label)))
  end)

type table = database * string * Types.row
   deriving (Show, Pickle)

type primitive_value = [
| `Bool of bool
| `Int of num
| `Float of float
| `Char of char
| `XML of xmlitem
| `Database of (database * string)
| `Table of table
                ]
    deriving (Show, Pickle)

type contin_frame = 
  | Definition of (environment * string)
  | FuncArg of (rexpr * environment) (* FIXME: This is twiddled *)
  | FuncApply of (result * environment )  (* FIXME: This is twiddled *)
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

  | Recv of (environment)
and result = [
  | `PFunction of (string * result list)
  | `Function of  (string * environment (*locals*) * unit (*globals*) * rexpr)
  | `Record of ((string * result) list)
  | `Variant of (string * result)
  | `List of (result list)
  | `Continuation of continuation
  |  primitive_value

]
and continuation = contin_frame list
and binding = (string * result)
and environment = (binding list)
    deriving (Show, Pickle)

let expr_of_prim_val : result -> expression option = function
    `Bool b -> Some(Boolean(b, Syntax.no_expr_data))
  | `Int i -> Some(Integer(i, Syntax.no_expr_data))
  | `Char ch -> Some(Char(ch, Syntax.no_expr_data))
  | `Float f -> Some(Float(f, Syntax.no_expr_data))
  | _ -> None

let prim_val_of_expr : expression -> result option = function
    Boolean(b, _) -> Some( `Bool b)
  | Integer(i, _) -> Some(`Int i)
  | Char(ch, _) -> Some(`Char ch)
  | Float(f, _) -> Some(`Float f)
  | _ -> None

let (toplevel: continuation) = [] 

let xmlitem_of : result -> xmlitem = function
  | `XML x -> x
  | _ -> raise (Match_failure ("", 0,0))

and bool b =`Bool b
and int i = `Int i
and float f = `Float f
and char c = `Char c
and listval es = `List es
and xmlnodeval contents = `XML (Node contents)

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

let delay_expr expr = `Function(gensym (), [], (), expr)

let pp_continuation = Show_continuation.show
  
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
  | `PFunction (name, _) -> name
  | `Function (_, _, _, Placeholder (str, _)) -> "fun [" ^ Show_label.show str ^ "]"
  | `Function _ -> "fun"
  | `Record fields ->
      (try string_of_tuple fields
       with Not_tuple ->
         "(" ^ mapstrcat "," (fun (label, value) -> 
                                label ^ "=" ^ string_of_result value) 
           fields ^ ")")
  | `Variant (label, `Record []) -> label ^ "()"
  | `Variant (label, value) -> label ^ "(" ^ string_of_result value ^ ")"
  | `List [] -> "[]"
  | `List (`Char _::_) as c  -> "\"" ^ escape (charlist_as_string c) ^ "\""
  | `List ((`XML _)::_ as elems) -> mapstrcat "" string_of_xresult elems
  | `List (elems) -> "[" ^ String.concat ", " (map string_of_result elems) ^ "]"
  | `Continuation cont -> pp_continuation cont
and string_of_primitive : primitive_value -> string = function
  | `Bool value -> string_of_bool value
  | `Int value -> string_of_num value
  | `Float value -> string_of_float value
  | `Char c -> "'"^ Char.escaped c ^"'"
  | `XML x -> string_of_item x
  | `Database (_, params) -> "(database " ^ params ^")"
  | `Table (_, table_name, _) -> "(table " ^ table_name ^")"

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

(* generic visitation functions for results *)

let rec map_result result_f expr_f contframe_f : result -> result = function
  | #primitive_value as x -> result_f x
  | `PFunction (str, pargs) ->
      result_f(`PFunction(str, map (map_result result_f expr_f contframe_f) pargs))

  | `Function (str, locals, globals, body) ->
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
  | FuncArg(arg, env) -> 
      contframe_f(FuncArg((map_expr result_f expr_f contframe_f) arg, (map_env result_f expr_f contframe_f) env))
  | FuncApply(f, env) -> 
      contframe_f(FuncApply((map_result result_f expr_f contframe_f) f, (map_env result_f expr_f contframe_f) env))
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
and map_expr _ expr_f _ expr =
  expr_f expr
and map_env result_f expr_f contframe_f env =
  alistmap (map_result result_f expr_f contframe_f) env
and map_cont result_f expr_f contframe_f kappa =
  map (map_contframe result_f expr_f contframe_f) kappa

(* resolving labels *)

let label_table (program : Syntax.expression) = 
  reduce_expression (fun visit_children expr ->
		       match expression_data expr with
			 | `T(_, _, Some label) ->
                             (label, expr) :: visit_children expr
 			 | `T(_, _, None) ->
			     visit_children expr
		    ) (fun (_, lists) -> concat lists) program 
    
(** resolve_label
    Given a program and label, return the expression having the
    corresponding label. Currently very inefficient because it
    generates a complete label table for the program each time. We
    should generate this table once and keep it with the program.
*)
let resolve_label program label : 'a expression' =
  try
    assoc label (concat_map label_table program) 
  with
      Not_found -> (prerr_endline("Placeholder not found: " ^ Show_label.show label);
                    raise Not_found)

let resolve_placeholder program = function
    Placeholder(s,_) -> resolve_label program s
  | x -> x
      
let resolve_placeholders_result program rslt = 
  map_result identity (resolve_placeholder program) identity rslt

let resolve_placeholders_cont program rslt = 
  map_cont identity (resolve_placeholder program) identity rslt

let resolve_placeholders_env program env = 
  map_env identity (resolve_placeholder program) identity env

let resolve_placeholders_expr program expr = 
  map_expr identity (resolve_placeholder program) identity expr

let label_of_expression expr =
  let (_, _, label) = expression_data expr in fromOption "TUNLABELED" label

(* boxing and unboxing of primitive types *)
let box_bool b = `Bool b
and unbox_bool : result -> bool   = function
  | `Bool b  -> b | _ -> failwith "Type error unboxing bool"
and box_int i = `Int i      
and unbox_int  : result -> num    = function
  | `Int i   -> i | _ -> failwith "Type error unboxing int"
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
let retain names env = filter (fun (x, _) -> mem x names) env

(* Pickling interface *)
(* TODO: re-open db connections as necessary *)

module Pickle_ExprEnv = Pickle.Pickle_2(Pickle_rexpr)(Pickle_environment)

let marshal_continuation : continuation -> string
  = Pickle_continuation.pickleS ->- Netencoding.Base64.encode
let marshal_exprenv : (expression * environment) -> string
  = Pickle_ExprEnv.pickleS ->- Netencoding.Base64.encode

let unmarshal_continuation program : string -> continuation
  = Netencoding.Base64.decode
  ->- Pickle_continuation.unpickleS
  ->- resolve_placeholders_cont program 
let unmarshal_exprenv program : string -> (expression * environment)
  = let resolve (expr, env) = 
     resolve_placeholders_expr program expr, 
     resolve_placeholders_env program env  in
  Netencoding.Base64.decode
  ->- Pickle_ExprEnv.unpickleS
  ->- resolve
