(*pp deriving *)

open Utility
open List
open Num

type name = string deriving (Show, Pickle)
type label = name deriving (Show, Pickle)
type op = [Syntaxutils.comparison | `And | `Or] deriving (Show, Pickle)
type pat = (label * name) list
type literal = [`True | `False | `Str of string | `N of num] 
    deriving (Show, Pickle)

type like_expr = [
| `Percent
| `Seq of like_expr list
| `Str of string
| `Var of name ] 
    deriving (Show, Pickle)
type baseexpr = [
|`Op  of op * baseexpr * baseexpr
|`Let of name * baseexpr * baseexpr (* not in the paper, but an easy extension *)
|`Like of baseexpr * like_expr
|`Not of baseexpr
|`Rec of (label * baseexpr) list
|`Var of name
| literal] deriving (Show)
type valueexpr = [
|`Var of name
| literal] deriving (Show)
type sorting = [`Asc of (string * string) | `Desc of (string * string)]
    deriving (Show, Pickle)
type simpleExpr = [
|`For    of pat * simpleExpr * simpleExpr
|`Where  of baseexpr * simpleExpr
|`Let    of name * baseexpr * simpleExpr
|`Table  of (name * Types.datatype) list * string * string * sorting list
|`Return of baseexpr]
type expr = [
|`Take of num * expr
|`Drop of num * expr
| simpleExpr]

type field = {table:name; column:name; ty:Types.datatype} deriving (Show, Pickle)
type sqlexpr = [
| literal
|`Rec  of (name * sqlexpr) list
|`Op   of op * sqlexpr * sqlexpr
|`Not  of sqlexpr
|`Like of sqlexpr * like_expr
|`F    of field
|`V    of name]
    deriving (Show, Pickle)
type ninf = I of num | Inf
    deriving (Show, Pickle)
type tabSpec = [
  `TableVar of (string (*real name*) * string (* alias*) )
| `TableName of (string (*variable*) * string (* alias*) ) ] 
    deriving (Show, Pickle)
type sqlQuery = {
  cols : (sqlexpr * name) list; (* (e, n) means "select e as n"*)
  tabs : tabSpec list;
  (* (x, a) means "from x as a", and x is a free variable representing a table.*)
  cond : sqlexpr list;
  most : ninf;
  from : num;
  sort : sorting list;
}
    deriving (Show, Pickle)

let rec freevars_like_expr = function
    `Var x -> StringSet.singleton x
  | `Seq xs -> StringSet.union_all (map freevars_like_expr xs)
  | _ -> StringSet.empty

let rec freevars_sqlexpr : sqlexpr -> StringSet.t = function
| `Rec fields -> StringSet.union_all(map (freevars_sqlexpr -<- snd) fields)
| `Op(op, lhs, rhs) -> StringSet.union (freevars_sqlexpr lhs) 
                                       (freevars_sqlexpr rhs)
| `Not e -> freevars_sqlexpr e
| `Like(e, l) -> StringSet.union (freevars_like_expr l) (freevars_sqlexpr e)
| `V x -> StringSet.singleton x
| _ -> StringSet.empty

let freevars_sqlQuery q = 
  StringSet.union (StringSet.union_all (map freevars_sqlexpr q.cond))
    (StringSet.union_all (map freevars_sqlexpr (map fst q.cols)))

let rec subst_likeExpr e = assert false

let rec subst_sqlExpr name expr : sqlexpr -> sqlexpr = function
  | `V var when var = name -> expr
  | `Like(e, l) -> `Like(subst_sqlExpr name expr e, subst_likeExpr name expr l)
  | `Not e -> `Not(subst_sqlExpr name expr e)
  | `Op (op, lhs, rhs) -> `Op (op, subst_sqlExpr name expr lhs, 
                               subst_sqlExpr name expr rhs)
  | `Rec fields -> `Rec (alistmap (subst_sqlExpr name expr) fields)
  | lit -> lit
and subst_sqlQuery var expr query =
  {query with cond = map (subst_sqlExpr var expr) query.cond}
    (* TBD: subst in cols as well? *)

let owning_table of_col qry =
  match List.find (fun (_expr, name) -> name = of_col) qry.cols with
    | (`F field, name) -> field.table
    | _ -> assert false

let sorting_to_sql = function
  | `Asc (table, col)  -> table ^ "." ^ col ^ " ASC" 
  | `Desc (table, col) -> table ^ "." ^ col ^ " DESC"

let rec like_as_string = function 
  | `Percent -> "%"
  | `Seq exprs -> mapstrcat "" like_as_string exprs
  | `Str str -> str
  | `Var x -> "(VARIABLE:" ^ x ^ ")"
  
let string_of_op = function
    `And -> "AND"
  | `Or -> "OR"
  | `Less -> "<"
  | `LessEq -> "<="
  | `Equal -> "="
  | `NotEq -> "<>"

let rec string_of_expression : sqlexpr -> string = function
  | `F field     -> field.table ^"."^ field.column
(*   | NamedField field         -> field *)
  | `V name                  -> "VARIABLE:"^ name
  | `N value                 -> string_of_num value
(*   | Float value              -> string_of_float value *)
  | `True                    -> "TRUE"
  | `False                   -> "FALSE"
  | `Like(e, l)              -> string_of_expression e ^ " LIKE '" ^ 
                                  like_as_string l ^ "'"
  | `Str value               -> "\'"^ value ^"\'"
  | `Op (op, l, r)           -> "("^ string_of_expression l ^" "^ 
                                string_of_op op ^" "^ string_of_expression r ^")"
  | `Not expr                -> "(NOT "^ (string_of_expression expr) ^")"
  | `Rec _                   -> assert false
and string_of_query (qry:sqlQuery) : string =
    "SELECT "
     ^ (match qry.cols with
	  | [] -> "NULL as null"
	  | _ -> (Utility.mapstrcat ", " 
                    (function (expr, alias) ->
                       string_of_expression expr ^ " AS " ^ alias)
                    qry.cols))
     ^ " FROM " ^ (Utility.mapstrcat ", " 
                     (function
                          `TableName (table, alias) ->
                            table ^ " AS " ^ alias
                        | `TableVar(var, alias) -> 
                            "VARIABLE:" ^ var ^ " AS " ^ alias
                     ) qry.tabs) ^
       string_of_condition qry.cond
     ^ (match qry.sort with
	  | [] -> "" 
	  | orders -> " ORDER BY " ^ 
                        Utility.mapstrcat ", " sorting_to_sql orders)
     ^ (match qry.most with
          | Inf   -> ""
          | I m -> " limit " ^ string_of_num m 
              (* NOTE: should allow variable here. *)
	      ^ " offset " ^ string_of_num qry.from)

and string_of_condition cond =
  match (mapstrcat " AND " string_of_expression cond) with
    | "" -> ""
    | where_clause  -> " WHERE " ^ where_clause
