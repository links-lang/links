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
type simpleExpr = [
|`For    of pat * simpleExpr * simpleExpr
|`Where  of baseexpr * simpleExpr
|`Let    of name * baseexpr * simpleExpr
|`Table  of (name * Types.datatype) list * string * string
|`Return of baseexpr]
type expr = [
|`Take of num * expr
|`Drop of num * expr
| simpleExpr]
type sorting = [`Asc of (string * string) | `Desc of (string * string)]
    deriving (Show, Pickle)

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
type tabSpec = [ `TableVar of (string * name)
| `TableName of (string * string) ] 
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

let rec freevars_sqlExpr q = assert false

let freevars_sqlQuery q = 
  StringSet.union (StringSet.union_all (freevars_sqlExpr q.cond))
    (StringSet.union_all (map freevars_sqlExpr (map fst q.cols)))

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

let string_of_query qry = "[some-sql-query]" (* TBD *)
let string_of_expression expr = "[some-sql-expr]" (* TBD *)
