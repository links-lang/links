(* The syntax tree created by the parser. *)

type name = string
type unary_op = [
| `Minus
| `FloatMinus
| `Not
                ]
type comparison_binop = [`Eq | `Less | `LessEq | `Greater | `GreaterEq | `NotEq | `RegexMatch ]
type arith_binop = [`Times | `Div | `Exp | `Plus | `Minus | `FloatTimes | `FloatDiv | `FloatExp | `FloatPlus | `FloatMinus]
type logical_binop = [`And | `Or]
type binop = [comparison_binop | logical_binop | arith_binop | `Concat | `Cons]

type operator = [ unary_op | binop | `Project of name ]

type pposition = Lexing.position * Lexing.position (* start * end *)

type location = Syntax.location
type datatype = 
  | TypeVar of string
  | FunctionType of datatype * datatype
  | MuType of string * datatype
  | UnitType
  | TupleType of (datatype list)
  | RecordType of row
  | VariantType of row
  | TableType of row
  | ListType of datatype
  | MailboxType of datatype
  | PrimitiveType of Types.primitive
  | DBType
and row = (string * [`Present of datatype | `Absent]) list * string option

type pattern = [
  | `Any
  | `Nil
  | `Cons of (ppattern * ppattern)
  | `List of (ppattern list)
  | `Variant of (string * ppattern option)
  | `Record of ((string * ppattern) list * ppattern option)
  | `Tuple of (ppattern list)
  | `Constant of phrase
  | `Variable of string
  | `As of (string * ppattern)
  | `HasType of ppattern * datatype
]
and ppattern = pattern * pposition
and phrasenode =
  | FloatLit of (float)
  | IntLit of (Num.num)
  | StringLit of (string)
  | BoolLit of (bool)
  | CharLit of (char)
  | Var of (name)
  | FunLit of (name option * ppattern list * phrase)
  | Spawn of phrase
  | ListLit of (phrase list)
  | Definition of (ppattern * phrase * location)
  | Iteration of (ppattern * phrase * phrase * (*where:*)phrase option * (*orderby:*)phrase option)
  | Escape of (name * phrase)
  | HandleWith of (phrase * name * phrase)
  | Section of ([arith_binop|`Project of name])
  | Conditional of (phrase * phrase * phrase)
  | Binding of binder
  | Block of (phrase list * phrase)
  | Foreign of (name * name * datatype)
  | InfixAppl of (binop * phrase * phrase)
  | Regex of (regex)
  | UnaryAppl of (unary_op * phrase)
  | FnAppl of (phrase * (phrase list * pposition))
  | Send of (phrase * phrase)
  | TupleLit of (phrase list)
  | RecordLit of ((name * phrase) list * phrase option)
  | Projection of (phrase * name)
  | SortBy_Conc of (ppattern * phrase * phrase)
  | TypeAnnotation of (phrase * datatype)
  | TypeSignature of (name * datatype)
  | ConstructorLit of (name * phrase option)
  | Switch of (phrase * (binder list) * (name * phrase) option)
  | Receive of ((binder list) * (name * phrase) option)
  | DatabaseLit of (phrase)
  | TableLit of (phrase * datatype * bool (* unique *) * phrase)
  | DBDelete of (phrase * phrase)
  | DBInsert of (phrase * phrase)
  | Xml of (name * (string * (phrase list)) list * phrase list)
  | XmlForest of (phrase list)
  | TextNode of (string)
and phrase = phrasenode * pposition
and binder = ppattern * phrase
and regex = | Range of (char * char)
            | Simply of string
            | Any
            | Seq of regex list
            | Repeat of (Regex.repeat * regex)
            | Splice of phrase

type directive = string * string list
type sentence = (phrase list, directive) Utility.either
type sentence' = (Syntax.untyped_expression list, directive) Utility.either

