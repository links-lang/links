(** The syntax tree created by the parser. *)

type name = string

(* The operators named here are the ones that it is difficult or
   impossible to define as "user" infix operators:

      - -.  are both infix and prefix
     && ||  have special evaluation
     ::     is also used in patterns
     ~      triggers a lexer state switch
*)
type unary_op = [
| `Minus
| `FloatMinus
| `Name of name
]
type logical_binop = [`And | `Or]
type binop = [ `Minus | `FloatMinus | `RegexMatch | logical_binop | `Cons | `Name of name]

type operator = [ unary_op | binop | `Project of name ]

type pposition = Lexing.position * Lexing.position (* start * end *)

type location = Syntax.location
type datatype = 
  | TypeVar of string
  | RigidTypeVar of string
  | FunctionType of datatype * datatype * datatype
  | MuType of string * datatype
  | UnitType
  | TupleType of (datatype list)
  | RecordType of row
  | VariantType of row
  | TableType of datatype * datatype
  | ListType of datatype
  | TypeApplication of (string * datatype list)
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
  | Definition of (name * phrase * location)
  | Iteration of (generatorphrase * phrase * (*where:*)phrase option * (*orderby:*)phrase option)
  | Escape of (name * phrase)
  | HandleWith of (phrase * name * phrase)
  | Section of ([`Minus | `FloatMinus|`Project of name|`Name of name])
  | Conditional of (phrase * phrase * phrase)
  | Binding of binder
  | Block of (phrase list * phrase)
  | Foreign of (name * name * datatype)
  | InfixDecl
  | InfixAppl of (binop * phrase * phrase)
  | Regex of (regex)
  | UnaryAppl of (unary_op * phrase)
  | FnAppl of (phrase * (phrase list * pposition))
  | TupleLit of (phrase list)
  | RecordLit of ((name * phrase) list * phrase option)
  | Projection of (phrase * name)
  | With of (phrase * (name * phrase) list)
  | SortBy_Conc of (ppattern * phrase * phrase)
  | TypeAnnotation of (phrase * datatype)
  | TypeDeclaration of (name * name list * datatype)
  | ConstructorLit of (name * phrase option)
  | Switch of (phrase * binder list)
  | Receive of binder list
  | DatabaseLit of (phrase * (phrase option * phrase option))
  | TableLit of (phrase * datatype * (string * fieldconstraint list) list * phrase)
  | DBDelete of (rawgeneratorphrase * phrase option)
  | DBInsert of (phrase * phrase)
  | DBUpdate of (rawgeneratorphrase * phrase option * (name * phrase) list)
  | Xml of (name * (string * (phrase list)) list * phrase list)
  | XmlForest of (phrase list)
  | TextNode of (string)
  | Formlet of (phrase * phrase)
  | FormBinding of (phrase * ppattern)
and phrase = phrasenode * pposition
and binder = ppattern * phrase
and regex = | Range of (char * char)
            | Simply of string
            | Any
            | Seq of regex list
            | Repeat of (Regex.repeat * regex)
            | Splice of phrase
and rawgeneratorphrase = ppattern * phrase
and generatorphrase = [ `List of rawgeneratorphrase | `Table of rawgeneratorphrase ]
and fieldconstraint = [ `Readonly ]

type directive = string * string list
type sentence =
[ `Definitions of phrase list
| `Expression of phrase
| `Directive of directive
]

type sentence' = 
[ `Definitions of Syntax.untyped_expression list
| `Expression of Syntax.untyped_expression
| `Directive of directive
]
