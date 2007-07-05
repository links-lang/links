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
| `Abs
]
type logical_binop = [`And | `Or ]
type binop = [ `Minus | `FloatMinus | `RegexMatch | logical_binop | `Cons | `Name of name | `App ]

type operator = [ unary_op | binop | `Project of name ]

type pposition = Lexing.position * Lexing.position (* start * end *)

type event_name = string

type location = Syntax.location
type datatype = 
  | TypeVar of string
  | RigidTypeVar of string
  | FunctionType of datatype list * datatype * datatype
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
and row = (string * [`Present of datatype | `Absent]) list * row_var
and row_var = [ `Closed | `Open of string | `Recursive of string * row ]

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
  | FunLit of (name option * ppattern list list * phrase)
  | Spawn of phrase
  | ListLit of (phrase list)
  | Definition of (name * phrase * location)
  | Iteration of (generatorphrase * phrase * (*where:*)phrase option 
                  * (*orderby:*)phrase option)
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
  | Formlet of (phrase * phrase * handle_clause list)
  | FormBinding of (phrase * ppattern)
and phrase = phrasenode * pposition
and binder = ppattern * phrase
and regex' = | Range of (char * char)
             | Simply of string
             | Quote of regex'
             | Any
             | StartAnchor
             | EndAnchor
             | Seq of regex' list
             | Alternate of (regex' * regex')
             | Group of regex'
             | Repeat of (Regex.repeat * regex')
             | Splice of phrase
	     | Replace of (regex' * [`ReplaceLiteral of string | `ReplaceSplice of phrase])
and regexflags = RegexList | RegexNative | RegexGlobal
and regex = regex' * (regexflags list)
and handle_clause = Catch of (event_name * phrase)
	 
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
[ `Definitions of Syntax.untyped_definition list
| `Expression of Syntax.untyped_expression
| `Directive of directive
]
