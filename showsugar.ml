(*pp deriving *)

type datatype = Sugartypes.datatype 
    deriving (Show)
type name = string deriving (Show)
type binop = Sugartypes.binop
 deriving (Show)
type fieldconstraint = Sugartypes.fieldconstraint
    deriving (Show)
type unary_op = Sugartypes.unary_op
    deriving (Show)
type location = Sugartypes.location
    deriving (Show)
type num = Num.num


type constant =
    [ `Bool of bool
    | `Char of char
    | `Float of float
    | `Int of num
    | `String of string ]
and pattern =
    [ `Any
    | `As of string * ppattern
    | `Cons of ppattern * ppattern
    | `Constant of constant
    | `HasAnd of ppattern * datatype
    | `List of ppattern list
    | `Nil
    | `Record of (string * ppattern) list * ppattern option
    | `Tuple of ppattern list
    | `Variable of string
    | `Variant of string * ppattern option ]
and phrasenode =
    [ `Block of block
    | `Conditional of phrase * phrase * phrase
    | `Constant of constant
    | `ConstructorLit of name * phrase option
    | `DBDelete of ppattern * phrase * phrase option
    | `DBInsert of phrase * phrase
    | `DBUpdate of ppattern * phrase * phrase option * (name * phrase) list
    | `DatabaseLit of phrase * (phrase option * phrase option)
    | `Escape of name * phrase
    | `FnAppl of phrase * phrase list
    | `FormBinding of phrase * ppattern
    | `Formlet of phrase * phrase
    | `FunLit of funlit
    | `InfixAppl of binop * phrase * phrase
    | `Iteration of
        foo *
        phrase * phrase option * phrase option
    | `ListLit of phrase list
    | `Projection of phrase * name
    | `Receive of (ppattern * phrase) list
    | `RecordLit of (name * phrase) list * phrase option
    | `Regex of regex
    | `Section of foo2
    | `Spawn of phrase
    | `SpawnWait of phrase
    | `Switch of phrase * (ppattern * phrase) list
    | `TableLit of
        phrase * datatype * (string * fieldconstraint list) list * phrase
    | `TextNode of string
    | `TupleLit of phrase list
    | `AndAnnotation of phrase * datatype
    | `UnaryAppl of unary_op * phrase
    | `Var of name
    | `With of phrase * (name * phrase) list
    | `Xml of name * (string * phrase list) list * phrase list ]
and block = binding list * phrase
and binding' =
    [ `Exp of phrase
    | `Foreign of name * name * datatype
    | `Fun of name * funlit * location * datatype option
    | `Funs of (name * funlit * location * datatype option) list
    | `Infix
    | `And of name * name list * datatype
    | `Val of ppattern * phrase * location * datatype option ]
and funlit = ppattern list list * phrase
and regex =
    [ `Alternate of regex * regex
    | `Any
    | `EndAnchor
    | `Group of regex
    | `Quote of regex
    | `Range of char * char
    | `Repeat of Regex.repeat * regex
    | `Replace of regex * foo3
    | `Seq of regex list
    | `Simply of string
    | `Splice of phrase
    | `StartAnchor ]
and directive = string * string list
and sentence =
    [ `Definitions of binding list
    | `Directive of directive
    | `Expression of phrase ]
and foo = [ `List of ppattern * phrase | `Table of ppattern * phrase ]
and foo2 = [ `FloatMinus | `Minus | `Name of name | `Project of name ]
and foo3 = [ `Literal of string | `Splice of phrase ]
and ppattern = pattern * Sugartypes.pposition
and phrase = phrasenode * Sugartypes.pposition
and binding = binding' * Sugartypes.pposition
      deriving (Show)
