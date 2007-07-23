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

type fieldconstraint = [ `Readonly ]

module type PhraseArgs = sig
  type ppattern
  type phrase
  type binder
end

module type Phrase = sig
  module P : PhraseArgs

  type pattern = [
  | `Any
  | `Nil
  | `Cons of (P.ppattern * P.ppattern)
  | `List of (P.ppattern list)
  | `Variant of (string * P.ppattern option)
  | `Record of ((string * P.ppattern) list * P.ppattern option)
  | `Tuple of (P.ppattern list)
  | `Constant of P.phrase
  | `Variable of string
  | `As of (string * P.ppattern)
  | `HasType of P.ppattern * datatype
  ]
      
  type phrasenode = [
  | `FloatLit of (float)
  | `IntLit of (Num.num)
  | `StringLit of (string)
  | `BoolLit of (bool)
  | `CharLit of (char)
  | `Var of (name)
  | `FunLit of (name option * P.ppattern list list * P.phrase)
  | `Spawn of P.phrase
  | `ListLit of (P.phrase list)
  | `Definition of (name * P.phrase * location)
  | `Iteration of ([ `List of P.binder | `Table of P.binder ] * P.phrase * (*where:*)P.phrase option 
                  * (*orderby:*)P.phrase option)
  | `Escape of (name * P.phrase)
  | `HandleWith of (P.phrase * name * P.phrase)
  | `Section of ([`Minus | `FloatMinus|`Project of name|`Name of name])
  | `Conditional of (P.phrase * P.phrase * P.phrase)
  | `Binding of P.binder
  | `Block of (P.phrase list * P.phrase)
  | `Foreign of (name * name * datatype)
  | `InfixDecl
  | `InfixAppl of (binop * P.phrase * P.phrase)
  | `Regex of (regex)
  | `UnaryAppl of (unary_op * P.phrase)
  | `FnAppl of (P.phrase * (P.phrase list * pposition))
  | `TupleLit of (P.phrase list)
  | `RecordLit of ((name * P.phrase) list * P.phrase option)
  | `Projection of (P.phrase * name)
  | `With of (P.phrase * (name * P.phrase) list)
  | `SortBy_Conc of (P.ppattern * P.phrase * P.phrase)
  | `TypeAnnotation of (P.phrase * datatype)
  | `TypeDeclaration of (name * name list * datatype)
  | `ConstructorLit of (name * P.phrase option)
  | `Switch of (P.phrase * P.binder list)
  | `Receive of P.binder list
  | `DatabaseLit of (P.phrase * (P.phrase option * P.phrase option))
  | `TableLit of (P.phrase * datatype * (string * fieldconstraint list) list * P.phrase)
  | `DBDelete of (P.binder * P.phrase option)
  | `DBInsert of (P.phrase * P.phrase)
  | `DBUpdate of (P.binder * P.phrase option * (name * P.phrase) list)
  | `Xml of (name * (string * (P.phrase list)) list * P.phrase list)
  | `TextNode of (string)
  | `Formlet of (P.phrase * P.phrase)
  | `FormBinding of (P.phrase * P.ppattern) ]
and regex' = [ `Range of (char * char)
             | `Simply of string
             | `Quote of regex'
             | `Any
             | `StartAnchor
             | `EndAnchor
             | `Seq of regex' list
             | `Alternate of (regex' * regex')
             | `Group of regex'
             | `Repeat of (Regex.repeat * regex')
             | `Splice of P.phrase
	     | `Replace of (regex' * [`Literal of string | `Splice of P.phrase]) ]
and regexflags = [`RegexList | `RegexNative | `RegexGlobal]
and regex = regex' * (regexflags list)
end

module rec UntypedSyntax
 : Phrase with module P = UntypedArgs
 = UntypedSyntax
and UntypedArgs : sig
  type binder  = ppattern * phrase
  and phrase   = UntypedSyntax.phrasenode * pposition
  and ppattern = UntypedSyntax.pattern * pposition
end
  = UntypedArgs

include UntypedSyntax
include UntypedArgs

type directive = string * string list

type sentence = [ 
  `Definitions of phrase list
| `Expression of phrase
| `Directive of directive ]
      
type sentence' = [ `Definitions of Syntax.untyped_definition list
| `Expression of Syntax.untyped_expression
| `Directive of directive ]
