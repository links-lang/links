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
type regexflag = [`RegexList | `RegexNative | `RegexGlobal | `RegexReplace ]
type logical_binop = [`And | `Or ]
type binop = [ `Minus | `FloatMinus | `RegexMatch of regexflag list | logical_binop | `Cons | `Name of name | `App ]

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
  type binding
end

module type Phrase = sig
  module P : PhraseArgs

  type phrase = P.phrase
  type ppattern = P.ppattern
  type binding = P.binding

  type binder = ppattern * phrase

  type constant = [
  | `Float of float
  | `Int of Num.num
  | `String of string
  | `Bool of bool
  | `Char of char ]

  type pattern = [
  | `Any
  | `Nil
  | `Cons of (ppattern * ppattern)
  | `List of (ppattern list)
  | `Variant of (string * ppattern option)
  | `Record of ((string * ppattern) list * ppattern option)
  | `Tuple of (ppattern list)
  | `Constant of constant
  | `Variable of string
  | `As of (string * ppattern)
  | `HasType of ppattern * datatype
  ]
      
  type phrasenode = [
  | `Constant of constant
  | `Var of (name)
  | `FunLit of funlit
  | `Spawn of phrase
  | `ListLit of (phrase list)
  | `Iteration of ([ `List of binder | `Table of binder ] * phrase * (*where:*)phrase option 
                  * (*orderby:*)phrase option)
  | `Escape of (name * phrase)
  | `Section of ([`Minus | `FloatMinus|`Project of name|`Name of name])
  | `Conditional of (phrase * phrase * phrase)
  | `Block of block
  | `InfixAppl of (binop * phrase * phrase)
  | `Regex of (regex)
  | `UnaryAppl of (unary_op * phrase)
  | `FnAppl of (phrase * (phrase list * pposition))
  | `TupleLit of (phrase list)
  | `RecordLit of ((name * phrase) list * phrase option)
  | `Projection of (phrase * name)
  | `With of (phrase * (name * phrase) list)
  | `TypeAnnotation of (phrase * datatype)
  | `ConstructorLit of (name * phrase option)
  | `Switch of (phrase * binder list)
  | `Receive of binder list
  | `DatabaseLit of (phrase * (phrase option * phrase option))
  | `TableLit of (phrase * datatype * (string * fieldconstraint list) list * phrase)
  | `DBDelete of (binder * phrase option)
  | `DBInsert of (phrase * phrase)
  | `DBUpdate of (binder * phrase option * (name * phrase) list)
  | `Xml of (name * (string * (phrase list)) list * phrase list)
  | `TextNode of (string)
  | `Formlet of (phrase * phrase)
  | `FormBinding of (phrase * ppattern) ]
  and block = binding list * phrase
  and binding' = [
  | `Val of ppattern * phrase * location * datatype option
  | `Fun of name * funlit * location * datatype option
  | `Funs of (name * funlit * location * datatype option) list
  | `Foreign of name * name * datatype
  | `Type of (name * name list * datatype)
  | `Infix
  | `Exp of phrase ]
  and funlit = ppattern list list * phrase
  and regex = [
  | `Range of (char * char)
  | `Simply of string
  | `Quote of regex
  | `Any
  | `StartAnchor
  | `EndAnchor
  | `Seq of regex list
  | `Alternate of (regex * regex)
  | `Group of regex
  | `Repeat of (Regex.repeat * regex)
  | `Splice of phrase
  | `Replace of (regex * [`Literal of string | `Splice of phrase]) ]
end

module rec Untyped
 : Phrase with module P = UntypedArgs
 = Untyped
and UntypedArgs : sig
  type phrase   = Untyped.phrasenode * pposition
  type ppattern = Untyped.pattern * pposition
  type binding = Untyped.binding' * pposition
end
  = UntypedArgs

include Untyped

type directive = string * string list

type sentence = [ 
  `Definitions of binding list
| `Expression of phrase
| `Directive of directive ]
      
type sentence' = [ `Definitions of Syntax.untyped_definition list
| `Expression of Syntax.untyped_expression
| `Directive of directive ]
