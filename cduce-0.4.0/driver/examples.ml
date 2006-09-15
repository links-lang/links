(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

let examples = [ "xml","(* Syntax for XML elements *)

type A = <a x=String y=?String>[ B* ]
type B = <b>[ PCDATA A? PCDATA ]

let x : A = 
 <a x=\"Bla\" y=\"Blo\">[ 
   <b>[ 'blabla' ]
   <b>[ 
     <a x=\"Foo\">[] 'bla' 'bla' 
   ] 
 ]
";"functions","(* Simple functions can be defined this way: *)
let f1 (x : Int) : Int = x + 3
;;
f1 5

(* With several arguments: *)
let f2 (x : Int, y : Int) : Int = x + y
;;
f2 (10,20)

(* Currified form *)
let add (x : Int) (y : Int) : Int = x + y
;;
add 10 20

(* You may directly deconstruct the arguments: *)
type A = <a href=String>String
let f3 (<a href=url>txt : A) : String = url @ \"=>\" @ txt
;;
f3 <a href=\"http://www.cduce.org\">\"CDuce homepage\";;

(* In general, if you want to specify several arrow types, or
   use several pattern matching branches, you have the general
   form: *)

let f4 (A -> String; ['0'--'9'+] -> Int)
| x & A -> f3 x
| x -> int_of x
;;
f4 \"123\"
";"mutrec","(* Adjacent type declarations are mutually recursive *)
type T = <t>S
type S = [ (Char | T)* ]
let x : S = [ 'abc' <t>['def'] 'ghi' ]

(* Similarly for toplevel function definitions *)

let f (x : Int) : Int = g x
let g (x : Int) : Int = 3
let a = 2
let h (x : Int) : Int = f x
   (* f and g are mutually recursive, but they cannot use h *)
";"sequence","(* Sequence are just defined with pairs and the atom `nil;
   the following notation are equivalent: *)
let l1 = (1,2,3,`nil)
let l2 = (1,(2,(3,`nil)))
let l3 = [ 1 2 3 ]

(* The [...] notation allow to specify a tail after a semi-colon : *)
let l4 = (10,20,l1)
let l5 = [ 10 20 ; l1 ]

(* Concatenation @ *)
let l6 = [ 1 2 3 ] @ [ 4 5 6 ]

(* Inside [...], it is possible to escape a subsequence with a ! *)
let l7 = [ 1 2 !l6 !l1 5 ]
";"seqtypes","(* Sequence types are defined with regular expression over types *)
type IntList = [ Int* ]
type IntStringList = [ (Int String)* ]
type IntNonEmptyList = [ Int+ ]

let l : IntList = [ 1 2 3 ]
";"integers","(* Yes, CDuce can handle large integers! *)
let facto (Int -> Int)
 | 0 | 1 -> 1
 | n -> n * (facto (n - 1))
in
facto 300

(* The tail-recursive way *)
let facto ((Int,Int) -> Int)
 | (x, 0 | 1) -> x
 | (x, n) -> facto (x * n, n - 1)
in
facto (1,10000)
";"sumtype","type Expr = 
    (`add, Expr, Expr)
  | (`mul, Expr, Expr)
  | (`sub, Expr, Expr)
  | (`div, Expr, Expr)
  | Int
 
let eval ( Expr -> Int )  
  | (`add,x,y) -> eval x + eval y
  | (`mul,x,y) -> eval x * eval y
  | (`sub,x,y) -> eval x - eval y
  | (`div,x,y) -> (eval x) div (eval y)
  | n -> n 
in
eval (`add, 10, (`mul, 20, 5))
";"ovfun","type Person = FPerson | MPerson 
type FPerson = <person gender = \"F\" >[ Name Children (Tel | Email)?] 
type MPerson = <person gender=\"M\">[ Name Children (Tel | Email)?] 
type Children = <children>[Person*] 
type Name = <name>[ PCDATA ]
type Tel = <tel kind=?\"home\"|\"work\">['0'--'9'+ '-' '0'--'9'+]
type Email = <email>[PCDATA '@' PCDATA]

type Man = <man name=String>[ Sons Daughters ]
type Woman = <woman name=String>[ Sons Daughters ]
type Sons = <sons>[ Man* ]
type Daughters = <daughters>[ Woman* ]

let split (MPerson -> Man ; FPerson -> Woman)
  <person gender=g>[ <name>n <children>[(mc::MPerson | fc::FPerson)*]; _] ->
     let tag = match g with \"F\" -> `woman | \"M\" -> `man in
     let s = map mc with x -> split x in
     let d = map fc with x -> split x in
     <(tag) name=n>[ <sons>s  <daughters>d ] 
 

let base : Person = 
<person gender=\"F\">[ 
  <name>\"Themis\"
  <children>[ 
    <person gender=\"M\">[
      <name>\"Prometheus\"
      <children>[
        <person gender=\"M\">[
          <name>\"Deucalion\"
          <children>[]
        ]
      ]
      <email>\"focifero@olympus.com\"
    ] 
    <person gender=\"M\">[
      <name>\"Epimetheus\"
      <children>[]
      <tel> \"314-1592654\"
    ]
  ] 
  <tel kind=\"home\"> \"271-828182\"
]
in
split base
";"note","type Doc = <doc>Text
type Text = [ (Char | (Letter+ ' '* Note))* ]
type Letter = 'a'--'z' | 'A'--'Z'
type Note = <note>[ PCDATA ]

type Flow = [ (Char | <ref no=Int>[ PCDATA ])* ]
type Notes = [ <note no=Int>[ PCDATA ]* ]
type Result = <doc>[ <body>Flow <notes>Notes ]

let format (<doc>s : Doc) : Result = 
  let (body,notes) = text (s,1) in
  <doc>[ <body>body <notes>notes ]

let text ( (Text,Int) -> (Flow,Notes) )
 | ([ pre::Char*? (word::Letter+ ' '* <note>n); rem ], count) ->
      let (body,notes) = text (rem, count + 1) in
      (pre @ [<ref no=count>word] @ body, 
       [<note no=count>n] @ notes)
 | (body,_) -> (body, [])

let src : Doc = <doc>[ 'CDuce ' <note>\"Frisch, Castagna, Benzaken\"
		 ' is an XML ' <note>\"a W3C standard\"
		 '-friendly programming language.' ]
in
format src
";"biblio","type Biblio  = <bibliography>[Heading Paper*]
type Heading = <heading>[ PCDATA ]
type Paper   = <paper>[ Author+ Title Conference File ]
type Author  = <author>[ PCDATA ]
type Title   = <title>[ PCDATA ]
type Conference = <conference>[ PCDATA ]
type File    = <file>[ PCDATA ]

(* Simplified HTML *)
type Html  = <html>[ <head>[ <title>[ PCDATA ] ]  <body>Mix ]
type Mix   = [ ( <h1>Mix | <a href=String>Mix | <p>Mix | <em>Mix 
	       | <ul>[ <li>Mix +] | Char )* ]

let do_authors ([Author+] -> Mix)
 | [ <author>a ] -> a
 | [ <author>a <author>b ] -> a @ \" and, \" @ b
 | [ <author>a; x] -> a @ \", \" @ (do_authors x)

let do_paper (Paper -> <li>Mix)
  <paper>[ x::_* <title>t <conference>c <file>f ] ->
    <li>[ <a href=f>t !(do_authors x) '; in ' <em>c '.' ]

let do_biblio (Biblio -> Html)
  <bibliography>[ <heading>h; p ] ->
      let body = match p with
      | [] -> \"Empty bibliography\"
      | l -> [ <h1>h <ul>(map l with x -> do_paper x) ]
      in    
      <html>[ <head>[ <title>h ] <body>body ]

let bib : Biblio = 
  <bibliography>[
    <heading>\"Alain Frisch's bibliography\"
    <paper>[
      <author>\"Alain Frisch\"
      <author>\"Giuseppe Castagna\"
      <author>\"Véronique Benzaken\"
      <title>\"Semantic subtyping\"
      <conference>\"LICS 02\"
      <file>\"semsub.ps.gz\"
    ]
    <paper>[
      <author>\"Mariangiola Dezani-Ciancaglini\"
      <author>\"Alain Frisch\"
      <author>\"Elio Giovannetti\"
      <author>\"Yoko Motohama\"
      <title>\"The Relevance of Semantic Subtyping\"
      <conference>\"ITRS'02\"
      <file>\"itrs02.ps.gz\"
    ]
    <paper>[
      <author>\"Véronique Benzaken\"
      <author>\"Giuseppe Castagna\"
      <author>\"Alain Frisch\"
      <title>\"CDuce: a white-paper\"
      <conference>\"PLANX-02\"
      <file>\"planx.ps.gz\"
    ]
 ]
in
do_biblio bib
";"projection","(* The projection  e/t   is translated to:
   transform e with [ (x::t|_)* ]  -> x *)

type Biblio  = <bibliography>[Heading Paper*]
type Heading = <heading>[ PCDATA ]
type Paper   = <paper>[ Author+ Title Conference File ]
type Author  = <author>[ PCDATA ]
type Title   = <title>[ PCDATA ]
type Conference = <conference>[ PCDATA ]
type File    = <file>[ PCDATA ]

let bib : Biblio = 
  <bibliography>[
    <heading>\"Alain Frisch's bibliography\"
    <paper>[
      <author>\"Alain Frisch\"
      <author>\"Giuseppe Castagna\"
      <author>\"Véronique Benzaken\"
      <title>\"Semantic subtyping\"
      <conference>\"LICS 02\"
      <file>\"semsub.ps.gz\"
    ]
    <paper>[
      <author>\"Mariangiola Dezani-Ciancaglini\"
      <author>\"Alain Frisch\"
      <author>\"Elio Giovannetti\"
      <author>\"Yoko Motohama\"
      <title>\"The Relevance of Semantic Subtyping\"
      <conference>\"ITRS'02\"
      <file>\"itrs02.ps.gz\"
    ]
    <paper>[
      <author>\"Véronique Benzaken\"
      <author>\"Giuseppe Castagna\"
      <author>\"Alain Frisch\"
      <title>\"CDuce: a white-paper\"
      <conference>\"PLANX-02\"
      <file>\"planx.ps.gz\"
    ]
 ]

let titles = [bib]/<paper>_/<title>_
let authors = [bib]/<paper>_/<author>_
let titles_concat = [bib]/<paper>_/<title>_/Char
";"xtransform","
(* For the purpose of the example we can consider this hugely
   simplified definition of Xhtml
*)

type Flow = Char | Block | Inline  ;;
type Block = P | Heading | Lists | Blocktext | Char
type Lists = Ul
type Blocktext = Pre |  Address | Center;;
type Inline = Char | A | Fontstyle
type Fontstyle = Tt | I | B | Big | Small;;

type Xhtml = <html>[ Head Body ];;
type Head = <head>[ Title <link>[ ]];;
type Title = <title>[ PCDATA ];;
type Body = <body bgcolor=?String>[ Block* ];;

type P = <p>[ Inline* ];;
type Heading = <(`h1 | `h2 | `h3 | `h4)>[ Inline* ];;

type Ul = <ul>[Li+];;
type Li = <li>[ Flow* ];;

type Address = <address>[ Inline* ];;
type Pre = <pre>[ (PCDATA | A | Fontstyle)* ];;
type Center = <center>[ Block* ];;

type A = <a ({ name = String } | { href = String })>[ (Inline \ A)* ];;
type Tt = <tt>[ Inline* ];;
type I = <i>[ Inline* ];;
type B = <b>[ Inline* ];;
type Big = <big>[ Inline* ];;
type Small = <small>[ Inline* ];;


(* xtransform matches the patterns against the root element of each
   XML tree and, if it fails, it recursively applies itself to the
   sequence of sons of the root.

   It can be used to put in boldface all the links of an XHTML
   document as follows
*)

let bold(x:[Xhtml]):[Xhtml]=xtransform x with <a (y)>t -> [ <a(y)>[<b>t] ]


(* let us apply the function to a document where links appear
   at different depths
*)


let doc : Xhtml =
  <html>[
    <head>[<title>\"Example\" <link>[]]
    <body>[
      <h2>['You can have links ' <a href=\"here\">\"here\"]
      <pre>['Or they can be down']
      <ul>[
        <li>['In ' <a name=\"list\">\"lists\" ' for instance']
	<li>['or you oddly decided to ' 
             <center>[<p>[<a href=\"what?\">\"center\"]] 
             ' them '
            ]
      ]
      <address>[
        'and even if they are in fancy ' <a name=\"address\">\"address boxes\"
      ]
      <p>[
          'nevertheless ' <a href=\"http://www.cduce.org\">\"Cduce\" ' and '
          <a href=\"xtransform\">[<tt>\"xtransform\"] 
          ' will put all links in bold so that when'
          ' you program your transformation you '
          <big>[<a name=\"\">\" don\'t \" ] ' have to worry about it'
     ]
   ]
  ];;

bold [doc];;

let [x] = bold [doc] in print_xml x;;
";"reference","(* In CDuce the expression  \"ref T exp\" returns a reference  *)
(* to the result of \"exp\" and has type \"ref T\" provided that *)
(* \"exp\" is of type \"T\". References come equipped with three *)
(* operators: \":=\" (assignment), \"!\" (dereferencing), and \";\"*) 
(* (sequencing).                                             *)


let stack = ref [Int*] []

let fun push(x : Int) : []  = 
  stack := [x; !stack]

let fun pop ([] : []) : Int = 
  match !stack with [x; y] -> stack := y; x | _ -> raise \"Empty stack\"


(* In a pattern [ ... ; y] the variable y captures the tail  *)
(* of the sequence. It is equivalent to [ ... y::_*].        *)
(* In an expression [ ... ; e ] the expression e denotes the *)
(* tail of the sequence. It is equivalent to [ ... ] @ e     *)


;;

push 1;;
push 2;;
push 3;;
pop [];;
pop [];;
pop [];;
pop [];;
";"pm_compil","(* This example demonstrates the efficient compilation of pattern
   matching. *)

type A = <a>[ Int* ]
type B = <b>[ Char* ]

(* Imagine we want to compile the following function:
   fun ([A+|B+] -> Bool) [A+] -> 0 | [B+] -> 1

   For an arbitrary value, it is expensive to check whether it has
   type [A+] or not. But if we know statically that it has type [A+|B+],
   we just have to check the tag of the first element !

   This is demonstrated by the following internal debugging feature.
   The syntax is:  

   debug compile T P1 ... Pn

   where T is the input type (static information about the matched
   value) and P1,...,Pn are the patterns to compile (simultaneously).

   The \"debug compile\" instruction displays an human-readable
   representation of the automaton corresponding to the pattern
   matching. Note: in actual evaluation, this automaton is build
   lazily (= on-the-fly, = JIT).
*)

debug compile [A+|B+] [A+] [B+]


(* You can see on the output that the pattern matching is actually
   compiled as:

   fun ([A+|B+] -> Int) [ <a>_ ; _ ] -> 0 | _ -> 1
*)
"; ]
let present = "<ul><li><a href=\"/cgi-bin/cduce?example=xml\">XML elements.</a> 
XML elements.
</li><li><a href=\"/cgi-bin/cduce?example=functions\">Functions.</a> 
Several syntaxes to define functions.
</li><li><a href=\"/cgi-bin/cduce?example=mutrec\">Mutual recursion.</a> 
Mutual toplevel definition for types and functions.
</li><li><a href=\"/cgi-bin/cduce?example=sequence\">Sequence literals.</a> 
How to write sequences.
</li><li><a href=\"/cgi-bin/cduce?example=seqtypes\">Sequence types.</a> 
Types for sequences.
</li><li><a href=\"/cgi-bin/cduce?example=integers\">The factorial function.</a> 
What about computing 10000! ?
</li><li><a href=\"/cgi-bin/cduce?example=sumtype\">Sum types.</a> 
How to simulate ML sum types.
</li><li><a href=\"/cgi-bin/cduce?example=ovfun\">Overloaded functions.</a> 
This examples demonstrates the use of overloaded functions.
</li><li><a href=\"/cgi-bin/cduce?example=note\">Footnotes.</a> 
 This example shows how to bind an XML element with surrounding text.
</li><li><a href=\"/cgi-bin/cduce?example=biblio\">Bibliography.</a> 
The good old XML bibliography example.
</li><li><a href=\"/cgi-bin/cduce?example=projection\">Projection.</a> 
Syntactic sugar for projection.
</li><li><a href=\"/cgi-bin/cduce?example=xtransform\">Tree transformations.</a> 
How to perform XSLT-like transformations.
</li><li><a href=\"/cgi-bin/cduce?example=reference\">References.</a> 
Mutable values.
</li><li><a href=\"/cgi-bin/cduce?example=pm_compil\">Compilation of pattern matching.</a> 
This example demonstrates the efficient compilation of pattern
matching.
</li></ul>"