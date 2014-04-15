(* Christian Lindig's OCaml pretty-printer [1].
   Based on Phil Wadler's Haskell pretty-printer [2].

[1] Lindig, Christian. Strictly Pretty. Available at 
    http://citeseer.ist.psu.edu/lindig00strictly.hml.

[2] 
*)

let strlen = String.length
let nl = "\n"

type doc =
  | DocNil
  | DocCons of doc * doc
  | DocText of string
  | DocNest of int * doc
  | DocBreak of string
  | DocGroup of doc

let (^^) x y = DocCons(x,y)
let empty = DocNil
let text s = DocText(s)
let nest i x = DocNest(i,x)
let break = DocBreak(" ")
let breakWith s = DocBreak(s)
let group d = DocGroup(d)

type sdoc =
  | SNil
  | SText of string * sdoc
  | SLine of int * sdoc (* newline + spaces *)

let rec sdocToString buf = function
  | SNil -> ()
  | SText(s,d) -> Buffer.add_string buf s; sdocToString buf d
  | SLine(i,d) ->
      let prefix = String.make i ' ' in
        Buffer.add_string buf nl;
        Buffer.add_string buf prefix;
        sdocToString buf d

let sdocToString d =
  let buf = Buffer.create 16 in
    sdocToString buf d;
    Buffer.contents buf

type mode =
  | Flat
  | Break

let rec fits w = function
  | _ when w < 0 -> false
  | [] -> true
  | (_,_,DocNil) :: z -> fits w z
  | (i,m,DocCons(x,y)) :: z -> fits w ((i,m,x)::(i,m,y)::z)
  | (i,m,DocNest(j,x)) :: z -> fits w ((i+j,m,x)::z)
  | (_,_,DocText s) :: z -> fits (w - strlen s) z
  | (_,Flat, DocBreak s) :: z -> fits (w - strlen s) z
  | (_,Break,DocBreak _) :: _ -> true
  | (i,_,DocGroup x) :: z -> fits w ((i,Flat,x)::z)

let rec format w k = function
  | [] -> SNil
  | (_,_,DocNil) :: z -> format w k z
  | (i,m,DocCons(x,y)) :: z -> format w k ((i,m,x)::(i,m,y)::z)
  | (i,m,DocNest(j,x)) :: z -> format w k ((i+j,m,x)::z)
  | (_,_,DocText s) :: z -> SText(s,format w (k + strlen s) z)
  | (_,Flat, DocBreak s) :: z -> SText(s,format w (k + strlen s) z)
  | (i,Break,DocBreak _) :: z -> SLine(i,format w i z)
  | (i,_,DocGroup x) :: z -> if fits (w-k) ((i,Flat,x)::z)
                              then format w k ((i,Flat ,x)::z)
                              else format w k ((i,Break,x)::z)

let (^|) x y = match x,y with
  | DocNil, _ -> y
  | _, DocNil -> x
  | _, _ -> x ^^ break ^^ y

let (^+^) x y = match x, y with
  | DocNil, _ -> y
  | _, DocNil -> x
  | _, _ -> x ^^ text " " ^^ y

(* let ($$) x y = x ^^ break ^^ y *)

let rec unsnoc = function
  | [] -> invalid_arg "unsnoc"
  | [x] -> ([], x)
  | x::xs -> let (ys, y) = unsnoc xs in
      (x::ys, y)

let punctuate punc = 
  let punc = text punc in
  function
    [] -> []
  | xs -> let (xs, x) = unsnoc xs in
      (List.map (fun x -> x ^^ punc) xs) @ [x]

let doc_concat sep l =
  match l with 
      [] -> empty
    | (h::t) -> h ^^ List.fold_right (fun d a -> sep ^^ d ^^ a) t empty

let doc_join f l = doc_concat break (List.map f l)


let vsep xs = List.fold_right (^|) xs DocNil

let hsep xs = List.fold_right (^+^) xs DocNil

let binop left op right = group (nest 2
                                   ( group (left ^| text op)
                                     ^| right
                                   )
                                )
let trinop left op1 middle op2 right =
  group (nest 2
           ( left ^| group (nest 2 (text op1 ^+^ middle))
               ^| group (nest 2 (text op2 ^+^ right))
           )
        )

let parens doc = 
  text "(" ^^ group doc ^^ text ")"

let braces doc = 
  text "{" ^^ group doc ^^ text "}"

let brackets doc = 
  text "[" ^^ group doc ^^ text "]"

let arglist xs =
  parens (hsep (punctuate "," xs))

let formal_list xs =
  parens (hsep (punctuate "," (List.map text xs)))

let cond = binop (text "a") "==" (text "b")
let expr1 = binop (text "a") "<<" (text "2")
let expr2 = binop (text "a") "+" (text "b")
let ifthen c e1 e2 = group ( group (nest 2 (text "if" ^| c ))
                             ^| group (nest 2 (text "then" ^| e1))
                               ^| group (nest 2 (text "else" ^| e2))
                           )

let doc = ifthen cond expr1 expr2

let pretty w doc =
  let sdoc = format w 0 [0,Flat, DocGroup doc] in
  let str = sdocToString sdoc in
    str
