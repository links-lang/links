(* Christian Lindig's OCaml pretty-printer [1].
   Based on Phil Wadler's Haskell pretty-printer.

[1] Lindig, Christian. Strictly Pretty. Available at
   http://citeseer.ist.psu.edu/lindig00strictly.html. *)

let strlen  = String.length

let nl      = "\n"

type gmode =
    | GFlat             (* hgrp *)
    | GBreak            (* vgrp *)
    | GFill             (* fgrp *)
    | GAuto             (* agrp *)

type t =
    | DocNil
    | DocCons           of t * t
    | DocText           of string
    | DocNest           of int * t
    | DocBreak          of string
    | DocGroup          of gmode * t

let ($) x y             = DocCons(x,y)
let empty               = DocNil
let text s              = DocText(s)
let nest i x            = DocNest(i,x)
let break               = DocBreak(" ")
let break_null          = DocBreak ("")
let break_with s        = DocBreak(s)

let hgrp d              = DocGroup(GFlat, d)
let vgrp d              = DocGroup(GBreak,d)
let agrp d              = DocGroup(GAuto, d)
let fgrp d              = DocGroup(GFill, d)

type sdoc =
    | SNil
    | SText             of string * sdoc
    | SLine             of int    * sdoc    (* newline + spaces *)

let sdoc_to_string sdoc =
    let buf = Buffer.create 256 in
    let rec loop = function
        | SNil              -> ()
        | SText(s,d)        -> ( Buffer.add_string buf s
                               ; loop d
                               )
        | SLine(i,d)        -> let prefix = String.make i ' ' in
                               ( Buffer.add_char   buf '\n'
                               ; Buffer.add_string buf prefix
                               ; loop d
                               )
    in
        ( loop sdoc
        ; Buffer.contents buf
        )

let sdoc_to_file oc doc =
    let pstr = output_string oc in
    let rec loop = function
        | SNil          -> ()
        | SText(s,d)    -> pstr s; loop d
        | SLine(i,d)    -> let prefix = String.make i ' '
                           in  pstr nl;
                               pstr prefix;
                               loop d
    in
        loop doc

type mode =
    | Flat
    | Break
    | Fill

let rec fits w = function
    | _ when w < 0                   -> false
    | []                             -> true
    | (_,_,DocNil)              :: z -> fits w z
    | (i,m,DocCons(x,y))        :: z -> fits w ((i,m,x)::(i,m,y)::z)
    | (i,m,DocNest(j,x))        :: z -> fits w ((i+j,m,x)::z)
    | (_,_,DocText(s))          :: z -> fits (w - strlen s) z
    | (_,Flat, DocBreak(s))     :: z -> fits (w - strlen s) z
    | (_,Fill, DocBreak(_))     :: _ -> true
    | (_,Break,DocBreak(_))     :: _ -> true
    | (i,_,DocGroup(_,x))       :: z -> fits w ((i,Flat,x)::z)

(* format is cps to avoid stack overflow *)
let cons  s post z = post (SText (s, z))
let consl i post z = post (SLine (i, z))
let rec format w k l post = match l with
    | []                             -> post SNil
    | (_,_,DocNil)              :: z -> format w k z post
    | (i,m,DocCons(x,y))        :: z -> format w k ((i,m,x)::(i,m,y)::z) post
    | (i,m,DocNest(j,x))        :: z -> format w k ((i+j,m,x)::z) post
    | (_,_,DocText(s))          :: z -> format w (k + strlen s) z (cons s post)
    | (_,Flat, DocBreak(s))     :: z -> format w (k + strlen s) z (cons s post)
    | (i,Fill, DocBreak(s))     :: z -> let l = strlen s in
                                            if   fits (w - k - l) z
                                            then format w (k+l) z (cons s post)
                                            else format w  i    z (consl i post)
    | (i,Break,DocBreak(_))     :: z -> format w i z (consl i post)
    | (i,_,DocGroup(GFlat ,x))  :: z -> format w k ((i,Flat ,x)::z) post
    | (i,_,DocGroup(GFill ,x))  :: z -> format w k ((i,Fill ,x)::z) post
    | (i,_,DocGroup(GBreak,x))  :: z -> format w k ((i,Break,x)::z) post
    | (i,_,DocGroup(GAuto, x))  :: z -> if fits (w-k) ((i,Flat,x)::z)
                                        then format w k ((i,Flat ,x)::z) post
                                        else format w k ((i,Break,x)::z) post

let default_width = 80

let to_string ?(width=default_width) doc = format width 0 [0,Flat,agrp(doc)] sdoc_to_string
let to_file ?(width=default_width) oc doc = format width 0 [0,Flat,agrp(doc)] (sdoc_to_file oc)

let list ~sep ~f xs =
    let rec loop acc = function
        | []    -> acc
        | [x]   -> acc $ f x
        | x::xs -> loop (acc $ f x $ sep) xs
    in
    loop empty xs

let commalist ~f = list ~sep:(text "," $ break) ~f

let ($/) x y   = x $ break $ y
let ($//) x y = x $ break_null $ y

(* let block ?(indent=4) ~f xs =
 *   agrp (nest indent (text "{"
 *     $/
 *       begin
 *         list ~sep:(text ";" $ break) ~f xs
 *       end)
 *     $/ text "}")
 *
 * module Infix = struct
 *   let ($) = ($)
 *   and ($/) = ($/)
 *   and ($//) = ($//)
 * end *)

let vlist, alist, hlist =
  let group f l = f (List.fold_left (fun pp p -> pp $ break $ p) empty l) in
  group vgrp, group agrp, group hgrp
