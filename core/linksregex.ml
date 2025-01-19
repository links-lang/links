open Regex
open Utility

let internal_error message =
  Errors.internal_error ~filename:"linksregex.ml" ~message

let unit = `Record []

module type Links_type =
sig
  type a
  val datatype : string
  val ofLinks : Value.t -> a
  val ofLinksNGroups : Value.t -> (a * int)
  val asLinks : a -> Value.t
end

(* These conversions (and conversions from OCaml values generally)
could be derived automatically using "deriving" *)
module Repeat : Links_type with type a = repeat =
struct
  type a = repeat

  let datatype = "[| Star | Plus | Question |]"

  let rec asLinks : repeat -> Value.t = function
    | Star      -> `Variant ("Star", unit)
    | Plus      -> `Variant ("Plus", unit)
    | Question  -> `Variant ("Question", unit)
  and ofLinks : Value.t -> repeat = function
    | `Variant ("Star", _)     -> Star
    | `Variant ("Plus", _)     -> Plus
    | `Variant ("Question", _) -> Question
    | v                        ->
        raise (internal_error ("Attempt to treat " ^ Value.show v ^ " as a repeat value"))
  and ofLinksNGroups r = ofLinks r, 0
end

module Regex : Links_type with type a = regex =
struct
  type a = regex
  let datatype = "                                      \
      mu regex . (                                      \
      [|                                                \
         Range : (Char, Char)                           \
       | Simply : String                                \
       | Quote : regex                                  \
       | Any                                            \
       | StartAnchor                                    \
       | EndAnchor                                      \
       | Seq : [regex]                                  \
       | Alternate : (regex, regex)                     \
       | Group : regex                                  \
       | Repeat : ([| Star | Plus | Question |], regex) \
       | Replace : (regex, String)                      \
      |]                                                \
      )                                                 \
      "
  let rec asLinks : regex -> Value.t = function
    | Range (f,t)        -> `Variant ("Range", `Record [("1", `Char f);
                                                        ("2", `Char t)])
    | Simply s           -> `Variant ("Simply", Value.box_string s)
    | Quote s           -> `Variant ("Quote", asLinks s)
    | Any                -> `Variant ("Any", unit)
    | StartAnchor       -> `Variant ("StartAnchor", unit)
    | EndAnchor         -> `Variant ("EndAnchor", unit)
    | Seq rs             -> `Variant ("Seq", `List (List.map asLinks rs))
    | Alternate (r1, r2)  -> `Variant ("Alternate", `Record [("1", asLinks r1);
                                                         ("2", asLinks r2)])
    | Group s             -> `Variant ("Group", asLinks s)
    | Repeat (repeat, r) -> `Variant ("Repeat", `Record [("1", Repeat.asLinks repeat);
                                                         ("2", asLinks r)])
    | Replace(re, tmpl) -> `Variant ("Replace", `Record [("1", asLinks re);
                              ("2", Value.box_string tmpl)])

  let ofLinksNGroups res =
    let rec ofLinksCount count : Value.t -> (regex*int) = function
      | `Variant ("Range", `Record ( [("1", `Char f); ("2", `Char t)]
      | [("2", `Char t); ("1", `Char f)]))
    -> (Range (f,t), count)
      | `Variant ("Simply", s)     -> Simply (Value.unbox_string s), count
      | `Variant ("Quote", s)     -> Quote (fst (ofLinksCount 0 s)), count
      | `Variant ("Any", _)        -> Any, count
      | `Variant ("StartAnchor", _)        -> StartAnchor, count
      | `Variant ("EndAnchor", _)        -> EndAnchor, count
      | `Variant ("Seq", `List rs) ->
      let result = (List.map (ofLinksCount 0) rs) in
      let regexes = List.map fst result in
      let sum = List.fold_right ((+) -<- snd) result count in
      Seq regexes, sum
      | `Variant ("Alternate", `Record([("1", r1); ("2", r2)])) ->
      let ((r1', c1), (r2', c2)) = (ofLinksCount 0 r1, ofLinksCount 0 r2)  in
        Alternate(r1', r2'), count + c1 + c2
      | `Variant ("Group", s) ->
      let (s', count')  = ofLinksCount (count+1) s in
        Group s', count'
      | `Variant ("Repeat", `Record ([("1", repeat); ("2", r)]
      | [("2", r); ("1", repeat)]))
    ->
      let (re, count)  = ofLinksCount count r in
         Repeat (Repeat.ofLinks repeat, re), count
      | `Variant ("Replace", `Record ([("1", re); ("2", tmpl)]))
    ->
      let (re, count) = ofLinksCount count re in
      Replace(re, Value.unbox_string tmpl), count
      | v  ->
          raise (internal_error ("Attempt to treat " ^
            Value.show v ^ " as a regex value")) in
    ofLinksCount 0 res


  let ofLinks = fst -<- ofLinksNGroups

end
