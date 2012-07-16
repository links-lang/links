open Notfound

open Regex
open Utility
open Irquery

let record l = 
  `Record (StringMap.from_alist l)

let string s = 
  `Constant (`String s)
let get_string = function
  | `Constant (`String s) -> s
  | _ -> assert false

let char c = 
  `Constant (`Char c)

let unit = record []

module type Links_type = 
sig
  type a 
  val datatype : string
  val ofLinks : query -> a
  val ofLinksNGroups : query -> (a * int)
  val asLinks : a -> query
end

(* These conversions (and conversions from OCaml values generally)
could be derived automatically using "deriving" *)
module Repeat : Links_type with type a = repeat = 
struct
  type a = repeat

  let datatype = "[| Star | Plus | Question |]"

  let rec asLinks : repeat -> query = function
    | Star      -> `Variant ("Star", unit)
    | Plus      -> `Variant ("Plus", unit)
    | Question  -> `Variant ("Question", unit)
  and ofLinks : query -> repeat = function
    | `Variant ("Star", _)     -> Star
    | `Variant ("Plus", _)     -> Plus
    | `Variant ("Question", _) -> Question 
    | v                        -> failwith ("Internal error")
  and ofLinksNGroups r = ofLinks r, 0
end

module Regex : Links_type with type a = regex = 
struct
  type a = regex
  let datatype = "
      mu regex . (
      [|
         Range : (Char, Char) 
       | Simply : String
       | Quote : regex
       | Any
       | StartAnchor
       | EndAnchor
       | Seq : [regex]
       | Alternate : (regex, regex)
       | Group : regex
       | Repeat : ([| Star | Plus | Question |], regex)
       | Replace : (regex, String)
      |]
      )
      "
  let rec asLinks : regex -> query = function
    | Range (f,t)        -> `Variant ("Range", record [("1", char f);
                                                        ("2", char t)])
    | Simply s           -> `Variant ("Simply", string s)
    | Quote s           -> `Variant ("Quote", asLinks s)
    | Any                -> `Variant ("Any", unit)
    | StartAnchor       -> `Variant ("StartAnchor", unit)
    | EndAnchor         -> `Variant ("EndAnchor", unit)
    | Seq rs             -> `Variant ("Seq", `Concat (List.map asLinks rs))
    | Alternate (r1, r2)  -> `Variant ("Alternate", record [("1", asLinks r1);
                                                         ("2", asLinks r2)])
    | Group s             -> `Variant ("Group", asLinks s)
    | Repeat (repeat, r) -> `Variant ("Repeat", record [("1", Repeat.asLinks repeat);
                                                         ("2", asLinks r)])
    | Replace(re, tmpl) -> `Variant ("Replace", record [("1", asLinks re);
							  ("2", string tmpl)])

  let ofLinksNGroups res = 
    let rec ofLinksCount count : query -> (regex*int) = function 
      | `Variant ("Range", `Record r ) when StringMap.mem "1" r && StringMap.mem "2" r
				-> let get_char = function `Constant (`Char s) -> s | _ -> failwith "not a char" in
					Range (get_char (StringMap.find "1" r), get_char (StringMap.find "2" r) ), count
      | `Variant ("Simply", `Constant (`String s))     -> Simply s, count
      | `Variant ("Quote", s)     -> Quote (fst (ofLinksCount 0 s)), count
      | `Variant ("Any", _)        -> Any, count
      | `Variant ("StartAnchor", _)        -> StartAnchor, count
      | `Variant ("EndAnchor", _)        -> EndAnchor, count
      | `Variant ("Seq", `Concat rs) ->
	  let result = (List.map (ofLinksCount 0) rs) in
	  let regexes = List.map fst result in
	  let sum = List.fold_right ((+) -<- snd) result count in
	  Seq regexes, sum
      | `Variant ("Alternate", `Record r ) when StringMap.mem "1" r && StringMap.mem "2" r ->
	  let ((r1', c1), (r2', c2)) = 
		 (ofLinksCount 0 (StringMap.find "1" r), ofLinksCount 0 (StringMap.find "2" r))  in
	    Alternate(r1', r2'), count + c1 + c2
      | `Variant ("Group", s) -> 
	  let (s', count')  = ofLinksCount (count+1) s in
	    Group s', count'
      | `Variant ("Repeat", `Record r ) when StringMap.mem "1" r && StringMap.mem "2" r -> 
			 let (re, count)  = ofLinksCount count (StringMap.find "2" r) in
	     Repeat (Repeat.ofLinks (StringMap.find "1" r), re), count
      | `Variant ("Replace", `Record r ) when StringMap.mem "1" r && StringMap.mem "2" r ->
			 let re,tmpl = (StringMap.find "1" r),(StringMap.find "2" r) in
			 let (re, count) = ofLinksCount count re in
			 Replace(re, get_string tmpl), count
      | v  -> failwith ("Internal error") in
    ofLinksCount 0 res
      
      
  let ofLinks = fst -<- ofLinksNGroups 

end
