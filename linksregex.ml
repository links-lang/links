open Regex

let unit = `Record []

module type Links_type = 
sig
  type a 
  val datatype : string
  val ofLinks : Result.result -> a
  val asLinks : a -> Result.result
end

(* These conversions (and conversions from OCaml values generally)
could be derived automatically using "deriving" *)
module Repeat : Links_type with type a = repeat = 
struct
  type a = repeat

  let datatype = "[| Star | Plus | Question |]"

  let asLinks : repeat -> Result.result = function
    | Star      -> `Variant ("Star", unit)
    | Plus      -> `Variant ("Plus", unit)
    | Question  -> `Variant ("Question", unit)
  and ofLinks : Result.result -> repeat = function
    | `Variant ("Star", _)     -> Star
    | `Variant ("Plus", _)     -> Plus
    | `Variant ("Question", _) -> Question 
    | r                       -> failwith ("Internal error: attempt to treat " 
                                           ^ Result.Show_result.show r ^ " as a repeat value")
end

module Regex : Links_type with type a = regex = 
struct
  type a = regex
  let datatype = "
      mu regex . (
      [|
         Range : (Char, Char) 
       | Simply : String
       | Any
       | Seq : [regex]
       | Repeat : ([| Star | Plus | Question |], regex)
      |]
      )
      "
  let rec asLinks : regex -> Result.result = function
    | Range (f,t)        -> `Variant ("Range", `Record [("1", `Char f);
                                                        ("2", `Char t)])
    | Simply s           -> `Variant ("Simply", Result.box_string s)
    | Any                -> `Variant ("Any", unit)
    | Seq rs             -> `Variant ("Seq", `List (List.map asLinks rs))
    | Repeat (repeat, r) -> `Variant ("Repeat", `Record [("1", Repeat.asLinks repeat);
                                                         ("2", asLinks r)])

  let rec ofLinks : Result.result -> regex = function 
    | `Variant ("Range", `Record ( [("1", `Char f); ("2", `Char t)]
                                 | [("2", `Char t); ("1", `Char f)]))
      -> Range (f,t)
    | `Variant ("Simply", s)     -> Simply (Result.unbox_string s)
    | `Variant ("Any", _)        -> Any
    | `Variant ("Seq", `List rs) -> Seq (List.map ofLinks rs)
    | `Variant ("Repeat", `Record ([("1", repeat); ("2", r)]
                                 | [("2", r); ("1", repeat)]))
      -> Repeat (Repeat.ofLinks repeat, ofLinks r)
    | r                          -> failwith ("Internal error: attempt to treat " 
                                              ^ Result.Show_result.show r ^ " as a regex value")
end
