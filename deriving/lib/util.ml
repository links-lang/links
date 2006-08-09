let rec last : 'a list -> 'a = function
    | []    -> raise (Invalid_argument "last")
    | [x]   -> x
    | _::xs -> last xs

let rec rassoc (rkey : 'b) : ('a * 'b) list -> 'a = function
  | []                     -> raise Not_found
  | (a,b)::_ when b = rkey -> a
  | _::xs                  -> rassoc rkey xs
