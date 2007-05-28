module List = 
struct
  include List

  let fold_left1 : ('a -> 'a -> 'a) -> 'a list -> 'a
    = fun f l -> match l with
      | x::xs  -> List.fold_left f x xs
      | []     -> invalid_arg "fold_left1"
          
  let rec fold_right1 : ('a -> 'a -> 'a) -> 'a list -> 'a
    = fun f l -> match l with
      | [x]   -> x
      | x::xs -> f x (fold_right1 f xs)
      | []    -> invalid_arg "fold_right1"

  let rec range from upto =
    let rec aux f t result = 
      if f = t then result
      else aux (f+1) t (f::result)
    in if upto < from then raise (Invalid_argument "range")
      else List.rev (aux from upto [])

end
