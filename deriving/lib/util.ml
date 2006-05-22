let rec last : 'a list -> 'a = function
    | []    -> raise (Invalid_argument "last")
    | [x]   -> x
    | _::xs -> last xs

let rec rassoc (rkey : 'b) : ('a * 'b) list -> 'a = function
  | []                     -> raise Not_found
  | (a,b)::_ when b = rkey -> a
  | _::xs                  -> rassoc rkey xs

let bitsof int : string =
  let buf = String.make 31 '0' in
    for i = 0 to 30 do
      String.set buf (30-i) 
	(if int land (1 lsl i) <> 0 then '1'
	 else '0')
    done;
    buf

let ofbits string : int = 
  let rv = ref 0 
  and len = String.length string in
    for i = 0 to len -1 do
        match String.get string i with
          | '0' -> ()
          | '1' -> rv := !rv lor (1 lsl (len-i-1))
          | c   -> failwith (Printf.sprintf "unexpected character : %c" c)
    done;
    !rv

let ofbits64 string : int64 = 
  let rv = ref 0L 
  and len = String.length string in
    for i = 0 to len -1 do
        match String.get string i with
          | '0' -> ()
          | '1' -> rv := Int64.logor !rv (Int64.shift_left 1L (len-i-1))
          | c   -> failwith (Printf.sprintf "unexpected character : %c" c)
    done;
    !rv

let bitsof64 int : string =
  let buf = String.make 64 '0' in
    for i = 0 to 63 do
      String.set buf (63-i) 
	(if Int64.logand int (Int64.shift_left 1L i) <> 0L then '1'
	 else '0')
    done;
    buf
