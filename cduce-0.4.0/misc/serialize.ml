(*
module Put = struct
  type t = { 
    buf : Buffer.t;
    mutable cur_byte : int; (* 0..255 *)
    mutable cur_bits : int; (* 0..7 *)
  }

  let pos t = t.cur_bits + 8 * Buffer.length t.buf

  type 'a f = t -> 'a -> unit

  type 'b property = (t * 'b) list ref

  let properties = ref []

  let get_property prop t = 
    match !prop with
      | (s,x) :: _ when t == s -> x
      | l ->
	  let x = List.assq t l in
	  prop := (t,x) :: (List.remove_assq t l);
	  x
	    (* Put in front of the list for quick access ... *)

  let mk_property init = 
    let prop = ref [] in
    properties := 
    ((fun t -> prop := (t, init t) :: !prop),
     (fun t -> prop := List.remove_assq t !prop)) :: !properties;
    prop

  let run f x =
    let t = { buf = Buffer.create 1024; cur_byte = 0; cur_bits = 0 } in
    List.iter (fun (f,_) -> f t) !properties;
    f t x;
    List.iter (fun (_,f) -> f t) !properties;
    if t.cur_bits > 0 then Buffer.add_char t.buf (Char.chr t.cur_byte); 
    Buffer.contents t.buf

  let bool t b =
    if b then t.cur_byte <- t.cur_byte lor (1 lsl t.cur_bits);
    if t.cur_bits = 7 then (
      Buffer.add_char t.buf (Char.chr t.cur_byte);
      t.cur_byte <- 0;
      t.cur_bits <- 0
    ) else
      t.cur_bits <- succ t.cur_bits
    
  let rec bits nb t i = (* TODO: opt *)
    if (nb > 0) then (bool t ((i land 1) <> 0); bits (pred nb) t (i lsr 1))

  let rec int t i =
    bits 4 t i;
    let i = i lsr 4 in
    if i <> 0 then (bool t true; int t i) else (bool t false)

  let substring t s pos len =
    int t len;
    for i = pos to pos + len - 1 do
      bits 8 t (Char.code (s.[i]))
    done

  let string t s =
    substring t s 0 (String.length s)

  let magic t s =
    for i = 0 to String.length s - 1 do
      bits 8 t (Char.code (s.[i]))
    done

  let rec list f t = function
    | [] -> bool t false
    | hd::tl -> bool t true; f t hd; list f t tl

  let array f t a =
    int t (Array.length a);
    for i = 0 to Array.length a - 1 do
      f t a.(i)
    done

  let pair f1 f2 t (x,y) = f1 t x; f2 t y

  let env f1 f2 it t arg =
    it (fun x y -> bool t true; f1 t x; f2 t y) arg;
    bool t false

end


module Get = struct
  type t = { buf : string; mutable idx : int; mutable idx_bits : int }

  let pos t = t.idx_bits + 8 * t.idx

  type 'a f = t -> 'a

  type 'b property = (t * 'b) list ref

  let properties = ref []

  let get_property prop t = 
    match !prop with
      | (s,x) :: _ when t == s -> x
      | l ->
	  let x = List.assq t l in
	  prop := (t,x) :: (List.remove_assq t l);
	  x
	    (* Put in front of the list for quick access ... *)

  let mk_property init = 
    let prop = ref [] in
    properties := 
    ((fun t -> prop := (t, init t) :: !prop),
     (fun t -> prop := List.remove_assq t !prop)) :: !properties;
    prop



  let run f s =
    let t = { buf = s; idx = 0; idx_bits = 0 } in
    List.iter (fun (f,_) -> f t) !properties;
    let res = f t in
    List.iter (fun (_,f) -> f t) !properties;
    res

  let bool t =
    let b = ((Char.code t.buf.[t.idx]) lsr t.idx_bits) land 1 <> 0 in
    if t.idx_bits = 7 then (t.idx_bits <- 0; t.idx <- succ t.idx)
    else t.idx_bits <- succ t.idx_bits;
    b
    
  let rec bits nb t =
    if nb = 0 then 0 
    else if bool t 
    then succ (bits (pred nb) t lsl 1)
    else bits (pred nb) t lsl 1

  let rec int t =
    let i = bits 4 t in
    if bool t then i + (int t) lsl 4 
    else i

  let string t =
    let l = int t in
    let s = String.create l in
    for i = 0 to l - 1 do
      s.[i] <- Char.chr (bits 8 t)
    done;
    s

  let magic t s =
    for i = 0 to String.length s - 1 do
      let c = bits 8 t in
      if (Char.code (s.[i]) != c) then failwith "Invalid magic code."
    done

  let rec list f t =
    if bool t then let hd = f t in hd::(list f t)
    else []

  let array f t =
    let n = int t in
    if n = 0 then [| |]
    else
      let a = Array.create n (f t) in
      for i = 1 to Array.length a - 1 do
	a.(i) <- f t
      done;
      a

  let pair f1 f2 t =
    let x = f1 t in
    let y = f2 t in
    (x,y)

  let rec env f1 f2 add init t =
    if bool t then
      let x = f1 t in
      let y = f2 t in
      env f1 f2 add (add x y init) t
    else 
      init
end
*)
