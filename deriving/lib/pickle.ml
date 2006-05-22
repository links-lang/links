(* Is it possible to have an interface that can include the
   structure-sharing picklers?

   NB: the structure-sharing pickler should detect cycles and fail.
*)

(** Pickle **)

(* TODO: we could have an additional debugging unpickling method. *)
module type Pickle = sig
  type a
  val pickle : Buffer.t -> a -> unit
  val unpickle : char Stream.t -> a
  val pickleS : a -> string
  val unpickleS : string -> a
end

module type SimplePickle = sig
  type a
  val pickle : Buffer.t -> a -> unit
  val unpickle : char Stream.t -> a
end

exception Unpickling_failure of string

let bad_tag tag stream typename =
  raise (Unpickling_failure
           (Printf.sprintf 
              "Failure during %s unpickling at character %d; unexpected tag %d" 
              typename (Stream.count stream) tag))

module Pickle_defaults (P : sig   
			  type a
			  val pickle : Buffer.t -> a -> unit
			  val unpickle : char Stream.t -> a
			end) : Pickle with type a = P.a = 
struct
  include P
  let pickleS obj = 
    let buffer = Buffer.create 128 (* is there a reasonable value to use here? *) in
      P.pickle buffer obj;
      Buffer.sub buffer 0 (Buffer.length buffer)
      (* should we explicitly deallocate the buffer? *)
  and unpickleS string = P.unpickle (Stream.of_string string)
end


(* Generic int pickler.  This should work for any (fixed-size) integer
   type with suitable operations. *)
module Pickle_intN (P : sig
                      type t
                      val zero : t
                      val logand : t -> t -> t
                      val logor : t -> t -> t
                      val lognot : t -> t
                      val shift_right_logical : t -> int -> t
                      val shift_left : t -> int -> t
                      val of_int : int -> t
                      val to_int : t -> int
                    end) : Pickle with type a = P.t = Pickle_defaults (
  struct
    type a = P.t
	(* Format an integer using the following scheme:
	   
	   The lower 7 bits of each byte are used to store successive 7-bit
	   chunks of the integer.
	   
	   The highest bit of each byte is used as a flag to indicate
	   whether the next byte is present.
	*)
    open Buffer
    open Char
    open P

    let pickle buffer =
      let rec aux int =
        (* are there more than 7 bits? *)
        if logand int (lognot (of_int 0x7f)) <> zero
        (* if there are, write the lowest 7 bite plus a high bit (to
           indicate that there's more).  Then recurse, shifting the value
           7 bits right *)
        then begin
          add_char buffer (chr (to_int (logor (of_int 0x80) (logand int (of_int 0x7f)))));
	  aux (shift_right_logical int 7)
        end
          (* otherwise, write the bottom 7 bits only *)
        else add_char buffer (chr (to_int int))
      in aux

    and unpickle stream = 
      let rec aux (int : t) shift = 
        let c = of_int (code (Stream.next stream)) in
        let int = logor int (shift_left (logand c (of_int 0x7f)) shift) in
          if logand c (of_int 0x80) <> zero then aux int (shift + 7)
          else int 
      in aux zero 0
  end
)

module Pickle_int32 = Pickle_intN (Int32)
module Pickle_int64 = Pickle_intN (Int64)
module Pickle_nativeint = Pickle_intN (Nativeint)
module Pickle_int : Pickle with type a = int = Pickle_defaults (
  struct
    type a = int
    let pickle buffer int = Pickle_nativeint.pickle buffer (Nativeint.of_int int)
    and unpickle stream = Nativeint.to_int (Pickle_nativeint.unpickle stream)
  end
)

module Pickle_char : Pickle with type a = char = Pickle_defaults (
  struct
    type a = char
    let pickle = Buffer.add_char
    and unpickle = Stream.next
  end
)

module Pickle_string : Pickle with type a = string = Pickle_defaults (
  struct
    type a = string
    let pickle buffer string = 
      begin
        Pickle_int.pickle buffer (String.length string);
        Buffer.add_string buffer string
      end
    and unpickle stream = 
      let len = Pickle_int.unpickle stream in
      let s = String.create len in
        for i = 0 to len - 1 do
          String.set s i (Stream.next stream) (* could use String.unsafe_set here *)
        done;
        s
  end
)

module Pickle_float : Pickle with type a = float = Pickle_defaults (
  struct
    type a = float
    let pickle buffer f = Pickle_int64.pickle buffer (Int64.bits_of_float f)
    and unpickle stream = Int64.float_of_bits (Pickle_int64.unpickle stream)
  end
)

module Pickle_list (P : SimplePickle) : Pickle with type a = P.a list = Pickle_defaults (
  (* This could perhaps be more efficient by pickling the list in
     reverse: this would result in only one traversal being needed
     during pickling, and no "reverse" being needed during unpickling.
     (However, pickling would no longer be tail-recursive) *)
  struct
    type a = P.a list
    let pickle buffer items = 
      begin
        Pickle_int.pickle buffer (List.length items);
        List.iter (P.pickle buffer) items
      end
    and unpickle stream = 
      let rec aux items = function
        | 0 -> items
        | n -> aux (P.unpickle stream :: items) (n-1)
      in List.rev (aux [] (Pickle_int.unpickle stream))
  end
)

module Pickle_ref (P : SimplePickle) : Pickle with type a = P.a ref = Pickle_defaults (
  struct
    type a = P.a ref
    let pickle buffer item = P.pickle buffer (!item)
    and unpickle stream = ref (P.unpickle stream)
  end
)

module Pickle_option (P : SimplePickle) : Pickle with type a = P.a option = Pickle_defaults (
  struct
    type a = P.a option
    let pickle buffer = function
      | None   -> Pickle_int.pickle buffer 0
      | Some s -> 
          begin
            Pickle_int.pickle buffer 1;
            P.pickle buffer s
          end
    and unpickle stream = 
      match Pickle_int.unpickle stream with
        | 0 -> None
        | 1 -> Some (P.unpickle stream)
        | i      -> bad_tag i stream "option"
  end
)

module Pickle_array (P : SimplePickle) : Pickle with type a = P.a array = Pickle_defaults (
  struct
    type a = P.a array
        (* rather inefficient *)
    let pickle buffer items = 
    let module PList = Pickle_list (P) in
      PList.pickle buffer (Array.to_list items)
    and unpickle stream = 
      let module PList = Pickle_list (P) in
        Array.of_list (PList.unpickle stream)
  end
)

module Pickle_bool : Pickle with type a = bool = Pickle_defaults (
  struct
    type a = bool
    let pickle buffer = function
      | false -> Buffer.add_char buffer '\000'
      | true  -> Buffer.add_char buffer '\001'
    and unpickle stream =
      match Stream.next stream with
        | '\000' -> false
        | '\001' -> true
        | c      -> bad_tag (Char.code c) stream "bool"
  end
)

module Pickle_unit : Pickle with type a = unit = Pickle_defaults (
  struct
    type a = unit
    let pickle _ () = ()
    and unpickle _ = ()
  end
)


module Pickle_0 = Pickle_unit
module Pickle_1
  (P1 : SimplePickle)
  : Pickle with type a = (P1.a) = Pickle_defaults (
    struct
      type a = (P1.a)
      let pickle buffer (p1) = 
        begin
          P1.pickle buffer p1;
        end
      and unpickle stream = 
        let p1 = P1.unpickle stream in
          (p1)
    end)
module Pickle_2
  (P1 : SimplePickle)
  (P2 : SimplePickle)
  : Pickle with type a = (P1.a * P2.a) = Pickle_defaults (
    struct
      type a = (P1.a * P2.a)
      let pickle buffer (p1, p2) = 
        begin
          P1.pickle buffer p1;
          P2.pickle buffer p2;
        end
      and unpickle stream = 
        let p1 = P1.unpickle stream in
        let p2 = P2.unpickle stream in
          (p1, p2)
    end)
module Pickle_3
  (P1 : SimplePickle)
  (P2 : SimplePickle)
  (P3 : SimplePickle)
  : Pickle with type a = (P1.a * P2.a * P3.a) = Pickle_defaults (
    struct
      type a = (P1.a * P2.a * P3.a)
      let pickle buffer (p1, p2, p3) = 
        begin
          P1.pickle buffer p1;
          P2.pickle buffer p2;
          P3.pickle buffer p3;
        end
      and unpickle stream = 
        let p1 = P1.unpickle stream in
        let p2 = P2.unpickle stream in
        let p3 = P3.unpickle stream in
          (p1, p2, p3)
    end)

module Pickle_4
  (P1 : SimplePickle)
  (P2 : SimplePickle)
  (P3 : SimplePickle)
  (P4 : SimplePickle)
  : Pickle with type a = (P1.a * P2.a * P3.a * P4.a) = Pickle_defaults (
    struct
      type a = (P1.a * P2.a * P3.a * P4.a)
      let pickle buffer (p1, p2, p3, p4) = 
        begin
          P1.pickle buffer p1;
          P2.pickle buffer p2;
          P3.pickle buffer p3;
          P4.pickle buffer p4;
        end
      and unpickle stream = 
        let p1 = P1.unpickle stream in
        let p2 = P2.unpickle stream in
        let p3 = P3.unpickle stream in
        let p4 = P4.unpickle stream in
          (p1, p2, p3, p4)
    end)
module Pickle_5
  (P1 : SimplePickle)
  (P2 : SimplePickle)
  (P3 : SimplePickle)
  (P4 : SimplePickle)
  (P5 : SimplePickle)
  : Pickle with type a = (P1.a * P2.a * P3.a * P4.a * P5.a) = Pickle_defaults (
    struct
      type a = (P1.a * P2.a * P3.a * P4.a * P5.a)
      let pickle buffer (p1, p2, p3, p4, p5) = 
        begin
          P1.pickle buffer p1;
          P2.pickle buffer p2;
          P3.pickle buffer p3;
          P4.pickle buffer p4;
          P5.pickle buffer p5;
        end
      and unpickle stream = 
        let p1 = P1.unpickle stream in
        let p2 = P2.unpickle stream in
        let p3 = P3.unpickle stream in
        let p4 = P4.unpickle stream in
        let p5 = P5.unpickle stream in
          (p1, p2, p3, p4, p5)
    end)

module Pickle_6
  (P1 : SimplePickle)
  (P2 : SimplePickle)
  (P3 : SimplePickle)
  (P4 : SimplePickle)
  (P5 : SimplePickle)
  (P6 : SimplePickle)
  : Pickle with type a = (P1.a * P2.a * P3.a * P4.a * P5.a * P6.a) = Pickle_defaults (
    struct
      type a = (P1.a * P2.a * P3.a * P4.a * P5.a * P6.a)
      let pickle buffer (p1, p2, p3, p4, p5, p6) = 
        begin
          P1.pickle buffer p1;
          P2.pickle buffer p2;
          P3.pickle buffer p3;
          P4.pickle buffer p4;
          P5.pickle buffer p5;
          P6.pickle buffer p6;
        end
      and unpickle stream = 
        let p1 = P1.unpickle stream in
        let p2 = P2.unpickle stream in
        let p3 = P3.unpickle stream in
        let p4 = P4.unpickle stream in
        let p5 = P5.unpickle stream in
        let p6 = P6.unpickle stream in
          (p1, p2, p3, p4, p5, p6)
    end)
module Pickle_7
  (P1 : SimplePickle)
  (P2 : SimplePickle)
  (P3 : SimplePickle)
  (P4 : SimplePickle)
  (P5 : SimplePickle)
  (P6 : SimplePickle)
  (P7 : SimplePickle)
  : Pickle with type a = (P1.a * P2.a * P3.a * P4.a * P5.a * P6.a * P7.a) = Pickle_defaults (
    struct
      type a = (P1.a * P2.a * P3.a * P4.a * P5.a * P6.a * P7.a)
      let pickle buffer (p1, p2, p3, p4, p5, p6, p7) = 
        begin
          P1.pickle buffer p1;
          P2.pickle buffer p2;
          P3.pickle buffer p3;
          P4.pickle buffer p4;
          P5.pickle buffer p5;
          P6.pickle buffer p6;
          P7.pickle buffer p7;
        end
      and unpickle stream = 
        let p1 = P1.unpickle stream in
        let p2 = P2.unpickle stream in
        let p3 = P3.unpickle stream in
        let p4 = P4.unpickle stream in
        let p5 = P5.unpickle stream in
        let p6 = P6.unpickle stream in
        let p7 = P7.unpickle stream in
          (p1, p2, p3, p4, p5, p6, p7)
    end)
module Pickle_8
  (P1 : SimplePickle)
  (P2 : SimplePickle)
  (P3 : SimplePickle)
  (P4 : SimplePickle)
  (P5 : SimplePickle)
  (P6 : SimplePickle)
  (P7 : SimplePickle)
  (P8 : SimplePickle)
  : Pickle  with type a = (P1.a * P2.a * P3.a * P4.a * P5.a * P6.a * P7.a * P8.a) = Pickle_defaults (
    struct
      type a = (P1.a * P2.a * P3.a * P4.a * P5.a * P6.a * P7.a * P8.a)
      let pickle buffer (p1, p2, p3, p4, p5, p6, p7, p8) = 
        begin
          P1.pickle buffer p1;
          P2.pickle buffer p2;
          P3.pickle buffer p3;
          P4.pickle buffer p4;
          P5.pickle buffer p5;
          P6.pickle buffer p6;
          P7.pickle buffer p7;
          P8.pickle buffer p8;
        end
      and unpickle stream = 
        let p1 = P1.unpickle stream in
        let p2 = P2.unpickle stream in
        let p3 = P3.unpickle stream in
        let p4 = P4.unpickle stream in
        let p5 = P5.unpickle stream in
        let p6 = P6.unpickle stream in
        let p7 = P7.unpickle stream in
        let p8 = P8.unpickle stream in
          (p1, p2, p3, p4, p5, p6, p7, p8)
    end)
module Pickle_9
  (P1 : SimplePickle)
  (P2 : SimplePickle)
  (P3 : SimplePickle)
  (P4 : SimplePickle)
  (P5 : SimplePickle)
  (P6 : SimplePickle)
  (P7 : SimplePickle)
  (P8 : SimplePickle)
  (P9 : SimplePickle) : Pickle 
  with type a = (P1.a * P2.a * P3.a * P4.a * P5.a * P6.a * P7.a * P8.a * P9.a) = Pickle_defaults (
    struct
      type a = (P1.a * P2.a * P3.a * P4.a * P5.a * P6.a * P7.a * P8.a * P9.a)
      let pickle buffer (p1, p2, p3, p4, p5, p6, p7, p8, p9) = 
        begin
          P1.pickle buffer p1;
          P2.pickle buffer p2;
          P3.pickle buffer p3;
          P4.pickle buffer p4;
          P5.pickle buffer p5;
          P6.pickle buffer p6;
          P7.pickle buffer p7;
          P8.pickle buffer p8;
          P9.pickle buffer p9;
        end
      and unpickle stream = 
        (* NB: Can't use either 

           (P1.unpickle stream, P2.unpickle stream, ... )

           or 

           let p1 = P1.unpickle stream
           and p2 = P2.unpickle stream
           ...

           because of unspecified evaluation order 
        *)
        let p1 = P1.unpickle stream in
        let p2 = P2.unpickle stream in
        let p3 = P3.unpickle stream in
        let p4 = P4.unpickle stream in
        let p5 = P5.unpickle stream in
        let p6 = P6.unpickle stream in
        let p7 = P7.unpickle stream in
        let p8 = P8.unpickle stream in
        let p9 = P9.unpickle stream in
          (p1, p2, p3, p4, p5, p6, p7, p8, p9)
    end)


module Pickle_num : Pickle with type a = Num.num = Pickle_defaults (
  struct
    (* TODO: a less wasteful pickler for nums.  A good start would be
       using half a byte per decimal-coded digit, instead of a whole
       byte. *)
    type a = Num.num
    let pickle buffer n = Pickle_string.pickle buffer (Num.string_of_num n)
    and unpickle stream = Num.num_of_string (Pickle_string.unpickle stream)
  end
)

module Pickle_unpicklable (P : sig type a end) : Pickle with type a = P.a = Pickle_defaults ( 
  struct 
    type a = P.a
    let pickle buffer _ = failwith "boom"
    let unpickle _ = failwith "boom"
  end
)



type 'a fruit = 
  | Apple
  | Banana of 'a
  | Orange of ('a * int * ('a fruit))

module Pickle_fruit (P1 : SimplePickle) : Pickle with type a = P1.a fruit =
Pickle_defaults(
  struct
    type a = P1.a fruit
    let rec pickle buffer = 
      let module This =
        struct
          type a = P1.a fruit
          let pickle = pickle
          and unpickle = unpickle
        end 
      in
        function
          | Apple    -> Pickle_int.pickle buffer 0
          | Banana v -> 
              begin
		Pickle_int.pickle buffer 1;
                let module S = Pickle_1(P1) in
                  S.pickle buffer v
              end
          | Orange v ->
              begin
		Pickle_int.pickle buffer 2;
                let module S = Pickle_3 (P1) (Pickle_int) (This) in
                  S.pickle buffer v
              end
    and unpickle stream = 
      let module This = 
        struct
          type a = P1.a fruit
          let pickle = pickle
          and unpickle = unpickle
        end
      in
      match Pickle_int.unpickle stream with
        | 0 -> Apple
        | 1 -> 
            let v1 = P1.unpickle stream in
              Banana (v1) 
        | 2 ->
            let v1 = P1.unpickle stream in
            let v2 = Pickle_int.unpickle stream in
            let v3 = This.unpickle stream in
              Orange (v1, v2, v3)
        | i -> bad_tag i stream "fruit"
  end
)

