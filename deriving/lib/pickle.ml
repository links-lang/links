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

module Defaults (P : sig   
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
                    end) = Defaults (
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
module Pickle_int = Defaults (
  struct
    type a = int
    let pickle buffer int = Pickle_nativeint.pickle buffer (Nativeint.of_int int)
    and unpickle stream = Nativeint.to_int (Pickle_nativeint.unpickle stream)
  end
)

module Pickle_char = Defaults (
  struct
    type a = char
    let pickle = Buffer.add_char
    and unpickle = Stream.next
  end
)

(* This is questionable; it doesn't preserve sharing *)
module Pickle_string = Defaults (
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

module Pickle_float = Defaults (
  struct
    type a = float
    let pickle buffer f = Pickle_int64.pickle buffer (Int64.bits_of_float f)
    and unpickle stream = Int64.float_of_bits (Pickle_int64.unpickle stream)
  end
)

(* This should end up a bit more compact than the derived version *)
module Pickle_list (P : SimplePickle) = Defaults (
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

(* Pickle_ref cannot preserve sharing, so we don't provide an implementation *)

module Pickle_option (P : SimplePickle) = Defaults (
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

(* This doesn't preserve sharing, so it shouldn't be allowed *)
module Pickle_array (P : SimplePickle) = Defaults (
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

module Pickle_bool = Defaults (
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

module Pickle_unit = Defaults (
  struct
    type a = unit
    let pickle _ () = ()
    and unpickle _ = ()
  end
)

module Pickle_num = Defaults (
  struct
    (* TODO: a less wasteful pickler for nums.  A good start would be
       using half a byte per decimal-coded digit, instead of a whole
       byte. *)
    type a = Num.num
    let pickle buffer n = Pickle_string.pickle buffer (Num.string_of_num n)
    and unpickle stream = Num.num_of_string (Pickle_string.unpickle stream)
  end
)

module Pickle_unpicklable (P : sig type a val tname : string end) = Defaults ( 
  struct 
    type a = P.a
    let pickle _ _ = failwith ("attempt to pickle a value of unpicklable type : " ^ P.tname)
    let unpickle _ = failwith ("attempt to unpickle a value of unpicklable type : " ^ P.tname)
  end
)

(* Uses Marshal to pickle the values that the parse-the-declarations
   technique can't reach. *)
module Pickle_via_marshal (P : sig type a end) = Defaults (
(* Rather inefficient. *)
  struct
    include P
    let pickle buffer obj = Buffer.add_string buffer (Marshal.to_string obj [Marshal.Closures])
    let unpickle stream = 
      let readn n = 
        let s = String.create n in
          for i = 0 to n - 1 do
            String.set s i (Stream.next stream)
          done;
          s
      in
      let header = readn Marshal.header_size in
      let datasize = Marshal.data_size header 0 in
      let datapart = readn datasize in
        Marshal.from_string (header ^ datapart) 0
  end)
