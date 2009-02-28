(** Dump **)

(* TODO: we could have an additional debugging deserialisation method. *)

type 'a dump = {
  to_buffer : Buffer.t -> 'a -> unit ;
  from_stream : char Stream.t -> 'a 
}

exception Dump_error of string

let bad_tag tag stream typename =
  raise (Dump_error
           (Printf.sprintf 
              "Dump: failure during %s deserialisation at character %d; unexpected tag %d" 
              typename (Stream.count stream) tag))

let buffer_size = 128

let to_string dump obj = 
    let buffer = Buffer.create buffer_size in
      dump.to_buffer buffer obj;
      Buffer.contents buffer
        (* should we explicitly deallocate the buffer? *)

let to_buffer d = d.to_buffer
let from_string dump string = dump.from_stream (Stream.of_string string)
let from_channel dump in_channel = dump.from_stream (Stream.of_channel in_channel)
let to_channel dump out_channel obj = 
  let buffer = Buffer.create buffer_size in
    dump.to_buffer buffer obj;
    Buffer.output_buffer out_channel buffer
let from_stream d = d.from_stream

(* Generic int dumper.  This should work for any (fixed-size) integer
   type with suitable operations. *)
module Dump_intN (P : sig
                      type t
                      val zero : t
                      val logand : t -> t -> t
                      val logor : t -> t -> t
                      val lognot : t -> t
                      val shift_right_logical : t -> int -> t
                      val shift_left : t -> int -> t
                      val of_int : int -> t
                      val to_int : t -> int
                    end) = 
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
  
  let dump = 
    { to_buffer = (fun buffer ->
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
                     in aux);
      
      from_stream = (fun stream ->
                       let rec aux (int : t) shift = 
                         let c = of_int (code (Stream.next stream)) in
                         let int = logor int (shift_left (logand c (of_int 0x7f)) shift) in
                           if logand c (of_int 0x80) <> zero then aux int (shift + 7)
                           else int 
                       in aux zero 0)}
end

let dump_int32 = let module D = Dump_intN (Int32) in D.dump
let dump_int64 = let module D = Dump_intN (Int64) in D.dump
let dump_nativeint = let module D = Dump_intN (Nativeint) in D.dump
let dump_int = {
  to_buffer = (fun buffer int -> dump_nativeint.to_buffer buffer (Nativeint.of_int int));
  from_stream = (fun stream -> Nativeint.to_int (dump_nativeint.from_stream stream))
}

let dump_char = {
  to_buffer = Buffer.add_char;
  from_stream = Stream.next
}
 
(* This is questionable; it doesn't preserve sharing *)
let dump_string = {
  to_buffer = (fun buffer string ->
                 begin
                   dump_int.to_buffer buffer (String.length string);
                   Buffer.add_string buffer string
                 end);

  from_stream = (fun stream ->
                   let len = dump_int.from_stream stream in
                   let s = String.create len in
                     for i = 0 to len - 1 do
                       String.set s i (Stream.next stream) (* could use String.unsafe_set here *)
                     done;
                     s)
}

let dump_float = {
  to_buffer = (fun buffer f -> dump_int64.to_buffer buffer (Int64.bits_of_float f));
  from_stream = (fun stream -> Int64.float_of_bits (dump_int64.from_stream stream))
}

(* This should end up a bit more compact than the derived version *)
let dump_list p = {
  (* This could perhaps be more efficient by serialising the list in
     reverse: this would result in only one traversal being needed
     during serialisation, and no "reverse" being needed during
     deserialisation.  (However, dumping would no longer be
     tail-recursive) *)
  to_buffer = (fun buffer items ->
                 begin
                   dump_int.to_buffer buffer (List.length items);
                   List.iter (p.to_buffer buffer) items
                 end);

  from_stream = (fun stream ->
                   let rec aux items = function
                     | 0 -> items
                     | n -> aux (p.from_stream stream :: items) (n-1)
                   in List.rev (aux [] (dump_int.from_stream stream)))
}

(* dump_ref and dump_array cannot preserve sharing, so we don't
   provide implementations *)

let dump_option p = {
  to_buffer = (fun buffer -> function
                 | None   -> dump_int.to_buffer buffer 0
                 | Some s -> 
                     begin
                       dump_int.to_buffer buffer 1;
                       p.to_buffer buffer s
                     end);
  from_stream = (fun stream ->
                   match dump_int.from_stream stream with
                     | 0 -> None
                     | 1 -> Some (p.from_stream stream)
                     | i      -> bad_tag i stream "option")
}


let dump_bool = {
  to_buffer = (fun buffer -> function
                 | false -> Buffer.add_char buffer '\000'
                 | true  -> Buffer.add_char buffer '\001');
  from_stream = (fun stream ->
                   match Stream.next stream with
                     | '\000' -> false
                     | '\001' -> true
                     | c      -> bad_tag (Char.code c) stream "bool")
}

let dump_unit = {
  to_buffer = (fun _ () -> ());
  from_stream = (fun _ -> ())
}

let dump_num = {
  (* TODO: a less wasteful dumper for nums.  A good start would be
     using half a byte per decimal-coded digit, instead of a whole
     byte. *)
  to_buffer = (fun buffer n -> dump_string.to_buffer buffer (Num.string_of_num n));
  from_stream = (fun stream -> Num.num_of_string (dump_string.from_stream stream))
}

let dump_undumpable tname = {
  to_buffer = (fun _ _ -> failwith ("Dump: attempt to serialise a value of unserialisable type : " ^ tname));
  from_stream = (fun _ -> failwith ("Dump: attempt to deserialise a value of unserialisable type : " ^ tname))
}

(* Uses Marshal to serialise the values that the parse-the-declarations
   technique can't reach. *)
let dump_via_marshal = {
  (* Rather inefficient. *)
  to_buffer = (fun buffer obj -> Buffer.add_string buffer (Marshal.to_string obj [Marshal.Closures]));
  from_stream = (fun stream -> 
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
                     Marshal.from_string (header ^ datapart) 0)
}
