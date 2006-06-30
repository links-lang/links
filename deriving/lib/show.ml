(** Show **)
type showS = string -> string

module type Show = sig
  type a
    (* showsPrec seems unnecessary without infix constructors *)
    (*  val showsPrec : int -> a -> showS
        val show : a -> string
        val showList : a list -> showS
    *)
  val show : a -> string
  val showList : a list -> string
  val showBuf : a -> Buffer.t -> unit
  val showBufList : a list -> Buffer.t -> unit
end

module WriteBufDefault 
  (S : (sig
          type a
          val showBuf : a -> Buffer.t -> unit
        end)) =
struct
  include S
  let showBufList items buffer = 
    let rec writeItems = function
      | []     -> ()
      | [x]    -> S.showBuf x buffer
      | x::xs  -> S.showBuf x buffer; Buffer.add_char buffer ';'; writeItems xs
    in
      Buffer.add_char buffer '[';
      writeItems items;
      Buffer.add_char buffer ']'
end

module ShowDefaults' 
  (S : (sig
          type a
          val showBuf : a -> Buffer.t -> unit
          val showBufList : a list -> Buffer.t -> unit
        end)) : Show with type a = S.a =
struct
  include S
  let show item = 
    let b = Buffer.create 16 in 
      S.showBuf item b;
      Buffer.sub b 0 (Buffer.length b)
  let showList items = 
    let b = Buffer.create 16 in 
      S.showBufList items b;
      Buffer.sub b 0 (Buffer.length b)
end

module ShowDefaults (S : (sig
                            type a
                            val showBuf : a -> Buffer.t -> unit
                          end)) : Show with type a = S.a =
  ShowDefaults' (WriteBufDefault (S))


module Show_unprintable (S : sig type a end) (*: Show with type a = S.a *) = 
  ShowDefaults (struct
                  type a = S.a
                  let showBuf _ buffer = Buffer.add_string buffer "..."
                end)
    
(* instance Show a => Show [a] *)
module Show_list (S : Show) : Show with type a = S.a list = 
  ShowDefaults (struct
                  type a = S.a list
                  let showBuf = S.showBufList
                end)
    
(* instance Show a => Show (a ref) *)
module Show_ref (S : Show) : Show with type a = S.a ref =
  ShowDefaults (struct
                  type a = S.a ref
                  let showBuf obj buffer = 
                    Buffer.add_string buffer "ref ";
                    S.showBuf !obj buffer;
                end)

(* instance Show a => Show (a option) *)
module Show_option (S : Show) : Show with type a = S.a option =
  ShowDefaults (struct
                  type a = S.a option
                  let showBuf obj buffer = 
                    match obj with
                      | None ->
                          Buffer.add_string buffer "None";
                      | Some s -> 
                          Buffer.add_string buffer "Some ";
                          S.showBuf s buffer;
                end)

(* instance Show a => Show (a array) *)
module Show_array (S : Show) : Show with type a = S.a array =
  ShowDefaults (struct
                  type a = S.a array
                  let showBuf obj buffer = 
                    let length = Array.length obj in
                      Buffer.add_string buffer "[|";
                      for i = 0 to length - 2 do
                        S.showBuf (Array.get obj i) buffer;
                        Buffer.add_string buffer "; ";
                      done;
                      if length <> 0 then
                        S.showBuf (Array.get obj (length -1)) buffer;
                      Buffer.add_string buffer "|]"
                end)


module Show_0 : Show with type a = unit = 
  ShowDefaults (struct
                  type a = unit
                  let showBuf () buffer = Buffer.add_string buffer "()"
                end)

module Show_1 (S1 : Show) : Show with type a = S1.a =
  ShowDefaults (struct
                  type a = S1.a
                  let showBuf s1 b = 
                    S1.showBuf s1 b;
                end)


module Show_2 (S1 : Show) (S2 : Show) : Show with type a = S1.a * S2.a = 
  ShowDefaults (struct
                  type a = S1.a * S2.a
                  let showBuf (s1, s2) b = 
                    Buffer.add_char b '(';
                    S1.showBuf s1 b; Buffer.add_char b ',';
                    S2.showBuf s2 b; Buffer.add_char b ')';
                end)
module Show_3
  (S1 : Show)
  (S2 : Show)
  (S3 : Show)
  : Show with type a = S1.a * S2.a * S3.a =
  ShowDefaults (struct
                  type a = S1.a * S2.a * S3.a
                  let showBuf (s1, s2, s3) b = 
                    Buffer.add_char b '(';
                    S1.showBuf s1 b; Buffer.add_char b ',';
                    S2.showBuf s2 b; Buffer.add_char b ',';
                    S3.showBuf s3 b; Buffer.add_char b ')';
                end)
module Show_4
  (S1 : Show)
  (S2 : Show)
  (S3 : Show)
  (S4 : Show)
  : Show with type a = S1.a * S2.a * S3.a * S4.a =
  ShowDefaults (struct
                  type a = S1.a * S2.a * S3.a * S4.a
                  let showBuf (s1, s2, s3, s4) b = 
                    Buffer.add_char b '(';
                    S1.showBuf s1 b; Buffer.add_char b ',';
                    S2.showBuf s2 b; Buffer.add_char b ',';
                    S3.showBuf s3 b; Buffer.add_char b ',';
                    S4.showBuf s4 b; Buffer.add_char b ')';
                end)
module Show_5
  (S1 : Show)
  (S2 : Show)
  (S3 : Show)
  (S4 : Show)
  (S5 : Show)
  : Show with type a = S1.a * S2.a * S3.a * S4.a * S5.a =
  ShowDefaults (struct
                  type a = S1.a * S2.a * S3.a * S4.a * S5.a
                  let showBuf (s1, s2, s3, s4, s5) b = 
                    Buffer.add_char b '(';
                    S1.showBuf s1 b; Buffer.add_char b ',';
                    S2.showBuf s2 b; Buffer.add_char b ',';
                    S3.showBuf s3 b; Buffer.add_char b ',';
                    S4.showBuf s4 b; Buffer.add_char b ',';
                    S5.showBuf s5 b; Buffer.add_char b ')';
                end)
module Show_6
  (S1 : Show)
  (S2 : Show)
  (S3 : Show)
  (S4 : Show)
  (S5 : Show)
  (S6 : Show)
  : Show with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a =
  ShowDefaults (struct
                  type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a
                  let showBuf (s1, s2, s3, s4, s5, s6) b = 
                    Buffer.add_char b '(';
                    S1.showBuf s1 b; Buffer.add_char b ',';
                    S2.showBuf s2 b; Buffer.add_char b ',';
                    S3.showBuf s3 b; Buffer.add_char b ',';
                    S4.showBuf s4 b; Buffer.add_char b ',';
                    S5.showBuf s5 b; Buffer.add_char b ',';
                    S6.showBuf s6 b; Buffer.add_char b ')';
                end)
module Show_7
  (S1 : Show)
  (S2 : Show)
  (S3 : Show)
  (S4 : Show)
  (S5 : Show)
  (S6 : Show)
  (S7 : Show)
  : Show with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a =
  ShowDefaults (struct
                  type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a
                  let showBuf (s1, s2, s3, s4, s5, s6, s7) b = 
                    Buffer.add_char b '(';
                    S1.showBuf s1 b; Buffer.add_char b ',';
                    S2.showBuf s2 b; Buffer.add_char b ',';
                    S3.showBuf s3 b; Buffer.add_char b ',';
                    S4.showBuf s4 b; Buffer.add_char b ',';
                    S5.showBuf s5 b; Buffer.add_char b ',';
                    S6.showBuf s6 b; Buffer.add_char b ',';
                    S7.showBuf s7 b; Buffer.add_char b ')';
                end)
module Show_8
  (S1 : Show)
  (S2 : Show)
  (S3 : Show)
  (S4 : Show)
  (S5 : Show)
  (S6 : Show)
  (S7 : Show)
  (S8 : Show)
  : Show with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a * S8.a =
  ShowDefaults (struct
                  type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a * S8.a
                  let showBuf (s1, s2, s3, s4, s5, s6, s7, s8) b = 
                    Buffer.add_char b '(';
                    S1.showBuf s1 b; Buffer.add_char b ',';
                    S2.showBuf s2 b; Buffer.add_char b ',';
                    S3.showBuf s3 b; Buffer.add_char b ',';
                    S4.showBuf s4 b; Buffer.add_char b ',';
                    S5.showBuf s5 b; Buffer.add_char b ',';
                    S6.showBuf s6 b; Buffer.add_char b ',';
                    S7.showBuf s7 b; Buffer.add_char b ',';
                    S8.showBuf s8 b; Buffer.add_char b ')';
                end)
module Show_9
  (S1 : Show)
  (S2 : Show)
  (S3 : Show)
  (S4 : Show)
  (S5 : Show)
  (S6 : Show)
  (S7 : Show)
  (S8 : Show)
  (S9 : Show)
  : Show with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a * S8.a * S9.a =
  ShowDefaults (struct
                  type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a * S8.a * S9.a
                  let showBuf (s1, s2, s3, s4, s5, s6, s7, s8, s9) b = 
                    Buffer.add_char b '(';
                    S1.showBuf s1 b; Buffer.add_char b ',';
                    S2.showBuf s2 b; Buffer.add_char b ',';
                    S3.showBuf s3 b; Buffer.add_char b ',';
                    S4.showBuf s4 b; Buffer.add_char b ',';
                    S5.showBuf s5 b; Buffer.add_char b ',';
                    S6.showBuf s6 b; Buffer.add_char b ',';
                    S7.showBuf s7 b; Buffer.add_char b ',';
                    S8.showBuf s8 b; Buffer.add_char b ',';
                    S9.showBuf s9 b; Buffer.add_char b ')';
                end)
(*

type 'a fruit = 
  | Apple
  | Banana of 'a
  | Orange of ('a * int * ('a fruit))
*)


(*
  Doesn't work (unsafe module recursion)

  module rec Fix
  : functor (A : functor (S : Show)->Show) -> Show = functor (A : functor (S : Show) -> Show) -> 
  A (Fix (A))
*)

(* It'd be great if we could use `module rec ...' here, but O'Caml
   decides that it's not safe (because of the recursion, apparently) *)
(*
module Show_fruit (A : Show) : Show with type a = A.a fruit = 
  ShowDefaults (struct
                  type a = A.a fruit
                  let rec showBuf obj buffer = 
                    let module This = ShowDefaults (struct
                                                      type a = A.a fruit
                                                      let showBuf = showBuf
                                                    end) in
                      match obj with 
                        | Apple -> Buffer.add_string buffer "Apple "
                        | Banana (a) -> Buffer.add_string buffer "Banana ";
                            let module Shower = Show_1 (A) in
                              Shower.showBuf a buffer
                        | Orange (a,b,c) ->
                            Buffer.add_string buffer "Orange ";
                            let module Shower = Show_3 (A) (Show_int) (This) in
                              Shower.showBuf (a,b,c) buffer
                end)
*)
(*
showable
    <:ctyp< $t1$ . $t2$ >>: access in module. 
    <:ctyp< $t1$ as $t2$ >>: type alias. 
    <:ctyp< $t1$ $t2$ >>: application. 
    <:ctyp< $lid:s$ >>: identifier starting with a lowercase letter. 
    <:ctyp< ! $list:sl$ . $t$ >>: polymorphic type. 
    <:ctyp< '$s$ >>: type variable. 
    <:ctyp< { $list:sbtl$ } >>: record definition. 
    <:ctyp< [ $list:stll$ ] >>: concrete type definition. 
    <:ctyp< ( $list:tl$ ) >>: tuple. 
    <:ctyp< $uid:s$ >>: identifier starting with an uppercase letter. 
    <:ctyp< [| $list:rfl$ |] >>: variant type definition. 

not showable
    <:ctyp< _ >>: wildcard. 
    <:ctyp< $t1$ -> $t2$ >>: arrow. 
    <:ctyp< # $list:sl$ >>: class type. 
    <:ctyp< < $list:fl$ > >>: object type. 

unknown:
    <:ctyp< ~ $s$ : $t$ >>: label type. 
    <:ctyp< $t1$ == $t2$ >>: type manifest. 
    <:ctyp< ? $s$ : $t$ >>: optional label type. 
*)
