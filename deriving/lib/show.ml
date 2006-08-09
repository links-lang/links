(** Show **)
type showS = string -> string

module type Show = sig
  type a
  val format : Format.formatter -> a -> unit
  val formatList : Format.formatter -> a list -> unit
  val show : a -> string
  val showList : a list -> string
(*  val showBuf : a -> Buffer.t -> unit
  val showBufList : a list -> Buffer.t -> unit*)
end

(*
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
*)
module ShowFormatterDefault
  (S : (sig
          type a
          val format : Format.formatter -> a -> unit
        end)) =
struct
  include S
  let formatList formatter items = 
    let rec writeItems = function
      | []      -> ()
      | [x]     -> S.format formatter x;
      | x :: xs -> 
          begin
            S.format formatter x; 
            Format.pp_print_char formatter ';';
            Format.pp_print_break formatter 1 0;
            writeItems xs
          end
    in 
      Format.pp_open_hovbox formatter 1;
      Format.pp_print_char formatter '['; 
      writeItems items;
      Format.pp_print_char formatter ']';
      Format.pp_close_box formatter ()
end

module ShowDefaults' 
  (S : (sig
          type a
          val format : Format.formatter -> a -> unit
          val formatList : Format.formatter -> a list -> unit
        end)) : Show with type a = S.a =
struct
  include S
  let showFormatted f item =
    let b = Buffer.create 16 in 
    let formatter = Format.formatter_of_buffer b in
      Format.pp_open_hovbox formatter 0;
      f formatter item;
      Format.pp_close_box formatter ();
      Format.pp_print_flush formatter ();
      Buffer.sub b 0 (Buffer.length b)

  (* Warning: do not eta-reduce *)
  let show item = showFormatted S.format item
  (* Warning: do not eta-reduce *)
  let showList items = showFormatted S.formatList items

end

module ShowDefaults (S : (sig
                            type a
                            val format : Format.formatter -> a -> unit
                          end)) : Show with type a = S.a =
  ShowDefaults' (ShowFormatterDefault (S))


module Show_unprintable (S : sig type a end) (*: Show with type a = S.a *) = 
  ShowDefaults (struct
                  type a = S.a
                  let format formatter _ = Format.pp_print_string formatter "..."
                end)
    
(* instance Show a => Show [a] *)
module Show_list (S : Show) : Show with type a = S.a list = 
  ShowDefaults (struct
                  type a = S.a list
                  let format = S.formatList
                end)
    
(* instance Show a => Show (a ref) *)
module Show_ref (S : Show) : Show with type a = S.a ref =
  ShowDefaults (struct
                  type a = S.a ref
                  let format formatter obj = 
                    Format.pp_print_string formatter "ref ";
                    S.format formatter !obj;
                end)

(* instance Show a => Show (a option) *)
module Show_option (S : Show) : Show with type a = S.a option =
  ShowDefaults (struct
                  type a = S.a option
                  let format formatter obj = 
                    Format.pp_open_box formatter 0;
                    begin
                      match obj with
                        | None ->
                            Format.pp_print_string formatter "None";
                        | Some s -> 
                            Format.pp_print_string formatter "Some ";
                            S.format formatter s;
                    end;
                    Format.pp_close_box formatter ()
                end)


(* instance Show a => Show (a array) *)
module Show_array (S : Show) : Show with type a = S.a array =
  ShowDefaults (struct
                  type a = S.a array
                  let format formatter obj = 
                    let length = Array.length obj in
                      Format.pp_print_string formatter "[|";
                      for i = 0 to length - 2 do
                        S.format formatter (Array.get obj i);
                        Format.pp_print_string formatter "; ";
                      done;
                      if length <> 0 then
                        S.format formatter (Array.get obj (length -1));
                      Format.pp_print_string formatter "|]"
                end)

let comma formatter = 
  Format.pp_print_char formatter ',';
  Format.pp_print_break formatter 1 0;

module Show_0 : Show with type a = unit = 
  ShowDefaults (struct
                  type a = unit
                  let format formatter () = Format.pp_print_string formatter "()"
                end)
module Show_1 (S1 : Show) : Show with type a = S1.a =
  ShowDefaults (struct
                  type a = S1.a
                  let format formatter s1 = 
                    S1.format formatter s1;
                end)
module Show_2 (S1 : Show) (S2 : Show) : Show with type a = S1.a * S2.a = 
  ShowDefaults (struct
                  type a = S1.a * S2.a
                  let format formatter (s1, s2) = 
                    Format.pp_open_hovbox formatter 1;
                    Format.pp_print_char formatter '(';
                    S1.format formatter s1; comma formatter;
                    S2.format formatter s2; Format.pp_print_char formatter ')';
                    Format.pp_close_box formatter ()
                end)
module Show_3
  (S1 : Show)
  (S2 : Show)
  (S3 : Show)
  : Show with type a = S1.a * S2.a * S3.a =
  ShowDefaults (struct
                  type a = S1.a * S2.a * S3.a
                  let format formatter (s1, s2, s3) = 
                    Format.pp_open_hovbox formatter 1;
                    Format.pp_print_char formatter '(';
                    S1.format formatter s1; comma formatter;
                    S2.format formatter s2; comma formatter;
                    S3.format formatter s3; Format.pp_print_char formatter ')';
                    Format.pp_close_box formatter ()
                end)
module Show_4
  (S1 : Show)
  (S2 : Show)
  (S3 : Show)
  (S4 : Show)
  : Show with type a = S1.a * S2.a * S3.a * S4.a =
  ShowDefaults (struct
                  type a = S1.a * S2.a * S3.a * S4.a
                  let format formatter (s1, s2, s3, s4) = 
                    Format.pp_open_hovbox formatter 1;
                    Format.pp_print_char formatter '(';
                    S1.format formatter s1; comma formatter;
                    S2.format formatter s2; comma formatter;
                    S3.format formatter s3; comma formatter;
                    S4.format formatter s4; Format.pp_print_char formatter ')';
                    Format.pp_close_box formatter ()
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
                  let format formatter (s1, s2, s3, s4, s5) = 
                    Format.pp_open_hovbox formatter 1;
                    Format.pp_print_char formatter '(';
                    S1.format formatter s1; comma formatter;
                    S2.format formatter s2; comma formatter;
                    S3.format formatter s3; comma formatter;
                    S4.format formatter s4; comma formatter;
                    S5.format formatter s5; Format.pp_print_char formatter ')';
                    Format.pp_close_box formatter ()
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
                  let format formatter (s1, s2, s3, s4, s5, s6) = 
                    Format.pp_open_hovbox formatter 1;
                    Format.pp_print_char formatter '(';
                    S1.format formatter s1; comma formatter;
                    S2.format formatter s2; comma formatter;
                    S3.format formatter s3; comma formatter;
                    S4.format formatter s4; comma formatter;
                    S5.format formatter s5; comma formatter;
                    S6.format formatter s6; Format.pp_print_char formatter ')';
                    Format.pp_close_box formatter ()
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
                  let format formatter (s1, s2, s3, s4, s5, s6, s7) = 
                    Format.pp_open_hovbox formatter 1;
                    Format.pp_print_char formatter '(';
                    S1.format formatter s1; comma formatter;
                    S2.format formatter s2; comma formatter;
                    S3.format formatter s3; comma formatter;
                    S4.format formatter s4; comma formatter;
                    S5.format formatter s5; comma formatter;
                    S6.format formatter s6; comma formatter;
                    S7.format formatter s7; Format.pp_print_char formatter ')';
                    Format.pp_close_box formatter ()
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
                  let format formatter (s1, s2, s3, s4, s5, s6, s7, s8) = 
                    Format.pp_open_hovbox formatter 1;
                    Format.pp_print_char formatter '(';
                    S1.format formatter s1; comma formatter;
                    S2.format formatter s2; comma formatter;
                    S3.format formatter s3; comma formatter;
                    S4.format formatter s4; comma formatter;
                    S5.format formatter s5; comma formatter;
                    S6.format formatter s6; comma formatter;
                    S7.format formatter s7; comma formatter;
                    S8.format formatter s8; Format.pp_print_char formatter ')';
                    Format.pp_close_box formatter ()
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
                  let format formatter (s1, s2, s3, s4, s5, s6, s7, s8, s9) = 
                    Format.pp_open_hovbox formatter 1;
                    Format.pp_print_char formatter '(';
                    S1.format formatter s1; comma formatter;
                    S2.format formatter s2; comma formatter;
                    S3.format formatter s3; comma formatter;
                    S4.format formatter s4; comma formatter;
                    S5.format formatter s5; comma formatter;
                    S6.format formatter s6; comma formatter;
                    S7.format formatter s7; comma formatter;
                    S8.format formatter s8; comma formatter;
                    S9.format formatter s9; Format.pp_print_char formatter ')';
                    Format.pp_close_box formatter ()
                end)

