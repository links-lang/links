(*pp deriving *)
module Show = 
struct 
(** Show **)
type -'a show = {
  format : Format.formatter -> 'a -> unit ;
}

let show f item =
  let b = Buffer.create 16 in 
  let formatter = Format.formatter_of_buffer b in
    Format.fprintf formatter "@[<hov 0>%a@]@?" (f).format item;
    Buffer.sub b 0 (Buffer.length b)


let show_unprintable = 
  { format = fun formatter _ -> Format.pp_print_string formatter "..." }

(* instance Show a => Show (a list) *)
let show_list f =
  let {format = format} = f in
  let rec writeItems formatter = function
    | []      -> ()
    | [x]     -> format formatter x;
    | x :: xs -> Format.fprintf formatter "%a;@;%a" format x writeItems xs
  in 
    { format = fun formatter items -> Format.fprintf formatter "@[<hov 1>[%a]@]" writeItems items }
    
(* instance Show a => Show (a option) *)
let show_option (s : _ show) =
  let {format = format} = s in 
  { format = 
      fun formatter -> function
        | None   -> Format.fprintf formatter "@[None@]"
        | Some v -> Format.fprintf formatter "@[Some@;<1 2>%a@]" format v}

(* instance Show a => Show (a array) *)
let show_array (s : _ show) =
  let {format = format} = s in 
  {format =
      fun formatter obj ->
        let writeItems formatter items = 
          let length = Array.length items in
            for i = 0 to length - 2 do
              Format.fprintf formatter "@[%a;@;@]" format (Array.get items i)
            done;
            if length <> 0 then
              format formatter (Array.get items (length -1));
        in 
          Format.fprintf formatter "@[[|%a|]@]" writeItems obj}

module Show_map (O : Map.OrderedType) =
struct
  module M = Map.Make(O)
  let show_t show_k show_v =  {
    format = fun formatter map ->
      Format.pp_open_box formatter 0;
      Format.pp_print_string formatter "{";
      M.iter (fun key value -> 
                Format.pp_open_box formatter 0;
                (show_k).format formatter key;
                Format.pp_print_string formatter " => ";
                (show_v).format formatter value;
                Format.pp_close_box formatter ();
             ) map;
      Format.pp_print_string formatter "}, ";
      Format.pp_close_box formatter ();
  }
end

module Show_set (O : Set.OrderedType) =
struct
  module S = Set.Make(O)
  let show_t show_k =  {
    format = fun formatter set ->
      Format.pp_open_box formatter 0;
      Format.pp_print_string formatter "{";
      let remaining = ref (S.cardinal set) in
        begin
          S.iter (fun elt -> 
                    decr remaining ;
                    Format.pp_open_box formatter 0;
                    (show_k).format formatter elt;
                    if (!remaining <> 0) then
                      Format.pp_print_string formatter ", "
                    else ();
                    Format.pp_close_box formatter ();
                 ) set;
          Format.pp_print_string formatter "}";
          Format.pp_close_box formatter ()
        end
  }
end

let show_bool = 
  {format = fun formatter -> function
     | true  -> Format.pp_print_string formatter "true"
     | false -> Format.pp_print_string formatter "false"}

let show_from_string_of (to_string : 'a -> string) = 
  {format = fun formatter item -> Format.pp_print_string formatter (to_string item)}
 
let show_int32 = show_from_string_of Int32.to_string
let show_int64 = show_from_string_of Int64.to_string
let show_nativeint = show_from_string_of Nativeint.to_string
let show_int = show_from_string_of string_of_int
let show_num = show_from_string_of Num.string_of_num
let show_float = show_from_string_of string_of_float
let show_char =  
  {format = fun formatter item -> Format.pp_print_string formatter ("'" ^ Char.escaped item ^ "'")}

let show_string =  {format = fun formatter item ->
                     Format.pp_print_char formatter '"';
                     Format.pp_print_string formatter (String.escaped item);
                     Format.pp_print_char formatter '"'}
let show_unit =  {format = fun formatter () -> Format.pp_print_string formatter "()"}

end

include Show

type open_flag = Pervasives.open_flag  =
                 | Open_rdonly
                 | Open_wronly
                 | Open_append
                 | Open_creat
                 | Open_trunc
                 | Open_excl
                 | Open_binary
                 | Open_text
                 | Open_nonblock
                     deriving (Show)

type fpclass = Pervasives.fpclass =
               | FP_normal
               | FP_subnormal
               | FP_zero
               | FP_infinite
               | FP_nan
                   deriving (Show)

type 'a ref = 'a Pervasives.ref = { mutable contents : 'a; }
    deriving (Show)

let show_6 s1 s2 s3 s4 s5 s6 =
  {format =
      fun formatter (a1, a2, a3, a4, a5, a6) ->
        Format.fprintf formatter "@[<hov 1>(%a,@;%a,@;%a,@;%a,@;%a,@;%a)@]"
          s1.format a1
          s2.format a2
          s3.format a3
          s4.format a4
          s5.format a5
          s6.format a6}

let show_5 s1 s2 s3 s4 s5 =
  {format =
      fun formatter (a1, a2, a3, a4, a5) ->
        Format.fprintf formatter "@[<hov 1>(%a,@;%a,@;%a,@;%a,@;%a)@]"
          s1.format a1
          s2.format a2
          s3.format a3
          s4.format a4
          s5.format a5}

let show_4 s1 s2 s3 s4 =
  {format =
      fun formatter (a1, a2, a3, a4) ->
        Format.fprintf formatter "@[<hov 1>(%a,@;%a,@;%a,@;%a)@]"
          s1.format a1
          s2.format a2
          s3.format a3
          s4.format a4}

let show_3 s1 s2 s3 =
  {format =
      fun formatter (a1, a2, a3) ->
        Format.fprintf formatter "@[<hov 1>(%a,@;%a,@;%a)@]"
          s1.format a1
          s2.format a2
          s3.format a3}
    
let show_2 s1 s2 =
  {format =
      fun formatter (a1, a2) ->
        Format.fprintf formatter "@[<hov 1>(%a,@;%a)@]"
          s1.format a1
          s2.format a2}
