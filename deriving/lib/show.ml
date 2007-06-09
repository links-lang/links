(** Show **)
module type Show = sig
  type a
  val format : Format.formatter -> a -> unit
  val formatList : Format.formatter -> a list -> unit
  val show : a -> string
  val showList : a list -> string
end

module type SimpleFormatter = 
sig
  type a
  val format : Format.formatter -> a -> unit
end

module ShowFormatterDefault (S : SimpleFormatter) =
struct
  include S
  let formatList formatter items = 
    let rec writeItems formatter = function
      | []      -> ()
      | [x]     -> S.format formatter x;
      | x :: xs -> Format.fprintf formatter "%a;@;%a" S.format x writeItems xs
    in 
      Format.fprintf formatter "@[<hov 1>[%a]@]" writeItems items
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
      Format.fprintf formatter "@[<hov 0>%a@]@?" f item;
      Buffer.sub b 0 (Buffer.length b)

  (* Warning: do not eta-reduce either of the following *)
  let show item = showFormatted S.format item
  let showList items = showFormatted S.formatList items
end

module ShowDefaults (S : SimpleFormatter) : Show with type a = S.a =
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
                    Format.fprintf formatter "@[ref@;%a]" S.format !obj
                end)

(* instance Show a => Show (a option) *)
module Show_option (S : Show) : Show with type a = S.a option =
  ShowDefaults (struct
                  type a = S.a option
                  let format formatter = function
                    | None   -> Format.fprintf formatter "@[None@]"
                    | Some s -> Format.fprintf formatter "@[Some@;<1 2>%a@]" S.format s
                end)

(* instance Show a => Show (a array) *)
module Show_array (S : Show) : Show with type a = S.a array =
  ShowDefaults (struct
                  type a = S.a array
                  let format formatter obj = 
                    let writeItems formatter items = 
                      let length = Array.length items in
                        for i = 0 to length - 2 do
                          Format.fprintf formatter "@[%a;@;@]" S.format (Array.get items i)
                        done;
                        if length <> 0 then
                          S.format formatter (Array.get items (length -1));
                    in 
                      Format.fprintf formatter "@[[|%a|]@]" writeItems obj
                end)

module Show_map
  (O : Map.OrderedType) 
  (K : Show with type a = O.t)
  (V : Show)
  : Show with type a = V.a Map.Make(O).t =
ShowDefaults(
  struct
    module M = Map.Make(O)
    type a = V.a M.t
    let format formatter map = 
      Format.pp_open_box formatter 0;
      Format.pp_print_string formatter "{";
      M.iter (fun key value -> 
                Format.pp_open_box formatter 0;
                K.format formatter key;
                Format.pp_print_string formatter " => ";
                V.format formatter value;
                Format.pp_close_box formatter ();
             ) map;
      Format.pp_print_string formatter "}";
      Format.pp_close_box formatter ();
      
  end)

module Show_set
  (O : Set.OrderedType) 
  (K : Show with type a = O.t)
  : Show with type a = Set.Make(O).t =
ShowDefaults(
  struct
    module S = Set.Make(O)
    type a = S.t
    let format formatter set = 
      Format.pp_open_box formatter 0;
      Format.pp_print_string formatter "{";
      S.iter (fun elt -> 
                Format.pp_open_box formatter 0;
                K.format formatter elt;
                Format.pp_close_box formatter ();
             ) set;
      Format.pp_print_string formatter "}";
      Format.pp_close_box formatter ();
  end)
