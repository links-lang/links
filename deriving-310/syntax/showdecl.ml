module Show =
struct
  module Show = 
  struct 
  (** Show **)
  module type Show = sig
    type a
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
  
  module type SimpleFormatter = 
  sig
    type a
    val format : Format.formatter -> a -> unit
  end
  
  module ShowFormatterDefault (S : SimpleFormatter) =
  struct
    include S
    let format_list formatter items = 
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
            val format_list : Format.formatter -> a list -> unit
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
    let show_list items = showFormatted S.format_list items
  end
  
  module Defaults (S : SimpleFormatter) : Show with type a = S.a =
    ShowDefaults' (ShowFormatterDefault (S))
  
  module Show_unprintable (S : sig type a end) (*: Show with type a = S.a *) = 
    Defaults (struct
                type a = S.a
                let format formatter _ = Format.pp_print_string formatter "..."
              end)
      
  (* instance Show a => Show [a] *)
  module Show_list (S : Show) : Show with type a = S.a list = 
    Defaults (struct
                type a = S.a list
                let format = S.format_list
              end)
      
  (* instance Show a => Show (a option) *)
  module Show_option (S : Show) : Show with type a = S.a option =
    Defaults (struct
                type a = S.a option
                let format formatter = function
                  | None   -> Format.fprintf formatter "@[None@]"
                  | Some s -> Format.fprintf formatter "@[Some@;<1 2>%a@]" S.format s
              end)
      
  (* instance Show a => Show (a array) *)
  module Show_array (S : Show) : Show with type a = S.a array =
    Defaults (struct
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
  Defaults(
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
  Defaults(
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
  
  module Show_bool = Defaults (struct
    type a = bool
    let format formatter item =
      match item with
        | true  -> Format.pp_print_string formatter "true"
        | false -> Format.pp_print_string formatter "false"
  end) 
  
  module Show_integer (S : sig type t val to_string : t -> string end) = Defaults (struct
    type a = S.t
    let format formatter item = Format.pp_print_string formatter (S.to_string item)
  end)
   
  module Show_int32 = Show_integer(Int32)
  module Show_int64 = Show_integer(Int64)
  module Show_nativeint = Show_integer(Nativeint)
  
  module Show_char = Defaults (struct
    type a = char
    let format formatter item = Format.pp_print_string formatter ("'" ^ Char.escaped item ^ "'")
  end)
  
  module Show_int = Defaults (struct
    type a = int
    let format formatter item = Format.pp_print_string formatter (string_of_int item)
  end)
  
  module Show_float = Defaults(struct
      type a = float
      let format formatter item = Format.pp_print_string formatter (string_of_float item)
  end)
  
  module Show_string = Defaults (struct
    type a = string
    let format formatter item = 
      Format.pp_print_char formatter '"';
      Format.pp_print_string formatter (String.escaped item);
      Format.pp_print_char formatter '"'
  end)  
  
  module Show_unit = Defaults(struct
    type a = unit
    let format formatter () = Format.pp_print_string formatter "()"
  end)
  
  end
  include Show
  
  module Show_6 (A1 : Show) (A2 : Show) (A3 : Show) (A4 : Show) (A5 : Show) (A6 : Show)
    : Show with type a = A1.a * A2.a * A3.a * A4.a * A5.a * A6.a = Defaults (
  struct
    type a = A1.a * A2.a * A3.a * A4.a * A5.a * A6.a
    let format formatter (a1, a2, a3, a4, a5, a6) =
      Format.fprintf formatter "@[<hov 1>(%a,@;%a,@;%a,@;%a,@;%a,@;%a)@]"
        A1.format a1
        A2.format a2
        A3.format a3
        A4.format a4
        A5.format a5
        A6.format a6
  end)
  
  module Show_5 (A1 : Show) (A2 : Show) (A3 : Show) (A4 : Show) (A5 : Show)
    : Show with type a = A1.a * A2.a * A3.a * A4.a * A5.a = Defaults (
  struct
    type a = A1.a * A2.a * A3.a * A4.a * A5.a
    let format formatter (a1, a2, a3, a4, a5) =
      Format.fprintf formatter "@[<hov 1>(%a,@;%a,@;%a,@;%a,@;%a)@]"
        A1.format a1
        A2.format a2
        A3.format a3
        A4.format a4
        A5.format a5
  end)
  
  module Show_4 (A1 : Show) (A2 : Show) (A3 : Show) (A4 : Show)
    : Show with type a = A1.a * A2.a * A3.a * A4.a = Defaults (
  struct
    type a = A1.a * A2.a * A3.a * A4.a
    let format formatter (a1, a2, a3, a4) =
      Format.fprintf formatter "@[<hov 1>(%a,@;%a,@;%a,@;%a)@]"
        A1.format a1
        A2.format a2
        A3.format a3
        A4.format a4
  end)
  
  module Show_3 (A1 : Show) (A2 : Show) (A3 : Show)
    : Show with type a = A1.a * A2.a * A3.a = Defaults (
  struct
    type a = A1.a * A2.a * A3.a
    let format formatter (a1, a2, a3) =
      Format.fprintf formatter "@[<hov 1>(%a,@;%a,@;%a)@]"
        A1.format a1
        A2.format a2
        A3.format a3
  end)
  
  module Show_2 (A1 : Show) (A2 : Show)
    : Show with type a = A1.a * A2.a = Defaults (
  struct
    type a = A1.a * A2.a
    let format formatter (a1, a2) =
      Format.fprintf formatter "@[<hov 1>(%a,@;%a)@]"
        A1.format a1
        A2.format a2
  end)
end



open Utils





type name = string



open Show
  
module rec Show_name : Show.Show with type a = name =
             Show.Defaults(Show_string)
  
type qname = deriving_qname_1 list and deriving_qname_1 = name

open Show
  
module  Show_deriving_qname_1 : Show.Show with type a = deriving_qname_1 =
    Show.Defaults(Show_name)

module Show_qname : Show.Show with type a = qname =
             Show.Defaults(Show_list(Show_deriving_qname_1))

  
module NameMap = 
  struct
    include StringMap
    module Show_t (A : Show) =
      Show_map (String) (Show_string) (A)
  end


  
module NameSet = Set.Make(String)
  
(* Things not handled:
     records with polymorphic field types
     constraints 
     object types
     classes
     types with non-regular recursion
     types called 'a' (!)
*)
(* A type parameter with an optional variance annotation *)
type param =
  (deriving_param_4 * deriving_param_2)
  and deriving_param_4 =
  name
  and deriving_param_3 =
  [ | `Plus | `Minus
  ]
  and deriving_param_2 =
  deriving_param_3 option

open Show
  
module rec Show_deriving_param_3 : Show.Show with type a = deriving_param_3 =
             Show.Defaults
               (struct
                  type a = deriving_param_3
                  
                  let format formatter =
                    function
                    | `Plus -> Format.pp_print_string formatter "`Plus "
                    | `Minus -> Format.pp_print_string formatter "`Minus "
                    | _ -> assert false
                    
                end)
and
  Show_deriving_param_4 : Show.Show with type a = deriving_param_4 =
    Show.Defaults(Show_name)
and
  Show_deriving_param_2 : Show.Show with type a = deriving_param_2 =
    Show.Defaults(Show_option(Show_deriving_param_3))
and
  Show_param : Show.Show with type a = param =
    Show.Defaults(Show_2(Show_deriving_param_4)(Show_deriving_param_2))
  
(* A reference to one of the type constructors bound in the same
   declaration.  e.g. within the declaration

     type t1 = e1
      and ...
      and tn = en

   this refers to one of the ti.
*)
type localtype =
  [ | `Local of deriving_localtype_1
  ]
  and deriving_localtype_1 =
  name

open Show
  
module rec Show_localtype : Show.Show with type a = localtype =
             Show.Defaults
               (struct
                  type a = localtype
                  
                  let format formatter =
                    function
                    | `Local x ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "`Local ";
                         Show_deriving_localtype_1.format formatter x;
                         Format.pp_close_box formatter ())
                    | _ -> assert false
                    
                end)
and
  Show_deriving_localtype_1 : Show.Show with type a = deriving_localtype_1 =
    Show.Defaults(Show_name)
  
(* A reference to a type parameter *)
type tyvar = [ | `Tyvar of deriving_tyvar_1 ] and deriving_tyvar_1 = name

open Show
  
module rec Show_tyvar : Show.Show with type a = tyvar =
             Show.Defaults
               (struct
                  type a = tyvar
                  
                  let format formatter =
                    function
                    | `Tyvar x ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "`Tyvar ";
                         Show_deriving_tyvar_1.format formatter x;
                         Format.pp_close_box formatter ())
                    | _ -> assert false
                    
                end)
and
  Show_deriving_tyvar_1 : Show.Show with type a = deriving_tyvar_1 =
    Show.Defaults(Show_name)
  
(* An "atomic" expression.  These are the only things allowed to
   appear as subexpressions. *)
type deriving_atomic_2 =
  localtype
  and deriving_atomic_1 =
  tyvar
  and atomic =
  [ | localtype | tyvar
  ]

open Show
  
module rec Show_atomic : Show.Show with type a = atomic =
             Show.Defaults
               (struct
                  type a = atomic
                  
                  let format formatter =
                    function
                    | (#deriving_atomic_2 as x) ->
                        (Format.pp_open_hovbox formatter 0;
                         Show_deriving_atomic_2.format formatter x;
                         Format.pp_close_box formatter ())
                    | (#deriving_atomic_1 as x) ->
                        (Format.pp_open_hovbox formatter 0;
                         Show_deriving_atomic_1.format formatter x;
                         Format.pp_close_box formatter ())
                    | _ -> assert false
                    
                end)
and
  Show_deriving_atomic_2 : Show.Show with type a = deriving_atomic_2 =
    Show.Defaults(Show_localtype)
and
  Show_deriving_atomic_1 : Show.Show with type a = deriving_atomic_1 =
    Show.Defaults(Show_tyvar)
  
(* An application of a type constructor declared elsewhere *)
type deriving_appl_9 =
  qname
  and deriving_appl_8 =
  atomic
  and deriving_appl_7 =
  deriving_appl_8 list
  and deriving_appl_5 =
  (deriving_appl_9 * deriving_appl_7)
  and appl =
  [ | `Appl of deriving_appl_5
  ]

open Show
  
module rec Show_appl : Show.Show with type a = appl =
             Show.Defaults
               (struct
                  type a = appl
                  
                  let format formatter =
                    function
                    | `Appl x ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "`Appl ";
                         Show_deriving_appl_5.format formatter x;
                         Format.pp_close_box formatter ())
                    | _ -> assert false
                    
                end)
and
  Show_deriving_appl_9 : Show.Show with type a = deriving_appl_9 =
    Show.Defaults(Show_qname)
and
  Show_deriving_appl_8 : Show.Show with type a = deriving_appl_8 =
    Show.Defaults(Show_atomic)
and
  Show_deriving_appl_7 : Show.Show with type a = deriving_appl_7 =
    Show.Defaults(Show_list(Show_deriving_appl_8))
and
  Show_deriving_appl_5 : Show.Show with type a = deriving_appl_5 =
    Show.Defaults(Show_2(Show_deriving_appl_9)(Show_deriving_appl_7))
  
(* A polymorphic variant declaration *)
type tagspec =
  [ | `Tag of deriving_tagspec_6 | localtype
  ]
  and deriving_tagspec_9 =
  atomic
  and deriving_tagspec_8 =
  deriving_tagspec_9 option
  and deriving_tagspec_6 =
  (deriving_tagspec_10 * deriving_tagspec_8)
  and deriving_tagspec_10 =
  name
  and deriving_tagspec_1 =
  localtype

open Show
  
module rec Show_tagspec : Show.Show with type a = tagspec =
             Show.Defaults
               (struct
                  type a = tagspec
                  
                  let format formatter =
                    function
                    | `Tag x ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "`Tag ";
                         Show_deriving_tagspec_6.format formatter x;
                         Format.pp_close_box formatter ())
                    | (#deriving_tagspec_1 as x) ->
                        (Format.pp_open_hovbox formatter 0;
                         Show_deriving_tagspec_1.format formatter x;
                         Format.pp_close_box formatter ())
                    | _ -> assert false
                    
                end)
and
  Show_deriving_tagspec_9 : Show.Show with type a = deriving_tagspec_9 =
    Show.Defaults(Show_atomic)
and
  Show_deriving_tagspec_8 : Show.Show with type a = deriving_tagspec_8 =
    Show.Defaults(Show_option(Show_deriving_tagspec_9))
and
  Show_deriving_tagspec_10 : Show.Show with type a = deriving_tagspec_10 =
    Show.Defaults(Show_name)
and
  Show_deriving_tagspec_1 : Show.Show with type a = deriving_tagspec_1 =
    Show.Defaults(Show_localtype)
and
  Show_deriving_tagspec_6 : Show.Show with type a = deriving_tagspec_6 =
    Show.Defaults(Show_2(Show_deriving_tagspec_10)(Show_deriving_tagspec_8))
  
type variant =
  [ | `Variant of deriving_variant_5
  ]
  and deriving_variant_9 =
  [ | `Gt | `Lt | `Eq
  ]
  and deriving_variant_8 =
  tagspec
  and deriving_variant_7 =
  deriving_variant_8 list
  and deriving_variant_5 =
  (deriving_variant_9 * deriving_variant_7)

open Show
  
module rec Show_variant : Show.Show with type a = variant =
             Show.Defaults
               (struct
                  type a = variant
                  
                  let format formatter =
                    function
                    | `Variant x ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "`Variant ";
                         Show_deriving_variant_5.format formatter x;
                         Format.pp_close_box formatter ())
                    | _ -> assert false
                    
                end)
and
  Show_deriving_variant_9 : Show.Show with type a = deriving_variant_9 =
    Show.Defaults
      (struct
         type a = deriving_variant_9
         
         let format formatter =
           function
           | `Gt -> Format.pp_print_string formatter "`Gt "
           | `Lt -> Format.pp_print_string formatter "`Lt "
           | `Eq -> Format.pp_print_string formatter "`Eq "
           | _ -> assert false
           
       end)
and
  Show_deriving_variant_8 : Show.Show with type a = deriving_variant_8 =
    Show.Defaults(Show_tagspec)
and
  Show_deriving_variant_7 : Show.Show with type a = deriving_variant_7 =
    Show.Defaults(Show_list(Show_deriving_variant_8))
and
  Show_deriving_variant_5 : Show.Show with type a = deriving_variant_5 =
    Show.Defaults(Show_2(Show_deriving_variant_9)(Show_deriving_variant_7))
  
(* A record type *)
type field =
  (deriving_field_3 * deriving_field_2 * deriving_field_1)
  and deriving_field_3 =
  name
  and deriving_field_2 =
  atomic
  and deriving_field_1 =
  [ | `Mutable | `Immutable
  ]

open Show
  
module rec Show_deriving_field_1 : Show.Show with type a = deriving_field_1 =
             Show.Defaults
               (struct
                  type a = deriving_field_1
                  
                  let format formatter =
                    function
                    | `Mutable ->
                        Format.pp_print_string formatter "`Mutable "
                    | `Immutable ->
                        Format.pp_print_string formatter "`Immutable "
                    | _ -> assert false
                    
                end)
and
  Show_deriving_field_3 : Show.Show with type a = deriving_field_3 =
    Show.Defaults(Show_name)
and
  Show_deriving_field_2 : Show.Show with type a = deriving_field_2 =
    Show.Defaults(Show_atomic)
and
  Show_field : Show.Show with type a = field =
    Show.Defaults
      (Show_3(Show_deriving_field_3)(Show_deriving_field_2)
         (Show_deriving_field_1))
  
type record =
  [ | `Record of deriving_record_2
  ]
  and deriving_record_3 =
  field
  and deriving_record_2 =
  deriving_record_3 list

open Show
  
module rec Show_record : Show.Show with type a = record =
             Show.Defaults
               (struct
                  type a = record
                  
                  let format formatter =
                    function
                    | `Record x ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "`Record ";
                         Show_deriving_record_2.format formatter x;
                         Format.pp_close_box formatter ())
                    | _ -> assert false
                    
                end)
and
  Show_deriving_record_3 : Show.Show with type a = deriving_record_3 =
    Show.Defaults(Show_field)
and
  Show_deriving_record_2 : Show.Show with type a = deriving_record_2 =
    Show.Defaults(Show_list(Show_deriving_record_3))
  
(* A sum type *)
type summand =
  (deriving_summand_4 * deriving_summand_2)
  and deriving_summand_4 =
  name
  and deriving_summand_3 =
  atomic
  and deriving_summand_2 =
  deriving_summand_3 list

open Show
  
module rec Show_deriving_summand_4 :
             Show.Show with type a = deriving_summand_4 =
             Show.Defaults(Show_name)
and
  Show_deriving_summand_3 : Show.Show with type a = deriving_summand_3 =
    Show.Defaults(Show_atomic)
and
  Show_deriving_summand_2 : Show.Show with type a = deriving_summand_2 =
    Show.Defaults(Show_list(Show_deriving_summand_3))
and
  Show_summand : Show.Show with type a = summand =
    Show.Defaults(Show_2(Show_deriving_summand_4)(Show_deriving_summand_2))
  
type sum =
  [ | `Sum of deriving_sum_2
  ]
  and deriving_sum_3 =
  summand
  and deriving_sum_2 =
  deriving_sum_3 list

open Show
  
module rec Show_sum : Show.Show with type a = sum =
             Show.Defaults
               (struct
                  type a = sum
                  
                  let format formatter =
                    function
                    | `Sum x ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "`Sum ";
                         Show_deriving_sum_2.format formatter x;
                         Format.pp_close_box formatter ())
                    | _ -> assert false
                    
                end)
and
  Show_deriving_sum_3 : Show.Show with type a = deriving_sum_3 =
    Show.Defaults(Show_summand)
and
  Show_deriving_sum_2 : Show.Show with type a = deriving_sum_2 =
    Show.Defaults(Show_list(Show_deriving_sum_3))
  
(* A generative type *)
type fresh =
  [ | sum | record
  ]
  and deriving_fresh_2 =
  sum
  and deriving_fresh_1 =
  record

open Show
  
module rec Show_fresh : Show.Show with type a = fresh =
             Show.Defaults
               (struct
                  type a = fresh
                  
                  let format formatter =
                    function
                    | (#deriving_fresh_2 as x) ->
                        (Format.pp_open_hovbox formatter 0;
                         Show_deriving_fresh_2.format formatter x;
                         Format.pp_close_box formatter ())
                    | (#deriving_fresh_1 as x) ->
                        (Format.pp_open_hovbox formatter 0;
                         Show_deriving_fresh_1.format formatter x;
                         Format.pp_close_box formatter ())
                    | _ -> assert false
                    
                end)
and
  Show_deriving_fresh_2 : Show.Show with type a = deriving_fresh_2 =
    Show.Defaults(Show_sum)
and
  Show_deriving_fresh_1 : Show.Show with type a = deriving_fresh_1 =
    Show.Defaults(Show_record)
  
(* A type expression *)
type expr =
  [
    | `Function of deriving_expr_9
    | `Tuple of deriving_expr_5
    | appl
    | variant
    | atomic
  ]
  and deriving_expr_9 =
  (deriving_expr_11 * deriving_expr_10)
  and deriving_expr_6 =
  atomic
  and deriving_expr_5 =
  deriving_expr_6 list
  and deriving_expr_3 =
  appl
  and deriving_expr_2 =
  variant
  and deriving_expr_11 =
  atomic
  and deriving_expr_10 =
  atomic
  and deriving_expr_1 =
  atomic

open Show
  
module rec Show_expr : Show.Show with type a = expr =
             Show.Defaults
               (struct
                  type a = expr
                  
                  let format formatter =
                    function
                    | `Function x ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "`Function ";
                         Show_deriving_expr_9.format formatter x;
                         Format.pp_close_box formatter ())
                    | `Tuple x ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "`Tuple ";
                         Show_deriving_expr_5.format formatter x;
                         Format.pp_close_box formatter ())
                    | (#deriving_expr_3 as x) ->
                        (Format.pp_open_hovbox formatter 0;
                         Show_deriving_expr_3.format formatter x;
                         Format.pp_close_box formatter ())
                    | (#deriving_expr_2 as x) ->
                        (Format.pp_open_hovbox formatter 0;
                         Show_deriving_expr_2.format formatter x;
                         Format.pp_close_box formatter ())
                    | (#deriving_expr_1 as x) ->
                        (Format.pp_open_hovbox formatter 0;
                         Show_deriving_expr_1.format formatter x;
                         Format.pp_close_box formatter ())
                    | _ -> assert false
                    
                end)
and
  Show_deriving_expr_6 : Show.Show with type a = deriving_expr_6 =
    Show.Defaults(Show_atomic)
and
  Show_deriving_expr_5 : Show.Show with type a = deriving_expr_5 =
    Show.Defaults(Show_list(Show_deriving_expr_6))
and
  Show_deriving_expr_3 : Show.Show with type a = deriving_expr_3 =
    Show.Defaults(Show_appl)
and
  Show_deriving_expr_2 : Show.Show with type a = deriving_expr_2 =
    Show.Defaults(Show_variant)
and
  Show_deriving_expr_11 : Show.Show with type a = deriving_expr_11 =
    Show.Defaults(Show_atomic)
and
  Show_deriving_expr_10 : Show.Show with type a = deriving_expr_10 =
    Show.Defaults(Show_atomic)
and
  Show_deriving_expr_1 : Show.Show with type a = deriving_expr_1 =
    Show.Defaults(Show_atomic)
and
  Show_deriving_expr_9 : Show.Show with type a = deriving_expr_9 =
    Show.Defaults(Show_2(Show_deriving_expr_11)(Show_deriving_expr_10))
  
(* The right hand side of a declaration *)
type rhs =
  [ | `Fresh of deriving_rhs_7 | expr
  ]
  and deriving_rhs_9 =
  fresh
  and deriving_rhs_8 =
  [ | `Private | `Public
  ]
  and deriving_rhs_7 =
  (deriving_rhs_11 * deriving_rhs_9 * deriving_rhs_8)
  and deriving_rhs_12 =
  expr
  and deriving_rhs_11 =
  deriving_rhs_12 option
  and deriving_rhs_1 =
  expr

open Show
  
module rec Show_rhs : Show.Show with type a = rhs =
             Show.Defaults
               (struct
                  type a = rhs
                  
                  let format formatter =
                    function
                    | `Fresh x ->
                        (Format.pp_open_hovbox formatter 0;
                         Format.pp_print_string formatter "`Fresh ";
                         Show_deriving_rhs_7.format formatter x;
                         Format.pp_close_box formatter ())
                    | (#deriving_rhs_1 as x) ->
                        (Format.pp_open_hovbox formatter 0;
                         Show_deriving_rhs_1.format formatter x;
                         Format.pp_close_box formatter ())
                    | _ -> assert false
                    
                end)
and
  Show_deriving_rhs_8 : Show.Show with type a = deriving_rhs_8 =
    Show.Defaults
      (struct
         type a = deriving_rhs_8
         
         let format formatter =
           function
           | `Private -> Format.pp_print_string formatter "`Private "
           | `Public -> Format.pp_print_string formatter "`Public "
           | _ -> assert false
           
       end)
and
  Show_deriving_rhs_9 : Show.Show with type a = deriving_rhs_9 =
    Show.Defaults(Show_fresh)
and
  Show_deriving_rhs_12 : Show.Show with type a = deriving_rhs_12 =
    Show.Defaults(Show_expr)
and
  Show_deriving_rhs_11 : Show.Show with type a = deriving_rhs_11 =
    Show.Defaults(Show_option(Show_deriving_rhs_12))
and
  Show_deriving_rhs_1 : Show.Show with type a = deriving_rhs_1 =
    Show.Defaults(Show_expr)
and
  Show_deriving_rhs_7 : Show.Show with type a = deriving_rhs_7 =
    Show.Defaults
      (Show_3(Show_deriving_rhs_11)(Show_deriving_rhs_9)(Show_deriving_rhs_8))
  
type sigrhs = [ | rhs | `Nothing ] and deriving_sigrhs_1 = rhs

open Show
  
module rec Show_sigrhs : Show.Show with type a = sigrhs =
             Show.Defaults
               (struct
                  type a = sigrhs
                  
                  let format formatter =
                    function
                    | (#deriving_sigrhs_1 as x) ->
                        (Format.pp_open_hovbox formatter 0;
                         Show_deriving_sigrhs_1.format formatter x;
                         Format.pp_close_box formatter ())
                    | `Nothing ->
                        Format.pp_print_string formatter "`Nothing "
                    | _ -> assert false
                    
                end)
and
  Show_deriving_sigrhs_1 : Show.Show with type a = deriving_sigrhs_1 =
    Show.Defaults(Show_rhs)
  
(* A type declaration group *)
type deriving_decl_6 =
  param
  and deriving_decl_5 =
  deriving_decl_6 list
  and deriving_decl_3 =
  rhs
  and deriving_decl_2 =
  deriving_decl_3 NameMap.t
  and decl =
  (deriving_decl_5 * deriving_decl_2)

open Show
  
module rec Show_deriving_decl_6 : Show.Show with type a = deriving_decl_6 =
             Show.Defaults(Show_param)
and
  Show_deriving_decl_5 : Show.Show with type a = deriving_decl_5 =
    Show.Defaults(Show_list(Show_deriving_decl_6))
and
  Show_deriving_decl_3 : Show.Show with type a = deriving_decl_3 =
    Show.Defaults(Show_rhs)
and
  Show_deriving_decl_2 : Show.Show with type a = deriving_decl_2 =
    Show.Defaults(NameMap.Show_t(Show_deriving_decl_3))
and
  Show_decl : Show.Show with type a = decl =
    Show.Defaults(Show_2(Show_deriving_decl_5)(Show_deriving_decl_2))
  
(* A type declaration group in a signature *)
type sigdecl =
  (deriving_sigdecl_5 * deriving_sigdecl_2)
  and deriving_sigdecl_6 =
  param
  and deriving_sigdecl_5 =
  deriving_sigdecl_6 list
  and deriving_sigdecl_3 =
  sigrhs
  and deriving_sigdecl_2 =
  deriving_sigdecl_3 NameMap.t

open Show
  
module rec Show_deriving_sigdecl_6 :
             Show.Show with type a = deriving_sigdecl_6 =
             Show.Defaults(Show_param)
and
  Show_deriving_sigdecl_5 : Show.Show with type a = deriving_sigdecl_5 =
    Show.Defaults(Show_list(Show_deriving_sigdecl_6))
and
  Show_deriving_sigdecl_3 : Show.Show with type a = deriving_sigdecl_3 =
    Show.Defaults(Show_sigrhs)
and
  Show_deriving_sigdecl_2 : Show.Show with type a = deriving_sigdecl_2 =
    Show.Defaults(NameMap.Show_t(Show_deriving_sigdecl_3))
and
  Show_sigdecl : Show.Show with type a = sigdecl =
    Show.Defaults(Show_2(Show_deriving_sigdecl_5)(Show_deriving_sigdecl_2))
  
