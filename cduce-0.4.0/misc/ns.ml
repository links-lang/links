(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(* TODO:
   special treatment of prefixes xml and xmlns.
   Disallow: namespace xml="..."
*)

module U = Encodings.Utf8
let empty_str = U.mk ""

let split_qname s =
  let s = U.get_str s in
  try
    let i = String.rindex s ':' in
    let ns = String.sub s 0 i in
    let s = String.sub s (i + 1) (String.length s - i - 1) in
    (ns,U.mk s)
  with Not_found -> 
    ("",U.mk s)

let form_qname ns local =
  let ns' = U.get_str ns and local' = U.get_str local in
  if ns' = "" then local else U.mk (ns' ^ ":" ^ local')

module Uri = struct 
  include Upool.Make(U)
  let print ppf x = U.print ppf (value x)
end

let empty = Uri.mk empty_str
let xml_ns_str = "http://www.w3.org/XML/1998/namespace"
let xml_ns = Uri.mk (U.mk xml_ns_str)
let xsd_ns = Uri.mk (U.mk "http://www.w3.org/2001/XMLSchema")
let xsi_ns = Uri.mk (U.mk "http://www.w3.org/2001/XMLSchema-instance")






module Table = Map.Make(U)
type table = Uri.t Table.t
    (* Tables prefix->uri *)

let mktbl = List.fold_left (fun table (pr,ns) -> Table.add (U.mk pr) ns table)
let empty_table = mktbl Table.empty ["", empty; "xml", xml_ns]
let def_table = mktbl empty_table ["xsd", xsd_ns; "xsi", xsi_ns]

let mk_table =
  List.fold_left (fun table (pr,ns) -> Table.add pr ns table) empty_table
let get_table table =
  Table.fold 
    (fun prefix ns r -> 
       let std =
	 try Uri.equal (Table.find prefix empty_table) ns
	 with Not_found -> false in
       if std then r else (prefix,ns)::r) table []



(* TODO: avoid re-inserting the same hint for the same
   namespace ==> otherwise memory leak with load_xml ... *)
let global_hints = Hashtbl.create 63

module Printer = struct
(* TODO: detect the case when there is no unqualified tag.
   In this case, it is possible to use a default namespace for
   the whole document... *)


  type slot = Hint of U.t list | Set of U.t
  module H = Hashtbl.Make(Uri)

  type printer = {
    ns_to_prefix : slot ref H.t;
    mutable prefixes : (U.t * Uri.t) list;
    table : table;
    mutable hints : U.t list;
    mutable counter : int;
  }

  let get_prefix p ns =
    try H.find p.ns_to_prefix ns
    with Not_found ->
      let r = ref (Hint []) in
      H.add p.ns_to_prefix ns r;
      r

  let printer table =
    let p = 
      { ns_to_prefix = H.create 63;
	prefixes     = [];
	table        = table;
	hints        = [];
	counter      = 0
      } in
    H.add p.ns_to_prefix empty (ref (Set empty_str));
    Table.iter 
      (fun pr ns ->
	 if (U.get_str pr <> "") then
	   match get_prefix p ns  with
	     | { contents = Hint l } as r -> 
		 p.hints <- pr::p.hints; 
		 r := Hint (pr::l)
	     | _ -> assert false)  table;
    p


  let is_prefix_free p pr =
    not (List.exists (fun (pr',_) -> U.equal pr pr') p.prefixes)

  let is_really_free p pr =
    (is_prefix_free p pr) &&
     not (List.exists (fun pr' -> U.equal pr pr') p.hints)

  let rec fresh_prefix p =
    p.counter <- succ p.counter;
    let s = U.mk (Printf.sprintf "ns%i" p.counter) in
    if (is_really_free p s) then s else fresh_prefix p

  let find_good_prefix p ns hint =
    try List.find (is_prefix_free p) hint
    with Not_found -> 
      try List.find (is_really_free p) (Hashtbl.find_all global_hints ns)
      with Not_found -> fresh_prefix p

  let add_prefix p pr ns =
    if (ns != empty) || (U.get_str pr <> "")
    then p.prefixes <- (pr, ns) :: p.prefixes

  let register_ns p ns = 
    if ns == xml_ns then ()
    else match get_prefix p ns with
      | { contents = Hint l } as r ->
	  let pr = find_good_prefix p ns l in
	  r := Set pr;
	  add_prefix p pr ns
      | _ -> ()

  let register_qname p (ns,_) = register_ns p ns

  let prefixes p = p.prefixes

  let tag p (ns,l) =
    let l = U.get_str l in
    if ns == xml_ns then "xml:" ^ l
    else match !(get_prefix p ns) with
      | Set pr ->
	  let pr = U.get_str pr in
	  if pr = "" then l
          else pr ^ ":" ^ l
      | _ -> assert false

  let attr p (ns,l) =
    let l = U.get_str l in
    if ns == xml_ns then "xml:" ^ l
    else if ns == empty then l
    else
      match !(get_prefix p ns) with
	| Set pr ->
	    let pr = U.get_str pr in
	    assert(pr <> "");
            pr ^ ":" ^ l
	| _ -> assert false


  let any_ns p ns =
    match !(get_prefix p ns) with
      | Set pr ->
	  let pr = U.get_str pr in
	  if pr = "" then ".:*"
          else pr ^ ":*"
      | _ -> assert false
end


module InternalPrinter =
struct
  let p = ref (Printer.printer def_table)

  let set_table t = 
    p := Printer.printer t

  let any_ns ns =
    Printer.register_ns !p ns;
    Printer.any_ns !p ns

  let tag q =
    Printer.register_qname !p q;
    Printer.tag !p q

  let attr q =
    Printer.register_qname !p q;
    Printer.attr !p q

  let dump ppf =
    List.iter
      (fun (pr, ns) ->
	 Format.fprintf ppf "%a=>\"%a\"@." U.print pr Uri.print ns
      )	(Printer.prefixes !p)

  let print_tag ppf q =
    Format.fprintf ppf "%s" (tag q)

  let print_attr ppf q =
    Format.fprintf ppf "%s" (attr q)

  let print_any_ns ppf ns =
    Format.fprintf ppf "%s" (any_ns ns)
end

module Label = struct
  include Upool.Make(Custom.Pair(Uri)(U))

  let print_attr ppf q = InternalPrinter.print_attr ppf (value q)
  let print_tag ppf q = InternalPrinter.print_tag ppf (value q)
  let print_quote ppf q = Format.fprintf ppf "`"; print_tag ppf q

  let string_of_attr q = InternalPrinter.attr (value q)
  let string_of_tag q = InternalPrinter.tag (value q)
(*
  let to_string s = U.to_string (value s)
  let print ppf s = U.print ppf (value s)
*)

  let mk_ascii s = mk (empty, U.mk s)
  let get_ascii q = U.get_str (snd (value q))
(*
  let split q =
    let ns,local = split_qname (value q) in
    U.mk ns, local
*)
end







let add_prefix pr ns table =
  if (U.get_str pr <> "") then Hashtbl.add global_hints ns pr;
  Table.add pr ns table

let merge_tables t1 t2 =
  Table.fold add_prefix t2 t1

let dump_table ppf table =
  Table.iter
    (fun pr ns ->
       Format.fprintf ppf "%a=>\"%a\"@." U.print pr Uri.print ns
    ) table
       

exception UnknownPrefix of U.t
let map_prefix table pr =
  try Table.find pr table 
  with Not_found -> raise (UnknownPrefix pr)

let map_tag table tag =
  let pr, local = split_qname tag in
  (map_prefix table (U.mk pr), local)

let map_attr table n =
  let pr, local = split_qname n in
  ((if pr="" then empty else map_prefix table (U.mk pr)),local)

let att table ((pr,local),v) = 
  Label.mk
    ((if pr="" then empty else map_prefix table (U.mk pr)),local), v

let process_start_tag table tag attrs =
  let rec aux (table : table) (attrs : ((string * U.t) * U.t) list) = function
    | [] -> (table, map_tag table (U.mk tag), List.rev_map (att table) attrs)
    | ("xmlns",uri)::rest ->
	let table = add_prefix empty_str (Uri.mk (U.mk uri)) table in
	aux table attrs rest
    | (n,v)::rest ->
	match split_qname (U.mk n) with
	  | ("xmlns",pr) ->
	      let table = add_prefix pr (Uri.mk (U.mk v)) table in
	      aux table attrs rest
	  | x ->
	      aux table ((x,U.mk v)::attrs) rest in
  aux table [] attrs




module QName = struct
  include Custom.Pair(Uri)(U)
  let print = InternalPrinter.print_tag
(*  let mk_ascii s = (empty, U.mk s)
  let get_ascii (_,s) = U.get_str s *)
  let to_string = InternalPrinter.tag 
end

