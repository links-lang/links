(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Encodings

let pos_int = Types.interval (Intervals.right (Intervals.V.mk "1"))
let non_neg_int = Types.interval (Intervals.right (Intervals.V.mk "0"))
let neg_int = Types.interval (Intervals.left (Intervals.V.mk "-1"))
let non_pos_int = Types.interval (Intervals.left (Intervals.V.mk "0"))

let mk_interval_type l r =
  Types.interval (Intervals.bounded (Intervals.V.mk l) (Intervals.V.mk r))
let long_int = mk_interval_type "-9223372036854775808" "9223372036854775807"
let int_int = mk_interval_type "-2147483648" "2147483647"
let short_int = mk_interval_type "-32768" "32767"
let byte_int = mk_interval_type "-128" "127"
let caml_int = mk_interval_type (string_of_int min_int) (string_of_int max_int)
let byte_int = mk_interval_type "0" "255"

let non_zero_int = Types.cup pos_int neg_int

let decimal_intstr =
  Sequence.plus (Types.char (Chars.char_class
			       (Chars.V.mk_char '0')
			       (Chars.V.mk_char '9')
			    )
		)

let octal_intstr =
  Sequence.plus (Types.char (Chars.char_class
			       (Chars.V.mk_char '0')
			       (Chars.V.mk_char '7')
			    )
		)

let binary_intstr =
  Sequence.plus (Types.char (Chars.char_class
			       (Chars.V.mk_char '0')
			       (Chars.V.mk_char '1')
			    )
		)

let hex_intstr =
  Sequence.plus (
       Types.cup  
          (Types.char (Chars.char_class
			       (Chars.V.mk_char '0')
			       (Chars.V.mk_char '9')
			    )
	  )
          (Types.cup
               (Types.char (Chars.char_class
			       (Chars.V.mk_char 'a')
			       (Chars.V.mk_char 'f')
			    )
	        )
                (Types.char (Chars.char_class
			       (Chars.V.mk_char 'A')
			       (Chars.V.mk_char 'F')
			    )
		)
	   )
   )


let hex_str = 
  Types.times 
    (Types.cons (Types.char (Chars.atom (Chars.V.mk_char '0'))))
    (Types.cons(
      Types.times 
        (Types.cons(
           Types.cup 
             (Types.char (Chars.atom (Chars.V.mk_char 'X')))
             (Types.char (Chars.atom (Chars.V.mk_char 'x')))
           )
        )
        (Types.cons hex_intstr)
      )
    )                        (* [ '0' ('X' | 'x') hex+] *)

let oct_str =  
  Types.times 
    (Types.cons (Types.char (Chars.atom (Chars.V.mk_char '0'))))
    (Types.cons(
      Types.times 
        (Types.cons(
           Types.cup 
             (Types.char (Chars.atom (Chars.V.mk_char 'O')))
             (Types.char (Chars.atom (Chars.V.mk_char 'o')))
           )
        )
        (Types.cons octal_intstr)
      )
    )   


let bin_str =
  Types.times 
    (Types.cons (Types.char (Chars.atom (Chars.V.mk_char '0'))))
    (Types.cons(
      Types.times 
        (Types.cons(
           Types.cup 
             (Types.char (Chars.atom (Chars.V.mk_char 'B')))
             (Types.char (Chars.atom (Chars.V.mk_char 'b')))
           )
        )
        (Types.cons binary_intstr)
      )
    )   

let pos_intstr =
  Types.cup 
      decimal_intstr (Types.cup 
      hex_str     (Types.cup 
      bin_str
      oct_str))

let neg_intstr =
  Types.times 
    (Types.cons (Types.char (Chars.atom (Chars.V.mk_char '-'))))
    (Types.cons pos_intstr)

let intstr = Types.cup pos_intstr neg_intstr  (* [ '-'? '0'--'9'+ ] *)


let true_atom = Atoms.V.mk_ascii "true"
let false_atom = Atoms.V.mk_ascii "false"
let true_type = Types.atom (Atoms.atom true_atom)
let false_type = Types.atom (Atoms.atom false_atom)

let bool = Types.cup true_type false_type
let nil = Sequence.nil_type
let string = Sequence.string
let char = Types.Char.any
let any = Types.any
let int = Types.Int.any
let atom = Types.atom Atoms.any

let char_latin1 = Types.char (Chars.mk_classes [ (0,255) ])
let string_latin1 = Sequence.star char_latin1

let time_kind =
  List.fold_left (fun acc t -> Types.cup acc t) Types.empty
    (List.map
      (fun s -> Types.atom (Atoms.atom (Atoms.V.mk_ascii s)))
      [ "duration"; "dateTime"; "time"; "date"; "gYearMonth"; "gYear";
        "gMonthDay"; "gDay"; "gMonth" ])

open Ident
let get_label = Label.mk_ascii "get"
let set_label = Label.mk_ascii "set"

let mk_ref ~get ~set =
  LabelMap.from_list_disj [ get_label, get; set_label, set ]

let ref_type t =
  let get = Types.cons (Types.arrow Sequence.nil_node t)
  and set = Types.cons (Types.arrow t Sequence.nil_node) in
  Types.record_fields (false , mk_ref ~get ~set)

let float_abs =
  "float"

let float =
  Types.abstract (Types.Abstract.atom float_abs)

let any_attr_node = Types.cons (Types.record_fields (true,LabelMap.empty)) 
let any_xml,any_xml_seq,any_xml_content = 
  let elt = Types.make () in
  let seq = Types.make () in
  let any_xml_content = Types.cons (Types.times any_attr_node seq) in
  let elt_d = Types.xml (Types.cons atom) any_xml_content in
  let elt_char_d = Types.cup elt_d char in
  let seq_d = Types.cup nil (Types.times (Types.cons elt_char_d) seq) in
  Types.define elt elt_d;
  Types.define seq seq_d;
  elt_d,seq_d,any_xml_content

let any_xml_with_tag t =
  Types.xml (Types.cons (Types.atom t)) any_xml_content
    
