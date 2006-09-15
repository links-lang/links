(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Printf

open Encodings
open Schema_pcre
open Schema_common
open Schema_types

(* TODO dates: boundary checks (e.g. 95/26/2003) *)
(* TODO a lot of almost cut-and-paste code, expecially in gFoo types validation
*)

(* TODO: distinguish primitive and derived types in the interface *)

  (** {2 Aux/Misc stuff} *)

let zero = Intervals.V.mk "0"

let xsd = Schema_xml.xsd
let add_xsd_prefix s = (xsd, Utf8.mk s)

let unsupported = [ "NOTATION"; "QName" ]

let is_empty s = Utf8.equal s (Utf8.mk "")

let xml_S_RE = pcre_regexp "[ \\t\\r\\n]+"
  (* split a string at XML recommendation "S" production boundaries *)
let split_xml_S s = pcre_split ~rex:xml_S_RE s
let norm_RE = pcre_regexp "[\\t\\r\\n]"

let char_of_hex =
  let int_of_hex_char = function
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4 | '5' -> 5 | '6' -> 6
    | '7' -> 7 | '8' -> 8 | '9' -> 9 | 'a' | 'A' -> 10 | 'b' | 'B' -> 11
    | 'c' | 'C' -> 12 | 'd' | 'D' -> 13 | 'e' | 'E' -> 14 | 'f' | 'F' -> 15
    | _ -> assert false
  in
    (* most significative, least significative *)
  fun ms ls -> Char.unsafe_chr (int_of_hex_char ms * 16 + int_of_hex_char ls)

let strip_parens s = Pcre.replace ~pat:"[()]" s
let add_limits s = "^" ^ s ^ "$"

exception Error of string
let simple_type_error name = raise (Error name)

let qualify = Ns.Label.mk_ascii

  (* regular expressions used to validate built-in types *)

let timezone_RE_raw = "(Z)|(([+-])?(\\d{2}):(\\d{2}))"
let date_RE_raw = "(\\d{4,})-(\\d{2})-(\\d{2})"
let time_RE_raw = "(\\d{2}):(\\d{2}):(\\d{2})"

let gYearMonth_RE_raw = sprintf "(-)?(\\d{4,})-(\\d{2})(%s)?" timezone_RE_raw
let gYear_RE_raw = sprintf "(-)?(\\d{4,})(%s)?" timezone_RE_raw
let gMonthDay_RE_raw = sprintf "--(\\d{2})-(\\d{2})(%s)?" timezone_RE_raw
let gDay_RE_raw = sprintf "---(\\d{2})(%s)?" timezone_RE_raw
let gMonth_RE_raw = "--(\\d{2})--(%s)?"

  (** {2 CDuce types} *)

let positive_field = false, qualify "positive", Builtin_defs.bool
let year_field = false, qualify "year", Builtin_defs.int
let month_field = false, qualify "month", Builtin_defs.int
let day_field = false, qualify "day", Builtin_defs.int
let hour_field = false, qualify "hour", Builtin_defs.int
let minute_field = false, qualify "minute", Builtin_defs.int
let second_field = false, qualify "second", Builtin_defs.int
  (* TODO this should be a decimal *)
let time_type_fields = [ hour_field; minute_field; second_field ]
let date_type_fields = [ year_field; month_field; day_field ]

let time_kind_field = false, qualify "time_kind", Builtin_defs.time_kind
let time_kind kind = (qualify "time_kind", Value.Atom (Atoms.V.mk_ascii kind))

  (* TODO the constraint that at least one part should be present isn't easily
  expressible with CDuce types *)
let duration_type = Types.rec_of_list false [
  time_kind_field;
  positive_field;
  true, qualify "year", Builtin_defs.int;
  true, qualify "month", Builtin_defs.int;
  true, qualify "day", Builtin_defs.int;
  true, qualify "hour", Builtin_defs.int;
  true, qualify "minute", Builtin_defs.int;
  true, qualify "second", Builtin_defs.int; (* TODO this should be a decimal *)
]
let timezone_type = Types.rec_of_list false [
  positive_field;
  hour_field; minute_field
]
let timezone_type_fields = [ true, qualify "timezone", timezone_type ]
let time_type = Types.rec_of_list false (time_kind_field :: time_type_fields @ timezone_type_fields)
let date_type = Types.rec_of_list false (time_kind_field :: positive_field :: date_type_fields)
let dateTime_type =
  Types.rec_of_list false (time_kind_field :: positive_field ::
    (date_type_fields @ time_type_fields @ timezone_type_fields))
let gYearMonth_type = Types.rec_of_list false [
  positive_field; time_kind_field; year_field; month_field
]
let gYear_type = Types.rec_of_list false [ time_kind_field; positive_field; year_field ]
let gMonthDay_type = Types.rec_of_list false [ time_kind_field; month_field; day_field ]
let gDay_type = Types.rec_of_list false [ time_kind_field; day_field ]
let gMonth_type = Types.rec_of_list false [ time_kind_field; month_field ]
let nonPositiveInteger_type = Builtin_defs.non_pos_int
let negativeInteger_type = Builtin_defs.neg_int
let nonNegativeInteger_type = Builtin_defs.non_neg_int
let positiveInteger_type = Builtin_defs.pos_int
let long_type = Builtin_defs.long_int
let int_type = Builtin_defs.int_int
let short_type = Builtin_defs.short_int
let byte_type = Builtin_defs.byte_int

let string_list_type = Sequence.star Builtin_defs.string

  (** {2 Validation functions (string -> Value.t)} *)

let parse_sign s =
  if Utf8.equal s (Utf8.mk "+") || Utf8.equal s (Utf8.mk "") then
    Value.vtrue
  else
    Value.vfalse

let validate_integer s =
  let s = Utf8.get_str s in
  if (String.length s = 0) then simple_type_error "integer"
  else
    try Value.Integer (Intervals.V.mk s)
    with Failure _ -> simple_type_error "integer"

let validate_decimal s =
  let s = Utf8.get_str s in
  try Value.float (float_of_string s)
  with Failure _ -> simple_type_error "decimal"

let strip_decimal_RE = Pcre.regexp "\\..*$"

let parse_date =
  let rex = Pcre.regexp (add_limits date_RE_raw) in
  fun s ->
  let abort () = simple_type_error "date" in
  let subs = try pcre_extract ~rex s with Not_found -> abort () in
  [ qualify "year", validate_integer subs.(1);
    qualify "month", validate_integer subs.(2);
    qualify "day", validate_integer subs.(3) ]

let parse_time =
  let rex = Pcre.regexp (add_limits time_RE_raw) in
  fun s ->
  let abort () = simple_type_error "time" in
  let subs = try pcre_extract ~rex s with Not_found -> abort () in
  [ qualify "hour", validate_integer subs.(1);
    qualify "minute", validate_integer subs.(2);
    qualify "second", validate_integer subs.(3) ]

let parse_timezone =
  let rex = Pcre.regexp (add_limits timezone_RE_raw) in
  fun s ->
  let abort () = simple_type_error "timezone" in
  let subs = try pcre_extract ~rex s with Not_found -> abort () in
  if Utf8.equal subs.(1) (Utf8.mk "Z") then
    [qualify "positive", Value.vtrue;
     qualify "hour", validate_integer (Utf8.mk "0");
     qualify "minute", validate_integer (Utf8.mk "0")]
  else
    [qualify "positive", parse_sign subs.(3);
     qualify "hour", validate_integer subs.(4);
     qualify "minute", validate_integer subs.(5)]
  (* parse a timezone from a string, if it's empty return the empty list,
  otherwise return a list containing a pair <"timezone", timezone value> *)
let parse_timezone' s =
  if is_empty s then
    []
  else
    [ qualify "timezone", Value.vrecord (parse_timezone s) ]

let validate_string s = Value.string_utf8 s
let validate_normalizedString s =
  validate_string (normalize_white_space `Replace s)
let validate_token s =
  validate_string (normalize_white_space `Collapse s)
let validate_token_list s =
  Value.sequence (List.map validate_token (split_xml_S s))

let validate_interval interval type_name s =
  let integer =   
    let s = Utf8.get_str s in
    if (String.length s = 0) then simple_type_error "integer"
    else
      try Intervals.V.mk s
      with Failure _ -> simple_type_error "integer"
  in
  if Intervals.contains integer interval then
    Value.Integer integer
  else
    simple_type_error type_name

let validate_bool s =
  if Utf8.equal s (Utf8.mk "true") || Utf8.equal s (Utf8.mk "1") then
    Value.vtrue
  else if Utf8.equal s (Utf8.mk "false") || Utf8.equal s (Utf8.mk "0") then
    Value.vfalse
  else
    simple_type_error "boolean"

let validate_duration =
  let rex = pcre_regexp
  "^([+-])?P((\\d+)Y)?((\\d+)M)?((\\d+)D)?(T((\\d+)H)?((\\d+)M)?((\\d+)S)?)?$"
  in
  fun s ->
  let abort () = simple_type_error "duration" in
  let subs = try pcre_extract ~rex s with Not_found -> abort () in
  try
    let fields =
      time_kind "duration" ::
      [qualify "positive", parse_sign subs.(1) ] @
      (if is_empty subs.(3) then []
      else [qualify "year", validate_integer subs.(3)]) @
      (if is_empty subs.(5) then []
      else [qualify "month", validate_integer subs.(5)]) @
      (if is_empty subs.(7) then []
      else [qualify "day", validate_integer subs.(7)]) @
      (if is_empty subs.(10) then []
      else [qualify "hour", validate_integer subs.(10)]) @
      (if is_empty subs.(12) then []
      else [qualify "minute", validate_integer subs.(12)]) @
      (if is_empty subs.(14) then []
      else [qualify "second", validate_integer subs.(14)])
    in
    Value.vrecord fields
  with Error _ -> abort ()

let validate_dateTime =
  let rex = Pcre.regexp (sprintf "^([+-])?(%s)T(%s)(%s)?$"
    (strip_parens date_RE_raw) (strip_parens time_RE_raw)
    (strip_parens timezone_RE_raw))
  in
  fun s ->
  let abort () = simple_type_error "dateTime" in
  let subs = try pcre_extract ~rex s with Not_found -> abort () in
  try
    let fields =
      time_kind "dateTime" ::
      [ qualify "positive", parse_sign subs.(1) ] @
      parse_date subs.(2) @
      parse_time subs.(3) @
      parse_timezone' subs.(4)
    in
    Value.vrecord fields
  with Error _ -> abort ()

let validate_gYearMonth =
  let rex = Pcre.regexp (add_limits gYearMonth_RE_raw) in
  fun s ->
    let abort () = simple_type_error "gYearMonth" in
    let subs = try pcre_extract ~rex s with Not_found -> abort () in
    try
      let fields = [
        time_kind "gYearMonth";
        qualify "positive", parse_sign subs.(1);
        qualify "year", validate_integer subs.(2);
        qualify "month", validate_integer subs.(3)
      ] @ parse_timezone' subs.(4)
      in
      Value.vrecord fields
    with Error _ -> abort ()

let validate_gYear =
  let rex = Pcre.regexp (add_limits gYear_RE_raw) in
  fun s ->
    let abort () = simple_type_error "gYear" in
    let subs = try pcre_extract ~rex s with Not_found -> abort () in
    try
      let fields = [
        time_kind "gYear";
        qualify "positive", parse_sign subs.(1);
        qualify "year", validate_integer subs.(2);
      ] @ parse_timezone' subs.(3)
      in
      Value.vrecord fields
    with Error _ -> abort ()

let validate_gMonthDay =
  let rex = Pcre.regexp (add_limits gMonthDay_RE_raw) in
  fun s ->
    let abort () = simple_type_error "gMonthDay" in
    let subs = try pcre_extract ~rex s with Not_found -> abort () in
    try
      let fields = [
        time_kind "gMonthDay";
        qualify "month", validate_integer subs.(1);
        qualify "day", validate_integer subs.(2);
      ] @ parse_timezone' subs.(3)
      in
      Value.vrecord fields
    with Error _ -> abort ()

let validate_gDay =
  let rex = Pcre.regexp (add_limits gDay_RE_raw) in
  fun s ->
    let abort () = simple_type_error "gDay" in
    let subs = try pcre_extract ~rex s with Not_found -> abort () in
    try
      let fields =
        time_kind "gDay" ::
        (qualify "day", validate_integer subs.(1)) ::
        (parse_timezone' subs.(2))
      in
      Value.vrecord fields
    with Error _ -> abort ()

let validate_gMonth =
  let rex = Pcre.regexp (add_limits gMonth_RE_raw) in
  fun s ->
    let abort () = simple_type_error "gMonth" in
    let subs = try pcre_extract ~rex s with Not_found -> abort () in
    try
      let fields =
        time_kind "gMonth" ::
        (qualify "month", validate_integer subs.(1)) ::
        (parse_timezone' subs.(2))
      in
      Value.vrecord fields
    with Error _ -> abort ()

let validate_time =
  let rex = Pcre.regexp (sprintf "^(%s)(%s)?$" (strip_parens time_RE_raw)
    (strip_parens timezone_RE_raw))
  in
  fun s ->
  let abort () = simple_type_error "time" in
  let subs = try pcre_extract ~rex s with Not_found -> abort () in
  try
    let fields =
      time_kind "time" ::
      parse_time subs.(1) @
      (if is_empty subs.(2) then []
      else [ qualify "timezone", Value.vrecord (parse_timezone subs.(2)) ])
    in
    Value.vrecord fields
  with Error _ -> abort ()

let validate_date =
  let rex = Pcre.regexp (sprintf "^(-)?(%s)(%s)?$" (strip_parens date_RE_raw)
    (strip_parens timezone_RE_raw))
  in
  fun s ->
  let abort () = simple_type_error "date" in
  let subs = try pcre_extract ~rex s with Not_found -> abort () in
  try
    let fields =
      time_kind "date" ::
      [ qualify "positive", parse_sign subs.(1) ] @
      parse_date subs.(2) @
      (if is_empty subs.(3) then []
      else [ qualify "timezone", Value.vrecord (parse_timezone subs.(3)) ])
    in
    Value.vrecord fields
  with Error _ -> abort ()

let validate_hexBinary s =
  let s = Utf8.get_str s in
  let len = String.length s in
  if len mod 2 <> 0 then
    simple_type_error "hexBinary";
  let res = String.create (len / 2) in
  let rec aux idx =
    if idx < len then begin
      String.unsafe_set res (idx / 2)
        (char_of_hex (String.unsafe_get s idx) (String.unsafe_get s (idx + 1)));
      aux (idx + 2)
    end
  in
  aux 0;
  validate_string (Utf8.mk res)

let validate_base64Binary s =
  let s = Utf8.get_str s in
  validate_string (Utf8.mk (Netencoding.Base64.decode s))

let validate_anyURI s =
  let s = Utf8.get_str s in
  try
    validate_string (Utf8.mk (Neturl.string_of_url (Neturl.url_of_string
      Neturl.ip_url_syntax s)))
  with Neturl.Malformed_URL -> simple_type_error "anyURI"

  (** {2 API backend} *)

type t = simple_type_definition * Types.t * (Utf8.t -> Value.t)

module QTable = Hashtbl.Make(Ns.QName)

let builtins : t QTable.t = QTable.create 50
let reg = QTable.add builtins


let restrict name (base,_,_) facets cd v =
  let name = add_xsd_prefix name in
  let t = simple_restrict (Some name) base facets in
  let b = (t,cd,v) in
  reg name b;
  b
  
let list name (item,_,_) cd v =
  let name = add_xsd_prefix name in
  let t = simple_list (Some name) item in
  let b = (t,cd,v) in
  reg name b;
  b


let primitive name cd v = 
  let name = add_xsd_prefix name in
  let rec t =
    { st_name = Some name;
      st_variety = Atomic t;
      st_facets = no_facets;
      st_base = None } in
  let b = (t,cd,v) in
  reg name b;
  b

let alias name b =
  let name = add_xsd_prefix name in
  reg name b

let any_simple_type = 
  primitive "anySimpleType" Builtin_defs.string validate_string
let string =
  primitive "string" Builtin_defs.string validate_string
let _ = 
  primitive "boolean" Builtin_defs.bool validate_bool
let _ = 
  primitive "hexBinary" Builtin_defs.string validate_hexBinary
let _ = 
  primitive "base64Binary" Builtin_defs.string validate_base64Binary
let _ = 
  primitive "anyURI" Builtin_defs.string validate_anyURI
let _ = 
  primitive "duration" duration_type validate_duration
let _ = 
  primitive "dateTime" dateTime_type validate_dateTime
let _ = 
  primitive "time" time_type validate_time
let _ = 
  primitive "date" date_type validate_date
let _ = 
  primitive "gYearMonth" gYearMonth_type validate_gYearMonth
let _ = 
  primitive "gYear" gYear_type validate_gYear
let _ = 
  primitive "gMonthDay" gMonthDay_type validate_gMonthDay
let _ = 
  primitive "gDay" gDay_type validate_gDay
let _ = 
  primitive "gMonth" gMonth_type validate_gMonth
let decimal = 
  primitive "decimal" Builtin_defs.float validate_decimal

let _ =
  alias "float" decimal;
  alias "double" decimal


let _ = 
  List.iter (fun n -> alias n string) unsupported

let int_type (name,min,max) = 
  let ival = match min,max with
    | Some min, Some max ->
	let min = Intervals.V.mk min and max = Intervals.V.mk max  in
	Intervals.bounded min max
    | None, Some max ->
	let max = Intervals.V.mk max  in
	Intervals.left max
    | Some min, None ->
	let min = Intervals.V.mk min  in
	Intervals.right min
    | None, None ->
	Intervals.any 
  in
  ignore (primitive name (Types.interval ival) (validate_interval ival name))

let () =
  List.iter int_type [ 
    "integer", None, None;
    "nonPositiveInteger", None, Some "0";
    "negativeInteger", None, Some "-1";
    "long", Some "-9223372036854775808", Some "9223372036854775807";
    "int", Some "-2147483648", Some "2147483647"; 
    "short", Some "-32768", Some "32767";
    "byte", Some "-128", Some "127";
    "nonNegativeInteger", Some "0", None;
    "unsignedLong", Some "0", Some "18446744073709551615";
    "unsignedInt", Some "0", Some "4294967295";
    "unsignedShort", Some "0", Some "65535";
    "unsignedByte", Some "0", Some "255";
    "positiveInteger", Some "1", None 
  ]
    


let normalized_string = 
  restrict "normalizedString" string
    { no_facets with whiteSpace = `Replace, false }
  Builtin_defs.string validate_normalizedString
let token = 
  restrict "token" normalized_string
    { no_facets with whiteSpace = `Collapse, false }
    Builtin_defs.string validate_token

let _ =
  alias "language" token;
  alias "Name" token;
  alias "NMTOKEN" token;
  alias "NCName" token;
  alias "ID" token;
  alias "IDREF" token;
  alias "ENTITY" token

let nmtokens =
  list "NMTOKENS" token string_list_type validate_token_list

let _ =
  alias "IDREFS" nmtokens;
  alias "ENTITIES" nmtokens



  (** {2 Printing} *)

open Big_int

type kind =
  Duration | DateTime | Time | Date | GYearMonth | GYear | GMonthDay | GDay |
  GMonth
type timezone = bool * Intervals.V.t * Intervals.V.t
  (* positive, hour, minute *)
type time_value = {
  kind: kind option; positive: bool option; year: Intervals.V.t option;
  month: Intervals.V.t option; day: Intervals.V.t option;
  hour: Intervals.V.t option; minute: Intervals.V.t option;
  second: Intervals.V.t option; timezone: timezone option
}
let null_value = {
  kind = None; positive = None; year = None; month = None; day = None;
  hour = None; minute = None; second = None; timezone = None
}

let string_of_time_type fields =
  let fail () = raise (Error "") in
  let parse_int = function Value.Integer i -> i | _ -> fail () in
  let parse_timezone v =
    let fields =
      try
        Value.get_fields v
      with Invalid_argument _ -> fail ()
    in
    let (positive, hour, minute) = (ref true, ref zero, ref zero) in
    List.iter
      (fun (lab, value) ->
	 let ns,name = Ns.Label.value lab in
        if ns != Ns.empty then fail ();
        (match Utf8.get_str name with
        | "positive" -> positive :=  (Value.equal value Value.vtrue)
        | "hour" -> hour := parse_int value
        | "minute" -> minute := parse_int value
        | _ -> fail ()))
      fields;
    !positive, !hour, !minute
  in
  let parse_time_kind = function
    | Value.Atom q ->
	let _,s = Atoms.V.value q in
        (match Utf8.get_str s with
        | "duration" -> Duration | "dateTime" -> DateTime | "time" -> Time
        | "date" -> Date | "gYearMonth" -> GYearMonth | "gYear" -> GYear
        | "gMonthDay" -> GMonthDay | "gDay" -> GDay | "gMonth" -> GMonth
        | _ -> fail ())
    | _ -> fail ()
  in
  let parse_positive = function
    | v when Value.equal v Value.vfalse -> false
    | _ -> true
  in
  let string_of_positive v =
    match v.positive with Some false -> "-" | _ -> ""
  in
  let string_of_year v =
    match v.year with None -> fail () | Some i -> Intervals.V.to_string i
  in
  let string_of_month v =
    match v.month with None -> fail () | Some i -> Intervals.V.to_string i
  in
  let string_of_day v =
    match v.day with None -> fail () | Some i -> Intervals.V.to_string i
  in
  let string_of_hour v =
    match v.hour with None -> fail () | Some i -> Intervals.V.to_string i
  in
  let string_of_minute v =
    match v.minute with None -> fail () | Some i -> Intervals.V.to_string i
  in
  let string_of_second v =
    match v.second with None -> fail () | Some i -> Intervals.V.to_string i
  in
  let string_of_date v =
    sprintf "%s-%s-%s" (string_of_year v) (string_of_month v) (string_of_day v)
  in
  let string_of_timezone v =
    match v.timezone with
    | Some (positive, hour, minute) ->
        sprintf "Z%s%s:%s" (if not positive then "-" else "")
          (Intervals.V.to_string hour) (Intervals.V.to_string minute)
    | None -> ""
  in
  let string_of_time v =
    sprintf "%s:%s:%s" (string_of_hour v) (string_of_minute v)
      (string_of_second v)
  in
  let v =
    List.fold_left
      (fun acc (lab, value) ->
	 let ns,local = Ns.Label.value lab in
         if ns != Ns.empty then fail ();
        (match Utf8.get_str local with
        | "year" -> { acc with year = Some (parse_int value) }
        | "month" -> { acc with month = Some (parse_int value) }
        | "day" -> { acc with day = Some (parse_int value) }
        | "hour" -> { acc with hour = Some (parse_int value) }
        | "minute" -> { acc with minute = Some (parse_int value) }
        | "second" -> { acc with second = Some (parse_int value) }
        | "timezone" -> { acc with timezone = Some (parse_timezone value) }
        | "time_kind" -> { acc with kind = Some (parse_time_kind value) }
        | "positive" -> { acc with positive = Some (parse_positive value) }
        | s -> fail ()))
      null_value fields
  in
  let s =
    match v.kind with
    | None -> fail ()
    | Some Duration ->
        sprintf "%sP%s%s%s%s"
          (string_of_positive v)
          (match v.year with Some v -> Intervals.V.to_string v ^ "Y" | _ -> "")
          (match v.month with Some v -> Intervals.V.to_string v ^ "M" | _ -> "")
          (match v.day with Some v -> Intervals.V.to_string v ^ "D" | _ -> "")
          (if v.hour = None && v.minute = None && v.second = None then
            ""
          else
            "T" ^
            (match v.hour with Some v -> Intervals.V.to_string v ^ "H" | _ ->
              "") ^
            (match v.minute with Some v -> Intervals.V.to_string v ^ "M" | _ ->
              "") ^
            (match v.second with Some v -> Intervals.V.to_string v ^ "S" | _ ->
              ""))
    | Some DateTime ->
        sprintf "%s%sT%s%s" (string_of_positive v) (string_of_date v)
          (string_of_time v) (string_of_timezone v)
    | Some Time ->
        sprintf "%s%s%s" (string_of_positive v) (string_of_time v)
          (string_of_timezone v)
    | Some Date ->
        sprintf "%s%s%s" (string_of_positive v) (string_of_date v)
          (string_of_timezone v)
    | Some GYearMonth ->
        sprintf "%s%s-%s%s" (string_of_positive v) (string_of_year v)
          (string_of_month v) (string_of_timezone v)
    | Some GYear ->
        sprintf "%s%s%s" (string_of_positive v) (string_of_year v)
          (string_of_timezone v)
    | Some GMonthDay ->
        sprintf "--%s%s%s" (string_of_month v) (string_of_day v)
          (string_of_timezone v)
    | Some GDay ->
        sprintf "---%s%s" (string_of_day v) (string_of_timezone v)
    | Some GMonth ->
        sprintf "--%s--%s" (string_of_month v) (string_of_timezone v)
  in
  Utf8.mk s

  (** {2 API} *)

let xsd_any = add_xsd_prefix "anyType"
let is s = QTable.mem builtins s || (Ns.QName.equal s xsd_any)
let iter f = QTable.iter f builtins

let get name = QTable.find builtins name
let simple_type (st,_,_) = st
let cd_type (_,t,_) = t
let validate (_,_,v) = v

let of_st = function
  | { st_name = Some n } -> get n
  | _ -> assert false


