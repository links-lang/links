(* Parse multipart formdata... ripped out of ocamlcgi *)

(* Using the following function, we avoid the use of the Str library
 * which is not compatible with the threads of ocaml.
 * Thanks to Olivier Montanuy. *)
let split separator text =
  let len = String.length text in
  let rec loop pos =
    if pos < len then
      try
           let last = String.index_from text pos separator in
     let str = String.sub text pos (last-pos) in
      str::(loop (succ last))
      with Not_found ->
     if pos < len then [String.sub text pos (len-pos)]
     else []
    else []
  in
  loop 0

let string_starts_with s pref =
  String.length s >= String.length pref &&
  String.sub s 0 (String.length pref) = pref

(* parse_multipart_args: parsing of the arguments for
   multipart/form-data encoding *)

let boundary_re1 =
  Str.regexp_case_fold "boundary=\"\\([^\"]+\\)\""
let boundary_re2 =
  Str.regexp_case_fold "boundary=\\([^ \t\r\n]+\\)"
let name_re1 =
  Str.regexp_case_fold "name=\"\\([^\"]+\\)\""
let name_re2 =
  Str.regexp_case_fold "name=\\([^ \t\r\n;:]+\\)"
let filename_re1 =
  Str.regexp_case_fold "filename=\"\\([^\"]*\\)\""
let filename_re2 =
  Str.regexp_case_fold "filename=\\([^ \t\r\n;:]+\\)"
let content_type_re1 =
  Str.regexp_case_fold "Content-type:[ \t]*\"\\([^\"]+\\)\""
let content_type_re2 =
  Str.regexp_case_fold "Content-type:[ \t]*\\([^ \t\r\n;:]+\\)"
let separator_re =
  Str.regexp "\r\n\r\n"

let match_string re1 re2 str =
  try
    ignore(Str.search_forward re1 str 0); Str.matched_group 1 str
  with Not_found ->
    ignore(Str.search_forward re2 str 0); Str.matched_group 1 str

(* Extract field name and value from a chunk.  Raise Not_found if not
   a valid chunk. *)

type field_data = {
  value: string;
  filename: string;
  content_type: string
}

let extract_field chunk =
  let pos_separator = Str.search_forward separator_re chunk 0 in
  let header = String.sub chunk 0 pos_separator in
  let field_name = match_string name_re1 name_re2 header in
  let field_filename =
    try match_string filename_re1 filename_re2 header
    with Not_found -> "" in
  let field_content_type =
    try match_string content_type_re1 content_type_re2 header
    with Not_found -> "" in
  let beg_value = pos_separator + 4 in
  (* Chop final \r\n that browsers insist on putting *)
  let end_value =
    let len = String.length chunk in
    if len >= beg_value && String.sub chunk (len - 2) 2 = "\r\n"
    then len - 2
    else len in
  let field_value =
    String.sub chunk beg_value (end_value - beg_value) in
  (field_name, { filename = field_filename;
                 content_type = field_content_type;
                 value = field_value })

(* Same, for a list of chunks *)

let rec extract_fields accu = function
  | [] ->
      accu
  | chunk :: rem ->
      extract_fields
    (try extract_field chunk :: accu with Not_found -> accu)
    rem

let parse_multipart_args mime_type content : (string * field_data) list =
  if not (string_starts_with mime_type "multipart/form-data")
  then
    raise (Errors.runtime_error ("Cannot handle multipart form data of type " ^ mime_type));
  (* Determine boundary delimiter *)
  let boundary =
    try
      match_string boundary_re1 boundary_re2 mime_type
    with Not_found ->
      raise (Errors.runtime_error ("No boundary provided in " ^ mime_type)) in
  extract_fields [] (Str.split (Str.regexp_string ("--" ^ boundary)) content)
