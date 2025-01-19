open ProcessTypes

type request_data = {
  cgi_parameters : (string * string) list ref;
  cookies : (string * string) list ref;
  http_response_headers : (string * string) list ref;
  http_response_code : int ref;
  client_id : client_id ref;
}
  [@@deriving show]

let new_empty_request_data () = {
  cgi_parameters = ref [];
  cookies = ref [];
  http_response_headers = ref [];
  http_response_code = ref 200;
  client_id = ref (dummy_client_id);
}

let new_request_data cgi_params cookies client_id = {
    cgi_parameters = ref cgi_params;
    cookies = ref cookies;
    http_response_headers = ref [];
    http_response_code = ref 200;
    client_id = ref client_id;
}

let get_cgi_parameters req_data = !(req_data.cgi_parameters)
let set_cgi_parameters req_data x = req_data.cgi_parameters := x

let get_cookies req_data = !(req_data.cookies)
let set_cookies req_data x = req_data.cookies := x

let get_http_response_headers req_data = !(req_data.http_response_headers)
let set_http_response_headers req_data x = req_data.http_response_headers := x

let get_http_response_code req_data = !(req_data.http_response_code)
let set_http_response_code req_data x = req_data.http_response_code := x

let get_client_id req_data = !(req_data.client_id)
let set_client_id req_data x = req_data.client_id := x



module DecodeRequestHeaders =
  struct

let hexa_val conf =
  match conf with
    | '0'..'9' -> Char.code conf - Char.code '0'
    | 'a'..'f' -> Char.code conf - Char.code 'a' + 10
    | 'A'..'F' -> Char.code conf - Char.code 'A' + 10
    | _ -> 0

let raw_decode : string -> string = fun s ->
  let rec need_decode i =
    if i < String.length s then
      match s.[i] with
    | '%' | '+' -> true
    | _ -> need_decode (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
          | '%' when i + 2 < String.length s -> i + 3
          | _ -> succ i
      in
    compute_len i (succ i1)
    else i1
  in
  let rec copy_decode_in : bytes -> int -> int -> bytes = fun s1 i i1 ->
    if i < String.length s then
      let i =
        match s.[i] with
          | '%' when i + 2 < String.length s ->
              let v = hexa_val s.[i + 1] * 16 + hexa_val s.[i + 2] in
        Bytes.set s1 i1 (Char.chr v); i + 3
          | '+' -> Bytes.set s1 i1  ' '; succ i
          | x -> Bytes.set s1 i1  x; succ i
      in
    copy_decode_in s1 i (succ i1)
    else s1
  in
  if need_decode 0 then
    let len = compute_len 0 0 in
    let s1 = Bytes.create len in
    Bytes.to_string (copy_decode_in s1 0 0)
  else
    s

let decode : string -> string = fun s ->
  let rs = raw_decode s in
  let rec strip_heading_and_trailing_spaces s =
    if String.length s > 0 then
      if s.[0] == ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 1 (String.length s - 1))
      else if s.[String.length s - 1] == ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 0 (String.length s - 1))
      else
    s
    else
      s
  in
  strip_heading_and_trailing_spaces rs


  end

(** remote client->server call *)
let is_remote_call params =
  List.mem_assoc "__name" params && List.mem_assoc "__args" params

(** return __result from server->client call with server continuation __continuation *)
let is_client_return params =
  List.mem_assoc "__continuation" params && List.mem_assoc "__result" params

let is_ajax_call cgi_args =
  (is_remote_call cgi_args) || (is_client_return cgi_args)
