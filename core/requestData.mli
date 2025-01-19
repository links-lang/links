open ProcessTypes

type request_data
  [@@deriving show]

val new_empty_request_data : unit -> request_data
val new_request_data :
  (string * string) list -> (* CGI parameters *)
  (string * string) list -> (* Cookies *)
  client_id -> (* Client URL *)
  request_data

val get_cgi_parameters : request_data -> (string * string) list
val set_cgi_parameters : request_data -> (string * string) list -> unit

val get_cookies : request_data -> (string * string) list
val set_cookies : request_data -> (string * string) list -> unit

val get_http_response_headers : request_data -> (string * string) list
val set_http_response_headers : request_data -> (string * string) list -> unit

val get_http_response_code : request_data -> int
val set_http_response_code : request_data -> int -> unit

val get_client_id : request_data -> client_id
val set_client_id : request_data -> client_id -> unit


module DecodeRequestHeaders :
sig
  val decode : string -> string
end

val is_remote_call : (string * string) list -> bool
val is_client_return : (string * string) list -> bool
val is_ajax_call : (string * string) list -> bool