(*pp deriving *)

type request_data
  deriving (Show)

val new_request_data : unit -> request_data

val get_cgi_parameters : request_data -> (string * string) list
val set_cgi_parameters : request_data -> (string * string) list -> unit

val get_cookies : request_data -> (string * string) list
val set_cookies : request_data -> (string * string) list -> unit

val get_http_response_headers : request_data -> (string * string) list
val set_http_response_headers : request_data -> (string * string) list -> unit

val get_http_response_code : request_data -> int
val set_http_response_code : request_data -> int -> unit
