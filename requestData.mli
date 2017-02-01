(*pp deriving *)

type request_data
  deriving (Show)

(* At the moment, it doesn't make sense for this to be properly abstract. *)
(* It would be lovely to at some point though? *)
type client_id = int

val new_request_data : (string * string) list -> (string * string) list -> client_id -> request_data

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
