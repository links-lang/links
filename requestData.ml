(*pp deriving *)
open ProcessTypes

type request_data = {
  cgi_parameters : (string * string) list ref;
  cookies : (string * string) list ref;
  http_response_headers : (string * string) list ref;
  http_response_code : int ref;
  client_id : client_id ref;
}
  deriving (Show)

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
