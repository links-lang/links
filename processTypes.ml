(*pp deriving *)
open Lwt
open Utility

module type NAME = sig
  type t
  val create : unit -> t Lwt.t
  val create_unsafe : unit -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
  val of_string : string -> t
  val to_json : t -> string
  module Show_t : Deriving_Show.Show with type a = t
end

let _ = ignore @@ Nocrypto_entropy_lwt.initialize ()

let name_source : int ref = ref 0
let name_mutex = Lwt_mutex.create ()

let get_and_increment_id_unsafe = fun () ->
    let ret = !name_source in
    incr name_source;
    ret

let get_and_increment_id : unit -> int Lwt.t = fun () ->
  Lwt_mutex.with_lock name_mutex (fun () ->
    Lwt.return (get_and_increment_id_unsafe ()))

  (*
module Random_string_name = struct
  type t = string
  let gen_random_int = fun () -> Nocrypto.Rng.Int.gen max_int

  let create_name () =
    (get_and_increment_id ()) >>= fun id ->
    Lwt.return (make_name_with_id id)

  let make_name_with_id id =
    Cstruct.to_string @@
      Nocrypto.Base64.encode @@
      Nocrypto.Hash.digest `SHA256 @@
      Cstruct.of_string @@
      "srv_" ^ (string_of_int id) ^ "_" ^ (string_of_int @@ gen_random_int ())

  let create_name_unsafe () =
    make_name_with_id (get_and_increment_id_unsafe ())

  let create = create_name
  let create_unsafe = create_name_unsafe
  let compare n1 n2 = Pervasives.compare n1 n2
  let equal n1 n2 = (compare n1 n2) = 0
  let to_string n = n
  let of_string n = n
  let to_json n = "\"" ^ n ^ "\""
  module Show_t = Deriving_Show.Show_string
end

module Int_name = struct
  type t = int
  let create = get_and_increment_id
  let create_unsafe = get_and_increment_id_unsafe
  let compare n1 n2 = Pervasives.compare n1 n2
  let equal n1 n2 = (compare n1 n2) = 0
  let to_string = string_of_int
  let of_string = int_of_string
  let to_json = to_string
  module Show_t = Deriving_Show.Show_int
end
*)

module Simple_string_name = struct
  type t = string

  let make_name_with_id id = "srv_" ^ (string_of_int id)

  let create_name () =
    (get_and_increment_id ()) >>= fun id ->
    Lwt.return (make_name_with_id id)

  let create_name_unsafe () =
    make_name_with_id (get_and_increment_id_unsafe ())

  let create = create_name
  let create_unsafe = create_name_unsafe
  let compare n1 n2 = Pervasives.compare n1 n2
  let equal n1 n2 = (compare n1 n2) = 0
  let to_string n = n
  let of_string n = n
  let to_json n = "\"" ^ n ^ "\""
  module Show_t = Deriving_Show.Show_string
end

module ClientID  : NAME = Simple_string_name
module ProcessID : NAME = Simple_string_name

let main_process_pid = ProcessID.of_string "MAIN"
let dummy_client_id = ClientID.of_string "MAIN"

module type PIDMAP = Utility.Map with type key = ProcessID.t
module type CLIENTIDMAP = Utility.Map with type key = ClientID.t

module PidMap : PIDMAP = Map.Make(ProcessID)
module ClientIDMap : CLIENTIDMAP = Map.Make(ClientID)

module type PIDSET = Utility.Set with type elt = ProcessID.t
module type CLIENTIDSET = Utility.Set with type elt = ClientID.t

module PidSet : PIDSET = Set.Make(ProcessID)
module ClientIDSet : CLIENTIDSET = Set.Make(ClientID)

type client_id = ClientID.t
  deriving (Show)

type process_id = ProcessID.t
  deriving (Show)

type 'a pid_map = 'a PidMap.t
type 'a client_id_map = 'a ClientIDMap.t

type pid_set = PidSet.t
type client_id_set = ClientIDSet.t

