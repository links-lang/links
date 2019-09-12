open Utility

(* Should be safe to not have an explicit lock due to the co-operative
 * nature of the scheduling, since things shouldn't be pre-empted mid
 * counter update. *)
module type NAME = sig
  type t
  val create : unit -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
  val of_string : string -> t
  val to_json : t -> Yojson.Basic.t
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

let name_source : int ref = ref 0

let fresh_name = fun () ->
    let ret = !name_source in
    incr name_source;
    ret

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

module type NAMEINFO = sig
  val prefix : string
end

module Simple_string_name = functor ( NameInfo : NAMEINFO ) ->
struct
  type t = string

  let make_name_with_id id = NameInfo.prefix ^ (string_of_int id)

  let create () =
    fresh_name ()
    |> make_name_with_id

  let compare n1 n2 = compare n1 n2
  let equal n1 n2 = (compare n1 n2) = 0
  let to_string n = n
  let of_string n = n
  let to_json n = `String n
  let pp = Format.pp_print_string
  let show = fun x  -> Format.asprintf "%a" pp x
end

module ClientID  : NAME = Simple_string_name(struct let prefix = "cid_" end)
module ProcessID : NAME = Simple_string_name(struct let prefix = "srvPid_" end)
module AccessPointID : NAME = Simple_string_name(struct let prefix = "srvAp_" end)
module ChannelID : NAME = Simple_string_name(struct let prefix = "chan_" end)

let main_process_pid = ProcessID.of_string "MAIN"
let dummy_client_id = ClientID.of_string "DUMMY_CLIENT"

module type PIDMAP = Utility.Map with type key = ProcessID.t
module type CLIENTIDMAP = Utility.Map with type key = ClientID.t
module type ACCESSPOINTIDMAP = Utility.Map with type key = AccessPointID.t
module type CHANNELIDMAP = Utility.Map with type key = ChannelID.t

module PidMap : PIDMAP = Map.Make(ProcessID)
module ClientIDMap : CLIENTIDMAP = Map.Make(ClientID)
module AccessPointIDMap : ACCESSPOINTIDMAP = Map.Make(AccessPointID)
module ChannelIDMap : CHANNELIDMAP = Map.Make(ChannelID)

module type PIDSET = Utility.Set with type elt = ProcessID.t
module type CLIENTIDSET = Utility.Set with type elt = ClientID.t
module type ACCESSPOINTIDSET = Utility.Set with type elt = AccessPointID.t
module type CHANNELIDSET = Utility.Set with type elt = ChannelID.t

module PidSet : PIDSET = Set.Make(ProcessID)
module ClientIDSet : CLIENTIDSET = Set.Make(ClientID)
module AccessPointIDSet : ACCESSPOINTIDSET = Set.Make(AccessPointID)
module ChannelIDSet: CHANNELIDSET = Set.Make(ChannelID)


type client_id = ClientID.t
  [@@deriving show]

type process_id = ProcessID.t
  [@@deriving show]

type apid = AccessPointID.t
  [@@deriving show]

type channel_id = ChannelID.t
  [@@deriving show]

type 'a pid_map = 'a PidMap.t
type 'a client_id_map = 'a ClientIDMap.t
type 'a apid_map = 'a AccessPointIDMap.t
type 'a channel_id_map = 'a ChannelIDMap.t

type pid_set = PidSet.t
type client_id_set = ClientIDSet.t
type apid_set = AccessPointIDSet.t
type channelid_set = ChannelIDSet.t
