
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


module ClientID : NAME
module ProcessID : NAME
module AccessPointID : NAME
module ChannelID : NAME

type client_id = ClientID.t
  [@@deriving show]

type process_id = ProcessID.t
  [@@deriving show]

type apid = AccessPointID.t
  [@@deriving show]

type channel_id = ChannelID.t
  [@@deriving show]

(* Distinguished PID for main thread *)
val main_process_pid : process_id

(* Dummy client ID *)
val dummy_client_id : client_id

(* Maps *)
module type PIDMAP = Utility.Map with type key = ProcessID.t
module type CLIENTIDMAP = Utility.Map with type key = ClientID.t
module type ACCESSPOINTIDMAP = Utility.Map with type key = AccessPointID.t
module type CHANNELIDMAP = Utility.Map with type key = ChannelID.t

module PidMap : PIDMAP
module ClientIDMap : CLIENTIDMAP
module AccessPointIDMap : ACCESSPOINTIDMAP
module ChannelIDMap : CHANNELIDMAP

type 'a pid_map = 'a PidMap.t
type 'a client_id_map = 'a ClientIDMap.t
type 'a apid_map = 'a AccessPointIDMap.t
type 'a channel_id_map = 'a ChannelIDMap.t

(* Sets *)
module type PIDSET = Utility.Set with type elt = ProcessID.t
module type CLIENTIDSET = Utility.Set with type elt = ClientID.t
module type ACCESSPOINTIDSET = Utility.Set with type elt = AccessPointID.t
module type CHANNELIDSET = Utility.Set with type elt = ChannelID.t

module PidSet : PIDSET
module ClientIDSet : CLIENTIDSET
module AccessPointIDSet : ACCESSPOINTIDSET
module ChannelIDSet: CHANNELIDSET

type pid_set = PidSet.t
type client_id_set = ClientIDSet.t
type apid_set = AccessPointIDSet.t
type channelid_set = ChannelIDSet.t
