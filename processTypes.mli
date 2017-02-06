(*pp deriving *)

module type NAME = sig
  type t
  val create : unit -> t Lwt.t
  val create_unsafe : unit -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
  val of_string : string -> t
  module Show_t : Deriving_Show.Show with type a = t
end


module ClientID : NAME
module ProcessID : NAME

type client_id = ClientID.t
  deriving (Show)

type process_id = ProcessID.t
  deriving (Show)

(* Distinguished PID for main thread *)
val main_process_pid : process_id

(* Dummy client ID *)
val dummy_client_id : client_id

module type PIDMAP = Utility.Map with type key = ProcessID.t
module type CLIENTIDMAP = Utility.Map with type key = ClientID.t

module PidMap : PIDMAP
module ClientIDMap : CLIENTIDMAP

type 'a pid_map = 'a PidMap.t
type 'a client_id_map = 'a ClientIDMap.t

module type PIDSET = Utility.Set with type elt = ProcessID.t
module type CLIENTIDSET = Utility.Set with type elt = ClientID.t

module PidSet : PIDSET
module ClientIDSet : CLIENTIDSET

type pid_set = PidSet.t
type client_id_set = ClientIDSet.t
