type serialised_t = string

module type SERIALISER = sig
  type s
  module Continuation: sig
    val save : Value.t Value.Continuation.t -> s
    val load : ?globals:Value.t Value.Env.t -> s -> Value.t Value.Continuation.t
  end

  module Value: sig
    val save : Value.t -> s
    val load : ?globals:Value.t Value.Env.t -> s -> Value.t
  end
end

module MarshalSerialiser : SERIALISER with type s := serialised_t
module YojsonSerialiser : SERIALISER with type s := serialised_t
module UnsafeJsonSerialiser : SERIALISER with type s := Yojson.Basic.t

module Continuation: sig
  val serialise : Value.t Value.Continuation.t -> serialised_t
  val deserialise : serialised_t -> Value.t Value.Continuation.t
end

module Value: sig
  val serialise : Value.t -> serialised_t
  val deserialise : serialised_t -> Value.t
end
