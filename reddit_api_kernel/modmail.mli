open! Core

module Conversation : sig
  type t [@@deriving sexp_of]

  module Id : Id36.S
  include Json_object.S_with_fields with type t := t

  val id : t -> Id.t
end
