open! Core

module Conversation : sig
  type t [@@deriving sexp_of]

  include Jsonable.S with type t := t
end
