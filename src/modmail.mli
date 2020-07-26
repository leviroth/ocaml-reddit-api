open! Core

module Conversation : sig
  type t [@@deriving sexp_of]

  val of_json : Json.t -> t
  val to_json : t -> Json.t
end
