open! Core

module type S = sig
  type t

  val of_json : Json.t -> t
  val to_json : t -> Json.t
end
